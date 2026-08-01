# Path: "server/LogRegCalibrator_server.R"

source("server/utils/transform_to_ln.R", local = TRUE)
source("server/utils/LogReg_LOOCV_helper.R", local = TRUE)

server <- function(input, output, session){
  
  # ======================== Calibration Mode (server) =========================
  single_data <- reactive({
    req(input$single_cal_file)
    read.csv(input$single_cal_file$datapath, stringsAsFactors = FALSE)
  })
  
  output$single_col_select <- renderUI({
    req(single_data())
    df <- single_data()
    tagList(
      selectInput("single_label_col", "Select Label Column",
                  choices = names(df)),
      selectInput("single_score_col", "Select Score Column(s)",
                  choices = names(df), multiple = TRUE),
      selectInput("single_scale", "LR Scale for the Calibration Set",
                  choices = c("Raw", "log10(LR)", "ln(LR)"), 
                  selected = "log10(LR)")
    )
  }) # dynamic module for calibration set reading
  
  output$single_E_input <- renderUI({
    req(input$single_score_col)  
    score_cols <- input$single_score_col
    tagList(
      lapply(score_cols, function(colname) {
        numericInput(inputId = paste0("E_", colname),
                     label = paste("Evidence Score of '", colname, "'"),
                     value = 1, step = 0.1)
      }),
      selectInput(inputId = "evidence_scale",
                  label = "LR Scale for the Evidence Score",
                  choices = c("Raw", "log10(LR)", "ln(LR)"), 
                  selected = "log10(LR)")
    )
  }) # dynamic module for evidential scores reading
  
  output$single_logreg_setting <- renderUI({
    req(input$single_logreg_select)
    
    if (input$single_logreg_select == "Robust Version") {
      tagList(
        numericInput("single_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("single_robust_weight", "Robust Weight", value = 0, step = 0.01),
        numericInput("single_max_iter", "Max Iterations", value = 5000, min = 1, step = 1000)
      )
    } else if (input$single_logreg_select == "Regularized Version") {
      tagList(
        numericInput("single_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("single_kappa", "Regularization Strength ( kappa )", value = 0, step = 0.01),
        textInput("single_df", "Degree of Freedom ( df )", value = ""),
        numericInput("single_max_iter", "Max Iterations", value = 5000, min = 1, step = 1000)
      )
    }
  })
  
  output$single_cal_set_preview <- renderDT({
    req(single_data())
    datatable(
      single_data(),
      rownames = FALSE,
      options = list(
        pageLength = 5,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        ordering = TRUE,
        dom = 'tip'
      )
    )
  })
  
  observeEvent(input$single_run_calibration, {
    
    req(single_data(), input$single_label_col, input$single_score_col)
    
    df <- single_data()
    score_cols <- input$single_score_col
    
    ss_scores <- df[df[[input$single_label_col]] == SS_LABEL, score_cols, drop = FALSE]
    ds_scores <- df[df[[input$single_label_col]] == DS_LABEL, score_cols, drop = FALSE]
    
    ss_lnLR <- as.matrix(apply(ss_scores, 2, transform_to_ln, scale = input$single_scale))
    ds_lnLR <- as.matrix(apply(ds_scores, 2, transform_to_ln, scale = input$single_scale))
    
    evidence_vec <- sapply(score_cols, function(col) as.numeric(input[[paste0("E_", col)]]))
    evidence_lnLR <- transform_to_ln(evidence_vec, input$evidence_scale)
    evidence_lnLR <- matrix(evidence_lnLR, nrow = 1)
    
    if (isTRUE(input$single_zscore)) {
      z_train <- rbind(ss_lnLR, ds_lnLR)
      z_mu <- colMeans(z_train, na.rm = TRUE)
      z_sd <- apply(z_train, 2, sd, na.rm = TRUE)
      z_sd[is.na(z_sd) | z_sd == 0] <- 1
      
      ss_lnLR <- sweep(sweep(ss_lnLR, 2, z_mu, "-"), 2, z_sd, "/")
      ds_lnLR <- sweep(sweep(ds_lnLR, 2, z_mu, "-"), 2, z_sd, "/")
      evidence_lnLR <- sweep(sweep(evidence_lnLR, 2, z_mu, "-"), 2, z_sd, "/")
    }
    
    single_reg_df_val <- if (is.null(input$single_df) || input$single_df == "") NULL else as.numeric(input$single_df)
    
    fusion_w <- if (input$single_logreg_select == "Robust Version") {
      train_llr_fusion_robust(targets = ss_lnLR,
                              non_targets = ds_lnLR,
                              prior = input$single_prior,
                              robust_weight = input$single_robust_weight,
                              max_iter = input$single_max_iter)
    } else {
      train_llr_fusion_regularized(targets = ss_lnLR,
                                   non_targets = ds_lnLR,
                                   prior = input$single_prior,
                                   kappa = input$single_kappa,
                                   df = single_reg_df_val,
                                   max_iter = input$single_max_iter)
    }
    
    calibrated_lnLR <- lin_fusion(weights = fusion_w, scores = evidence_lnLR)
    calibrated_LR <- exp(calibrated_lnLR)
    calibrated_log10LR <- calibrated_lnLR / log(10)
    
    fusion_w <- fusion_w
    d <- length(score_cols)
    beta <- fusion_w[1:d]
    alpha <- fusion_w[d + 1]
    
    output$single_paras <- renderPrint({
      cat("Fusion Weights:\n")
      cat("   - Beta (weights):\n")
      for (i in seq_along(score_cols)) {
        cat("       ", score_cols[i], ": ", round(beta[i], 6), "\n", sep = "")
      }
      cat("   - Alpha (bias): ", round(alpha, 6), "\n", sep = "")
    })
    
    output$single_results <- renderPrint({
      cat("Calibrated LR:\n")
      cat("   - LR: ", calibrated_LR, "\n", sep = "")
      cat("   - log10(LR): ", calibrated_log10LR, "\n", sep = "")
      cat("   - ln(LR): ", calibrated_lnLR, "\n", sep = "")
    })
  })
  # ============================================================================
  
  # ======================== Hold-out Validation (server) ======================
  hv_cal_data <- reactive({
    req(input$hv_cal_file)
    read.csv(input$hv_cal_file$datapath, stringsAsFactors = FALSE)
  })
  
  output$hv_cal_label_select <- renderUI({
    req(hv_cal_data())
    df <- hv_cal_data()
    selectInput("hv_cal_label_col", "Select Label Column",
                choices = names(df))
  }) # dynamic module for calibration label reading
  
  hv_val_data <- reactive({
    req(input$hv_val_file)
    read.csv(input$hv_val_file$datapath, stringsAsFactors = FALSE)
  })
  
  output$hv_val_label_select <- renderUI({
    req(hv_val_data())
    df <- hv_val_data()
    selectInput("hv_val_label_col", "Select Label Column",
                choices = names(df))
  }) # dynamic module for validation label reading
  
  output$hv_common_score_select <- renderUI({
    req(hv_cal_data(), hv_val_data())
    common_cols <- intersect(names(hv_cal_data()), names(hv_val_data()))
    
    if (length(common_cols) == 0) {
      return(h5("No common score columns found between calibration and validation sets."))
    }
    
    selectInput("hv_score_col", "Select Score Column(s)",
                choices = common_cols, multiple = TRUE)
  }) # dynamic module for scores reading
  
  output$hv_logreg_setting <- renderUI({
    req(input$hv_logreg_select)
    
    if (input$hv_logreg_select == "Robust Version") {
      tagList(
        numericInput("hv_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("hv_robust_weight", "Robust Weight", value = 0, step = 0.01),
        numericInput("hv_max_iter", "Max Iterations", value = 5000, min = 1, step = 1000)
      )
    } else if (input$hv_logreg_select == "Regularized Version") {
      tagList(
        numericInput("hv_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("hv_kappa", "Regularization Strength ( kappa )", value = 0, step = 0.01),
        textInput("hv_df", "Degree of Freedom ( df )", value = ""),
        numericInput("hv_max_iter", "Max Iterations", value = 5000, min = 1, step = 1000)
      )
    }
  })
  
  output$hv_cal_set_preview <- renderDT({
    req(hv_cal_data())
    datatable(
      hv_cal_data(),
      rownames = FALSE,
      options = list(
        pageLength = 5,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        ordering = TRUE,
        dom = 'tip'
      )
    )
  })
  
  output$hv_val_set_preview <- renderDT({
    req(hv_val_data())
    datatable(
      hv_val_data(),
      rownames = FALSE,
      options = list(
        pageLength = 5,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        ordering = TRUE,
        dom = 'tip'
      )
    )
  })
  
  observeEvent(input$hv_run_calibration, {
    req(hv_cal_data(), hv_val_data(),
        input$hv_cal_label_col, input$hv_val_label_col,input$hv_score_col)
    
    cal_df <- hv_cal_data()
    val_df <- hv_val_data()
    score_cols <- input$hv_score_col
    
    cal_ss <- cal_df[cal_df[[input$hv_cal_label_col]] == SS_LABEL, score_cols, drop = FALSE]
    cal_ds <- cal_df[cal_df[[input$hv_cal_label_col]] == DS_LABEL, score_cols, drop = FALSE]
    val_scores <- val_df[, score_cols, drop = FALSE]
    
    cal_ss_lnLR <- as.matrix(apply(cal_ss, 2, transform_to_ln, scale = input$hv_scale))
    cal_ds_lnLR <- as.matrix(apply(cal_ds, 2, transform_to_ln, scale = input$hv_scale))
    val_lnLR <- as.matrix(apply(val_scores, 2, transform_to_ln, scale = input$hv_scale))
    
    if (isTRUE(input$hv_zscore)) {
      z_train <- rbind(cal_ss_lnLR, cal_ds_lnLR)
      z_mu <- colMeans(z_train, na.rm = TRUE)
      z_sd <- apply(z_train, 2, sd, na.rm = TRUE)
      z_sd[is.na(z_sd) | z_sd == 0] <- 1
      
      cal_ss_lnLR <- sweep(sweep(cal_ss_lnLR, 2, z_mu, "-"), 2, z_sd, "/")
      cal_ds_lnLR <- sweep(sweep(cal_ds_lnLR, 2, z_mu, "-"), 2, z_sd, "/")
      val_lnLR <- sweep(sweep(val_lnLR, 2, z_mu, "-"), 2, z_sd, "/")
    }
    
    hv_reg_df_val <- if (is.null(input$hv_df) || input$hv_df == "") NULL else as.numeric(input$hv_df)
    
    hv_fusion_w <- if (input$hv_logreg_select == "Robust Version") {
      train_llr_fusion_robust(targets = cal_ss_lnLR,
                              non_targets = cal_ds_lnLR,
                              prior = input$hv_prior,
                              robust_weight = input$hv_robust_weight,
                              max_iter = input$hv_max_iter)
    } else {
      train_llr_fusion_regularized(targets = cal_ss_lnLR,
                                   non_targets = cal_ds_lnLR,
                                   prior = input$hv_prior,
                                   kappa = input$hv_kappa,
                                   df = hv_reg_df_val,
                                   max_iter = input$hv_max_iter)
    }
    
    calibrated_lnLR <- lin_fusion(weights = hv_fusion_w,
                                  scores = val_lnLR)
    calibrated_LR <- exp(calibrated_lnLR)
    calibrated_log10LR <- calibrated_lnLR / log(10)
    
    val_df$calibrated_lnLR <- calibrated_lnLR
    val_df$calibrated_LR <- calibrated_LR
    val_df$calibrated_log10LR <- calibrated_log10LR
    
    calibrated_results <- val_df
    d <- length(score_cols)
    beta <- hv_fusion_w[1:d]
    alpha <- hv_fusion_w[d + 1]
    
    output$hv_paras <- renderPrint({
      cat("Fusion Weights:\n")
      cat("   - Beta (weights):\n")
      for (i in seq_along(score_cols)) {
        cat("       ", score_cols[i], ": ", round(beta[i], 6), "\n")
      }
      cat("   - Alpha (bias): ", round(alpha, 6), "\n")
    })
    
    output$hv_results <- renderDT({
      datatable(
        calibrated_results,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          ordering = TRUE,
          dom = 'tip'
        )
      )
    })
    
    ss_llr <- calibrated_results[calibrated_results[[input$hv_val_label_col]] == SS_LABEL, "calibrated_lnLR"]
    ds_llr <- calibrated_results[calibrated_results[[input$hv_val_label_col]] == DS_LABEL, "calibrated_lnLR"]
    
    ss_llr <- ss_llr[!is.na(ss_llr)]
    ds_llr <- ds_llr[!is.na(ds_llr)]
    
    cllr_pooled <- cllr(ss_llr, ds_llr)
    cllr_min <- cllr_min(ss_llr, ds_llr)
    cllr_cal <- cllr_cal(ss_llr, ds_llr)
    eer_result <- eer(ss_llr, ds_llr)
    
    output$hv_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled, "\n")
      cat("Cllr (min):", cllr_min, "\n")
      cat("Cllr (cal):", cllr_cal, "\n")
      cat("EER:", eer_result$EER, "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):", eer_result$threshold_raw, "\n")
    })
    
    output$hv_tippettPlot <- renderPlot({
      tippett_plot(
        ss_llr = ss_llr,
        ds_llr = ds_llr,
        x_lab = expression(log[10](Lambda)), 
        y_lab = "cumulative proportion"
      )
    })
    
    output$hv_downloadData <- downloadHandler(
      filename = function() {
        original_name <- tools::file_path_sans_ext(input$hv_val_file$name)
        paste0(original_name, "_calibrated_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(calibrated_results, file, row.names = FALSE)
      }
    )
  })
  # ============================================================================
  
  # ================ Leave-one-out Cross Validation (server) ===================
  loo_data <- reactive({
    req(input$loo_file)
    read.csv(input$loo_file$datapath, stringsAsFactors = FALSE)
  })
  
  output$loo_col_select <- renderUI({
    req(loo_data())
    df <- loo_data()
    tagList(
      selectInput("loo_id1_col", "Select ID-1 Column",
                  choices = names(df)),
      selectInput("loo_id2_col", "Select ID-2 Column",
                  choices = names(df)),
      selectInput("loo_score_col", "Select Score Column(s)",
                  choices = names(df), multiple = TRUE)
    )
  }) # dynamic module for calibration set reading
  
  output$loo_set_preview <- renderDT({
    req(loo_data())
    datatable(
      loo_data(),
      rownames = FALSE,
      options = list(
        pageLength = 5,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        ordering = TRUE,
        dom = 'tip'
      )
    )
  })
  
  output$loo_logreg_setting <- renderUI({
    req(input$loo_logreg_select)
    
    if (input$loo_logreg_select == "Robust Version") {
      tagList(
        numericInput("loo_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("loo_robust_weight", "Robust Weight", value = 0, step = 0.01),
        numericInput("loo_max_iter", "Max Iterations", value = 5000, min = 1, step = 1000)
      )
    } else if (input$loo_logreg_select == "Regularized Version") {
      tagList(
        numericInput("loo_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("loo_kappa", "Regularization Strength ( kappa )", value = 0, step = 0.01),
        textInput("loo_df", "Degree of Freedom ( df )", value = ""),
        numericInput("loo_max_iter", "Max Iterations", value = 5000, min = 1, step = 1000)
      )
    }
  })
  
  observeEvent(input$loo_run_calibration, {
    req(loo_data(), input$loo_id1_col, input$loo_id2_col, input$loo_score_col)
    
    df <- loo_data()
    score_cols <- input$loo_score_col
    
    calibrated_results <- if (input$loo_logreg_select == "Robust Version") {
      
      LogReg_LOOCV_robust(
        df = df,
        id1_col = input$loo_id1_col,
        id2_col = input$loo_id2_col,
        score_col = score_cols,
        score_scale = input$loo_scale,
        prior = input$loo_prior,
        robust_weight = input$loo_robust_weight,
        max_iter = input$loo_max_iter,
        z_score = input$loo_zscore,
        ss_label = SS_LABEL,
        ds_label = DS_LABEL
      )
      
    } else {
      
      loo_df_reg_val <- if (is.null(input$loo_df) || input$loo_df == "") {
        NULL
      } else {
        as.numeric(input$loo_df)
      }
      
      LogReg_LOOCV_regularized(
        df = df,
        id1_col = input$loo_id1_col,
        id2_col = input$loo_id2_col,
        score_col = score_cols,
        score_scale = input$loo_scale,
        prior = input$loo_prior,
        kappa = input$loo_kappa,
        df_reg = loo_df_reg_val,
        max_iter = input$loo_max_iter,
        z_score = input$loo_zscore,
        ss_label = SS_LABEL,
        ds_label = DS_LABEL
      )
    }
    
    output$loo_results <- renderDT({
      datatable(
        calibrated_results,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          ordering = TRUE,
          dom = 'tip'
        )
      )
    })
    
    ss_llr <- calibrated_results[calibrated_results$label == SS_LABEL, "calibrated_lnLR"]
    ds_llr <- calibrated_results[calibrated_results$label == DS_LABEL, "calibrated_lnLR"]
    
    ss_llr <- ss_llr[!is.na(ss_llr)]
    ds_llr <- ds_llr[!is.na(ds_llr)]
    
    output$loo_tippettPlot <- renderPlot({
      tryCatch({
        tippett_plot(
          ss_llr = ss_llr,
          ds_llr = ds_llr,
          x_lab = expression(log[10](Lambda)), 
          y_lab = "cumulative proportion"
        )
      }, error = function(e) {
        message("Tippett plot skipped due to error: ", e$message)
        plot.new()
        title("Tippett plot skipped.")
      })
    })
    
    cllr_pooled <- cllr(ss_llr, ds_llr)
    cllr_min_result <- cllr_min(ss_llr, ds_llr)
    cllr_cal_result <- cllr_cal(ss_llr, ds_llr)
    eer_result <- eer(ss_llr, ds_llr)
    
    output$loo_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled, "\n")
      cat("Cllr (min):", cllr_min_result, "\n")
      cat("Cllr (cal):", cllr_cal_result, "\n")
      cat("EER:", eer_result$EER, "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):", eer_result$threshold_raw, "\n")
    })
    
    output$loo_downloadData <- downloadHandler(
      filename = function() {
        original_name <- tools::file_path_sans_ext(input$loo_file$name)
        paste0(original_name, "_calibrated_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(calibrated_results, file, row.names = FALSE)
      }
    )
  })
  # ============================================================================
  
}