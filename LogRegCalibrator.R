# ------------------------------------------------------------------------------
# Updated: October 7, 2025
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------
APP_VERSION <- "Version 1.2.0"
SS_LABEL <- "ss"
DS_LABEL <- "ds"

if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("DT")) install.packages("DT", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

library(shiny)
library(DT)
library(ggplot2)

source("func/train_llr_fusion_robust.R")
source("func/train_llr_fusion_regularized.R")
source("func/lin_fusion.R")
source("metric/Cllr.R")
source("metric/EER.R")
source("metric/TippettPlot.R")

transform_to_ln <- function(x, scale) {
  x <- as.numeric(x)
  if (scale == "Raw") {
    if (any(x <= 0, na.rm = TRUE)) {
      warning("Some LR values are <= 0.")
    }
    return(log(x))
  } else if (scale == "log10(LR)") {
    return(x * log(10))  # log10 -> ln
  } else if (scale == "ln(LR)") {
    return(x)
  } else {
    stop("Unknown scale provided.")
  }
} # all calibration is based on log-scaled LR

# Shiny UI
ui <- fluidPage(
  titlePanel(
    tagList(
      "Linear Logistic-Regression Calibrator",
      tags$span(APP_VERSION,
                style = "font-size: 16px; color: gray; margin-left: 12px;")
    )
  ),
  tabsetPanel(
    
    # ======================== Calibration Mode (UI) ===========================
    tabPanel("Calibration Mode",
             sidebarLayout(
               sidebarPanel(
                 fileInput("single_cal_file", "Upload Calibration Set ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("single_col_select"),
                 uiOutput("single_E_input"),
                 hr(),
                 selectInput("single_logreg_select",
                             "Select the LogReg Mode",
                             choices = c("Robust Version", "Regularized Version"),
                             selected = "Robust Version"),
                 uiOutput("single_logreg_setting"),
                 hr(),
                 actionButton("single_run_calibration", "Run Calibration", class = "btn-primary")
               ),
               mainPanel(
                 h5("Calibration Set Preview:"),
                 DTOutput("single_cal_set_preview"),
                 hr(),
                 h5("Calibration Results:"),
                 verbatimTextOutput("single_results")
               )
             )
    ),
    # ==========================================================================
    
    # ======================== Hold-out Validation (UI) ========================
    tabPanel("Hold-out Validation",
             sidebarLayout(
               sidebarPanel(
                 fileInput("hv_cal_file", "Upload Calibration Set ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("hv_cal_label_select"),
                 hr(),
                 fileInput("hv_val_file", "Upload Validation Set ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("hv_val_label_select"),
                 hr(),
                 uiOutput("hv_common_score_select"),
                 selectInput(inputId = "hv_scale",
                             label = "LR Scale for the Score",
                             choices = c("Raw", "log10(LR)", "ln(LR)"), 
                             selected = "log10(LR)"),
                 hr(),
                 selectInput("hv_logreg_select",
                             "Select the LogReg Mode",
                             choices = c("Robust Version", "Regularized Version"),
                             selected = "Robust Version"),
                 uiOutput("hv_logreg_setting"),
                 hr(),
                 actionButton("hv_run_calibration", "Run Calibration", class = "btn-primary"),
                 downloadButton("hv_downloadData", "Download Calibrated Data")
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                          h5("Calibration Set Preview:"),
                          DTOutput("hv_cal_set_preview")),
                   column(6,
                          h5("Validation Set Preview:"),
                          DTOutput("hv_val_set_preview"))
                 ),
                 hr(),
                 h5("Parameters of the Bi-Gaussianized Step:"),
                 verbatimTextOutput("hv_paras"),
                 h5("Calibration Results:"),
                 DTOutput("hv_results"),
                 hr(),
                 h5("Tippett Plot:"),
                 plotOutput("hv_tippettPlot"),
                 h5("Performance Metrics:"),
                 verbatimTextOutput("hv_metrics")
               )
             )
    ),
    # ==========================================================================
    
    # ================ Leave-one-out Cross Validation (UI) =====================
    tabPanel("Leave-one-out Cross Validation",
             sidebarLayout(
               sidebarPanel(
                 fileInput("loo_file", "Upload Calibration Set ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("loo_col_select"),
                 hr(),
                 selectInput(inputId = "loo_scale",
                             label = "LR Scale for the Score",
                             choices = c("Raw", "log10(LR)", "ln(LR)"), 
                             selected = "log10(LR)"),
                 hr(),
                 selectInput("loo_logreg_select",
                             "Select the LogReg Mode",
                             choices = c("Robust Version", "Regularized Version"),
                             selected = "Robust Version"),
                 uiOutput("loo_logreg_setting"),
                 hr(),
                 actionButton("loo_run_calibration", "Run Calibration", class = "btn-primary"),
                 downloadButton("loo_downloadData", "Download Calibrated Data")
               ),
               mainPanel(
                 h5("Calibration Set Preview:"),
                 DTOutput("loo_set_preview"),
                 hr(),
                 h5("Calibration Results:"),
                 DTOutput("loo_results"),
                 hr(),
                 h5("Tippett Plot:"),
                 plotOutput("loo_tippettPlot"),
                 h5("Performance Metrics:"),
                 verbatimTextOutput("loo_metrics")
               )
             )
    )
    # ==========================================================================
  )
)

# Shiny server
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
                     label = paste("Input Score of", colname),
                     value = 1, step = 0.1)
      }),
      selectInput(inputId = "evidence_scale",
                  label = "LR Scale for the Evidence Score",
                  choices = c("Raw", "log10(LR)", "ln(LR)"), 
                  selected = "log10(LR)"),
    )
  }) # dynamic module for evidential scores reading
  
  output$single_logreg_setting <- renderUI({
    req(input$single_logreg_select)
    
    if (input$single_logreg_select == "Robust Version") {
      tagList(
        numericInput("single_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("single_robust_weight", "Robust Weight", value = 0, step = 0.1),
        numericInput("single_max_iter", "Max Iterations", value = 5000, min = 1, step = 100)
      )
    } else if (input$single_logreg_select == "Regularized Version") {
      tagList(
        numericInput("single_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("single_kappa", "Regularization Strength ( kappa )", value = 0, step = 0.1),
        textInput("single_df", "Degree of Freedom ( df )", value = ""),
        numericInput("single_max_iter", "Max Iterations", value = 1000, min = 1, step = 100)
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
    
    df_val <- if (is.null(input$single_df) || input$single_df == "") NULL else as.numeric(input$single_df)
    
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
                                     df = df_val,
                                     max_iter = input$single_max_iter)
        }
    
    calibrated_lnLR <- lin_fusion(weights = fusion_w, scores = evidence_lnLR)
    calibrated_LR <- exp(calibrated_lnLR)
    calibrated_log10LR <- calibrated_lnLR / log(10)
    
    fusion_w <- fusion_w
    d <- length(score_cols)
    beta <- fusion_w[1:d]
    alpha <- fusion_w[d + 1]
    
    result_text <- paste0(
      "Calibrated LR:\n",
      "   - LR: ", calibrated_LR, "\n",
      "   - log10(LR): ", calibrated_log10LR, "\n",
      "   - ln(LR): ", calibrated_lnLR, "\n\n",
      
      "Fusion Weights:\n",
      "   - Beta (weights):\n",
      paste0(paste0("       ", score_cols, ": ", round(beta, 6)), collapse = "\n"), "\n",
      "   - Alpha (bias): ", round(alpha, 6), "\n\n"
    )
    
    output$single_results <- renderPrint({ cat(result_text) })
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
        numericInput("hv_robust_weight", "Robust Weight", value = 0, step = 0.1),
        numericInput("hv_max_iter", "Max Iterations", value = 5000, min = 1, step = 100)
      )
    } else if (input$hv_logreg_select == "Regularized Version") {
      tagList(
        numericInput("hv_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("hv_kappa", "Regularization Strength ( kappa )", value = 0, step = 0.1),
        textInput("hv_df", "Degree of Freedom ( df )", value = ""),
        numericInput("hv_max_iter", "Max Iterations", value = 1000, min = 1, step = 100)
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
    
    df_val <- if (is.null(input$hv_df) || input$hv_df == "") NULL else as.numeric(input$hv_df)
    
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
                                   df = df_val,
                                   max_iter = input$hv_max_iter)
    }
    
    calibrated_lnLR <- lin_fusion(weights = hv_fusion_w,
                                  scores = val_lnLR)
    calibrated_LR <- exp(calibrated_lnLR)
    calibrated_log10LR <- calibrated_lnLR / log(10)
    
    val_df$calibrated_LR <- calibrated_LR
    val_df$calibrated_lnLR <- calibrated_lnLR
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
    
    ss_lr <- calibrated_results[calibrated_results[[input$hv_val_label_col]] == SS_LABEL, "calibrated_LR"]
    ds_lr <- calibrated_results[calibrated_results[[input$hv_val_label_col]] == DS_LABEL, "calibrated_LR"]
    
    ss_lr <- ss_lr[!is.na(ss_lr)]
    ds_lr <- ds_lr[!is.na(ds_lr)]
    
    cllr_pooled <- Cllr(ss_lr, ds_lr)
    cllr_min <- Cllr_min(ss_lr, ds_lr)
    cllr_cal <- Cllr_cal(ss_lr, ds_lr)
    eer_result <- EER(ss_lr, ds_lr)
    
    output$hv_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled, "\n")
      cat("Cllr (min):", cllr_min, "\n")
      cat("Cllr (cal):", cllr_cal, "\n")
      cat("EER:", eer_result$EER, "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):", eer_result$threshold_Raw, "\n")
    })
    
    output$hv_tippettPlot <- renderPlot({
      tippett.plot(
        ss_lr = ss_lr,
        ds_lr = ds_lr,
        x_lab = "Log10 Likelihood Ratio",
        y_lab = "Cumulative Proportion"
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
        numericInput("loo_robust_weight", "Robust Weight", value = 0, step = 0.1),
        numericInput("loo_max_iter", "Max Iterations", value = 5000, min = 1, step = 100)
      )
    } else if (input$loo_logreg_select == "Regularized Version") {
      tagList(
        numericInput("loo_prior", "Prior", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("loo_kappa", "Regularization Strength ( kappa )", value = 0, step = 0.1),
        textInput("loo_df", "Degree of Freedom ( df )", value = ""),
        numericInput("loo_max_iter", "Max Iterations", value = 1000, min = 1, step = 100)
      )
    }
  })
  
  observeEvent(input$loo_run_calibration, {
    req(loo_data(), input$loo_id1_col, input$loo_id2_col, input$loo_score_col)
    
    df <- loo_data()
    score_cols <- input$loo_score_col
    
    id1 <- as.character(df[[input$loo_id1_col]])
    id2 <- as.character(df[[input$loo_id2_col]])
    df$label <- ifelse(id1 == id2, "ss", "ds")
    
    make_key <- function(a, b) {
      ab <- sort(c(as.character(a), as.character(b)))
      paste0(ab[1], "|", ab[2])
    }
    df$leave_out_key <- mapply(make_key, id1, id2, USE.NAMES = FALSE)
    
    all_ids <- sort(unique(c(id1, id2)))
    id2rows <- setNames(vector("list", length(all_ids)), all_ids)
    for (s in all_ids) {
      id2rows[[s]] <- which(id1 == s | id2 == s)
    }
    
    unique_keys <- unique(df$leave_out_key)
    
    ln_mat_all <- as.matrix(do.call(cbind, lapply(
      score_cols,
      function(col) transform_to_ln(df[[col]], input$loo_scale)
    )))
    colnames(ln_mat_all) <- score_cols
    
    withProgress(message = "Calibrating by Leave-out Keys:", value = 0, {
      nkeys <- length(unique_keys)
      
      calibrated_lnLR <- rep(NA_real_, nrow(df))
      d <- length(score_cols)
      beta_mat <- matrix(NA_real_, nrow = nrow(df), ncol = d)
      alpha_vec <- rep(NA_real_, nrow(df))
      
      for (k_i in seq_along(unique_keys)) {
        key <- unique_keys[k_i]
        parts <- strsplit(key, "\\|")[[1]]
        a <- parts[1]; b <- parts[2]
        
        excl_idx <- if (a == b) id2rows[[a]] else union(id2rows[[a]], id2rows[[b]])
        train_idx <- setdiff(seq_len(nrow(df)), excl_idx)
        train_labels <- df$label[train_idx]
        
        train_ss_ln <- ln_mat_all[train_idx[train_labels == "ss"], , drop = FALSE]
        train_ds_ln <- ln_mat_all[train_idx[train_labels == "ds"], , drop = FALSE]
        
        if (nrow(train_ss_ln) == 0 || nrow(train_ds_ln) == 0) {
          message("Skipped key [", key, "]: insufficient data.")
          incProgress(1 / nkeys, detail = paste0(k_i, "/", nkeys))
          next
        }
        
        model <- tryCatch({
          if (input$loo_logreg_select == "Robust Version") {
            train_llr_fusion_robust(
              targets = train_ss_ln,
              non_targets = train_ds_ln,
              prior = input$loo_prior,
              robust_weight = input$loo_robust_weight,
              max_iter = input$loo_max_iter
            )
          } else {
            df_val <- if (is.null(input$loo_df) || input$loo_df == "") NULL else as.numeric(input$loo_df)
            train_llr_fusion_regularized(
              targets = train_ss_ln,
              non_targets = train_ds_ln,
              prior = input$loo_prior,
              kappa = input$loo_kappa,
              df = df_val,
              max_iter = input$loo_max_iter
            )
          }
        }, error = function(e) {
          message("Training failed for key [", key, "]: ", e$message)
          NULL
        })
        
        if (is.null(model)) {
          incProgress(1 / nkeys, detail = paste0(k_i, "/", nkeys))
          next
        }
        
        if (length(model) != d + 1) {
          message("Model length mismatch for key [", key, "]. Expected ", d + 1, " got ", length(model))
          incProgress(1 / nkeys, detail = paste0(k_i, "/", nkeys))
          next
        }
        
        idx <- which(df$leave_out_key == key)
        if (length(idx) > 0) {
          batch_scores <- ln_mat_all[idx, , drop = FALSE]
          cal_llr_batch <- tryCatch({
            lin_fusion(weights = model,
                       scores = batch_scores)
          }, error = function(e) {
            message("Calibration failed for key [", key, "]: ", e$message)
            rep(NA_real_, length(idx))
          })
          
          calibrated_lnLR[idx] <- as.numeric(cal_llr_batch)
          
          beta_vals <- model[1:d]
          alpha_val <- model[d + 1]
          beta_mat[idx, ] <- matrix(beta_vals, nrow = length(idx), ncol = d, byrow = TRUE)
          alpha_vec[idx] <- alpha_val
        }
        
        rm(model); gc()
        incProgress(1 / nkeys, detail = paste0(k_i, "/", nkeys))
      }
    })
    
    beta_colnames <- paste0("Beta_", make.names(score_cols, unique = TRUE))
    for (j in seq_len(d)) {
      df[[beta_colnames[j]]] <- beta_mat[, j]
    }
    df$Alpha <- alpha_vec
    
    df$calibrated_lnLR <- calibrated_lnLR
    df$calibrated_LR <- exp(calibrated_lnLR)
    df$calibrated_log10LR <- calibrated_lnLR / log(10)
    
    output$loo_results <- renderDT({
      datatable(
        df,
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
    
    ss_lr <- df[df$label == "ss", "calibrated_LR"]
    ds_lr <- df[df$label == "ds", "calibrated_LR"]
    
    ss_lr <- ss_lr[!is.na(ss_lr)]
    ds_lr <- ds_lr[!is.na(ds_lr)]
    
    output$loo_tippettPlot <- renderPlot({
      tryCatch({
        tippett.plot(
          ss_lr = ss_lr,
          ds_lr = ds_lr,
          x_lab = "Log10 Likelihood Ratio",
          y_lab = "Cumulative Proportion"
        )
      }, error = function(e) {
        message("Tippett plot skipped due to error: ", e$message)
        plot.new()
        title("Tippett plot skipped.")
      })
    })
    
    cllr_pooled <- Cllr(ss_lr, ds_lr)
    cllr_min    <- Cllr_min(ss_lr, ds_lr)
    cllr_cal    <- Cllr_cal(ss_lr, ds_lr)
    eer_result  <- EER(ss_lr, ds_lr)
    
    output$loo_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled, "\n")
      cat("Cllr (min):",    cllr_min,    "\n")
      cat("Cllr (cal):",    cllr_cal,    "\n")
      cat("EER:",           eer_result$EER,            "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):",   eer_result$threshold_Raw,   "\n")
    })
    
    output$loo_downloadData <- downloadHandler(
      filename = function() {
        original_name <- tools::file_path_sans_ext(input$loo_file$name)
        paste0(original_name, "_calibrated_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
  # ============================================================================
  
}

# Shiny application
options(shiny.maxRequestSize = 600*1024^2)
shinyApp(ui = ui, server = server)
