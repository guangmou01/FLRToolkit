# Path: "server/TippettPlotter_server.R"

server <- function(input, output, session){
  
  # ===================== Single System Evaluation (server) ====================
  data <- reactive({
    req(input$single_data_file)
    read.csv(input$single_data_file$datapath, stringsAsFactors = FALSE)
  }) # read the file
  
  output$single_label_col_select <- renderUI({
    req(data())
    selectInput("single_label_col", "Select Label Column",
                choices = names(data()))
  }) # dynamic module
  
  output$single_lr_col_select <- renderUI({
    req(data())
    selectInput("single_lr_col", "Select LR Column",
                choices = names(data()))
  }) # dynamic module
  
  single_analysis_data <- eventReactive(input$single_start_analysis, {
    
    req(input$single_label_col, input$single_lr_col)
    
    df <- data()
    lr_values <- as.numeric(df[[input$single_lr_col]])
    labels <- df[[input$single_label_col]]
    
    list(lr_values = lr_values, labels = labels)
  })
  
  single_plot_reactive <- reactive({
    
    single_data <- single_analysis_data()
    lr_values <- single_data$lr_values
    
    if (input$single_scale == "Raw") {
      llr_values <- log10(lr_values)
    } else if (input$single_scale == "log10(LR)") {
      llr_values <- lr_values
    } else if (input$single_scale == "ln(LR)") {
      llr_values <- lr_values / log(10)
    } # scale transformation (to log10-scale)
    
    labels <- single_data$labels
    ss_LLR <- llr_values[labels == SS_LABEL]
    ds_LLR <- llr_values[labels == DS_LABEL]
    
    data_ss <- data.frame(LLR = sort(ss_LLR))
    data_ss$Cumulative_Prop <- seq_along(data_ss$LLR) / length(data_ss$LLR)
    data_ds <- data.frame(LLR = sort(ds_LLR, decreasing = TRUE))
    data_ds$Cumulative_Prop <- seq_along(data_ds$LLR) / length(data_ds$LLR)
    
    single_plot <- ggplot() +
      geom_line(data = data_ss, aes(x = LLR, y = Cumulative_Prop), color = "red",
                linewidth = 0.8, na.rm = TRUE) +
      geom_line(data = data_ds, aes(x = LLR, y = Cumulative_Prop), color = "blue",
                linewidth = 0.8, na.rm = TRUE) +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      labs(x = expression(log[10](Lambda)),
           y = "cumulative proportion") +
      scale_x_continuous(
        expand = c(0, 0),
        breaks = pretty(c(input$single_x_min, input$single_x_max), n = 8)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(input$single_y_min, input$single_y_max, length.out = 11)
      ) +
      coord_cartesian(
        xlim = c(input$single_x_min, input$single_x_max),
        ylim = c(input$single_y_min, input$single_y_max)
      ) +
      theme_minimal(base_size = input$single_font_size) +
      theme(legend.position = "none",
            panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 1),
            panel.grid.minor = element_blank(),
            axis.ticks.length = unit(-0.2, "cm"),
            axis.ticks = element_line(color = "black", linewidth = 0.2),
            axis.text = element_text(color = "black", size = input$single_font_size),
            axis.title = element_text(color = "black", size = input$single_font_size),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
    
    # Evidence mark
    if (!is.null(input$single_E) && !is.na(input$single_E) && input$single_E > 0) {
      single_plot <- single_plot + geom_vline(xintercept = log10(input$single_E),
                                              color = "darkgreen",
                                              linetype = "solid",
                                              linewidth = 0.8)
    }
    
    return(single_plot) # return the final plot
  })
  
  output$single_tippettPlot <- renderPlot({
    print(single_plot_reactive())
  }) # print the tippett plot
  
  output$single_metrics <- renderPrint({
    
    single_data <- single_analysis_data()
    lr_values <- single_data$lr_values
    
    if (input$single_scale == "Raw") {
      llr_values <- log(lr_values)
    } else if (input$single_scale == "log10(LR)") {
      llr_values <- lr_values * log(10)
    } else if (input$single_scale == "ln(LR)") {
      llr_values <- lr_values
    } # scale transformation (to natural-log-scale)
    
    labels <- single_data$labels
    ss_LLR <- llr_values[labels == SS_LABEL]
    ds_LLR <- llr_values[labels == DS_LABEL]
    
    cllr_pooled <- cllr(ss_LLR, ds_LLR)
    cllr_min <- cllr_min(ss_LLR, ds_LLR)
    cllr_cal <- cllr_cal(ss_LLR, ds_LLR)
    eer_result <- eer(ss_LLR, ds_LLR)
    
    cat("Cllr (pooled):", cllr_pooled, "\n")
    cat("Cllr (min):", cllr_min, "\n")
    cat("Cllr (cal):", cllr_cal, "\n")
    cat("EER:", eer_result$EER, "\n")
    cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
    cat("EER Threshold (raw):", eer_result$threshold_raw, "\n")
  })
  
  output$single_downloadPlot <- downloadHandler(
    filename = function() {
      paste0("tippett_plot_", Sys.Date(), ".", input$single_fig_down)
    },
    content = function(file) {
      plot_obj <- single_plot_reactive()
      file <- as.character(file)
      
      if (input$single_fig_down == "png") {
        ggsave(file, plot = plot_obj, device = "png", width = 8, height = 6, dpi = 1200)
      } else if (input$single_fig_down == "pdf") {
        ggsave(file, plot = plot_obj, device = "pdf", width = 8, height = 6)
      } else if (input$single_fig_down == "svg") {
        ggsave(file, plot = plot_obj, device = "svg", width = 8, height = 6)
      }
    }
  )
  # ============================================================================
  
  # ===================== Multi-system Comparison (server) =====================
  multi_data_list <- reactive({
    req(input$multi_data_file)
    n_files <- nrow(input$multi_data_file)
    lapply(1:n_files, function(i) {
      read.csv(input$multi_data_file$datapath[i], stringsAsFactors = FALSE)
    })
  }) # read the file
  
  output$multi_file_options <- renderUI({
    req(input$multi_data_file)
    n_files <- nrow(input$multi_data_file)
    file_options <- lapply(1:n_files, function(i) {
      file_name <- input$multi_data_file$name[i]
      df <- read.csv(input$multi_data_file$datapath[i], stringsAsFactors = FALSE)
      wellPanel(
        h5(paste("Options for :", file_name)),
        selectInput(inputId = paste0("multi_label_col_", i),
                    label = "Select Label Column",
                    choices = names(df)),
        selectInput(inputId = paste0("multi_lr_col_", i),
                    label = "Select LR Column",
                    choices = names(df)),
        selectInput(inputId = paste0("multi_line_type_", i),
                    label = "Select Line Type",
                    choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                    selected = ifelse(i %% 2 == 1, "solid", "dashed")),
        numericInput(inputId = paste0("multi_E_", i),
                     label = "Evidence LR ( raw )", value = NULL, min = 0, step = 0.1)
      )
    })
    do.call(tagList, file_options)
  }) # dynamic module
  
  multi_analysis_data <- eventReactive(input$multi_start_analysis, {
    
    req(input$multi_data_file)
    
    multi_dfs <- multi_data_list()
    n_files <- length(multi_dfs)
    
    out <- vector("list", n_files)
    
    for (i in 1:n_files) {
      
      df <- multi_dfs[[i]]
      
      label_col <- input[[paste0("multi_label_col_", i)]]
      lr_col <- input[[paste0("multi_lr_col_", i)]]
      line_type <- input[[paste0("multi_line_type_", i)]]
      E_value <- input[[paste0("multi_E_", i)]]
      
      if (is.null(label_col) || is.null(lr_col)) next
      
      lr_values <- as.numeric(df[[lr_col]])
      labels <- df[[label_col]]
      
      out[[i]] <- list(
        file_name = input$multi_data_file$name[i],
        lr_values = lr_values,
        labels = labels,
        line_type = line_type,
        E_value = E_value
      )
    }
    out
  })
  
  # Construct multi-tippett plot
  multi_plot_reactive <- reactive({
    multi_data <- multi_analysis_data()
    
    multi_plot <- ggplot() +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      labs(x = expression(log[10](Lambda)),
           y = "cumulative proportion") +
      scale_x_continuous(
        expand = c(0, 0),
        breaks = pretty(c(input$multi_x_min, input$multi_x_max), n = 8)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(input$multi_y_min, input$multi_y_max, length.out = 11)
      ) +
      coord_cartesian(
        xlim = c(input$multi_x_min, input$multi_x_max),
        ylim = c(input$multi_y_min, input$multi_y_max)
      ) +
      theme_minimal(base_size = input$multi_font_size) +
      theme(legend.position = "none",
            panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 1),
            panel.grid.minor = element_blank(),
            axis.ticks.length = unit(-0.2, "cm"),
            axis.ticks = element_line(color = "black", linewidth = 0.2),
            axis.text = element_text(color = "black", size = input$multi_font_size),
            axis.title = element_text(color = "black", size = input$multi_font_size),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
    
    n_files <- length(multi_data)
    for (i in 1:n_files) {
      
      one_data <- multi_data[[i]]
      if (is.null(one_data)) next
      
      lr_values <- one_data$lr_values
      labels <- one_data$labels
      line_type <- one_data$line_type
      E_value <- one_data$E_value
      
      if (input$multi_scale == "Raw") {
        llr_values <- log10(lr_values)
      } else if (input$multi_scale == "log10(LR)") {
        llr_values <- lr_values
      } else if (input$multi_scale == "ln(LR)") {
        llr_values <- lr_values/log(10)
      } # scale transformation (to log10-scale)
      
      ss_LLR <- llr_values[labels == SS_LABEL]
      ds_LLR <- llr_values[labels == DS_LABEL]
      
      data_ss <- data.frame(LLR = sort(ss_LLR))
      data_ss$Cumulative_Prop <- seq_along(data_ss$LLR) / length(data_ss$LLR)
      data_ds <- data.frame(LLR = sort(ds_LLR, decreasing = TRUE))
      data_ds$Cumulative_Prop <- seq_along(data_ds$LLR) / length(data_ds$LLR)
      
      multi_plot <- multi_plot + geom_line(data = data_ss,
                                           aes(x = LLR, y = Cumulative_Prop),
                                           color = "red", linetype = line_type,
                                           linewidth = 0.8, na.rm = TRUE)
      multi_plot <- multi_plot + geom_line(data = data_ds,
                                           aes(x = LLR, y = Cumulative_Prop),
                                           color = "blue", linetype = line_type,
                                           linewidth = 0.8, na.rm = TRUE)
      if (!is.null(E_value) && !is.na(E_value) && E_value > 0) {
        multi_plot <- multi_plot + geom_vline(xintercept = log10(E_value),
                                              color = "darkgreen", linetype = line_type,
                                              linewidth = 0.8)
      }
    }
    return(multi_plot)
  })
  
  output$multi_tippettPlot <- renderPlot({
    print(multi_plot_reactive())
  })
  
  output$multi_metrics <- renderPrint({
    
    multi_data <- multi_analysis_data()
    
    n_files <- length(multi_data)
    for(i in 1:n_files){
      
      one_data <- multi_data[[i]]
      if (is.null(one_data)) next
      
      lr_values <- one_data$lr_values
      labels <- one_data$labels
      
      if (input$multi_scale == "Raw") {
        llr_values <- log(lr_values)
      } else if (input$multi_scale == "log10(LR)") {
        llr_values <- lr_values * log(10)
      } else if (input$multi_scale == "ln(LR)") {
        llr_values <- lr_values
      } # scale transformation (to natural-log-scale)
      
      ss_LLR <- llr_values[labels == SS_LABEL]
      ds_LLR <- llr_values[labels == DS_LABEL]
      
      cllr_pooled <- cllr(ss_LLR, ds_LLR)
      cllr_min <- cllr_min(ss_LLR, ds_LLR)
      cllr_cal <- cllr_cal(ss_LLR, ds_LLR)
      eer_result <- eer(ss_LLR, ds_LLR)
      
      cat("System", i, ":", one_data$file_name, "\n")
      cat("  Cllr (pooled):", cllr_pooled, "\n")
      cat("  Cllr (min):", cllr_min, "\n")
      cat("  Cllr (cal):", cllr_cal, "\n")
      cat("  EER:", eer_result$EER, "\n")
      cat("  EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("  EER Threshold (raw):", eer_result$threshold_raw, "\n\n")
    }
  })
  
  output$multi_downloadPlot <- downloadHandler(
    filename = function() {
      paste0("multi_tippett_plot_", Sys.Date(), ".", input$multi_fig_down)
    },
    content = function(file) {
      plot_obj <- multi_plot_reactive()
      file <- as.character(file)
      
      if (input$multi_fig_down == "png") {
        ggsave(file, plot = plot_obj, device = "png", width = 8, height = 6, dpi = 1200)
      } else if (input$multi_fig_down == "pdf") {
        ggsave(file, plot = plot_obj, device = "pdf", width = 8, height = 6)
      } else if (input$multi_fig_down == "svg") {
        ggsave(file, plot = plot_obj, device = "svg", width = 8, height = 6)
      }
    }
  )
  # ============================================================================
  
  # ======================== Precision Analysis (server) =======================
  precision_data <- reactive({
    req(input$precision_data_file)
    read.csv(input$precision_data_file$datapath, stringsAsFactors = FALSE)
  })
  
  output$precision_id1_col_select <- renderUI({
    req(precision_data())
    selectInput("precision_id1_col", "Select ID-1 Column", choices = names(precision_data()))
  })
  
  output$precision_id2_col_select <- renderUI({
    req(precision_data())
    selectInput("precision_id2_col", "Select ID-2 Column", choices = names(precision_data()))
  })
  
  output$precision_lr_col_select <- renderUI({
    req(precision_data())
    selectInput("precision_lr_col", "Select LR Column", choices = names(precision_data()))
  })
  
  ci_input_df <- eventReactive(input$start_analysis, {
    req(precision_data(),
        input$precision_id1_col, input$precision_id2_col,
        input$precision_lr_col, input$precision_scale)
    df <- precision_data()
    
    id1 <- as.character(df[[input$precision_id1_col]])
    id2 <- as.character(df[[input$precision_id2_col]])
    lr  <- suppressWarnings(as.numeric(df[[input$precision_lr_col]]))
    
    if (input$precision_scale == "Raw") {
      log10LR <- log10(lr)
    } else if (input$precision_scale == "log10(LR)") {
      log10LR <- lr
    } else if (input$precision_scale == "ln(LR)") {
      log10LR <- lr / log(10)
    } # scale transformation (to log10-scale)
    
    data.frame(
      id_1 = id1,
      id_2 = id2,
      log10LR = log10LR,
      stringsAsFactors = FALSE
    )
  })
  
  ci_res <- eventReactive(input$start_analysis, {
    req(ci_input_df())
    CI_para(df = ci_input_df(),
            symmetric_trial = input$precision_symmetric_trial,
            SS_LABEL = SS_LABEL,
            DS_LABEL = DS_LABEL)
  })
  
  # Performance metrics
  output$precision_metrics <- renderPrint({
    req(ci_res())
    res <- ci_res()
    cat("Cllr (mean):", res$cllr_mean, "\n")
    cat("±95% CI (log10):", res$CI_half_log10, "\n")
  })
  
  # Precision results
  output$precision_stat <- renderDT({
    req(ci_res())
    datatable(
      ci_res()$result,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        ordering = TRUE,
        dom = 'tip')
    )
  })
  
  # Tippett Plot (with precision)
  precision_plot_reactive <- reactive({
    req(ci_res())
    result <- ci_res()$result
    
    ss_stat <- subset(result, label == SS_LABEL)
    ds_stat <- subset(result, label == DS_LABEL)
    
    ss_log10LR <- data.frame(
      lg_LR = sort(ss_stat$log10LR_mean),
      Cumulative_Prop = seq_len(nrow(ss_stat)) / nrow(ss_stat)
    )
    ss_CI_lower <- data.frame(
      lg_LR = sort(ss_stat$CI_lower),
      Cumulative_Prop = seq_len(nrow(ss_stat)) / nrow(ss_stat)
    )
    ss_CI_upper <- data.frame(
      lg_LR = sort(ss_stat$CI_upper),
      Cumulative_Prop = seq_len(nrow(ss_stat)) / nrow(ss_stat)
    )
    
    ds_log10LR <- data.frame(
      lg_LR = sort(ds_stat$log10LR_mean, decreasing = TRUE),
      Cumulative_Prop = seq_len(nrow(ds_stat)) / nrow(ds_stat)
    )
    ds_CI_lower <- data.frame(
      lg_LR = sort(ds_stat$CI_lower, decreasing = TRUE),
      Cumulative_Prop = seq_len(nrow(ds_stat)) / nrow(ds_stat)
    )
    ds_CI_upper <- data.frame(
      lg_LR = sort(ds_stat$CI_upper, decreasing = TRUE),
      Cumulative_Prop = seq_len(nrow(ds_stat)) / nrow(ds_stat)
    )
    
    precision_plot <- ggplot() +
      geom_line(data = ss_log10LR, aes(x = lg_LR, y = Cumulative_Prop), color = "red",
                linewidth = 0.8, na.rm = TRUE) +
      geom_line(data = ss_CI_lower, aes(x = lg_LR, y = Cumulative_Prop), color = "red",
                linewidth = 0.8, linetype = "dotted", na.rm = TRUE) +
      geom_line(data = ss_CI_upper, aes(x = lg_LR, y = Cumulative_Prop), color = "red",
                linewidth = 0.8, linetype = "dotted", na.rm = TRUE) +
      
      geom_line(data = ds_log10LR, aes(x = lg_LR, y = Cumulative_Prop), color = "blue",
                linewidth = 0.8, na.rm = TRUE) +
      geom_line(data = ds_CI_lower, aes(x = lg_LR, y = Cumulative_Prop), color = "blue",
                linewidth = 0.8, linetype = "dotted", na.rm = TRUE) +
      geom_line(data = ds_CI_upper, aes(x = lg_LR, y = Cumulative_Prop), color = "blue",
                linewidth = 0.8, linetype = "dotted", na.rm = TRUE) +
      
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      labs(x = expression(log[10](Lambda)),
           y = "cumulative proportion") +
      scale_x_continuous(
        expand = c(0, 0),
        breaks = pretty(c(input$precision_x_min, input$precision_x_max), n = 8)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(input$precision_y_min, input$precision_y_max, length.out = 11)
      ) +
      coord_cartesian(
        xlim = c(input$precision_x_min, input$precision_x_max),
        ylim = c(input$precision_y_min, input$precision_y_max)
      ) +
      theme_minimal(base_size = input$precision_font_size) +
      theme(
        legend.position = "none",
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 1),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color = "black", linewidth = 0.2),
        axis.text = element_text(color = "black", size = input$precision_font_size),
        axis.title = element_text(color = "black", size = input$precision_font_size),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
      )
    
    # Evidence mark
    if (!is.null(input$precision_E) && !is.na(input$precision_E) && input$precision_E > 0) {
      precision_plot <- precision_plot +
        geom_vline(xintercept = log10(input$precision_E), color = "darkgreen",
                   linewidth = 0.8, linetype = "solid")
    }
    
    return(precision_plot)
  })
  
  
  output$precision_plot <- renderPlot({
    print(precision_plot_reactive())
  })
  
  output$precision_downloadPlot <- downloadHandler(
    filename = function() {
      paste0("precision_tippett_plot_", Sys.Date(), ".", input$precision_fig_down)
    },
    content = function(file) {
      plot_obj <- precision_plot_reactive()
      file <- as.character(file)
      
      if (input$precision_fig_down == "png") {
        ggsave(file, plot = plot_obj, device = "png", width = 8, height = 6, dpi = 1200)
      } else if (input$precision_fig_down == "pdf") {
        ggsave(file, plot = plot_obj, device = "pdf", width = 8, height = 6)
      } else if (input$precision_fig_down == "svg") {
        ggsave(file, plot = plot_obj, device = "svg", width = 8, height = 6)
      }
    }
  )
  
  output$precision_downloadData <- downloadHandler(
    filename = function() {
      paste("precision_analysis_data(", Sys.Date(), ").csv", sep = "")
    },
    content = function(file) {
      req(ci_res())
      write.csv(ci_res()$result, file, row.names = FALSE)
    }
  )
  # ============================================================================
}