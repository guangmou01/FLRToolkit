# ------------------------------------------------------------------------------
# Updated: October 7, 2025
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------
APP_VERSION <- "Version 2.2.0"
SS_LABEL <- "ss"
DS_LABEL <- "ds"

if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("DT")) install.packages("DT", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

library(shiny)
library(DT)
library(ggplot2)

source("metric/Cllr.R")
source("metric/EER.R")
source("metric/CI.R")

# Shiny UI

ui <- fluidPage(
  titlePanel(
    tagList(
      "Tippett Plot Generator",
      tags$span(APP_VERSION,
                style = "font-size: 16px; color: gray; margin-left: 12px;")
    )
  ),
  tabsetPanel(
    
    # ====================== Single System Evaluation (UI) =====================
    tabPanel("Single System Evaluation",
             sidebarLayout(
               sidebarPanel(
                 fileInput("single_data_file", "Upload Data File ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("single_label_col_select"),
                 uiOutput("single_lr_col_select"),
                 hr(),
                 selectInput("single_scale", "Choose LR Scale",
                             choices = c("Raw", "log10(LR)", "ln(LR)"), 
                             selected = "Raw"),
                 numericInput("single_E", "Evidence LR Value ( raw )", value = NULL, min = 0, step = 0.1),
                 fluidRow(
                   column(6, numericInput("single_x_min", "X-axis min", value = -5)),
                   column(6, numericInput("single_x_max", "X-axis max", value = 5))
                 ),
                 fluidRow(
                   column(6, numericInput("single_y_min", "Y-axis min", value = 0)),
                   column(6, numericInput("single_y_max", "Y-axis max", value = 1))
                 ),
                 numericInput("single_font_size", "Font Size", value = 14, min = 6, max = 24),
                 selectInput("single_fig_down", "Download Format", 
                             choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"), 
                             selected = "png"),
                 downloadButton("single_downloadPlot", "Download Plot")
               ),
               mainPanel(
                 h5("Tippett Plot:"),
                 plotOutput("single_tippettPlot"),
                 h5("Performance Metrics:"),
                 verbatimTextOutput("single_metrics")
               ),
             )),
    # ==========================================================================
    
    # ======================= Multi-system Comparison (UI) =====================
    tabPanel("Multi-system Comparison",
      sidebarLayout(
        sidebarPanel(
          fileInput("multi_data_file", "Upload Multiple Data Files ( .csv )", 
                    accept = ".csv", multiple = TRUE),
          uiOutput("multi_file_options"),
          hr(),
          selectInput("multi_scale", "Choose LR Scale",
                      choices = c("Raw", "log10(LR)", "ln(LR)"), 
                      selected = "Raw"),
          fluidRow(
            column(6, numericInput("multi_x_min", "X-axis min", value = -5)),
            column(6, numericInput("multi_x_max", "X-axis max", value = 5))
          ),
          fluidRow(
            column(6, numericInput("multi_y_min", "Y-axis min", value = 0)),
            column(6, numericInput("multi_y_max", "Y-axis max", value = 1))
          ),
          numericInput("multi_font_size", "Font Size", value = 14, min = 6, max = 24),
          selectInput("multi_fig_down", "Download Format", 
                      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"), 
                      selected = "png"),
          downloadButton("multi_downloadPlot", "Download Plot")
        ),
        mainPanel(
          h5("Multi-Tippett Plot:"),
          plotOutput("multi_tippettPlot"),
          h5("Performance Metrics:"),
          verbatimTextOutput("multi_metrics")
        )
      )
    ),
    # ==========================================================================
    
    # ========================= Precision Analysis (UI) ========================
    tabPanel("Precision Analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("precision_data_file", "Upload Data File ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("precision_id1_col_select"),
                 uiOutput("precision_id2_col_select"),
                 uiOutput("precision_lr_col_select"),
                 hr(),
                 selectInput("precision_scale", "Choose LR Scale",
                             choices = c("Raw", "log10(LR)", "ln(LR)"), 
                             selected = "Raw"),
                 numericInput("precision_E", "Evidence LR Value ( raw )", value = NULL, min = 0, step = 0.1),
                 actionButton("start_analysis", "Perform Analysis", class = "btn-primary"),
                 hr(),
                 fluidRow(
                   column(6, numericInput("precision_x_min", "X-axis min", value = -5)),
                   column(6, numericInput("precision_x_max", "X-axis max", value = 5))
                 ),
                 fluidRow(
                   column(6, numericInput("precision_y_min", "Y-axis min", value = 0)),
                   column(6, numericInput("precision_y_max", "Y-axis max", value = 1))
                 ),
                 numericInput("precision_font_size", "Font Size", value = 14, min = 6, max = 24),
                 selectInput("precision_fig_down", "Download Format", 
                             choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"), 
                             selected = "png"),
                 downloadButton("precision_downloadPlot", "Download Plot"),
                 downloadButton("precision_downloadData", "Download Precision Data")
               ),
               mainPanel(
                 h5("Tippett Plot ( with precision ):"),
                 plotOutput("precision_plot"),
                 h5("Performance Metrics:"),
                 verbatimTextOutput("precision_metrics"),
                 h5("Precision Analysis Data:"),
                 DTOutput("precision_stat")
               )
             ))
    # ==========================================================================
  )
)

# Shiny server

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
  
  single_plot_reactive <- reactive({
    
    req(input$single_label_col, input$single_lr_col)
    lr_values <- as.numeric(data()[[input$single_lr_col]])
    
    if (input$single_scale == "Raw") {
      llr_values <- log10(lr_values)
    } else if (input$single_scale == "log10(LR)") {
      llr_values <- lr_values
    } else if (input$single_scale == "ln(LR)") {
      llr_values <- lr_values/log(10)
    } # scale transformation (to log10-scale)
    
    labels <- data()[[input$single_label_col]]
    if (!(SS_LABEL %in% labels) || !(DS_LABEL %in% labels)) {
      showNotification(
        paste0("Error: Label column must contain both '", SS_LABEL, 
               "' and '", DS_LABEL, "' values."),
        type = "error", duration = 8
      )
      validate(need(FALSE, "Invalid label configuration"))
    }
    
    ss_LLR <- llr_values[labels == SS_LABEL]
    ds_LLR <- llr_values[labels == DS_LABEL]
    
    data_ss <- data.frame(LLR = sort(ss_LLR))
    data_ss$Cumulative_Prop <- seq_along(data_ss$LLR) / length(data_ss$LLR)
    data_ds <- data.frame(LLR = sort(ds_LLR, decreasing = TRUE))
    data_ds$Cumulative_Prop <- seq_along(data_ds$LLR) / length(data_ds$LLR)
    
    single_plot <- ggplot() +
      geom_line(data = data_ss, aes(x = LLR, y = Cumulative_Prop), color = "red") +
      geom_line(data = data_ds, aes(x = LLR, y = Cumulative_Prop), color = "blue") +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      labs(x = "Log10 Likelihood Ratio", y = "Cumulative Proportion") +
      scale_x_continuous(
        limits = c(input$single_x_min, input$single_x_max),
        expand = c(0, 0),
        breaks = pretty(c(input$single_x_min, input$single_x_max), n = 8)
      ) +
      scale_y_continuous(
        limits = c(input$single_y_min, input$single_y_max),
        expand = c(0, 0),
        breaks = seq(input$single_y_min, input$single_y_max, length.out = 11)
      ) +
      theme_minimal(base_size = input$single_font_size) +
      theme(legend.position = "none",
            panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 5),
            panel.grid.minor = element_blank(),
            axis.ticks.length = unit(-0.2, "cm"),
            axis.ticks = element_line(color = "black", linewidth = 0.2),
            axis.text = element_text(color = "black", size = input$single_font_size),
            axis.title = element_text(color = "black", size = input$single_font_size),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
    
    # Evidence mark
    if (!is.null(input$single_E) && !is.na(input$single_E) && input$single_E > 0) {
      single_plot <- single_plot + geom_vline(xintercept = log10(input$single_E),
                                              color = "darkgreen",
                                              linetype = "solid")
    }
    
    return(single_plot) # return the final plot
  })
  
  output$single_tippettPlot <- renderPlot({
    print(single_plot_reactive())
  }) # print the tippett plot
  
  output$single_metrics <- renderPrint({
    
    req(input$single_label_col, input$single_lr_col)
    lr_values <- as.numeric(data()[[input$single_lr_col]])
    
    if (input$single_scale == "Raw") {
      lr_values <- lr_values
    } else if (input$single_scale == "log10(LR)") {
      lr_values <- 10^(lr_values)
    } else if (input$single_scale == "ln(LR)") {
      lr_values <- exp(lr_values)
    } # scale transformation (to Raw-scale)
    
    labels <- data()[[input$single_label_col]]
    if (!(SS_LABEL %in% labels) || !(DS_LABEL %in% labels)) {
      showNotification(
        paste0("Error: Label column must contain both '", SS_LABEL, 
               "' and '", DS_LABEL, "' values."),
        type = "error", duration = 8
      )
      validate(need(FALSE, "Invalid label configuration"))
    }
    
    ss_LR <- lr_values[labels == SS_LABEL]
    ds_LR <- lr_values[labels == DS_LABEL]
    
    cllr_pooled <- Cllr(ss_LR, ds_LR)
    cllr_min <- Cllr_min(ss_LR, ds_LR)
    cllr_cal <- Cllr_cal(ss_LR, ds_LR)
    eer_result <- EER(ss_LR, ds_LR)
    
    cat("Cllr (pooled):", cllr_pooled, "\n")
    cat("Cllr (min):", cllr_min, "\n")
    cat("Cllr (cal):", cllr_cal, "\n")
    cat("EER:", eer_result$EER, "\n")
    cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
    cat("EER Threshold (raw):", eer_result$threshold_Raw, "\n")
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
                    label = "Select Label Column ( labeled as ‘ss’ / ‘ds’ )",
                    choices = names(df)),
        selectInput(inputId = paste0("multi_lr_col_", i),
                    label = "Select LR Column",
                    choices = names(df)),
        selectInput(inputId = paste0("multi_line_type_", i),
                    label = "Select Line Type",
                    choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                    selected = ifelse(i %% 2 == 1, "solid", "dashed")),
        numericInput(inputId = paste0("multi_E_", i),
                     label = "Evidence LR Value ( raw )", value = NULL, min = 0, step = 0.1)
      )
    })
    do.call(tagList, file_options)
  }) # dynamic module
  
  # Construct muti-tippett plot
  multi_plot_reactive <- reactive({
    req(input$multi_data_file)
    multi_dfs <- multi_data_list()
    
    multi_plot <- ggplot() +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      labs(x = "Log10 Likelihood Ratio", y = "Cumulative Proportion") +
      scale_x_continuous(
        limits = c(input$multi_x_min, input$multi_x_max),
        expand = c(0, 0),
        breaks = pretty(c(input$multi_x_min, input$multi_x_max), n = 8)
      ) +
      scale_y_continuous(
        limits = c(input$multi_y_min, input$multi_y_max),
        expand = c(0, 0),
        breaks = seq(input$multi_y_min, input$multi_y_max, length.out = 11)
      ) +
      theme_minimal(base_size = input$multi_font_size) +
      theme(legend.position = "none",
            panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 5),
            panel.grid.minor = element_blank(),
            axis.ticks.length = unit(-0.2, "cm"),
            axis.ticks = element_line(color = "black", linewidth = 0.2),
            axis.text = element_text(color = "black", size = input$multi_font_size),
            axis.title = element_text(color = "black", size = input$multi_font_size),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
    
    n_files <- length(multi_dfs)
    for (i in 1:n_files) {
      df <- multi_dfs[[i]]
      label_col <- input[[paste0("multi_label_col_", i)]]
      lr_col <- input[[paste0("multi_lr_col_", i)]]
      line_type <- input[[paste0("multi_line_type_", i)]]
      E_value <- input[[paste0("multi_E_", i)]]
      
      if(is.null(label_col) || is.null(lr_col)) next
      
      lr_values <- as.numeric(df[[lr_col]])
      
      if (input$multi_scale == "Raw") {
        llr_values <- log10(lr_values)
      } else if (input$multi_scale == "log10(LR)") {
        llr_values <- lr_values
      } else if (input$multi_scale == "ln(LR)") {
        llr_values <- lr_values/log(10)
      } # scale transformation (to log10-scale)
      
      labels <- df[[label_col]]
      if (!(SS_LABEL %in% labels) || !(DS_LABEL %in% labels)) {
        showNotification(
          paste0("Error in ", input$multi_data_file$name[i], 
                 ": Label column must contain both '", SS_LABEL, 
                 "' and '", DS_LABEL, "' values."),
          type = "error", duration = 8
        )
        next
      }
      
      ss_LLR <- llr_values[labels == SS_LABEL]
      ds_LLR <- llr_values[labels == DS_LABEL]
      
      data_ss <- data.frame(LLR = sort(ss_LLR))
      data_ss$Cumulative_Prop <- seq_along(data_ss$LLR) / length(data_ss$LLR)
      data_ds <- data.frame(LLR = sort(ds_LLR, decreasing = TRUE))
      data_ds$Cumulative_Prop <- seq_along(data_ds$LLR) / length(data_ds$LLR)
      
      multi_plot <- multi_plot + geom_line(data = data_ss,
                                           aes(x = LLR, y = Cumulative_Prop),
                                           color = "red", linetype = line_type)
      multi_plot <- multi_plot + geom_line(data = data_ds,
                                           aes(x = LLR, y = Cumulative_Prop),
                                           color = "blue", linetype = line_type)
      if (!is.null(E_value) && !is.na(E_value) && E_value > 0) {
        multi_plot <- multi_plot + geom_vline(xintercept = log10(E_value),
                                              color = "darkgreen", linetype = line_type)
        }
      }
    return(multi_plot)
  })
  
  output$multi_tippettPlot <- renderPlot({
    print(multi_plot_reactive())
  })
  
  output$multi_metrics <- renderPrint({
    req(input$multi_data_file)
    multi_dfs <- multi_data_list()
    
    n_files <- length(multi_dfs)
    for(i in 1:n_files){
      df <- multi_dfs[[i]]
      label_col <- input[[paste0("multi_label_col_", i)]]
      lr_col <- input[[paste0("multi_lr_col_", i)]]
      if(is.null(label_col) || is.null(lr_col)) next
      lr_values <- as.numeric(df[[lr_col]])
      
      if (input$multi_scale == "Raw") {
        lr_values <- lr_values
      } else if (input$multi_scale == "log10(LR)") {
        lr_values <- 10^(lr_values)
      } else if (input$multi_scale == "ln(LR)") {
        lr_values <- exp(lr_values)
      } # scale transformation (to Raw-scale)
      
      labels <- df[[label_col]]
      if (!(SS_LABEL %in% labels) || !(DS_LABEL %in% labels)) {
        showNotification(
          paste0("Error in ", input$multi_data_file$name[i], 
                 ": Label column must contain both '", SS_LABEL, 
                 "' and '", DS_LABEL, "' values."),
          type = "error", duration = 8
        )
        next
      }
      
      ss_LR <- lr_values[labels == SS_LABEL]
      ds_LR <- lr_values[labels == DS_LABEL]
      
      cllr_pooled <- Cllr(ss_LR, ds_LR)
      cllr_min <- Cllr_min(ss_LR, ds_LR)
      cllr_cal <- Cllr_cal(ss_LR, ds_LR)
      eer_result <- EER(ss_LR, ds_LR)
      
      cat("System", i, ":", input$multi_data_file$name[i], "\n")
      cat("  Cllr (pooled):", cllr_pooled, "\n")
      cat("  Cllr (min):", cllr_min, "\n")
      cat("  Cllr (cal):", cllr_cal, "\n")
      cat("  EER:", eer_result$EER, "\n")
      cat("  EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("  EER Threshold (raw):", eer_result$threshold_Raw, "\n\n")
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
    req(precision_data(), input$precision_id1_col, input$precision_id2_col, input$precision_lr_col, input$precision_scale)
    df <- precision_data()
    
    id1 <- as.character(df[[input$precision_id1_col]])
    id2 <- as.character(df[[input$precision_id2_col]])
    lr  <- suppressWarnings(as.numeric(df[[input$precision_lr_col]]))
    
    if (input$precision_scale == "Raw") {
      Log10LR <- log10(lr)
    } else if (input$precision_scale == "log10(LR)") {
      Log10LR <- lr
    } else if (input$precision_scale == "ln(LR)") {
      Log10LR <- lr / log(10)
    } # scale transformation (to log10-scale)
    
    data.frame(
      id_1 = id1,
      id_2 = id2,
      Log10LR = Log10LR,
      stringsAsFactors = FALSE
    )
  })
  
  ci_res <- eventReactive(input$start_analysis, {
    req(ci_input_df())
    CI_para(ci_input_df())
  })
  
  # Performance metrics
  output$precision_metrics <- renderPrint({
    req(ci_res())
    res <- ci_res()
    cat("Cllr (mean):", res$Cllr_mean, "\n")
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
    
    ss_stat <- subset(result, label == "ss")
    ds_stat <- subset(result, label == "ds")
    
    ss_log10LR <- data.frame(
      lg_LR = sort(ss_stat$Log10LR_mean),
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
      lg_LR = sort(ds_stat$Log10LR_mean, decreasing = TRUE),
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
      geom_line(data = ss_log10LR, aes(x = lg_LR, y = Cumulative_Prop), color = "red") +
      geom_line(data = ss_CI_lower, aes(x = lg_LR, y = Cumulative_Prop), color = "red", linetype = "dotted") +
      geom_line(data = ss_CI_upper, aes(x = lg_LR, y = Cumulative_Prop), color = "red", linetype = "dotted") +
      
      geom_line(data = ds_log10LR, aes(x = lg_LR, y = Cumulative_Prop), color = "blue") +
      geom_line(data = ds_CI_lower, aes(x = lg_LR, y = Cumulative_Prop), color = "blue", linetype = "dotted") +
      geom_line(data = ds_CI_upper, aes(x = lg_LR, y = Cumulative_Prop), color = "blue", linetype = "dotted") +
      
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      labs(x = "Log10 Likelihood Ratio", y = "Cumulative Proportion") +
      scale_x_continuous(
        limits = c(input$precision_x_min, input$precision_x_max),
        expand = c(0, 0),
        breaks = pretty(c(input$precision_x_min, input$precision_x_max), n = 8)
      ) +
      scale_y_continuous(
        limits = c(input$precision_y_min, input$precision_y_max),
        expand = c(0, 0),
        breaks = seq(input$precision_y_min, input$precision_y_max, length.out = 11)
      ) +
      theme_minimal(base_size = input$precision_font_size) +
      theme(
        legend.position = "none",
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 5),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color = "black", linewidth = 0.2),
        axis.text = element_text(color = "black", size = input$precision_font_size),
        axis.title = element_text(color = "black", size = input$precision_font_size),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
      )
    
    # Evidence mark
    if (!is.null(input$precision_E) && !is.na(input$precision_E) && input$precision_E > 0) {
      precision_plot <- precision_plot +
        geom_vline(xintercept = log10(input$precision_E), color = "darkgreen", linetype = "solid")
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

# Shiny application
options(shiny.maxRequestSize = 300*1024^2)
shinyApp(ui = ui, server = server)

