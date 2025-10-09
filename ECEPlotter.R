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

source("metric/Cllr.R")
source("metric/ECE.R")
source("metric/EER.R")

# Shiny UI
ui <- fluidPage(
  titlePanel(
    tagList(
      "Empirical Cross-Entropy Plot Generator",
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
          fluidRow(
            column(6, numericInput("single_x_min", "X-axis min", value = -5)),
            column(6, numericInput("single_x_max", "X-axis max", value = 5))
          ),
          fluidRow(
            column(6, numericInput("single_y_min", "Y-axis min", value = 0)),
            column(6, numericInput("single_y_max", "Y-axis max", value = 0.5))
          ),
          checkboxInput("show_legend", "Show Legend", value = TRUE),
          numericInput("single_font_size", "Font Size", value = 14, min = 6, max = 24),
          selectInput("single_fig_down", "Download Format", 
                      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"), 
                      selected = "png"),
          downloadButton("single_downloadPlot", "Download Plot")
        ),
        mainPanel(
          h5("Data Preview:"),
          DTOutput("single_data_preview"),
          h5("ECE Plot:"),
          plotOutput("single_ecePlot"),
          h5("Performance Metrics:"),
          verbatimTextOutput("single_metrics")
        ),
      )
    )
    # ==========================================================================
    
  )
)

# Shiny server
server <- function(input, output, session){
  
  # ====================== Single System Evaluation (server) ===================
  single_data <- reactive({
    req(input$single_data_file)
    read.csv(input$single_data_file$datapath, stringsAsFactors = FALSE)
  })
  
  output$single_data_preview <- renderDT({
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
  
  output$single_label_col_select <- renderUI({
    req(single_data())
    selectInput("single_label_col", "Select Label Column",
                choices = names(single_data()))
  }) # dynamic module
  
  output$single_lr_col_select <- renderUI({
    req(single_data())
    selectInput("single_lr_col", "Select LR Column",
                choices = names(single_data()))
  }) # dynamic module
  
  single_plot_reactive <- reactive({
    
    req(input$single_label_col, input$single_lr_col)
    lr_values <- as.numeric(single_data()[[input$single_lr_col]])
    
    if (input$single_scale == "Raw") {
      lr_values <- lr_values
    } else if (input$single_scale == "log10(LR)") {
      lr_values <- 10^lr_values
    } else if (input$single_scale == "ln(LR)") {
      lr_values <- exp(lr_values)
    } # scale transformation (to raw-scale)
    
    labels <- single_data()[[input$single_label_col]]
    if (!(SS_LABEL %in% labels) || !(DS_LABEL %in% labels)) {
      showNotification(
        paste0("Error: Label column must contain both '", SS_LABEL, 
               "' and '", DS_LABEL, "' values."),
        type = "error", duration = 8
      )
      validate(need(FALSE, "Invalid label configuration"))
    }
    
    log10_priors <- seq(from = input$single_x_min - 5,
                        to = input$single_x_max + 5, length.out = 1000)
    
    ss_LR <- lr_values[labels == SS_LABEL]
    ds_LR <- lr_values[labels == DS_LABEL]
    
    ss_LR_base <- rep(1, length(ss_LR))
    ds_LR_base <- rep(1, length(ds_LR))
    
    opt_res <- opt_loglr(tar_scores = log(ss_LR),
                         nontar_scores = log(ds_LR), option = "raw")
    
    ss_LR_pav <- exp(opt_res$tar_llrs)
    ds_LR_pav <- exp(opt_res$nontar_llrs)
    
    ece_sys  <- sapply(log10_priors, function(p) ECE(p, ss_LR, ds_LR))
    ece_pav  <- sapply(log10_priors, function(p) ECE(p, ss_LR_pav, ds_LR_pav))
    ece_base <- sapply(log10_priors, function(p) ECE(p, ss_LR_base, ds_LR_base))
    
    ece_df <- data.frame(
      log10_prior = rep(log10_priors, 3),
      ECE = c(ece_pav, ece_sys, ece_base),
      Type = factor(rep(c("after PAV", "LR values", "LR = 1"), each = length(log10_priors)),
                    levels = c("LR = 1", "LR values", "after PAV"))
    )
    
    single_plot <- ggplot(ece_df, aes(x = log10_prior,
                                      y = ECE,
                                      color = Type, linetype = Type)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      labs(x = "Log10 Prior Odds", y = "Empirical Cross-entropy Value") +
      scale_color_manual(values = c(
        "after PAV" = "blue",
        "LR values" = "red",
        "LR = 1" = "black"
      )) +
      scale_linetype_manual(values = c(
        "after PAV" = "dashed",
        "LR values" = "solid",
        "LR = 1" = "dotted"
      )) +
      guides(
        color = guide_legend(keywidth = unit(1.2, "cm")),
        linetype = guide_legend(keywidth = unit(1.2, "cm"))
      ) +
      scale_x_continuous(
        limits = c(input$single_x_min, input$single_x_max),
        expand = c(0, 0),
        breaks = pretty(c(input$single_x_min, input$single_x_max), n = 11)
      ) +
      scale_y_continuous(
        limits = c(input$single_y_min, input$single_y_max),
        expand = c(0, 0),
        breaks = seq(input$single_y_min, input$single_y_max, length.out = 11)
      ) +
      theme_minimal(base_size = input$single_font_size) +
      theme(
        panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 5),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color = "black", linewidth = 0.2),
        axis.text = element_text(color = "black", size = input$single_font_size),
        axis.title = element_text(color = "black", size = input$single_font_size),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.position = if (input$show_legend) c(0.98, 0.98) else "none",
        legend.key.height = unit(0.5, "cm"),
        legend.spacing.y = unit(0.75, "cm"),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha("white", 0.6), color = "grey40"),
        legend.title = element_blank(),
        legend.text = element_text(size = input$single_font_size * 0.9)
      )
    
    return(single_plot)
  })
  
  output$single_ecePlot <- renderPlot({
    print(single_plot_reactive())
  }) # print the ECE plot
  
  output$single_downloadPlot <- downloadHandler(
    filename = function() {
      paste0("ECE_plot_", Sys.Date(), ".", input$single_fig_down)
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
  
  output$single_ecePlot <- renderPlot({
    print(single_plot_reactive())
  }) # print the ECE plot
  
  output$single_metrics <- renderPrint({
    
    req(input$single_label_col, input$single_lr_col)
    lr_values <- as.numeric(single_data()[[input$single_lr_col]])
    
    if (input$single_scale == "Raw") {
      lr_values <- lr_values
    } else if (input$single_scale == "log10(LR)") {
      lr_values <- 10^(lr_values)
    } else if (input$single_scale == "ln(LR)") {
      lr_values <- exp(lr_values)
    } # scale transformation (to Raw-scale)
    
    labels <- single_data()[[input$single_label_col]]
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
  # ==========================================================================
  
}

# Shiny application
options(shiny.maxRequestSize = 300*1024^2)
shinyApp(ui = ui, server = server)
