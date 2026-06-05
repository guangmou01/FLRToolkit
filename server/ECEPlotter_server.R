# Path: ""server/ECEPlotter_server.R""

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
      llr_values <- log(lr_values)
    } else if (input$single_scale == "log10(LR)") {
      llr_values <- lr_values * log(10)
    } else if (input$single_scale == "ln(LR)") {
      llr_values <- lr_values
    } # scale transformation (to natural-log-scale)
    
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
    
    ss_LLR <- llr_values[labels == SS_LABEL]
    ds_LLR <- llr_values[labels == DS_LABEL]
    
    ss_LLR_base <- rep(0, length(ss_LLR))
    ds_LLR_base <- rep(0, length(ds_LLR))
    
    opt_res <- opt_loglr(tar_scores = ss_LLR,
                         nontar_scores = ds_LLR, option = "raw")
    
    ss_LLR_pav <- opt_res$tar_llrs
    ds_LLR_pav <- opt_res$nontar_llrs
    
    ece_sys  <- sapply(log10_priors, function(p) ECE(p, ss_LLR, ds_LLR))
    ece_pav  <- sapply(log10_priors, function(p) ECE(p, ss_LLR_pav, ds_LLR_pav))
    ece_base <- sapply(log10_priors, function(p) ECE(p, ss_LLR_base, ds_LLR_base))
    
    ece_df <- data.frame(
      log10_prior = rep(log10_priors, 3),
      ECE = c(ece_pav, ece_sys, ece_base),
      Type = factor(rep(c("after PAV", "LR outputs", "LR = 1"), each = length(log10_priors)),
                    levels = c("LR = 1", "LR outputs", "after PAV"))
    )
    
    single_plot <- ggplot(ece_df, aes(x = log10_prior,
                                      y = ECE,
                                      color = Type, linetype = Type)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      labs(x = expression(log[10]~"prior odds"),
           y = "empirical cross-entropy value") +
      scale_color_manual(values = c(
        "after PAV" = "blue",
        "LR outputs" = "red",
        "LR = 1" = "black"
      )) +
      scale_linetype_manual(values = c(
        "after PAV" = "dashed",
        "LR outputs" = "solid",
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
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.position = if (input$show_legend) c(0.98, 0.98) else "none",
        legend.key.height = unit(0.5, "cm"),
        legend.spacing.y = unit(0.75, "cm"),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha("white", 0.6), color = "grey40"),
        legend.title = element_blank(),
        legend.text = element_text(size = input$single_font_size * 0.8)
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
      llr_values <- log(lr_values)
    } else if (input$single_scale == "log10(LR)") {
      llr_values <- lr_values * log(10)
    } else if (input$single_scale == "ln(LR)") {
      llr_values <- lr_values
    } # scale transformation (to natural-log-scale)
    
    labels <- single_data()[[input$single_label_col]]
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
  # ==========================================================================
  
}