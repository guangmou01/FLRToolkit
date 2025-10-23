# ------------------------------------------------------------------------------
# Updated: October 9, 2025
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------
APP_VERSION <- "Version 7.0.0"

if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("DT")) install.packages("DT", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

library(shiny)
library(DT)
library(ggplot2)

source("func/MVKD.R")
source("metric/Cllr.R")
source("metric/EER.R")
source("metric/TippettPlot.R")

ui <- fluidPage(
  titlePanel(
    tagList(
      "MVKD-based Likelihood Ratio",
      tags$span(APP_VERSION,
                style = "font-size: 16px; color: gray; margin-left: 12px;")
    )
  ),
  tabsetPanel(
    
    # ====================== MVKD-based LR Calculator (UI) =====================
    tabPanel("MVKD-based LR Calculator",
             sidebarLayout(
               sidebarPanel(
                 fileInput("single_off_df", "Upload Q-item Data ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 fileInput("single_sus_df", "Upload K-item Data ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 fileInput("single_bg_df", "Upload Background Data ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 hr(),
                 uiOutput("single_bg_id_ui"),
                 uiOutput("single_feat_ui"),
                 hr(),
                 actionButton("single_calc", "Calculate LR", class = "btn-primary")
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                          h5("Q-item ( Offender ) Data:"),
                          DTOutput("single_offPreview")
                   ),
                   column(6,
                          h5("K-item ( Suspect ) Data:"),
                          DTOutput("single_susPreview")
                   )
                 ),
                 fluidRow(
                   column(12,
                          h5("Background Data:"),
                          DTOutput("single_bgPreview")
                   )
                 ),
                 hr(),
                 h5("Raw LR:"),
                 verbatimTextOutput("single_res_LR"),
                 h5("ln(LR):"),
                 verbatimTextOutput("single_res_lnLR"),
                 h5("log10(LR):"),
                 verbatimTextOutput("single_res_log10LR"),
               )
             )
    ),
    # ==========================================================================
    
    # ======================== Hold-out Validation (UI) ========================
    tabPanel("Hold-out Validation",
             sidebarLayout(
               sidebarPanel(
                 fileInput("hv_bg_df", "Upload Training Set ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("hv_bg_id_ui"),
                 fileInput("hv_test_df", "Upload Validation Set ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("hv_test_id_ui"),
                 uiOutput("hv_test_item_ui"),
                 uiOutput("hv_feat_ui"),
                 hr(),
                 radioButtons("hv_mode", "Select Pairing Mode",
                              choices = c("Item vs Item" = "item_vs_item",
                                          "Item vs Suspect" = "item_vs_suspect"),
                              selected = "item_vs_item"),
                 actionButton("hv_calc", "Start Validation", class = "btn-primary"),
                 hr(),
                 downloadButton("hv_downloadResults", "Download Results")
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                          h5("Training Set Preview:"),
                          DTOutput("hv_trainPreview")
                   ),
                   column(6,
                          h5("Validation Set Preview:"),
                          DTOutput("hv_testPreview")
                   )
                 ),
                 hr(),
                 h5("Validation Results:"),
                 DTOutput("hv_resultPreview"),
                 hr(),
                 h5("Tippett Plot:"),
                 plotOutput("hv_tippettPlot"),
                 hr(),
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
                 fileInput("loo_val_df", "Upload Validation Set ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("loo_val_id_ui"),
                 uiOutput("loo_val_item_ui"),
                 uiOutput("loo_feat_ui"),
                 hr(),
                 radioButtons("loo_mode", "Select Pairing Mode",
                              choices = c("Item vs Item" = "item_vs_item",
                                          "Item vs Suspect" = "item_vs_suspect"),
                              selected = "item_vs_item"),
                 actionButton("loo_calc", "Start Validation", class = "btn-primary"),
                 hr(),
                 downloadButton("loo_downloadResults", "Download Results")
               ),
               mainPanel(
                 h5("Validation Set Preview:"),
                 DTOutput("loo_valPreview"),
                 hr(),
                 h5("Validation Results:"),
                 DTOutput("loo_resultPreview"),
                 hr(),
                 h5("Tippett Plot:"),
                 plotOutput("loo_tippettPlot"),
                 hr(),
                 h5("Performance Metrics:"),
                 verbatimTextOutput("loo_metrics")
               )
             )
    ),
    # ==========================================================================
    
    # ========================== Quick Metrics (UI) ============================
    tabPanel("Quick Metrics",
             sidebarLayout(
               sidebarPanel(
                 fileInput("quick_result", "Upload Validation Result ( .csv )", accept = ".csv"),
                 uiOutput("quick_label_ui"),
                 uiOutput("quick_lr_ui"),
                 actionButton("quick_calc", "Calculate Metrics", class = "btn-primary")
               ),
               mainPanel(
                 h5("Data Preview:"),
                 DTOutput("quick_preview"),
                 hr(),
                 h5("Tippett Plot:"),
                 plotOutput("quick_tippettPlot"),
                 hr(),
                 h5("Performance Metrics:"),
                 verbatimTextOutput("quick_metrics")
               )
             )
    )
    # ==========================================================================
  )
)

server <- function(input, output, session){
  
  # ====================== MVKD-based LR Calculator (server) ===================
  SingleOffData <- reactive({
    req(input$single_off_df)
    read.csv(input$single_off_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  SingleSusData <- reactive({
    req(input$single_sus_df)
    read.csv(input$single_sus_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  SingleBGData <- reactive({
    req(input$single_bg_df)
    read.csv(input$single_bg_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$single_bg_id_ui <- renderUI({
    req(SingleBGData())
    selectInput("single_bg_id", "Select ID Column for Background Data", 
                choices = names(SingleBGData()), selected = names(SingleBGData())[1])
  }) # Identify the ID column in Background Data
  
  output$single_feat_ui <- renderUI({
    req(SingleOffData(), SingleSusData(), SingleBGData(), input$single_bg_id)
    common <- Reduce(intersect, list(names(SingleOffData()), names(SingleSusData()), names(SingleBGData())))
    common <- setdiff(common, input$single_bg_id)
    selectInput("single_feats", "Select Feature Columns", 
                choices = common, selected = common, multiple = TRUE)
  }) # Identify the feature column(s)
  
  output$single_offPreview <- renderDT({
    req(SingleOffData())
    datatable(
      SingleOffData(),
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
  
  output$single_susPreview <- renderDT({
    req(SingleSusData())
    datatable(
      SingleSusData(),
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
  
  output$single_bgPreview <- renderDT({
    req(SingleBGData())
    datatable(
      SingleBGData(),
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
  
  observeEvent(input$single_calc, {
    req(SingleOffData(), SingleSusData(), SingleBGData(), input$single_bg_id, input$single_feats)
    
    off_data <- SingleOffData()[, input$single_feats, drop = FALSE]
    off_data <- as.matrix(off_data)
    sus_data <- SingleSusData()[, input$single_feats, drop = FALSE]
    sus_data <- as.matrix(sus_data)
    bg_data <- SingleBGData()
    if(input$single_bg_id != "id"){
      names(bg_data)[names(bg_data) == input$single_bg_id] <- "id"
    }
    bg_data <- bg_data[, c("id", input$single_feats)]
    
    log10LR <- MVKD_llr(off_data = off_data,
                        sus_data = sus_data,
                        bg_data = bg_data)
    LR <- 10^log10LR
    lnLR <- log(LR)
    
    output$single_res_LR <- renderPrint({ cat(LR) })
    output$single_res_lnLR <- renderPrint({ cat(lnLR) })
    output$single_res_log10LR <- renderPrint({ cat(log10LR) })
  })
  # ============================================================================
  
  # ======================== Hold-out Validation (server) ======================
  hvTrainData <- reactive({
    req(input$hv_bg_df)
    read.csv(input$hv_bg_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$hv_bg_id_ui <- renderUI({
    req(hvTrainData())
    selectInput("hv_bg_id", "Select Training Set ID Column", 
                choices = names(hvTrainData()), selected = names(hvTrainData())[1])
  })
  
  hvTestData <- reactive({
    req(input$hv_test_df)
    read.csv(input$hv_test_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$hv_test_id_ui <- renderUI({
    req(hvTestData())
    selectInput("hv_test_id", "Select Validation Set ID Column", 
                choices = names(hvTestData()), selected = names(hvTestData())[1])
  })
  
  output$hv_test_item_ui <- renderUI({
    req(hvTestData())
    selectInput("hv_test_item", "Select Validation Set Item Column", 
                choices = names(hvTestData()), 
                selected = if(length(names(hvTestData())) >= 2) names(hvTestData())[2] else names(hvTestData())[1])
  })
  
  output$hv_feat_ui <- renderUI({
    req(hvTestData(), input$hv_test_id, input$hv_test_item)
    feats <- setdiff(names(hvTestData()), c(input$hv_test_id, input$hv_test_item))
    selectInput("hv_feats", "Select Feature Columns", 
                choices = feats, selected = feats, multiple = TRUE)
  })
  
  output$hv_trainPreview <- renderDT({
    req(hvTrainData())
    datatable(
      hvTrainData(),
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
  
  output$hv_testPreview <- renderDT({
    req(hvTestData())
    datatable(
      hvTestData(),
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
  
  observeEvent(input$hv_calc, {
    req(hvTrainData(), hvTestData(),
        input$hv_bg_id, input$hv_test_id, input$hv_test_item, input$hv_feats,
        input$hv_mode)
    
    bg_df <- hvTrainData()
    if(input$hv_bg_id != "id"){
      names(bg_df)[names(bg_df) == input$hv_bg_id] <- "id"
    }
    bg_df <- bg_df[, c("id", input$hv_feats)]
    
    hv_MVKD_model <- MVKD_train(bg_data = bg_df) # Estimate the Background Parameters
    
    test_df <- hvTestData()
    if(input$hv_test_id != "id"){
      names(test_df)[names(test_df) == input$hv_test_id] <- "id"
    }
    if(input$hv_test_item != "item"){
      names(test_df)[names(test_df) == input$hv_test_item] <- "item"
    }
    test_df <- test_df[, c("id", "item", input$hv_feats)]
    test_df$item <- paste0(test_df$id, "|", test_df$item) # [id, item, feat_1, feat_2, ...]
    
    hv_results <- data.frame()
    
    if(input$hv_mode == "item_vs_item"){
      
      items <- unique(test_df$item)
      pairings <- combn(items, 2, simplify = FALSE)
      
      hv_results <- data.frame(offender_id = character(),
                               suspect_id = character(),
                               offender_item = character(),
                               suspect_item = character(),
                               label = character(),
                               log10LR = numeric(),
                               lnLR = numeric(),
                               LR = numeric(),
                               stringsAsFactors = FALSE)
      
      total_pairs <- length(pairings)
      print(paste("Total pairs:", total_pairs))
      withProgress(message = "Calculating LR: ", value = 0, {
        
        for(i in seq_along(pairings)) {
          pair <- pairings[[i]]
          off_item <- pair[1]
          sus_item <- pair[2]
          
          offender_data <- test_df[test_df$item == off_item, ]
          offender_mat <- as.matrix(offender_data[, input$hv_feats, drop = FALSE])
          off_id <- unique(offender_data$id)
          
          suspect_data <- test_df[test_df$item == sus_item, ]
          suspect_mat <- as.matrix(suspect_data[, input$hv_feats, drop = FALSE])
          sus_id <- unique(suspect_data$id)
          
          label_val <- ifelse(off_id == sus_id, "ss", "ds")
          
          log10LR <- MVKD_scorer(off_data = offender_mat,
                                 sus_data = suspect_mat,
                                 bg_para = hv_MVKD_model)
          LR <- 10^log10LR
          lnLR <- log(LR)
          
          hv_results <- rbind(hv_results, data.frame(offender_id = off_id,
                                                     suspect_id = sus_id,
                                                     offender_item = off_item,
                                                     suspect_item = sus_item,
                                                     label = label_val,
                                                     log10LR = log10LR,
                                                     lnLR = lnLR,
                                                     LR = LR,
                                                     stringsAsFactors = FALSE))
          incProgress(1/total_pairs, detail = paste(i, "of", total_pairs))
        }
      })
    } else if(input$hv_mode == "item_vs_suspect"){
      items <- unique(test_df$item)
      
      hv_results <- data.frame(offender_id = character(),
                               offender_item = character(),
                               suspect_id = character(),
                               label = character(),
                               log10LR = numeric(),
                               lnLR = numeric(),
                               LR = numeric(),
                               stringsAsFactors = FALSE)
      
      total_pairs <- length(items) * length(unique(test_df$id))
      print(paste("Total pairs:", total_pairs))
      progress_counter <- 0
      
      withProgress(message = "Calculating LR: ", value = 0, {
        
        for(off_item in items) {
          
          offender_data <- test_df[test_df$item == off_item, ]
          off_id <- offender_data$id[1]
          offender_mat <- as.matrix(offender_data[, input$hv_feats, drop = FALSE])
          
          pool_df <- test_df[test_df$item != off_item, ]
          sus_groups <- split(pool_df, pool_df$id)
          
          for(sus_id in names(sus_groups)) {
            
            suspect_data <- sus_groups[[sus_id]]
            suspect_mat <- as.matrix(suspect_data[, input$hv_feats, drop = FALSE])
            label_val <- ifelse(off_id == sus_id, "ss", "ds")
            
            log10LR <- MVKD_scorer(off_data = offender_mat,
                                   sus_data = suspect_mat,
                                   bg_para = hv_MVKD_model)
            LR <- 10^log10LR
            lnLR <- log(LR)
            
            hv_results <- rbind(hv_results, data.frame(offender_id = off_id,
                                                       offender_item = off_item,
                                                       suspect_id = sus_id,
                                                       label = label_val,
                                                       log10LR = log10LR,
                                                       lnLR = lnLR,
                                                       LR = LR,
                                                       stringsAsFactors = FALSE))
            progress_counter <- progress_counter + 1
            incProgress(1/total_pairs, detail = paste(progress_counter, "of", total_pairs))
          }
        }
      })
    }
    
    output$hv_resultPreview <- renderDT({
      datatable(
        hv_results,
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
    
    ss_lr <- hv_results$LR[hv_results$label == "ss"]
    ds_lr <- hv_results$LR[hv_results$label == "ds"]
    
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
    
    output$hv_downloadResults <- downloadHandler(
      filename = function() {
        paste("hv_results(", Sys.Date(), ").csv", sep = "")
      },
      content = function(file) {
        write.csv(hv_results, file, row.names = FALSE)
      }
    )
    
  })
  
  # ============================================================================
  
  # ================ Leave-one-out Cross Validation (server) ===================
  looValData <- reactive({
    req(input$loo_val_df)
    read.csv(input$loo_val_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$loo_val_id_ui <- renderUI({
    req(looValData())
    selectInput("loo_val_id", "Select Validation Set ID Column", 
                choices = names(looValData()), selected = names(looValData())[1])
  })
  
  output$loo_val_item_ui <- renderUI({
    req(looValData())
    selectInput("loo_val_item", "Select Validation Set Item Column", 
                choices = names(looValData()), 
                selected = if(length(names(looValData())) >= 2) names(looValData())[2] else names(looValData())[1])
  })
  
  output$loo_feat_ui <- renderUI({
    req(looValData(), input$loo_val_id, input$loo_val_item)
    feats <- setdiff(names(looValData()), c(input$loo_val_id, input$loo_val_item))
    selectInput("loo_feats", "Select Feature Columns", 
                choices = feats, selected = feats, multiple = TRUE)
  })
  
  output$loo_valPreview <- renderDT({
    req(looValData())
    datatable(
      looValData(),
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
  
  observeEvent(input$loo_calc, {
    req(looValData(), input$loo_val_id, input$loo_val_item, input$loo_feats, input$loo_mode)
    
    full_df <- looValData()
    if(input$loo_val_id != "id"){
      names(full_df)[names(full_df) == input$loo_val_id] <- "id"
    }
    if(input$loo_val_item != "item"){
      names(full_df)[names(full_df) == input$loo_val_item] <- "item"
    }
    
    full_df <- full_df[, c("id", "item", input$loo_feats)]
    full_df$item <- paste0(full_df$id, "|", full_df$item)
    
    if(input$loo_mode == "item_vs_item"){
      
      items <- unique(full_df$item)
      pairings <- combn(items, 2, simplify = FALSE)
      
      results_list <- vector("list", length(pairings))
      
      total_steps <- length(pairings)
      withProgress(message = "Preparing Leave-out Structures...", value = 0, {
        for (i in seq_along(pairings)) {
          pair <- pairings[[i]]
          off_item <- pair[1]
          sus_item <- pair[2]
          
          off_data <- full_df[full_df$item == off_item, ]
          sus_data <- full_df[full_df$item == sus_item, ]
          
          off_id <- off_data$id[1]
          sus_id <- sus_data$id[1]
          
          LO_key <- paste(sort(c(off_id, sus_id)), collapse = "|")
          label_val <- ifelse(off_id == sus_id, "ss", "ds")
          
          results_list[[i]] <- data.frame(
            offender_id = off_id,
            suspect_id = sus_id,
            offender_item = off_item,
            suspect_item = sus_item,
            LO_key = LO_key,
            label = label_val,
            log10LR = NA,
            lnLR = NA,
            LR = NA,
            stringsAsFactors = FALSE
          )
        }
        incProgress(1 / total_steps)
      })
      
      loo_results <- do.call(rbind, results_list)
      
      unique_keys <- unique(loo_results$LO_key)
      total_keys <- length(unique_keys)
      print(paste("Total keys:", total_keys))
      progress_counter <- 0
      
      withProgress(message = "Calculating LR by Leave-out Keys:", value = 0, {
        
        for (key in unique_keys) {
          progress_counter <- progress_counter + 1
          
          key_ids <- unlist(strsplit(key, "\\|"))
          
          bg_df <- full_df[!full_df$id %in% key_ids, ]
          bg_df <- bg_df[, c("id", input$loo_feats)]
          
          loo_model <- MVKD_train(bg_data = bg_df)
          
          trails <- loo_results[loo_results$LO_key == key, ]
          
          for (t_idx in seq_len(nrow(trails))) {
            off_item <- trails$offender_item[t_idx]
            sus_item <- trails$suspect_item[t_idx]
            
            off_data <- full_df[full_df$item == off_item, input$loo_feats, drop = FALSE]
            sus_data <- full_df[full_df$item == sus_item, input$loo_feats, drop = FALSE]
            
            off_mat <- as.matrix(off_data)
            sus_mat <- as.matrix(sus_data)
            
            log10LR <- MVKD_scorer(off_data = off_mat,
                                   sus_data = sus_mat,
                                   bg_para = loo_model)
            LR <- 10^log10LR
            lnLR <- log(LR)
            
            loo_results$log10LR[loo_results$LO_key == key &
                                  loo_results$offender_item == off_item &
                                  loo_results$suspect_item == sus_item] <- log10LR
            loo_results$lnLR[loo_results$LO_key == key &
                               loo_results$offender_item == off_item &
                               loo_results$suspect_item == sus_item] <- lnLR
            loo_results$LR[loo_results$LO_key == key &
                             loo_results$offender_item == off_item &
                             loo_results$suspect_item == sus_item] <- LR
          }
          
          rm(loo_model)
          gc(verbose = FALSE)
          incProgress(1 / total_keys,
                      detail = paste(progress_counter, "of", total_keys))
        }
      })
    } else if(input$loo_mode == "item_vs_suspect"){
      
      items <- unique(full_df$item)
      unique_ids <- unique(full_df$id)
      
      results_list <- list()
      idx <- 1
      
      total_steps <- length(items)
      withProgress(message = "Preparing Leave-out Structures...", value = 0, {
        for (off_item in items) {
          off_id <- strsplit(off_item, "\\|")[[1]][1]
          
          for (sus_id in unique_ids) {
            
            if (sus_id == off_id && sum(full_df$id == sus_id) < 2) next
            
            LO_key <- paste(sort(c(off_id, sus_id)), collapse = "|")
            label_val <- ifelse(off_id == sus_id, "ss", "ds")
            
            results_list[[idx]] <- data.frame(
              offender_id = off_id,
              offender_item = off_item,
              suspect_id = sus_id,
              LO_key = LO_key,
              label = label_val,
              log10LR = NA,
              lnLR = NA,
              LR = NA,
              stringsAsFactors = FALSE
            )
            idx <- idx + 1
          }
          incProgress(1 / total_steps)
        }
      })
      
      loo_results <- do.call(rbind, results_list)
      loo_results <- subset(
        loo_results,
        !is.na(LO_key) & !is.na(offender_id) & !is.na(suspect_id) & !is.na(offender_item)
      )
      unique_keys <- unique(loo_results$LO_key)
      total_keys <- length(unique_keys)
      print(paste("Total keys:", total_keys))
      progress_counter <- 0
      
      item_list <- split(full_df, as.character(full_df$item))
      id_groups <- split(full_df, as.character(full_df$id))
      
      withProgress(message = "Calculating LR by Leave-out Keys:", value = 0, {
        
        for (key in unique_keys) {
          progress_counter <- progress_counter + 1
          
          key_ids <- unlist(strsplit(key, "\\|"))
          
          bg_df <- full_df[!full_df$id %in% key_ids, ]
          bg_df <- bg_df[, c("id", input$loo_feats)]
          
          if (nrow(bg_df) < 2) {
            warning(paste("Skipping LO_key", key, ": insufficient background data."))
            next
          }
          
          loo_model <- MVKD_train(bg_data = bg_df)
          
          trails <- loo_results[loo_results$LO_key == key, ]
          
          for (t_idx in seq_len(nrow(trails))) {
            
            off_item <- as.character(trails$offender_item[t_idx])
            sus_id   <- as.character(trails$suspect_id[t_idx])
            label_val <- trails$label[t_idx]
            
            offender_data <- item_list[[off_item]]
            offender_mat  <- as.matrix(offender_data[, input$loo_feats, drop = FALSE])
            
            if (label_val == "ss") {
              id_data <- id_groups[[sus_id]]
              suspect_data <- id_data[id_data$item != off_item, ]
            } else {
              suspect_data <- id_groups[[sus_id]]
            }
            
            if (is.null(suspect_data) || nrow(suspect_data) < 1) {
              next
            }
            
            suspect_mat <- as.matrix(suspect_data[, input$loo_feats, drop = FALSE])
            
            log10LR <- MVKD_scorer(off_data = offender_mat,
                                   sus_data = suspect_mat,
                                   bg_para = loo_model)
            LR <- 10^log10LR
            lnLR <- ifelse(LR > 0, log(LR), NA)
            
            idx <- loo_results$LO_key == key &
              loo_results$offender_item == off_item &
              loo_results$suspect_id == sus_id
            
            loo_results$log10LR[idx] <- log10LR
            loo_results$lnLR[idx] <- lnLR
            loo_results$LR[idx] <- LR
          }
          
          rm(loo_model)
          gc(verbose = FALSE)
          incProgress(1 / total_keys, detail = paste(progress_counter, "of", total_keys))
        }
      })
    }
    
    output$loo_resultPreview <- renderDT({
      datatable(
        loo_results,
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
    
    ss_lr <- loo_results$LR[loo_results$label == "ss"]
    ds_lr <- loo_results$LR[loo_results$label == "ds"]
    
    ss_lr <- ss_lr[!is.na(ss_lr)]
    ds_lr <- ds_lr[!is.na(ds_lr)]
    
    cllr_pooled <- Cllr(ss_lr, ds_lr)
    cllr_min <- Cllr_min(ss_lr, ds_lr)
    cllr_cal <- Cllr_cal(ss_lr, ds_lr)
    eer_result <- EER(ss_lr, ds_lr)
    
    output$loo_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled, "\n")
      cat("Cllr (min):", cllr_min, "\n")
      cat("Cllr (cal):", cllr_cal, "\n")
      cat("EER:", eer_result$EER, "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):", eer_result$threshold_Raw, "\n")
    })
    
    output$loo_tippettPlot <- renderPlot({
      tippett.plot(
        ss_lr = ss_lr,
        ds_lr = ds_lr,
        x_lab = "Log10 Likelihood Ratio",
        y_lab = "Cumulative Proportion"
      )
    })
    
    output$loo_downloadResults <- downloadHandler(
      filename = function() {
        paste("loo_results(", Sys.Date(), ").csv", sep = "")
      },
      content = function(file) {
        write.csv(loo_results, file, row.names = FALSE)
      }
    )
  })
  
  # ============================================================================
  
  # ========================== Quick Metrics (server) ==========================
  quickData <- reactive({
    req(input$quick_result)
    read.csv(input$quick_result$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$quick_label_ui <- renderUI({
    req(quickData())
    choices <- names(quickData())
    default <- if("label" %in% choices) "label" else choices[1]
    selectInput("quick_label", "Select Label Column ( ss/ds )", choices = choices, selected = default)
  })
  
  output$quick_lr_ui <- renderUI({
    req(quickData())
    choices <- names(quickData())
    default <- if("LR" %in% choices) "LR" else choices[1]
    selectInput("quick_lr", "Select LR Column ( Raw )", choices = choices, selected = default)
  })
  
  output$quick_preview <- renderDT({
    req(quickData())
    datatable(
      quickData(),
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
  
  observeEvent(input$quick_calc, {
    req(quickData(), input$quick_label, input$quick_lr)
    
    quick_data_df <- quickData()
    
    labels <- as.character(quick_data_df[[input$quick_label]])
    lr_vals <- as.numeric(quick_data_df[[input$quick_lr]])
    
    ss_lr <- lr_vals[labels == "ss"]
    ds_lr <- lr_vals[labels == "ds"]
    
    ss_lr <- ss_lr[!is.na(ss_lr)]
    ds_lr <- ds_lr[!is.na(ds_lr)]
    
    cllr_pooled <- Cllr(ss_lr, ds_lr)
    cllr_min <- Cllr_min(ss_lr, ds_lr)
    cllr_cal <- Cllr_cal(ss_lr, ds_lr)
    eer_result <- EER(ss_lr, ds_lr)
    
    output$quick_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled, "\n")
      cat("Cllr (min):", cllr_min, "\n")
      cat("Cllr (cal):", cllr_cal, "\n")
      cat("EER:", eer_result$EER, "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):", eer_result$threshold_Raw, "\n")
    })
    
    output$quick_tippettPlot <- renderPlot({
      tippett.plot(
        ss_lr = ss_lr,
        ds_lr = ds_lr,
        x_lab = "Log10 Likelihood Ratio",
        y_lab = "Cumulative Proportion"
      )
    })
  })
  # ============================================================================
}

options(shiny.maxRequestSize = 30 * 1024^2)
shinyApp(ui = ui, server = server)