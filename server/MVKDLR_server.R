# Path: "server/MVKDLR_server.R"

source("server/utils/add_lr_scales.R", local = TRUE)

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
    selectInput("single_bg_id", "Select ID Column for the Reference Dataset", 
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
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  output$single_susPreview <- renderDT({
    req(SingleSusData())
    datatable(
      SingleSusData(),
      rownames = FALSE,
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  output$single_bgPreview <- renderDT({
    req(SingleBGData())
    datatable(
      SingleBGData(),
      rownames = FALSE,
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  observeEvent(input$single_calc, {
    req(SingleOffData(), SingleSusData(), SingleBGData(),
        input$single_bg_id, input$single_feats)
    
    off_data <- SingleOffData()[, input$single_feats, drop = FALSE]
    sus_data <- SingleSusData()[, input$single_feats, drop = FALSE]
    bg_data <- SingleBGData()
    
    off_data <- as.matrix(off_data)
    sus_data <- as.matrix(sus_data)
    
    storage.mode(off_data) <- "numeric"
    storage.mode(sus_data) <- "numeric"
    
    if(input$single_bg_id != "id"){
      names(bg_data)[names(bg_data) == input$single_bg_id] <- "id"
    }
    bg_data <- bg_data[, c("id", input$single_feats), drop = FALSE]
    
    for(feat in input$single_feats){
      bg_data[[feat]] <- as.numeric(bg_data[[feat]])
    }
    
    lnLR <- MVKD_llr(off_data = off_data,
                     sus_data = sus_data,
                     bg_data = bg_data)
    LR <- exp(lnLR)
    log10LR <- lnLR / log(10)
    
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
    selectInput("hv_bg_id", "ID Column", 
                choices = names(hvTrainData()), selected = names(hvTrainData())[1])
  })
  
  output$hv_validation_upload_ui <- renderUI({
    req(input$hv_vali_mode)
    
    if(input$hv_vali_mode == "qk_cross"){
      tagList(
        wellPanel(
          h5("Testing Set of Questioned-condition"),
          fileInput("hv_q_df", "Upload the Dataset ( .csv )",
                    accept = ".csv", multiple = FALSE),
          uiOutput("hv_q_id_ui"),
          uiOutput("hv_q_item_ui")
        ),
        
        wellPanel(
          h5("Testing Set of Known-condition"),
          fileInput("hv_k_df", "Upload the Dataset ( .csv )",
                    accept = ".csv", multiple = FALSE),
          uiOutput("hv_k_id_ui"),
          uiOutput("hv_k_item_ui")
        )
      )
    } else {
      wellPanel(
        h5("Testing Set"),
        fileInput("hv_test_df", "Upload the Dataset ( .csv )",
                  accept = ".csv", multiple = FALSE),
        uiOutput("hv_test_id_ui"),
        uiOutput("hv_test_item_ui")
      )
    }
  })
  
  hvQData <- reactive({
    req(input$hv_q_df)
    read.csv(input$hv_q_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  hvKData <- reactive({
    req(input$hv_k_df)
    read.csv(input$hv_k_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  hvTestData <- reactive({
    req(input$hv_test_df)
    read.csv(input$hv_test_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$hv_q_id_ui <- renderUI({
    req(hvQData())
    selectInput("hv_q_id", "ID Column",
                choices = names(hvQData()),
                selected = names(hvQData())[1])
  })
  
  output$hv_k_id_ui <- renderUI({
    req(hvKData())
    selectInput("hv_k_id", "ID Column",
                choices = names(hvKData()),
                selected = names(hvKData())[1])
  })
  
  output$hv_test_id_ui <- renderUI({
    req(hvTestData())
    selectInput("hv_test_id", "ID Column",
                choices = names(hvTestData()),
                selected = names(hvTestData())[1])
  })
  
  output$hv_q_item_ui <- renderUI({
    req(hvQData())
    selectInput("hv_q_item", "Item Column",
                choices = names(hvQData()),
                selected = if(length(names(hvQData())) >= 2) names(hvQData())[2] else names(hvQData())[1])
  })
  
  output$hv_k_item_ui <- renderUI({
    req(hvKData())
    selectInput("hv_k_item", "Item Column",
                choices = names(hvKData()),
                selected = if(length(names(hvKData())) >= 2) names(hvKData())[2] else names(hvKData())[1])
  })
  
  output$hv_test_item_ui <- renderUI({
    req(hvTestData())
    selectInput("hv_test_item", "Item Column",
                choices = names(hvTestData()),
                selected = if(length(names(hvTestData())) >= 2) names(hvTestData())[2] else names(hvTestData())[1])
  })
  
  output$hv_feat_ui <- renderUI({
    req(hvTrainData(), input$hv_bg_id, input$hv_vali_mode)
    
    if(input$hv_vali_mode == "qk_cross"){
      req(hvQData(), hvKData(),
          input$hv_q_id, input$hv_k_id,
          input$hv_q_item, input$hv_k_item)
      
      feats <- Reduce(intersect, list(
        names(hvTrainData()),
        names(hvQData()),
        names(hvKData())
      ))
      
      feats <- setdiff(feats, c(input$hv_bg_id, input$hv_q_id, input$hv_k_id,
                                input$hv_q_item, input$hv_k_item))
      
    } else {
      req(hvTestData(), input$hv_test_id, input$hv_test_item)
      
      feats <- Reduce(intersect, list(
        names(hvTrainData()),
        names(hvTestData())
      ))
      
      feats <- setdiff(feats, c(input$hv_bg_id, input$hv_test_id,
                                input$hv_test_item))
    }
    
    selectInput("hv_feats", "Select Feature Columns", 
                choices = feats, selected = feats, multiple = TRUE)
  })
  
  output$hv_validation_preview_ui <- renderUI({
    req(input$hv_vali_mode)
    if(input$hv_vali_mode == "qk_cross"){
      fluidRow(
        column(6,
               h5("Questioned-condition Testing Set Preview:"),
               DTOutput("hv_qPreview")
        ),
        column(6,
               h5("Known-condition Testing Set Preview:"),
               DTOutput("hv_kPreview")
        )
      )
    } else {
      fluidRow(
        column(12,
               h5("Testing Set Preview:"),
               DTOutput("hv_testPreview")
        )
      )
    }
  })
  
  output$hv_trainPreview <- renderDT({
    req(hvTrainData())
    datatable(
      hvTrainData(),
      rownames = FALSE,
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  output$hv_testPreview <- renderDT({
    req(hvTestData())
    datatable(
      hvTestData(),
      rownames = FALSE,
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  output$hv_qPreview <- renderDT({
    req(hvQData())
    datatable(
      hvQData(),
      rownames = FALSE,
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  output$hv_kPreview <- renderDT({
    req(hvKData())
    datatable(
      hvKData(),
      rownames = FALSE,
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  hv_results_store <- reactiveVal(NULL)
  
  observeEvent(input$hv_calc, {
    req(hvTrainData(),
        input$hv_bg_id, input$hv_feats, input$hv_vali_mode, input$hv_pairing_mode)
    
    # 1. Train MVKD model
    bg_df <- hvTrainData()
    
    if(input$hv_bg_id != "id"){
      names(bg_df)[names(bg_df) == input$hv_bg_id] <- "id"
    }
    
    bg_df <- bg_df[, c("id", input$hv_feats), drop = FALSE]
    
    for(feat in input$hv_feats){
      bg_df[[feat]] <- as.numeric(bg_df[[feat]])
    }
    
    hv_MVKD_model <- MVKD_train(bg_data = bg_df)
    
    # 2. Prepare testing data and validation table
    validation_table <- data.frame()
    
    if(input$hv_vali_mode == "qk_cross"){
      req(hvQData(), hvKData(),
          input$hv_q_id, input$hv_k_id,
          input$hv_q_item, input$hv_k_item)
      
      q_df <- hvQData()
      k_df <- hvKData()
      
      if(input$hv_q_id != "id"){
        names(q_df)[names(q_df) == input$hv_q_id] <- "id"
      }
      if(input$hv_q_item != "item"){
        names(q_df)[names(q_df) == input$hv_q_item] <- "item"
      }
      
      if(input$hv_k_id != "id"){
        names(k_df)[names(k_df) == input$hv_k_id] <- "id"
      }
      if(input$hv_k_item != "item"){
        names(k_df)[names(k_df) == input$hv_k_item] <- "item"
      }
      
      q_df <- q_df[, c("id", "item", input$hv_feats), drop = FALSE]
      k_df <- k_df[, c("id", "item", input$hv_feats), drop = FALSE]
      
      q_df$item <- paste0(q_df$id, "|", q_df$item)
      k_df$item <- paste0(k_df$id, "|", k_df$item)
      
      q_items <- unique(q_df$item)
      
      if(input$hv_pairing_mode == "item_vs_item"){
        k_items <- unique(k_df$item)
        
        validation_table <- expand.grid(
          offender_item = q_items,
          suspect_item = k_items,
          stringsAsFactors = FALSE
        )
        
        validation_table$offender_id <- sapply(
          validation_table$offender_item,
          function(x) q_df$id[q_df$item == x][1]
        )
        
        validation_table$suspect_id <- sapply(
          validation_table$suspect_item,
          function(x) k_df$id[k_df$item == x][1]
        )
        
      } else if(input$hv_pairing_mode == "item_vs_suspect"){
        k_ids <- unique(k_df$id)
        
        validation_table <- expand.grid(
          offender_item = q_items,
          suspect_id = k_ids,
          stringsAsFactors = FALSE
        )
        
        validation_table$offender_id <- sapply(
          validation_table$offender_item,
          function(x) q_df$id[q_df$item == x][1]
        )
        
        validation_table <- validation_table[, c(
          "offender_item",
          "offender_id",
          "suspect_id"
        )]
      }
    }
    
    if(input$hv_vali_mode == "full_cross"){
      req(hvTestData(), input$hv_test_id, input$hv_test_item)
      
      test_df <- hvTestData()
      
      if(input$hv_test_id != "id"){
        names(test_df)[names(test_df) == input$hv_test_id] <- "id"
      }
      if(input$hv_test_item != "item"){
        names(test_df)[names(test_df) == input$hv_test_item] <- "item"
      }
      
      test_df <- test_df[, c("id", "item", input$hv_feats), drop = FALSE]
      test_df$item <- paste0(test_df$id, "|", test_df$item)
      
      items <- unique(test_df$item)
      
      if(input$hv_pairing_mode == "item_vs_item"){
        pairings <- combn(items, 2, simplify = FALSE)
        
        validation_table <- do.call(
          rbind,
          lapply(pairings, function(pair){
            data.frame(offender_item = pair[1],
                       suspect_item = pair[2],
                       stringsAsFactors = FALSE)
          })
        )
        
        validation_table$offender_id <- sapply(
          validation_table$offender_item,
          function(x) test_df$id[test_df$item == x][1]
        )
        
        validation_table$suspect_id <- sapply(
          validation_table$suspect_item,
          function(x) test_df$id[test_df$item == x][1]
        )
        
      } else if(input$hv_pairing_mode == "item_vs_suspect"){
        ids <- unique(test_df$id)
        
        validation_table <- expand.grid(
          offender_item = items,
          suspect_id = ids,
          stringsAsFactors = FALSE
        )
        
        validation_table$offender_id <- sapply(
          validation_table$offender_item,
          function(x) test_df$id[test_df$item == x][1]
        )
        
        validation_table <- validation_table[
          !(validation_table$offender_id == validation_table$suspect_id &
              sapply(validation_table$offender_item, function(x){
                this_id <- test_df$id[test_df$item == x][1]
                sum(test_df$id == this_id)
              }) < 2),
        ]
        
        validation_table <- validation_table[, c(
          "offender_item",
          "offender_id",
          "suspect_id"
        )]
      }
    }
    
    validation_table$label <- ifelse(
      validation_table$offender_id == validation_table$suspect_id,
      SS_LABEL, DS_LABEL
    )
    
    validation_table$lnLR <- NA_real_
    validation_table$LR <- NA_real_
    validation_table$log10LR <- NA_real_
    
    # 3. Score row by row
    total_rows <- nrow(validation_table)
    
    withProgress(message = "Calculating LR:", value = 0, {
      for(i in seq_len(total_rows)){
        
        offender_item <- validation_table$offender_item[i]
        suspect_id <- validation_table$suspect_id[i]
        suspect_item <- if("suspect_item" %in% names(validation_table)) {
          validation_table$suspect_item[i]
        } else {
          NA_character_
        }
        
        if(input$hv_vali_mode == "qk_cross"){
          offender_data <- q_df[q_df$item == offender_item, ]
          
          if(input$hv_pairing_mode == "item_vs_item"){
            suspect_data <- k_df[k_df$item == suspect_item, ]
          } else {
            suspect_data <- k_df[k_df$id == suspect_id, ]
          }
        }
        
        if(input$hv_vali_mode == "full_cross"){
          offender_data <- test_df[test_df$item == offender_item, ]
          
          if(input$hv_pairing_mode == "item_vs_item"){
            suspect_data <- test_df[test_df$item == suspect_item, ]
          } else {
            suspect_data <- test_df[test_df$id == suspect_id, ]
            
            if(validation_table$offender_id[i] == suspect_id){
              suspect_data <- suspect_data[suspect_data$item != offender_item, ]
            }
          }
        }
        
        if(nrow(suspect_data) < 1){
          next
        }
        
        offender_mat <- as.matrix(offender_data[, input$hv_feats, drop = FALSE])
        suspect_mat <- as.matrix(suspect_data[, input$hv_feats, drop = FALSE])
        
        storage.mode(offender_mat) <- "numeric"
        storage.mode(suspect_mat) <- "numeric"
        
        lnLR <- MVKD_scorer(
          off_data = offender_mat,
          sus_data = suspect_mat,
          bg_para = hv_MVKD_model
        )
        
        lr_vals <- add_lr_scales(lnLR)
        
        validation_table$lnLR[i] <- lr_vals$lnLR
        validation_table$LR[i] <- lr_vals$LR
        validation_table$log10LR[i] <- lr_vals$log10LR
        
        incProgress(1 / total_rows, detail = paste(i, "of", total_rows))
      }
    })
    
    hv_results <- validation_table
    hv_results_store(hv_results)
    
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
    
    ss_llr <- hv_results$lnLR[hv_results$label == SS_LABEL]
    ds_llr <- hv_results$lnLR[hv_results$label == DS_LABEL]
    
    ss_llr <- ss_llr[!is.na(ss_llr)]
    ds_llr <- ds_llr[!is.na(ds_llr)]
    
    cllr_pooled_value <- cllr(ss_llr, ds_llr)
    cllr_min_value <- cllr_min(ss_llr, ds_llr)
    cllr_cal_value <- cllr_cal(ss_llr, ds_llr)
    eer_result <- eer(ss_llr, ds_llr)
    
    output$hv_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled_value, "\n")
      cat("Cllr (min):", cllr_min_value, "\n")
      cat("Cllr (cal):", cllr_cal_value, "\n")
      cat("EER:", eer_result$EER, "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):", eer_result$threshold_raw, "\n")
    })
    
    output$hv_tippettPlot <- renderPlot({
      tippett_plot(ss_llr = ss_llr,
                   ds_llr = ds_llr,
                   x_lab = expression(log[10](Lambda)),
                   y_lab = "cumulative proportion")
    })
  })
  
  output$hv_downloadResults <- downloadHandler(
    filename = function() {
      paste("hv_results(", Sys.Date(), ").csv", sep = "")
    },
    content = function(file) {
      req(hv_results_store())
      write.csv(hv_results_store(), file, row.names = FALSE)
    }
  )
  
  # ============================================================================
  
  # ================ Leave-one/two-out Cross Validation (server) ===============
  output$loo_validation_upload_ui <- renderUI({
    req(input$loo_vali_mode)
    
    if(input$loo_vali_mode == "qk_cross"){
      tagList(
        wellPanel(
          h5("Training/Testing Set of Questioned-condition"),
          fileInput("loo_q_df", "Upload the Dataset ( .csv )",
                    accept = ".csv", multiple = FALSE),
          uiOutput("loo_q_id_ui"),
          uiOutput("loo_q_item_ui")
        ),
        
        wellPanel(
          h5("Training/Testing Set of Known-condition"),
          fileInput("loo_k_df", "Upload the Dataset ( .csv )",
                    accept = ".csv", multiple = FALSE),
          uiOutput("loo_k_id_ui"),
          uiOutput("loo_k_item_ui")
        )
      )
    } else {
      wellPanel(
        h5("Training/Testing Set"),
        fileInput("loo_test_df", "Upload the Dataset ( .csv )",
                  accept = ".csv", multiple = FALSE),
        uiOutput("loo_test_id_ui"),
        uiOutput("loo_test_item_ui")
      )
    }
  })
  
  looQData <- reactive({
    req(input$loo_q_df)
    read.csv(input$loo_q_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$loo_q_id_ui <- renderUI({
    req(looQData())
    selectInput("loo_q_id", "ID Column",
                choices = names(looQData()),
                selected = names(looQData())[1])
  })
  
  output$loo_q_item_ui <- renderUI({
    req(looQData())
    selectInput("loo_q_item", "Item Column",
                choices = names(looQData()),
                selected = if(length(names(looQData())) >= 2) {
                  names(looQData())[2]
                } else {
                  names(looQData())[1]
                })
  })
  
  looKData <- reactive({
    req(input$loo_k_df)
    read.csv(input$loo_k_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$loo_k_id_ui <- renderUI({
    req(looKData())
    selectInput("loo_k_id", "ID Column",
                choices = names(looKData()),
                selected = names(looKData())[1])
  })
  
  output$loo_k_item_ui <- renderUI({
    req(looKData())
    selectInput("loo_k_item", "Item Column",
                choices = names(looKData()),
                selected = if(length(names(looKData())) >= 2) {
                  names(looKData())[2]
                } else {
                  names(looKData())[1]
                })
  })
  
  looTestData <- reactive({
    req(input$loo_test_df)
    read.csv(input$loo_test_df$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  output$loo_test_id_ui <- renderUI({
    req(looTestData())
    selectInput("loo_test_id", "ID Column",
                choices = names(looTestData()),
                selected = names(looTestData())[1])
  })
  
  output$loo_test_item_ui <- renderUI({
    req(looTestData())
    selectInput("loo_test_item", "Item Column",
                choices = names(looTestData()),
                selected = if(length(names(looTestData())) >= 2) {
                  names(looTestData())[2]
                } else {
                  names(looTestData())[1]
                })
  })
  
  output$loo_feat_ui <- renderUI({
    req(input$loo_vali_mode)
    
    if(input$loo_vali_mode == "qk_cross"){
      req(looQData(), looKData(),
          input$loo_q_id, input$loo_k_id,
          input$loo_q_item, input$loo_k_item)
      
      feats <- intersect(names(looQData()), names(looKData()))
      
      feats <- setdiff(feats, c(
        input$loo_q_id,
        input$loo_k_id,
        input$loo_q_item,
        input$loo_k_item
      ))
      
    } else {
      req(looTestData(), input$loo_test_id, input$loo_test_item)
      
      feats <- setdiff(
        names(looTestData()),
        c(input$loo_test_id, input$loo_test_item)
      )
    }
    
    selectInput("loo_feats", "Select Feature Columns",
                choices = feats,
                selected = feats,
                multiple = TRUE)
  })
  
  output$loo_validation_preview_ui <- renderUI({
    req(input$loo_vali_mode)
    
    if(input$loo_vali_mode == "qk_cross"){
      fluidRow(
        column(6,
               h5("Questioned-condition Training/Testing Set Preview:"),
               DTOutput("loo_qPreview")
        ),
        column(6,
               h5("Known-condition Training/Testing Set Preview:"),
               DTOutput("loo_kPreview")
        )
      )
    } else {
      fluidRow(
        column(12,
               h5("Training/Testing Set Preview:"),
               DTOutput("loo_testPreview")
        )
      )
    }
  })
  
  output$loo_qPreview <- renderDT({
    req(looQData())
    datatable(
      looQData(),
      rownames = FALSE,
      options = list( pageLength = 5,
                      lengthMenu = c(10, 25, 50, 100),
                      scrollX = TRUE,
                      ordering = TRUE,
                      dom = 'tip')
    )
  })
  
  output$loo_kPreview <- renderDT({
    req(looKData())
    datatable(
      looKData(),
      rownames = FALSE,
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  output$loo_testPreview <- renderDT({
    req(looTestData())
    datatable(
      looTestData(),
      rownames = FALSE,
      options = list(pageLength = 5,
                     lengthMenu = c(10, 25, 50, 100),
                     scrollX = TRUE,
                     ordering = TRUE,
                     dom = 'tip')
    )
  })
  
  loo_results_store <- reactiveVal(NULL)
  
  observeEvent(input$loo_calc, {
    req(input$loo_vali_mode, input$loo_pairing_mode, input$loo_feats)
    
    # 1. Prepare the full set
    if(input$loo_vali_mode == "qk_cross"){
      req(looQData(), looKData(),
          input$loo_q_id, input$loo_k_id,
          input$loo_q_item, input$loo_k_item)
      
      q_df <- looQData()
      k_df <- looKData()
      
      if(input$loo_q_id != "id"){
        names(q_df)[names(q_df) == input$loo_q_id] <- "id"
      }
      if(input$loo_q_item != "item"){
        names(q_df)[names(q_df) == input$loo_q_item] <- "item"
      }
      
      if(input$loo_k_id != "id"){
        names(k_df)[names(k_df) == input$loo_k_id] <- "id"
      }
      if(input$loo_k_item != "item"){
        names(k_df)[names(k_df) == input$loo_k_item] <- "item"
      }
      
      q_df <- q_df[, c("id", "item", input$loo_feats), drop = FALSE]
      k_df <- k_df[, c("id", "item", input$loo_feats), drop = FALSE]
      
      q_df$item <- paste0(q_df$id, "|", q_df$item)
      k_df$item <- paste0(k_df$id, "|", k_df$item)
      
      full_df <- rbind(q_df, k_df)
    }
    
    if(input$loo_vali_mode == "full_cross"){
      req(looTestData(), input$loo_test_id, input$loo_test_item)
      
      test_df <- looTestData()
      
      if(input$loo_test_id != "id"){
        names(test_df)[names(test_df) == input$loo_test_id] <- "id"
      }
      if(input$loo_test_item != "item"){
        names(test_df)[names(test_df) == input$loo_test_item] <- "item"
      }
      
      test_df <- test_df[, c("id", "item", input$loo_feats), drop = FALSE]
      test_df$item <- paste0(test_df$id, "|", test_df$item)
      
      full_df <- test_df
    }
    
    for(feat in input$loo_feats){
      full_df[[feat]] <- as.numeric(full_df[[feat]])
    }
    
    # 2. Build validation table
    validation_table <- data.frame()
    
    if(input$loo_vali_mode == "qk_cross"){
      
      q_items <- unique(q_df$item)
      
      if(input$loo_pairing_mode == "item_vs_item"){
        k_items <- unique(k_df$item)
        
        validation_table <- expand.grid(
          offender_item = q_items,
          suspect_item = k_items,
          stringsAsFactors = FALSE
        )
        
        validation_table$offender_id <- sapply(
          validation_table$offender_item,
          function(x) q_df$id[q_df$item == x][1]
        )
        
        validation_table$suspect_id <- sapply(
          validation_table$suspect_item,
          function(x) k_df$id[k_df$item == x][1]
        )
        
      } else if(input$loo_pairing_mode == "item_vs_suspect"){
        k_ids <- unique(k_df$id)
        
        validation_table <- expand.grid(
          offender_item = q_items,
          suspect_id = k_ids,
          stringsAsFactors = FALSE
        )
        
        validation_table$offender_id <- sapply(
          validation_table$offender_item,
          function(x) q_df$id[q_df$item == x][1]
        )
        
        validation_table <- validation_table[, c(
          "offender_item",
          "offender_id",
          "suspect_id"
        )]
      }
    }
    
    if(input$loo_vali_mode == "full_cross"){
      
      items <- unique(test_df$item)
      
      if(input$loo_pairing_mode == "item_vs_item"){
        pairings <- combn(items, 2, simplify = FALSE)
        
        validation_table <- do.call(
          rbind,
          lapply(pairings, function(pair){
            data.frame(
              offender_item = pair[1],
              suspect_item = pair[2],
              stringsAsFactors = FALSE
            )
          })
        )
        
        validation_table$offender_id <- sapply(
          validation_table$offender_item,
          function(x) test_df$id[test_df$item == x][1]
        )
        
        validation_table$suspect_id <- sapply(
          validation_table$suspect_item,
          function(x) test_df$id[test_df$item == x][1]
        )
        
      } else if(input$loo_pairing_mode == "item_vs_suspect"){
        ids <- unique(test_df$id)
        
        validation_table <- expand.grid(
          offender_item = items,
          suspect_id = ids,
          stringsAsFactors = FALSE
        )
        
        validation_table$offender_id <- sapply(
          validation_table$offender_item,
          function(x) test_df$id[test_df$item == x][1]
        )
        
        validation_table <- validation_table[
          !(validation_table$offender_id == validation_table$suspect_id &
              sapply(validation_table$offender_item, function(x){
                this_id <- test_df$id[test_df$item == x][1]
                sum(test_df$id == this_id)
              }) < 2),
        ]
        
        validation_table <- validation_table[, c(
          "offender_item",
          "offender_id",
          "suspect_id"
        )]
      }
    }
    
    validation_table$label <- ifelse(
      validation_table$offender_id == validation_table$suspect_id,
      SS_LABEL, DS_LABEL
    )
    
    # 3. Generate leave-out key
    validation_table$leave_out_key <- mapply(
      function(off_id, sus_id){
        paste(sort(c(off_id, sus_id)), collapse = "|")
      },
      validation_table$offender_id,
      validation_table$suspect_id
    )
    
    validation_table$lnLR <- NA_real_
    validation_table$LR <- NA_real_
    validation_table$log10LR <- NA_real_
    
    # 4. Train MVKD model by leave_out_key and score row by row
    unique_keys <- unique(validation_table$leave_out_key)
    total_keys <- length(unique_keys)
    progress_counter <- 0
    
    withProgress(message = "Calculating LR by Leave-out Keys:", value = 0, {
      
      for(key in unique_keys){
        progress_counter <- progress_counter + 1
        
        key_ids <- unlist(strsplit(key, "\\|"))
        
        bg_df <- full_df[!full_df$id %in% key_ids, ]
        bg_df <- bg_df[, c("id", input$loo_feats), drop = FALSE]
        
        if(nrow(bg_df) < 2){
          warning(paste("Skipping leave_out_key", key, ": insufficient reference data."))
          incProgress(1 / total_keys,
                      detail = paste(progress_counter, "of", total_keys))
          next
        }
        
        for(feat in input$loo_feats){
          bg_df[[feat]] <- as.numeric(bg_df[[feat]])
        }
        
        loo_MVKD_model <- MVKD_train(bg_data = bg_df)
        
        row_idx <- which(validation_table$leave_out_key == key)
        
        for(i in row_idx){
          
          offender_item <- validation_table$offender_item[i]
          suspect_id    <- validation_table$suspect_id[i]
          
          suspect_item <- if("suspect_item" %in% names(validation_table)) {
            validation_table$suspect_item[i]
          } else {
            NA_character_
          }
          
          if(input$loo_vali_mode == "qk_cross"){
            offender_data <- q_df[q_df$item == offender_item, ]
            
            if(input$loo_pairing_mode == "item_vs_item"){
              suspect_data <- k_df[k_df$item == suspect_item, ]
            } else {
              suspect_data <- k_df[k_df$id == suspect_id, ]
            }
          }
          
          if(input$loo_vali_mode == "full_cross"){
            offender_data <- test_df[test_df$item == offender_item, ]
            
            if(input$loo_pairing_mode == "item_vs_item"){
              suspect_data <- test_df[test_df$item == suspect_item, ]
            } else {
              suspect_data <- test_df[test_df$id == suspect_id, ]
              
              if(validation_table$offender_id[i] == suspect_id){
                suspect_data <- suspect_data[suspect_data$item != offender_item, ]
              }
            }
          }
          
          if(nrow(suspect_data) < 1){
            next
          }
          
          offender_mat <- as.matrix(offender_data[, input$loo_feats, drop = FALSE])
          suspect_mat  <- as.matrix(suspect_data[, input$loo_feats, drop = FALSE])
          
          storage.mode(offender_mat) <- "numeric"
          storage.mode(suspect_mat)  <- "numeric"
          
          lnLR <- MVKD_scorer(
            off_data = offender_mat,
            sus_data = suspect_mat,
            bg_para = loo_MVKD_model
          )
          
          lr_vals <- add_lr_scales(lnLR)
          
          validation_table$lnLR[i]    <- lr_vals$lnLR
          validation_table$LR[i]      <- lr_vals$LR
          validation_table$log10LR[i] <- lr_vals$log10LR
        }
        
        rm(loo_MVKD_model)
        gc(verbose = FALSE)
        
        incProgress(1 / total_keys,
                    detail = paste(progress_counter, "of", total_keys))
      }
    })
    
    loo_results <- validation_table
    loo_results_store(loo_results)
    
    # 5. Outputs
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
    
    ss_llr <- loo_results$lnLR[loo_results$label == SS_LABEL]
    ds_llr <- loo_results$lnLR[loo_results$label == DS_LABEL]
    
    ss_llr <- ss_llr[!is.na(ss_llr)]
    ds_llr <- ds_llr[!is.na(ds_llr)]
    
    cllr_pooled_value <- cllr(ss_llr, ds_llr)
    cllr_min_value <- cllr_min(ss_llr, ds_llr)
    cllr_cal_value <- cllr_cal(ss_llr, ds_llr)
    eer_result <- eer(ss_llr, ds_llr)
    
    output$loo_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled_value, "\n")
      cat("Cllr (min):", cllr_min_value, "\n")
      cat("Cllr (cal):", cllr_cal_value, "\n")
      cat("EER:", eer_result$EER, "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):", eer_result$threshold_raw, "\n")
    })
    
    output$loo_tippettPlot <- renderPlot({
      tippett_plot(
        ss_llr = ss_llr,
        ds_llr = ds_llr,
        x_lab = expression(log[10](Lambda)),
        y_lab = "cumulative proportion"
      )
    })
  })
  
  output$loo_downloadResults <- downloadHandler(
    filename = function() {
      paste("loo_results(", Sys.Date(), ").csv", sep = "")
    },
    content = function(file) {
      req(loo_results_store())
      write.csv(loo_results_store(), file, row.names = FALSE)
    }
  )
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
  
  output$quick_llr_ui <- renderUI({
    req(quickData())
    choices <- names(quickData())
    default <- if("lnLR" %in% choices) "lnLR" else choices[1]
    selectInput("quick_llr", "Select ln(LR) Column", choices = choices, selected = default)
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
    req(quickData(), input$quick_label, input$quick_llr)
    
    quick_data_df <- quickData()
    
    labels <- as.character(quick_data_df[[input$quick_label]])
    llr_vals <- as.numeric(quick_data_df[[input$quick_llr]])
    
    ss_llr <- llr_vals[labels == SS_LABEL]
    ds_llr <- llr_vals[labels == DS_LABEL]
    
    ss_llr <- ss_llr[!is.na(ss_llr)]
    ds_llr <- ds_llr[!is.na(ds_llr)]
    
    cllr_pooled_value <- cllr(ss_llr, ds_llr)
    cllr_min_value <- cllr_min(ss_llr, ds_llr)
    cllr_cal_value <- cllr_cal(ss_llr, ds_llr)
    eer_result <- eer(ss_llr, ds_llr)
    
    output$quick_metrics <- renderPrint({
      cat("Cllr (pooled):", cllr_pooled_value, "\n")
      cat("Cllr (min):", cllr_min_value, "\n")
      cat("Cllr (cal):", cllr_cal_value, "\n")
      cat("EER:", eer_result$EER, "\n")
      cat("EER Threshold (log10):", eer_result$threshold_log10, "\n")
      cat("EER Threshold (raw):", eer_result$threshold_raw, "\n")
    })
    
    output$quick_tippettPlot <- renderPlot({
      tippett_plot(
        ss_llr = ss_llr,
        ds_llr = ds_llr,
        x_lab = expression(log[10](Lambda)), 
        y_lab = "cumulative proportion"
      )
    })
  })
  # ============================================================================
}