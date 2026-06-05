# Path: "ui/MVKDLR_ui.R"

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
                 fileInput("single_off_df", "Upload Questioned-item Data ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 fileInput("single_sus_df", "Upload Known-item Data ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 fileInput("single_bg_df", "Upload the Reference Dataset ( .csv )",
                           accept = ".csv", multiple = FALSE),
                 uiOutput("single_bg_id_ui"),
                 hr(),
                 uiOutput("single_feat_ui"),
                 actionButton("single_calc", "Calculate LR", class = "btn-primary")
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                          h5("Questioned-item Data Preview:"),
                          DTOutput("single_offPreview")
                   ),
                   column(6,
                          h5("Known-item Data Preview:"),
                          DTOutput("single_susPreview")
                   )
                 ),
                 fluidRow(
                   column(12,
                          h5("Reference Dataset Preview:"),
                          DTOutput("single_bgPreview")
                   )
                 ),
                 hr(),
                 h5("raw LR:"),
                 verbatimTextOutput("single_res_LR"),
                 h5("ln(LR):"),
                 verbatimTextOutput("single_res_lnLR"),
                 h5("log10(LR):"),
                 verbatimTextOutput("single_res_log10LR")
               )
             )
    ),
    # ==========================================================================
    
    # ======================== Hold-out Validation (UI) ========================
    tabPanel("Hold-out Validation",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   selectInput("hv_vali_mode", "Select Validation Mode",
                               choices = c("Cross-condition" = "qk_cross",
                                           "Fully-cross" = "full_cross"),
                               selected = "qk_cross"),
                   
                   selectInput("hv_pairing_mode", "Select Pairing Mode",
                               choices = c("Item vs Item" = "item_vs_item",
                                           "Item vs Suspect" = "item_vs_suspect"),
                               selected = "item_vs_item")
                 ),
                 wellPanel(
                   h5("Training/Reference Set"),
                   fileInput("hv_bg_df", "Upload the Dataset ( .csv )",
                             accept = ".csv", multiple = FALSE),
                   uiOutput("hv_bg_id_ui")
                 ),
                 uiOutput("hv_validation_upload_ui"),
                 hr(),
                 uiOutput("hv_feat_ui"),
                 actionButton("hv_calc", "Start Validation", class = "btn-primary"),
                 hr(),
                 downloadButton("hv_downloadResults", "Download Results")
               ),
               mainPanel(
                 fluidRow(
                   column(12,
                          h5("Training/Reference Set Preview:"),
                          DTOutput("hv_trainPreview")
                   )
                 ),
                 hr(),
                 uiOutput("hv_validation_preview_ui"),
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
    
    # ================ Leave-one/two-out Cross Validation (UI) =================
    tabPanel("Leave-one/two-out Cross Validation",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   selectInput("loo_vali_mode", "Select Validation Mode",
                               choices = c("Cross-condition" = "qk_cross",
                                           "Fully-cross" = "full_cross"),
                               selected = "qk_cross"),
                   
                   selectInput("loo_pairing_mode", "Select Pairing Mode",
                               choices = c("Item vs Item" = "item_vs_item",
                                           "Item vs Suspect" = "item_vs_suspect"),
                               selected = "item_vs_item")
                 ),
                 uiOutput("loo_validation_upload_ui"),
                 hr(),
                 uiOutput("loo_feat_ui"),
                 actionButton("loo_calc", "Start Validation", class = "btn-primary"),
                 hr(),
                 downloadButton("loo_downloadResults", "Download Results")
               ),
               
               mainPanel(
                 uiOutput("loo_validation_preview_ui"),
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
                 fileInput("quick_result",
                           "Upload Validation Result ( .csv )", accept = ".csv"),
                 uiOutput("quick_label_ui"),
                 uiOutput("quick_llr_ui"),
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