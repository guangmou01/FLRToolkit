# Path: "ui/Bi-GaussCalibrator_ui.R"

ui <- fluidPage(
  titlePanel(
    tagList(
      "Bi-Gaussianized Calibrator",
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
                 numericInput("single_grid_k", "Search Range ( σ Multiples )",
                              value = 4, min = 1, step = 1),
                 numericInput("single_grid_len", "Interpolation Grid Points",
                              value = 10000, min = 100, step = 100),
                 hr(),
                 checkboxInput("single_zscore", "Apply Z-score Transformation", value = FALSE),
                 actionButton("single_run_calibration", "Run Calibration", class = "btn-primary")
               ),
               mainPanel(
                 h5("Calibration Set Preview:"),
                 DTOutput("single_cal_set_preview"),
                 hr(),
                 h5("Parameters of the Bi-Gaussianized Step:"),
                 verbatimTextOutput("single_paras"),
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
                 numericInput("hv_grid_k", "Search Range ( σ Multiples )",
                              value = 4, min = 1, step = 1),
                 numericInput("hv_grid_len", "Interpolation Grid Points",
                              value = 10000, min = 100, step = 100),
                 hr(),
                 checkboxInput("hv_zscore", "Apply Z-score Transformation", value = FALSE),
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
                 numericInput("loo_grid_k", "Search Range ( σ Multiples )",
                              value = 4, min = 1, step = 1),
                 numericInput("loo_grid_len", "Interpolation Grid Points",
                              value = 10000, min = 100, step = 100),
                 hr(),
                 checkboxInput("loo_zscore", "Apply Z-score Transformation", value = FALSE),
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