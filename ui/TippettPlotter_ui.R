# Path: "ui/TippettPlotter_ui.R"

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
                 numericInput("single_E", "Evidence LR ( raw )", value = NULL, min = 0, step = 0.1),
                 fluidRow(
                   column(6, numericInput("single_x_min", "X-axis min", value = -5)),
                   column(6, numericInput("single_x_max", "X-axis max", value = 5))
                 ),
                 fluidRow(
                   column(6, numericInput("single_y_min", "Y-axis min", value = 0)),
                   column(6, numericInput("single_y_max", "Y-axis max", value = 1))
                 ),
                 numericInput("single_font_size", "Font Size", value = 18, min = 6, max = 24),
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
                 numericInput("multi_font_size", "Font Size", value = 18, min = 6, max = 24),
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
                 numericInput("precision_E", "Evidence LR ( raw )", value = NULL, min = 0, step = 0.1),
                 checkboxInput("precision_symmetric_trial",
                               "Symmetric Pairing",
                               value = FALSE),
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
                 numericInput("precision_font_size", "Font Size", value = 18, min = 6, max = 24),
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