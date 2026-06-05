# Path: "ui/ECEPlotter_ui.R"

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
                 checkboxInput("show_legend", "Show Legend", value = FALSE),
                 numericInput("single_font_size", "Font Size", value = 18, min = 6, max = 24),
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