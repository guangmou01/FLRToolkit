# ------------------------------------------------------------------------------
# Updated: 2026/06/04
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------
APP_VERSION <- "Version 8.0.0"
SS_LABEL <- "ss"
DS_LABEL <- "ds"

if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("DT")) install.packages("DT", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

library(shiny)
library(DT)
library(ggplot2)

source("scoring/MVKD.R")
source("metric/Cllr.R")
source("metric/EER.R")
source("metric/TippettPlot.R")

source("ui/MVKDLR_ui.R", local = TRUE)
source("server/MVKDLR_server.R", local = TRUE)

options(shiny.maxRequestSize = 30 * 1024^2)
shinyApp(ui = ui, server = server)
