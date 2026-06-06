# ------------------------------------------------------------------------------
# Updated: 2026/06/06
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------
APP_VERSION <- "Version 3.0.1"
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

source("ui/TippettPlotter_ui.R", local = TRUE)
source("server/TippettPlotter_server.R", local = TRUE)

options(shiny.maxRequestSize = 300*1024^2)
shinyApp(ui = ui, server = server)

