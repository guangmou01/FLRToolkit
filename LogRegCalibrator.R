# ------------------------------------------------------------------------------
# Updated: 2026/06/06
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------
APP_VERSION <- "Version 2.0.6"
SS_LABEL <- "ss"
DS_LABEL <- "ds"

if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("DT")) install.packages("DT", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

library(shiny)
library(DT)
library(ggplot2)

source("calibration/train_llr_fusion_robust.R")
source("calibration/train_llr_fusion_regularized.R")
source("calibration/lin_fusion.R")
source("metric/Cllr.R")
source("metric/EER.R")
source("metric/TippettPlot.R")

source("ui/LogRegCalibrator_ui.R", local = TRUE)
source("server/LogRegCalibrator_server.R", local = TRUE)

options(shiny.maxRequestSize = 600*1024^2)
shinyApp(ui = ui, server = server)
