# Path: "calibration/logit.R"
# Compute the log-odds (logit) of probability p in (0,1).
# for fitting LogReg weights.

# ------------------------------------------------------------------------------
# Updated: 2026/06/02
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

logit <- function(p) {
  log(p / (1 - p))
}