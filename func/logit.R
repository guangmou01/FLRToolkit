# Path: "func/logit.R"
# Compute the log-odds (logit) of probability p in (0,1).
# For training LogReg weights.

logit <- function(p) {
  log(p / (1 - p))
}