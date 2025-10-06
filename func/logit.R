# Path: "func/logit.R"
# Compute the log-odds (logit) of probability p in (0,1).

logit <- function(p) {
  log(p / (1 - p))
}