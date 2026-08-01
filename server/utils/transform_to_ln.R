# Path: "server/utils/transform_to_ln.R"
# all calibration is based on log-scaled LR

transform_to_ln <- function(x, scale) {
  x <- as.numeric(x)
  if (scale == "Raw") {
    return(log(x))
  } else if (scale == "log10(LR)") {
    return(x * log(10))
  } else if (scale == "ln(LR)") {
    return(x)
  } else {
    stop("Unknown scale provided.")
  }
}