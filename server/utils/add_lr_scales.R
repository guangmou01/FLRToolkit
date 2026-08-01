# Path: "server/utils/add_lr_scales.R"

add_lr_scales <- function(lnLR){
  LR <- exp(lnLR)
  log10LR <- lnLR / log(10)
  return(list(lnLR = lnLR, LR = LR, log10LR = log10LR))
}