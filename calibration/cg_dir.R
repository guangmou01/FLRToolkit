# Path: "calibration/cg_dir.R"
# Conjugate-gradient method in logistic-regression training.
#
# ------------------------------------------------------------------------------
# Updated: 2026/06/26
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

cg_dir <- function(old_dir, grad, old_grad) {
  g <- grad
  
  grad <- as.vector(grad)
  old_grad <- as.vector(old_grad)
  old_dir_vec <- as.vector(old_dir)
  
  delta <- grad - old_grad
  den <- sum(old_dir_vec * delta)
  
  if (den == 0) {
    return(g * 0)
  } else {
    beta <- sum(grad * delta) / den
    return(g - beta * old_dir)
  }
}