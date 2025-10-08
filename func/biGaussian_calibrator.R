# Path: "func/biGaussian_calibrator.R"
# R implementation of the Bi-Gaussianized calibration/fusion (calibrator module)
# Based on Morrison (2024)

# R implementation of the calibration stage for Bi-Gaussianized calibration/fusion,
# based on Morrison (2024). This module maps uncalibrated LR-like scores into
# calibrated log-likelihood ratios (lnLR) using the model pre-trained by
# train_biGaussian_robust() or train_biGaussian_regularized().

# Workflow:
#   1. Apply the logistic regression fusion weights to transform the 
#      uncalibrated scores into quasi-scores (pre-calibrated lnLR).
#   2. Map the quasi-scores into their empirical cumulative distribution (ECDF) values
#      based on the calibration data.
#   3. Use the target bi-Gaussianized cumulative distribution function (CDF) derived 
#      from training to invert ECDF values into well-calibrated lnLR values.

# References:

# Morrison, G. S. (2024). Bi-Gaussianized calibration of likelihood ratios. 
# Law, Probability and Risk, 23(1), 1-34.
# https://doi.org/10.1093/lpr/mgae004

# Input:
#   uncal_score  - [n × d] numeric matrix of uncalibrated scores to be calibrated 
#                  (n = trials, d = number of systems)
#   model        - list returned by train_biGaussian_*() containing:
#                   fusion_w      - learned LogReg calibration/fusion weights
#                   Cllr          - estimated Cllr
#                   sigma2_target - variance of the bi-Gaussianized distribution
#                   weighted_ecdf - empirical CDF function of quasi-scores
#                   bigmm_cdf     - bi-Gaussianized CDF function
#   grid_k       - range (in multiples of σ) for constructing the interpolation grid
#   grid_len     - number of grid points (default = 10000)

# Example of the input score matrix:
#         sys-1 sys-2  ...  sys-d
# trial-1 [0.8,  1.0,  ...,  0.9],
# trial-2 [1.5,  1.7,  ...,  1.7],
# ...     [...,  ...,  ...,  ...],
# trial-n [0.3,  1.4,  ...,  0.8]

# Output:
#   calibrated_lnLR - [n × 1] numeric matrix of calibrated log-likelihood-
#                     ratios corresponding to uncalibrated input scores.

# ------------------------------------------------------------------------------
# Updated: September 30, 2025
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

biGaussian_calibrator <- function(uncal_score, model,
                                  grid_k = 8, grid_len = 10000) {
  
  fusion_w <- model$fusion_w
  sigma2_target <- model$sigma2_target
  weighted_ecdf <- model$weighted_ecdf
  bigmm_cdf <- model$bigmm_cdf
  
  d <- length(fusion_w) - 1
  
  X <- as.matrix(uncal_score)
  if (ncol(X) != d) stop("Mismatch in score dimension (uncalibrated score).")
  
  beta  <- fusion_w[1:d]
  alpha <- fusion_w[d+1]
  
  # 1. Uncalibrated score to quasi score
  quasi_uncal <- as.vector(X %*% beta + alpha)
  
  # 2. Quasi score to the ECDF value
  qvals <- weighted_ecdf(quasi_uncal)
  
  # 3. ECDF value to calibrated score (lnLR)
  sigma_target <- sqrt(sigma2_target)
  
  grid <- seq(-grid_k * sigma_target, grid_k * sigma_target,
              length.out = grid_len)
  target_cdf <- bigmm_cdf(grid)
  
  unique_idx <- !duplicated(target_cdf)
  target_cdf_unique <- target_cdf[unique_idx]
  grid_unique <- grid[unique_idx]
  
  calibrated_lnLR <- approx(x = target_cdf_unique, y = grid_unique,
                            xout = qvals, rule = 2)$y
  
  if (length(calibrated_lnLR) == 1) {
    return(as.numeric(calibrated_lnLR))
  } else {
    return(matrix(calibrated_lnLR, ncol = 1))
  }
}
