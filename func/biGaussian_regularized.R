# Path: "func/biGaussian_regularized.R"
# R implementation of the Bi-Gaussianized calibration/fusion (regularized version)
# Based on Morrison (2024)

# This implementation combines logistic regression calibration/fusion (regularized version)
# with a bi-Gaussianized procedure in calibrating LR-like scores to achieve well-calibrated
# LR outputs.
# Unlike the robust version, this implementation includes a regularization term
# to stabilize parameter estimation and mitigate overfitting.

# - Workflow:
#   1. Train a regularized logistic regression model on target (ss) and non-target (ds) scores.
#   2. Transform scores into "quasi-scores" (pre-calibrated log-likelihood ratios).
#   3. Estimate the Log-likelihood-ratio Cost (Cllr) and map it to the variance (σ²) 
#      of the target bi-Gaussianized distribution (Morrison, 2024).
#   4. Fit a weighted empirical CDF on the quasi-scores.
#   5. Construct a target bi-Gaussianized CDF model and store both CDFs.
#   6. Use above pipeline to map new scores to calibrated log-LR values by biGaussian_calibrator().

# References:

# Morrison, G. S. (2024). Bi-Gaussianized calibration of likelihood ratios. 
# Law, Probability and Risk, 23(1), 1-34.
# https://doi.org/10.1093/lpr/mgae004

# Input:
#   targets      - [n_ss × d] matrix of log-LR scores for same-source trials
#   non_targets  - [n_ds × d] matrix of log-LR scores for different-source trials
#   prior        - prior probability of the target hypothesis (default = 0.5)
#   kappa        - regularization strength for logistic regression (default = 0)
#   df           - degrees of freedom for regularization (optional)
#   max_iter     - maximum number of iterations for optimization (default = 1000)
#   uncal_score  - [n × d] matrix of uncalibrated log-LR scores to be calibrated
#   grid_k       - range (in multiples of σ) for interpolation grid
#   grid_len     - number of grid points for interpolation (default = 10000)

# Example of the input score matrix:
#         sys-1 sys-2  ...  sys-d
# trial-1 [0.8,  1.0,  ...,  0.9]
# trial-2 [1.5,  1.7,  ...,  1.7]
# ...     [...,  ...,  ...,  ...]
# trial-n [0.3,  1.4,  ...,  0.8]

# Output:
#   train_biGaussian_regularized() -> list containing:
#       fusion_w       - learned LogReg calibration/fusion weights
#       Cllr           - estimated Cllr
#       sigma2_target  - variance of the target bi-Gaussianized distribution
#       weighted_ecdf  - empirical CDF function fitted on quasi-scores
#       bigmm_cdf      - bi-Gaussian CDF function
#
#   biGaussian_regularized() -> list containing:
#       calibrated_lnLR - calibrated log-likelihood ratios for input scores
#       fusion_w        - learned LogReg calibration/fusion weights
#       Cllr            - estimated Cllr
#       sigma2_target   - variance of the target bi-Gaussianized distribution

# ------------------------------------------------------------------------------
# Updated: October 1, 2025
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

source("func/train_llr_fusion_regularized.R")
source("func/biGaussian_calibrator.R")

train_biGaussian_regularized <- function(targets, non_targets,
                                         prior = 0.5, kappa = 0, df = NULL,
                                         max_iter = 1000) {
  
  n1 <- nrow(targets)
  n0 <- nrow(non_targets)
  d  <- ncol(targets)
  if (ncol(non_targets) != d) stop("Mismatch in score dimension (calibration set).")
  
  # Train a LogReg fusion/calibration model (regularized)
  fusion_w <- train_llr_fusion_regularized(
    targets = targets,
    non_targets = non_targets,
    prior = prior,
    kappa = kappa,
    df = df,
    max_iter = max_iter
  )
  beta  <- fusion_w[1:d]
  alpha <- fusion_w[d+1]
  
  # Pre-calibrated (quasi) score
  quasi_ss <- as.vector(targets %*% beta + alpha)
  quasi_ds <- as.vector(non_targets %*% beta + alpha)
  
  # Estimate Cllr
  cal_ss_lr <- exp(quasi_ss)
  cal_ds_lr <- exp(quasi_ds)
  punish_ss <- log(1 + 1 / cal_ss_lr, base = 2)
  punish_ds <- log(1 + cal_ds_lr, base = 2)
  Cllr <- 0.5 * (mean(punish_ss) + mean(punish_ds))
  
  # Map the Cllr into the sigma2 of the target bi-Gaussianized model
  b <- 17.665396790464737
  c <-  0.009333834837656
  sigma2_target <- - log((log(Cllr) / b) + 1) / c
  sigma_target  <- sqrt(sigma2_target)
  half_sigma2   <- sigma2_target / 2
  
  # Use the quasi score to fit a weighted ECDF function
  w_ss <- rep(1 / ((n1 + 1) * 2), n1)
  w_ds <- rep(1 / ((n0 + 1) * 2), n0)
  ecdf_w <- c(w_ds, w_ss)
  
  quasi_cal <- c(quasi_ds, quasi_ss)
  ord <- order(quasi_cal)
  sorted_scores <- quasi_cal[ord]
  sorted_weights <- ecdf_w[ord]
  
  unique_df <- aggregate(sorted_weights ~ sorted_scores, FUN = sum)
  unique_df <- unique_df[order(unique_df$sorted_scores), ]
  weighted_ecdf <- approxfun(unique_df$sorted_scores, cumsum(unique_df$sorted_weights), rule = 2)
  
  # Fit the target bi-Gaussianized CDF function
  bigmm_cdf <- function(x) {
    0.5 * stats::pnorm(x, mean = -half_sigma2, sd = sigma_target) +
      0.5 * stats::pnorm(x, mean =  +half_sigma2, sd = sigma_target)
  }
  
  # Return the model
  list(
    fusion_w = fusion_w,
    Cllr = Cllr,
    sigma2_target = sigma2_target,
    weighted_ecdf = weighted_ecdf,
    bigmm_cdf = bigmm_cdf
  )
}

biGaussian_regularized <- function(uncal_score, targets, non_targets,
                                   prior = 0.5, kappa = 0, df = NULL,
                                   max_iter = 1000,
                                   grid_k = 8,
                                   grid_len = 10000) {
  
  # Train a bi-Gaussianized model
  model <- train_biGaussian_regularized(
    targets = targets,
    non_targets = non_targets,
    prior = prior,
    kappa = kappa,
    df = df,
    max_iter = max_iter
  )
  
  # Calibration
  calibrated_lnLR <- biGaussian_calibrator(
    model = model,
    uncal_score = uncal_score,
    grid_k = grid_k,
    grid_len = grid_len
  )
  
  list(
    calibrated_lnLR = calibrated_lnLR,
    fusion_w = model$fusion_w,
    Cllr = model$Cllr,
    sigma2_target = model$sigma2_target
  )
}
