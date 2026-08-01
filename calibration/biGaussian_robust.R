# Path: "calibration/biGaussian_robust.R"
# R implementation of the Bi-Gaussianized calibration/fusion (robust LogReg
# variant) based on Morrison (2024)

# This implementation combines robust logistic regression calibration/fusion
# (Morrison, 2009) with a bi-Gaussianized procedure.

# Workflow:
# 1. Fit a robust logistic regression model on target (ss) and non-target (ds) scores.
# 2. Transform scores into quasi-scores (pre-calibrated lnLR).
# 3. Estimate the log-likelihood-ratio Cost (Cllr) and map it to the variance (σ²) 
#    of the target bi-Gaussian distribution (Morrison, 2024).
# 4. Fit a weighted empirical CDF on the quasi-scores.
# 5. Construct a target bi-Gaussian mixture CDF and store both CDFs.
# 6. Use above pipeline to map new scores to calibrated lnLR by biGaussian_calibrator().
#
# References:
# Morrison, G. S. (2009).
# Robust version of train_llr_fusion.m from Niko Brümmer’s FoCal Toolbox [MATLAB script].
# https://geoff-morrison.net/#TrainFus
# Morrison, G. S. (2024).
# Bi-Gaussianized calibration of likelihood ratios. 
# Law, Probability and Risk, 23(1), 1-34.
# https://doi.org/10.1093/lpr/mgae004
#
# Input:
# @param targets:
#        [n_ss × d] matrix of log-LR scores for same-source trials.
# @param non_targets:
#        [n_ds × d] matrix of log-LR scores for different-source trials.
# @param prior:
#        prior probability of the target hypothesis (default = 0.5).
# @param robust_weight:
#        robustness weight for class imbalance and outlier resistance.
# @param max_iter:
#        maximum number of iterations for optimization.
#
# @param uncal_score:
#        [n × d] matrix of uncalibrated log-LR scores to be calibrated.
# @param grid_k: 
#        range (in multiples of σ) for constructing the interpolation grid.
# @param grid_len: 
#        number of grid points (default = 10000).
#
# Example of the input score matrix:
#         sys-1 sys-2  ...  sys-d
# trial-1 [0.8,  1.0,  ...,  0.9],
# trial-2 [1.5,  1.7,  ...,  1.7],
# ...     [...,  ...,  ...,  ...],
# trial-n [0.3,  1.4,  ...,  0.8]
#
# Output:
# train_biGaussian_regularized() -> list containing:
# @param fusion_w:
#        fitted LogReg calibration/fusion weights.
# @param cllr_target:
#        Cllr of the target bi-Gaussian distribution.
# @param sigma2_target:
#        variance of the target bi-Gaussian distribution.
# @param weighted_ecdf:
#        empirical CDF function fitted on quasi-scores.
# @param bigmm_cdf:
#        target bi-Gaussian CDF function.
#
# biGaussian_regularized() -> list containing:
# @param calibrated_lnLR:
#        calibrated natural-log-likelihood ratios for input scores.
# @param fusion_w:
#        fitted LogReg calibration/fusion weights.
# @param cllr_target:
#        Cllr of the target bi-Gaussian distribution.
# @param sigma2_target:
#        variance of the target bi-Gaussian distribution.
#
# ------------------------------------------------------------------------------
# Updated: 2026/06/26
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

source("calibration/train_llr_fusion_robust.R")
source("calibration/biGaussian_calibrator.R")

train_biGaussian_robust <- function(targets, non_targets,
                                    prior = 0.5, robust_weight = 0,
                                    max_iter = 5000) {
  
  n1 <- nrow(targets)
  n0 <- nrow(non_targets)
  d  <- ncol(targets)
  if (ncol(non_targets) != d) stop("Mismatch in score dimension (calibration set).")
  
  # train a robust logistic-regression fusion/calibration model
  fusion_w <- train_llr_fusion_robust(
    targets = targets,
    non_targets = non_targets,
    prior = prior,
    robust_weight = robust_weight,
    max_iter = max_iter
  )
  beta  <- fusion_w[1:d]
  alpha <- fusion_w[d+1]
  
  # pre-calibrated (quasi) score
  quasi_ss <- as.vector(targets %*% beta + alpha)
  quasi_ds <- as.vector(non_targets %*% beta + alpha)
  
  # estimate the target Cllr
  punish_ss <- log1p(exp(-quasi_ss)) / log(2)
  punish_ds <- log1p(exp(quasi_ds))  / log(2)
  cllr_target <- 0.5 * (mean(punish_ss) + mean(punish_ds))
  
  # map the target Cllr into the sigma2 of the target bi-Gaussianized model
  b <- 17.665396790464737
  c <-  0.009333834837656
  sigma2_target <- - log((log(cllr_target) / b) + 1) / c
  sigma_target  <- sqrt(sigma2_target)
  half_sigma2   <- sigma2_target / 2
  
  # use the quasi score to fit a weighted ECDF function
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
  
  # fit the target bi-Gaussianized CDF function
  bigmm_cdf <- function(x) {
    0.5 * stats::pnorm(x, mean = -half_sigma2, sd = sigma_target) +
      0.5 * stats::pnorm(x, mean =  +half_sigma2, sd = sigma_target)
  }
  
  # return the model
  list(
    fusion_w = fusion_w,
    cllr_target = cllr_target,
    sigma2_target = sigma2_target,
    weighted_ecdf = weighted_ecdf,
    bigmm_cdf = bigmm_cdf
  )
}

biGaussian_robust <- function(uncal_score, targets, non_targets,
                              prior = 0.5, robust_weight = 0,
                              max_iter = 5000,
                              grid_k = 8,
                              grid_len = 10000) {
  
  # train a bi-Gaussianized model
  model <- train_biGaussian_robust(
    targets = targets,
    non_targets = non_targets,
    prior = prior,
    robust_weight = robust_weight,
    max_iter = max_iter
  )
  
  # calibration
  calibrated_lnLR <- biGaussian_calibrator(
    model = model,
    uncal_score = uncal_score,
    grid_k = grid_k,
    grid_len = grid_len
  )
  
  list(
    calibrated_lnLR = calibrated_lnLR,
    fusion_w = model$fusion_w,
    cllr_target = model$cllr_target,
    sigma2_target = model$sigma2_target
  )
}
