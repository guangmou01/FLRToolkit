# Path: "scoring/MVKD.R"
# R implementation of MVKD-based LR function
# based on Aitken & Lucy (2004), and Morrison (2007)
#
# References:
# Aitken, C. G. G., & Lucy, D. (2004).
# Evaluation of trace evidence in the form of multivariate data.
# Journal of the Royal Statistical Society: Series C (Applied Statistics), 53(1), 109–122.
# https://doi.org/10.1046/j.0035-9254.2003.05271.x
# Morrison, G. S. (2007).
# Matlab implementation of Aitken & Lucy’s (2004) forensic likelihood-ratio software 
# using multivariate-kernel-density estimation (Version 17/07/2008) [MATLAB script].
# http://geoff-morrison.net/#MVKD
#
# Input:
# @param off_data:
#        offender measurements (rows = observations, columns = features) [numeric matrix].
# @param sus_data:
#        suspect measurements (rows = observations, columns = features) [numeric matrix].
# @param bg_data:
#        background/reference dataset [dataframe]:
#        1. the first column must be the ID labels [character or numeric].
#        2. the remaining columns are features [numeric].
#
# Output:
# @param bg_para:
#        background parameters estimated from bg_data [list].
#
# @param llr:
#        natural-log-likelihood-ratio [numeric].
#
# ------------------------------------------------------------------------------
# Updated: 2026/10/27
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

# stable log-sum-exp calculation
log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

# log absolute determinant
log_abs_det <- function(x) {
  as.numeric(determinant(x, logarithm = TRUE)$modulus)
}

# sum of squared errors
sse <- function(x) {
  m <- nrow(x)
  xc <- sweep(x, 2, colMeans(x))
  return(t(xc) %*% xc)
}

# function used for estimating background parameters
MVKD_train <- function(bg_data) {
  
  bg_ids  <- as.factor(bg_data[[1]])  # the 1st column must be ID labels
  bg_meas <- as.matrix(bg_data[, -1, drop = FALSE])
  
  num_observations <- nrow(bg_meas)
  num_variables    <- ncol(bg_meas)
  
  individual_factor  <- as.factor(bg_ids)
  individual_indices <- as.integer(individual_factor)
  individual_names   <- levels(individual_factor)
  num_individuals    <- length(individual_names)
  
  background_data_per_individual <- vector("list", num_individuals)
  for (i in 1:num_individuals) {
    background_data_per_individual[[i]] <- bg_meas[individual_indices == i, , drop = FALSE]
  }
  
  # mean [individual x matrix]
  background_means <- do.call(
    rbind,
    lapply(background_data_per_individual, function(x) colMeans(x))
  )
  
  # U: within covariance
  within_covar_list <- lapply(background_data_per_individual, sse)
  within_covar_sum  <- Reduce("+", within_covar_list)
  within_covar      <- within_covar_sum / (num_observations - num_individuals)
  
  background_within_covar_list <- lapply(background_data_per_individual, cov)
  background_within_covar_sum  <- Reduce("+", background_within_covar_list)
  background_within_covar      <- background_within_covar_sum / num_observations
  
  # C: between covariance
  background_between_covar_raw <- cov(background_means)
  background_between_covar     <- background_between_covar_raw - background_within_covar
  
  bg_para <- list(
    within_covar       = within_covar,
    between_covar      = background_between_covar,
    background_means   = background_means,
    num_individuals    = num_individuals,
    num_variables      = num_variables
  )
  return(bg_para)
}

# function used for scoring ln(LR) by background parameters
MVKD_scorer <- function(off_data, sus_data, bg_para) {
  
  off_data <- as.matrix(off_data)
  sus_data <- as.matrix(sus_data)
  
  suspect_num_measures  <- nrow(sus_data)
  offender_num_measures <- nrow(off_data)
  
  suspect_mean  <- as.matrix(colMeans(sus_data))
  offender_mean <- as.matrix(colMeans(off_data))
  
  # U/n:
  within_covar <- bg_para$within_covar
  suspect_covar  <- within_covar / suspect_num_measures
  offender_covar <- within_covar / offender_num_measures
  
  suspect_covar_inv  <- solve(suspect_covar)
  offender_covar_inv <- solve(offender_covar)
  
  # mean distance
  suspect_offender_mean_difference <- offender_mean - suspect_mean
  
  A <- offender_covar_inv + suspect_covar_inv
  b <- solve(offender_covar, offender_mean) + solve(suspect_covar, suspect_mean)
  suspect_offender_mean_typicality <- solve(A, b)
  
  # kernel settings
  num_variables   <- bg_para$num_variables
  num_individuals <- bg_para$num_individuals
  C               <- bg_para$between_covar
  means_mat       <- bg_para$background_means
  
  smooth_power        <- 1 / (num_variables + 4)
  smoothing_parameter <- (4 / (2 * num_variables + 1))^smooth_power * num_individuals^(-smooth_power)
  
  kernel     <- smoothing_parameter^2 * C
  inv_kernel <- solve(kernel)
  
  log_kernel_typicality <- numeric(num_individuals)
  log_kernel_suspect <- numeric(num_individuals)
  log_kernel_offender <- numeric(num_individuals)
  
  for (i in 1:num_individuals) {
    current_mean <- matrix(means_mat[i, ], ncol = 1)
    
    typicality <- suspect_offender_mean_typicality - current_mean
    M <- solve(solve(offender_covar_inv + suspect_covar_inv) + kernel)
    log_kernel_typicality[i] <-
      -0.5 * drop(t(typicality) %*% M %*% typicality)
    
    dist_to_suspect <- suspect_mean - current_mean
    M_suspect <- solve(suspect_covar + kernel)
    log_kernel_suspect[i] <-
      -0.5 * drop(t(dist_to_suspect) %*% M_suspect %*% dist_to_suspect)
    
    dist_to_offender <- offender_mean - current_mean
    M_offender <- solve(offender_covar + kernel)
    log_kernel_offender[i] <-
      -0.5 * drop(t(dist_to_offender) %*% M_offender %*% dist_to_offender)
  }
  
  # Numerator
  log_num1 <- -num_variables * log(2 * pi) -
    0.5 * log_abs_det(offender_covar) -
    0.5 * log_abs_det(suspect_covar) -
    0.5 * log_abs_det(C)
  
  log_num2 <- -log(num_individuals * smoothing_parameter^num_variables)
  
  log_num3 <- -0.5 * log_abs_det(
    offender_covar_inv + suspect_covar_inv + inv_kernel
  )
  
  log_num4 <- -0.5 * drop(
    t(suspect_offender_mean_difference) %*%
      solve(offender_covar + suspect_covar) %*%
      suspect_offender_mean_difference
  )
  
  log_numerator <- log_num1 + log_num2 + log_num3 + log_num4 +
    log_sum_exp(log_kernel_typicality)
  
  # Denominator
  log_denom1 <- -num_variables * log(2 * pi) - log_abs_det(C)
  
  log_denom2 <- -2 * log(num_individuals * smoothing_parameter^num_variables)
  
  log_denom3 <- -0.5 * log_abs_det(offender_covar) -
    0.5 * log_abs_det(offender_covar_inv + inv_kernel) +
    log_sum_exp(log_kernel_offender)
  
  log_denom4 <- -0.5 * log_abs_det(suspect_covar) -
    0.5 * log_abs_det(suspect_covar_inv + inv_kernel) +
    log_sum_exp(log_kernel_suspect)
  
  log_denominator <- log_denom1 + log_denom2 + log_denom3 + log_denom4
  
  # natural-log-likelihood-ratio
  llr <- as.numeric(log_numerator - log_denominator)
  
  return(llr)
}

# wrapper function
MVKD_llr <- function(off_data, sus_data, bg_data) {
  if (missing(off_data) || missing(sus_data) || missing(bg_data)) {
    stop("Please provide [off_data], [sus_data], and [bg_data].")
  }
  
  off_data <- as.matrix(off_data)
  sus_data <- as.matrix(sus_data)
  bg_meas  <- as.matrix(bg_data[, -1, drop = FALSE])
  
  if (ncol(off_data) != ncol(sus_data)) {
    stop("The column number of [off_data] and [sus_data] is inconsistent!")
  }
  if (ncol(off_data) != ncol(bg_meas)) {
    stop("The column number of [off_data] and [bg_data] (after removing the id column) is inconsistent!")
  }
  
  bg_para <- MVKD_train(bg_data)
  llr <- MVKD_scorer(off_data, sus_data, bg_para)
  
  return(llr)
}
