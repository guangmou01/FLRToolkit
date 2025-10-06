# Path: "func/MVKD.R"
# R implementation of MVKD-based LR function for forensic interpretation
# Based on Aitken & Lucy (2004), and Morrison (2007)

# References:

# Aitken, C. G. G., & Lucy, D. (2004).
# Evaluation of trace evidence in the form of multivariate data.
# Journal of the Royal Statistical Society: Series C (Applied Statistics), 53(1), 109–122.
# https://doi.org/10.1046/j.0035-9254.2003.05271.x

# Morrison, G. S. (2007).
# Matlab implementation of Aitken & Lucy’s (2004) forensic likelihood-ratio software 
# using multivariate-kernel-density estimation (Version 17/07/2008) [MATLAB script].
# http://geoff-morrison.net/#MVKD

# Input:
# off_data - offender measurements (rows = observations, columns = features) [numeric matrix]
# sus_data - suspect measurements (rows = observations, columns = features) [numeric matrix]
#  bg_data - background dataset [dataframe]
#            1. the first column must be the ID labels [character or numeric]
#            2. the remaining columns are features [numeric]

# Output:
#  bg_para - background parameters estimated from bg_data [list]
#      llr - likelihood ratio in log10-scale [numeric]

# ------------------------------------------------------------------------------
# Updated: September 4, 2025
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

# sum of squared errors
sse <- function(x) {
  m <- nrow(x)
  xc <- sweep(x, 2, colMeans(x))
  return(t(xc) %*% xc)
}

# function used for estimating background parameters
MVKD_train <- function(bg_data) {
  if (missing(bg_data)) {
    stop("Please provide [bg_data].")
  }

  bg_ids  <- as.factor(bg_data[[1]])  # the 1st column should be ID labels
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

# function used for scoring log10LR by background parameters
MVKD_scorer <- function(off_data, sus_data, bg_para) {
  if (missing(off_data) || missing(sus_data) || missing(bg_para)) {
    stop("Please provide [off_data], [sus_data], and [bg_para].")
  }
  
  off_data <- as.matrix(off_data)
  sus_data <- as.matrix(sus_data)
  
  if (ncol(off_data) != ncol(sus_data)) {
    stop("The column number of [off_data] and [sus_data] is inconsistent!")
  }
  if (ncol(off_data) != bg_para$num_variables) {
    stop("The column number of [off_data] and [bg_data] (after removing the id column) is inconsistent!")
  }

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
  
  kernel_density_at_typicality <- 0
  dist_backindividuals_to_suspect <- 0
  dist_backindividuals_to_offender <- 0
  
  for (i in 1:num_individuals) {
    current_mean <- matrix(means_mat[i, ], ncol = 1)
    
    typicality <- suspect_offender_mean_typicality - current_mean
    M <- solve(solve(offender_covar_inv + suspect_covar_inv) + kernel)
    kernel_density_at_typicality <- kernel_density_at_typicality +
      exp(-0.5 * t(typicality) %*% M %*% typicality)
    
    dist_to_suspect <- suspect_mean - current_mean
    M_suspect <- solve(suspect_covar + kernel)
    dist_backindividuals_to_suspect <- dist_backindividuals_to_suspect +
      exp(-0.5 * t(dist_to_suspect) %*% M_suspect %*% dist_to_suspect)
    
    dist_to_offender <- offender_mean - current_mean
    M_offender <- solve(offender_covar + kernel)
    dist_backindividuals_to_offender <- dist_backindividuals_to_offender +
      exp(-0.5 * t(dist_to_offender) %*% M_offender %*% dist_to_offender)
  }
  
  # numerator
  num1 <- (2 * pi)^(-num_variables) *
    (det(offender_covar))^(-0.5) *
    (det(suspect_covar))^(-0.5) *
    abs(det(C))^(-0.5)
  num2 <- (num_individuals * smoothing_parameter^num_variables)^(-1)
  num3 <- abs(det(offender_covar_inv + suspect_covar_inv + inv_kernel))^(-0.5)
  num4 <- exp(-0.5 * t(suspect_offender_mean_difference) %*%
                solve(offender_covar + suspect_covar) %*%
                suspect_offender_mean_difference)
  
  numerator <- num1 * num2 * num3 * num4 * kernel_density_at_typicality
  
  # denominator
  denom1 <- (2 * pi)^(-num_variables) * abs(det(C))^(-1)
  denom2 <- (num_individuals * smoothing_parameter^num_variables)^(-2)
  denom3 <- (det(offender_covar))^(-0.5) *
    abs(det(offender_covar_inv + inv_kernel))^(-0.5) *
    dist_backindividuals_to_offender
  denom4 <- (det(suspect_covar))^(-0.5) *
    abs(det(suspect_covar_inv + inv_kernel))^(-0.5) *
    dist_backindividuals_to_suspect
  
  denominator <- denom1 * denom2 * denom3 * denom4
  
  likelihood_ratio <- numerator / denominator
  
  # numerical safeguards
  if (likelihood_ratio == 0) {
    likelihood_ratio <- .Machine$double.xmin
  }
  
  llr <- log10(likelihood_ratio)
  
  if (is.infinite(llr) && llr > 0) {
    llr <- log(.Machine$double.xmax) / log(10)
  }
  if (is.infinite(llr) && llr < 0) {
    llr <- log(.Machine$double.xmin) / log(10)
  }
  
  return(as.numeric(llr))
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
