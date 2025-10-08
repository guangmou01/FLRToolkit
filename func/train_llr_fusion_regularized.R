# Path: "func/train_llr_fusion_regularized.R"
# R implementation of regularized LogReg from Focal toolkit for LR calibration & fusion
# Based on Morrison’s MATLAB implementation (2017)

# References:

# Brümmer, N. (2005).
# Focal Toolbox [MATLAB script].
# http://www.dsp.sun.ac.za/nbrummer/focal

# Morrison, G. S. (2017).
# Regularized version of train_llr_fusion.m from Niko Brümmer’s FoCal Toolbox [MATLAB script].
# https://geoff-morrison.net

# Morrison, G. S., & Poh, N. (2018).
# Avoiding overstating the strength of forensic evidence: Shrunk likelihood ratios/Bayes factors.
# Science & Justice, 58(3), 200–218.
# https://doi.org/10.1016/j.scijus.2017.12.005

# Input:
#       targets - scores from target (same-source) trials 
#                 (rows = trials, columns = systems) [numeric matrix]
#   non_targets - scores from non-target (different-source) trials 
#                 (rows = trials, columns = systems) [numeric matrix]
#         prior - prior probability of the target hypothesis [numeric scalar, default = 0.5]
#         kappa - regularization strength (larger values impose stronger shrinkage 
#                 towards the flat prior, reducing overfitting) [numeric scalar, default = 0]
#            df - effective degrees of freedom used to scale the flat-prior penalty
#                 if NULL, defaults to the total number of trials (nt + nn) [integer]
#      max_iter - Max iteration number for Conjugate Gradient [integer, default = 1000]

# Example of the input score matrix:
#         sys-1 sys-2  ...  sys-d
# trial-1 [0.8,  1.0,  ...,  0.9],
# trial-2 [1.5,  1.7,  ...,  1.7],
# ...     [...,  ...,  ...,  ...],
# trial-n [0.3,  1.4,  ...,  0.8]

# Output:
#             w - weight vector for logistic regression fusion [numeric vector, length = d + 1]
#                 (d system weights + 1 bias term)

# ------------------------------------------------------------------------------
# Updated: September 26, 2025
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

source("func/logit.R")
source("func/cg_dir.R")

train_llr_fusion_regularized <- function(targets, non_targets,
                                         prior = 0.5, kappa = 0, df = NULL,
                                         max_iter = 1000) {
  
  if (!is.matrix(targets)) {
    stop("'targets' must be a [n × d] numeric matrix.")
  }
  if (!is.matrix(non_targets)) {
    stop("'non_targets' must be a [n × d] numeric matrix.")
  }
  if (ncol(targets) != ncol(non_targets)) {
    stop("Mismatch in system dimension between targets and non_targets.")
  }
  max_iter <- max_iter
  
  targets <- t(as.matrix(targets))
  non_targets <- t(as.matrix(non_targets))
  d <- nrow(targets)
  nt <- ncol(targets)
  nn <- ncol(non_targets)
  ntnn <- nt + nn
  prop <- nt / ntnn
  
  if (is.null(df)) {
    df <- ntnn
  }
  
  if (kappa == 0) {
    weights <- c(
      rep(prior / prop, nt),
      rep((1 - prior) / (1 - prop), nn)
    )
    
    x <- cbind(
      rbind(targets, rep(1, nt)),
      -rbind(non_targets, rep(1, nn))
    )
    
    offset <- logit(prior) * c(rep(1, nt), rep(-1, nn))
    
  } else {
    # Weighted logistic regression with regularization
    weights_temp <- c(
      rep(prior / prop, nt),
      rep((1 - prior) / (1 - prop), nn)
    )
    
    weight_flat_prior <- kappa / (2 * df)
    weights_temp_flat_priors <- rep(weight_flat_prior, 2 * ntnn)
    
    weights <- c(weights_temp, weights_temp_flat_priors)
    
    x_temp <- cbind(
      rbind(targets, rep(1, nt)),
      -rbind(non_targets, rep(1, nn))
    )
    
    # Add symmetric prior samples: [x, x, -x]
    x <- cbind(x_temp, x_temp, -x_temp)
    
    offset_temp <- logit(prior) * c(rep(1, nt), rep(-1, nn))
    offset <- c(offset_temp, offset_temp, -offset_temp)
  }
  
  w <- rep(0, d + 1)
  old_g <- rep(0, d + 1)
  
  for (iter in 1:max_iter) {
    old_w <- w
    
    s1 <- as.vector(1 / (1 + exp(as.vector(t(w) %*% x + offset))))
    g <- x %*% (s1 * weights)
    
    if (iter == 1) {
      u <- g
    } else {
      u <- cg_dir(u, g, old_g)
    }
    
    ug <- sum(u * g)
    ux <- as.vector(t(u) %*% x)
    a <- weights * s1 * (1 - s1)
    uhu <- sum((ux^2) * a)
    
    w <- w + as.numeric(ug / uhu) * u
    old_g <- g
    
    if (max(abs(w - old_w)) < 1e-5) {
      break
    }
  }
  
  if (iter == max_iter) {
    warning("Not enough iterations")
  }
  
  return(w)
}

