# Path: "func/train_llr_fusion_robust.R"
# R implementation of robust LogReg from Focal for LR calibration & fusion
# Based on Morrison (2009), and Brümmer et al. (2005)

# References:

# Brümmer, N. (2005).
# Focal Toolkit [MATLAB script].
# http://www.dsp.sun.ac.za/nbrummer/focal

# Morrison, G. S. (2009).
# Robust version of train_llr_fusion.m from Niko Brümmer’s FoCal Toolbox [MATLAB script].
# https://geoff-morrison.net/#TrainFus

# Input:
#       targets - scores from target (same-source) trials
#                 (rows = trials, columns = systems) [numeric matrix]
#   non_targets - scores from non-target (different-source) trials
#                 (rows = trials, columns = systems) [numeric matrix]
#         prior - prior probability of the target hypothesis [numeric scalar, default = 0.5]
# robust_weight - robustness weight for class balancing and outlier resistance [numeric scalar, default = 0]
#      max_iter - Max iteration number for Conjugate Gradient [integer, default = 5000]

# Example of the input score matrix:
#         sys-1 sys-2  ...  sys-d
# trial-1 [0.8,  1.0,  ...,  0.9]
# trial-2 [1.5,  1.7,  ...,  1.7]
# ...     [...,  ...,  ...,  ...]
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

train_llr_fusion_robust <- function(targets, non_targets,
                                    prior = 0.5, robust_weight = 0,
                                    max_iter = 5000) {
  
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
  
  # Construct input matrix x: dimension [d+1, 2*(nt+nn)]
  x <- cbind(
    rbind(targets, rep(1, nt)), 
    rbind(non_targets, rep(1, nn)), 
    -rbind(non_targets, rep(1, nn)), 
    -rbind(targets, rep(1, nt))
  )
  
  # Weights and offset vector
  robust_weight_targets <- nt / nn * robust_weight
  robust_weight_non_targets <- nn / nt * robust_weight
  
  prop_targets <- nt / (nt + nn)
  prop_non_targets <- 1 - prop_targets
  
  prior_adjusted_targets <- prior / prop_targets
  prior_adjusted_non_targets <- (1 - prior) / prop_non_targets
  
  weights <- c(
    rep(prior_adjusted_targets, nt),
    rep(prior_adjusted_targets * robust_weight_targets, nn),
    rep(prior_adjusted_non_targets, nn),
    rep(prior_adjusted_non_targets * robust_weight_non_targets, nt)
  )
  
  offset <- logit(prior) * c(rep(1, ntnn), rep(-1, ntnn))
  
  # Initialize weights
  w <- rep(0, d + 1)
  old_g <- rep(0, d + 1)
  
  for (iter in 1:max_iter) {
    old_w <- w
    s1 <- as.vector(1 / (1 + exp(as.vector(t(w) %*% x + offset))))
    g <- x %*% as.vector(s1 * weights)
    
    if (iter == 1) {
      u <- g
    } else {
      u <- cg_dir(u, g, old_g)
    }
    
    ug <- sum(u * g)
    ux <- t(u) %*% x
    a <- weights * s1 * (1 - s1)
    uhu <- sum((ux^2) * a)
    
    w <- w + as.numeric((ug / uhu)) * u
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