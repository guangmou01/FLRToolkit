# Path: "calibration/lin_fusion.R"
# R implementation of (FoCal) linear fusion based on Brümmer (2005).
# 
# References:
# Brümmer, N. (2005).
# Focal Toolbox [MATLAB script].
# http://www.dsp.sun.ac.za/nbrummer/focal
#
# Input:
# @param weights:
#        numeric vector of length d + 1 pre-trained by train_llr_fusion_*().
#        (first d elements = system weights, last = bias)
# @param scores:
#        [n × d] numeric matrix.
#        (rows = trials, columns = systems)
#
# Example of the input score matrix:
#         sys-1 sys-2  ...  sys-d
# trial-1 [0.8,  1.0,  ...,  0.9]
# trial-2 [1.5,  1.7,  ...,  1.7]
# ...     [...,  ...,  ...,  ...]
# trial-n [0.3,  1.4,  ...,  0.8]
#
# Output:
# @param fused:
#        [n × 1] numeric matrix (or scalar if n = 1)
#
# ------------------------------------------------------------------------------
# Updated: 2026/06/26
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

lin_fusion <- function(weights, scores) {
  
  d <- ncol(scores)
  
  # Add a bias column of 1
  scores_ext <- cbind(scores, rep(1, nrow(scores)))
  
  # Multiply scores by weights (matrix multiplication)
  fused <- scores_ext %*% as.matrix(weights)
  
  # If only one trial, return scalar instead of [1 × 1] matrix
  if (nrow(scores) == 1) {
    return(as.numeric(fused))
  } else {
    return(matrix(fused, ncol = 1))
  }
}

