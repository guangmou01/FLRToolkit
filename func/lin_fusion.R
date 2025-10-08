# Path: "func/lin_fusion.R"
# R implementation of linear fusion (FoCal style)
# Based on Brümmer (2005)

# References:

# Brümmer, N. (2005).
# Focal Toolbox [MATLAB script].
# http://www.dsp.sun.ac.za/nbrummer/focal

# Input:
#   weights - numeric vector of length d + 1 pre-trained by train_llr_fusion_*()
#             (first d elements = system weights, last = bias)
#    scores - [n × d] numeric matrix 
#             (rows = trials, columns = systems)

# Example of the input score matrix:
#         sys-1 sys-2  ...  sys-d
# trial-1 [0.8,  1.0,  ...,  0.9]
# trial-2 [1.5,  1.7,  ...,  1.7]
# ...     [...,  ...,  ...,  ...]
# trial-n [0.3,  1.4,  ...,  0.8]

# Output:
#     fused - [n × 1] numeric matrix (or scalar if n = 1)

# ------------------------------------------------------------------------------
# Updated: September 26, 2025
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

lin_fusion <- function(weights, scores) {
  
  if (!is.numeric(weights)) {
    stop("'weights' must be a numeric vector.")
  }
  if (!is.matrix(scores)) {
    stop("'scores' must be a [n × d] numeric matrix.")
  }
  d <- ncol(scores)
  if (length(weights) != d + 1) {
    stop(sprintf("'weights' must have length d + 1 (found %d, expected %d).", 
                 length(weights), d + 1))
  }
  
  # ---- fusion ----
  # Add a bias column of 1
  scores_ext <- cbind(scores, rep(1, nrow(scores)))
  
  # Multiply scores by weights (matrix multiplication)
  fused <- scores_ext %*% as.matrix(weights)
  
  # ---- output ----
  # If only one trial, return scalar instead of [1 × 1] matrix
  if (nrow(scores) == 1) {
    return(as.numeric(fused))
  } else {
    return(matrix(fused, ncol = 1))
  }
}

