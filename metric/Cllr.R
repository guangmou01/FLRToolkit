# Path: "metric/Cllr.R"
# Log-likelihood-ratio Cost (Cllr).
#
# Input:
# @param ss_llr: 
#        same-source natural-log-likelihood-ratio [numeric vector].
# @param ds_llr: 
#        different-source natural-log-likelihood-ratio [numeric vector].
#
# Output:
# @param cllr: 
#        Log-likelihood-ratio Cost [numeric].
# @param cllr_min: 
#        Discrimination Loss optimized by PAV [numeric].
# @param cllr_cal:
#        Calibration Loss [numeric].
#
# ------------------------------------------------------------------------------
# Updated: 2026/05/30
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

cllr <- function(ss_llr, ds_llr){
  
  ss_llr <- as.numeric(ss_llr)
  ds_llr <- as.numeric(ds_llr)
  
  n_ss <- length(ss_llr)
  n_ds <- length(ds_llr)
  
  punish_ss <- log1p(exp(-ss_llr)) / log(2)
  punish_ds <- log1p(exp(ds_llr))  / log(2)
  
  cllr <- 0.5 * (mean(punish_ss) + mean(punish_ds))
  
  return(cllr)
}

# Discrimination Loss: Cllr_min

cllr_min <- function(ss_llr, ds_llr){
  
  ss_llr <- as.numeric(ss_llr)
  ds_llr <- as.numeric(ds_llr)
  
  opt_res <- opt_loglr(ss_llr, ds_llr, option = "raw")
  opt_tar_llrs <- opt_res$tar_llrs
  opt_nontar_llrs <- opt_res$nontar_llrs
  
  cllr_min <- cllr(opt_tar_llrs, opt_nontar_llrs)
  
  return(cllr_min)
}

# Calibration Loss: Cllr_cal

cllr_cal <- function(ss_llr, ds_llr){
  
  ss_llr <- as.numeric(ss_llr)
  ds_llr <- as.numeric(ds_llr)
  
  cllr_pooled <- cllr(ss_llr, ds_llr)
  cllr_min <- cllr_min(ss_llr, ds_llr)
  
  cllr_cal <- cllr_pooled - cllr_min
  
  return(cllr_cal)
}

# PAV: Pool Adjacent Violators

pav <- function(y) {
  
  n <- length(y)
  
  index <- integer(n)
  len   <- integer(n)
  ghat  <- numeric(n)
  
  ci <- 1L
  
  index[ci] <- 1L
  len[ci]   <- 1L
  ghat[ci]  <- y[1]
  
  if (n >= 2L) {
    for (j in 2:n) {
      ci <- ci + 1L
      
      index[ci] <- j
      len[ci]   <- 1L
      ghat[ci]  <- y[j]
      
      while (ci >= 2L && ghat[max(ci - 1L, 1L)] >= ghat[ci]) {
        nw <- len[ci - 1L] + len[ci]
        ghat[ci - 1L] <- ghat[ci - 1L] + (len[ci] / nw) * (ghat[ci] - ghat[ci - 1L])
        len[ci - 1L]  <- nw
        ci <- ci - 1L
      }
    }
  }
  
  while (n >= 1L) {
    for (j in index[ci]:n) {
      ghat[j] <- ghat[ci]
    }
    n  <- index[ci] - 1L
    ci <- ci - 1L
  }
  
  ghat
}

# Non-parametric Optimal-monotonic-mapping

opt_loglr <- function(tar_scores,
                      nontar_scores,
                      option = "laplace") {
  
  tar_scores <- as.numeric(tar_scores)
  nontar_scores <- as.numeric(nontar_scores)
  
  Nt <- length(tar_scores)
  Nn <- length(nontar_scores)
  N  <- Nt + Nn
  
  tar_scores <- tar_scores - 1.0e-6
  
  scores <- c(nontar_scores, tar_scores)
  Pideal <- c(rep(0, Nn), rep(1, Nt))
  
  ord <- order(scores)
  scores <- scores[ord]
  Pideal <- Pideal[ord]
  
  if (option == "laplace") {
    Pideal <- c(1, 0, Pideal, 1, 0)
  }
  
  Popt <- pav(Pideal)
  
  if (option == "laplace") {
    Popt <- Popt[3:(length(Popt) - 2)]
  }
  
  suppressWarnings({
    posterior_log_odds <- log(Popt) - log(1 - Popt)
  })
  
  log_prior_odds <- log(Nt) - log(Nn)
  llrs <- posterior_log_odds - log_prior_odds
  
  llrs <- llrs + (1:N) * 1.0e-6 / N
  
  llrs_unsorted <- numeric(N)
  llrs_unsorted[ord] <- llrs
  
  nontar_llrs <- llrs_unsorted[1:Nn]
  tar_llrs    <- llrs_unsorted[(Nn+1):(Nn+Nt)]
  
  return(list(
    tar_llrs = tar_llrs,
    nontar_llrs = nontar_llrs
  ))
}
