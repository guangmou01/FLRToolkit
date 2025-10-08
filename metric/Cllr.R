# Path: "metric/Cllr.R"
# Log-likelihood-ratio Cost

# Input:
# ss_lr - same-source likelihood ratios in Raw-scale [numeric vector]
# ds_lr - different-source likelihood ratios in Raw-scale [numeric vector]

# Output:
# Cllr - Log-likelihood-ratio Cost [numeric]
# Cllr_min - Discrimination Loss optimized by PAV [numeric]
# Cllr_cal - Calibration Loss [numeric]

# Standard Cllr Calculation Formula

Cllr <- function(ss_lr, ds_lr){
  
  ss_lr <- as.numeric(ss_lr)
  ds_lr <- as.numeric(ds_lr)
  
  n_vali_ss <- length(ss_lr)
  n_vali_ds <- length(ds_lr)
  
  punish_ss <- log(1 + (1/ss_lr), base = 2)
  punish_ds <- log(1 + ds_lr, base = 2)
  
  Cllr <- 0.5*(1/n_vali_ss*sum(punish_ss) + 1/n_vali_ds*sum(punish_ds))
  Cllr <- as.numeric(Cllr)
  
  return(Cllr)
}

# Discrimination Loss: Cllr-min

Cllr_min <- function(ss_lr, ds_lr){
  
  ss_lr <- as.numeric(ss_lr)
  ds_lr <- as.numeric(ds_lr)
  ss_llr <- log(ss_lr)
  ds_llr <- log(ds_lr)
  
  opt_res <- opt_loglr(ss_llr, ds_llr, option = "raw")
  
  tar_llrs <- opt_res$tar_llrs
  nontar_llrs <- opt_res$nontar_llrs
  tar_lrs <- exp(tar_llrs)
  nontar_lrs <- exp(nontar_llrs)
  
  Cllr_min <- Cllr(tar_lrs, nontar_lrs)
  return(Cllr_min)
}

# Calibration Loss: Cllr-cal

Cllr_cal <- function(ss_lr, ds_lr){
  
  Cllr_pool <- Cllr(ss_lr, ds_lr)
  Cllr_min <- Cllr_min(ss_lr, ds_lr)
  
  Cllr_cal <- Cllr_pool - Cllr_min
  return(Cllr_cal)
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

# Non-parametric Optimal Monotonic Mapping

opt_loglr <- function(tar_scores, nontar_scores, option = "laplace") {
  
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
