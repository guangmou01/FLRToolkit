# Path: "metric/ECE.R"
# Empirical Cross Entropy (ECE) function

# Input:
# log10_prior_odds - prior odds in log10 scale [numeric]
# ss_llr - same-source natural-log-likelihood-ratio [numeric vector]
# ds_llr - different-source natural-log-likelihood-ratio [numeric vector]

# Output:
# ECE_val - ECE value at the 'log10_prior_odds'

# ------------------------------------------------------------------------------
# Updated: 2026/06/02
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

ECE <- function(log10_prior_odds,
                ss_llr, ds_llr) {
  
  tar_llrs <- as.numeric(ss_llr)
  nontar_llrs <- as.numeric(ds_llr)
  
  prior_odds <- 10^log10_prior_odds
  Ptar <- prior_odds / (1 + prior_odds)
  logit_prior <- log(Ptar) - log(1 - Ptar)
  
  punish_ss <- log(1 + exp(-(tar_llrs + logit_prior))) / log(2)
  punish_ds <- log(1 + exp( nontar_llrs + logit_prior)) / log(2)
  
  ECE_val <- Ptar * mean(punish_ss) + (1 - Ptar) * mean(punish_ds)
  
  return(ECE_val)
}

