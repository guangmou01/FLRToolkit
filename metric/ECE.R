# Path: "metric/ECE.R"
# Empirical Cross Entropy (ECE)

# Input:
# log_prior_odds - prior odds in log10 scale [numeric]
# ss_lr - same-source likelihood ratios in Raw-scale [numeric vector]
# ds_lr - different-source likelihood ratios in Raw-scale [numeric vector]

# Output:
# ECE_val - Empirical Cross Entropy

ECE <- function(log_prior_odds, ss_lr, ds_lr) {
  
  tar_llrs <- log(as.numeric(ss_lr))
  nontar_llrs <- log(as.numeric(ds_lr))
  
  Nss <- length(tar_llrs)
  Nds <- length(nontar_llrs)
  
  prior_odds <- 10^log_prior_odds
  Ptar <- prior_odds / (1 + prior_odds)
  logit_prior <- log(Ptar) - log(1 - Ptar)
  
  punish_ss <- log(1 + exp(-(tar_llrs + logit_prior))) / log(2)
  punish_ds <- log(1 + exp( nontar_llrs + logit_prior)) / log(2)
  
  ECE_val <- Ptar * mean(punish_ss) + (1 - Ptar) * mean(punish_ds)
  
  return(ECE_val)
}

