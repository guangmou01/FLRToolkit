# Path: "metric/EER.R"
# Equal Error Rate (EER) estimation

# Input:
# ss_llr - same-source natural-log-likelihood-ratio [numeric vector]
# ds_llr - different-source natural-log-likelihood-ratio [numeric vector]
# num_thresholds - the number of pseudo thresholds used to estimate EER [integer]

# Output:
# EER - Equal Error Rate [numeric]
# threshold_log10 - estimated EER threshold in log10-scale [numeric]
# threshold_raw - estimated EER threshold in raw-LR-scale [numeric]

# ------------------------------------------------------------------------------
# Updated: 2026/05/31
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

eer <- function(ss_llr, ds_llr,
                num_thresholds = 10000){
  
  ss_llr <- as.numeric(ss_llr)/log(10)
  ds_llr <- as.numeric(ds_llr)/log(10)
  num_thresholds <- as.integer(num_thresholds)
  
  min_threshold <- min(c(ss_llr, ds_llr))
  max_threshold <- max(c(ss_llr, ds_llr))
  
  if(max_threshold == Inf) max_threshold <- log10(.Machine$double.xmax)
  if(min_threshold == -Inf) min_threshold <- log10(.Machine$double.xmin)
  
  thresholds <- seq(from = min_threshold, to = max_threshold, length.out = num_thresholds)
  
  SS_corr <- sapply(thresholds, function(th) sum(ss_llr > th))
  DS_corr <- sapply(thresholds, function(th) sum(ds_llr <= th))
  
  SS_corr <- SS_corr / length(ss_llr)
  DS_corr <- DS_corr / length(ds_llr)
  
  diff <- abs(SS_corr - DS_corr)
  idx_all <- which(diff == min(diff))
  
  threshold_log10 <- mean(thresholds[idx_all])
  threshold_log10 <- as.numeric(threshold_log10)
  
  threshold_raw <- 10^threshold_log10
  threshold_raw <- as.numeric(threshold_raw)
  
  false_positive_eer <- mean(ds_llr > threshold_log10)
  false_negative_eer <- mean(ss_llr <= threshold_log10)
  
  EER <- mean(c(false_positive_eer, false_negative_eer))
  EER <- as.numeric(EER)
  
  res <- list(
    EER = EER,
    threshold_log10 = threshold_log10,
    threshold_raw = threshold_raw
  )
  
  return(res)
}