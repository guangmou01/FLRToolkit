# Path: "metric/EER.R"
# Equal Error Rate (EER) Estimation

# Input:
#          ss_lr - same-source likelihood ratios in Raw-scale [numeric vector]
#          ds_lr - different-source likelihood ratios in Raw-scale [numeric vector]
# num_thresholds - the threshold number of samples used to estimate EER [integer]

# Output:
#             EER - Equal Error Rate [numeric]
# threshold_log10 - estimated EER threshold in log10-scale [numeric]
#   threshold_Raw - estimated EER threshold in Raw-LR-scale [numeric]

EER <- function(ss_lr, ds_lr,
                num_thresholds = 10000){

  ss_llr <- log10(as.numeric(ss_lr))
  ds_llr <- log10(as.numeric(ds_lr))
  
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
  
  idx <- which.min(abs(SS_corr - DS_corr))
  
  EER <- 1 - (SS_corr[idx] + DS_corr[idx]) / 2
  EER <- as.numeric(EER)
  
  threshold_log10 <- thresholds[idx]
  threshold_log10 <- as.numeric(threshold_log10)
  threshold_Raw <- 10^threshold_log10
  threshold_Raw = as.numeric(threshold_Raw)
  
  return(list(
    EER = EER,
    threshold_log10 = threshold_log10,
    threshold_Raw = threshold_Raw
  ))
  
}