# Path: "metric/CI.R"
# 95% Credible Interval (CI) estimation for LR-based system
#
# Parametric Procedure Described in:
# Morrison, G. S., Thiruvaran, T., & Epps, J. (2010).
# Estimating the precision of the Likelihood-ratio output of a forensic-voice-comparison system.
# The Speaker and Language Recognition Workshop, 63-70.
#
# Input:
# @param df: 
#        a data.frame with at least the following columns:
#        id_1    : ID of the 1st individual [character or factor].
#        id_2    : ID of the 2nd individual [character or factor].
#        log10LR : log10-likelihood-ratio values for the trial [numeric].
# @param symmetric_trial:
#        whether to treat (id_1, id_2) and (id_2, id_1) as the same trial [logical].
#
# Notes:
# 1. Each row corresponds to a ID-trial (id_1, id_2).
# 2. Multiple rows for the same trial are required (repeated measurements).
#
# Output:
# @param cllr_mean:
#        Cllr calculated on the means of the groups defined in the description of the 95% CI metric.
# @param CI_half_log10:
#        95% CI estimation in log10-scale (t-distribution-based).
# @param result:
#        a data.frame where each row corresponds to one unique trial.
#        id_1         : 1st ID.
#        id_2         : 2nd ID.
#        trial_key    : unique trial identifier in the format "id_1|id_2".
#        n            : count of trials within the trial key.
#        label        : "ss" if same-source (id_1 == id_2), otherwise "ds".
#        log10LR_mean : mean log10-likelihood-ratio for the trial.
#        LR_mean      : mean likelihood-ratio for the trial.
#        CI_half      : pooled half-width of the 95% confidence interval.
#        CI_lower     : lower bound of the 95% CI.
#        CI_upper     : upper bound of the 95% CI.
#
# ------------------------------------------------------------------------------
# Updated: 2026/06/27
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

CI_para <- function(df,
                    symmetric_trial = FALSE,
                    SS_LABEL = "ss",
                    DS_LABEL = "ds") {
  
  stopifnot(all(c("id_1", "id_2", "log10LR") %in% names(df)))
  
  df$id_1 <- as.character(df$id_1)
  df$id_2 <- as.character(df$id_2)
  
  if (symmetric_trial) {
    pair_sorted <- t(apply(df[, c("id_1", "id_2")], 1, sort))
    df$id_1 <- pair_sorted[, 1]
    df$id_2 <- pair_sorted[, 2]
  }
  
  df$trial_key <- paste(df$id_1, df$id_2, sep = "|")
  df$label <- ifelse(df$id_1 == df$id_2, SS_LABEL, DS_LABEL)
  
  split_log10 <- split(df$log10LR, df$trial_key)
  
  ns <- sapply(split_log10, function(x) sum(is.finite(x)))
  
  means <- sapply(split_log10, function(x) {
    x <- x[is.finite(x)]
    if (length(x) > 0) mean(x) else NA_real_
  })
  
  weighted_SS <- sapply(split_log10, function(x) {
    x <- x[is.finite(x)]
    if (length(x) > 0) {
      m <- mean(x)
      sum((x - m)^2) / length(x)
    } else {
      NA_real_
    }
  })
  
  std_LR <- sqrt(mean(weighted_SS, na.rm = TRUE))
  num_LRs <- length(df$log10LR)
  unique_keys <- names(split_log10)
  num_unique_pairs <- length(unique_keys)
  df_total <- num_LRs - num_unique_pairs - 1
  
  CI_half_log10 <- if (is.finite(std_LR) && df_total > 0){
    qt(0.975, df = df_total) * std_LR
  } else {
    NA_real_ 
  }
  
  first_idx <- match(unique_keys, df$trial_key)
  
  result <- data.frame(
    id_1 = df$id_1[first_idx],
    id_2 = df$id_2[first_idx],
    trial_key = unique_keys,
    n = as.integer(ns[unique_keys]),
    label = df$label[first_idx],
    log10LR_mean = as.numeric(means[unique_keys]),
    LR_mean = 10 ** as.numeric(means[unique_keys]),
    CI_half = rep(CI_half_log10, num_unique_pairs),
    stringsAsFactors = FALSE
  )
  result$CI_lower <- result$log10LR_mean - result$CI_half
  result$CI_upper <- result$log10LR_mean + result$CI_half
  result$trial_key <- factor(result$trial_key)
  result$label <- factor(result$label, levels = c(SS_LABEL, DS_LABEL))
  
  ss_m_lr <- result$LR_mean[result$label == SS_LABEL]
  ds_m_lr <- result$LR_mean[result$label == DS_LABEL]
  n_m_ss <- length(ss_m_lr)
  n_m_ds <- length(ds_m_lr)
  punish_m_ss <- log(1 + (1 / ss_m_lr), base = 2)
  punish_m_ds <- log(1 + ds_m_lr, base = 2)
  cllr_mean <- 0.5 * ((1 / n_m_ss) * sum(punish_m_ss) + (1 / n_m_ds) * sum(punish_m_ds))
  
  res <- list(cllr_mean = cllr_mean,
              CI_half_log10 = CI_half_log10,
              result = result)
  
  return(res)
}




