# Path: "metric/CI.R"
# 95% Credible Interval (CI) Estimation for LR-based System

# Parametric Procedure Referenced from:
# Morrison, G. S., Thiruvaran, T., & Epps, J. (2010).
# Estimating the Precision of the Likelihood-Ratio Output
# of a Forensic-Voice-Comparison System.
# The Speaker and Language Recognition Workshop, 63-70.

# Input:
#   df - A data.frame with at least the following columns:
#        * id_1    : character or factor, ID of the 1st individual
#        * id_2    : character or factor, ID of the 2nd individual
#        * Log10LR : numeric, log10-likelihood-ratio values for the trial
#
#        Notes:
#        - Each row corresponds to a trial (id_1, id_2).
#        - Order of id_1 and id_2 does not matter; (A,B) and (B,A) are treated as the same trial.
#        - Multiple rows for the same trial are required (repeated measurements).
#
# Output:
#   Cllr_mean - Cllr calculated on the means of the groups defined in the description of the 95% CI metric.
#   CI_half_log10 - 95% CI estimation in log10-scale
#   result - A data.frame where each row corresponds to one unique trial.
#       * id_1         : normalized first ID (the smaller after sorting the pair)
#       * id_2         : normalized second ID (the larger after sorting the pair)
#       * trial_key    : unique trial identifier in the format "id_1|id_2"
#       * n            : count of trials within the trial key
#       * label        : "ss" if same-source (id_1 == id_2), otherwise "ds"
#       * Log10LR_mean : mean log10-likelihood-ratio for the trial
#       * LR_mean      : mean likelihood-ratio for the trial
#       * CI_half      : pooled half-width of the 95% confidence interval (t-distribution-based)
#       * CI_lower     : lower bound of the 95% CI
#       * CI_upper     : upper bound of the 95% CI

CI_para <- function(df) {
  stopifnot(all(c("id_1", "id_2", "Log10LR") %in% names(df)))
  
  df$id_1 <- as.character(df$id_1)
  df$id_2 <- as.character(df$id_2)
  
  pair_sorted <- t(apply(df[, c("id_1", "id_2")], 1, sort))
  df$id_1u <- pair_sorted[, 1]
  df$id_2u <- pair_sorted[, 2]
  
  df$trial_key <- paste(df$id_1u, df$id_2u, sep = "|")
  df$label <- ifelse(df$id_1u == df$id_2u, "ss", "ds")
  
  unique_keys <- unique(df$trial_key)
  
  ns <- tapply(df$Log10LR, df$trial_key, function(x) sum(is.finite(x)))
  means <- tapply(df$Log10LR, df$trial_key, function(x) {
    x <- x[is.finite(x)]
    if (length(x) > 0) mean(x) else NA_real_
  })
  
  weighted_SS <- tapply(df$Log10LR, df$trial_key, function(x) {
    x <- x[is.finite(x)]
    if (length(x) > 0) {
      m <- mean(x)
      sum((x - m)^2) / length(x)
    } else {
      NA_real_
    }
  })
  
  std_LR <- sqrt(mean(weighted_SS, na.rm = TRUE))
  num_LRs <- length(df$Log10LR)
  num_unique_pairs <- length(unique_keys)
  df_total <- num_LRs - num_unique_pairs - 1
  
  CI_half_log10 <- if (is.finite(std_LR) && df_total > 0) {
    qt(0.975, df = df_total) * std_LR
  } else {
    NA_real_
  }
  
  first_idx <- match(unique_keys, df$trial_key)
  
  result <- data.frame(
    id_1 = df$id_1u[first_idx],
    id_2 = df$id_2u[first_idx],
    trial_key = unique_keys,
    n = as.integer(ns[unique_keys]),
    label = df$label[first_idx],
    Log10LR_mean = as.numeric(means[unique_keys]),
    LR_mean = 10 ** as.numeric(means[unique_keys]),
    CI_half = rep(CI_half_log10, length(unique_keys)),
    stringsAsFactors = FALSE
  )
  
  result$CI_lower <- result$Log10LR_mean - result$CI_half
  result$CI_upper <- result$Log10LR_mean + result$CI_half
  
  result$trial_key <- factor(result$trial_key)
  result$label <- factor(result$label, levels = c("ss", "ds"))
  
  ss_m_lr <- result$LR_mean[result$label == "ss"]
  ds_m_lr <- result$LR_mean[result$label == "ds"]
  n_m_ss <- length(ss_m_lr)
  n_m_ds <- length(ds_m_lr)
  punish_m_ss <- log(1 + (1 / ss_m_lr), base = 2)
  punish_m_ds <- log(1 + ds_m_lr, base = 2)
  Cllr_mean <- 0.5 * ((1 / n_m_ss) * sum(punish_m_ss) + (1 / n_m_ds) * sum(punish_m_ds))
  
  out_list <- list(
    Cllr_mean = Cllr_mean,
    CI_half_log10 = CI_half_log10,
    result = result
  )
  
  return(out_list)
}
