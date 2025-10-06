# 95% Credible Interval (CI) Estimation for LR-based System (direction considered)
# "forensic_eval_01" benchmark validation version

CI_para <- function(df) {
  stopifnot(all(c("id_1", "id_2", "Log10LR") %in% names(df)))
  
  df$id_1 <- as.character(df$id_1)
  df$id_2 <- as.character(df$id_2)
  
  df$trial_key <- paste(df$id_1, df$id_2, sep = "|")
  df$label <- ifelse(df$id_1 == df$id_2, "ss", "ds")
  
  split_log10 <- split(df$Log10LR, df$trial_key)
  
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
  num_LRs <- length(df$Log10LR)
  unique_keys <- names(split_log10)
  num_unique_pairs <- length(unique_keys)
  df_total <- num_LRs - num_unique_pairs - 1
  CI_half_log10 <- if (is.finite(std_LR) && df_total > 0) qt(0.975, df = df_total) * std_LR else NA_real_
  
  first_idx <- match(unique_keys, df$trial_key)
  result <- data.frame(
    id_1 = df$id_1[first_idx],
    id_2 = df$id_2[first_idx],
    trial_key = unique_keys,
    n = as.integer(ns[unique_keys]),
    label = df$label[first_idx],
    Log10LR_mean = as.numeric(means[unique_keys]),
    LR_mean = 10 ** as.numeric(means[unique_keys]),
    CI_half = rep(CI_half_log10, num_unique_pairs),
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
  
  list(Cllr_mean = Cllr_mean, CI_half_log10 = CI_half_log10, result = result)
}
