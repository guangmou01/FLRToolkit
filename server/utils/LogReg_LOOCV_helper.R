# Path: "server/utils/LogReg_LOOCV_helper.R"
#
# This is a helper function to run logistic-regression calibration using
# leave-one/two-source-out cross-validation protocol.

source("server/utils/transform_to_ln.R")
source("server/utils/make_leave_out_key.R")

LogReg_LOOCV_regularized <- function(df,
                                     id1_col, id2_col,
                                     score_col,
                                     score_scale = c("ln(LR)", "log10(LR)", "Raw"),
                                     prior = 0.5,
                                     kappa = 0,
                                     df_reg = NULL,
                                     max_iter = 1000,
                                     z_score = TRUE,
                                     ss_label = SS_LABEL,
                                     ds_label = DS_LABEL) {
  
  # 1. Prepare the data
  score_scale <- match.arg(score_scale)
  out_df <- df
  
  id1_chr <- as.character(out_df[[id1_col]])
  id2_chr <- as.character(out_df[[id2_col]])
  
  out_df$label <- ifelse(id1_chr == id2_chr, ss_label, ds_label)
  out_df$leave_out_key <- mapply(make_leave_out_key,
                                 id1_chr, id2_chr,
                                 USE.NAMES = FALSE)
  
  ln_mat_all <- as.matrix(do.call(
    cbind,
    lapply(score_col, function(col) {
      transform_to_ln(out_df[[col]], score_scale)
    })
  ))
  colnames(ln_mat_all) <- score_col
  
  # 2. Prepare leave-out key map
  all_ids <- sort(unique(c(id1_chr, id2_chr)))
  id2rows <- setNames(vector("list", length(all_ids)), all_ids)
  
  for (s in all_ids) {
    id2rows[[s]] <- which(id1_chr == s | id2_chr == s)
  }
  
  unique_keys <- unique(out_df$leave_out_key)
  n_keys <- length(unique_keys)
  
  # 3. Prepare outputs
  n <- nrow(out_df)
  d <- length(score_col)
  
  calibrated_lnLR <- rep(NA_real_, n)
  beta_mat <- matrix(NA_real_, nrow = n, ncol = d)
  alpha_vec <- rep(NA_real_, n)
  
  # 4. LOOCV
  shiny::withProgress(message = "Calibrating by Leave-out Keys:", value = 0, {
    
    for (k_i in seq_along(unique_keys)) {
      
      key <- unique_keys[k_i]
      parts <- strsplit(key, "\\|")[[1]]
      
      a <- parts[1]
      b <- parts[2]
      
      if (a == b) {
        excl_idx <- id2rows[[a]]
      } else {
        excl_idx <- union(id2rows[[a]], id2rows[[b]])
      }
      
      train_idx <- setdiff(seq_len(n), excl_idx)
      test_idx <- which(out_df$leave_out_key == key)
      
      train_scores <- ln_mat_all[train_idx, , drop = FALSE]
      test_scores <- ln_mat_all[test_idx, , drop = FALSE]
      train_labels <- out_df$label[train_idx]
      
      if (z_score) {
        mu <- colMeans(train_scores)
        
        centered <- sweep(train_scores, 2, mu, "-")
        sdv <- sqrt(colMeans(centered^2))
        sdv[sdv == 0 | is.na(sdv)] <- 1
        
        train_scores <- sweep(centered, 2, sdv, "/")
        test_scores <- sweep(test_scores, 2, mu, "-")
        test_scores <- sweep(test_scores, 2, sdv, "/")
      }
      
      train_ss <- train_scores[train_labels == ss_label, , drop = FALSE]
      train_ds <- train_scores[train_labels == ds_label, , drop = FALSE]
      
      fusion_w <- train_llr_fusion_regularized(
        targets = train_ss,
        non_targets = train_ds,
        prior = prior,
        kappa = kappa,
        df = df_reg,
        max_iter = max_iter
      )
      
      calibrated_lnLR[test_idx] <- as.numeric(
        lin_fusion(weights = fusion_w,
                   scores = test_scores)
      )
      
      beta_mat[test_idx, ] <- matrix(fusion_w[1:d],
                                     nrow = length(test_idx),
                                     ncol = d,
                                     byrow = TRUE)
      
      alpha_vec[test_idx] <- fusion_w[d + 1]
      
      shiny::incProgress(
        amount = 1 / n_keys,
        detail = paste0(k_i, "/", n_keys)
      )
    }
  })
  
  beta_colnames <- paste0("Beta_", make.names(score_col, unique = TRUE))
  
  for (j in seq_len(d)) {
    out_df[[beta_colnames[j]]] <- beta_mat[, j]
  }
  
  out_df$Alpha <- alpha_vec
  out_df$calibrated_lnLR <- calibrated_lnLR
  out_df$calibrated_LR <- exp(calibrated_lnLR)
  out_df$calibrated_log10LR <- calibrated_lnLR / log(10)
  
  return(out_df)
}


LogReg_LOOCV_robust <- function(df,
                                id1_col, id2_col,
                                score_col,
                                score_scale = c("ln(LR)", "log10(LR)", "Raw"),
                                prior = 0.5,
                                robust_weight = 0,
                                max_iter = 1000,
                                z_score = TRUE,
                                ss_label = SS_LABEL,
                                ds_label = DS_LABEL) {
  
  # 1. Prepare the data
  score_scale <- match.arg(score_scale)
  out_df <- df
  
  id1_chr <- as.character(out_df[[id1_col]])
  id2_chr <- as.character(out_df[[id2_col]])
  
  out_df$label <- ifelse(id1_chr == id2_chr, ss_label, ds_label)
  out_df$leave_out_key <- mapply(make_leave_out_key,
                                 id1_chr, id2_chr,
                                 USE.NAMES = FALSE)
  
  ln_mat_all <- as.matrix(do.call(
    cbind,
    lapply(score_col, function(col) {
      transform_to_ln(out_df[[col]], score_scale)
    })
  ))
  colnames(ln_mat_all) <- score_col
  
  # 2. Prepare leave-out key map
  all_ids <- sort(unique(c(id1_chr, id2_chr)))
  id2rows <- setNames(vector("list", length(all_ids)), all_ids)
  
  for (s in all_ids) {
    id2rows[[s]] <- which(id1_chr == s | id2_chr == s)
  }
  
  unique_keys <- unique(out_df$leave_out_key)
  n_keys <- length(unique_keys)
  
  # 3. Prepare outputs
  n <- nrow(out_df)
  d <- length(score_col)
  
  calibrated_lnLR <- rep(NA_real_, n)
  beta_mat <- matrix(NA_real_, nrow = n, ncol = d)
  alpha_vec <- rep(NA_real_, n)
  
  # 4. LOOCV
  shiny::withProgress(message = "Calibrating by Leave-out Keys:", value = 0, {
    
    for (k_i in seq_along(unique_keys)) {
      
      key <- unique_keys[k_i]
      parts <- strsplit(key, "\\|")[[1]]
      
      a <- parts[1]
      b <- parts[2]
      
      if (a == b) {
        excl_idx <- id2rows[[a]]
      } else {
        excl_idx <- union(id2rows[[a]], id2rows[[b]])
      }
      
      train_idx <- setdiff(seq_len(n), excl_idx)
      test_idx <- which(out_df$leave_out_key == key)
      
      train_scores <- ln_mat_all[train_idx, , drop = FALSE]
      test_scores <- ln_mat_all[test_idx, , drop = FALSE]
      train_labels <- out_df$label[train_idx]
      
      if (z_score) {
        mu <- colMeans(train_scores)
        
        centered <- sweep(train_scores, 2, mu, "-")
        sdv <- sqrt(colMeans(centered^2))
        sdv[sdv == 0 | is.na(sdv)] <- 1
        
        train_scores <- sweep(centered, 2, sdv, "/")
        test_scores <- sweep(test_scores, 2, mu, "-")
        test_scores <- sweep(test_scores, 2, sdv, "/")
      }
      
      train_ss <- train_scores[train_labels == ss_label, , drop = FALSE]
      train_ds <- train_scores[train_labels == ds_label, , drop = FALSE]
      
      fusion_w <- train_llr_fusion_robust(
        targets = train_ss,
        non_targets = train_ds,
        prior = prior,
        robust_weight = robust_weight,
        max_iter = max_iter
      )
      
      calibrated_lnLR[test_idx] <- as.numeric(
        lin_fusion(weights = fusion_w,
                   scores = test_scores)
      )
      
      beta_mat[test_idx, ] <- matrix(fusion_w[1:d],
                                     nrow = length(test_idx),
                                     ncol = d,
                                     byrow = TRUE)
      
      alpha_vec[test_idx] <- fusion_w[d + 1]
      
      shiny::incProgress(
        amount = 1 / n_keys,
        detail = paste0(k_i, "/", n_keys)
      )
    }
  })
  
  beta_colnames <- paste0("Beta_", make.names(score_col, unique = TRUE))
  
  for (j in seq_len(d)) {
    out_df[[beta_colnames[j]]] <- beta_mat[, j]
  }
  
  out_df$Alpha <- alpha_vec
  out_df$calibrated_lnLR <- calibrated_lnLR
  out_df$calibrated_LR <- exp(calibrated_lnLR)
  out_df$calibrated_log10LR <- calibrated_lnLR / log(10)
  
  return(out_df)
}