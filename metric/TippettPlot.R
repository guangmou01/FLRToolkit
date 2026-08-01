# Path: "metric/TippettPlot.R"
# Tippett plot drawing function (only for quick check purpose).
#
# Input:
# @param ss_llr:
#        same-source natural-log-likelihood-ratio [numeric vector].
# @param ds_llr:
#        different-source natural-log-likelihood-ratio [numeric vector].
#
# Output:
# @param tippett_plot:
#        ggplot object showing a Tippett plot in log10-scale [ggplot].
#
# ------------------------------------------------------------------------------
# Updated: 2026/06/26
# Author: Deng, Guangmou
# Contact: guangmou01@outlook.com
# ------------------------------------------------------------------------------

if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

tippett_plot <- function(ss_llr, ds_llr,
                         x_lab = expression(log[10](Lambda)), 
                         y_lab = "cumulative proportion",
                         line.type = 1,
                         font = "sans",
                         font.size = 18){
  
  ss_llr <- as.numeric(ss_llr)
  ds_llr <- as.numeric(ds_llr)
  
  ss_llr <- ss_llr[is.finite(ss_llr)]
  ds_llr <- ds_llr[is.finite(ds_llr)]
  
  ss_llr <- sort(ss_llr, decreasing = FALSE)
  data_ss <- data.frame(
    log10LR = ss_llr/log(10),
    Cumulative_Prop = seq_along(ss_llr) / length(ss_llr)
  )
  
  ds_llr <- sort(ds_llr, decreasing = TRUE)
  data_ds <- data.frame(
    log10LR = ds_llr/log(10),
    Cumulative_Prop = seq_along(ds_llr) / length(ds_llr)
  )
  
  tippett_plot <- ggplot() +
    geom_line(data = data_ds, aes(x = log10LR, y = Cumulative_Prop),
              color = "blue", linetype = line.type) +
    geom_line(data = data_ss, aes(x = log10LR, y = Cumulative_Prop),
              color = "red", linetype = line.type) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(expand = expansion(mult = 0.1),
                       breaks = pretty(c(data_ss$log10LR, data_ds$log10LR, 0), n = 8)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 1, length.out = 11)) +
    coord_cartesian(
      xlim = range(c(data_ss$log10LR, data_ds$log10LR, 0)) +
        c(-1, 1) * diff(range(c(data_ss$log10LR, data_ds$log10LR, 0))) * 0.1,
      ylim = c(0, 1),
      expand = FALSE
    ) +
    labs(x = x_lab, y = y_lab) +
    theme_minimal(base_size = font.size, base_family = font) +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 1),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(-0.2, "cm"),
      axis.ticks = element_line(color = "black", linewidth = 0.2),
      axis.text = element_text(color = "black", size = font.size),
      axis.title = element_text(color = "black", size = font.size),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
  
  return(tippett_plot)
}