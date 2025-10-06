# Path: "metric/TippettPlot.R"
# Tippett Plot (only for quick check)

if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

tippett.plot <- function(ss_lr, ds_lr,
                         x_lab = "Log10 Likelihood Ratio", 
                         y_lab = "Cumulative Proportion",
                         line.type = 1,
                         font = "sans", font.size = 14) {
  
  ss_lr <- sort(ss_lr, decreasing = FALSE)
  data_ss <- data.frame(
    lg_LR = log10(ss_lr),
    Cumulative_Prop = seq_along(ss_lr) / length(ss_lr)
  )
  
  ds_lr <- sort(ds_lr, decreasing = TRUE)
  data_ds <- data.frame(
    lg_LR = log10(ds_lr),
    Cumulative_Prop = seq_along(ds_lr) / length(ds_lr)
  )
  
  ggplot() +
    geom_line(data = data_ds, aes(x = lg_LR, y = Cumulative_Prop),
              color = "blue", linetype = line.type) +
    geom_line(data = data_ss, aes(x = lg_LR, y = Cumulative_Prop),
              color = "red", linetype = line.type) +
    geom_vline(xintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(limits = range(c(data_ss$lg_LR, data_ds$lg_LR)),
                       expand = c(0, 0),
                       breaks = pretty(c(data_ss$lg_LR, data_ds$lg_LR), n = 8)) +
    scale_y_continuous(limits = c(0, 1),
                       expand = c(0, 0),
                       breaks = seq(0, 1, length.out = 11)) +
    labs(x = x_lab, y = y_lab) +
    theme_minimal(base_size = font.size, base_family = font) +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(color = "lightgrey", linewidth = 0.4, linetype = 5),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(-0.2, "cm"),
      axis.ticks = element_line(color = "black", linewidth = 0.2),
      axis.text = element_text(color = "black", size = font.size),
      axis.title = element_text(color = "black", size = font.size),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}