# Path: "func/cg_dir.R"
# Conjugate Gradient in LogReg training

cg_dir <- function(u, g, old_g) {
  beta <- sum(g * (g - old_g)) / sum(old_g * old_g)
  return(g + beta * u)
}