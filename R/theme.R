# The plot theme every script shares, so that a figure looks the same whichever
# script drew it. Kept here rather than repeated per script for the reason
# Section 22.7 gives about one implementation and several callers.

library(ggplot2)

theme_set(theme_bw(base_size = 10))

# The chapters open with set.seed(1); scripts inherit it so that any chunk
# drawing its own data reproduces the figure in print.
set.seed(1)

# Figures are written here rather than shown, so a script can run unattended.
out_path <- function(name) file.path("out", name)

save_figure <- function(plot, name, width = 6.2, height = 3.4) {
  dir.create("out", showWarnings = FALSE)
  ggsave(out_path(name), plot, width = width, height = height, dpi = 300)
  invisible(out_path(name))
}
