pkgs <- c(
  "googlesheets4", "googledrive",
  "dplyr", "tidyr", "lubridate", "readr", "stringr",
  "ggplot2", "scales", "gt", "broom", "quarto"
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
