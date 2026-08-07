packages <- c(
  "ipumsr",
  "readxl",
  "dplyr",
  "tibble",
  "readr",
  "did",
  "ggplot2",
  "haven",
  "tidyr",
  "fixest"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}