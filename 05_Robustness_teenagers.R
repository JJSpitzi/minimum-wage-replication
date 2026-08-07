packages <- c("ipumsr", "dplyr", "did", "haven", "tibble", "readr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(ipumsr)
library(dplyr)
library(did)
library(haven)
library(tibble)
library(readr)

set.seed(899)

main_data <- readRDS("estimation_cells_main.rds")

treatment_info <- main_data %>%
  distinct(STATEFIP, first_treat)

sample_start <- min(main_data$YEAR)
sample_end   <- max(main_data$YEAR)


ddi <- read_ipums_ddi("usa_00003.xml")

acs <- read_ipums_micro(
  ddi,
  data_file = "usa_00003.dat",
  vars = c("YEAR", "STATEFIP", "GQ", "PERWT", "SEX", "AGE", "EMPSTAT")
)

teenager_panel <- acs %>%
  transmute(
    YEAR = as.integer(as.numeric(zap_labels(YEAR))),
    STATEFIP = as.integer(as.numeric(zap_labels(STATEFIP))),
    GQ = as.integer(as.numeric(zap_labels(GQ))),
    PERWT = as.numeric(zap_labels(PERWT)),
    SEX = as.integer(as.numeric(zap_labels(SEX))),
    AGE = as.integer(as.numeric(zap_labels(AGE))),
    EMPSTAT = as.integer(as.numeric(zap_labels(EMPSTAT)))
  ) %>%
  filter(
    YEAR >= sample_start, YEAR <= sample_end,
    AGE >= 16, AGE <= 19,
    GQ %in% c(1, 2),
    SEX %in% c(1, 2),
    EMPSTAT %in% c(1, 2, 3),
    !is.na(PERWT)
  ) %>%
  mutate(employed = if_else(EMPSTAT == 1, 1, 0)) %>%
  group_by(STATEFIP, YEAR) %>%
  summarise(
    emp_pop_ratio = weighted.mean(employed, PERWT, na.rm = TRUE),
    cell_weight = sum(PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(treatment_info, by = "STATEFIP") %>%
  arrange(STATEFIP, YEAR)

teenager_att <- att_gt(
  yname = "emp_pop_ratio",
  tname = "YEAR",
  idname = "STATEFIP",
  gname = "first_treat",
  weightsname = "cell_weight",
  xformla = ~ 1,
  data = teenager_panel,
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated",
  anticipation = 0,
  bstrap = TRUE,
  biters = 999,
  clustervars = "STATEFIP",
  print_details = FALSE
)

teenager_result <- aggte(
  teenager_att,
  type = "simple",
  bstrap = TRUE,
  biters = 999
)


estimate <- 100 * teenager_result$overall.att
std_error <- 100 * teenager_result$overall.se
p_value <- 2 * pnorm(abs(estimate / std_error), lower.tail = FALSE)

table06 <- tibble(
  Sample = "Teenagers aged 16–19",
  Estimate = round(estimate, 3),
  `Standard error` = round(std_error, 3),
  `p-value` = round(p_value, 3)
)

write_csv(table06, "Table06_teenagers.csv")

print(table06)

cat("\n05_Robustness_teenagers.R completed successfully.\n")