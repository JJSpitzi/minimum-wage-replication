packages <- c("did", "dplyr", "tibble", "readr", "haven", "tidyr", "fixest")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(did)
library(dplyr)
library(tibble)
library(readr)
library(haven)
library(tidyr)
library(fixest)

set.seed(899)

data <- readRDS("estimation_cells_no_later_increases.rds") %>%
  mutate(
    STATEFIP = as.integer(as.numeric(zap_labels(STATEFIP))),
    YEAR = as.integer(YEAR),
    first_treat = as.numeric(first_treat),
    female = as.integer(female),
    gender = if_else(female == 1, "Women", "Men")
  )

data_all <- data %>%
  group_by(STATEFIP, YEAR) %>%
  summarise(
    emp_pop_ratio = weighted.mean(emp_rate, cell_weight, na.rm = TRUE),
    cell_weight = sum(cell_weight, na.rm = TRUE),
    first_treat = first(first_treat),
    .groups = "drop"
  )

data_men <- data %>%
  filter(gender == "Men") %>%
  mutate(emp_pop_ratio = emp_rate)

data_women <- data %>%
  filter(gender == "Women") %>%
  mutate(emp_pop_ratio = emp_rate)

data_women_minus_men <- data %>%
  select(STATEFIP, YEAR, first_treat, female, emp_rate, cell_weight) %>%
  pivot_wider(
    id_cols = c(STATEFIP, YEAR, first_treat),
    names_from = female,
    values_from = c(emp_rate, cell_weight),
    names_glue = "{.value}_{female}"
  ) %>%
  transmute(
    STATEFIP,
    YEAR,
    first_treat,
    emp_pop_ratio = emp_rate_1 - emp_rate_0,
    cell_weight = cell_weight_1 + cell_weight_0
  )


quiet_att_gt <- function(...) {
  suppressWarnings(suppressMessages(att_gt(...)))
}

quiet_aggte <- function(...) {
  suppressWarnings(suppressMessages(aggte(...)))
}

run_cs <- function(df) {
  
  att <- quiet_att_gt(
    yname = "emp_pop_ratio",
    tname = "YEAR",
    idname = "STATEFIP",
    gname = "first_treat",
    weightsname = "cell_weight",
    xformla = ~ 1,
    data = df,
    panel = TRUE,
    allow_unbalanced_panel = TRUE,
    control_group = "notyettreated",
    anticipation = 0,
    base_period = "universal",
    bstrap = TRUE,
    biters = 999,
    clustervars = "STATEFIP",
    print_details = FALSE
  )
  
  simple <- quiet_aggte(
    att,
    type = "simple",
    na.rm = TRUE,
    bstrap = TRUE,
    biters = 999
  )
  
  # Keep to preserve bootstrap RNG sequence
  quiet_aggte(
    att,
    type = "dynamic",
    min_e = -5,
    max_e = 5,
    na.rm = TRUE,
    bstrap = TRUE,
    biters = 999
  )
  
  simple
}

res_all <- run_cs(data_all)
res_men <- run_cs(data_men)
res_women <- run_cs(data_women)
res_women_minus_men <- run_cs(data_women_minus_men)


run_twfe <- function(df, sample) {
  
  model <- feols(
    emp_pop_ratio ~ treated | STATEFIP + YEAR,
    data = df %>%
      mutate(treated = if_else(first_treat > 0 & YEAR >= first_treat, 1, 0)),
    weights = ~ cell_weight,
    cluster = ~ STATEFIP
  )
  
  tibble(
    Sample = sample,
    twfe_estimate = 100 * unname(coef(model)["treated"]),
    twfe_se = 100 * unname(se(model)["treated"])
  )
}

extract_cs <- function(res, sample) {
  tibble(
    Sample = sample,
    cs_estimate = 100 * res$overall.att,
    cs_se = 100 * res$overall.se
  )
}

cs_table <- bind_rows(
  extract_cs(res_all, "Full sample"),
  extract_cs(res_men, "Men"),
  extract_cs(res_women, "Women"),
  extract_cs(res_women_minus_men, "Women - Men")
)

twfe_table <- bind_rows(
  run_twfe(data_all, "Full sample"),
  run_twfe(data_men, "Men"),
  run_twfe(data_women, "Women"),
  run_twfe(data_women_minus_men, "Women - Men")
)

table05 <- left_join(cs_table, twfe_table, by = "Sample") %>%
  transmute(
    Sample,
    `Callaway-Sant'Anna` = sprintf("%.3f (%.3f)", cs_estimate, cs_se),
    TWFE = sprintf("%.3f (%.3f)", twfe_estimate, twfe_se)
  )

write_csv(table05, "Table05_drop_later_increases.csv")

print(table05)

cat("\n04_Robustness_drop_later_increases.R completed successfully.\n")