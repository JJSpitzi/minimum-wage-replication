packages <- c("did", "dplyr", "tibble", "readr", "haven", "tidyr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(did)
library(dplyr)
library(tibble)
library(readr)
library(haven)
library(tidyr)

set.seed(899)


data <- readRDS("estimation_cells_main.rds") %>%
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
    first_increase_size = first(first_increase_size),
    state_name = first(state_name),
    state_abbr = first(state_abbr),
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



run_cs <- function(df, sample_name) {
  
  att <- att_gt(
    yname = "emp_pop_ratio",
    tname = "YEAR",
    idname = "STATEFIP",
    gname = "first_treat",
    weightsname = "cell_weight",
    xformla = ~ 1,
    data = df,
    panel = TRUE,
    control_group = "nevertreated",
    anticipation = 0,
    base_period = "universal",
    bstrap = TRUE,
    biters = 999,
    clustervars = "STATEFIP",
    print_details = FALSE
  )
  
  simple <- aggte(att, type = "simple", bstrap = TRUE, biters = 999)
  
  # Keep this calculation to preserve the bootstrap RNG sequence
  dynamic <- aggte(
    att,
    type = "dynamic",
    min_e = -5,
    max_e = 5,
    bstrap = TRUE,
    biters = 999
  )
  
  list(
    sample_name = sample_name,
    simple = simple,
    dynamic = dynamic
  )
}


res_all <- run_cs(data_all, "Full sample")
res_men <- run_cs(data_men, "Men")
res_women <- run_cs(data_women, "Women")
res_women_minus_men <- run_cs(data_women_minus_men, "Women - Men")


extract_result <- function(res) {
  tibble(
    Sample = res$sample_name,
    Estimate = 100 * res$simple$overall.att,
    `Standard error` = 100 * res$simple$overall.se,
    `p-value` = 2 * pnorm(
      abs(res$simple$overall.att / res$simple$overall.se),
      lower.tail = FALSE
    )
  )
}

table04 <- bind_rows(
  extract_result(res_all),
  extract_result(res_men),
  extract_result(res_women),
  extract_result(res_women_minus_men)
) %>%
  mutate(
    Estimate = round(Estimate, 3),
    `Standard error` = round(`Standard error`, 3),
    `p-value` = round(`p-value`, 3)
  )

write_csv(table04, "Table04_never_treated.csv")
print(table04)

cat("\n03_Robustness_never_treated.R completed successfully.\n")