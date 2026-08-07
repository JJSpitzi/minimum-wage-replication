packages <- c("did", "dplyr", "ggplot2", "tibble", "readr", "tidyr", "fixest")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(did)
library(dplyr)
library(ggplot2)
library(tibble)
library(readr)
library(tidyr)
library(fixest)

set.seed(899)


data <- readRDS("estimation_cells_main.rds")

data_all <- data %>%
  group_by(STATEFIP, YEAR) %>%
  summarise(
    emp_pop_ratio = weighted.mean(emp_rate, cell_weight, na.rm = TRUE),
    cell_weight = sum(cell_weight, na.rm = TRUE),
    first_treat = first(first_treat),
    first_increase_size = first(first_increase_size),
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


run_cs <- function(df) {
  att <- att_gt(
    yname = "emp_pop_ratio",
    tname = "YEAR",
    idname = "STATEFIP",
    gname = "first_treat",
    weightsname = "cell_weight",
    xformla = ~ 1,
    data = df,
    panel = TRUE,
    control_group = "notyettreated",
    anticipation = 0,
    base_period = "universal",
    bstrap = TRUE,
    biters = 999,
    clustervars = "STATEFIP",
    print_details = FALSE
  )
  
  list(
    simple = aggte(att, type = "simple", bstrap = TRUE, biters = 999),
    dynamic = aggte(att, type = "dynamic", min_e = -5, max_e = 5,
                    bstrap = TRUE, biters = 999)
  )
}

res_all <- run_cs(data_all)
res_men <- run_cs(data_men)
res_women <- run_cs(data_women)
res_women_minus_men <- run_cs(data_women_minus_men)


extract_cs <- function(res, sample) {
  estimate <- 100 * res$simple$overall.att
  std_error <- 100 * res$simple$overall.se
  
  tibble(
    sample = sample,
    cs_estimate = estimate,
    cs_se = std_error,
    cs_p = 2 * pnorm(abs(estimate / std_error), lower.tail = FALSE)
  )
}

cs_table <- bind_rows(
  extract_cs(res_all, "Full sample"),
  extract_cs(res_men, "Men"),
  extract_cs(res_women, "Women"),
  extract_cs(res_women_minus_men, "Women - Men")
)

run_twfe <- function(df, sample) {
  model <- feols(
    emp_pop_ratio ~ treated | STATEFIP + YEAR,
    data = df %>%
      mutate(treated = if_else(first_treat > 0 & YEAR >= first_treat, 1, 0)),
    weights = ~ cell_weight,
    cluster = ~ STATEFIP
  )
  
  estimate <- 100 * coef(model)["treated"]
  std_error <- 100 * se(model)["treated"]
  
  tibble(
    sample = sample,
    twfe_estimate = estimate,
    twfe_se = std_error,
    twfe_p = 2 * pnorm(abs(estimate / std_error), lower.tail = FALSE)
  )
}

twfe_table <- bind_rows(
  run_twfe(data_all, "Full sample"),
  run_twfe(data_men, "Men"),
  run_twfe(data_women, "Women"),
  run_twfe(data_women_minus_men, "Women - Men")
)

table02 <- left_join(cs_table, twfe_table, by = "sample") %>%
  transmute(
    Sample = sample,
    `Callaway-Sant'Anna` = sprintf(
      "%.3f%s (%.3f)",
      cs_estimate,
      ifelse(cs_p < 0.05, "**", ""),
      cs_se
    ),
    TWFE = sprintf(
      "%.3f%s (%.3f)",
      twfe_estimate,
      ifelse(twfe_p < 0.05, "**", ""),
      twfe_se
    )
  )

write_csv(table02, "Table02_main_results.csv")


extract_dynamic <- function(res, sample) {
  tibble(
    sample = sample,
    event_time = res$dynamic$egt,
    estimate_pp = 100 * res$dynamic$att.egt,
    ci_low_pp = 100 * (res$dynamic$att.egt - 1.96 * res$dynamic$se.egt),
    ci_high_pp = 100 * (res$dynamic$att.egt + 1.96 * res$dynamic$se.egt)
  )
}

event_gender <- bind_rows(
  extract_dynamic(res_men, "Men"),
  extract_dynamic(res_women, "Women")
) %>%
  filter(event_time != -1) %>%
  bind_rows(
    tibble(
      sample = c("Men", "Women"),
      event_time = -1,
      estimate_pp = 0,
      ci_low_pp = NA_real_,
      ci_high_pp = NA_real_
    )
  )

fig_event_gender <- ggplot(event_gender, aes(event_time, estimate_pp)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point() +
  geom_errorbar(
    data = event_gender %>% filter(!is.na(ci_low_pp)),
    aes(ymin = ci_low_pp, ymax = ci_high_pp),
    width = 0.15
  ) +
  facet_wrap(~ sample, nrow = 1) +
  scale_x_continuous(breaks = -5:5) +
  labs(
    x = "Years relative to minimum wage policy initiation",
    y = "Effect on employment-to-population ratio, percentage points",
    title = "Event-study estimates relative to policy-path initiation"
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = grid::unit(2, "cm"),
    panel.border = element_rect(fill = NA)
  )

ggsave(
  "Fig01_event_study_gender.png",
  fig_event_gender,
  width = 8.5,
  height = 4.5,
  dpi = 300
)


treated_sizes <- data_all %>%
  filter(first_treat > 0) %>%
  distinct(STATEFIP, first_increase_size)

median_increase <- median(treated_sizes$first_increase_size, na.rm = TRUE)

data_size <- data %>%
  mutate(
    increase_group = case_when(
      first_treat == 0 ~ "Never treated",
      first_increase_size < median_increase ~ "Small increase",
      first_increase_size >= median_increase ~ "Large increase"
    )
  )

run_size_cs <- function(df, group_name, gender_name) {
  df <- df %>%
    filter(increase_group %in% c(group_name, "Never treated")) %>%
    mutate(emp_pop_ratio = emp_rate)
  
  att <- att_gt(
    yname = "emp_pop_ratio",
    tname = "YEAR",
    idname = "STATEFIP",
    gname = "first_treat",
    weightsname = "cell_weight",
    xformla = ~ 1,
    data = df,
    panel = TRUE,
    control_group = "notyettreated",
    anticipation = 0,
    bstrap = TRUE,
    biters = 999,
    clustervars = "STATEFIP",
    print_details = FALSE
  )
  
  simple <- aggte(att, type = "simple", bstrap = TRUE, biters = 999)
  
  tibble(
    Sample = gender_name,
    `Increase group` = group_name,
    estimate = 100 * simple$overall.att,
    std_error = 100 * simple$overall.se,
    p_value = 2 * pnorm(
      abs(simple$overall.att / simple$overall.se),
      lower.tail = FALSE
    )
  )
}

estimate_size_att <- function(df, group_name) {
  df <- df %>%
    filter(increase_group %in% c(group_name, "Never treated")) %>%
    mutate(emp_pop_ratio = emp_rate)
  
  att <- att_gt(
    yname = "emp_pop_ratio",
    tname = "YEAR",
    idname = "STATEFIP",
    gname = "first_treat",
    weightsname = "cell_weight",
    xformla = ~ 1,
    data = df,
    panel = TRUE,
    control_group = "notyettreated",
    anticipation = 0,
    bstrap = FALSE,
    print_details = FALSE
  )
  
  aggte(att, type = "simple", bstrap = FALSE)$overall.att
}

jackknife_size_difference <- function(df, gender_name) {
  df <- df %>% filter(gender == gender_name)
  
  estimate <- estimate_size_att(df, "Large increase") -
    estimate_size_att(df, "Small increase")
  
  state_ids <- sort(unique(df$STATEFIP))
  
  leave_one_out <- vapply(
    state_ids,
    function(state_id) {
      df_without_state <- df %>% filter(STATEFIP != state_id)
      
      tryCatch(
        estimate_size_att(df_without_state, "Large increase") -
          estimate_size_att(df_without_state, "Small increase"),
        error = function(e) NA_real_
      )
    },
    numeric(1)
  )
  
  if (any(is.na(leave_one_out))) {
    stop("At least one leave-one-state-out estimate failed.")
  }
  
  std_error <- sqrt(
    (length(state_ids) - 1) / length(state_ids) *
      sum((leave_one_out - mean(leave_one_out))^2)
  )
  
  tibble(
    Sample = gender_name,
    `Increase group` = "Large - Small",
    estimate = 100 * estimate,
    std_error = 100 * std_error,
    p_value = 2 * pnorm(abs(estimate / std_error), lower.tail = FALSE)
  )
}

table03 <- bind_rows(
  run_size_cs(data_size %>% filter(gender == "Men"), "Small increase", "Men"),
  run_size_cs(data_size %>% filter(gender == "Men"), "Large increase", "Men"),
  jackknife_size_difference(data_size, "Men"),
  run_size_cs(data_size %>% filter(gender == "Women"), "Small increase", "Women"),
  run_size_cs(data_size %>% filter(gender == "Women"), "Large increase", "Women"),
  jackknife_size_difference(data_size, "Women")
) %>%
  transmute(
    Sample,
    `Increase group`,
    Estimate = sprintf("%.3f (%.3f)", estimate, std_error),
    `p-value` = sprintf("%.3f", p_value)
  )

write_csv(table03, "Table03_heterogeneity.csv")

cat("\n02_Empirical.R completed successfully.\n")