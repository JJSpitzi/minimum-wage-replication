packages <- c("ipumsr", "readxl", "dplyr", "tibble", "readr")
for (p in packages) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(ipumsr)
library(readxl)
library(dplyr)
library(tibble)
library(readr)

ddi <- read_ipums_ddi("usa_00003.xml")

acs <- read_ipums_micro(
  ddi,
  data_file = "usa_00003.dat",
  vars = c("YEAR", "STATEFIP", "GQ", "PERWT", "SEX", "AGE", "EMPSTAT")
)

mw <- read_xlsx("VZ_state_annual.xlsx") %>%
  transmute(
    STATEFIP = as.integer(`State FIPS Code`),
    state_name = as.character(Name),
    state_abbr = as.character(`State Abbreviation`),
    YEAR = as.integer(Year),
    federal_average = as.numeric(`Annual Federal Average`),
    state_average = as.numeric(`Annual State Average`),
    federal_maximum = as.numeric(`Annual Federal Maximum`),
    state_maximum = as.numeric(`Annual State Maximum`)
  )

sample_start <- max(2001, min(acs$YEAR), min(mw$YEAR))
sample_end <- min(max(acs$YEAR), max(mw$YEAR))


acs_cells <- acs %>%
  filter(
    YEAR >= sample_start, YEAR <= sample_end,
    GQ %in% c(1, 2),
    AGE >= 16, AGE <= 64,
    SEX %in% c(1, 2),
    EMPSTAT %in% c(1, 2, 3),
    !is.na(PERWT)
  ) %>%
  mutate(
    female = if_else(SEX == 2, 1L, 0L),
    employed = if_else(EMPSTAT == 1, 1, 0)
  ) %>%
  group_by(STATEFIP, YEAR, female) %>%
  summarise(
    emp_rate = weighted.mean(employed, PERWT, na.rm = TRUE),
    cell_weight = sum(PERWT, na.rm = TRUE),
    n_persons = n(),
    mean_age = weighted.mean(AGE, PERWT, na.rm = TRUE),
    .groups = "drop"
  )


eps <- 0.005

mw_sample <- mw %>%
  filter(YEAR >= sample_start, YEAR <= sample_end) %>%
  arrange(STATEFIP, YEAR) %>%
  mutate(
    federal_average = round(federal_average, 3),
    state_average = round(state_average, 3),
    federal_maximum = round(federal_maximum, 3),
    state_maximum = round(state_maximum, 3),
    effective_mw = pmax(state_average, federal_average, na.rm = TRUE),
    state_above_federal = state_average > federal_average + eps
  )

treat_info <- mw_sample %>%
  group_by(STATEFIP, state_name, state_abbr) %>%
  summarise(
    state_above_start = state_above_federal[YEAR == sample_start][1],
    first_treat = ifelse(
      any(state_above_federal & YEAR > sample_start, na.rm = TRUE),
      min(YEAR[state_above_federal & YEAR > sample_start], na.rm = TRUE),
      0
    ),
    .groups = "drop"
  ) %>%
  mutate(
    already_above_start = state_above_start,
    first_treat = ifelse(already_above_start, 0, first_treat),
    ever_treated_in_sample = first_treat != 0,
    never_treated = !already_above_start & first_treat == 0
  )


first_increase_sizes <- mw_sample %>%
  select(STATEFIP, YEAR, state_average) %>%
  left_join(treat_info %>% select(STATEFIP, first_treat), by = "STATEFIP") %>%
  group_by(STATEFIP) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    lag_state_average = lag(state_average),
    first_increase_size_tmp = ifelse(
      YEAR == first_treat,
      state_average - lag_state_average,
      NA_real_
    )
  ) %>%
  summarise(
    first_increase_size =
      first(first_increase_size_tmp[!is.na(first_increase_size_tmp)],
            default = NA_real_),
    .groups = "drop"
  )

second_increase_info <- mw_sample %>%
  select(STATEFIP, YEAR, state_average) %>%
  left_join(
    treat_info %>% select(STATEFIP, first_treat, already_above_start),
    by = "STATEFIP"
  ) %>%
  group_by(STATEFIP) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    state_increase =
      !is.na(lag(state_average)) &
      state_average > lag(state_average) + eps,
    second_increase_candidate =
      !already_above_start &
      first_treat > 0 &
      YEAR > first_treat &
      state_increase
  ) %>%
  summarise(
    second_increase_year = {
      x <- YEAR[coalesce(second_increase_candidate, FALSE)]
      if (length(x) == 0) 0 else min(x)
    },
    .groups = "drop"
  )

treat_info <- treat_info %>%
  left_join(first_increase_sizes, by = "STATEFIP") %>%
  left_join(second_increase_info, by = "STATEFIP")


estimation_cells_all <- acs_cells %>%
  inner_join(
    mw_sample %>%
      select(
        STATEFIP, YEAR, state_name, state_abbr,
        federal_average, state_average,
        effective_mw, state_above_federal
      ),
    by = c("STATEFIP", "YEAR")
  ) %>%
  left_join(
    treat_info %>%
      select(
        STATEFIP, already_above_start, first_treat,
        first_increase_size, second_increase_year,
        ever_treated_in_sample, never_treated
      ),
    by = "STATEFIP"
  ) %>%
  mutate(
    treated = if_else(first_treat > 0 & YEAR >= first_treat, 1L, 0L),
    post = treated,
    after_second_increase =
      first_treat > 0 &
      second_increase_year > 0 &
      YEAR >= second_increase_year,
    gender = if_else(female == 1, "Women", "Men")
  )

estimation_cells_main <- estimation_cells_all %>%
  filter(!already_above_start)

estimation_cells_no_later_increases <- estimation_cells_all %>%
  filter(!already_above_start, !after_second_increase)


table01 <- tibble(
  Variable = c(
    "Employment-to-population ratio",
    "Employment-to-population ratio, men",
    "Employment-to-population ratio, women",
    "Effective minimum wage ($/hour)",
    "Mean age (years)",
    "Number of states",
    "State-year-sex cells",
    "Treated states",
    "Never-treated states"
  ),
  Mean = c(
    mean(estimation_cells_main$emp_rate, na.rm = TRUE),
    mean(estimation_cells_main$emp_rate[estimation_cells_main$gender == "Men"], na.rm = TRUE),
    mean(estimation_cells_main$emp_rate[estimation_cells_main$gender == "Women"], na.rm = TRUE),
    mean(estimation_cells_main$effective_mw, na.rm = TRUE),
    mean(estimation_cells_main$mean_age, na.rm = TRUE),
    n_distinct(estimation_cells_main$STATEFIP),
    nrow(estimation_cells_main),
    n_distinct(estimation_cells_main$STATEFIP[estimation_cells_main$first_treat > 0]),
    n_distinct(estimation_cells_main$STATEFIP[estimation_cells_main$first_treat == 0])
  ),
  SD = c(
    sd(estimation_cells_main$emp_rate, na.rm = TRUE),
    sd(estimation_cells_main$emp_rate[estimation_cells_main$gender == "Men"], na.rm = TRUE),
    sd(estimation_cells_main$emp_rate[estimation_cells_main$gender == "Women"], na.rm = TRUE),
    sd(estimation_cells_main$effective_mw, na.rm = TRUE),
    sd(estimation_cells_main$mean_age, na.rm = TRUE),
    NA, NA, NA, NA
  )
)

write_csv(table01, "Table01_summary_statistics.csv")


table07 <- treat_info %>%
  filter(!already_above_start) %>%
  mutate(
    `First treatment year` =
      if_else(first_treat == 0, "Never treated", as.character(first_treat))
  ) %>%
  count(`First treatment year`, name = "Number of states") %>%
  mutate(
    order = if_else(
      `First treatment year` == "Never treated",
      Inf,
      as.numeric(`First treatment year`)
    )
  ) %>%
  arrange(order) %>%
  select(-order)

write_csv(table07, "Table07_treatment_timing.csv")


table08 <- treat_info %>%
  filter(!already_above_start, first_treat > 0) %>%
  arrange(first_treat, state_name) %>%
  group_by(first_treat) %>%
  summarise(States = paste(state_name, collapse = ", "), .groups = "drop") %>%
  rename(`First treatment year` = first_treat)

write_csv(table08, "Table08_treated_states.csv")


table09 <- treat_info %>%
  filter(!already_above_start, first_treat == 0) %>%
  arrange(state_name) %>%
  transmute(State = state_name)

write_csv(table09, "Table09_never_treated_states.csv")


table10 <- treat_info %>%
  filter(already_above_start) %>%
  arrange(state_name) %>%
  transmute(State = state_name)

write_csv(table10, "Table10_already_treated_states.csv")


table11 <- mw_sample %>%
  distinct(YEAR, federal_average) %>%
  arrange(YEAR) %>%
  mutate(
    period = cumsum(
      federal_average != lag(
        federal_average,
        default = first(federal_average)
      )
    )
  ) %>%
  group_by(period, federal_average) %>%
  summarise(
    start_year = min(YEAR),
    end_year = max(YEAR),
    .groups = "drop"
  ) %>%
  transmute(
    Year = if_else(
      start_year == end_year,
      as.character(start_year),
      paste0(start_year, "–", end_year)
    ),
    `Annual-average federal minimum wage ($/hour)` = federal_average
  )

write_csv(table11, "Table11_federal_minimum_wage.csv")


saveRDS(estimation_cells_main, "estimation_cells_main.rds")
saveRDS(estimation_cells_all, "estimation_cells_all.rds")
saveRDS(
  estimation_cells_no_later_increases,
  "estimation_cells_no_later_increases.rds"
)

cat("\n01_Data.R completed successfully.\n")