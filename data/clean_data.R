# Import and clean data for app
library(dplyr)
library(here)
library(readr)
library(stringr)

version_suffix <- "v8_2024-09-12"

#### Import data ####
all_costs <- read_delim(
  here("data", paste0("all_costs_", version_suffix, ".csv")),
  delim = "|",
  col_types = list(.default = "d", "cost_component" = "c", "var_pop" = "c")
)
  
assess_ph_dat <- read_delim(
    here("data", paste0("assess_ph_dat_", version_suffix, ".csv")),
    delim = "|",
    col_types = list(.default = "c", fu_time = "d", surv = "d")
  ) %>%
  rename(var_pop_num = var_pop)

codelist <-  read_delim(
  here("data", paste0("codelist_", version_suffix, ".csv")),
  delim = "|",
  col_types = list(.default = "c"),
  locale = readr::locale(encoding = "latin1")
)

concurrent_diseases <-  read_delim(
  here("data", paste0("concurrent_diseases_", version_suffix, ".csv")),
  delim = "|",
  col_types = list(.default = "c", conc_dis_percent = "d")
)

cost_reg_est <-  read_delim(
  here("data", paste0("cost_reg_est_", version_suffix, ".csv")),
  delim = "|",
  col_types = list(
    .default = "c",
    "Estimate" = "d",
    "LowerCL" = "d",
    "UpperCL" = "d"
  )
)

format_var_pop <- read_delim(
  here("data", paste0("format_var_pop_", version_suffix, ".csv")),
  delim = "|",
  col_types = list(.default = "c")
) %>%
rename(var_pop_num = start, var_pop = label) %>%
select(var_pop_num, var_pop)

pat_char_tbl_dat <-  read_delim(
  here("data", paste0("pat_char_tbl_dat_", version_suffix, ".csv")),
  delim = "|",
  col_types = list(
    .default = "c",
    "__stat_num1" = "d",
    "__stat_num2" = "d",
    "__stat_num3" = "d"
  )
)

prev_inc <-  read_delim(
  here("data", paste0("prev_inc_", version_suffix, ".csv")),
  delim = "|",
  col_types = list(.default = "d","var_pop" = "c")
)

risk_cox <-  read_delim(
  here("data", paste0("risk_cox_", version_suffix, ".csv")),
  delim = "|",
  col_types = list(.default = "d","var_pop" = "c", "case" = "c")
)

#### Create additional datasets ####

var_name_labels <- codelist %>%
  filter(group %in% c("var_def01", "var_def02", "var_def03", "var_def04", "cci")) %>%
  select(var_name, var_label) %>%
  rename(var_name_label = var_label) %>%
  distinct()

cost_component_labels <- all_costs %>%
  select(cost_component) %>%
  distinct() %>%
  mutate(
    cost_component_label = case_match(
      cost_component,
      "secondary_sector" ~ "Hospital care",
      "home_care" ~ "Home care",
      "filled_prescriptions" ~ "Filled prescriptions",
      "nursing_home" ~ "Nursing home / sheltered accomodation",
      "primary_sector" ~ "Primary sector",
      "lost_production_sickness" ~ "Lost productivity associated with illness",
      .default  = cost_component
    )
  )

var_def_labels <- tibble(
  var_def = c("def01", "def02", "def03", "def04"),
  var_def_label = c(
    "Sensitivity analysis 1", "Sensitivity analysis 2",
    "Main analysis", "Sensitivity analysis 3")
)

population_labels <- codelist %>%
  filter(group == "population") %>%
  select(var_name) %>%
  mutate(
    population_label = case_match(
      var_name,
      "inc_2011_2015" ~ "Incident cohort 2011-2015",
      "inc_2016_2021" ~ "Incident cohort 2016-2021",
      "prev_2015" ~ "Prevalent cohort 2015",
      "prev_2016" ~ "Prevalent cohort 2016",
      "prev_2017" ~ "Prevalent cohort 2017",
      "prev_2018" ~ "Prevalent cohort 2018",
      "prev_2019" ~ "Prevalent cohort 2019",
      "prev_2020" ~ "Prevalent cohort 2020",
      "prev_2021" ~ "Prevalent cohort 2021",
      .default = var_name
    )
  ) %>%
  rename(population = var_name)

#### Add variables to data ####
all_costs <- all_costs %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    var_def = word(var_name, 1, sep = "_"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(cost_component_labels, by = "cost_component") %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(var_def_labels, by = "var_def") %>%
  left_join(population_labels, by = "population")

assess_ph_dat <- assess_ph_dat %>%
  left_join(format_var_pop, by = "var_pop_num") %>%
  select(var_pop, case, fu_time, surv) %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    var_def = word(var_name, 1, sep = "_"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(var_def_labels, by = "var_def") %>%
  left_join(population_labels, by = "population")


concurrent_diseases <- concurrent_diseases %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    var_def = word(var_name, 1, sep = "_"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(var_def_labels, by = "var_def") %>%
  left_join(population_labels, by = "population") %>%
  mutate(
    conc_dis_percent = ifelse(var_name == concurrent_disease, NA, conc_dis_percent),
    conc_dis_pct_fmt = formatC(conc_dis_percent, digits = 1, format = "f", decimal.mark = "."),
    conc_dis_pct_fmt = ifelse(conc_dis_pct_fmt == "NA", "", conc_dis_percent),
    conc_dis_order = as.integer(word(concurrent_disease, start = 2, sep = "_")),
    conc_dis_order = ifelse(
      word(concurrent_disease, start = 1, sep = "_") == "cci",
      conc_dis_order + 100,
      conc_dis_order
    )
  ) %>%
  filter(
    concurrent_disease != "cci_5" &
    word(concurrent_disease, 2, sep = "_") != "00" &
    word(var_name, 2, sep = "_") != "00"
  ) %>%
  left_join(
    var_name_labels %>% rename(concurrent_disease_label = var_name_label),
    by = join_by(concurrent_disease == var_name)
  )

dkk_to_eur <- codelist %>%
filter(var_name == "eur_to_dkk") %>%
select(code_include) %>%
pull() %>%
as.numeric()

cost_reg_est <- cost_reg_est %>%
  rename_all(tolower) %>%
  mutate(var_name = word(parameter, 1, sep = " ")) %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(var_def_labels, by = "var_def") %>%
  left_join(population_labels, by = "population") %>%
  filter(
    model %in% c("cci#all", "no_cci#all")
    & estimate != 0
    & word(model, 2, sep = "#") == "all"
  ) %>%
  rename(ucl = uppercl, lcl = lowercl) %>%
  mutate(
    model = word(model, sep = fixed("#")),
    model_label = case_match(
      model,
      "cci" ~ "CCI variables included",
      "no_cci" ~ "CCI variables excluded"
    ),
    # Add mising labels
    var_name_label = case_when(
      var_name == "Intercept" ~ "Intercept",
      var_name == "male" ~ "Male",
      parameter == "age_g     1" ~ "Age 10-19",
      parameter == "age_g     2" ~ "Age 20-29",
      parameter == "age_g     3" ~ "Age 30-39",
      parameter == "age_g     4" ~ "Age 40-49",
      parameter == "age_g     5" ~ "Age 50-59",
      parameter == "age_g     6" ~ "Age 60-69",
      parameter == "age_g     7" ~ "Age 70-79",
      parameter == "age_g     8" ~ "Age 80-89",
      parameter == "age_g     9" ~ "Age 90+",
      .default = var_name_label
    ),
    # Recalculate costs from 100.000 DKK to 10.000 EUR
    estimate = estimate * 10 / dkk_to_eur,
    lcl = lcl * 10 / dkk_to_eur,
    ucl = ucl * 10 / dkk_to_eur,
    est_ci = paste0(
      formatC(estimate, digits = 2, format = "f"),
      " (",
      formatC(lcl, digits = 2, format = "f"),
      " - ",
      formatC(ucl, digits = 2, format = "f"),
      ")"
    )
  )
  
pat_char_tbl_dat <- pat_char_tbl_dat %>%
  rename(
    stat_char = `__stat_char`,
    label = `__label`,
    var = `__var`,
    stat_num1 = `__stat_num1`,
    stat_num2 = `__stat_num2`,
    stat_num3 = `__stat_num3`
  ) %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    var_def = word(var_name, 1, sep = "_"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(var_def_labels, by = "var_def") %>%
  left_join(population_labels, by = "population") %>%
  mutate(
    var_order = case_match(
      label,
      "__n" ~ 1,
      "male" ~ 2,
      "age_index" ~ 3,
      "cci_g: title" ~ 4,
      "cci_g: 0" ~ 5,
      "cci_g: 1-2" ~ 6,
      "cci_g: +3" ~ 7
    )
  )
  
prev_inc <- prev_inc %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    var_def = word(var_name, 1, sep = "_"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(var_def_labels, by = "var_def") %>%
  left_join(population_labels, by = "population")

risk_cox <- risk_cox %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    var_def = word(var_name, 1, sep = "_"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(var_def_labels, by = "var_def") %>%
  left_join(population_labels, by = "population")

#### Reorder variables and sort data ####
all_costs <- all_costs %>%
  relocate(
    var_pop,
    var_def,
    var_def_label,
    population,
    population_label,
    var_name,
    var_name_label,
    cost_component,
    cost_component_label,
    act_cost,
    act_py,
    att_cost,
    att_py,
    act_cost_py,
    att_cost_py
  ) %>%
  arrange(var_pop, var_name, cost_component)

concurrent_diseases <- concurrent_diseases %>%
  relocate(
    var_pop,
    var_def,
    var_def_label,
    population,
    population_label,
    var_name,
    var_name_label,
    concurrent_disease,
    concurrent_disease_label,
    conc_dis_percent,
    conc_dis_pct_fmt,
    conc_dis_order
  ) %>%
  arrange(var_pop, var_name, concurrent_disease)

cost_reg_est <- cost_reg_est %>%
  relocate(
    var_def,
    var_def_label,
    population,
    population_label,
    model,
    model_label,
    var_name,
    var_name_label,
    parameter,
    estimate,
    lcl,
    ucl,
    est_ci
  ) %>%
  arrange(var_def, population, model, var_name)

risk_cox <- risk_cox %>%
  relocate(
    var_pop,
    var_def,
    var_def_label,
    population,
    population_label,
    var_name,
    var_name_label,
    case,
    n_pt,
    deaths_n,
    deaths_prop,
    hr,
    hr_lcl,
    hr_ucl
  ) %>%
  arrange(var_pop, var_name, case)

pat_char_tbl_dat <- pat_char_tbl_dat %>%
  relocate(
    var_pop,
    var_def,
    var_def_label,
    population,
    population_label,
    var_name,
    var_name_label,
    var,
    label,
    stat_char,
    stat_num1,
    stat_num2,
    stat_num3
  ) %>%
  arrange(var_pop, var_name, var_order)

prev_inc <- prev_inc %>%
  relocate(
    var_pop,
    var_def,
    var_def_label,
    population,
    population_label,
    var_name,
    var_name_label,
    n_diag,
    risk_time
  ) %>%
  arrange(var_pop, var_name)


#### Save cleaned data for app ####

# We choose not to compress the data to make it slightly faster to load
# the app
saveRDS(all_costs, here("data", "all_costs.rds"), compress = FALSE)
saveRDS(assess_ph_dat, here("data", "assess_ph_dat.rds"), compress = FALSE)
saveRDS(codelist, here("data", "codelist.rds"), compress = FALSE)
saveRDS(concurrent_diseases, here("data", "concurrent_diseases.rds"), compress = FALSE)
saveRDS(cost_reg_est, here("data", "cost_reg_est.rds"), compress = FALSE)
saveRDS(pat_char_tbl_dat, here("data", "pat_char_tbl_dat.rds"), compress = FALSE)
saveRDS(prev_inc, here("data", "prev_inc.rds"), compress = FALSE)
saveRDS(risk_cox, here("data", "risk_cox.rds"), compress = FALSE)


  