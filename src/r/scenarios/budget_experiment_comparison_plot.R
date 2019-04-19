library(megadaptr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
source('../scenarios/util.R')

cache_path <- "../scenarios/budget_experiment_cross_section"
megadapt <- build_megadapt_model(
  data_root_dir = data_root_dir,
  mental_model_file_names = mental_model_file_names
)
if (fs::dir_exists(cache_path)) {
  cache <- load_scenario_cache(cache_path)
} else {
  cache <- create_cartesian_scenario_cache(
    model = megadapt,
    path = cache_path,
    params = list(budget=6:12*100))
}

meta <- tibble::tribble(
  ~colname, ~name,
  "water_vulnerability_index", "Mean Water Vulnerability Index",
  "water_system_intervention_count", "Mean Water System Interventions",
  "water_sensitivity_index", "Mean Water Sensitivity Index",
  "water_infrastructure_age", "Mean Water Infrastructure Age",
  "percent_lacking", "Percent Lacking Public Infrastructure"
)

get_name <- function(colname) {
  meta$name[meta$colname == colname]
}

scenarios <- as_tibble(load_scenarios(cache) %>% tidyr::unnest())
potable_non_potable_comparison <- scenarios %>%
  filter(year_sim != 2018) %>%
  mutate(budget = factor(budget)) %>%
  group_by(budget, year_sim) %>%
  summarize(
    potable_water_vulnerability_index=mean(potable_water_vulnerability_index, na.rm = TRUE),
    nonpotable_water_vulnerability_index=mean(non_potable_water_vulnerability_index, na.rm = TRUE),
    potable_water_system_intervention_count=mean(potable_water_system_intervention_count),
    nonpotable_water_system_intervention_count=mean(potable_water_system_intervention_count),
    potable_water_sensitivity_index=mean(potable_water_sensitivity_index),
    nonpotable_water_sensitivity_index=mean(non_potable_water_sensitivity_index),
    potable_water_infrastructure_age=mean(potable_water_infrastructure_age),
    nonpotable_water_infrastructure_age=mean(potable_water_infrastructure_age),
    potable_percent_lacking = mean(potable_percent_lacking),
    nonpotable_percent_lacking = mean(non_potable_percent_lacking)) %>%
  gather(
    key = "name",
    value = "value",
    potable_water_vulnerability_index,
    nonpotable_water_vulnerability_index,
    potable_water_system_intervention_count,
    nonpotable_water_system_intervention_count,
    potable_water_sensitivity_index,
    nonpotable_water_sensitivity_index,
    potable_water_infrastructure_age,
    nonpotable_water_infrastructure_age,
    potable_percent_lacking,
    nonpotable_percent_lacking
  ) %>%
  extract(name, into = c("system", "statistic"), "([[:alnum:]]+)_(.+)") %>%
  mutate(statistic = recode(statistic, !!! (meta %>% spread(colname, name) %>% as.list))) %>%
  mutate(system = recode(system, potable = "Potable", nonpotable = "Non Potable")) %>%
  rename(Budget=budget)

ggplot(potable_non_potable_comparison, aes_string(x = "year_sim", y = "value", color = "Budget")) +
  xlab("Year") +
  geom_line() +
  facet_wrap(vars(system, statistic), scales = "free_y", ncol = 5)

meta_water_outcomes <- tibble::tribble(
  ~colname, ~name,
  "mean_days_with_flooding", "Mean Days With Flooding",
  "mean_days_with_ponding", "Mean Days With Ponding",
  "mean_water_scarcity_index", "Mean Water Scarcity Index"
)

water_outcomes <- scenarios %>%
  filter(year_sim != 2018) %>%
  mutate(budget = factor(budget)) %>%
  rename(Budget = budget) %>%
  group_by(Budget, year_sim) %>%
  summarize(
    mean_days_with_flooding = mean(days_with_flooding),
    mean_days_with_ponding = mean(days_with_ponding),
    mean_water_scarcity_index = mean(water_scarcity_index)
  ) %>%
  gather(key = 'name', value = 'value',
         mean_days_with_flooding,
         mean_days_with_ponding,
         mean_water_scarcity_index) %>%
  mutate(name = recode(name, !!! (meta_water_outcomes %>% spread(colname, name))))

ggplot(water_outcomes, aes(x = year_sim, y = value, color = Budget)) +
  geom_line() +
  xlab("Year") +
  facet_wrap(vars(name), scale="free_y")

