library(megadaptr)
library(magrittr)
library(tidyr)
source('../scenarios/util.R')

cache_path <- "budget_experiment_cross_section"
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
  "water_infrastructure_age", "Mean Water Infrastructure Age"
)

get_name <- function(colname) {
  meta$name[meta$colname == colname]
}

scenarios <- load_scenarios(cache) %>% tidyr::unnest()
df <- scenarios %>%
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
    nonpotable_water_infrastructure_age=mean(potable_water_infrastructure_age)) %>%
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
    nonpotable_water_infrastructure_age
  ) %>%
  extract(name, into = c("system", "statistic"), "([[:alnum:]]+)_(.+)") %>%
  mutate(statistic = recode(statistic, !!! (meta %>% spread(colname, name) %>% as.list))) %>%
  mutate(system = recode(system, potable = "Potable", nonpotable = "Non Potable")) %>%
  rename(Budget=budget)

ggplot(df, aes_string(x = "year_sim", y = "value", color = "Budget")) +
  xlab("Year") +
  geom_line() +
  facet_wrap(vars(system, statistic), scales = "free_y", ncol = 4)

