source('r/setup.R')
source('r/example.R')

set.seed(1000)

results <- simulate_megadapt(megadapt)
print(results %>%
  group_by(year_sim) %>%
  summarize(mean_day_without_water_per_month=mean(days_wn_water_month),
            mean_water_capacity=mean(capac_w, na.rm=T),
            mean_pooling=mean(encharca),
            mean_social_pressure=mean(social_pressure),
            mean_sensitivity_Ab=mean(sensitivity_Ab),
            mean_vulnerability_Ab=mean(vulnerability_Ab, na.rm=T),
            mean_interventions_Ab=mean(Interventions_Ab)))