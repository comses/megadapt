library(dplyr)
library(RColorBrewer)
library(glue)

components <- tibble::tribble(
  ~component_name,
  'update_scarcity_model',
  'update_climate_model',
  'update_ponding',
  'determine_site_suitability',
  'determine_site_selection',
  'take_actions_sacmcx',
  'take_actions_residents',
  'update_protests',
  'update_adaptation_and_sensitivity',
  'update_infrastructure_age'
)

input <- tibble::tribble(
  ~component_name, ~study_data,
  'update_scarcity_model', 'critic_z',
  'update_scarcity_model', 'antiguedad_dist',
  'update_scarcity_model', 'falta_dist',
  'update_ponding', 'f_prec_v',
  'update_ponding', 'f_esc',
  'update_ponding', 'n_tramos',
  'update_ponding', 'q100',
  'update_ponding', 'bombeo_tot',
  'update_ponding', 'rejillas',
  'determine_site_suitability', 'antiguedad_dist',
  'determine_site_suitability', 'antiguedad_dren',
  'determine_site_suitability', 'q100',
  'determine_site_suitability', 'falta_dist',
  'determine_site_suitability', 'falta_dren',
  'determine_site_suitability', 'falta_in',
  'determine_site_suitability', 'days_wn_water_year',
  'determine_site_suitability', 'encharca',
  'determine_site_suitability', 'social_pressure',
  'determine_site_suitability', 'PR_2008',
  'determine_site_suitability', 'escurri',
  'determine_site_suitability', 'basura',
  'determine_site_suitability', 'subsidenci',
  'determine_site_suitability', 'pres_hid',
  'determine_site_suitability', 'ageb_id',
  'determine_site_suitability', 'gasto',
  'determine_site_suitability', 'abastecimi',
  'determine_site_suitability', 'pet_del_dr',
  'determine_site_suitability', 'pet_usr_d',
  'determine_site_suitability', 'pres_med',
  'determine_site_suitability', 'crec_rb',
  'determine_site_suitability', 'cal_agua',
  'determine_site_suitability', 'enf_14',
  'determine_site_suitability', 'NOWater_twoweeks',
  'determine_site_suitability', 'desv_agua',
  'determine_site_suitability', 'desp_agua',
  'determine_site_suitability', 'days_wn_water_month',
  'determine_site_suitability', 'pop_growth',
  'determine_site_suitability', 'falla_dist',
  'take_actions_sacmcx', 'antiguedad_dren',
  'take_actions_sacmcx', 'q100',
  'take_actions_sacmcx', 'Interventions_D',
  'take_actions_sacmcx', 'falta_dren',
  'take_actions_sacmcx', 'bombeo_tot',
  'take_actions_sacmcx', 'antiguedad_dist',
  'take_actions_sacmcx', 'Interventions_Ab',
  'take_actions_sacmcx', 'falta_dist',
  'update_adaptation_and_sensitivity', 'house_modifications_D',
  'update_adaptation_and_sensitivity', 'house_modifications_Ab',
  'update_adaptation_and_sensitivity', 'days_wn_water_year',
  'update_adaptation_and_sensitivity', 'ingreso',
  'update_adaptation_and_sensitivity', 'encharca',
  'update_infrastructure_age', 'antiguedad_dren',
  'update_infrastructure_age', 'antiguedad_dist',
  'update_infrastructure_age', 'capac_w',
  'update_infrastructure_age', 'falla_dist',
  'update_infrastructure_age', 'pop_growth',
  'update_infrastructure_age', 'falta_dren'
)

output <- tibble::tribble(
  ~component_name, ~study_data,
  'update_scarcity_model', 'NOWater_twoweeks',
  'update_scarcity_model', 'NOWater_week_pois',
  'update_scarcity_model', 'days_wn_water_month',
  'update_scarcity_model', 'days_wn_water_year',
  'update_climate_model', 'f_prec_v',
  'update_climate_model', 'f_esc',
  'update_ponding', 'encharca',
  'take_actions_sacmcx', 'antiguedad_dren',
  'take_actions_sacmcx', 'q100',
  'take_actions_sacmcx', 'Interventions_D',
  'take_actions_sacmcx', 'falta_dren',
  'take_actions_sacmcx', 'bombeo_tot',
  'take_actions_sacmcx', 'antiguedad_dist',
  'take_actions_sacmcx', 'Interventions_Ab',
  'take_actions_sacmcx', 'falta_dist',
  'update_protests', 'protesta',
  'update_protests', 'social_pressure',
  'update_adaptation_and_sensitivity', 'house_modifications_D',
  'update_adaptation_and_sensitivity', 'sensitivity_D',
  'update_adaptation_and_sensitivity', 'house_modifications_Ab',
  'update_adaptation_and_sensitivity', 'sensitivity_Ab',
  'update_adaptation_and_sensitivity', 'vulnerability_Ab',
  'update_adaptation_and_sensitivity', 'vulnerability_D',
  'update_infrastructure_age', 'antiguedad_dren',
  'update_infrastructure_age', 'antiguedad_dist',
  'update_infrastructure_age', 'capac_w',
  'update_infrastructure_age', 'falta_dist',
  'update_infrastructure_age', 'falta_dren'

)

rw_display <- function(type) {
  readable <- sum(type == 'Read')
  writable <- sum(type == 'Write')
  if (readable && writable) {
    return(glue('{readable} r / {writable} w'))
  } else if (readable) {
    return(glue('{readable} r '))
  } else if (writable) {
    return(glue('{writable} w'))
  } else {
    return(NA)
  }
}

study_data_variable_names <- input %>%
  select(study_data) %>%
  union(output %>%
          select(study_data)) %>%
  distinct(study_data) %>%
  arrange(study_data)

rw_data <- input %>%
  mutate(type='Read') %>%
  union(output %>%
          mutate(type='Write'))

rw_pivot <- pivottabler::PivotTable$new()
rw_pivot$addData(rw_data)
rw_pivot$addColumnDataGroups("component_name", totalCaption = 'Readers/Writers')
rw_pivot$addRowDataGroups("study_data", fromData = FALSE, explicitListOfValues = as.list(study_data_variable_names$study_data), totalCaption = 'Readers/Writers')
rw_pivot$defineCalculation(calculationName="TotalInputs", summariseExpression="rw_display(type)")
rw_pivot$renderPivot()
