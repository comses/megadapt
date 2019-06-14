---
output:
  html_document: default
  pdf_document: default
---
# Metadata for megadapt_wgs84 
This Shape file is use to operationalize the MEGADAPT model for Mexico City

## Path 
https://github.com/comses/megadapt/tree/master/src/r/megadaptr/inst/rawdata/censusblocks

## Contact

Laboratorio Nacional de Ciencias de la Sostenibilidad
School of Sustainability, Arizona State University

## Coordinate system 
EPSG:WGS 84 / UTM zone 14N (EPSG:32614)


## Extensión geográfica 
Norte = 19.5927572802
Sur = 19.1240921744
Este = -98.9466436802
Oeste = -99.3430557324

## Number of polygons (Census blocks)
2428


### Number of fields
36

### List of fields 

<div class="foo">

Name model | Field | Type |               Description               | Range | Units
-----|-----| -----|----------------------------------------------- | ----- | --- | 
censusblock_id |ageb_id | Integer |  | 5959 - 8390 | 
geographic_id|cvgeo | String | An universal identifier of each census block that includes the municipality, and the state  | NA |
Area |area | Real |  | 10876.65738 - 7928687.466 | mts squared |
resident_potable_water_lacking_count |abastecimi | Real | Represent the population of each census block that does not have access to piped water in their houses | 0.0 - 1.0 | index |
potable_system_pressure|pres_hid | Real | Estimation of the pressure of water in pipes as a function of altitude and distance to the Cutzamala system | 0.0 - 0.940521064592 | index |
sewer/potable water_infrastructure_age|antiguedad | Integer64 | Proxy for the age of infrastructure based on the sequence of urbanization periods in CDMX | 4 - 64 | years |
garbage_index| basura | Real |  The reports of ponding associated with obstruction of drainage for the year 2016 | 0.0 - 19.48415 | |
bombeo_tot |bombeo_tot | Integer64 | Number of pumps to direct stormwater in the sewer system | 0 - 3 | number of pomps |
waterquality_index|cal_agua | Real | Reports about the quality of the water per census block | 0.714 - 1.0 | index |
urbangrowth |crec_urb | Real | proportion of urbanized area in a census block | -0.0229128 - 0.0477761 | proportion |
critic_zones |critic_z | Real | Areas of the city SACMEX considers critical because of the difficulties of providing potable water | 0.0 - 1.0 | boolean |
resident_potable_water_waste_perception|desp_agua | Real | Perception of residents about exportation potable water | 0.0 - 100.0 | index |
resident_potable_water_exportation_perception|desv_agua | Real | Perception of residents about net potable water exports | 0.0 - 1.0 | index |
resident_reports_ponding_count_mean|prom_en | Real |Frequency of ponding events reported by residents in a year  | 0.0 - 6.30308292051 | Events per year |
resident_reports_potable_water_failure_count|fail_claim | Real | Number of reports of failures in the distribution potable water | 0.0 - 348.395163286 | number of reports |
household_potable_system_lacking_percent|falla_dist | Real | Number of reports collected by sacmex about lickages in the system and failures in the distribution potable water  | 0.0 - 387.38628 | number of reports| 
falla_dren | Real | Number of reports collected by sacmex about failure in the sewer system | 0.0 - 9.24135217868 | index |
household_potable_system_lacking_percent|falta_dist | Real | Proportion of households without connection to potable water distribution infrastructure | 0.0 - 1.0 | proportion of houses |
household_sewer_system_lacking_percent|falta_dren | Real |Proportion of households without connection to the sewer system  | 0.0 - 1.0 |  proportion of houses |
resident_diarrhea_per_capita|enf_14 | Real | Number of cases of Diarrheal diseases per 10000 individual in the year 2014 | 0.0 - 51.0 |  cases / 1000 people |
resident_income_per_capita |income_pc | Real | Income per capita in dollars (by municipality)  | 2412.06268436 - 8461.77738583 | dollars |
resident_reports_flooding_count_mean|inunda | Real | Frequency of flooding event in a year | 0.0 - 8.0266 | frequency of events |
runoff_bin|runoff_bin | Integer64 | runoff per census block in volume | 0 - 1 | |
household_days_no_potable_water_per_week_mean |lambdas | Integer | average number of days in a week a census block is without water. Data from INEGI | 0 - 7 | days in a week |
segments |n_tramos | Integer |Number of segments of the sewer system in each census block   | 0 - 4428 | number of segments |
delegation_social_pressure|pet_del_dr | Real |proxy for the attention government give to different delegations  | 1.0 - 1.0 | index |
resident_count |poblacion | Integer | Total population per census block | 0 - 17152 | people |
tc_pob|tc_pob | Real | rate of population change per census block | -0.0229128 - 0.0477761 | rate |
media_social_pressure|pres_med | Integer | proxy for media report about water scarcity or flooding events  | 0 - 5900 | index |
sewer_system_capacity_max |q100 | Real | The maximal capacity of the sewer system per census block| 0.0 - 2064.34 | mts. |
sewer_system_storm_drain_count|rejillas | Integer64 | Number of drainage per census block  | 0 - 293 | number of drainage |
subsidence_rate_per_year|subsidenci | Real | cm. per year of subsidence | 0.0 - 35.85 | cm/year |
household_water_storage_tank_percent |tanks | Real | water storage tanks per house  | 0.119729543559 - 1.28413210019 | taks/house |
resident_reports_potable_water_failure_count_per_area| wo_water | Real |  resident reports of days in a year without water normalized by area | 0.0 - 166.852558916 | reports | 



