extensions [matrix csv profiler]
__includes ["setup-procedures.nls" "socio-political.nls" "socio-institutional.nls" "risk.nls" "update-reporters.nls"]
globals [              ;;DEFINE GLOBAL VARIABLES
  real_rain            ;; real annual rainfall

  R                    ;; climatic risk (rainfall transformed into a normalized [0-1] scale)

  rain_max_obs         ;;max rainfall observed

  Prob_H_F             ;;probability of hazarous event flooding
  Prob_H_S             ;;probability of hazarous event scarcity

;##################################
;;;;;;weights of criterions for goverment decisions
;##################################
  w1
  w2
  w3
  w4
  w5
  w6
  w7
  w8


  C1max                  ;;max Demand for F recorded
  C2max                  ;;max Social pressure for F recorded
  C3max                  ;;max Age infra F recorded
  C4max                  ;;max Need for F recorded

  C5max                  ;;max Demand for S recorded
  C6max                  ;;max Social pressure for S recorded
  C7max                  ;;max age infra S recorded
  C8max                  ;;max Need for S recorded

  C1min                  ;;min Demand for F recorded
  C2min                  ;;min Social pressure for F recorded
  C3min                  ;;min Age infra F recorded
  C4min                  ;;min Need for F recorded

  C5min                  ;;min Demand for S recorded
  C6min                  ;;min Social pressure for S recorded
  C7min                  ;;min age infra S recorded
  C8min                  ;;min Need for S recorded

  matrix_F               ;; netlogo objects to save weighted matrices
  matrix_S
;##################################
;;Reporters
;##################################
  Var_list                ;;To report vulnerability at the final 100
  max_damage              ;;max damage in the city. It helps to calculate the tolerance threshold tau in the vulnerability procedure by comparing the state of the agents with respect to to wealth {W] and the average damage (mean_damage)

  lorenz-points_V         ;;variable to calculate lorenz curve for innequality (need to think what does it mean)
  lorenz-points_S
  lorenz-points_F
  gini_V                  ;;gini coefitient of the vulnerability
  gini_S
  gini_F
  max_v                   ;;auxiliar variable to calculate neighborhood with the maximal level of vulnerability
  counter                 ;;count in a for-loop
  max_protest_F           ;;maximum number of protest recorded in a year
  max_protest_S

  ExposureIndex           ;average number of events a person suffer in the city
  ExposureIndex_S         ;average level of scarcity of a person in the city
  ExposureIndex_F         ;average number of flooding a person suffer in the city

  InequalityExposureIndex          ;distribution of exposure to both events
  InequalityExposureIndex_S
  InequalityExposureIndex_F


  StateinfraQuantityIndex_S       ;report the number of patches with functioning infrastructure
  StateinfraQuantityIndex_F       ;report the number of patches with functioning infrastructure

  StateinfraAgeIndex_S    ;report the mean age of infrastructure S
  StateinfraAgeIndex_F    ;report the mean age of infrastructure F

  socialpressureIndex_S   ;report the sum of protest in the city
  socialpressureIndex_F   ;report the sum of protest in the city
  invest_here_max_F
  invest_here_max_S

  distance_metric_maintenanceIndex_F   ;;Metric for define distance from ideal point (MDCA)
  distance_metric_NewIndex_F           ;;Metric for define distance from ideal point (MDCA)
  distance_metric_maintenanceIndex_S   ;;Metric for define distance from ideal point (MDCA)
  distance_metric_NewIndex_S           ;;Metric for define distance from ideal point (MDCA)

  ts_F_protests     ;lists to save time series
  ts_S_protests
  ts_F_Infra_coverage
  ts_S_Infra_coverage
]




;######################################################################
;define patches and patch variables
;Patches can be seen as the minimal geostatitical unit at which goverment collect information to decide where to invest.
;######################################################################
patches-own[

  neighborhood_here?            ;;ture if the patch contains a neighborhood there
  Infra_flood               ;;IS= 1 If the patch has an infrastructure piece; IS = 0 if not
  Infra_supply              ;;IS= 1 If the patch has an infrastructure piece; IS = 0 if not

  c_F                     ;;Probability of failure of infrastructure-here (P_failure =1 if not infra here; P_failure =0 if infrastructure is new)
  c_S                     ;;Probability of failure of infrastructure-here (P_failure =1 if not infra here; P_failure =0 if infrastructure is new)
  infra_F_age                     ;;age infrastructure Flooding
  infra_S_age                     ;;age infrastructure supply

  A                               ;;altitute

  C1                      ;;criteria use to measure # people benefitiated if investment is made
  C2                      ;;criteria use by gov to measure social pressure due to flooding
  C3                      ;;variable that messure the age of infra for flooding
  C4                      ;;criteria to messure the need for infra based on # people without infrastructure for flooding

  C5                      ;;criteria use to measure # people benefitiated if investment is made
  C6                      ;;criteria use by gov to measure social pressure due to supply
  C7                      ;;variable that messure the age of infra for supply
  C8                      ;;criteria to messure the need for infra based on # people without infrastructure for supply


  distance_metric_maintenance_F   ;;Metric for define distance from ideal point (MDCA)
  distance_metric_New_F           ;;Metric for define distance from ideal point (MDCA)
  distance_metric_maintenance_S   ;;Metric for define distance from ideal point (MDCA)
  distance_metric_New_S           ;;Metric for define distance from ideal point (MDCA)


  prot_F                      ;0 1
  prot_S                      ;0 1
  protestas_here_F            ;;Social pressure (protests) accumulated over time in a neighborhood due to flooding
  protestas_here_S            ;;Social pressure (protests) accumulated over time in a neighborhood due to scarcity



  H_F                       ;;1 if a hazard event occur 0 otherwise
  H_S                       ;;1 if a hazard event occur 0 otherwise
  V                         ;; Vulnerability==Sensitivity * EE
  exposure_F                ;; exposure to harmful events (F Flooding or S scarcity). It is a "moving average" of events occuring over time
  exposure_S
  total_exposure_F
  total_exposure_S
  socialpressureTOTAL_S ;report the sum of protest in the city
  socialpressureTOTAL_F ;report the sum of protest in the city
  invest_here_F
  invest_here_S
]

;#####################################################################
;#####################################################################
;SETUP
;#####################################################################
;#####################################################################

to setup
;  random-seed semilla-aleatoria

  clear-all ;;clean plots and global variables

;;set global
  set Var_list []
  set lorenz-points_V []
  load_fixed_landscape
  ;create-Landscape         ;;define landscape topography (Altitute)
  Create-neighborhoods-Infra      ;;define the properties of the infrastructure and the neighborhoods
  ;read_weightsfrom_matrix

if-else designed-scenarios = TRUE [
    if policy_scenario = "Expand Access"[set w1 0.1 set w2 0.1 set w3 0.1 set w4 0.7 set w5 0.1 set w6 0.1 set w7 0.1 set w8 0.7]
    if policy_scenario = "Repair First"[set w1 0.1 set w2 0.1 set w3 0.7 set w4 0.1 set w5 0.1 set w6 0.1 set w7 0.7 set w8 0.1]
    if policy_scenario = "Squeaky Wheel"[set w1 0.1 set w2 0.7 set w3 0.1 set w4 0.1 set w5 0.1 set w6 0.7 set w7 0.1 set w8 0.1]
  ]
  [
   read_weights_from_csv
  ]
  set ExposureIndex 0
  set ExposureIndex_S 0
  set ExposureIndex_F 0
  set StateinfraQuantityIndex_S 0
  set StateinfraQuantityIndex_F 0
  set InequalityExposureIndex 0
  set InequalityExposureIndex_S 0
  set InequalityExposureIndex_F 0
  set socialpressureIndex_S 0
  set socialpressureIndex_F 0
  set rain_max_obs (max_rain_recorded p_rain) ;;set max rainfall observed
  ask patches [set V 0.1 Landscape-Visualization]
  set invest_here_max_F 0.1
  set invest_here_max_S 0.1
  set ts_F_protests []
  set ts_S_protests []
  set ts_F_Infra_coverage []
  set ts_S_Infra_coverage []

  reset-ticks
end

;#################################################################################
;#################################################################################
;;GO
;#################################################################################
;#################################################################################

to GO
;  profiler:start  ;;to check the computational time needed per procedude
  tick
  Update-Globals-Reporters ;; to update global and reporters
  To-Rain                   ;;set the magnitude of the climatic event and the risk factor
  ask patches [
    set_criteria_values
    Hazard                 ;; To define if a neighborhood suffer a hazard (H=1), or not (H=0), in a year
    vulnerability
    Protest
    Landscape-Visualization
  ]

  WA-Decisions ;; Water government authority decides in what (new vs. maitainance flooding vs. scarcity) and where (in what neighborhoods) to invest resources (budget)
  Update-Infrastructure ;update state of infrastructure (age, prob. of failure)
 ; time_series
; for experiments with different mental model


;if ticks = 100 [export_patches_atributes]
;if ticks = 300 [export_patches_atributes]
;if ticks = 600 [export_patches_atributes]
;if policy_scenario = "Expand Access"[set w1 0.1 set w2 0.1 set w3 0.1 set w4 0.7 set w5 0.1 set w6 0.1 set w7 0.1 set w8 0.7]
;if policy_scenario = "Repair First"[set w1 0.1 set w2 0.1 set w3 0.7 set w4 0.1 set w5 0.1 set w6 0.1 set w7 0.7 set w8 0.1]
;if policy_scenario = "Squeaky Wheel"[set w1 0.1 set w2 0.7 set w3 0.1 set w4 0.1 set w5 0.1 set w6 0.7 set w7 0.1 set w8 0.1]

;  profiler:stop          ;; stop profiling
;  print profiler:report
;  profiler:reset         ;; clear the data

end



;#################################################################################
;; Visualization
;#################################################################################

to Landscape-Visualization                                                                                                             ;;TO REPRESENT DIFFERENT INFORMATION IN THE LANDSCAPE
  if Visualization = "Elevation" [set pcolor scale-color grey  A 0  1]      ;;probability of Infrastructure failure
  if Visualization = "Infrastructure_F" [set pcolor ifelse-value (Infra_flood = 1)[scale-color grey  infra_F_age tau_ageInfra 0][65]]      ;;probability of Infrastructure failure
  if Visualization = "Infrastructure_S" [set pcolor ifelse-value (Infra_supply = 1)[scale-color grey  infra_S_age tau_ageInfra 0][65]]     ;;probability of Infrastructure failure
  if Visualization = "Vulnerability" [set pcolor ifelse-value (neighborhood_here? = TRUE) [scale-color blue V 0 max_v][65]]                ;;visualize vulnerability
  if visualization = "Social Pressure_F" [set pcolor ifelse-value (neighborhood_here? = TRUE) [scale-color red   protestas_here_F  0 10][black]];;visualized social pressure
  if visualization = "Social Pressure_S" [set pcolor ifelse-value (neighborhood_here? = TRUE) [scale-color red   protestas_here_S  0 10][black]];;visualized social pressure
  if visualization = "Spatial priorities maintanance F" [set pcolor scale-color magenta  distance_metric_maintenance_F 0 1]              ;;priorities
  if visualization = "Spatial priorities maintanance S" [set pcolor scale-color sky  distance_metric_maintenance_S 0 1]              ;;priorities

  if visualization ="Spatial priorities new F" [set pcolor scale-color magenta distance_metric_New_F 0 1]                               ;;priorities
  if visualization ="Spatial priorities new S" [set pcolor scale-color sky distance_metric_New_S 0 1]                               ;;priorities

  if visualization = "neighborhoods" [set pcolor ifelse-value (neighborhood_here? = TRUE) [magenta][65]]                                       ;;visualize if neighborhood are present in the landscape (green color if not)
  if visualization ="Harmful Events" [                                                                                                 ;; here the harmful events red color when both events( flloding and scarcity occur)
    ifelse neighborhood_here? = false [set pcolor 65]
    [
     set pcolor (ifelse-value (H_S > 0.3)[1][0]) * 35 + H_F * 85 - 105 * (H_F * (ifelse-value (H_S > 0.3)[1][0]))
    ]
  ]
end





;#################################################################################
;; Extras
;#################################################################################

;##############################################################################################################################
to export_view  ;;export snapshots of the landscape
export_patches_atributes
end

;###############################################################
to clear-2Dview
  ask patches[Landscape-Visualization]
end

;###############################################################
to-report  log-normal [#mu #sigma]
  let beta ln (1 + ((#sigma ^ 2) / (#mu ^ 2)))
  let x exp (random-normal (ln (#mu) - (beta / 2)) sqrt beta)
  report x
end
;###############################################################
to-report max_rain_recorded [p_rain_pro]

let ff 0
let hh_rain []
while [ff < 10000] [
  set hh_rain lput (log-normal 1 p_rain_pro) hh_rain
  set ff ff + 1
]
report (max hh_rain)
end


to read_weightsfrom_matrix


  ;let matrix_F csv:from-file  "c:/Users/abaezaca/Documents/MEGADAPT/ABM-empirical-V1/Mental-Models/OCVAM_Version_sin_GEO.limit.csv"

  set matrix_F matrix:from-row-list [                                                                                               ;read supermatrix

[0  0  0  0  0  0  0  0  0.09  0.0125  0.09  0.075]
[0  0  0  0  0.81  0.1125  0.81  0.675  0  0  0  0]
[0  0  0  0  0  0  0  0  0.01  0.0875  0.01  0.025]
[0  0  0  0  0.09  0.7875  0.09  0.225  0  0  0  0]
[0  0  0  0.258285  0  0.051456  0  0.044343  0  0  0  0]
[0  0.051328  0  0.636986  0.016667  0  0.01  0.01692  0  0  0  0]
[0  0.582022  0  0  0  0.00975  0  0.038737  0  0  0  0]
[0  0.366651  0  0.104729  0.083333  0.038795  0.09  0  0  0  0  0]
[0  0  0.139648  0  0  0  0  0  0  0.4631  0  0.573287]
[0.366651  0  0.527836  0  0  0  0  0  0.225  0  0.15  0.232456]
[0.051328  0  0  0  0  0  0  0  0  0.087748  0  0.094256]
[0.582022  0  0.332516  0  0  0  0  0  0.675  0.349153  0.75  0]
  ]


set matrix_S matrix:from-row-list [
[  0  0  0  0  0  0  0  0  0.81  0.1125  0.81  0.675]
[  0  0  0  0  0.09  0.0125  0.09  0.075  0  0  0  0]
[  0  0  0  0  0  0  0  0  0.09  0.7875  0.09  0.225]
[  0  0  0  0  0.01  0.0875  0.01  0.025  0  0  0  0]
[  0  0  0  0.258285  0  0.4631  0  0.399086  0  0  0  0]
[ 0  0.051328  0  0.636986  0.15  0  0.09  0.15228  0  0  0  0]
[  0  0.582022  0  0  0  0.087748  0  0.348634  0  0  0  0]
[  0  0.366651  0  0.104729  0.75  0.349153  0.81  0  0  0  0  0]
[  0  0  0.139648  0  0  0  0  0  0  0.051456  0  0.063699]
[  0.366651  0  0.527836  0  0  0  0  0  0.025  0  0.016667  0.025828]
[  0.051328  0  0  0  0  0  0  0  0  0.00975  0  0.010473]
[  0.582022  0  0.332516  0  0  0  0  0  0.075  0.038795  0.083333  0]
]


 let matrix_B (matrix:times matrix_S matrix_S matrix_S matrix_S matrix_S matrix_S matrix_S matrix_S matrix_S matrix_S matrix_S)  ;generate new limit matrix


;  set alpha1 item 10 sort (matrix:get-row matrix_B 0)
;  set alpha2 item 10 sort (matrix:get-row matrix_B 1)
;  set alpha3 item 10 sort (matrix:get-row matrix_B 2)
;  set alpha4 item 10 sort (matrix:get-row matrix_B 3)

 ; let alpha_tot alpha1 + alpha2 + alpha3 + alpha4
  ;set alpha1 alpha1 / alpha_tot
  ;set alpha2 alpha2 / alpha_tot
  ;set alpha3 alpha3 / alpha_tot
  ;set alpha4 alpha4 / alpha_tot


  set w1 item 10 sort (matrix:get-row matrix_B 8)                                                                    ;assige weights from the rows of the limit matrix
  set w2 item 10 sort (matrix:get-row matrix_B 11)
  set w3 item 10 sort (matrix:get-row matrix_B 9)

  set w4 item 10 sort (matrix:get-row matrix_B 10)
  set w5 item 10 sort (matrix:get-row matrix_B 4)
  set w6 item 10 sort (matrix:get-row matrix_B 7)
  set w7 item 10 sort (matrix:get-row matrix_B 5)
  ;set w8 item 10 sort (matrix:get-row matrix_B 6)



  let tot_weights (w1 + w2 + w3 + w4 + w5 + w6 + w7)

  set w1  w1 / tot_weights
  set w2  w2 / tot_weights
  set w3  w3 / tot_weights
  set w4  w4 / tot_weights
  set w5  w5 / tot_weights
  set w6  w6 / tot_weights
  set w7  w7 / tot_weights
  ;set w8  w8 / tot_weights


end

;to update_weights                                   ;generate a change in the supermatrix due to a change in pair comparisong of criterias with respect to action "nueva_F"
;
;
; ;matrix:set matrix_F 9 0 0.814212784               ;change values of weights of cluster F (inundaciones) with respect to create new_F
;; matrix:set matrix_F 10 0 0.113982647
; ;matrix:set matrix_F 11 0 0.113982647
;
;  let matrix_B (matrix:times matrix_F matrix_F matrix_F matrix_F matrix_F matrix_F matrix_F matrix_F matrix_F matrix_F matrix_F)    ;calculate limit matrix
;
; ;set alpha1 item 10 sort (matrix:get-row matrix_B 0)
; ;set alpha2 item 10 sort (matrix:get-row matrix_B 1)
; ;set alpha3 item 10 sort (matrix:get-row matrix_B 2)
; ;set alpha4 item 10 sort (matrix:get-row matrix_B 3)
;
;;  let alpha_tot alpha1 + alpha2 + alpha3 + alpha4
;;  set alpha1 alpha1 / alpha_tot
;;  set alpha2 alpha2 / alpha_tot
;;  set alpha3 alpha3 / alpha_tot
;;  set alpha4 alpha4 / alpha_tot
;
;
;  set w1 item 10 sort (matrix:get-row matrix_B 8)                                                                    ;assige weights from the rows of the limit matrix
;  set w2 item 10 sort (matrix:get-row matrix_B 11)
;  set w3 item 10 sort (matrix:get-row matrix_B 9)
;
;  set w4 item 10 sort (matrix:get-row matrix_B 10)
;  set w5 item 10 sort (matrix:get-row matrix_B 4)
;  set w6 item 10 sort (matrix:get-row matrix_B 7)
;  set w7 item 10 sort (matrix:get-row matrix_B 5)
;  ;set w8 item 10 sort (matrix:get-row matrix_B 6)
;
;  let tot_weights (w1 + w2 + w3 + w4 + w5 + w6 + w7)
;
;  set w1  w1 / tot_weights
;  set w2  w2 / tot_weights
;  set w3  w3 / tot_weights
;  set w4  w4 / tot_weights
;  set w5  w5 / tot_weights
;  set w6  w6 / tot_weights
;  set w7  w7 / tot_weights
;  set w8  w8 / tot_weights
;
;
;
;end






to export_value_patches

file-open "landscape.txt"
foreach sort patches
  [ ?1 ->
    ask ?1 [
      file-write pxcor
      file-write pycor
      file-write A]                                ;write the ID of each ageb using a numeric value (update acording to Marco's Identification)
  ]
file-close                                        ;close the File

end

to export_patches_atributes
  let directory "c:/Users/abaezaca/Dropbox (ASU)/MEGADAPT/ABM_V2/landscape_pics/"
let sim_n (word reps "-" (word ticks "-" (word radius_l "-" (word tau_ageInfra  "-" (word scenario_number "-")))))
let wr word  sim_n "spatialpatterns_sensitivity.txt"
file-open word directory wr
foreach sort patches
  [ ?1 ->
    ask ?1 [
      file-write pxcor
      file-write pycor
      file-write ifelse-value (neighborhood_here? = TRUE)[1][0]
      file-write infra_flood
      file-write infra_supply
      file-write A
      file-write precision  socialpressureTOTAL_S 3
      file-write precision  socialpressureTOTAL_F 3
      file-write precision  total_exposure_S 3
      file-write precision  total_exposure_F 3
      file-write precision  infra_S_age 3
      file-write precision  infra_F_age 3
      file-write precision  distance_metric_maintenance_F 3 ;;Metric for define distance from ideal point (MDCA)
      file-write precision  distance_metric_New_F 3      ;;Metric for define distance from ideal point (MDCA)
      file-write precision  distance_metric_maintenance_S 3  ;;Metric for define distance from ideal point (MDCA)
      file-write precision  distance_metric_New_S 3         ;;Metric for define distance from ideal point (MDCA)
    ]                                ;write the ID of each ageb using a numeric value (update acording to Marco's Identification)
  ]
file-close                                        ;close the File
end


;###############################################################
;to time_series
;  set ts_F_protests lput (sum [prot_F] of patches with [neighborhood_here? = TRUE]) ts_F_protests
;  set ts_S_protests lput (sum [prot_S] of patches with [neighborhood_here? = TRUE]) ts_S_protests
;  set ts_F_Infra_coverage lput (count patches with [infra_flood = 1 and infra_F_age < tau_ageInfra]) ts_F_Infra_coverage
;  set ts_S_infra_coverage lput (count patches with [infra_supply = 1 and infra_S_age < tau_ageInfra]) ts_S_Infra_coverage
;
;  if ticks = 600 [
;    let ts_full (list ts_F_protests ts_S_protests ts_F_Infra_coverage ts_S_infra_coverage)
;    let directory "c:/Users/abaezaca/Dropbox (ASU)/MEGADAPT/ABM_V2/landscape_pics/"
;    let sim_n word directory (word ticks "-" (word radius_l "-" (word tau_ageInfra "-" (word scenario_number "-"))))
;    let wr word  sim_n "-time_series.csv"
;    csv:to-file wr ts_full
;  ]
;end

;###############################################################
;End of Code
;###############################################################
;###############################################################
@#$#@#$#@
GRAPHICS-WINDOW
280
40
834
595
-1
-1
5.46
1
10
1
1
1
0
1
1
1
0
99
0
99
1
1
1
ticks
30.0

BUTTON
44
30
107
63
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
107
30
170
63
NIL
GO
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
169
30
232
63
NIL
GO
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
540
638
755
683
Visualization
Visualization
"Elevation" "Infrastructure_F" "Infrastructure_S" "Spatial priorities maintanance F" "Spatial priorities new F" "Spatial priorities maintanance S" "Spatial priorities new S" "Vulnerability" "Social Pressure_F" "Social Pressure_S" "Districts" "Harmful Events"
2

PLOT
840
64
1040
214
Rainfall
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot R"

CHOOSER
20
226
255
271
policy_scenario
policy_scenario
"Expand Access" "Repair First" "Squeaky Wheel"
0

PLOT
1042
64
1244
214
Mean vulnerability
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -11221820 true "" "if ticks > 10 [plot mean [sum exposure_F] of patches with [neighborhood_here? = TRUE]]"
"pen-2" 1.0 0 -6459832 true "" "if ticks > 10 [plot mean [sum exposure_S] of patches with [neighborhood_here? = TRUE]]"

PLOT
840
364
1045
523
Protests
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -11221820 true "" "plot sum [prot_F] of patches with [neighborhood_here? = TRUE]"
"pen-1" 1.0 0 -8431303 true "" "plot sum [prot_S] of patches with [neighborhood_here? = TRUE]"

SLIDER
43
451
230
484
New_infra_investment
New_infra_investment
0.001
1
0.0
0.001
1
NIL
HORIZONTAL

CHOOSER
22
319
258
364
Initial-Condition-Infrastructure
Initial-Condition-Infrastructure
"New" "Old"
0

PLOT
840
212
1040
362
GINI
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot gini_V / count patches with [neighborhood_here? = TRUE]"
"pen-1" 1.0 0 -7500403 true "" "plot gini_S / count patches with [neighborhood_here? = TRUE]"
"pen-2" 1.0 0 -2674135 true "" "plot gini_F / count patches with [neighborhood_here? = TRUE]"

SLIDER
43
558
233
591
p_rain
p_rain
0.25
1.5
0.5
0.05
1
NIL
HORIZONTAL

BUTTON
278
624
379
657
NIL
export_view
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
32
140
211
175
Define Scenarios
20
0.0
1

TEXTBOX
52
380
273
407
Define Parameter Values
18
0.0
1

SLIDER
43
419
228
452
maintenance
maintenance
0.001
1
0.0
0.001
1
NIL
HORIZONTAL

CHOOSER
20
183
255
228
landscape-type
landscape-type
"closed-watershed" "gradient" "many-hills"
0

TEXTBOX
590
605
778
629
Visualization
16
0.0
1

PLOT
1044
214
1244
364
Infrastructura coverage
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -8990512 true "" "plot count patches with [infra_flood = 1 and infra_F_age < 200]"
"pen-1" 1.0 0 -6459832 true "" "plot count patches with [infra_supply = 1 and infra_S_age < 200]"

INPUTBOX
70
72
188
140
semilla-aleatoria
48569.0
1
0
Number

SLIDER
43
665
230
698
scenario_number
scenario_number
0
2002
1208.0
1
1
NIL
HORIZONTAL

CHOOSER
21
272
254
317
budget-distribution
budget-distribution
"regional" "local" "local-bothactions"
1

SLIDER
43
593
232
626
motivation_to_protest
motivation_to_protest
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
44
629
232
662
tolerance_protest
tolerance_protest
0
1
0.133
0.001
1
NIL
HORIZONTAL

SLIDER
44
487
231
520
tau_ageInfra
tau_ageInfra
100
400
200.0
100
1
NIL
HORIZONTAL

SLIDER
44
522
231
555
radius_l
radius_l
0
4
0.0
1
1
NIL
HORIZONTAL

SLIDER
43
698
230
731
reps
reps
1
10
1.0
1
1
NIL
HORIZONTAL

SWITCH
1092
632
1250
665
designed-scenarios
designed-scenarios
1
1
-1000

@#$#@#$#@
# MEGADAPT PROTOTYPE ABM
## WHAT IS IT?
The model simulates the decisions of residents and the water authority in response to socio-hydrological risk.  Neighborhoods are located in a landscape with topographic complexity and two problems: water scarcity in the peripheral neighborhoods at high altitude and high risk of flooding in the lowlands, at the core of the city. The role of the water authority is to decide where investments in infrastructure should be allocated to reduce the risk to water scarcity and flooding events in the city, and these decisions are made via a multi-objective site selection procedure. This procedure accounts for the interdependencies and feedback between the urban landscape and a policy scenario that defines the importance, or priorities, that the authority places on four criteria.
Neighborhoods respond to the water authority decisions by protesting against the lack of investment and the level of exposure to water scarcity and flooding (figure 2). Protests thus simulate a form of feedback between local-level outcomes (flooding and water scarcity) and higher-level decision-making. Neighborhoods are located in a landscape with spatially correlated topographic variability, that is, a landscape with hills and valleys, where neighborhoods at high altitude are more likely to be exposed to water scarcity and lack infrastructure, whereas neighborhoods in the lowlands tend to suffer from recurrent flooding. The frequency of flooding is also a function of spatially uniform rainfall events. Likewise, neighborhoods at the periphery of the urban landscape lack infrastructure and suffer from chronic risk of water scarcity.


The goal of the project is to understand how the decisions of dominant actors of the Mexico City water governance system influence the dynamics of socio-hydrological vulnerability. The objective of this hypothetical model (https://github.com/sostenibilidad-unam/abm2) is to understand how the vulnerability patterns of an urban environment subjected to risk and exposure associated with water are influenced by the decision-making process of a central authority that manages water-related infrastructure. We focus on the feedback that emerges between the decisions of the water authority and a socio-political factor defined as resident protests, which is driven by their exposure to flooding and scarcity.
## HOW IT WORKS
Every time-step (tick) a hazardous event may occur with probability, independently for each district. Hazardous events when do occur they produce a damage proportional to the magnitude of the event.

The Government evaluates each district and the state of the infrastructure in which the district is located to decide where to invest the economic resources constrained by a budget. It does so by computing a distance metric between the actual state of the system evaluated for a set of indicators or criterion and the ideal state from the government perspective. The current version includes three criterion for the two actions:
1) The state of infrastructure, using its age as proxy for failure
2) The state of the social satisfaction, using the number of protest registered in the area surrounding a patch, and
3) The economic efficiency of each investment, using the number of neighborhoods that would beneficiate per dollar invested as indicator of efficiency.

Each action (To maintain or create new one) is associated with a criteria by a weight parameter that define the importance or level of prioritization of this factor in the decision. The set of weight parameters that linked each criteria to actions (invest in new infrastructure or maintain new pieces) define a "decision strategy".
A strategy to control social pressure for example, would weight more heavily on reducing the number of protests, for example that efficiency distribute dollar by only the age of the infrastructure.

Every time-step, districts will allocate time to undertake different actions to reduce damage to produce income and to have leisure time. The proportion of time that each n invests in these actions indicates its intensity in the landscape.
Each neighborhood will carried out a particular strategy. This strategy consists of dividing the time devoted to four actions: protesting LP, modifying the local environment to seek adaptation LA, spend time in social organizations LO and finally do nothing. Each district will evaluate the “optimal” strategy to use every time step by either maximizing benefits of each action (cost-benefit scenario) or learning from other district’s experience (social learning scenarios).


## HOW TO USE IT

The interface is composed of a set of “choosers” to define scenarios and “sliders” to set parameter values. It also includes a set of plots to show the dynamics of the different socio economic indices and the state of the infrastructure and the damage and vulnerability of the city.

Within the 2D view, a chooser called “Visualization” controls the type of information contained in each patch in the landscape that will be displayed along with the button “clear-2d-view”.
After defining scenarios and parameter values press the “Setup” button to create the landscape, the districts and the initial state of the infrastructure. Then press the “GO” button to simulate hazardous events and the response of districts and government.



### Policy scenario

**Expand Access**: government place more emphasis on district that generate more benefits to people determine by the number of districts beneficiated by dollar invested.

**Repair First**: government is more interested in allocated more resources with aged infrastructure or areas that lacks of infrastructure provision.

**Squeaky Wheel**: In this scenarios government is more interested in reducing the social pressure or the number of protests occur in each district.


### Initial Conditions

Two initial conditions regarding the age of the infrastructure:

**New**: this represent an scenario with an ideal situation, where infrastructure is new (Age = 1 decision cycles)

**Old**: This is a scenario with aged systems (Initial age = 10 decision cycles)


### Parameters

**Maintenance**: To set the budget for maintenance by defining % of neighborhoods can be attained per time-step

**New_infra_investment**: to set the budget for new infrastructure by defining a % of total resources needed to provide infrastructure to every neighborhood


**p_rain**: The mean of a log-normal distribution that will generate a random realization of a climatic events

**tau_ageInfra**: Age at which the water authority considers a system "old". It is a cut-off value for decisions in the value functions

**radius_l**: Defines the connectivity of the system that influence the condition for hazards.

**motivation_to_protest**: It defines the importance agents give to exposure relative to acction by the authority

**tolerance_protest**: Parameter that set the tolerance of the population to exposure

**scenario_number**: to define an scenarioa from the 2000 options from file "sampling_scenarios_var_W.csv"


### Visualization

**Topography**: Visualized the watershed by coloring based on altitude where lighter grey to white represent higher places

**Infrastructure**: Visualize the age of the infrastructure in the landscape using a grey scale. Areas with newer infrastructure show lighter tones of grey.

**Vulnerability**: Shows the value of the vulnerability index in a blue scale. Lighter tones indicate higher vulnerability

**Social Pressure**: Display the time districts devote to protesting. Lighter tones of red represent higher time protesting.


## THINGS TO NOTICE

(suggested things for the user to notice while running the model)


## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

### Wealth Distribution model
Wilensky, U. (1998). NetLogo Wealth Distribution model. http://ccl.northwestern.edu/netlogo/models/WealthDistribution. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="timeseries" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="600"/>
    <enumeratedValueSet variable="budget-distribution">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivation_to_protest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_rain">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="New_infra_investment">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Condition-Infrastructure">
      <value value="&quot;Old&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_protest">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation_number">
      <value value="2000"/>
      <value value="2001"/>
      <value value="2002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius_l">
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau_ageInfra">
      <value value="300"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="spatial_pattern" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export_view</final>
    <timeLimit steps="600"/>
    <enumeratedValueSet variable="budget-distribution">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="semilla-aleatoria">
      <value value="48569"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivation_to_protest">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_rain">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="New_infra_investment">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Condition-Infrastructure">
      <value value="&quot;Old&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Visualization">
      <value value="&quot;Infrastructure_S&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_protest">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GOVERNMENT_DECISION_MAKING">
      <value value="&quot;Increase Infra Coverage&quot;"/>
      <value value="&quot;Reduce age infrastructure&quot;"/>
      <value value="&quot;Reduce Social Pressure&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape-type">
      <value value="&quot;closed-watershed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivation_to_protest">
      <value value="0.006"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sensitivity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="600"/>
    <enumeratedValueSet variable="budget-distribution">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivation_to_protest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_rain">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="New_infra_investment">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Condition-Infrastructure">
      <value value="&quot;Old&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_protest">
      <value value="0.001"/>
    </enumeratedValueSet>
    <steppedValueSet variable="simulation_number" first="1953" step="1" last="2002"/>
    <enumeratedValueSet variable="radius_l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau_ageInfra">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reps">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_example" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="600"/>
    <metric>StateinfraQuantityIndex_S</metric>
    <metric>StateinfraQuantityIndex_F</metric>
    <metric>StateinfraAgeIndex_S</metric>
    <metric>StateinfraAgeIndex_F</metric>
    <metric>socialpressureIndex_S</metric>
    <metric>socialpressureIndex_F</metric>
    <enumeratedValueSet variable="motivation_to_protest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="New_infra_investment">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius_l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="budget-distribution">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_rain">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Condition-Infrastructure">
      <value value="&quot;Old&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reps">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance">
      <value value="0.069"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance_protest">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape-type">
      <value value="&quot;closed-watershed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario_number">
      <value value="1208"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;Squeaky Wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="semilla-aleatoria">
      <value value="48569"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau_ageInfra">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Visualization">
      <value value="&quot;Infrastructure_S&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="designed-scenarios">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Comparing" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>GO</go>
    <timeLimit steps="300"/>
    <metric>count turtles</metric>
    <metric>count patches with [infra_flood = 1 and infra_F_age &lt; 200]</metric>
    <enumeratedValueSet variable="motivation_to_protest">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="New_infra_investment">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius_l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="budget-distribution">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_rain">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Condition-Infrastructure">
      <value value="&quot;New&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reps">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance_protest">
      <value value="0.133"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape-type">
      <value value="&quot;closed-watershed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario_number">
      <value value="1208"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;Expand Access&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="semilla-aleatoria">
      <value value="48569"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau_ageInfra">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Visualization">
      <value value="&quot;Infrastructure_S&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="designed-scenarios">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="investment_on_vuln" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="600"/>
    <metric>count turtles</metric>
    <metric>mean [V] of patches with [neighborhood_here?]</metric>
    <enumeratedValueSet variable="motivation_to_protest">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius_l">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="New_infra_investment" first="0" step="0.2" last="1"/>
    <enumeratedValueSet variable="budget-distribution">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_rain">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Condition-Infrastructure">
      <value value="&quot;New&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reps">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance_protest">
      <value value="0.133"/>
    </enumeratedValueSet>
    <steppedValueSet variable="maintenance" first="0" step="0.2" last="1"/>
    <enumeratedValueSet variable="landscape-type">
      <value value="&quot;closed-watershed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario_number">
      <value value="1208"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;Expand Access&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="semilla-aleatoria">
      <value value="48569"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau_ageInfra">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="designed-scenarios">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Visualization">
      <value value="&quot;Infrastructure_S&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
