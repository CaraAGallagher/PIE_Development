; Energy Budget with Individual Variation model version 0.1

; Model Developed by:
; Cara A. Gallagher
; Postdoctoral researcher
; University of Potsdam
; Developed as part of the BioMove project (biomove.org)

; Development started: Dec 15th, 2021
; The model was developed and tested using NetLogo version 6.2.0.

; debug levels:
;   0   No debugging
;   1   Profiler
;   2   Energy intake
;   3   Basal metabolic costs
;   4   Cost of transport
;   5   Pregnancy
;   6   Lactation
;   7   Growth and lean mass deposition
;   8   Storage
;   9   FMR
;   10  Mortality

extensions [ profiler csv ]

globals [

  day
  year
  year-start
  year-end
  ticks-per-day             ; Number of timesteps per day

  max-resources             ; Maximum resources in a resource cell [g]
  cell-size-m               ; Cell size [m]
  resource-cells            ; Patchset of cells containing resources
  dens-cells                ; Patchset of cells to evaluate population density

  ; Energy intake
  stomach-fill-perc         ; Stomach fill as percent body mass [%]
  AE-food	                  ; Assimilation efficiency of food [%/100]
  HIF                       ; Heat increment of feeding [%]
  DM-food                   ; Dry matter content of foodstuffs [%]
  ED-food	                  ; Energy density of foodstuffs [J g-1]

  ; Basal metabolic rate parameters
  B0                        ; Normalization constant [J 30min-1]
  gamma                     ; Allometric scaling exponent [J kg 30min-1]

  ; Cost of transport parameters
  speed-mean                ; Mean movement velocity [m s-1]
  speed-max                 ; Maximum movement velocity [m s-1]
  HR-r-min                  ; Minimum home range radius [m]
  HR-r-max                  ; Maximum home range radius [m]
  intercept-pcot            ; Intercept of the postural cost function [J 30min-1]
  slope-pcot                ; Slope of the postural cost function [J kg-1 30min-1]
  intercept-icot            ; Intercept of the incremental cost of transport function [J 30min-1]
  slope-icot                ; Slope of the incremental cost of transport function [J kg-1 30min-1]

  ; Reproduction
  ; Pregnacy
  t-mating                  ; Mating days [day of year]
  t-mature                  ; Minimum age of conception [days]
  t-gest                    ; Gestation time [days]
  t-0	                      ; Implantation delay for lactating females [days]
  n-emb-range               ; Range of number of conceived embryos [N]
  emb-growth-c              ; Fetal growth constant [day-1]
  emb-mass-init             ; Fetal mass at conception [g]
  emb-mass-inf              ; Asymptotic fetal mass [g]
  percent-fat-emb           ; Fetal body composition - Fat [%]
  percent-pro-emb           ; Fetal body composition - Protein [%]
  ED-pl                     ; Energy density of placental tissue [J g-1]
  DE-pl                     ; Deposition efficiency of placental tissue [%]

  ; Lactation
  t-nurs                    ; Nursing time [days]
  off-BMR-red               ; Relative offspring basal costs multiplier [%]
  off-growth-eff            ; Efficiency of offspring growth [%]
  milk-prod-eff             ; Efficiency of producing milk from body stores [%]

  repro-outs-list           ; List of average yearly abortion and offspring mortality rates for all reproducing animals [%]

  ; Growth and lean mass deposition
  ED-pro	                  ; Protein energy density [J kg-1]
  DE-fat	                  ; Deposition efficiency of fat [%]
  DE-pro	                  ; Deposition efficiency of protein [%]
  growth-lm-inf	            ; Maximum lean mass [kg]
  growth-lm-k	              ; Lean mass growth constant [day-1]
  growth-lm-No              ; Lean mass initial value [kg]

  ; Storage
  ED-fat	                  ; Fat energy density [J kg-1]
  ED-cpro                   ; Energy density of catabolized protein [J kg-1]
  gamma-mobilize            ; Fuel partitioning constant [Unitless]
  perc-water-adi            ; Water percent in adipose tissue [%]
  SL-max                    ; Maximum adipose storage level [%]
  SL-list                   ; List of storage level values [%]
  SL-at-death-list          ; List of storage level values when animals died [%]

  ; Mortality
  t-max-age                 ; Maximum age [days]
  age-at-death-list         ; List of ages where animals died [days]
  age-at-death-temp-list
  winter-surv               ; Overwinter survival probability [%]

  pop-list
  off-list
  dens-list                 ; List of abundance on subset of cells (1 hectare in total area) to assess population density

  ; Calibration outputs
  p1-FMAB                   ; Pattern 1. Fetal mass at birth
  p2-BMxLS                  ; Pattern 2. Birth mass by litter size
  p34-BMLMxA                ; Pattern 3. Total body mass by age & Pattern 4. Lean mass by age
  p567-dsbxMMFILM           ; Pattern 5. Lactating mother mass by pup age, Pattern 6. Lactating mother food intake by pup age, & Pattern 7. Total litter mass by pup age
  p8910-LSxpFIEUMEO         ; Pattern 8. Mother peak food intake by litter size, Pattern 9. Mother peak energy use by litter size, & Pattern 10. Mother peak milk transfer by litter size
  p11-PMxLS                 ; Pattern 11. Pup mass at weaning by litter size
  p12-LSb                   ; Pattern 12. Litter size at birth
  p13-LSw                   ; Pattern 13. Litter size at weaning
  p14-BF                    ; Pattern 14. Average / range of body fat %
  p15-BF                    ; Pattern 15. Body fat % of living animals
  p16-FMRxBM                ; Pattern 16. Field metabolic rate by body mass

  dens-mean                 ; Mean density value (voles per hectare)
  dens-med                  ; Median density value (voles per hectare)

  ; Evaluation outputs
  p17-FMRxState              ; Pattern 17. State-dependent field metabolic rate
  p18-IRxState               ; Pattern 18. State-dependent food consumption
  p19-surv                   ; Pattern 19. Survival rates
  p20-BMadult                ; Pattern 20. Adult body mass
  p21-age1stbirth            ; Pattern 21. Age at first birth
  p22-BMRadult               ; Pattern 22. Mass-specific basal metabolic rate
  p23-LSb                    ; Pattern 23. Litter size
  p24-LpY                    ; Pattern 24. Litters per year
  p25-BMemb                  ; Pattern 25. Neonate body mass
  p26-BMwean                 ; Pattern 26. Weaning body mass
  p27-dens                   ; Pattern 27. Population density

  ; Scenario variables
  scenario-res-levs          ; Keeps track of the timing and magnitude of next resource change

  BM-list                    ; Body mass of animals
  activity-list              ; Activity time of animals
  BC-list                    ; Body condition of animals
  LSW-list                   ; Litter size at weaning of animals - updated in wean procedure
  LSW-temp-list
  neo-BM-list                ; Neonate body mass - updated in give-birth procedure
  neo-BM-temp-list
  wean-BM-list               ; Body mass at weaning - updated in wean procedure
  wean-BM-temp-list
;  IBI-list                   ; Interbirth interval (IBI) of animals within year - updated in give-birth procedure
;  IBI-temp-list
  LRS-list                   ; Lifetime reproductive success of animals
  LRS-temp-list
  age-1st-birth-list
  age-1st-birth-temp-list
  LPY-list
  LPY-temp-list

  abun-outs-list             ; Scenario output list of abundance values updated monthly in years greater than 10
  FMR-list          ; Total metabolic rate of animals by reproductive state
  loco-list
  repro-list
  growth-list


  ; list of values collected at the end of the first trait output year
  BM-list-base
  ;activity-list-base
  BC-list-base
  LSW-list-base
  neo-BM-list-base
  wean-BM-list-base
  ;IBI-list-base
  LRS-list-base
  age-1st-birth-list-base
  age-at-death-list-base
  LPY-list-base
  FMR-list-base
  loco-list-base
  repro-list-base
  growth-list-base
]

patches-own [

  resource-level            ; Food resource level of a cell [g]
  last-eaten                ; Timestep when the food cell was last eaten from [N]

]

turtles-own [

  mass                      ; Mass [kg]
  lean-mass                 ; Mass of lean (non-adipose) tissues [kg]
  adipose-mass              ; Mass of adipose tissues [kg]
  age                       ; Age in days [days]
  storage-level             ; Storage level; adipose stores as a percentage of mass [%]

  ; Energy intake
  m-HIF                     ; Heat increment of feeding [J]
  energy-assimilated        ; Assimilated energy to use on metabolic processes [J]
  food-debt                 ; Record of unfullfilled energy needs [g]
  daily-ingestion           ; Amount of food ingested daily
  stomach-fill-max          ; Maximum stomach fill [g]
  stomach-fill              ; Current stomach fill [g]
  stomach-clear-rate        ; Stomach clearance rate [timesteps]
  foraging-cell             ; Gridcell where forgaging occured in a timestep

  ; Basal metabolic rate
  m-BMR                     ; Basal metabolic rate [J 30min-1]
  ITV-BMR                   ; Individual level of variation in BMR costs, modifies base curve value [%]

  ; Cost of transport
  pcot                      ; Postural cost of transport [J s-1]
  icot                      ; Incremental cost of transport [J s-1]
  move-speed                ; Movement speed [m 30min-1]
  m-move                    ; Total cost of transport [J 30min-1]
  daily-activity            ; Binary list of activity

  ; Reproduction
  t-mating-offset           ; Offset for variation in start of breeding season [days]
  pregnancy-status          ; Pregnancy status [true or false]
  n-emb                     ; Number of embryos conceived [N]
  ds-mating                 ; Days since mating [days]
  mass-emb                  ; Embryo mass [g]
  mass-pl                   ; Placental mass [g]
  gest-mass                 ; Total mass of embryos and placentae [kg]
  m-preg                    ; Total cost of pregnancy [J 30min-1]
  m-preg-no-prod            ; Heat expended by pregnancy (no tissue production energy) [J 30min-1]

  lactation-status          ; Lactation status [true or false]
  n-off                     ; Number of offspring conceived [N]
  ds-birth                  ; Days since giving birth [days]
  mass-off                  ; Mass of dependent offspring [kg]
  SL-off                    ; Storage level of offspring; adipose stores as a percentage of mass [%]
  lean-mass-off             ; Lean mass of dependent offspring [kg]
  ITV-BMR-off-list          ; ITV value for BMR for dependent offspring [%]
  m-BMR-off                 ; Offspring maintenance costs [J 30min-1]
  m-growth-lm-off           ; Offspring costs of growth [J 30min-1]
  m-lact                    ; Total cost of lactation [J 30min-1]
  m-lact-no-prod            ; Heat expended by lactation (no tissue production energy) [J 30min-1]

  n-litters                 ; Count of how many litters of pups are conceived per season [N]
  LRS                       ; Lifetime reproductive success measured by pups survived to weaning [N]
  repro-outs                ; List of how many pups are conceived, born, and weaned per pregancy and abortion and offspring mortality rates [N & %]
  ITV-repro                 ; Individual level of variation in reproduction allocation, modifies base curve value [%]

  ; Growth and lean mass deposition
  lean-mass-perc-pro        ; Lean mass protein content [%]
  ED-lean-mass              ; Energy density of lean mass [J kg-1]
  m-lean-mass               ; Realized growth & lean mass deposition costs [J 30min -1]
  m-lean-mass-no-prod       ; Inefficiency of lean mass deposition (energy expended to deposit lean mass) [J 30min -1]
  ITV-growth                ; Individual level of variation in growth allocation, modifies base curve value [%]

  ; Storage
  energy-mobilized
  pro-storage-perc

  m-tot                     ; Total costs [J 30min -1]
  daily-m-tot               ; List of total daily costs [J day-1]

  ; Daily costs per process [J day-1]
  daily-m-HIF
  daily-m-BMR
  daily-m-move
  daily-m-preg
  daily-m-lact
  daily-m-lean-mass
  daily-m-preg-no-prod
  daily-m-lact-no-prod
  daily-m-lean-mass-no-prod
  m-tot-no-prod             ; Total costs without intake used to fuel tissue production [J 30min-1]
  daily-m-tot-no-prod       ; List of costs without intake used to fuel tissue production [J day-1]

  home-c

  ; For evaluation
  birth-day
  age-1st-birth
  litters-per-year

  ; For scenarios
  age-birth
  IBI-year

]

to setup

  ; Start by setting up landcape, globals, and creating animals
  clear-all
  reset-ticks
  if debug = "Scen" [ setup-scenarios ]
  setup-landscape
  setup-globals
  create-animals

end

to setup-landscape

  ; Initialize base properties
  ask patches [
    set pcolor 6
  ]

  let cover round ( count patches * perc-resource-cells )
  let cov 0
  set resource-cells no-patches

  while [cov < cover]
  [
    ask one-of patches
    [
      ifelse (( member? self resource-cells ) and ( any? neighbors with [not member? self resource-cells] ) and ( cov < cover ))
      [ ask one-of neighbors with [ not member? self resource-cells ] [ set resource-cells ( patch-set resource-cells self ) set cov cov + 1 ]]
      [ if random-float 1 < ( 1 - fragmentation-level ) and ( not member? self resource-cells ) and ( cov < cover ) [ set resource-cells ( patch-set resource-cells self ) set cov cov + 1 ]]
    ]
  ]

  ask resource-cells [
    ifelse debug = "Scen" [
      set max-resources ( item ( 1 + model ) scenario-res-levs ) * max-resources-base
    ]
    [
    set max-resources max-resources-base
    ]

    set resource-level max-resources
    set pcolor scale-color violet resource-level ( max-resources-base + 50 ) -50
  ]

  ; if debug = "Dens" or debug = "Cal" or debug = "Eval" [ set dens-cells patch-set n-of 100 resource-cells ]

end

to setup-globals

  ; Year settings
  set year 1
  ifelse overwinter-skip? = false [
    set year-start 1
    set year-end 365
    set day year-start
  ] [
    ;set year-start 116
    set year-end 276
    set year-start 90
    ;set year-end 365 - 30
    set day year-start
  ]

  set ticks-per-day 48               ; Assuming half hour timesteps

  set cell-size-m 10                 ; Assuming 10m cells

  ;;; Animal global values ;;;

  ;; Energy intake
  set stomach-fill-perc 0.0507       ; Meese 1971
  set	AE-food	0.836	                 ; Piątkowska & Weiner 1987, Kaczmarski 1966, Meese 1971, & Peacock and Speakman 2001
  set HIF 0.228                      ; Hastings et al. 1997 & Even and Blais 2016
  set DM-food 0.67                   ; Meese 1971
  set	ED-food	12281.1                ; Meese 1971

  ;; Basal metabolic rate
  set B0 6053.1                      ; From Sadowska et al. 2015, Grosiak et al. 2020, and Górecki 1968
  set gamma 0.64                     ; From Sadowska et al. 2015, Grosiak et al. 2020, and Górecki 1968

  ;; Cost of transport
  set speed-mean 0.166               ; From control animals in Maiti et al. 2019
  set speed-max 0.822                ; From control animals in Maiti et al. 2019
  set HR-r-min 11.3                  ; Mazurkiewicz 1983, Radda 1968, Eccard and Ylönen 2002, 2007, Mappes et al. 1995, Schirmer et al. 2019, Bujalska and Grüm 1987, Carslake 2003
  set HR-r-max 32.2                  ; Mazurkiewicz 1983, Radda 1968, Eccard and Ylönen 2002, 2007, Mappes et al. 1995, Schirmer et al. 2019, Bujalska and Grüm 1987, Carslake 2003
  set intercept-pcot 4.7             ; Derived from data in Chappell et al. 2004, Dlugosz et al. 2009, & Rezende et al. 2006
  set slope-pcot 0.63                ; Derived from data in Chappell et al. 2004, Dlugosz et al. 2009, & Rezende et al. 2006
  set intercept-icot 10.6            ; From relationship in Pontzer 2016
  set slope-icot -0.29               ; From relationship in Pontzer 2016

  ;; Reproduction
  ; Pregnacy
  set t-mating [121 273]             ; Oksanen et al. 2001, Koivula et al. 2003, & Nyholm & Meurling 1975
  set t-mature 45                    ; Oksanen et al. 2001, Bujalska 1983, & Buchalczyk 1970
  set t-gest 20                      ; Koivula et al. 2003, Bujalska & Ryszkowski 1966, & Kaczmarski 1966
  set t-0 4                          ; Brambell and Rowlands 1936, Bujalska & Ryszkowski 1966
  set n-emb-range [2 9]              ; Brambell and Rowlands 1936, Nerquaye-Tetteh and Clarke 1990, Nyholm and Meurling 1979, & Wiger 1979
  set	emb-growth-c	0.112	           ; Calculated using data in Ożdżeński and Mystkowska 1976 and following Ricklefs 2010
  set	emb-mass-init	0.000000069      ; Calculated using data in Ożdżeński and Mystkowska 1976 and following Ricklefs 2010
  set	emb-mass-inf	19.957	         ; Calculated using data in Ożdżeński and Mystkowska 1976 and following Ricklefs 2010
  set percent-fat-emb 0.038          ; Fedyk 1974 & Sawicka-Kapusta 1974
  set percent-pro-emb 0.102          ; Fedyk 1974 & Sawicka-Kapusta 1974
  set ED-pl 3249.9                   ; Luz and Griggio 1996
  set DE-pl 0.501                    ; Greizerstein 1982

  ; Lactation
  set t-nurs 21                      ; Kaczmarski 1966, Oksanen et al. 2001, Oksanen et al. 1999, & Horne & Ylönen 1998
  set off-BMR-red 0.501              ; Kam, Khokhlova and Degen 2006 & Koteja and Weiner 1993
  set off-growth-eff 0.88            ; Kam and Degen 1993
  set milk-prod-eff 0.825            ; Bondi 1982 & Romero et al. 1975

  if debug = 5 or debug = 6 [
    set repro-outs-list []
    set repro-outs-list lput ( list "year" "NReproducing" "NLittersMean" "NLittersSD" "NConceivedMean" "NConceivedSD" "NBornMean" "NBornSD" "NWeanedMean" "NWeanedSD" "AbortionRateMean" "AbortionRateSD" "OffspringMortalityMean" "OffspringMortalitySD" ) repro-outs-list
  ]

  ;; Growth and lean mass deposition
  set	ED-pro	23.5 * 1000 * 1000     ; From Fedyk 1974, Livesey 1984, Brody 1968, Kleiber 1975
  set	DE-fat	0.735	                 ; From Pullar and Webster 1977
  set	DE-pro	0.444	                 ; From Pullar and Webster 1977
  set	growth-lm-inf	0.0264           ; Estimated using data from Fedyk 1974, Hansson 1991, Rudolf et al. 2017, Sawicka-Kapusta 1974, Balčiauskienė 2007, & Gębczyński 1975
  set	growth-lm-k	0.0964 	           ; Estimated using data from Fedyk 1974, Hansson 1991, Rudolf et al. 2017, Sawicka-Kapusta 1974, Balčiauskienė 2007, & Gębczyński 1975
  set	growth-lm-No 0.0034	           ; Estimated using data from Fedyk 1974, Hansson 1991, Rudolf et al. 2017, Sawicka-Kapusta 1974, Balčiauskienė 2007, & Gębczyński 1975

  ;; Storage
  set	ED-fat 39.1	* 1000 * 1000      ; Fedyk 1974, Livesey 1984, Brody 1968
  set ED-cpro 19.1	* 1000 * 1000    ; Brody 1968, Kleiber 1975
  set gamma-mobilize 0.015           ; Belkhou et al. 1991, Dunn et al. 1982, & Cherel et al. 1992
  set perc-water-adi 0.119           ; DiGirolamo and Owens 1976 & Reinoso et al. 1997
  set SL-max 0.398                   ; Estimated as the maximum mean value in Fedyk 1974 plus one standard deviation adjusted for water content
  set SL-list []
  set SL-at-death-list []
  if debug = 8 [ file-open "C:\\Users\\cara\\Dropbox\\PC\\Documents\\PostdocBioMoveRepos\\MetabolicVariation\\EBIVRProject\\Data\\Verification\\Storage\\StorageLevelsState.txt" ]

  ;; Mortality
  set t-max-age 620                  ; Buchalczyk 1970, Rudolf et al. 2017, Sawicka-Kapusta 1974, Balčiauskienė 2007

  if debug = 10 or debug = "Scen" [
    set age-at-death-list [[][]]
    set age-at-death-temp-list []
  ]

  set winter-surv [0.20 0.632]       ; Koskela, 1998; Oksanen et al. 2001; Kallio et al. 2007; Boratyński et al. 2010; Haapakoski et al 2012

  set pop-list []
  set off-list []
  set dens-list []

;  ;; For Calibration:
;  if debug = "Cal" [
;    set p1-FMAB []
;    set p2-BMxLS [[][]]
;    set p34-BMLMxA [[][][]]
;    set p567-dsbxMMFILM [[][][][]]
;    set p8910-LSxpFIEUMEO [[][][][]]
;    set p11-PMxLS [[][]]
;    set p12-LSb []
;    set p13-LSw []
;    set p14-BF []
;    set p15-BF []
;    set p16-FMRxBM [[][]]
;  ]
;
;  ;; For Evaluation:
;  if debug = "Eval" [
;    set p17-FMRxState [[][]]
;    set p18-IRxState [[][]]
;    set p19-surv []
;    set p20-BMadult []
;    set p21-age1stbirth []
;    set p22-BMRadult []
;    set p23-LSb []
;    set p24-LpY []
;    set p25-BMemb []
;    set p26-BMwean []
;    set p27-dens []
;  ]

;  let row-no 0
;  ifelse debug = "Cal"
;  [ set row-no behaviorspace-run-number ]
;  [ set row-no 1 + random 29 ]

  let row-no 1 + random 29

  let lhc csv:from-file "ABCParameterTable.csv"
  set lhc item row-no lhc
  set growth-lm-prob-const item 0 lhc
  set growth-lm-prob-mid item 1 lhc
  set preg-prob-const item 2 lhc
  set preg-prob-mid item 3 lhc
  set lact-prob-const item 4 lhc
  set lact-prob-mid item 5 lhc
  set surv-prob-const item 6 lhc
  set surv-prob-mid item 7 lhc
  set surv-mod-embryo item 8 lhc
  set surv-mod-off item 9 lhc

end


to create-animals

  ; Create initial animals based on n-animals slider value and initialize state variables at default values
  create-turtles n-animals [
    setxy random-xcor random-ycor
    set color white
    set size 0.7

    ; Arbitrary starting values
    set mass 0.01
    set age 20
    set storage-level SL-max / 2

    set lean-mass mass * (1 - storage-level)
    set adipose-mass mass * storage-level

    ; Initialize metabolic traits with a uniform distribution
    set ITV-BMR ( 1 - ITV-var-BMR / 100 ) + random-float (( 1 + ITV-var-BMR / 100 ) - ( 1 - ITV-var-BMR / 100 ))
    set ITV-repro ( 1 - ITV-var-repro / 100 ) + random-float (( 1 + ITV-var-repro / 100 ) - ( 1 - ITV-var-repro / 100 ))
    set ITV-growth ( 1 - ITV-var-growth / 100 ) + random-float (( 1 + ITV-var-growth / 100 ) - ( 1 - ITV-var-growth / 100 ))

    set stomach-fill []
    set t-mating-offset -15 + random 30
    set pregnancy-status false
    set lactation-status false
    set ITV-BMR-off-list []
    if debug = 5 or debug = 6 [ set repro-outs ( list [] [] [] ) ]
    set daily-activity []
    set daily-ingestion []
    set daily-m-tot []
    set daily-m-tot-no-prod []

    set home-c patch-here

    set age-birth 0
    set LRS 0
    set IBI-year out-year

  ]

end

to setup-scenarios

  file-close-all ; Close any files open from last run


  ;let path "C:\\Users\\cara\\Dropbox\\PC\\Documents\\PostdocBioMoveRepos\\MetabolicVariation\\EBIVRProject\\Data\\Scenarios\\EcoEvoInputs\\"
  let path "NDVIInputs/"

  let site 0
  if sites = 1 [ set site "Asturias_" ]
  if sites = 2 [ set site "Bielowieza_" ]
  if sites = 3 [ set site "Calabria_" ]
  if sites = 4 [ set site "Evenstad_" ]
  if sites = 5 [ set site "Frýdek-Místek_" ]
  if sites = 6 [ set site "Havelaue_" ]
  if sites = 7 [ set site "Konnovesi_" ]
  if sites = 8 [ set site "Le Quartier_" ]
  if sites = 9 [ set site "Pallasjarvi_" ]
  if sites = 10 [ set site "Stroemsund_" ]

  let experiment 0
  if experiments = 1 [ set experiment "ssp245.csv" ]
  if experiments = 2 [ set experiment "ssp585.csv" ]

  if debug = "Scen"
  [
    let file (word "NDVIInputScenarios_" site experiment)

    file-open (word path file)

    set scenario-res-levs csv:from-row file-read-line
    set scenario-res-levs csv:from-row file-read-line

    ; print scenario-res-levs
   ]

  ; Initalize scenario outputs
  set BM-list [[][]]
  set activity-list [[][]]
  set BC-list [[][]]
  set LSW-list [[][]]
  set LSW-temp-list []
  set neo-BM-list [[][]]
  set neo-BM-temp-list []
  set wean-BM-list [[][]]
  set wean-BM-temp-list []
 ; set IBI-list [[][]]
 ; set IBI-temp-list []
  set LRS-list [[][]]
  set LRS-temp-list []
  set age-1st-birth-list [[][]]
  set age-1st-birth-temp-list []
  set LPY-list  [[][]]
  set LPY-temp-list []

  set abun-outs-list []


  set FMR-list [[][]]
  set loco-list [[][]]
  set repro-list [[][]]
  set growth-list [[][]]

end

to go

  if ( debug = 1 ) and ( ticks > 3 ) [                   ; Profiler
    profiler:start
  ]


  if count turtles <= 2 or year = final-year + 1 [           ; Model stops if all animals die or end of simulation period is reached
;    if ( debug = "Dens" or debug = "Cal" ) and length dens-list > 0 [
;      set dens-mean mean dens-list
;      set dens-med median dens-list
;    ]
;    if debug = "Eval" [ set p27-dens dens-list ]
    stop
  ]

  if remainder ticks ticks-per-day = 0 [ daily-tasks ]   ; Tasks performed once per day or less

  ask turtles [                                          ; Animals move and calculate their energy budgets
    move
    energy-budget
  ]

   if debug = "Scen" [
    if year > item 0 scenario-res-levs [ print "year mis-match" ]
  ]

  tick

  if ( debug = 1 ) and ( ticks > 3 ) [        ; Profiler
    profiler:stop
    print profiler:report
    profiler:reset
  ]

end

; Tasks performed once per day or less
to daily-tasks

  if ticks > 1 [
    update-monitors                                                               ; Update plots and monitors

    if debug = 8 and year = out-year [                                            ; If debugging storage, collect storage levels of all animals and dependant offspring
      set SL-list sentence SL-list ([storage-level] of turtles)
      ask turtles [
        ; State specific outputs
        if pregnancy-status = false and lactation-status = false and age > t-mature [ file-print (list (precision storage-level 2) ", " age ", " 3) ]
        if pregnancy-status = true and lactation-status = false [ file-print (list (precision storage-level 2) ", " age ", " 4) ]
        if pregnancy-status = false and lactation-status = true [ file-print (list (precision storage-level 2) ", " age ", " 5) ]
        if pregnancy-status = true and lactation-status = true [ file-print (list (precision storage-level 2) ", " age ", " 6) ]
        if age < t-mature [ file-print (list (precision storage-level 2) ", " age ", " 2) ]
      ]
      if any? turtles with [ lactation-status = true ] [
        ask turtles with [ lactation-status = true ] [
          repeat n-off [
            set SL-list lput SL-off SL-list
            file-print (list (precision SL-off 2) ", " (floor ds-birth) ", " 1) ]
        ]
      ]
    ]

    ask turtles [
      ; Starvation and age-related mortality are executed once per day
      mortality-max-age
      mortality-starvation
      ; Animals age once per day
      set age age + 1
    ]
  ]

;  if debug = "Pop" [
;    set pop-list lput (count turtles) pop-list
;    let on 0
;    if any? turtles with [ n-off > 0 ] [set on sum [ n-off ] of turtles with [ n-off > 0 ]]
;    set off-list lput on off-list
;  ]

;  if ( debug = "Dens" or debug = "Cal" or debug = "Eval" ) and ( year = out-year and day >= 121 and day <= 334 ) [   ; Collect local density values from patchset but only within seasons where data were available
;    let dens count turtles-on dens-cells
;    set dens-list lput dens dens-list
;  ]

  ; Update day and check if last day of year
  set day day + 1

  ; Scenario updates
  if debug = "Scen" [ update-scenarios ]

  ask turtles [
    ; If debugging FMR, reset monitors
    if debug = 9 or debug = "Scen" [
      ; print daily-m-move / daily-m-BMR

      set daily-m-HIF 0
      set daily-m-BMR 0
      set daily-m-move 0
      set daily-m-preg 0
      set daily-m-lact 0
      set daily-m-lean-mass 0
      set daily-m-preg-no-prod 0
      set daily-m-lact-no-prod 0
      set daily-m-lean-mass-no-prod 0
    ]
  ]

  ; If so, execute yearly tasks & overwinter mortality, update year, & reset day to first day of year
  if day >= year-end [ yearly-tasks ]

end

; Resources replenish once per day
to resources-grow

  let r-growth (ticks - last-eaten) * r-growth-ts
;  print last-eaten
;  print ticks
;  print r-growth
;  print resource-level


  ; Resource cells grow if lower than their maximum value
  if resource-level < max-resources [ set resource-level resource-level + r-growth ]
  ;  print resource-level

  ; Clamp at maximum value
  if resource-level > max-resources [ set resource-level max-resources ]
  ;  print resource-level

  if pcolor-update? [ set pcolor scale-color violet resource-level ( max-resources-base + 50 ) -50 ]

end

; Tasks performed once per year or less
to yearly-tasks

  ; If debugging reproduction update yearly reproduction outputs for all reproductively active animals ( will miss animals which died before the year ended )
  if debug = 5 or debug = 6 [
    ask turtles [
      let abort-rates ( map [ [ x y ] -> precision ( 1 - x / y ) 3 ] ( item 1 repro-outs ) ( item 0 repro-outs ) )
      set repro-outs lput abort-rates repro-outs
      let off-mort-rates ( map [ [ x y ] -> ifelse-value (y != 0) [ precision ( 1 - x / y ) 3 ] [ -99 ]] ( item 2 repro-outs ) ( item 1 repro-outs ) )  ; only calculate offspring mortality rates when animals didn't lose all offspring already to abortion
      set off-mort-rates remove -99 off-mort-rates
      set repro-outs lput off-mort-rates repro-outs
      ; if length item 0 repro-outs > 0 [ print repro-outs ]
    ]

    ; Number of offspring conceived
    let n-conc []
    if any? turtles with [ length item 0 repro-outs > 0 ] [
      ask turtles with [ length item 0 repro-outs > 0 ] [ set n-conc lput ( item 0 repro-outs ) n-conc]
      set n-conc reduce sentence n-conc
    ]

    ; Number of offspring born
    let n-birth []
    if any? turtles with [ length item 0 repro-outs > 0 ] [
      ask turtles with [ length item 0 repro-outs > 0 ] [ set n-birth lput ( item 1 repro-outs ) n-birth]
      set n-birth reduce sentence n-birth
    ]

    ; Number of offspring weaned
    let n-wean []
    if any? turtles with [ length item 0 repro-outs > 0 ] [
      ask turtles with [ length item 0 repro-outs > 0 ] [ set n-wean lput ( item 2 repro-outs ) n-wean]
      set n-wean reduce sentence n-wean
    ]

    ; Abortion rate
    let abort-rate []
    if any? turtles with [ length item 0 repro-outs > 0 ] [
      ask turtles with [ length item 0 repro-outs > 0 ] [ set abort-rate lput ( item 3 repro-outs )  abort-rate]
      set abort-rate reduce sentence abort-rate
      ; print abort-rate
    ]

    ; Offspring mortality rate
    let off-mort-rate []
    if any? turtles with [ length item 0 repro-outs > 0 ] [
      ask turtles with [ length item 0 repro-outs > 0 ] [ set off-mort-rate lput ( item 4 repro-outs )  off-mort-rate]
      set off-mort-rate reduce sentence off-mort-rate
      ; print off-mort-rate
    ]

    ; Collect population outputs
    let repro-outs-season []
    if any? turtles with [ length item 0 repro-outs > 0 ] [
      set repro-outs-season ( list
        year
        ( count turtles with [ length item 0 repro-outs > 0 ] )
        ( precision mean [n-litters] of turtles with [ length item 0 repro-outs > 0 ] 3 )
        ( precision standard-deviation [n-litters] of turtles with [ length item 0 repro-outs > 0 ] 3 )
        ( precision mean n-conc 3 ) ( precision standard-deviation n-conc 3 )
        ( precision mean n-birth 3 ) ( precision standard-deviation n-birth 3 )
        ( precision mean n-wean 3 ) ( precision standard-deviation n-wean 3 )
        ( precision mean abort-rate 3 ) ( precision standard-deviation abort-rate 3 )
        ( precision mean off-mort-rate 3 ) ( precision standard-deviation off-mort-rate 3 )
      )
    ]

    ; set repro-outs-list lput repro-outs-season repro-outs-list
    print repro-outs-season

    ; Reset monitors
    ask turtles [
      set repro-outs ( list [] [] [] )
      set n-litters 0
    ]
  ]

 ; if debug = "Eval" and year = out-year [ ask turtles with [ age > t-mature + t-gest + 5  ] [ set p24-LpY lput ( list floor age litters-per-year ) p24-LpY ]]


  ; When skipping winter just have food values reset at the beginning of the year
  ; Update ages of overwintering individuals
  if overwinter-skip?  = true [

    ; Calculate overwinter mortality
    mortality-overwinter

;    ask resource-cells [
;      set resource-level max-resources
;      if pcolor-update? [ set pcolor scale-color violet resource-level ( max-resources-base + 50 ) -50 ]
;    ]

    ask turtles [
      let days-skip ( 365 - year-end + year-start )

      ; Animals increase their age by the days skipped
      set age age + days-skip
      ; And grow to the infinite size modified by their percent allocation to growth
      let perc-allo-growth-lm 1 / (1 + e ^ (-1 * growth-lm-prob-const * ( storage-level - ( growth-lm-prob-mid  * ITV-growth ))))
      set lean-mass growth-lm-inf * perc-allo-growth-lm
      set mass lean-mass / ( 1 - storage-level )
      set adipose-mass mass - lean-mass
    ]
  ]

  ; Update year and day
  set year year + 1
  set day year-start

end

; Animals calculate their movement direction and distance then move once per tick
to move

  ; Adjust daily-activity list length if necessary
  if length daily-activity = ticks-per-day [ set daily-activity remove-item 0 daily-activity ]


  ; Probability of moving is based on energy balance, e.g. the relative amount of food debt compared to daily requirements, and the amount of food encountered by animals
  let daily-food-est ( sum daily-m-tot ) / ( AE-food * ED-food )
  let move-prob 0
  if daily-food-est > 0 [ set move-prob ( -1 / daily-food-est ) * food-debt + 1 ]

  if sum stomach-fill >= stomach-fill-max [ set move-prob 1 ]
  if food-debt > daily-food-est [ set move-prob 0 ]

  ; If a random number is less than the movement probability, then animals don't move that tick
  ; ( move-prob = 1 means animals have 0% chance of moving, move-prob = 0 means 100% chance of moving )

  ifelse random-float 1 < move-prob [
    ; if random value is less than move-prob animals don't move in that timestep
    set move-speed 0
    set daily-activity lput 0 daily-activity
  ]
  [
    ; Else animals move forward

    ; Movement speed is selected randomly in meters per s based on mean and maximum speeds (cm per s)
    let s-mean speed-mean * 100
    let s-max speed-max * 100

    let a s-mean * s-mean / (s-max - s-mean)
    let l 1 / ((s-max - s-mean) / s-mean)

    set move-speed random-gamma a l
    while [ move-speed > s-max ] [ set move-speed random-gamma a l ] ; Check to make sure speed is not higher than the maximum
    set move-speed move-speed / 100 * 60 * 30 ; Convert to meters per half hour

    ifelse home-ranging? [
      if patch-here != home-c [ move-to home-c ]

      let HR-r (HR-r-min / cell-size-m) + random-float ((HR-r-max - HR-r-min) / cell-size-m)
      let cs patches in-radius (HR-r + 0.5)
        let c one-of cs
        set foraging-cell c
      ]
      [
        set heading random 360
          fd ( move-speed / cell-size-m )
          set foraging-cell patch-here
      ]


    set daily-activity lput 1 daily-activity
  ]

end

;    --------------------------------------------    ;
;    |      BEGIN ENERGY BUDGET PROCEDURES      |    ;
;    --------------------------------------------    ;

; Animals update their energy budget once per timestep
to energy-budget

  ; Cost calculations
  basal-maintenance
  cost-of-transport
  reproduction
  growth-lean-mass

  update-records

  energy-intake

  energy-allocation

  storage-dynamics

end

;--- Basal metabolic rate ---;
to basal-maintenance

  set m-BMR ( B0 * mass ^ gamma ) * ITV-BMR                   ; From Kleiber 1975, Sibly et al. 2013

end

;--- Cost of transport ---;
to cost-of-transport

  set pcot 0
  set icot 0
  set m-move 0

  if move-speed = 0 [ stop ]                                   ; If not moving assume resting and set costs to zero and proceed to next process

  set pcot intercept-pcot * mass ^ slope-pcot                  ; Postural costs of transport
  set icot intercept-icot * mass ^ slope-icot                  ; Incremental costs of transport

  let move-speed-m-per-s move-speed / (30 * 60)                ; Convert to speed in m per second for COT equation

  set m-move (pcot + icot * mass * move-speed-m-per-s) * 1800  ; Pontzer 2016, Chappell et al 2013, Halsey 2013

  if debug = 4 and year = out-year and print? = true [
    print word "speed: " move-speed-m-per-s  print word "ratio m-move / m-BMR: " (m-move / m-BMR)
    print (m-move / m-BMR)
  ]

end

;--- Growth and lean mass deposition ---;
to growth-lean-mass

  ; Percentage protein in lean mass based on age [%]
  set lean-mass-perc-pro -lean-mass-perc-protein(age)

  ; Energy density of lean mass [J kg-1]
  set ED-lean-mass lean-mass-perc-pro * ED-pro

  ; Allocation to lean mass deposition based on storage level [%]
  let perc-allo-growth-lm 1 / (1 + e ^ (-1 * growth-lm-prob-const * ( storage-level - ( growth-lm-prob-mid  * ITV-growth ))))

  ; Lean mass deposition rate [kg 30min-1]
  let lean-mass-depo (( growth-lm-k * perc-allo-growth-lm ) / ticks-per-day )*((( growth-lm-inf * perc-allo-growth-lm ) ^( 1 / 3 )) * ( lean-mass ^ ( 2 / 3 )) - lean-mass ) ; Sibly et al. 2013
  if lean-mass-depo < 0 [ set lean-mass-depo 0 ]
  ; if lean-mass-dep = 0 [ print "lean-mass-dep = 0" ]

  ; Cost of lean mass deposition [J 30min-1]
  set m-lean-mass ( lean-mass-depo * ED-lean-mass ) / DE-pro                                                   ; Boult et al. 2018
  ; if m-lean-mass = 0 [ print word mass " m-lean-mass = 0" ]

  ; Inefficiency of lean mass deposition [J]
  set m-lean-mass-no-prod m-lean-mass * ( 1 - DE-pro )

  ; Update mass in storage-dynamics procedure

end

; Percentage protein in lean mass based on age [%]
to-report -lean-mass-perc-protein [a]

  let lean-mass-perc-protein-est 0.23 * ( 1 - e ^( -0.06 * ( a + 10.19 ))) ; Estimated using data in Fedyk 1974 & Sawicka-Kapusta 1974

  if debug = 7 and year = out-year [
    set-current-plot "Lean mass percent protein"
    plotxy a lean-mass-perc-protein-est
  ]

  report lean-mass-perc-protein-est

end


;--- Reproduction ---;
to reproduction

  ; Check if should get pregnant
  if pregnancy-status = false and remainder ticks ticks-per-day = 0 [ conceive ]

  if lactation-status = true [ lactation ]
  if pregnancy-status = true [ pregnancy ]

end

;--- Pregnancy ---;
to pregnancy

  ; Check if time to give birth
  if ds-mating >= t-gest [
    ; If so, give birth and exit pregnancy procedure
    give-birth
    stop
  ]

  if ds-mating >= 0 [                                                     ; Check if days since mating is greater than 0 to account for delayed implantation
    ; Maximum offspring growth per embryo [g]
    let max-growth-emb e ^ (ln mass-emb + ((emb-growth-c / ticks-per-day) * (ln emb-mass-inf - ln mass-emb))) - mass-emb

    ; Fetal tissue investment per embryo
    let e-fat ((max-growth-emb / 1000) * percent-fat-emb * ED-fat) / DE-fat ; Energy needed for fat deposition
    let e-pro ((max-growth-emb / 1000) * percent-pro-emb * ED-pro) / DE-pro ; Energy needed for protein deposition

    let m-growth-emb e-fat + e-pro                                          ; Energy needed to fuel growth

    let max-growth-pl 0
    let m-growth-pl 0

    if ds-mating > 9.35 [                                                 ; Only check if greater than 9.35 days ( source equation is only greater than 0 from this point )
      ; Placental growth based on gestation date [g]
      set max-growth-pl ( -0.542 + 0.079 * ds-mating - 0.002243 * ds-mating ^ 2) - mass-pl ; Based on relationship for mice in Mu et al. 2008

      ; Placental tissue investment [J 30 min-1]
      set m-growth-pl ( max-growth-pl * ED-pl )/ DE-pl
    ]

    ; Allocation to pregnancy based on storage level [%]
    let perc-allo-preg 1 / (1 + e ^ (-1 * preg-prob-const * ( storage-level - ( preg-prob-mid * ITV-repro ))))

    ; Total cost of pregancy [J 30min-1]
    set m-preg ( n-emb * ( m-growth-emb + m-growth-pl )) * perc-allo-preg

    ; Embryos and placentae grow
    let growth-emb perc-allo-preg * max-growth-emb
    set mass-emb mass-emb + growth-emb

    if ds-mating > 9.35 [
      let growth-pl perc-allo-preg * max-growth-pl
      if debug = 5 and print? = true [ if who = 0 [ if pregnancy-status = true [ print word "growth-pl:     " growth-pl ]]]
      set mass-pl mass-pl + growth-pl
    ]

    set gest-mass ( n-emb * mass-emb + n-emb * mass-pl ) / 1000           ; Total additional mass of embyros and placentae

    ; Calculate pregnancy costs not associated with tissue production
    set m-preg-no-prod (( n-emb * m-growth-emb ) * perc-allo-preg * percent-fat-emb * ( 1 - DE-fat )) + (( n-emb * m-growth-emb ) * perc-allo-preg * percent-pro-emb * ( 1 - DE-pro )) + (( n-emb * m-growth-pl ) * perc-allo-preg * ( 1 - DE-pl ))

    ; Pregnancy debug outputs
    if debug = 5 and print? = true [ if who = 0 [ if pregnancy-status = true [
      print word "max-growth-emb: " max-growth-emb
      print word "e-growth-emb:   " m-growth-emb
      print word "SL mom:         " storage-level
      print word "perc-allo-preg: " perc-allo-preg
      print word "growth-emb:     " growth-emb
      print word "mass-emb:       " mass-emb
      print word "e-growth-pl:    " m-growth-pl
      print word "max-growth-pl:  " max-growth-pl
      print word "mass-pl:        " mass-pl
      print word "m-preg:         " m-preg
      print " "
    ]]]
  ]

  set ds-mating ds-mating + 1 / ticks-per-day                                        ; Advance gestation ticker

end

;--- Lactation ---;
to lactation

  ; Check if time to wean offspring
  if ds-birth >= t-nurs [
    ; if so, wean and exit lactation procedure
    wean
    stop
  ]

  ; Added for calibration to avoid runtime errors
  if mass-off <= 0 or lean-mass-off <= 0 or SL-off <= 0 [
    mortality-starvation
    stop
  ]

  ; Offspring maintenance costs [J 30min -1]
  let m-BMR-off-list ( map [ y -> ( B0 * mass-off ^ gamma ) * off-BMR-red * y ] ITV-BMR-off-list ) ; from Kleiber 1975, Sibly et al. 2013
  set m-BMR-off sum m-BMR-off-list

  ; Offspring maximum lean mass growth [kg 30min -1]
  let max-growth-lm-off ( growth-lm-k / ticks-per-day )*(( growth-lm-inf ^( 1 / 3 ) * ( lean-mass-off ^ ( 2 / 3 ))) - lean-mass-off )

  ; Percentage protein in lean mass based on age [%]
  let lean-mass-perc-pro-off -lean-mass-perc-protein(ds-birth)
  let ED-lean-mass-off lean-mass-perc-pro-off * ED-pro                         ; Energy density of lean mass [J kg-1]
  set m-growth-lm-off ( max-growth-lm-off * ED-lean-mass-off )/ off-growth-eff

  ; Allocation to lactation based on storage level [%]
  let perc-allo-lact 1 / (1 + e ^ (-1 * lact-prob-const * ( storage-level - ( lact-prob-mid * ITV-repro )))) * 2

  ; Estimated cost of lactation [J 30min -1]
  let m-lact-est ( m-BMR-off + n-off * m-growth-lm-off ) / milk-prod-eff

  ; Total cost of lactation [J 30min -1]
  set m-lact m-lact-est * perc-allo-lact

  ; Milk energy output available to offspring [J 30min -1]
  let milk-energy-output m-lact * milk-prod-eff

  let adipose-mass-off 0
  ifelse m-lact > m-lact-est
  [
    ; if mothers provide more milk than calculated costs then BMR and growth costs are covered
    if debug = 6 and who = 0 and print? = true [ print "Lactation allocation decision 1" ]

    set lean-mass-off lean-mass-off + max-growth-lm-off                                          ; Offspring grow maximally
    let m-lact-adi-per-off ( m-lact - m-lact-est ) / n-off                                       ; Extra is stored as fat
    let adi-per-off ( m-lact-adi-per-off * DE-fat ) / ( ED-fat * ( 1 - perc-water-adi ))
    set adipose-mass-off ( mass-off * SL-off ) + adi-per-off
  ]
  [
    ifelse milk-energy-output > m-BMR-off
    [
      ; If milk energy is not greater than calculated costs, but is greater than BMR, then BMR is covered and offspring grow suboptimally
      if debug = 6 and who = 0 and print? = true [ print "Lactation allocation decision 2" ]
      let m-lact-grow milk-energy-output - m-BMR-off                                             ; Reduce milk energy by BMR costs
      let m-lact-grow-per-off m-lact-grow / n-off                                                ; Divvy remaining energy per offspring
      if m-lact-grow-per-off < 0 [ print "m-lact-grow-per-off < 0" ]
      let grow-per-off ( m-lact-grow-per-off * off-growth-eff ) / ED-lean-mass-off               ; Convert to mass
      set lean-mass-off lean-mass-off + grow-per-off                                             ; Offspring grow suboptimally
      set adipose-mass-off ( mass-off * SL-off ) + 0                                             ; And do not deposit fat

      if debug = 6 and print? = true [ if who = 0 [ if lactation-status = true [ print word "grow-per-off:          " grow-per-off ]]]
    ]
    [
      ; If milk energy is less than BMR costs, then offspring mobilize energy to cover only BMR
      if debug = 6 and who = 0 and print? = true [ print "Lactation allocation decision 3" ]
      let off-energy-mobilize ( m-BMR-off - milk-energy-output ) / n-off                                                              ; Calculate energy needed to be mobilized
      let pro-storage-perc-off ( ED-pro / 1000 * gamma-mobilize ) / ((ED-fat / 1000 * SL-off) + (ED-pro / 1000 * gamma-mobilize))     ; Determine protein content of metabolized tissue
      if debug = 6 and who = 0 and print? = true [ print word "pro-storage-perc-off:          " pro-storage-perc-off ]
      let lean-mass-mobilized ( off-energy-mobilize * pro-storage-perc-off ) / ED-lean-mass                                           ; Mobilize lean mass and adipose tissue
      let adipose-mobilized ( off-energy-mobilize * ( 1 - pro-storage-perc-off )) / (( 1 - perc-water-adi ) * ED-fat )
      set lean-mass-off lean-mass-off - lean-mass-mobilized                                                                           ; Update lean and adipose masses
      set adipose-mass-off ( mass-off * SL-off ) - adipose-mobilized
    ]
  ]

  ; Update total offspring masses and storage levels
  set mass-off adipose-mass-off + lean-mass-off
  set SL-off adipose-mass-off / mass-off

  ; Calculate lactation costs not associated with tissue production
  set m-lact-no-prod m-lact * ( 1 - milk-prod-eff )

  ; Lactation debug outputs
  if debug = 6 and print? = true [ if who = 0 [ if lactation-status = true [
    print word "perc-allo-lact:   " perc-allo-lact
    print word "m-lact-est:       " m-lact-est
    print word "m-lact:           " m-lact
    print word "m-BMR-off:        " m-BMR-off
    print word "max-growth-off:   " max-growth-lm-off
    print word "m-growth-lm-off:  " m-growth-lm-off
    print word "lean-mass-off:    " lean-mass-off
    print word "adipose-mass-off: " adipose-mass-off
    print word "mass-off:         " mass-off
    print word "SL-off:           " SL-off
    print " "
  ]]]

  set ds-birth ds-birth + (1 / ticks-per-day)                                                 ; Advance lactation ticker

end


;--- Energy intake ---;
to energy-intake

  ; reset energy trackers
  set energy-assimilated 0
  set energy-mobilized 0
  set m-HIF 0

  ; Calculate stomach capacity and transit time
  ; Maximum stomach fill [g]
  let BMR-inf 1 ; factor to adjust for ITV in BMR to adjust baseline simulations
  let stomach-mod ( m-BMR + m-preg + m-lact ) / ( m-BMR / (( ITV-BMR - 1 ) * BMR-inf + 1 ))
  set stomach-clear-rate ( 5.473 * mass ^ 0.278 ) * 2 * (( stomach-mod - 1 ) * -1 + 1 )  ; Abraham et al. 2021 relationship for mammal gut transit time
  if round stomach-clear-rate <= 0 [set stomach-clear-rate 1]
  set stomach-fill-max (( mass * 1000 * stomach-fill-perc ) / DM-food ) * stomach-mod
  while [ length stomach-fill >= round stomach-clear-rate ] [ set stomach-fill remove-item 0 stomach-fill ]

  ; Calculate energy intake needs
  ; Total potential ingestion rate for a timestep [g 30min-1]
  let IR-timestep ( m-tot - m-HIF ) / ( AE-food * ( 1 - HIF ) * ED-food )             ; Should try to maintain energy balance, per Hopkins and Blundell 2017

  ; Impact of body fat on hunger
  ; Inspired by dual intervention point theory in Speakman 2014

  ; Set lower and upper intervention points
  let LIP 0.184
  let UIP 0.214

  let IR-mod 1
  if storage-level < LIP
  [
   ; Scaled storage level from 0 to 1 [unitless]
   let SL-scaled storage-level / LIP
   ; Ingestion rate modifier based on storage level [%/100]
   set IR-mod ( -1 * SL-scaled + 1 )^ 3 + 1
  ]

  if storage-level > UIP
  [
    ; Scaled storage level from 0 to 1 [unitless]
    let SL-scaled ( storage-level - UIP ) / ( SL-max - UIP )
    ; Ingestion rate modifier based on storage level [%/100]
    set IR-mod (-1 * SL-scaled )^ 3 + 1
  ]

  if storage-level > SL-max [ set IR-mod 0 ]

  ; Update record of unmet food requirements [g]
  set food-debt food-debt + ( IR-timestep * IR-mod )

  ; Realized ingestion rate [food units]
  let IR-real 0

  ifelse move-speed != 0 [

    let c foraging-cell

    ; Ask cell to update food levels when encountered based on it's current food level and the last time it was consumed
    ask c [ if member? self resource-cells and resource-level < max-resources and last-eaten != ticks [ resources-grow ]]

    ; Resources found in cell here
    let food-here [resource-level] of c

    ; Realized ingestion rate [g 30min-1]
    ifelse food-here > food-debt
    [
      set IR-real food-debt
      if sum stomach-fill + IR-real >= stomach-fill-max [
          set IR-real stomach-fill-max - sum stomach-fill
          if IR-real < 0 [ set IR-real 0 ]]
      if IR-real < 0 [ print word who "'s IR is less than zero! 1" ]
    ]
    [
      set IR-real food-here
      if sum stomach-fill + IR-real >= stomach-fill-max [
          set IR-real stomach-fill-max - sum stomach-fill
          if IR-real < 0 [ set IR-real 0 ]]
      if IR-real < 0 [ print word who "'s IR is less than zero! 2" ]
    ]

    ; Update record of unmet food requirements [g]
    set food-debt food-debt - IR-real

    ; Remove food from cell
    if [resource-level] of c > 0 [
      ask c [
        set resource-level resource-level - IR-real
        set last-eaten ticks
        if pcolor-update? [ set pcolor scale-color violet resource-level ( max-resources-base + 50 ) -50 ]
    ]]
  ]
  [
    ; Do not eat if not moving
    set IR-real 0
  ]

  ; Update stomach fill
  set stomach-fill lput IR-real stomach-fill

  ; Metabolizable energy intake [J 30min-1]
  let MEI IR-real * AE-food * ED-food

  ; Update daily ingestion monitor
  if length daily-ingestion = ticks-per-day [ set daily-ingestion remove-item 0 daily-ingestion ]
  set daily-ingestion lput MEI daily-ingestion

  ; Energy spent on the heat increment of feeding [J]
  set m-HIF MEI * HIF

  ; Calculate assimilated energy based on ingestion [in g] and energy density and assimilation efficiency of food
  set energy-assimilated MEI * ( 1 - HIF )

  ; Energy intake debug outputs
  if debug = 2 and remainder ticks ticks-per-day = 0 [
    if print? = true [ if who = 0 [
      print word "IR-timestep " IR-timestep
      print word "IR-mod " IR-mod
      print word "IR " (IR-timestep * IR-mod)
     ;; print word "food here " food-here
      if move-speed > 0 [print "    & moving!" ]
      print word "IR-real " IR-real
      print word "daily-m-tot " sum daily-m-tot
      print word "daily-ingestion " sum daily-ingestion
      print word "m-HIF " m-HIF
      print word "energy-assimilated " energy-assimilated
      print " "
    ]]
  ]

end

;--- Allocation ---;
to energy-allocation

  ifelse energy-assimilated > ( m-tot - m-HIF )
  [
    ; If assimilated energy sufficient to cover costs, use that and reduce accordingly
    set energy-assimilated energy-assimilated - ( m-tot - m-HIF )
  ]
  [
    ; If not, use available assimilated energy and mobilize the remainder
    let energy-split ( m-tot - m-HIF ) - energy-assimilated
    set energy-assimilated 0
    set energy-mobilized energy-mobilized + energy-split
  ]

end

;--- Storage ---;
to storage-dynamics

  ; Protein contribution to storage dynamics [%]
  set pro-storage-perc ( ED-pro * gamma-mobilize ) / (( ED-fat * storage-level ) + ( ED-pro * gamma-mobilize ))

  ; Lean mass synthesized [J]
  let lean-mass-synthesized-energy ( energy-assimilated * pro-storage-perc )

  ; Lean mass mobilized [J]
  let lean-mass-mobilized-energy ( energy-mobilized * pro-storage-perc )

  ; Adipose synthesized [kg]
  let adipose-synthesized ( energy-assimilated * ( 1 - pro-storage-perc ) * DE-fat ) / ( ED-fat * ( 1 - perc-water-adi ) )

  ; Adipose mobilized [kg]
  let adipose-mobilized ( energy-mobilized * ( 1 - pro-storage-perc )) / ( ED-fat * ( 1 - perc-water-adi ))


  let lean-mass-change 0
  let e-diff ( lean-mass-synthesized-energy + m-lean-mass ) - lean-mass-mobilized-energy
  ifelse e-diff > 0
  [ set lean-mass-change ( e-diff * DE-pro ) / ED-lean-mass ]
  [ let EC-lean-mass -lean-mass-perc-protein(age) * ED-cpro
    set lean-mass-change e-diff / EC-lean-mass ]


  if who = 0 and debug = 7 and print? = TRUE [
    print word "energy-assimilated:           " energy-assimilated
    print word "energy-mobilized:             " energy-mobilized
    print word "pro-storage-perc:             " pro-storage-perc
    print word "lean-mass-mobilized-energy:   " (energy-mobilized * pro-storage-perc)
    print word "m-lean-mass:                  " m-lean-mass
    print word "lean-mass-assimilated-energy: " (energy-assimilated * pro-storage-perc)
    print word "e-diff:                       " e-diff
    print word "lean-mass-change:             " lean-mass-change
    print " " ]

  ; Storage debug outputs
  if debug = 8 and year = out-year and remainder ticks ticks-per-day = 0 [
    set-current-plot "Lean mass and adipose mobilized"
    set-current-plot-pen "LM"
    plotxy storage-level lean-mass-change
    set-current-plot-pen "fat"
    plotxy storage-level adipose-mobilized
  ]

  ; Update mass and storage level
  set lean-mass lean-mass + lean-mass-change
  set adipose-mass adipose-mass + adipose-synthesized - adipose-mobilized
  set mass lean-mass + adipose-mass + gest-mass
  set storage-level adipose-mass / ( mass - gest-mass )

  ; storage related mortality
  if storage-level <= 0.01 or mass <= 0
  [
    if print? = true [ print word who " died of low storage" ]
    if debug = 8 and year = out-year [ set SL-at-death-list lput ( precision storage-level 3 ) SL-at-death-list ]
    if debug = 10 or debug = "Scen" and year = out-year or year = final-year [
      set age-at-death-temp-list lput (round age) age-at-death-temp-list
      set LRS-temp-list lput LRS LRS-temp-list
      set age-1st-birth-temp-list lput age-1st-birth age-1st-birth-temp-list
      set LPY-temp-list lput litters-per-year LPY-temp-list
    ]
    ; if debug = "Eval" and year = out-year and age > ( t-mature + t-gest + 5 ) [ set p24-LpY lput ( list age litters-per-year ) p24-LpY ]
    ; if debug = "Eval" and year >= out-year and birth-day != 0  [ set p19-surv lput ( list ( floor age ) birth-day ( floor day )) p19-surv ]

    die
  ]


end

;    --------------------------------------------    ;
;    |        END ENERGY BUDGET PROCEDURES      |    ;
;    --------------------------------------------    ;

;--- REPRODUCTION AND MORTALITY ---;

; Check if conception occurs
to conceive

  if age >= t-mature and
  day >= (item 0 t-mating + t-mating-offset) and
  day <= (item 1 ( t-mating ) - ( t-gest + t-nurs + t-0 ))
  [
    let prob-ovul 0.22                                                                          ; Ovulation cycle of 4.5 days in Koskela et al. 1996
    if random-float 1.0 <= prob-ovul [
      ; If so, get pregnant
      set pregnancy-status true
      ifelse lactation-status = true [ set ds-mating ( - t-0 ) ] [ set ds-mating 0 ]            ; If lactating include delayed implantation
      set n-emb item 0 n-emb-range + random (item 1 n-emb-range - item 0 n-emb-range + 1)       ; Implant a random number of embryos
                                                                                                ; set n-emb 5 + random 5 - random 5
      set mass-emb emb-mass-init
      set mass-pl 0

      ; If debugging reproduction collect outputs
      if debug = 5 or debug = 6 [
        set n-litters n-litters + 1
        let conc-out-upd item 0 repro-outs
        set conc-out-upd lput n-emb conc-out-upd
        set repro-outs replace-item 0 repro-outs conc-out-upd
      ]
    ]
  ]

end


; Give birth to offspring
to give-birth

  if lactation-status = true [
    print "overlapping babies"
    wean ] ; if already have dependant offspring, wean them before giving birth

  ; Initialize birth specific parameters
  set pregnancy-status false
  set lactation-status true
  set n-off n-emb
  set n-emb 0
  set mass-off mass-emb / 1000
  set SL-off percent-fat-emb
  set mass-emb 0
  set mass-pl 0
  set lean-mass-off mass-off * ( 1 - SL-off )
  set gest-mass 0
  set ds-mating 0
  set m-preg 0
  set m-preg-no-prod 0

  set ITV-BMR-off-list -ITV-evo "BMR" ITV-BMR ITV-var-BMR

  ; If debugging reproduction, collect outputs
  if debug = 5 or debug = 6 [
    let birth-out-upd item 1 repro-outs
    set birth-out-upd lput n-off birth-out-upd
    set repro-outs replace-item 1 repro-outs birth-out-upd
  ]

;  ; For calibration:
;  if debug = "Cal" and year = out-year [
;    repeat n-off [ set p1-FMAB lput (precision mass-off 4) p1-FMAB ]
;    let BM item 0 p2-BMxLS
;    set BM lput (precision mass-off 4) BM
;    let LS item 1 p2-BMxLS
;    set LS lput n-off LS
;    set p2-BMxLS list BM LS
;
;    set p12-LSb lput n-off p12-LSb
;  ]

;  ; For evaluaion:
;  if debug = "Eval" and year = out-year [
;
    if age-1st-birth = 0 [
      set age-1st-birth floor age
;      set p21-age1stbirth lput age-1st-birth p21-age1stbirth
    ]
;
;    set litters-per-year litters-per-year + 1
;
;    repeat n-off [ set p25-BMemb lput (precision ( mass-off * 1000 ) 1) p25-BMemb ]
;    set p23-LSb lput n-off p23-LSb
;  ]

  ; For scenarios
  if debug = "Scen" and year = out-year or year = final-year
  [
    set neo-BM-temp-list sentence neo-BM-temp-list (n-values n-off [precision ( mass-off * 1000 ) 3])

;    if age-birth != 0 and IBI-year = year [
;     let IBI age - age-birth
;     set IBI-temp-list lput IBI IBI-temp-list
;    ]
;    set age-birth age
;    set IBI-year year
  ]

  ; Run lactation
  lactation

end

; Wean dependant offspring
to wean

  ; If debugging reproduction, collect outputs
  if debug = 5 or debug = 6
  [
    let wean-out-upd item 2 repro-outs
    set wean-out-upd lput n-off wean-out-upd
    set repro-outs replace-item 2 repro-outs wean-out-upd
  ]

;  ; For calibration:
;  if debug = "Cal" and year = out-year [
;    let PM item 0 p11-PMxLS
;    set PM lput (precision mass-off 4) PM
;    let LS item 1 p11-PMxLS
;    set LS lput n-off LS
;    set p11-PMxLS list PM LS
;
;    set p13-LSw lput n-off p13-LSw
;  ]

;  ; For evaluaion:
;  if debug = "Eval" and year = out-year [
;    repeat n-off [ set p26-BMwean lput (precision ( mass-off * 1000 ) 1) p26-BMwean ]
;  ]

   ; For scenarios
  if debug = "Scen" and year = out-year or year = final-year
  [
    set LSW-temp-list lput n-off LSW-temp-list
    set wean-BM-temp-list sentence wean-BM-temp-list (n-values n-off [precision ( mass-off * 1000 ) 3])
    set LRS LRS + n-off
    set litters-per-year litters-per-year + 1
  ]

  ; Create a new turtle for each offspring and initialize parameters
  let mass-pass mass-off
  let SL-pass SL-off
  let LM-pass lean-mass-off
  let age-pass ds-birth
  let ITV-r-pass ITV-repro
  let ITV-g-pass ITV-growth

  while [ n-off > 0 ]
  [
    let ITV-b-pass item 0 ITV-BMR-off-list

    ask patch-here [
      sprout 1
      [

        set mass mass-pass
        set storage-level SL-pass
        set adipose-mass mass * storage-level
        set lean-mass LM-pass
        set age age-pass

        ; Inherit traits with parental trait as mean and respective standard deviation as defined by global parameters and cap at zero
        set ITV-BMR ITV-b-pass
        set ITV-repro -ITV-evo "Repro" ITV-r-pass ITV-var-repro
        set ITV-growth -ITV-evo "Growth" ITV-g-pass ITV-var-growth

        set t-mating-offset -15 + random 30
        set pregnancy-status false
        set lactation-status false

        set stomach-fill []
        if debug = 5 or debug = 6 [ set repro-outs ( list [] [] [] ) ]
        set daily-ingestion []
        set daily-activity []
        set daily-m-tot []
        set daily-m-tot-no-prod []

        ifelse any? resource-cells with [ turtles-here = nobody ]
        [ set home-c one-of resource-cells with [ turtles-here = nobody ] ]
        [ set home-c one-of resource-cells ]

        set color white
        set size 0.7

        ; if debug = "Eval" and year = out-year [ set birth-day floor ( day - ds-birth ) ]

        set age-birth 0
        set LRS 0
        set litters-per-year 0
      ]
    ]

    set ITV-BMR-off-list remove-item 0 ITV-BMR-off-list

    set n-off n-off - 1

  ]

  ; Reset lactation parameters
  set lactation-status false
  set ds-birth 0
  set mass-off 0
  set SL-off 0
  set lean-mass-off 0
  set ITV-BMR-off-list []
  set m-BMR-off 0
  set m-BMR-off 0
  set m-growth-lm-off 0
  set m-lact 0


end

; Execute mortality daily
to mortality-max-age

  ; Based on maximum age
  if age > t-max-age
  [
    if print? = true [ print word who " died of old age" ]
    if debug = 10 or debug = "Scen" and year = out-year or year = final-year [
      set LRS-temp-list lput LRS LRS-temp-list
      set age-at-death-temp-list lput (round age) age-at-death-temp-list
      set age-1st-birth-temp-list lput age-1st-birth age-1st-birth-temp-list
      set LPY-temp-list lput litters-per-year LPY-temp-list
    ]
;    if debug = "Eval" [
;      if year >= out-year and birth-day != 0 [ set p19-surv lput ( list ( floor age ) birth-day ( floor day )) p19-surv ]
;     ; if debug = "Eval" and year = out-year [ set p24-LpY lput ( list age litters-per-year ) p24-LpY ]
;    ]
    die
  ]

end

to mortality-starvation

  ; Based on storage level
  ;; Adult mortality
  let surv-prob 1 / (1 + e ^ (-1 * surv-prob-const * ( storage-level - surv-prob-mid )))
  if ( random-float 1 >= surv-prob )
  [
    if print? = true [ print word who " died of low storage" ]
    if debug = 8 and year = out-year [ set SL-at-death-list lput ( precision storage-level 3 ) SL-at-death-list ]
    if debug = 10 or debug = "Scen" and year = out-year or year = final-year [
      set LRS-temp-list lput LRS LRS-temp-list
      set age-at-death-temp-list lput (round age) age-at-death-temp-list
      set age-1st-birth-temp-list lput age-1st-birth age-1st-birth-temp-list
      set LPY-temp-list lput litters-per-year LPY-temp-list
    ]
;    if debug = "Cal" and year = out-year [ set p15-BF lput ( precision storage-level 3 ) p15-BF ]
;    if debug = "Eval" [
;      if year >= out-year and birth-day != 0 [ set p19-surv lput ( list ( floor age ) birth-day ( floor day )) p19-surv ]
;      ;if debug = "Eval" and year = out-year and age > ( t-mature + t-gest + 5 ) [ set p24-LpY lput ( list age litters-per-year ) p24-LpY ]
;    ]

    die
  ]

  ;; Abortions
  if pregnancy-status = true
  [
    ; Estimate probability of abortion
    let surv-embryo-prob 1 / ( 1 + e ^ (( -1 * surv-prob-const * ( 1 + ( 1 - surv-mod-embryo ))) * ( storage-level - ( surv-prob-mid * surv-mod-embryo ))))
    ; Check survival for each embryo
    let n-emb-init n-emb
    let prob-emb-list []
    repeat n-emb [
      let prob-emb random-float 1
      if prob-emb >= surv-embryo-prob [
        set n-emb n-emb - 1
        set prob-emb-list lput prob-emb prob-emb-list
      ]
    ]

    set gest-mass n-emb * mass-emb

    if debug = 10 and print? = true [ if who = 0 [ if pregnancy-status = true [
      print "Embryo mortality: "
      print word "surv-embryo-prob:            " surv-embryo-prob
      print word "prob-emb-list:               " prob-emb-list
      print word "n-emb-init:                  " n-emb-init
      print word "n-emb:                       " n-emb
      print " "
    ]]]

    ; If all embryos die, stop being pregnant
    if n-emb = 0 [
      set pregnancy-status false
      set mass-pl 0
      set mass-emb 0
      set ds-mating 0
      set m-preg 0
      set m-preg-no-prod 0

      if debug = 5 or debug = 6 [
        let birth-out-upd item 1 repro-outs
        set birth-out-upd lput n-emb birth-out-upd
        set repro-outs replace-item 1 repro-outs birth-out-upd
        let wean-out-upd item 2 repro-outs
        set wean-out-upd lput 0 wean-out-upd
        set repro-outs replace-item 2 repro-outs wean-out-upd
      ]
    ]
  ]

  ;; Dependant offspring death
  if lactation-status = true
  [
    ; Estimate probability of offspring starvation
    let surv-prob-off 0
    if mass-off >= 0 and lean-mass-off >= 0 and SL-off >= 0 [
      set surv-prob-off 1 / ( 1 + e ^ (( -1 * surv-prob-const * ( 1 + ( 1 - surv-mod-off ))) * ( SL-off - ( surv-prob-mid * surv-mod-off ))))
    ]
    ; Check survival for each offspring
    let n-off-init n-off
    let prob-off-list []
    let pos 0
    let pos-list []
    repeat n-off [
      let prob-off random-float 1
      if prob-off >= surv-prob-off [ ;or mass-off <= 0 or lean-mass-off <= 0 or SL-off <= 0 [
        set n-off n-off - 1

        if debug = 8 and year = out-year [ set SL-at-death-list lput (precision SL-off 3) SL-at-death-list ]
        if debug = 10 or debug = "Scen" and year = out-year or year = final-year [
          set age-at-death-temp-list lput (round ds-birth) age-at-death-temp-list
         ; set prob-off-list lput prob-off prob-off-list
        ]
;        if debug = "Eval" and year = out-year [
;          let birthday floor ( day - ds-birth )
;          set p19-surv lput ( list ( floor ds-birth ) birthday ( floor day )) p19-surv
;        ]

        set pos-list lput pos pos-list
        set pos pos + 1
      ]
    ]

    while [ length pos-list >= 1 ][
      let p item 0 pos-list
      set ITV-BMR-off-list remove-item p ITV-BMR-off-list
      set pos-list remove-item 0 pos-list
      if length pos-list >= 1 [ set pos-list map [ i -> i - 1 ] pos-list ]
    ]

    if debug = 10 and print? = true [ if who = 0 [ if lactation-status = true [
      print "Offspring mortality:         "
      print word "surv-prob-off:               " surv-prob-off
      print word "prob-off-list:               " prob-off-list
      print word "n-off-init:                  " n-off-init
      print word "n-off:                       " n-off
      print word "mass-off:                    " mass-off
      print word "SL-off:                      " SL-off
      print word "lean-mass-off:               " lean-mass-off
      print word "m-BMR-off:                   " m-BMR-off
      print word "m-growth-lm-off:             " m-growth-lm-off
      print " "
    ]]]

    ; If all offspring die, stop lactating
    if n-off = 0 [
      set lactation-status false
      set ds-birth 0
      set mass-off 0
      set SL-off 0
      set lean-mass-off 0
      set ITV-BMR-off-list []
      set m-BMR-off 0
      set m-growth-lm-off 0
      set m-lact 0
      if debug = 5 or debug = 6 [
        let wean-out-upd item 2 repro-outs
        set wean-out-upd lput 0 wean-out-upd
        set repro-outs replace-item 2 repro-outs wean-out-upd
      ]
    ]
  ]

end


to mortality-overwinter

  ; Overwinter mortality occurs once per year when skipping
  let yr-overwinter-mortality 1 - ( item 0 winter-surv + random-float ( item 1 winter-surv - item 0 winter-surv ))
  if debug = 10 and print? = true [ print word "Overwinter mortality percent:     " ( yr-overwinter-mortality * 100 ) ]

  ask n-of ( round ( yr-overwinter-mortality * count turtles )) turtles
  [
    if print? = true [ print word who " died of overwinter mortality" ]
    if debug = 10 or debug = "Scen" and year = out-year or year = final-year  [
      set LRS-temp-list lput LRS LRS-temp-list
      set age-at-death-temp-list lput (round age) age-at-death-temp-list
      set age-1st-birth-temp-list lput age-1st-birth age-1st-birth-temp-list
      set LPY-temp-list lput litters-per-year LPY-temp-list
    ]
;    if debug = "Eval" and year >= out-year and birth-day != 0 [ set p19-surv lput ( list ( floor age ) birth-day ( floor day )) p19-surv ]

    die
  ]

end

; Update three trait values when called
; Inputs are: a - trait name ("BMR", "Growth", or "Repro"), b - mean value, c - variance
to-report -ITV-evo [a b c]
  ifelse a = "BMR"
  [
    let ITV-b-list []
    repeat n-off [
      ; Assign ITV in BMR and cap at zero
      let ITV-b-tmp random-normal b ( c / 100 )
      while [ ITV-b-tmp < 0 ] [ set ITV-b-tmp random-normal b ( c / 100 ) ]
      set ITV-b-list lput ITV-b-tmp ITV-b-list
    ]
    report ITV-b-list
  ]
  [
    let ITV-rg random-normal b ( c / 100 )
    while [ ITV-rg < 0 ] [ set ITV-rg random-normal b ( c / 100 ) ]
    report ITV-rg
  ]

end



;---  OUTPUTS ---;
to update-records

  set m-tot m-HIF + m-BMR + m-move + m-preg + m-lact + m-lean-mass
  if length daily-m-tot = ticks-per-day [ set daily-m-tot remove-item 0 daily-m-tot ]
  set daily-m-tot lput m-tot daily-m-tot
  if length daily-m-tot > ticks-per-day [ print word who "'s daily-m-tot list > ticks-per-day" ]

  set m-tot-no-prod m-HIF + m-BMR + m-move + m-preg-no-prod + m-lact-no-prod + m-lean-mass-no-prod
  if length daily-m-tot-no-prod = ticks-per-day [ set daily-m-tot-no-prod remove-item 0 daily-m-tot-no-prod ]
  set daily-m-tot-no-prod lput m-tot-no-prod daily-m-tot-no-prod
  if length daily-m-tot-no-prod > ticks-per-day [ print word who "'s daily-m-tot-no-prod list > ticks-per-day" ]

  ; For debugging FMR
  if debug = 9 or debug = "Scen" [

    set daily-m-HIF daily-m-HIF + m-HIF
    set daily-m-BMR daily-m-BMR + m-BMR
    set daily-m-move daily-m-move + m-move
    set daily-m-preg daily-m-preg + m-preg
    set daily-m-lact daily-m-lact + m-lact
    set daily-m-lean-mass daily-m-lean-mass + m-lean-mass
    set daily-m-preg-no-prod daily-m-preg-no-prod + m-preg-no-prod
    set daily-m-lact-no-prod daily-m-lact-no-prod + m-lact-no-prod
    set daily-m-lean-mass-no-prod daily-m-lean-mass-no-prod + m-lean-mass-no-prod
  ]


  ;; Calibration outputs

  if debug = "Cal" and year = out-year and remainder ticks ticks-per-day = 0 [

    ;; Pattern 1. Fetal mass at birth
    ; p1-FMAB - updated at birth

    ;; Pattern 2. Birth mass by litter size
    ; p2-BMxLS - updated at birth

    ;; Pattern 3. Total body mass by age &
    ;; Pattern 4. Lean mass by age
    if remainder ticks (ticks-per-day * 30) = 0 [
      let BM item 0 p34-BMLMxA
      set BM lput (precision mass 4) BM
      if lactation-status = true [set BM lput (precision mass-off 4) BM]

      let LM item 1 p34-BMLMxA
      set LM lput (precision lean-mass 4) LM
      if lactation-status = true [set LM lput (precision lean-mass-off 4) LM]

      let A item 2 p34-BMLMxA
      set A lput (round age) A
      if lactation-status = true [set A lput (round ds-birth) A]

      set p34-BMLMxA (list BM LM A)
    ]

    if lactation-status = true and round ds-birth <= 15 [

      ;; Pattern 5. Lactating mother mass by pup age &
      ;; Pattern 6. Lactating mother food intake by pup age &
      ;; Pattern 7. Total litter mass by pup age
      let dsb item 0 p567-dsbxMMFILM
      set dsb lput round ds-birth dsb
      let MM item 1 p567-dsbxMMFILM
      set MM lput ( precision mass 4 ) MM
      let FI item 2 p567-dsbxMMFILM
      set FI lput ( round ( sum daily-ingestion / ( ED-food * AE-food ))) FI   ; daily ingestion = MEI
      let LitM item 3 p567-dsbxMMFILM
      set LitM lput ( precision ( n-off * mass-off ) 4 ) LitM
      set p567-dsbxMMFILM ( list dsb MM FI LitM )

      ;; Pattern 8. Mother peak food intake by litter size
      ;; Pattern 9. Mother peak energy use by litter size
      ;; Pattern 10. Mother peak milk transfer by litter size
      if round ds-birth = 15 [
        let LS item 0 p8910-LSxpFIEUMEO
        set LS lput n-off LS
        let FIp item 1 p8910-LSxpFIEUMEO
        set FIp lput ( round ( sum daily-ingestion / ( ED-food * AE-food ))) FIp
        let EUp item 2 p8910-LSxpFIEUMEO
        set EUp lput ( round ( sum daily-m-tot-no-prod ) / 1000 ) EUp
        let MEOp item 3 p8910-LSxpFIEUMEO
        set MEOp lput ( precision (( sum daily-ingestion / 1000) - ( sum daily-m-tot-no-prod  / 1000)) 4 ) MEOp

        set p8910-LSxpFIEUMEO ( list LS FIp EUp MEOp )
      ]
    ]

    ;; Pattern 11. Pup mass at weaning by litter size
    ; p11-PMxLS - updated at weaning

    ;; Pattern 12. Litter size at birth
    ; p12-LSb - updated at birth

    ;; Pattern 13. Litter size at weaning
    ; p13-LSw - updated at weaning

    ;; Pattern 14. Average / range of body fat %
    if remainder ticks (ticks-per-day * 30) = 0 [
      set p14-BF lput (precision storage-level 3) p14-BF
      if lactation-status = true [ repeat n-off [ set p14-BF lput ( precision SL-off 3 ) p14-BF ]]
    ]

    ;; Pattern 15. Body fat % of living animals
    ; p15-BF - updated in mortality - starvation

    ;; Pattern 16. Field metabolic rate by body mass
    if remainder ticks (ticks-per-day * 30) = 0 and lactation-status = false and length daily-m-tot = ticks-per-day [
      let BodM item 0 p16-FMRxBM
      set BodM lput ( precision mass 4 ) BodM

      let FMR item 1 p16-FMRxBM
      set FMR lput ( round ( sum daily-m-tot-no-prod )) FMR

      set p16-FMRxBM ( list BodM FMR )
    ]
  ]

  ;; Evaluation outputs
  if debug = "Eval" and year = out-year and remainder ticks ticks-per-day = 0 [

    if remainder ticks (ticks-per-day * 7) = 0 [
      let state 0
      if pregnancy-status = false and lactation-status = false and age > t-mature [ set state 2 ]
      if pregnancy-status = false and lactation-status = true [ set state 4 ]
      if pregnancy-status = true and lactation-status = true [ set state 5 ]
      if pregnancy-status = true and lactation-status = false [ set state 3 ]
      if age < t-mature [ set state 1 ]

      ;; Pattern 17. State-dependent field metabolic rate
      ; only 2 states: non-reproducing and lactating
      ; all data points from animals with a mass greater than 20.0 and less than 37.1 g
      ; in J
      let state17 item 0 p18-IRxState
      set state17 lput state state17
      let FMR item 1 p17-FMRxState
      set FMR lput ( round ( sum daily-m-tot-no-prod )) FMR
      set p17-FMRxState ( list state17 FMR )

      ;; Pattern 18. State-dependent food consumption
      ; in g
      let state18 item 0 p18-IRxState
      set state18 lput state state18
      let IR item 1 p18-IRxState
      set IR lput ( round (( sum daily-ingestion ) / ( ED-food * AE-food ))) IR
      set p18-IRxState ( list state18 IR )

      ;; Pattern 20. Adult body mass
      ; Age is based on age of maturity in PanTHERIA database
      if age > 40.91 [ set p20-BMadult lput ( precision ( mass  * 1000 ) 1 ) p20-BMadult ]

      ;; Pattern 22. Mass-specific basal metabolic rate
      ; Mass is based on mass used for BMR in PanTHERIA database +- 5 grams to give some spread
      if mass * 1000 >= 21.9 - 5 and mass * 1000 <= 21.9 + 5 [ set p22-BMRadult lput (list ( round m-BMR ) ( precision ( mass  * 1000 ) 1 )) p22-BMRadult ]
    ]


    ;; Pattern 19. Survival rates
    ; p19-surv - updated at birth and death

    ;; Pattern 21. Age at first birth
    ; p21-age1stbirth - updated at birth

    ;; Pattern 23. Litter size
    ; p23-LSb - updated at birth

    ;; Pattern 24. Litters per year
    ; p24-LpY - updated in yearly tasks and death

    ;; Pattern 25. Neonate body mass
    ; p25-BMemb - updated at birth

    ;; Pattern 26. Weaning body mass
    ; p26-BMwean - updated at weaning

    ;; Pattern 27. Population density
    ; p27-dens - updated daily in daily tasks using dens-list then saved as p27-dens at end of simulation in go

  ]

end

; Update plots
to update-monitors


end


to update-scenarios
; update environmentals

  if debug = "Scen" [

    if day = item 1 scenario-res-levs and year = item 0 scenario-res-levs
    [
      let res-delta ( item ( 1 + model ) scenario-res-levs ) * max-resources-base

      set max-resources max-resources + res-delta
      if max-resources < 0 [ set max-resources 0 ]

      ask resource-cells [
        if res-delta < 0 [ set res-delta 0 ]
        set resource-level resource-level + res-delta
        if resource-level > max-resources [ set resource-level max-resources ]
        if resource-level < 0 [ set resource-level 0 ]
      ]

      if day = 91 [
        set max-resources ( item ( 1 + model ) scenario-res-levs ) * max-resources-base
        ask resource-cells [ set resource-level max-resources ]
        ; print "update"
      ]

      set scenario-res-levs csv:from-row file-read-line

      ; print max-resources
      ; print scenario-res-levs
    ]
  ]


  ; UPDATE OUTPUTS
  ; per month record: Average ITV-BMR, ITV-repro, ITV-growth, body mass, IBI, and population abundance

  let days-rec-abun [121 152 182 213 244 274]

  if year = out-year or year = final-year [
    ; Traits: body mass, activity patterns, body condition, litter size at weaning, neonate body mass, weaning body mass, IBI, lifetime reproductive success, age at death

    if member? day days-rec-abun [
      ;      set days item 0 activity-list
      ;      let Acts item 1 activity-list
      ;      let Acts-day [sum daily-activity / 48] of turtles with [age > t-mature]
      ;      set Acts-day map [ x -> precision x 3 ] Acts-day
      ;      set days-day n-values (length Acts-day) [day]
      ;      set activity-list list (sentence days days-day) (sentence Acts Acts-day)

      ; Body mass of adults
      let days item 0 BM-list
      let BMs item 1 BM-list
      let BMs-day [precision (mass * 1000) 3] of turtles with [age > t-mature]
      let days-day n-values (length BMs-day) [day]
      set BM-list list (sentence days days-day) (sentence BMs BMs-day)

      ; Body condition (percent fat mass) of adults
      set days item 0 BC-list
      let BCs item 1 BC-list
      let BCs-day [precision (storage-level) 3] of turtles with [age > t-mature]
      set days-day n-values (length BCs-day) [day]
      set BC-list list (sentence days days-day) (sentence BCs BCs-day)

      ; Total energy use of adults
      set days item 0 FMR-list
      let FMRs item 1 FMR-list
      let FMRs-day [round (sum daily-m-tot-no-prod)] of turtles with [age > t-mature]
      set days-day n-values (length FMRs-day) [day]
      set FMR-list list (sentence days days-day) (sentence FMRs FMRs-day)

      ; Locomotive costs of adults
      set days item 0 loco-list
      let locos item 1 loco-list
      let locos-day [round daily-m-move] of turtles with [age > t-mature]
      set days-day n-values (length locos-day) [day]
      set loco-list list (sentence days days-day) (sentence locos locos-day)

      ; Total reproductive costs of adults
      set days item 0 repro-list
      let repros item 1 repro-list
      let repros-day [round (daily-m-lact + daily-m-preg)] of turtles with [age > t-mature]
      set days-day n-values (length repros-day) [day]
      set repro-list list (sentence days days-day) (sentence repros repros-day)

      ; Lean mass growth costs of juveniles
      set days item 0 growth-list
      let growths item 1 growth-list
      let growths-day [round daily-m-lean-mass] of turtles with [age <= t-mature + 10]
      set days-day n-values (length growths-day) [day]
      set growth-list list (sentence days days-day) (sentence growths growths-day)
    ]

    ; Litter size (N pups) at weaning
    if length LSW-temp-list > 0 [
      let days item 0 LSW-list
      let LSWs item 1 LSW-list
      let days-day n-values (length LSW-temp-list) [day]
      set LSW-list list (sentence days days-day) (sentence LSWs LSW-temp-list)
      set LSW-temp-list []
    ]

    if length neo-BM-temp-list > 0 [
      let days item 0 neo-BM-list
      let neo-BMs item 1 neo-BM-list
      let days-day n-values (length neo-BM-temp-list) [day]
      set neo-BM-list list (sentence days days-day) (sentence neo-BMs neo-BM-temp-list)
      set neo-BM-temp-list []
    ]

    if length wean-BM-temp-list > 0 [
      let days item 0 wean-BM-list
      let wean-BMs item 1 wean-BM-list
      let days-day n-values (length wean-BM-temp-list) [day]
      set wean-BM-list list (sentence days days-day) (sentence wean-BMs wean-BM-temp-list)
      set wean-BM-temp-list []
    ]
;
;    if length IBI-temp-list > 0 [
;      let days item 0 IBI-list
;      let IBIs item 1 IBI-list
;      let days-day n-values (length IBI-temp-list) [day]
;      set IBI-list list (sentence days days-day) (sentence IBIs IBI-temp-list)
;      set IBI-temp-list []
;    ]


    if length age-at-death-temp-list > 0 [
      let days item 0 age-at-death-list
      let age-at-deaths item 1 age-at-death-list
      let days-day n-values (length age-at-death-temp-list) [day]
      set age-at-death-list list (sentence days days-day) (sentence age-at-deaths age-at-death-temp-list)
      set age-at-death-temp-list []
    ]

    if day = year-end [
      let LRSs [LRS] of turtles
      set LRS-temp-list sentence LRS-temp-list LRSs

      let first-bs [age-1st-birth] of turtles
      set age-1st-birth-temp-list sentence age-1st-birth-temp-list first-bs

      let LPYs [litters-per-year] of turtles
      set LPY-temp-list sentence LPY-temp-list LPYs
    ]

    if length LRS-temp-list > 0 [
      let days item 0 LRS-list
      let LRSss item 1 LRS-list
      let days-day n-values (length LRS-temp-list) [day]
      set LRS-list list (sentence days days-day) (sentence LRSss LRS-temp-list)
      set LRS-temp-list []
    ]

    if length age-1st-birth-temp-list > 0 [
      let days item 0 age-1st-birth-list
      let first-b item 1 age-1st-birth-list
      let days-day n-values (length age-1st-birth-temp-list) [day]
      set age-1st-birth-list list (sentence days days-day) (sentence first-b age-1st-birth-temp-list)
      set age-1st-birth-temp-list []
    ]


    if length LPY-temp-list > 0 [
      let days item 0 LPY-list
      let LPY item 1 LPY-list
      let days-day n-values (length LPY-temp-list) [day]
      set LPY-list list (sentence days days-day) (sentence LPY LPY-temp-list)
      set LPY-temp-list []
    ]

  ]


  if year >= out-year and member? day days-rec-abun [

    let pop-abun count turtles
    let off-abun sum [n-off] of turtles
    set abun-outs-list lput (list day year pop-abun off-abun) abun-outs-list

  ]

  if year = out-year + 1 and BM-list-base = 0 [

    set BM-list-base BM-list
  ;  set activity-list-base activity-list
    set BC-list-base BC-list
    set LSW-list-base LSW-list
    set neo-BM-list-base neo-BM-list
    set wean-BM-list-base wean-BM-list
  ;  set IBI-list-base IBI-list
    set LRS-list-base LRS-list
    set age-at-death-list-base age-at-death-list
    set age-1st-birth-list-base age-1st-birth-list
    set LPY-list-base LPY-list

    set FMR-list-base FMR-list
    set loco-list-base loco-list
    set repro-list-base repro-list
    set growth-list-base growth-list

    set BM-list [[][]]
    set activity-list [[][]]
    set BC-list [[][]]
    set LSW-list [[][]]
    set LSW-temp-list []
    set neo-BM-list [[][]]
    set neo-BM-temp-list []
    set wean-BM-list [[][]]
    set wean-BM-temp-list []
   ; set IBI-list [[][]]
   ; set IBI-temp-list []
    set LRS-list [[][]]
    set LRS-temp-list []
    set age-at-death-list [[][]]
    set age-at-death-temp-list []
    set age-1st-birth-list [[][]]
    set age-1st-birth-temp-list []
    set LPY-list [[][]]
    set LPY-temp-list []

    set FMR-list [[][]]
    set loco-list [[][]]
    set repro-list [[][]]
    set growth-list [[][]]

  ]


end
@#$#@#$#@
GRAPHICS-WINDOW
211
61
379
470
-1
-1
8.0
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
19
0
49
0
0
1
ticks
30.0

BUTTON
14
10
77
43
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
77
10
140
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
140
10
203
43
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
0
72
172
105
perc-resource-cells
perc-resource-cells
0
1
0.75
0.05
1
NIL
HORIZONTAL

SLIDER
0
107
172
140
n-animals
n-animals
1
1000
100.0
1
1
NIL
HORIZONTAL

TEXTBOX
0
53
150
71
Setup parameters\n
13
0.0
1

TEXTBOX
0
308
150
326
ITV parameters\n
13
0.0
1

MONITOR
211
10
268
55
Day
day
17
1
11

MONITOR
269
10
326
55
Year
year
17
1
11

CHOOSER
467
10
559
55
debug
debug
0 1 2 3 4 5 6 7 8 9 10 "Cal" "Dens" "Eval" "Scen"
14

MONITOR
327
10
392
55
N pregnant
count turtles with [ pregnancy-status = true ]
17
1
11

SWITCH
559
10
649
43
print?
print?
1
1
-1000

MONITOR
393
10
465
55
N lactating
count turtles with [ lactation-status = true ]
17
1
11

SLIDER
0
142
172
175
max-resources-base
max-resources-base
0.1
250
140.0
1
1
NIL
HORIZONTAL

SLIDER
0
175
172
208
r-growth-ts
r-growth-ts
0
2
0.011
0.01
1
NIL
HORIZONTAL

SLIDER
1
444
173
477
growth-lm-prob-const
growth-lm-prob-const
0
100
29.99
1
1
NIL
HORIZONTAL

SLIDER
1
476
173
509
growth-lm-prob-mid
growth-lm-prob-mid
-1
1
0.05
0.1
1
NIL
HORIZONTAL

TEXTBOX
0
426
150
444
Calibrated parameters
12
0.0
1

SLIDER
1
512
173
545
preg-prob-const
preg-prob-const
0
100
29.63
1
1
NIL
HORIZONTAL

SLIDER
1
544
173
577
preg-prob-mid
preg-prob-mid
0
1
0.07
0.1
1
NIL
HORIZONTAL

SLIDER
1
581
173
614
lact-prob-const
lact-prob-const
0
100
38.29
1
1
NIL
HORIZONTAL

SLIDER
1
613
173
646
lact-prob-mid
lact-prob-mid
0
1
0.13
0.1
1
NIL
HORIZONTAL

SLIDER
1
649
173
682
surv-prob-const
surv-prob-const
0
200
83.8
1
1
NIL
HORIZONTAL

SLIDER
1
681
173
714
surv-prob-mid
surv-prob-mid
0
1
0.03
0.05
1
NIL
HORIZONTAL

PLOT
677
134
1359
293
Number of voles
Ticks
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Adults" 1.0 0 -16777216 true "" "if remainder ticks 48 = 0 [ plot count turtles ]"
"Offspring" 1.0 0 -3026479 true "" "if remainder ticks 48 = 0 [ plot sum [ n-off ] of turtles with [ n-off > 0 ]]"

PLOT
212
504
1257
624
Food eaten (IR) and average stores
Ticks
NIL
0.0
10.0
0.0
250.0
true
true
"" ""
PENS
"IR" 1.0 2 -14439633 true "" ";if ticks > 1 [ plotxy ticks mean [IR-real] of turtles ]"
"Stores" 1.0 0 -16777216 true "" ";if ticks > 1 [ if remainder ticks 48 = 0 [ plotxy ticks mean [storage-level] of turtles ]] ; if ticks > 1 [ plotxy ticks mean [storage-level] of turtles ]"
"IR - Lact" 1.0 2 -7858858 true "" ";if ticks > 1 [ if any? turtles with [ lactation-status = true ][ plotxy ticks mean [IR-real] of turtles with [ lactation-status = true ] * 5 ]]"
"food" 1.0 0 -7500403 true "" "if ticks > 1 [ plotxy ticks max-resources ]"
"food-debt" 1.0 0 -2674135 true "" ";if ticks > 1 [ plotxy ticks mean [ food-debt ] of turtles ]"
"mass" 1.0 0 -955883 true "" ";if ticks > 1 [ plotxy ticks ( mean [mass] of turtles)] "
"max-res" 1.0 0 -6459832 true "" ";if ticks > 1 [ plotxy ticks max-resources ]"
"move" 1.0 0 -1184463 true "" ";if ticks > 1 [ plotxy ticks (count turtles with [move-speed > 0] / count turtles)] "
"pen-8" 1.0 0 -10899396 true "" ";if ticks > 1 [ if remainder ticks 48 = 0 [ plotxy ticks ( mean [ITV-BMR] of turtles )]]"
"pen-9" 1.0 0 -13840069 true "" ";if ticks > 1 [ if remainder ticks 48 = 0 [ plotxy ticks ( mean [sum stomach-fill / stomach-fill-max] of turtles )]]"
"tot-res" 1.0 0 -14835848 true "" "if ticks > 1 [ if remainder ticks 48 = 0 [ plotxy ticks ( mean [resource-level] of patches with [resource-level > 0] )]]"
"pen-11" 1.0 0 -11221820 true "" ""

PLOT
1180
1063
1380
1213
Peak food consumption by litter size
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
"default" 1.0 2 -16777216 true "" ""

PLOT
1007
1064
1180
1214
Peak milk energy output by litter size
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
"default" 1.0 2 -16777216 true "" ""

INPUTBOX
648
10
708
70
out-year
23.0
1
0
Number

SWITCH
0
241
173
274
overwinter-skip?
overwinter-skip?
0
1
-1000

INPUTBOX
2
713
84
773
surv-mod-embryo
0.6
1
0
Number

INPUTBOX
83
713
173
773
surv-mod-off
0.11
1
0
Number

INPUTBOX
128
332
192
392
ITV-var-growth
0.0
1
0
Number

INPUTBOX
0
332
65
392
ITV-var-BMR
0.0
1
0
Number

INPUTBOX
64
332
129
392
ITV-var-repro
0.0
1
0
Number

SLIDER
0
207
172
240
fragmentation-level
fragmentation-level
0.5
0.999
0.9
0.001
1
NIL
HORIZONTAL

SWITCH
682
81
820
114
home-ranging?
home-ranging?
0
1
-1000

SWITCH
823
81
962
114
pcolor-update?
pcolor-update?
1
1
-1000

PLOT
678
298
1296
448
Local density per hectare
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
"0" 1.0 0 -13345367 true "" "if remainder ticks 48 = 0 and ticks > 1 and debug = \"Dens\" [ plot 4.4 ]"
"25" 1.0 0 -13840069 true "" "if remainder ticks 48 = 0 and ticks > 1 and debug = \"Dens\" [ plot 8.9 ]"
"50" 1.0 0 -1184463 true "" "if remainder ticks 48 = 0 and ticks > 1 and debug = \"Dens\" [ plot 14.2 ]"
"75" 1.0 0 -955883 true "" "if remainder ticks 48 = 0 and ticks > 1 and debug = \"Dens\" [ plot 21.6 ]"
"100" 1.0 0 -2674135 true "" "if remainder ticks 48 = 0 and ticks > 1 and debug = \"Dens\" [ plot 41.4 ]"
"Density" 1.0 0 -16777216 true "" "if debug = \"Dens\" and remainder ticks 48 = 0 and ticks > 1 [ plot count turtles-on dens-cells ]"

INPUTBOX
393
111
548
171
sites
9.0
1
0
Number

INPUTBOX
393
170
548
230
experiments
2.0
1
0
Number

TEXTBOX
396
90
546
108
Scenario inputs: \n
12
0.0
1

MONITOR
393
298
488
343
NIL
max-resources
17
1
11

INPUTBOX
393
229
548
289
model
1.0
1
0
Number

INPUTBOX
710
10
865
70
final-year
99.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="NDVI_scenarios" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>BM-list-base</metric>
    <metric>BC-list-base</metric>
    <metric>LSW-list-base</metric>
    <metric>neo-BM-list-base</metric>
    <metric>wean-BM-list-base</metric>
    <metric>LRS-list-base</metric>
    <metric>age-at-death-list-base</metric>
    <metric>age-1st-birth-list-base</metric>
    <metric>LPY-list-base</metric>
    <metric>FMR-list-base</metric>
    <metric>loco-list-base</metric>
    <metric>repro-list-base</metric>
    <metric>growth-list-base</metric>
    <metric>BM-list</metric>
    <metric>BC-list</metric>
    <metric>LSW-list</metric>
    <metric>neo-BM-list</metric>
    <metric>wean-BM-list</metric>
    <metric>LRS-list</metric>
    <metric>age-at-death-list</metric>
    <metric>age-1st-birth-list</metric>
    <metric>LPY-list</metric>
    <metric>abun-outs-list</metric>
    <metric>FMR-list</metric>
    <metric>loco-list</metric>
    <metric>repro-list</metric>
    <metric>growth-list</metric>
    <enumeratedValueSet variable="pcolor-update?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ITV-var-BMR">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="overwinter-skip?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-animals">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="&quot;Scen&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ITV-var-repro">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ITV-var-growth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="home-ranging?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-resources-base">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-growth-ts">
      <value value="0.011"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-resource-cells">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation-level">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="out-year">
      <value value="23"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="final-year">
      <value value="99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sites" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="experiments">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
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
