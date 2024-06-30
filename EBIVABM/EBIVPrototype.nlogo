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

extensions [ profiler ]

globals[

  day
  year
  year-start
  year-end

  max-resources
  patch-size-m              ; Patch size [m]

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

  ; Storage
  ED-fat	                  ; Fat energy density [J kg-1]
  gamma-mobilize            ; Fuel partitioning constant [Unitless]
  percent-water-adi         ; Water percent in adipose tissue [%]
  SL-max                    ; Maximum adipose storage level [%]
  SL-list                   ; List of storage level values [%]
  SL-at-death-list          ; List of storage level values when animals died [%]

  ; Mortality
  t-max-age                 ; Maximum age [days]
  age-at-death-list         ; List of ages where animals died [days]
  winter-surv               ; Overwinter survival probability [%]

]

patches-own[

  resource-patch            ; Whether patch contains food resources [Bool]
  resource-level            ; Food resource level of a patch [g]
  last-eaten                ; Timestep when the food patch was last eaten from [N]

]

turtles-own[

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
  mass-emb-list             ; List of embryo masses [g]
  mass-pl-list              ; List of placental masses [g]
  gest-mass                 ; Total mass of embryos and placentae [kg]
  m-preg                    ; Total cost of pregnancy [J 30min-1]
  m-preg-no-prod            ; Heat expended by pregnancy (no tissue production energy) [J 30min-1]

  lactation-status          ; Lactation status [true or false]
  n-off                     ; Number of offspring conceived [N]
  ds-birth                  ; Days since giving birth [days]
  mass-off-list             ; Mass of dependent offspring [kg]
  SL-off-list               ; Storage level of offspring; adipose stores as a percentage of mass [%]
  lean-mass-off-list        ; Lean mass of dependent offspring [kg]
  ITV-BMR-off-list          ; ITV value for BMR for each dependent offspring [%]
  m-BMR-off-list            ; Offspring maintenance costs [J 30min-1]
  m-growth-lm-off-list      ; Offspring costs of growth [J 30min-1]
  m-lact                    ; Total cost of lactation [J 30min-1]
  m-lact-no-prod            ; Heat expended by lactation (no tissue production energy) [J 30min-1]

  n-litters                 ; Count of how many litters of pups are conceived per season [N]
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
  protein-storage-perc

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

]

to setup

  ; Start by setting up landcape, globals, and creating animals
  clear-all
  reset-ticks
  setup-landscape
  setup-globals
  create-animals

end

to setup-landscape

  ; Initialize base properties
  ask patches [
    set pcolor brown
    set resource-patch false
  ]

  ; For now select food patches randomly as a percentage of total number of patches
  ask n-of (count patches * perc-resource-patches) patches [
    set max-resources max-resources-base
    set resource-level max-resources
    set pcolor scale-color green resource-level ( max-resources-base + max-resources-base * 0.5 ) 0
    set resource-patch true
  ]

end

to setup-globals

  ; Year settings
  set year 1
  ifelse overwinter-skip? = false [
    set year-start 1
    set year-end 365
    set day year-start
  ] [
    set year-start 116
    set year-end 278
    set day year-start
  ]

  set patch-size-m 500               ; Assuming 500m patches

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
  set intercept-pcot 4.7             ; Derived from data in Chappell et al. 2004, Dlugosz et al. 2009, & Rezende et al. 2006
  set slope-pcot 0.63                ; Derived from data in Chappell et al. 2004, Dlugosz et al. 2009, & Rezende et al. 2006
  set intercept-icot 10.6            ; From relationship in Pontzer 2016
  set slope-icot -0.29               ; From relationship in Pontzer 2016

  ;; Reproduction
  ; Pregnacy
  set t-mating [121 273]             ; Oksanen et al. 2001, Koivula et al. 2003, & Nyholm & Meurling 1975
  set t-mature [30 45]               ; Oksanen et al. 2001, Bujalska 1983, & Buchalczyk 1970
  set t-gest 20                      ; Koivula et al. 2003, Bujalska & Ryszkowski 1966, & Kaczmarski 1966
  set t-0 4                          ; Brambell and Rowlands 1936, Bujalska & Ryszkowski 1966
  set n-emb-range [2 9]              ; Brambell and Rowlands 1936, Nerquaye-Tetteh and Clarke 1990, Nyholm and Meurling 1979, & Wiger 1979
  set	emb-growth-c	0.112	           ; Calculated using data in Ożdżeński and Mystkowska 1976 and following Ricklefs 2010
  set	emb-mass-init	0.000000069      ; Calculated using data in Ożdżeński and Mystkowska 1976 and following Ricklefs 2010
  set	emb-mass-inf	19.957	         ; Calculated using data in Ożdżeński and Mystkowska 1976 and following Ricklefs 2010
  set percent-fat-emb 0.038          ; Fedyk 1974 & Sawicka-Kapusta 1974
  set percent-pro-emb 0.102          ; Fedyk 1974 & Sawicka-Kapusta 1974

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
  set	ED-pro	23361.3	* 1000         ; From Fedyk 1974
  set	DE-fat	0.735	                 ; From Pullar and Webster 1977
  set	DE-pro	0.444	                 ; From Pullar and Webster 1977
  set	growth-lm-inf	0.0264           ; Estimated using data from Fedyk 1974, Hansson 1991, Rudolf et al. 2017, Sawicka-Kapusta 1974, Balčiauskienė 2007, & Gębczyński 1975
  set	growth-lm-k	0.0964 	           ; Estimated using data from Fedyk 1974, Hansson 1991, Rudolf et al. 2017, Sawicka-Kapusta 1974, Balčiauskienė 2007, & Gębczyński 1975

  ;; Storage
  set	ED-fat 38699.5	* 1000         ; Fedyk 1974
  set gamma-mobilize 0.015           ; Belkhou et al. 1991, Dunn et al. 1982, & Cherel et al. 1992
  set percent-water-adi 0.119        ; DiGirolamo and Owens 1976 & Reinoso et al. 1997
  set SL-max 0.398                   ; Estimated as the maximum mean value in Fedyk 1974 plus one standard deviation adjusted for water content
  set SL-list []
  set SL-at-death-list []
  if debug = 8 [ file-open "C:\\Users\\cara\\Dropbox\\PC\\Documents\\PostdocBioMoveRepos\\MetabolicVariation\\EBIVRProject\\Data\\Verification\\Storage\\StorageLevelsState.txt" ]

  ;; Mortality
  set t-max-age 620                  ; Buchalczyk 1970, Rudolf et al. 2017, Sawicka-Kapusta 1974, Balčiauskienė 2007
  if debug = 10 [ set age-at-death-list []]
  set winter-surv [0.20 0.632]       ; Koskela, 1998; Oksanen et al. 2001; Kallio et al. 2007; Boratyński et al. 2010; Haapakoski et al 2012

end


to create-animals

  ; Create initial animals based on n-animals slider value and initialize state variables at default values
  create-turtles n-animals [
    setxy random-xcor random-ycor
    set color black

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
    set mass-emb-list []
    set mass-pl-list []
    set lactation-status false
    set mass-off-list []
    set SL-off-list []
    set lean-mass-off-list []
    set ITV-BMR-off-list []
    set m-BMR-off-list []
    set m-growth-lm-off-list []
    if debug = 5 or debug = 6 [ set repro-outs ( list [] [] [] ) ]
    set daily-activity []
    set daily-ingestion []
    set daily-m-tot []
    set daily-m-tot-no-prod []
  ]

end

to go

  if ( debug = 1 ) and ( ticks > 3 ) [        ; Profiler
    profiler:start
  ]

  if not any? turtles [ stop ]                ; Model stops if all animals die

  if remainder ticks 48 = 0 [ daily-tasks ]   ; Tasks performed once per day or less

  ask turtles [                               ; Animals move and calculate their energy budgets
    move
    energy-budget
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

 ; If incorporating seasonality, update maximum resource values
  if seasonality? = true [
    let year-fract  360 * ( day / 365 )
    let modif -1 * seasonal-var * cos year-fract + 1
    set max-resources modif * max-resources-base
  ]

  if ticks > 1 [
    update-monitors                                                             ; Update plots and monitors

    if debug = 8 and year = out-year [                                            ; If debugging storage, collect storage levels of all animals and dependant offspring
      set SL-list sentence SL-list ([storage-level] of turtles)
      ask turtles [
        ; State specific outputs
        if pregnancy-status = false and lactation-status = false and age > (item 1 t-mature) [ file-print (list (precision storage-level 2) ", " age ", " 3) ]
        if pregnancy-status = true and lactation-status = false [ file-print (list (precision storage-level 2) ", " age ", " 4) ]
        if pregnancy-status = false and lactation-status = true [ file-print (list (precision storage-level 2) ", " age ", " 5) ]
        if pregnancy-status = true and lactation-status = true [ file-print (list (precision storage-level 2) ", " age ", " 6) ]
        if age < (item 1 t-mature) [ file-print (list (precision storage-level 2) ", " age ", " 2) ]
      ]
      if any? turtles with [ lactation-status = true ] [
        ask turtles with [ lactation-status = true ] [
          set SL-list sentence SL-list SL-off-list
          repeat n-off [ file-print (list (precision (mean SL-off-list) 2) ", " (floor ds-birth) ", " 1) ]
        ]
      ]
    ]

    ask turtles [
      ; Starvation and age-related mortality are executed once per day
      mortality-max-age
      mortality-starvation

      ; If debugging FMR, reset monitors
      if debug = 9 [
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

      ; Animals age once per day
      set age age + 1
    ]
  ]

  ; Update day and check if last day of year
  set day day + 1

  ; If so, execute yearly tasks & overwinter mortality, update year, & reset day to first day of year
  if day >= year-end
  [
    yearly-tasks

  ]

end

; Resources replenish once per day
to resources-grow

  let r-growth (ticks - last-eaten) * r-growth-ts
;  print last-eaten
;  print ticks
;  print r-growth
;  print resource-level


  ; Resource patches grow if lower than their maximum value
  if resource-level < max-resources [ set resource-level resource-level + r-growth ]
  ;  print resource-level

  ; Clamp at maximum value
  if resource-level > max-resources [ set resource-level max-resources ]
  ;  print resource-level

  set pcolor scale-color green resource-level ( max-resources-base + max-resources-base * 0.5 ) 0

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


  ; Calculate overwinter mortality
  mortality-overwinter

  ; When skipping winter just have food values reset at the beginning of the year
  ; Update ages of overwintering individuals
  if overwinter-skip?  = true [
    ask patches with [ resource-patch = true ] [ set resource-level max-resources ]
    ask turtles [
      let days-skip ( 365 - year-end + year-start )

      ; Animals increase their age by the days skipped
      set age age + days-skip
      ; And grow to the infinite size modified by their percent allocation to growth
      let perc-allo-growth-lm 1 / (1 + e ^ (- growth-lm-prob-const * (( storage-level / SL-max ) - ( growth-lm-prob-mid  * ITV-growth ))))
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

  ; Probability of moving is based on energy balance, e.g. the relative amount of food debt compared to daily requirements, and the amount of food encountered by animals
  let daily-food-est ( sum daily-m-tot ) / ( AE-food * ED-food )
  let move-prob 0
  if daily-food-est > 0 [ set move-prob ( -1 / daily-food-est ) * food-debt + 1 ]

  if sum stomach-fill >= stomach-fill-max [ set move-prob 1 ]
  if food-debt > daily-food-est [ set move-prob 0 ]

  ; If a random number is less than the movement probability, then animals don't move that tick
  ; ( move-prob = 1 means animals have 0% chance of moving, move-prob = 0 means 100% chance of moving )
  if length daily-activity = 48 [ set daily-activity remove-item 0 daily-activity ]
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

    ; Convert to meters per half hour
    set move-speed move-speed / 100 * 60 * 30

    repeat 3
    [
      ; Set heading randomly
      set heading random 360
      fd ( move-speed / patch-size-m ) / 3
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

  if debug = 4 and year = out-year and print? = true [ print word "speed: " move-speed-m-per-s  print word "ratio m-move / m-BMR: " (m-move / m-BMR) ]
  if debug = 4 and year = out-year and print? = true [ print (m-move / m-BMR) ]

end

;--- Growth and lean mass deposition ---;
to growth-lean-mass

  ; Percentage protein in lean mass based on age [%]
  set lean-mass-perc-pro -lean-mass-perc-protein(age)

  ; Energy density of lean mass [J kg-1]
  set ED-lean-mass lean-mass-perc-pro * ED-pro

  ; Allocation to lean mass deposition based on storage level [%]
  let perc-allo-growth-lm 1 / (1 + e ^ (- growth-lm-prob-const * (( storage-level / SL-max ) - ( growth-lm-prob-mid  * ITV-growth ))))

  ; Lean mass deposition rate [kg 30min-1]
  let lean-mass-depo (( growth-lm-k * perc-allo-growth-lm ) / 48 )*((( growth-lm-inf * perc-allo-growth-lm ) ^( 1 / 3 )) * ( lean-mass ^ ( 2 / 3 )) - lean-mass ) ; Sibly et al. 2013
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
  if pregnancy-status = false and
  age >= (item 1 t-mature) and
  day >= (item 0 t-mating + t-mating-offset) and
  day <= (item 1 ( t-mating ) - ( t-gest + t-nurs + t-0 ))
  [
    let preg-chance 1 / (1 + e ^ (-1 * preg-prob-const * (( storage-level / SL-max ) - ( preg-prob-mid * ITV-repro ))))
    if random-float 1.0 <= preg-chance [
      ; If so, get pregnant
      set pregnancy-status true
      ifelse lactation-status = true [ set ds-mating ( - t-0 ) ] [ set ds-mating 0 ]            ; If lactating include delayed implantation
      set n-emb item 0 n-emb-range + random (item 1 n-emb-range - item 0 n-emb-range + 1)       ; Implant a random number of embryos
      repeat n-emb [
        set mass-emb-list lput emb-mass-init mass-emb-list
        set mass-pl-list lput 0 mass-pl-list
      ]
      ; If debugging reproduction collect outputs
      if debug = 5 or debug = 6 [
        set n-litters n-litters + 1
        let conc-out-upd item 0 repro-outs
        set conc-out-upd lput n-emb conc-out-upd
        set repro-outs replace-item 0 repro-outs conc-out-upd
      ]
    ]
  ]

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
    let max-growth-emb-list map [ i -> e ^ (ln i + ((emb-growth-c / 48) * (ln emb-mass-inf - ln i))) - i ] mass-emb-list

    ; Fetal tissue investment per embryo
    let e-fat map [ i -> ((i / 1000) * percent-fat-emb * ED-fat) / DE-fat ] max-growth-emb-list ; Energy needed for fat deposition
    let e-pro map [ i -> ((i / 1000) * percent-pro-emb * ED-pro) / DE-pro ] max-growth-emb-list ; Energy needed for protein deposition

    let m-growth-emb-list ( map + e-fat e-pro )                             ; Energy needed to fuel growth

    let max-growth-pl-list []
    let m-growth-pl []

    if ds-mating > 9.35 [                                                 ; Only check if greater than 9.35 days ( source equation is only greater than 0 from this point )
      ; Placental growth based on gestation date [g]
      set max-growth-pl-list map [ i -> (( -0.54180 + 0.07887 * ds-mating - 0.002243 * ds-mating ^ 2) - i ) ] mass-pl-list ; Based on relationship for mice in Mu et al. 2008

      ; Placental tissue investment [J 30 min-1]
      let ED-pl 3249.9                                                    ; Energy density of placental tissue from Luz and Griggio 1996
      let DE-pl 0.501                                                     ; Deposition efficiency of placental tissue from Greizerstein 1982
      set m-growth-pl map [ i -> ( i * ED-pl )/ DE-pl] max-growth-pl-list
    ]

    ; Allocation to pregnancy based on storage level [%]
    let perc-allo-preg 1 / (1 + e ^ (-1 * preg-prob-const * (( storage-level / SL-max ) - ( preg-prob-mid * ITV-repro ))))

    ; Total cost of pregancy [J 30min-1]
    set m-preg ( sum m-growth-emb-list + sum m-growth-pl ) * perc-allo-preg

    ; Embryos and placentae grow
    let growth-emb-list map [ i -> perc-allo-preg * i ] max-growth-emb-list
    set mass-emb-list ( map + mass-emb-list growth-emb-list )

    if ds-mating > 9.35 [
      let growth-pl-list map [ i -> perc-allo-preg * i ] max-growth-pl-list
      if debug = 5 and print? = true [ if who = 0 [ if pregnancy-status = true [ print word "growth-pl-list:     " growth-pl-list ]]]
      set mass-pl-list (map + mass-pl-list growth-pl-list)
    ]

    set gest-mass ( sum mass-emb-list + sum mass-pl-list ) / 1000           ; Total additional mass of embyros and placentae

    ; Calculate pregnancy costs not associated with tissue production
    set m-preg-no-prod (( sum m-growth-emb-list ) * perc-allo-preg * percent-fat-emb * ( 1 - DE-fat )) + (( sum m-growth-emb-list ) * perc-allo-preg * percent-pro-emb * ( 1 - DE-pro )) + (( sum m-growth-pl ) * perc-allo-preg * ( 1 - 0.501 ))

    ; Pregnancy debug outputs
    if debug = 5 and print? = true [ if who = 0 [ if pregnancy-status = true [
      print word "max-growth-emb-list: " max-growth-emb-list
      print word "e-growth-emb-list:   " m-growth-emb-list
      print word "SL mom:              " storage-level
      print word "perc-allo-preg:      " perc-allo-preg
      print word "growth-emb-list:     " growth-emb-list
      print word "mass-emb-list:       " mass-emb-list
      print word "e-growth-pl:         " m-growth-pl
      print word "max-growth-pl-list:  " max-growth-pl-list
      print word "mass-pl-list:        " mass-pl-list
      print word "m-preg:              " m-preg
      print " "
    ]]]
  ]

  set ds-mating ds-mating + 1 / 48                                        ; Advance gestation ticker

end

;--- Lactation ---;
to lactation

  ; Check if time to wean offspring
  if ds-birth >= t-nurs [
    ; if so, wean and exit lactation procedure
    wean
    stop
  ]

  ; Offspring maintenance costs [J 30min -1]
  set m-BMR-off-list ( map [ [ x y ] -> ( B0 * x ^ gamma ) * off-BMR-red * y ] mass-off-list ITV-BMR-off-list ) ; from Kleiber 1975, Sibly et al. 2013

  ; Offspring maximum lean mass growth [kg 30min -1]
  let max-growth-lm-off-list map [ i -> ( growth-lm-k / 48 )*(( growth-lm-inf ^( 1 / 3 ) * ( i ^ ( 2 / 3 ))) - i ) ] lean-mass-off-list

  ; Percentage protein in lean mass based on age [%]
  let lean-mass-perc-pro-off -lean-mass-perc-protein(ds-birth)
  let ED-lean-mass-off lean-mass-perc-pro-off * ED-pro                         ; Energy density of lean mass [J kg-1]
  set m-growth-lm-off-list map [ i -> ( i * ED-lean-mass-off )/ off-growth-eff ] max-growth-lm-off-list

  ; Allocation to lactation based on storage level [%]
  let perc-allo-lact 1 / (1 + e ^ (-1 * lact-prob-const * (( storage-level / SL-max ) - ( lact-prob-mid * ITV-repro )))) * 2

  ; Maximum total cost of lactation [J 30min -1]
  let m-lact-max (sum m-BMR-off-list + sum m-growth-lm-off-list ) / milk-prod-eff

  ; Total cost of lactation [J 30min -1]
  set m-lact m-lact-max * perc-allo-lact

  ; Milk energy output available to offspring [J 30min -1]
  let milk-energy-output m-lact * milk-prod-eff

  let adipose-mass-off-list []
  ifelse m-lact > m-lact-max
  [
    ; if mothers provide more milk than calculated costs then BMR and growth costs are covered
    if debug = 6 and who = 0 and print? = true [ print "Lactation allocation decision 1" ]

    set lean-mass-off-list ( map + lean-mass-off-list max-growth-lm-off-list )                   ; Offspring grow maximally
    let m-lact-adi-per-off ( m-lact - m-lact-max ) / n-off                                       ; Extra is stored as fat
    let adi-per-off ( m-lact-adi-per-off * DE-fat ) / ( ED-fat * ( 1 - percent-water-adi ))
    set adipose-mass-off-list ( map [ [ x y ] -> ( x * y ) + adi-per-off ] SL-off-list mass-off-list )
  ]
  [
    ifelse milk-energy-output > sum m-BMR-off-list
    [
      ; If milk energy is not greater than calculated costs, but is greater than BMR, then BMR is covered and offspring grow suboptimally
      if debug = 6 and who = 0 and print? = true [ print "Lactation allocation decision 2" ]
      let m-lact-grow milk-energy-output - sum m-BMR-off-list                                    ; Reduce milk energy by BMR costs
      let m-lact-grow-per-off m-lact-grow / n-off                                                ; Divvy remaining energy per offspring
      if m-lact-grow-per-off < 0 [ print "m-lact-grow-per-off < 0" ]
      let grow-per-off ( m-lact-grow-per-off * off-growth-eff ) / ED-lean-mass-off               ; Convert to mass
      set lean-mass-off-list map [ i -> i + grow-per-off ] lean-mass-off-list                    ; Offspring grow suboptimally
      set adipose-mass-off-list ( map [ [ x y ] -> ( x * y ) + 0 ] SL-off-list mass-off-list )   ; And do not deposit fat

      if debug = 6 and print? = true [ if who = 0 [ if lactation-status = true [ print word "grow-per-off:          " grow-per-off ]]]
    ]
    [
      ; If milk energy is less than BMR costs, then offspring mobilize energy to cover only BMR
      if  debug = 6 and who = 0 and print? = true [ print "Lactation allocation decision 3" ]
      let off-energy-mobilize ( sum m-BMR-off-list - milk-energy-output ) / n-off                                                               ; Calculate energy needed to be mobilized
      let protein-storage-perc-off ( ED-pro / 1000 * gamma-mobilize ) / ((ED-fat / 1000 * mean SL-off-list) + (ED-pro / 1000 * gamma-mobilize)) ; Determine protein content of metabolized tissue
      if debug = 6 and who = 0 and print? = true [ print word "protein-storage-perc-off:          " protein-storage-perc-off ]
      let lean-mass-mobilized ( off-energy-mobilize * protein-storage-perc-off ) / ED-lean-mass                                                 ; Mobilize lean mass and adipose tissue
      let adipose-mobilized ( off-energy-mobilize * ( 1 - protein-storage-perc-off )) / (( 1 - percent-water-adi ) * ED-fat )
      set lean-mass-off-list ( map [ x -> x - lean-mass-mobilized ] lean-mass-off-list )                                                        ; Update lean and adipose masses
      set adipose-mass-off-list ( map [ [ x y ] -> ( x * y ) - adipose-mobilized ] SL-off-list mass-off-list )
    ]
  ]

  ; Update total offspring masses and storage levels
  set mass-off-list ( map + adipose-mass-off-list lean-mass-off-list )
  set SL-off-list ( map [ [ x y ] -> ( x / y ) ] adipose-mass-off-list mass-off-list )

  ; Calculate lactation costs not associated with tissue production
  set m-lact-no-prod m-lact * ( 1 - milk-prod-eff )

  ; Lactation debug outputs
  if debug = 6 and print? = true [ if who = 0 [ if lactation-status = true [
    print word "perc-allo-lact:        " perc-allo-lact
    print word "m-lact-max:            " m-lact-max
    print word "m-lact:                " m-lact
    print word "m-BMR-off-list:        " m-BMR-off-list
    print word "Sum m-BMR-off-list:    " sum m-BMR-off-list
    print word "max-growth-off-list:   " max-growth-lm-off-list
    print word "m-growth-lm-off-list:  " m-growth-lm-off-list
    print word "lean-mass-off-list:    " lean-mass-off-list
    print word "adipose-mass-off-list: " adipose-mass-off-list
    print word "mass-off-list:         " mass-off-list
    print word "SL-off-list:           " SL-off-list
    print " "
  ]]]

  set ds-birth ds-birth + (1 / 48)                                                 ; Advance lactation ticker

end


;--- Energy intake ---;
to energy-intake

  ; reset energy trackers
  set energy-assimilated 0
  set energy-mobilized 0
  set m-HIF 0

  ; Maximum stomach fill [g]
  let BMR-inf 1 ; factor to adjust for ITV in BMR to adjust baseline simulations
  let stom-mod ( m-BMR + m-preg + m-lact ) / ( m-BMR / (( ITV-BMR - 1 ) * BMR-inf + 1 )  )
  set stomach-clear-rate ( 5.473 * mass ^ 0.278 ) * 2 * (( stom-mod - 1 ) * -1 + 1 )  ; Abraham et al. 2021 relationship for mammal gut transit time
  if round stomach-clear-rate <= 0 [set stomach-clear-rate 1]
  set stomach-fill-max (( mass * 1000 * stomach-fill-perc ) / DM-food ) * stom-mod

  ; Calculate energy intake needs
  ; Total potential ingestion rate for a timestep [g 30min-1]
  let IR-timestep ( m-tot - m-HIF ) / ( AE-food * ( 1 - HIF ) * ED-food )             ; Should try to maintain energy balance, per Hopkins and Blundell 2017

  ; Scaled storage level from -1 to 1 [unitless]
  let SL-scaled 2 * ( storage-level / SL-max ) - 1

  ; Ingestion rate modifier based on storage level [%/100]
  ; Based on dual intervention point theory in Speakman 2014
  let IR-mod ( -1 * SL-scaled ^ 3 ) + 1
  if storage-level > SL-max [ set IR-mod 0 ]

  ; Update record of unmet food requirements [g]
  set food-debt food-debt + IR-timestep * IR-mod

  ; Ask patch to update food levels when encountered based on it's current food level and the last time it was consumed
  ask patch-here [ if resource-patch = true and resource-level < max-resources and last-eaten != ticks [ resources-grow ]]

  ; Resources found in patch here
  let food-here [resource-level] of patch-here

  ; Realized ingestion rate [food units]
  let IR-real 0

  ; Realized ingestion rate [g 30min-1]
  ifelse food-here > food-debt
  [
    set IR-real food-debt
    if sum stomach-fill + IR-real >= stomach-fill-max [ set IR-real stomach-fill-max - sum stomach-fill if IR-real < 0 [ set IR-real 0 ]]
    if IR-real < 0 [ print word who "'s IR is less than zero! 1" ]
  ]
  [
    set IR-real food-here
    if sum stomach-fill + IR-real >= stomach-fill-max [ set IR-real stomach-fill-max - sum stomach-fill if IR-real < 0 [ set IR-real 0 ]]
    if IR-real < 0 [ print word who "'s IR is less than zero! 2" ]
  ]

  ; Do not eat if not moving
  if move-speed = 0 [ set IR-real 0 ]

  ; Update record of unmet food requirements [g]
  set food-debt food-debt - IR-real

  ; Metabolizable energy intake [J 30min-1]
  let MEI IR-real * AE-food * ED-food

  ; Update daily ingestion monitor
  if length daily-ingestion = 48 [ set daily-ingestion remove-item 0 daily-ingestion ]
  set daily-ingestion lput MEI daily-ingestion

  ; Update stomach fill
  while [ length stomach-fill >= round stomach-clear-rate ] [ set stomach-fill remove-item 0 stomach-fill ]
  set stomach-fill lput IR-real stomach-fill
 ; if who = 0 [ print stomach-fill ]

  if move-speed != 0 [
    ; Remove food from patch
    let resources-consumed IR-real
    if resources-dynamic? = true and [resource-level] of patch-here > 0 [
      ask patch-here [
        set resource-level resource-level - resources-consumed
        set last-eaten ticks
        set pcolor scale-color green resource-level ( max-resources-base + max-resources-base * 0.5 ) 0
    ]]
  ]

  ; Energy spent on the heat increment of feeding [J]
  set m-HIF MEI * HIF

  ; Calculate assimilated energy based on ingestion [in g] and energy density and assimilation efficiency of food
  set energy-assimilated MEI * ( 1 - HIF )

  ; Energy intake debug outputs
  if debug = 2 and remainder ticks 48 = 0 [
    if print? = true [ if who = 0 [
      print word "IR-timestep " IR-timestep
      print word "IR-mod " IR-mod
      print word "IR " (IR-timestep * IR-mod)
      print word "food here " food-here
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
  set protein-storage-perc (( ED-pro / 1000 ) * gamma-mobilize ) / ((( ED-fat / 1000 ) * storage-level ) + (( ED-pro / 1000 ) * gamma-mobilize ))

  ; Adipose synthesized [kg]
  let adipose-synthesized ( energy-assimilated * ( 1 - protein-storage-perc ) * DE-fat ) / ( ED-fat * ( 1 - percent-water-adi ) )

  ; Lean mass synthesized [kg]
  let lean-mass-synthesized-energy ( energy-assimilated * protein-storage-perc )

  ; Reserve mobilization from adipose tissue [kg]
  let adipose-mobilized ( energy-mobilized * ( 1 - protein-storage-perc )) / ( ED-fat * ( 1 - percent-water-adi ))

  ; Reserve mobilization from lean mass [kg]
  let lean-mass-mobilized-energy ( energy-mobilized * protein-storage-perc )

  let lean-mass-change 0
  let e-diff ( lean-mass-synthesized-energy + m-lean-mass ) - lean-mass-mobilized-energy
  ifelse e-diff > 0
  [ set lean-mass-change ( e-diff * DE-pro ) / ED-lean-mass ]
  [ set lean-mass-change  e-diff  / ED-lean-mass ]

  if who = 0 and debug = 7 and print? = TRUE [
    print word "energy-assimilated:           " energy-assimilated
    print word "energy-mobilized:             " energy-mobilized
    print word "protein-storage-perc:         " protein-storage-perc
    print word "lean-mass-mobilized-energy:   " (energy-mobilized * protein-storage-perc)
    print word "m-lean-mass:                  " m-lean-mass
    print word "lean-mass-assimilated-energy: " (energy-assimilated * protein-storage-perc)
    print word "e-diff:                       " e-diff
    print word "lean-mass-change:             " lean-mass-change
    print " " ]

  ; Storage debug outputs
  if debug = 8 and year = out-year and remainder ticks 48 = 0 [
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

  ; For testing
;  set storage-level SL-test
;  set mass (lean-mass / ( 1 - storage-level ))  + gest-mass
;  set adipose-mass mass * storage-level

;  set mass 0.026 + random-float 0.002
;  set storage-level SL-test
;  set adipose-mass mass * storage-level
;  set lean-mass mass - adipose-mass

  ; storage related mortality
  if storage-level <= 0.01
  [
    if print? = true [ print word who " died of low storage" ]
    if debug = 8 and year = out-year [ set SL-at-death-list lput ( precision storage-level 3 ) SL-at-death-list ]
    if debug = 10 and year = out-year [ set age-at-death-list lput age age-at-death-list ]
    die
  ]


end

;    --------------------------------------------    ;
;    |        END ENERGY BUDGET PROCEDURES      |    ;
;    --------------------------------------------    ;

;--- REPRODUCTION AND MORTALITY ---;

; Give birth to offspring
to give-birth

  if lactation-status = true [ wean ] ; if already have dependant offspring, wean them before giving birth

  ; Initialize birth specific parameters
  set pregnancy-status false
  set lactation-status true
  set n-off n-emb
  set mass-pl-list []
  set mass-off-list ( map [ i -> i / 1000 ]  mass-emb-list )
  set mass-emb-list []
  set SL-off-list []
  set ITV-BMR-off-list []
  repeat n-off [
    set SL-off-list lput percent-fat-emb SL-off-list
    ; Assign ITV in BMR and cap at zero
    let ITV-b-tmp random-normal ITV-BMR ( ITV-var-BMR / 100 )
    while [ ITV-b-tmp < 0 ] [ set ITV-b-tmp random-normal ITV-BMR ( ITV-var-BMR / 100 ) ]
    set ITV-BMR-off-list lput ITV-b-tmp ITV-BMR-off-list
  ]
  set lean-mass-off-list ( map [ i -> i * ( 1 - percent-fat-emb ) ] mass-off-list )
  set ds-mating 0
  set m-preg 0
  set n-emb 0
  set gest-mass 0

  ; If debugging reproduction, collect outputs
  if debug = 5 or debug = 6 [
    let birth-out-upd item 1 repro-outs
    set birth-out-upd lput n-off birth-out-upd
    set repro-outs replace-item 1 repro-outs birth-out-upd
  ]

  ; Run lactation
  lactation

end

; Wean dependant offspring
to wean

  set lactation-status false

  ; If debugging reproduction, collect outputs
  if debug = 5 or debug = 6
  [
    let wean-out-upd item 2 repro-outs
    set wean-out-upd lput n-off wean-out-upd
    set repro-outs replace-item 2 repro-outs wean-out-upd
  ]

  while [ n-off > 0 ]
  [
    ; Create a new turtle for each offspring and initialize parameters
    let mass-pass item 0 mass-off-list
    let SL-pass item 0 SL-off-list
    let LM-pass item 0 lean-mass-off-list
    let age-pass ds-birth
    let ITV-b-pass item 0 ITV-BMR-off-list
    let ITV-r-pass ITV-repro
    let ITV-g-pass ITV-growth

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

        set ITV-repro random-normal ITV-r-pass ( ITV-var-repro / 100 )
        while [ ITV-repro < 0 ] [ set ITV-repro random-normal ITV-r-pass ( ITV-var-repro / 100 ) ]

        set ITV-growth random-normal ITV-g-pass ( ITV-var-growth / 100 )
        while [ ITV-growth < 0 ] [ set ITV-growth random-normal ITV-g-pass ( ITV-var-growth / 100 ) ]


        set stomach-fill []
        set t-mating-offset -15 + random 30
        set pregnancy-status false
        set mass-emb-list []
        set mass-pl-list []
        set lactation-status false
        set mass-off-list []
        set SL-off-list []
        set lean-mass-off-list []
        set ITV-BMR-off-list []
        set m-BMR-off-list []
        set m-growth-lm-off-list []
        if debug = 5 or debug = 6 [ set repro-outs ( list [] [] [] ) ]
        set daily-ingestion []
        set daily-activity []
        set daily-m-tot []
        set daily-m-tot-no-prod []

        set color black
      ]
    ]

    set mass-off-list remove-item 0 mass-off-list
    set SL-off-list remove-item 0 SL-off-list
    set lean-mass-off-list remove-item 0 lean-mass-off-list
    set m-BMR-off-list remove-item 0 m-BMR-off-list
    set ITV-BMR-off-list remove-item 0 ITV-BMR-off-list

    set n-off n-off - 1

  ]

  ; Reset lactation parameters
  set ds-birth 0
  set mass-off-list []
  set SL-off-list []
  set lean-mass-off-list []
  set ITV-BMR-off-list []
  set m-BMR-off-list []
  set m-lact 0
  set n-off 0

end

; Execute mortality daily
to mortality-max-age

  ; Based on maximum age
  if age > t-max-age
  [
    if print? = true [ print word who " died of old age" ]
    if debug = 10 and year = out-year  [ set age-at-death-list lput age age-at-death-list ]
    die
  ]

end

to mortality-starvation

  ; Based on storage level
  ;; Adult mortality
  let surv-prob 1 / (1 + e ^ (-1 * surv-prob-const * (( storage-level / SL-max ) - surv-prob-mid )))
  if ( random-float 1 >= surv-prob )
  [
    if print? = true [ print word who " died of low storage" ]
    if debug = 8 and year = out-year [ set SL-at-death-list lput ( precision storage-level 3 ) SL-at-death-list ]
    if debug = 10 and year = out-year  [ set age-at-death-list lput age age-at-death-list ]
    die
  ]

  ;; Abortions
  if pregnancy-status = true
  [
    ; Estimate probability of abortion
    let surv-embryo-prob 1 / ( 1 + e ^ (( -1 * surv-prob-const  * ( 1 + ( 1 - surv-mod-embryo )))* (( storage-level / SL-max ) - ( surv-prob-mid * surv-mod-embryo ))))
    ; Check survival for each embryo individually
    let surv-embryo-prob-per-embryo []
    repeat n-emb [ set surv-embryo-prob-per-embryo lput ( random-float 1 ) surv-embryo-prob-per-embryo ]
    let mort-embryo-check map [ i -> i >= surv-embryo-prob ] surv-embryo-prob-per-embryo
    let mort-embryo-pos []
    while [ member? true mort-embryo-check ] [
      let pos ( position true mort-embryo-check )
      set mort-embryo-pos lput pos mort-embryo-pos
      set mort-embryo-check remove-item pos mort-embryo-check
    ]

    if debug = 10 and print? = true [ if who = 0 [ if pregnancy-status = true [
      print "Embryo mortality: "
      print word "surv-embryo-prob:            " surv-embryo-prob
      print word "surv-embryo-prob-per-embryo: " surv-embryo-prob-per-embryo
      print word "mort-embryo-pos:             " mort-embryo-pos
      print word "mort-embryo-check:           " mort-embryo-check
      print " "
    ]]]

    ; Embryos which should die die
    while [ length mort-embryo-pos > 0 ] [
      let pos ( item 0 mort-embryo-pos )
      let gest-mass-loss ( item pos mass-emb-list ) / 1000 + ( item pos mass-pl-list ) / 1000
      set gest-mass gest-mass - gest-mass-loss
      set mass-emb-list remove-item pos mass-emb-list
      set mass-pl-list remove-item pos mass-pl-list
      set n-emb n-emb - 1

      if debug = 10 and print? = true [ if who = 0 [ if pregnancy-status = true [
        print word "gest-mass-loss:        " gest-mass-loss
        print word "gest-mass:             " gest-mass
        print word "mass-emb-list:         " mass-emb-list
        print word "mass-pl-list:          " mass-pl-list
        print word "n-emb:                 " n-emb
        print " "
      ]]]

      ifelse length mort-embryo-pos = 1
      [ set mort-embryo-pos [] ]
      [ set mort-embryo-pos remove-item 0 mort-embryo-pos ]
    ]

    ; If all embryos die, stop being pregnant
    if n-emb = 0 [
      set pregnancy-status false
      set mass-pl-list []
      set mass-emb-list []
      set ds-mating 0
      set m-preg 0

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
    ; Check survival for each offspring individually
    let surv-prob-off map [ i -> 1 / ( 1 + e ^ (( -1 * surv-prob-const  * ( 1 + ( 1 - surv-mod-off )))* (( i / SL-max ) - ( surv-prob-mid * surv-mod-off )))) ] SL-off-list
    let mort-off-check map [ i -> random-float 1 >= i ] surv-prob-off
    let mort-off-pos []
    while [ member? true mort-off-check ] [
      let pos ( position true mort-off-check )
      set mort-off-pos lput pos mort-off-pos
      set mort-off-check remove-item pos mort-off-check
    ]
    if debug = 10 and print? = true [ if who = 0 [ if lactation-status = true [
      print "Offspring mortality: "
      print word "surv-prob-off:         " surv-prob-off
      print word "mort-off-check:        " mort-off-check
      print word "mort-off-pos:          " mort-off-pos
      print " "
    ]]]

    ; Offspring die
    while [ length mort-off-pos > 0 ] [
      let pos ( item 0 mort-off-pos )
      set mass-off-list remove-item pos mass-off-list
      if debug = 8 and year = out-year [ set SL-at-death-list lput (precision item pos SL-off-list 3) SL-at-death-list ]
      set SL-off-list remove-item pos SL-off-list
      set lean-mass-off-list remove-item pos lean-mass-off-list
      set ITV-BMR-off-list remove-item pos ITV-BMR-off-list
      set m-BMR-off-list remove-item pos m-BMR-off-list
      set m-growth-lm-off-list remove-item pos m-growth-lm-off-list
      set n-off n-off - 1

      if debug = 10 and print? = true [ if who = 0 [ if lactation-status = true [
        print word "mass-off-list:         " mass-off-list
        print word "SL-off-list:           " SL-off-list
        print word "lean-mass-off-list:    " lean-mass-off-list
        print word "m-BMR-off-list:        " m-BMR-off-list
        print word "m-growth-lm-off-list:  " m-growth-lm-off-list
        print word "n-off:                 " n-off
        print " "
      ]]]

      if debug = 10 and year = out-year [ set age-at-death-list lput ds-birth age-at-death-list ]

      ifelse length mort-off-pos = 1
      [ set mort-off-pos [] ]
      [ set mort-off-pos remove-item 0 mort-off-pos ]
    ]

    ; If all offspring die, stop lactating
    if n-off = 0 [
      set lactation-status false
      set ds-birth 0
      set mass-off-list []
      set SL-off-list []
      set lean-mass-off-list []
      set ITV-BMR-off-list []
      set m-BMR-off-list []
      set m-growth-lm-off-list []
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
  let rand-overwinter-mort 1 - ( item 0 winter-surv + random-float ( item 1 winter-surv - item 0 winter-surv ))
  if debug = 10 and print? = true [ print word "Overwinter mortality percent:     " ( rand-overwinter-mort * 100 ) ]

  ask n-of ( round ( rand-overwinter-mort * count turtles )) turtles
  [
    if print? = true [ print word who " died of overwinter mortality" ]
    if debug = 10 and year = out-year [ set age-at-death-list lput age age-at-death-list ]
    die
  ]

end


;---  OUTPUTS ---;
to update-records

  set m-tot m-HIF + m-BMR + m-move + m-preg + m-lact + m-lean-mass
  if length daily-m-tot = 48 [ set daily-m-tot remove-item 0 daily-m-tot ]
  set daily-m-tot lput m-tot daily-m-tot
  if length daily-m-tot > 48 [ print word who "'s daily-m-tot list > 48" ]

  ; For debugging FMR
  if debug = 9 [

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

  ; For debugging FMR and lactation
  if debug = 6 or debug = 9 [
    set m-tot-no-prod m-HIF + m-BMR + m-move + m-preg-no-prod + m-lact-no-prod + m-lean-mass-no-prod

    if length daily-m-tot-no-prod = 48 [ set daily-m-tot-no-prod remove-item 0 daily-m-tot-no-prod ]
    set daily-m-tot-no-prod lput m-tot-no-prod daily-m-tot-no-prod
    if length daily-m-tot-no-prod > 48 [ print word who "'s daily-m-tot-no-prod list > 48" ]

  ]

end

; Update plots
to update-monitors

  set-current-plot "Age distribution"
  let aa [round age] of turtles
  let cc []
  if any? turtles with [lactation-status = true] [ ask turtles with [lactation-status = true] [ repeat n-emb [ set cc lput ds-birth cc ]]]
  set aa sentence cc aa
  histogram aa

  ask turtles [

    if debug = 2 [
      set-current-plot "Ingestion rates"
      if pregnancy-status = false and lactation-status = false and age > (item 1 t-mature)
      [
        set-current-plot-pen "NonLact"
        plotxy age ( sum daily-ingestion ) / ( ED-food * AE-food )
      ]
      if pregnancy-status = false and lactation-status = true
      [
        set-current-plot-pen "LactnonPreg"
        plotxy age ( sum daily-ingestion ) / ( ED-food * AE-food )
      ]
      if pregnancy-status = true and lactation-status = true
      [
        set-current-plot-pen "LactPreg"
        plotxy age ( sum daily-ingestion ) / ( ED-food * AE-food )
      ]
      if pregnancy-status = true and lactation-status = false
      [
        set-current-plot-pen "PregnonLact"
        plotxy age ( sum daily-ingestion ) / ( ED-food * AE-food )
      ]
      if age < (item 1 t-mature)
      [
        set-current-plot-pen "Juvs"
        plotxy age ( sum daily-ingestion ) / ( ED-food * AE-food )
      ]

      set-current-plot "Energy intake [J]"
      set-current-plot-pen "m-tot"
      plotxy ( sum daily-m-tot ) ( sum daily-ingestion )

      set-current-plot "Ingestion rate max"
      plotxy age sum stomach-fill

    ]

    if debug = 3 and year = out-year [
      set-current-plot "Basal metabolic rate"
      plotxy mass m-BMR
    ]

    if debug = 4 and year = out-year [
      set-current-plot "Cost of transport"
      set-current-plot-pen "BMR"
      plotxy move-speed / 1800 (m-BMR )
      set-current-plot-pen "pcot"
      plotxy move-speed / 1800 (m-BMR + pcot * 1800)
      set-current-plot-pen "icot"
      plotxy move-speed / 1800 (m-BMR + m-move)


      set-current-plot "Move probability"
      set-current-plot-pen "default"
      let daily-food-est ( sum daily-m-tot  ) / ( ED-food * AE-food )
      let mp 0
      if daily-food-est > 0 [
        set mp ( -1 / daily-food-est ) * food-debt + 1
      ]
      if sum stomach-fill >= stomach-fill-max [ set mp 1 ]
      if food-debt > daily-food-est [ set mp 0 ]
      plotxy (food-debt / daily-food-est) mp
      set-current-plot-pen "move"
      let mv 0
      ifelse move-speed > 0 [ set mv 1 ][ set mv 0 ]
      plotxy (food-debt / daily-food-est) mv


      set-current-plot "Activity by state"
      if pregnancy-status = false and lactation-status = false and age > (item 1 t-mature)
      [
        set-current-plot-pen "NonLact"
        plotxy 2 ( mean daily-activity )
      ]
      if pregnancy-status = false and lactation-status = true
      [
        set-current-plot-pen "LactnonPreg"
        plotxy 4 ( mean daily-activity )
      ]
      if pregnancy-status = true and lactation-status = true
      [
        set-current-plot-pen "LactPreg"
        plotxy 5 ( mean daily-activity )
      ]
      if pregnancy-status = true and lactation-status = false
      [
        set-current-plot-pen "PregnonLact"
        plotxy 3 ( mean daily-activity )
      ]
      if age < (item 1 t-mature)
      [
        set-current-plot-pen "Juvs"
        plotxy 1 ( mean daily-activity )
      ]

    ]

    if debug = 7 and year = out-year [
      set-current-plot "Growth rate"
      if pregnancy-status = false and lactation-status = false and age > (item 1 t-mature)
      [
        set-current-plot-pen "NonLact"
        plotxy lean-mass m-lean-mass
        ;plotxy storage-level m-lean-mass
      ]
      if pregnancy-status = false and lactation-status = true
      [
        set-current-plot-pen "LactnonPreg"
        plotxy lean-mass m-lean-mass
        ;plotxy storage-level m-lean-mass
      ]
      if pregnancy-status = true and lactation-status = true
      [
        set-current-plot-pen "LactPreg"
        plotxy lean-mass m-lean-mass
        ;plotxy storage-level m-lean-mass
      ]
      if pregnancy-status = true and lactation-status = false
      [
        set-current-plot-pen "PregnonLact"
        plotxy lean-mass m-lean-mass
        ;plotxy storage-level m-lean-mass
      ]
      if age < (item 1 t-mature)
      [
        set-current-plot-pen "Juvs"
        plotxy lean-mass m-lean-mass
        ;plotxy storage-level m-lean-mass
      ]

      if lactation-status = true [
        set-current-plot-pen "Offspring"
        plotxy (mean mass-off-list) (mean m-growth-lm-off-list)
        ;plotxy (mean SL-off-list) (mean m-growth-lm-off-list)
      ]

      set-current-plot "Body mass"
      set-current-plot-pen "total mass"
      plotxy age mass - gest-mass
      set-current-plot-pen "lean mass"
      plotxy age lean-mass
      if lactation-status = true [
        set-current-plot-pen "total mass"
        plotxy ds-birth (mean mass-off-list)
        set-current-plot-pen "lean mass"
        plotxy ds-birth (mean mass-off-list - (mean mass-off-list * mean SL-off-list))
      ]
    ]

    if debug = 8 and year = out-year [
      set-current-plot "Percent protein mobilized"
      plotxy storage-level protein-storage-perc * 100
    ]

    if debug = 9 [
      ;      set-current-plot "FMR by mass"
      ;      if pregnancy-status = false and lactation-status = false [
      ;        plotxy mass ( sum daily-m-tot-no-prod )
      ;      ]
      ;
      ;      set-current-plot "FMR by activity"
      ;      if pregnancy-status = false and lactation-status = false [
      ;        plotxy ( mean daily-activity ) ( ( sum daily-m-tot-no-prod ) / ( mass * 1000 ))
      ;      ]
      ;
      ;      set-current-plot "FMR by state"
      ;      if pregnancy-status = false and lactation-status = false and age > (item 1 t-mature)
      ;      [
      ;        set-current-plot-pen "NonLact"
      ;        plotxy 2 ( sum daily-m-tot-no-prod )
      ;      ]
      ;      if pregnancy-status = false and lactation-status = true
      ;      [
      ;        set-current-plot-pen "LactnonPreg"
      ;        plotxy 4 ( sum daily-m-tot-no-prod )
      ;      ]
      ;      if pregnancy-status = true and lactation-status = true
      ;      [
      ;        set-current-plot-pen "LactPreg"
      ;        plotxy 5 ( sum daily-m-tot-no-prod )
      ;      ]
      ;      if pregnancy-status = true and lactation-status = false
      ;      [
      ;        set-current-plot-pen "PregnonLact"
      ;        plotxy 3 ( sum daily-m-tot-no-prod )
      ;      ]
      ;      if age < (item 1 t-mature)
      ;      [
      ;        set-current-plot-pen "Juvs"
      ;        plotxy 1 ( sum daily-m-tot-no-prod )
      ;      ]
      ;

      if who < 100 [
        set-current-plot "m-tot"
        set-current-plot-pen "BMR"
        plotxy age daily-m-BMR
        set-current-plot-pen "Loco"
        plotxy age daily-m-move
        ; plotxy age daily-m-BMR + daily-m-move
        set-current-plot-pen "HIF"
        ;plotxy age daily-m-BMR + daily-m-move + daily-m-HIF
        plotxy age daily-m-HIF
        set-current-plot-pen "Grow"
        ;plotxy age daily-m-BMR + daily-m-move + daily-m-HIF + daily-m-lean-mass
        plotxy age daily-m-lean-mass
        if daily-m-preg > 0 [
          set-current-plot-pen "Preg"
          ;plotxy age daily-m-BMR + daily-m-move + daily-m-HIF + daily-m-lean-mass + daily-m-preg
          plotxy age daily-m-preg
        ]
        if daily-m-lact > 0 [
          set-current-plot-pen "Lact"
          ;plotxy age daily-m-BMR + daily-m-move + daily-m-HIF + daily-m-lean-mass + daily-m-preg + daily-m-lact
          plotxy age daily-m-lact
        ]
        set-current-plot-pen "m-tot"
        plotxy age daily-m-tot


        set-current-plot "m-tot-no-prod"
        set-current-plot-pen "BMR"
        plotxy age daily-m-BMR
        set-current-plot-pen "Loco"
        plotxy age daily-m-BMR + daily-m-move
        set-current-plot-pen "HIF"
        plotxy age daily-m-BMR + daily-m-move + daily-m-HIF
        set-current-plot-pen "Grow"
        plotxy age daily-m-BMR + daily-m-move + daily-m-HIF + daily-m-lean-mass-no-prod
        if daily-m-preg > 0 [
          set-current-plot-pen "Preg"
          plotxy age daily-m-BMR + daily-m-move + daily-m-HIF + daily-m-lean-mass-no-prod + daily-m-preg-no-prod
        ]
        if daily-m-lact > 0 [
          set-current-plot-pen "Lact"
          plotxy age daily-m-BMR + daily-m-move + daily-m-HIF + daily-m-lean-mass-no-prod + daily-m-preg-no-prod + daily-m-lact-no-prod
        ]
        set-current-plot-pen "m-tot"
        plotxy age ( sum daily-m-tot-no-prod )
      ]

    ]


  ]

  if debug = 5 and year = out-year [
    if any? turtles with [ pregnancy-status = true ] [
      ask turtles with [ pregnancy-status = true ] [
        set-current-plot "Count embryos"
        plotxy mass n-emb

        set-current-plot "Gestational masses"
        set-current-plot-pen "embryo"
        plotxy ds-mating mean mass-emb-list
        set-current-plot-pen "placenta"
        plotxy ds-mating mean mass-pl-list

        set-current-plot "Gestation costs"
        set-current-plot-pen "total"
        plotxy ds-mating m-preg / n-emb


        if round ds-mating = t-gest [
          set-current-plot "Pup mass at birth by litter size"
          plotxy n-emb mean mass-emb-list
        ]
    ]]
  ]

  if debug = 6 and year >= out-year [
;    set-current-plot "Count pups"
;    histogram [n-off] of turtles with [ n-off > 0 ]
;
    if any? turtles with [ lactation-status = true ] [
      ;ask turtles with [ lactation-status = true and ds-birth < 16 ] [
      ask turtles with [ lactation-status = true ] [

        set-current-plot "Offspring masses"
        set-current-plot-pen "total mass"
        plotxy ds-birth mean mass-off-list
        set-current-plot-pen "lean mass"
        plotxy ds-birth mean lean-mass-off-list

        set-current-plot "Litter mass"
        plotxy ds-birth ( sum mass-off-list ) * 1000

;        set-current-plot "Lactation costs"
;        set-current-plot-pen "BMR"
;        plotxy ds-birth mean m-BMR-off-list
;        set-current-plot-pen "lean-mass"
;        plotxy ds-birth mean m-growth-lm-off-list
;        set-current-plot-pen "total"
;        plotxy ds-birth m-lact / n-off

        set-current-plot "Lactation costs total"
        plotxy ds-birth ( m-lact * 48 ) / 1000

        set-current-plot "Lactation energy intake"
        set-current-plot-pen "default"
        plotxy ds-birth ( sum daily-ingestion ) / ( ED-food * AE-food )

        set-current-plot "Mom body mass"
        plotxy ds-birth mass * 1000

        set-current-plot "Mom to litter mass"
        plotxy mass ( sum mass-off-list )

        if round ds-birth = t-nurs [
          set-current-plot "Pup mass by litter size"
          plotxy n-off mean mass-off-list
        ]
      ]

      ask turtles with [ lactation-status = true and round ds-birth = 15 ] [
        set-current-plot "Peak energy use by litter size"
        plotxy n-off ( sum daily-m-tot-no-prod ) / 1000

        set-current-plot "Peak food consumption by litter size"
        plotxy n-off ( sum daily-ingestion ) / ( ED-food * AE-food )

        set-current-plot "Peak milk energy output by litter size"
        ; calculated following method from Sadowska et al. 2016 as difference between MEI and ADMR
        plotxy n-off (sum daily-ingestion / 1000) - (( sum daily-m-tot-no-prod ) / 1000)
    ]]
  ]

  if debug = 8 and year = out-year [
    set-current-plot "Storage levels"
    ;histogram [storage-level] of turtles
    histogram SL-list

    set-current-plot "Storage levels at death"
    histogram SL-at-death-list

  ]

  if debug = 4 and year = out-year [
    set-current-plot "Percent moving"
    set-current-plot-pen "default"
    plotxy ticks (count turtles with [move-speed > 0]) / count turtles
    if any? turtles with [ lactation-status = true ] [
      set-current-plot-pen "Lact"
      plotxy ticks (count turtles with [move-speed > 0 and lactation-status = true]) / count turtles with [ lactation-status = true ]
    ]

    set-current-plot "Movement speed"
    histogram [move-speed] of turtles with [move-speed > 0 ]
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
211
61
648
499
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
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
perc-resource-patches
perc-resource-patches
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
100
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
314
150
332
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

PLOT
654
10
1186
160
Basal metabolic rate
Mass [kg]
NIL
0.015
0.035
300.0
800.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
654
160
831
310
Cost of transport
Speed [m s-1]
NIL
0.0
0.4
0.0
1.75
true
false
"" ""
PENS
"BMR" 1.0 2 -16777216 true "" ""
"pcot" 1.0 2 -7500403 true "" ""
"icot" 1.0 2 -2674135 true "" ""

CHOOSER
467
10
559
55
debug
debug
0 1 2 3 4 5 6 7 8 9 10
0

PLOT
648
467
824
617
Growth rate
Mass [kg]
NIL
0.0
0.03
0.0
0.03
true
false
"" ""
PENS
"LactnonPreg" 1.0 2 -16777216 true "" ""
"NonLact" 1.0 2 -7500403 true "" ""
"LactPreg" 1.0 2 -2674135 true "" ""
"PregnonLact" 1.0 2 -955883 true "" ""
"Juvs" 1.0 2 -6459832 true "" ""
"Offspring" 1.0 2 -1184463 true "" ""

PLOT
824
467
1008
617
Body mass
Age [days]
NIL
-1.0
300.0
0.0
0.03
true
false
"" ""
PENS
"total mass" 1.0 2 -16777216 true "" ""
"lean mass" 1.0 2 -7500403 true "" ""

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

PLOT
648
616
827
766
Count embryos
Mass [kg]
NIL
0.015
0.03
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

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

PLOT
823
616
1008
766
Gestational masses
Days since mating
NIL
0.0
21.0
0.0
2.0
true
false
"" ""
PENS
"embryo" 1.0 2 -16777216 true "" ""
"placenta" 1.0 2 -7858858 true "" ""

PLOT
1007
467
1180
617
Lean mass percent protein
Age [days]
NIL
0.0
10.0
0.1
0.2
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
823
765
1008
915
Offspring masses
Days since birth
NIL
0.0
16.0
0.0
0.01
true
false
"" ""
PENS
"total mass" 1.0 2 -16777216 true "" ""
"lean mass" 1.0 2 -7500403 true "" ""

PLOT
1007
765
1180
915
Lactation costs
Days since birth
NIL
0.0
16.0
0.0
10.0
true
false
"" ""
PENS
"BMR" 1.0 2 -5298144 true "" ""
"growth" 1.0 2 -14070903 true "" ""
"fat" 1.0 2 -2064490 true "" ""
"total" 1.0 2 -16777216 true "" ""

PLOT
1007
616
1180
766
Gestation costs
Days since mating
NIL
0.0
21.0
0.0
10.0
true
false
"" ""
PENS
"total" 1.0 2 -16777216 true "" ""

PLOT
648
765
824
915
Count pups
N
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -13791810 true "" ""

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

PLOT
1187
10
1387
160
Percent protein mobilized
Storage level [%/100]
NIL
0.0
0.5
0.0
0.3
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1386
10
1586
160
Lean mass and adipose mobilized
Storage level [%/100]
NIL
0.0
0.4
0.0
0.001
true
false
"" ""
PENS
"lm" 1.0 2 -16777216 true "" ""
"fat" 1.0 2 -7500403 true "" ""

PLOT
1386
160
1586
310
Functional response
NIL
NIL
0.0
1.0
0.0
0.03
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

SLIDER
0
142
174
175
max-resources-base
max-resources-base
0.1
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
177
172
210
r-growth-ts
r-growth-ts
0
2
0.02
0.01
1
NIL
HORIZONTAL

PLOT
1586
159
1786
309
Ingestion rates
NIL
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"NonLact" 1.0 2 -16777216 true "" ""
"LactPreg" 1.0 2 -2674135 true "" ""
"LactnonPreg" 1.0 2 -7500403 true "" ""
"PregnonLact" 1.0 2 -955883 true "" ""
"Juvs" 1.0 2 -6459832 true "" ""

SLIDER
3
429
175
462
growth-lm-prob-const
growth-lm-prob-const
0
100
16.0
1
1
NIL
HORIZONTAL

SLIDER
3
461
175
494
growth-lm-prob-mid
growth-lm-prob-mid
-1
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
2
411
152
429
Calibrated parameters
12
0.0
1

PLOT
1585
10
1785
160
Storage levels
[%/100]
NIL
0.0
0.4
0.0
10.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" ""
"offspring" 1.0 1 -7500403 true "" ""

SLIDER
3
497
175
530
preg-prob-const
preg-prob-const
0
100
13.0
1
1
NIL
HORIZONTAL

SLIDER
3
529
175
562
preg-prob-mid
preg-prob-mid
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
3
566
175
599
lact-prob-const
lact-prob-const
0
30
15.0
1
1
NIL
HORIZONTAL

SLIDER
3
598
175
631
lact-prob-mid
lact-prob-mid
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
3
634
175
667
surv-prob-const
surv-prob-const
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
3
666
175
699
surv-prob-mid
surv-prob-mid
0
1
0.1
0.05
1
NIL
HORIZONTAL

PLOT
212
625
648
775
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
"Offspring" 1.0 0 -7500403 true "" "if remainder ticks 48 = 0 [ plot sum [ n-off ] of turtles with [ n-off > 0 ]]"

PLOT
1181
467
1381
617
Energy intake [J]
Energy use [J]
NIL
0.0
0.04
0.0
10.0
true
false
"" ""
PENS
"m-tot" 1.0 2 -8053223 true "" ""
"EI" 1.0 2 -14985354 true "" ""

PLOT
8
767
174
908
Age distribution
NIL
NIL
0.0
600.0
0.0
10.0
true
false
"" ""
PENS
"default" 3.0 1 -16777216 true "" ""

PLOT
212
504
648
624
Food eaten (IR) and average stores
Ticks
NIL
0.0
10.0
0.0
0.4
true
true
"" ""
PENS
"IR" 1.0 2 -14439633 true "" ";if ticks > 1 [ plotxy ticks mean [IR-real] of turtles ]"
"Stores" 1.0 0 -16777216 true "" "if ticks > 1 [ if remainder ticks 48 = 0 [ plotxy ticks mean [storage-level] of turtles ]] ; if ticks > 1 [ plotxy ticks mean [storage-level] of turtles ]"
"IR - Lact" 1.0 2 -7858858 true "" ";if ticks > 1 [ if any? turtles with [ lactation-status = true ][ plotxy ticks mean [IR-real] of turtles with [ lactation-status = true ] * 5 ]]"
"food" 1.0 0 -7500403 true "" ";if ticks > 1 [ plotxy ticks sum [ resource-level ] of patches with [ resource-level > 0 ] ]"
"food-debt" 1.0 0 -2674135 true "" ";if ticks > 1 [ plotxy ticks mean [ food-debt ] of turtles ]"
"mass" 1.0 0 -955883 true "" ";if ticks > 1 [ plotxy ticks ( mean [mass] of turtles)] "
"max-res" 1.0 0 -6459832 true "" ";if ticks > 1 [ plotxy ticks max-resources ]"
"move" 1.0 0 -1184463 true "" ";if ticks > 1 [ plotxy ticks (count turtles with [move-speed > 0] / count turtles)] "
"pen-8" 1.0 0 -10899396 true "" "if ticks > 1 [ if remainder ticks 48 = 0 [ plotxy ticks ( mean [ITV-BMR] of turtles )]]"
"pen-9" 1.0 0 -13840069 true "" "if ticks > 1 [ if remainder ticks 48 = 0 [ plotxy ticks ( mean [sum stomach-fill / stomach-fill-max] of turtles )]]"
"pen-10" 1.0 0 -14835848 true "" ""
"pen-11" 1.0 0 -11221820 true "" ""

PLOT
1380
616
1798
834
m-tot
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
"BMR" 1.0 2 -16777216 true "" ""
"Loco" 1.0 2 -7500403 true "" ""
"Grow" 1.0 2 -2674135 true "" ""
"Preg" 1.0 2 -955883 true "" ""
"Lact" 1.0 2 -6459832 true "" ""
"m-tot" 1.0 0 -10141563 true "" ""
"HIF" 1.0 2 -14439633 true "" ""

PLOT
1380
834
1800
1018
m-tot-no-prod
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
"BMR" 1.0 2 -16777216 true "" ""
"Loco" 1.0 2 -7500403 true "" ""
"Grow" 1.0 2 -2674135 true "" ""
"Preg" 1.0 2 -955883 true "" ""
"Lact" 1.0 2 -6459832 true "" ""
"m-tot" 1.0 0 -10141563 true "" ""
"HIF" 1.0 2 -10899396 true "" ""

SLIDER
1804
635
1976
668
SL-test
SL-test
0
0.5
0.19
0.01
1
NIL
HORIZONTAL

SLIDER
1804
669
1976
702
tester
tester
0.6
1
1.0
0.01
1
NIL
HORIZONTAL

PLOT
824
915
1008
1065
Litter mass
NIL
NIL
0.0
16.0
0.0
0.03
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1380
466
1580
616
Ingestion rate max
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
"default" 1.0 2 -16777216 true "" ""

PLOT
1006
915
1180
1065
Lactation costs total
NIL
NIL
0.0
16.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
648
915
824
1065
Lactation energy intake
NIL
NIL
0.0
16.0
0.0
10.0
true
false
"" ""
PENS
"max" 1.0 2 -3844592 true "" ""
"default" 1.0 2 -16777216 true "" ""

PLOT
1180
765
1380
915
Mom body mass
Days since birth
NIL
0.0
16.0
20.0
40.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1180
915
1380
1065
Peak energy use by litter size
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

SWITCH
0
211
173
244
resources-dynamic?
resources-dynamic?
0
1
-1000

PLOT
1014
160
1187
310
Move probability
Food debt / daily energy use
NIL
0.0
1.0
-0.1
1.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""
"move" 1.0 2 -5298144 true "" ""

PLOT
830
161
1015
311
Percent moving
Ticks
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 50.0 2 -16777216 true "" ""
"Lact" 1.0 2 -7858858 true "" ""

PLOT
1180
615
1380
765
Mom to litter mass
NIL
NIL
0.0
0.01
0.0
0.01
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
446
933
646
1083
Pup mass by litter size
NIL
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

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

PLOT
249
777
449
927
Pup mass at birth by litter size
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1787
10
1987
160
Storage levels at death
NIL
NIL
0.0
0.4
0.0
10.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 false "" ""

PLOT
1580
465
1780
615
FMR by mass
NIL
NIL
0.0
0.04
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1780
465
1980
615
FMR by state
NIL
NIL
0.0
6.0
0.0
10.0
true
false
"" ""
PENS
"Juvs" 1.0 0 -16777216 true "" ""
"NonLact" 1.0 2 -7500403 true "" ""
"LactnonPreg" 1.0 2 -2674135 true "" ""
"LactPreg" 1.0 2 -955883 true "" ""
"PregnonLact" 1.0 2 -6459832 true "" ""

PLOT
1785
159
1985
309
FMR by activity
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1187
160
1387
310
Activity by state
NIL
NIL
0.0
6.0
0.0
1.0
true
false
"" ""
PENS
"Juvs" 1.0 2 -16777216 true "" ""
"NonLact" 1.0 2 -7500403 true "" ""
"PregnonLact" 1.0 2 -2674135 true "" ""
"LactnonPreg" 1.0 2 -955883 true "" ""
"LactPreg" 1.0 2 -6459832 true "" ""

SWITCH
73
247
173
280
seasonality?
seasonality?
1
1
-1000

INPUTBOX
0
247
74
312
seasonal-var
0.5
1
0
Number

INPUTBOX
1805
704
1865
764
out-year
10.0
1
0
Number

SWITCH
73
279
173
312
overwinter-skip?
overwinter-skip?
0
1
-1000

INPUTBOX
4
698
86
758
surv-mod-embryo
1.2
1
0
Number

INPUTBOX
85
698
175
758
surv-mod-off
0.2
1
0
Number

PLOT
655
312
829
462
Movement speed
NIL
NIL
0.0
1000.0
0.0
10.0
true
false
"" ""
PENS
"default" 5.0 1 -16777216 true "" ""

INPUTBOX
128
338
192
398
ITV-var-growth
0.0
1
0
Number

INPUTBOX
0
338
65
398
ITV-var-BMR
0.0
1
0
Number

INPUTBOX
64
338
129
398
ITV-var-repro
0.0
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
