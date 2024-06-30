# Cara Gallagher
# December 10th, 2021
# Energy Budget with Individual Variation project
# Mortality related parameters

##################################################
# Packages:
library(tidyverse)


##################################################


#### average abortions #### 
# in Brambell & Rowlands, 1936
# in Nyholm & Meurling 1979 
# in Buchalczyk 1970 - "Relatively few young (1.6%) died at birth; 64.5% of all deaths to 21 days took place in the first 8 days post partum"

#### maximum lifespan (days) #### 
# 720 in Buchalczyk 1970
# 300 to 630 in the wild Rudolf et al. 2017 - refs Bernshtein et al., 1999; Petrusewicz, 1983
# 450 in Sawicka-Kapusta 1974
# 680 in Balčiauskienė 2007
LF <- mean(c(720,630,450,680))

#### average lifespan in the wild (days) #### 
# 90 in Rudolf et al. 2017

#### overwinter mortality #### 
# 60 and 63.2% - Koskela, 1998 for for male and female offspring from control litters which survived from weaning to breeding season
# 4.5 and 20.7% - Oksanen et al., 2001 for males and female offspring, respectively, from control litters which survived from weaning until the next breeding season
# 11.2% and 22.4% - Kallio et al., 2007 for males and female voles, respectively, which were PUUV -
# 13.9% and 21.9% - Boratyński et al. 2010 for males and females, respectively
# 31, 30, 21, 20% and 18, 17, 10, 10% - Haapakoski et al 2012 for females and males, respectively, from  November until the end of April

winterSurvivalMales <- c(0.60,0.045,0.112,0.139,0.18,0.17,0.10,0.10)
winterSurvivalFemales <- c(0.632,0.207,0.224,0.219,0.31,0.30,0.21,0.20)
# mean(winterSurvivalMales)
# range(winterSurvivalMales)
# mean(winterSurvivalFemales)
# range(winterSurvivalFemales)




