# Cara Gallagher
# November 22nd, 2021
# Energy Budget with Individual Variation project
# Body composition parameters

##################################################
# Packages:
library(tidyverse)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################


#### energy content of the new-born vole #### 
# 1.03 Kcal/g from  Górecki 1965 ref in Kaczmarski 1966 - Acta Theriologica

#### fetal body composition #### 
# from Fedyk 1974 and Sawicka-Kapusta 1974 - from animals aged 1 day in g
massFedyk <- c(1.810,1.967)
fatFedyk <- c(0.053,0.070)
waterFedyk <- c(1.527,1.667)
proteinFedyk <- c(0.190,0.203)
percFatFedyk <- fatFedyk / massFedyk * 100
percWaterFedyk <- waterFedyk / massFedyk * 100
percProteinFedyk <- proteinFedyk / massFedyk * 100

massSK <- c(2.13,2.20)
percFatwoWaterSK <- c(24.8,28.5)
percWaterSK <- c(84.5,83.2)
percProteinwoWaterSK <- c(63.0,59.8)
massNoWaterSK <- massSK * (1 - percWaterSK/100)
massFatSK <- massNoWaterSK * (percFatwoWaterSK/100)
massProteinSK <- massNoWaterSK * (percProteinwoWaterSK/100)
percFatSK <- massFatSK / massSK * 100
percProteinSK <- massProteinSK / massSK * 100

percWater <- c(percWaterFedyk, percWaterSK)
percFat <- c(percFatFedyk, percFatSK)
percProtein <- c(percProteinFedyk, percProteinSK)

# mean(percWater)
# mean(percFat)
# mean(percProtein)


#### energy density fat and protein #### 
# from Fedyk 1974 - converted to j per g
EDfat <- 9249.41 * 4.184 
EDpro <- 5583.48 * 4.184 


#### body fat range #### 
# 10.4 to 40.0% in Sawicka-Kapusta 1974 of no water mass
# 2.9 to 25.4 in Fedyk 1974 of whole body mass
# 3.4 to 25.7 for males in Tidhar and Speakman 2007 
# 4.1 to 22.4 for females in Tidhar and Speakman 2007

#### percent protein of lean mass #### 
# from Fedyk 1974
ageFedyk <- c(1,9,22,30,40,60,90,120,150,180,
              1,9,22,30,40,60,90,120,150,180)
percProFedyk <- c(10.9,15.5,20.0,21.3,22.5,23.4,23.3,23.4,23.4,23.2,
                  10.7,13.9,20.9,20.7,22.5,23.4,23.5,23.0,23.4,23.4)
# from Sawicka-Kapusta 1974
ageSW <- c(1,3,6,10,20,30,60,
           1,3,6,10,20,60,
           30,60,90,120,150,195,255,300,390)
percProSW <- c(10.2,11.7,13.6,15.7,16.9,17.6,18.9,
               10.5,11.0,13.3,15.1,16.4,19.0,
               19.1,20.0,19.6,20.3,22.2,22.3,22.0,21.3,21.3)
proteinPercLM <- tibble(
  age = c(ageFedyk,ageSW),
  percPro = c(percProFedyk,percProSW) / 100)

# fit using a von bertalanffy curve
fit.bertalanffy <- function(age, data) {                                 # create von bertalanffy fitting function
  d <- data.frame(age, data) 
  # pick starting values
  
  starting.values <- c(                                                  # starting values
    A=A1,                                                                # asymptotic starting mass set to max in dataset
    k=k1,
    to = to1)                                           
  # print("Starting Values for Optimization: ")
  # print(starting.values)
  
  formula.bertalanffy <- "data~A*(1 - exp((-k)*(age-to)))"               # fit using standard gompertz curve
  nls(formula.bertalanffy, d, starting.values)                           # use nls to fit curve using data and specified starting values
}

pots <- seq(0.000001, 0.020001, 0.0001)

for (i in pots) {
  tryCatch({
    k1 = i
    A1 =  max(proteinPercLM$percPro) 
    to1 = max(proteinPercLM$age)
    No1 = min(proteinPercLM$percPro)
    fit <- fit.bertalanffy(proteinPercLM$age, proteinPercLM$percPro)  # run formula
    # print(i)
  }, error=function(e){})
}

proteinPercLM <- proteinPercLM %>% 
  mutate(predProPerc = summary(fit)$coefficients[1,1]*(1 - exp(-summary(fit)$coefficients[2,1]*(age-(summary(fit)$coefficients[3,1])))))

proLMPlot <- 
  ggplot(proteinPercLM, aes(x = age)) +
  geom_line(aes(y = predProPerc * 100), size = 2, col = "#AABC4A") +
  geom_point(aes(y = percPro * 100), size = 3.5, col = "grey50") +
  labs(x = "Age [days]", y = "Lean mass protein content [%]")  +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey10", family = "Montserrat"),
        legend.position = "none")

proLMPlot

#ggsave("Figures/ODDplots/LMPercPro.png", proLMPlot, height = 4)

#### deposition efficiency of protein and fat #### 
# from Pullar and Webster 1977 for rats
DEproteinPW <- 1/2.25
DEfatPW <- 1/1.36

#### energy partitioning of protein and fat for catabolism #### 
# in Belkhou et al. 1991 - Nutrition Research
# final end points in stage III: 25% of initial protein stores, 95% loss of fat, adiposity decreased from 8% to 0.6% 
# in stage II: proteins contributed 17%, fats were 83%
# in stage III: proteins contributed 37%, fats were 63%

fatPerc <- c(mean(c(6.2,2.6)),mean(c(2.6,0.6)))/100 # as halfway point between stage transitions
proUse <- c(0.17,0.37) * 100
fatUse <- c(0.83,0.63) * 100

# from Dunn et al. 1982 -The Journal of nutrition
# from obese day 9, 31, 34 & nonobese day 9
bodyMass <- c(549,427,253,408,285)  # used closest provided values
lipidMassInit <- c(315,204,258,48)
lipidMassFinal <- c(239,45,51,4.0)
fatPerc <- c(fatPerc,
            mean(c((lipidMassInit[1] / bodyMass[1]), (lipidMassFinal[1] / bodyMass[2]))),
            mean(c((lipidMassInit[2] / bodyMass[1]), (lipidMassFinal[2] / bodyMass[3]))),
            mean(c((lipidMassInit[3] / bodyMass[1]), (lipidMassFinal[3] / bodyMass[3]))),
            mean(c((lipidMassInit[4] / bodyMass[4]), (lipidMassFinal[4] / bodyMass[5])))
                 ) # as halfway point
proUse <- c(proUse,(1.3/80)*100,(1.5/50)*100,(1.3/58)*100,(7/51)*100)
fatUse <- c(fatUse,(79/80)*100,(48/50)*100,(56/58)*100,(44/51)*100)

# Cherel et al. 1992 - Journal of Comparative Physiology B
# from lean stage II & III & obese stage II
fatPerc <- c(fatPerc,mean(c(3.1,9.2)) / 100,1.3 / 100,mean(c(26.6,44.2)) / 100)  # used closest provided values
proUse <- c(proUse,27.7,31.4,8.0)
fatUse <- c(fatUse,77.3,68.1,91.9)

## fitting curve to data
# basing curve on eqn 8 in Caloin 2004
# using energy density of protein and lipid from Fedyk 1974
cP <- 23.4
cL <- 38.7

allDat <- tibble(
  fatPerc,
  proUse = proUse / 100,
  fatUse,
  cP,
  cL
)

mod <- nls(proUse ~ cP * gamma / (cL * fatPerc + cP * gamma), data = allDat, start = list(gamma = 0.045), trace = T)
coef(mod)[1]

curve <- tibble(
  fatPerc = seq(0.01,0.61,0.01) )
curve <- curve %>% 
  mutate(predict = cP * coef(mod)[1] / (cL * fatPerc + cP * coef(mod)[1]))

label1 <- expression(P[ratio]~'='~23.4~'*'~0.015~'/'~(38.7~'*'~SL~'+'~23.4~'*'~0.015))

proMobPlot <- ggplot() +
  geom_point(data = allDat, aes(x = fatPerc, y = proUse), size = 3.5, col = "grey50") +
  geom_line(data = curve, aes(x = fatPerc, y = predict), size = 1.5, col = "darkblue") +
  theme_classic() +
  labs(x = "Adiposity (%/100)", y = "Protein contribution to energy mobilization (%/100)") +
 # annotate(geom="text", x=0.4, y=0.3, label=label1, parse = TRUE, color="darkblue")+
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
proMobPlot



#### fat percent water #### 
# DiGirolamo and Owens 1976 - AMERICAN JOURNAL OF PHYSIOLOGY 
# in rats of different adiposity
fatWaterDiGi <- c(21,15,8,7,8,9,9)
# Reinoso et al. 1997 - Journal of Pharmacological and Toxicological Methods 
# in rats
fatWaterReinoso <- c(18.3)
meanFatWater <- mean(c(fatWaterDiGi,fatWaterReinoso))

#### fat percent by age #### 
# from Sawicka-Kapusta 1974
fatPercAge <- tibble(
  ageDays = c(1,3,6,10,20,30,60,1,3,6,10,20,60),
  mass = c(2.13,2.54,4.2,5.80,8.58,15.61,17.43,2.2,2.3,4.93,5.33,7.86,18.40),
  fat = c(0.08,0.14,0.29,0.51,0.73,2.35,3.34,0.10,0.11,0.50,0.62,0.91,2.88),
  group = c(rep("lab", times = 7), rep("pen", times = 6))
)

fatPercAge <- fatPercAge %>% 
  mutate(fatPerc = fat / mass) 

ggplot(fatPercAge, aes( x = ageDays, y = fatPerc, col = group)) + 
  geom_point()


### ODD plot for pro-storage-perc and storage level
PSPplot <- tibble(
  SL = seq(0.0,0.567,0.001),
  PSP = ((23.5 * 1000 * 1000) * 0.015) / (((39.1	* 1000 * 1000) * SL) + ((23.5 * 1000 * 1000) * 0.015))
)

PSPplot <- ggplot(PSPplot, aes(x = SL * 100, y = PSP * 100)) +
  geom_line(col = "#278192", size = 2) +
  geom_point(data = allDat, aes(x = fatPerc  * 100, y = proUse  * 100), size = 3.5, col = "grey50") +
  labs(y = "Protein contribution to fuel ratio [%]", x = "Body fat [%]") +
#  xlim(0, 40) +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey10", family = "Montserrat"),
        legend.position = "none")


#ggsave("Figures/ODDplots/proStoragePerc.png", PSPplot, height = 4)

