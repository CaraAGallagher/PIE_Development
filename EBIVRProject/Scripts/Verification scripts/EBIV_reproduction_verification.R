# Cara Gallagher
# February 22nd, 2022
# Energy Budget with Individual Variation project
# Reproduction output verification

##################################################
# Packages:
library(tidyverse)
library(readxl)
library(patchwork)
library(showtext)
library(viridis)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()


##################################################
#### Pregnancy #####

#### fetal growth estimation by age #### 
# from Ożdżeński & Mystkowska 1976 - Acta Theriologica
# using only days 10 - 15 as values from larger fetuses can be "deceptive" per the authors
# max as 1.0 to 2.5 g (1.76 +- 0.01 g) in Koivula et al. 2003 Ecology
fetGrowth <- tibble(
  dev_day = c(10,11,12,13,14,15,20),
  mass_mg_hi = c(14,66,116,214,381,500,2500),
  mass_mg = c(11,41,91,192,323,437,1760),
  mass_mg_lo = c(8,26,61,156,257,376,1000)
)

fetGrowth <- fetGrowth %>% 
  mutate(max_mass = mass_mg_hi / 1000,
         mean_mass = mass_mg / 1000,
         min_mass = mass_mg_lo / 1000
  )

fetGrowth <- fetGrowth %>% 
  select(dev_day, max_mass, mean_mass, min_mass) %>% 
  gather(key = "mass", value = "values", -dev_day)

### model outputs ###

modOut <- read_csv("Data/Verification/Reproduction/EBIVPrototype_GestationalMasses.csv", skip = 17)


modOut <- modOut %>% 
  select(x...1, y...2, y...6) %>% 
  rename(gestDays = x...1, fetMass = y...2, placMass = y...6) %>% 
  filter(gestDays > 0)

pal <- lacroix_palette("Pamplemousse", type = "discrete")

PregVerOut <- 
  ggplot() + 
 # geom_violin(data = modOut, aes(x = as.factor(round(gestDays)), y = fetMass), fill = "#E4AA4E") +
  geom_point(data = modOut, aes(x = gestDays, y = fetMass), col = "#E4AA4E", alpha = 0.25,  position = position_jitter(width = 0.1)) +
  geom_point(data = fetGrowth, aes(x = dev_day, y = values, col = mass, shape = mass), size = 3) +
  scale_colour_grey(name = NULL, labels = c("Maximum", "Mean", "Minimum")) +
  scale_shape(name = NULL, labels = c("Maximum", "Mean", "Minimum")) +
  theme_classic() +
  labs( x = "Day in the gestation period", y = "Fetal mass [g]", fill = NULL) +
  theme(text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = c(0.17, 0.9),
        legend.background = element_blank())
PregVerOut
#ggsave("Figures/Verification/FetalGrowth.png", PregVerOut, height = 4, width = 5.5)


## placental calculation from Mu et al. 2008
placMass <- tibble(
  dsmating = seq(10,20,1),
  plMass = -0.54180+0.07887*dsmating-0.002243*dsmating^2
)

ggplot() + 
  geom_point(data = modOut, aes(x = gestDays, y = placMass), col = "grey50", alpha = .05) +
  geom_point(data = placMass, aes(x = dsmating, y = plMass), size = 2, col = pal[2]) +
  theme_classic() +
  labs( x = "Day in the gestation period", y = "Placental mass [g]", col = NULL)

## number of embryos

modOut <- read_csv("Data/Verification/Reproduction/EBIVPrototype_CountEmbryos.csv", skip = 16)

modOut <- modOut %>% 
  select(x, y) %>% 
  rename(momMass = x, nEmb = y)


ggplot(modOut, aes(x = momMass, y = nEmb)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_classic()

# from txt file (EBIVPrototype_ReproOuts.txt)  

## Gestation costs 

modOut <- read_csv("Data/Verification/Reproduction/EBIVPrototype_GestationCosts.csv", skip = 16)

modOut <- modOut %>% 
  select(x, y) %>% 
  rename(gestDays = x, gestCosts = y)

ggplot(modOut, aes(x = gestDays, y = gestCosts)) + 
  geom_point(col = "grey50", alpha = 0.05) + 
  theme_classic() +
  labs( x = "Day in the gestation period", y = "Gestation costs [J 30min-1]")


#### Pup mass at birth by litter size
# from Mappes and Koskela 2004 - Evolution
# Data pulled using Automeris WebPlotDigitizer
# Rounded to minimize measurement error
sizeMass <- read_csv2("Data/Parameterization/Reproduction/LitterSizePupMass.csv")

sizeMass <- sizeMass %>% 
  rename(litterSize = `Litter size`, pupMass = `Pup mass`)

mod <- lm(pupMass ~ litterSize, sizeMass)

sizeMass <- sizeMass %>% 
  mutate(pred = coef(mod)[[2]] * litterSize + coef(mod)[[1]]) %>% 
  mutate(litterMass = pred * litterSize)



modOutPMLSab <- read_csv("Data/Verification/Reproduction/EBIVPrototype_PupMassAtBirthByLitterSize.csv", skip = 16)

modOutPMLSab <- modOutPMLSab %>% 
  select(x, y) %>% 
  rename(litterSize = x, pupMass = y) %>% 
  mutate(pupMass = pupMass)


ggplot() + 
  geom_point(data = modOutPMLSab, aes (x = litterSize, y = pupMass), col = "grey50", alpha = 0.1) +
  geom_line(data = sizeMass, aes (x = litterSize, y = pred), size = 1.5, col = "maroon") + 
  labs(x = "Litter size [N]", y = "Average pup mass [g]") +
  theme_classic()


#### Lactation #####

### data from Sadowska et al. 2016

fig1Dat <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig1Dat.xlsx")

## body mass of the mother
fig1DatBM <- fig1Dat %>% 
  select(ageGest, Metric, bodyMass) %>% 
  pivot_wider(names_from = Metric, values_from = bodyMass)


# model outputs

modOutBM <- read_csv("Data/Verification/Reproduction/EBIVPrototype_MomBodyMass.csv", skip = 16)

modOutBMd <- modOutBM %>% 
  select(x, y) %>% 
  rename(ageGest = x, bodyMass = y) %>% 
  mutate(ageGestR = round(ageGest), bodyMassR = round(bodyMass / 1) * 1) %>%  
  group_by(ageGestR) %>% 
  mutate(countAG = n()) %>% 
  ungroup() %>% 
  group_by(ageGestR, bodyMassR) %>% 
  summarise(countAG = min(countAG), densBM = n()) %>% 
  mutate(densBM = densBM / countAG)

# colors shown as percent at that day in the gestation period 

fig1A <- ggplot() +
  geom_ribbon(data = fig1DatBM, aes(x = ageGest, ymin = Lo, ymax = Hi), fill ="grey70") +
 # geom_line(data = fig1DatBM, aes(x = ageGest, y = Mean), col = "grey30", size = 1) +
  geom_point(data = modOutBMd, aes(x = ageGestR, y = bodyMassR, color = densBM), size = 1.25, show.legend = F) +
  scale_color_viridis(discrete = FALSE, option = "mako",  direction = -1) + 
  theme_classic() +
  labs( x = NULL, y = "Mother body mass [g]")


## food consumption of the mother
fig1DatFC <- fig1Dat %>% 
  select(ageGest, Metric, foodCons) %>% 
  mutate(foodCons = foodCons * 17.80) %>%   # converted to kJ to compare
  pivot_wider(names_from = Metric, values_from = foodCons)

# model outputs

modOutFC <- read_csv("Data/Verification/Reproduction/EBIVPrototype_LactationEnergyIntake.csv", skip = 17)

modOutFCd <- modOutFC %>% 
  select(x...5, y...6) %>% 
  rename(ageGest = x...5, foodCons = y...6) %>% 
  mutate(foodCons = foodCons * 12.2811) %>% 
  mutate(ageGestR = round(ageGest), foodConsR = round(foodCons / 20) * 20) %>% 
  group_by(ageGestR) %>% 
  mutate(countAG = n()) %>% 
  ungroup() %>% 
  group_by(ageGestR, foodConsR) %>% 
  summarise(countAG = min(countAG), densFC = n()) %>% 
  mutate(densFC = densFC / countAG)

fig1B <- ggplot() +
  geom_ribbon(data = fig1DatFC, aes(x = ageGest - 0.5, ymin = Lo, ymax = Hi), fill ="grey70") +
  geom_point(data = modOutFCd, aes(x = ageGestR, y = foodConsR, color = densFC), size = 1.25, show.legend = F) +
  scale_color_viridis(discrete = FALSE, option = "mako",  direction = -1) + 
  theme_classic() +
  labs( x = NULL, y = "Mother food consumption [kJ day-1]")


## litter total mass 

fig1DatLM <- fig1Dat %>% 
  select(ageGest, Metric, LitterMass) %>% 
  pivot_wider(names_from = Metric, values_from = LitterMass)


# model outputs

modOutLM <- read_csv("Data/Verification/Reproduction/EBIVPrototype_LitterMass.csv", skip = 16)

modOutLMd <- modOutLM %>% 
  select(x, y) %>% 
  rename(ageGest = x, LitterMass = y) %>% 
  mutate(ageGestR = round(ageGest), LitterMassR = round(LitterMass / 3) * 3) %>% 
  group_by(ageGestR) %>% 
  mutate(countAG = n()) %>% 
  ungroup() %>% 
  group_by(ageGestR, LitterMassR) %>% 
  summarise(countAG = min(countAG), densLM = n()) %>% 
  mutate(densLM = densLM / countAG)


fig1C <- ggplot() +
  geom_ribbon(data = fig1DatLM, aes(x = ageGest, ymin = Lo, ymax = Hi), fill ="grey70") +
  geom_point(data = modOutLMd, aes(x = ageGestR, y = LitterMassR, color = densLM), size = 1.25, show.legend = F) +
  scale_color_viridis(discrete = FALSE, option = "mako",  direction = -1) + 
  theme_classic() +
  labs( x = "Day in the gestation period", y = "Litter mass [g]")


fig1Plot <- fig1A / fig1B / fig1C
fig1Plot

## Figure 2 data 

fig2Dat <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig2Dat.xlsx")

## peak ADMR by litter size

fig2DaADMR <- fig2Dat %>% 
  filter(Metric == "ADMR") 


# model outputs

modOutADMR <- read_csv("Data/Verification/Reproduction/EBIVPrototype_PeakEnergyUseByLitterSize.csv", skip = 16)

modOutADMRd <- modOutADMR %>% 
  select(x, y) %>% 
  rename(litterSize = x, ADMR = y) %>% 
  mutate(ADMRR = round(ADMR / 5) * 5) %>% 
  group_by(litterSize) %>% 
  mutate(countLS = n()) %>% 
  ungroup() %>% 
  group_by(litterSize, ADMRR) %>% 
  summarise(countLS = min(countLS), densAMDR = n()) %>% 
  mutate(densAMDR = densAMDR / countLS)

fig2A <- ggplot() +
  geom_line(data= fig2DaADMR, aes(x = litterSize, y = Value), col = "grey70", size = 2) +
  geom_point(data = modOutADMRd, aes(x = litterSize, y = ADMRR, color = densAMDR), size = 1.25, show.legend = F) +
  scale_color_viridis(discrete = FALSE, option = "mako",  direction = -1) + 
  theme_classic() +
  coord_cartesian(xlim=c(1, 9))+
  scale_x_continuous(breaks=seq(1,9,2)) +
  labs( x = "Litter size [N]", y = "Peak average daily metabolic rate [kJ day-1]")


## peak energy transfer by litter size
# calculated as difference between MEI and ADMR in source
# calculated as m-lactation * 48 in the model results for EBIVPrototype_PeakMilkEnergyOutputByLitterSize.csv
# calculated as in the paper for the results in EBIVPrototype_PeakMilkEnergyOutputByLitterSizeByDiff.csv

fig2DaMEO <- fig2Dat %>% 
  filter(Metric == "MEO") 

# model outputs

modOutMEO <- read_csv("Data/Verification/Reproduction/EBIVPrototype_PeakMilkEnergyOutputByLitterSize.csv", skip = 16)

modOutMEOd <- modOutMEO %>% 
  select(x, y) %>% 
  rename(litterSize = x, MEO = y) %>% 
  mutate(MEOR = round(MEO / 10) * 10) %>% 
  group_by(litterSize) %>% 
  mutate(countLS = n()) %>% 
  ungroup() %>% 
  group_by(litterSize, MEOR) %>% 
  summarise(countLS = min(countLS), densMEO = n()) %>% 
  mutate(densMEO = densMEO / countLS)

fig2B <- ggplot() +
  geom_line(data= fig2DaMEO, aes(x = litterSize, y = Value), col = "grey70", size = 2) +
  geom_point(data = modOutMEOd, aes(x = litterSize, y = MEOR, color = densMEO), size = 1.25, show.legend = F) +
  scale_color_viridis(discrete = FALSE, option = "mako",  direction = -1) + 
  theme_classic() +
  coord_cartesian(xlim=c(1, 9))+
  scale_x_continuous(breaks=seq(1,9,2)) +
  labs( x = "Litter size [N]", y = "Peak milk energy output [kJ day-1]")



## peak food consumption by litter size

fig2DaFC <- fig2Dat %>% 
  filter(Metric == "FC")  %>% 
  mutate(Value = Value * 17.80) # converted to kJ to compare

# model outputs

modOutFCp <- read_csv("Data/Verification/Reproduction/EBIVPrototype_PeakFoodConsumptionByLitterSize.csv", skip = 16)


modOutFCpd <- modOutFCp %>% 
  select(x, y) %>% 
  rename(litterSize = x, FCp = y) %>% 
  mutate(FCp = FCp * 12.2811) %>% 
  mutate(FCpR = round(FCp / 20) * 20) %>% 
  group_by(litterSize) %>% 
  mutate(countLS = n()) %>% 
  ungroup() %>% 
  group_by(litterSize, FCpR) %>% 
  summarise(countLS = min(countLS), densFCp = n()) %>% 
  mutate(densFCp = densFCp / countLS)

fig2C <- ggplot() +
  geom_line(data= fig2DaFC, aes(x = litterSize, y = Value), col = "grey70", size = 2) +
  geom_point(data = modOutFCpd, aes(x = litterSize, y = FCpR, color = densFCp), size = 1.25, show.legend = F) +
  scale_color_viridis(discrete = FALSE, option = "mako",  direction = -1) + 
  theme_classic() +
  coord_cartesian(xlim=c(1, 9))+
  scale_x_continuous(breaks=seq(1,9,2)) +
  labs( x = "Litter size [N]", y = "Peak food consumption [kJ day-1]")


fig2Plot <- (fig2C + fig2A + fig2B)  
fig2Plot




#### Pup mass by litter size


# model outputs

modOutPMLS <- read_csv("Data/Verification/Reproduction/EBIVPrototype_PupMassByLitterSize.csv", skip = 16)

modOutPMLS <- modOutPMLS %>% 
  select(x, y) %>% 
  rename(litterSize = x, pupMass = y) %>% 
  mutate(pupMass = pupMass * 1000)


ggplot(modOutPMLS, aes (x = litterSize, y = pupMass)) + 
  geom_point(col = "grey50", alpha = 0.1) +
  geom_smooth(method = "lm", col = "grey50") +
  labs(x = "Litter size [N]", y = "Average pup mass [g]") +
  theme_classic()
 

#### Mom mass to litter mass


# model outputs

modOutMMLM <- read_csv("Data/Verification/Reproduction/EBIVPrototype_MomToLitterMass.csv", skip = 16)

modOutMMLM <- modOutMMLM %>% 
  select(x, y) %>% 
  rename(momMass = x, litterMass = y) %>% 
  mutate(momMass = momMass * 1000, litterMass = litterMass * 1000)


ggplot(modOutMMLM, aes (x = momMass, y = litterMass)) + 
  geom_point(col = "grey50", alpha = 0.1) +
  geom_smooth(method = "lm", col = "grey50") +
  labs(x = "Mom mass [g]", y = "Litter mass [g]") +
  theme_classic()


## Reproduction outputs
# "year" "NReproducing""NLittersMean" "NLittersSD" "NConcievedMean" "NConcievedSD" "NBornMean" "NBornSD" "NWeanedMean" "NWeanedSD" "AbortionRateMean" "AbortionRateSD" "OffspringMortalityMean" "OffspringMortalitySD" 
# [10 72 3.403 1.553 4.229 1.425 3.478 1.714 2.927 1.74 0.17 0.293 0.168 0.298]

reproOuts <- tibble(
  means = c(2.594, 3.889, 3.29, 2.443, 0.141, 0.282),
  sd = c(0.695, 1.268, 1.536, 1.775, 0.295, 0.382),
  metric = c("NLitters", "Conceived", "Born", "Weaned", "AbortionRate", "OffspringMortality"),
  group = c(1,2,2,2,3,3)
)

reproOuts <- reproOuts %>% 
  mutate( metric = factor(metric, levels=c("NLitters", "Conceived", "Born", "Weaned", "AbortionRate", "OffspringMortality")))


reproDat <- tibble (
  means = c(c(4.9,5.27,5,4,4.08,6.1,4.5,3.6), c(4,1.8,1.56,1.28,5.17,2.34,5.28)),
  metric = c(rep("Born", times = 8), rep("Weaned", times = 7))
)

reproStatsPlot <- 
ggplot() + 
  geom_bar(data = subset(reproOuts, group == 2), aes(x = metric, y = means, fill = metric),stat="identity", col = "grey50", size = 1.5) +
  geom_errorbar(data = subset(reproOuts, group == 2), aes(x = metric, ymin = means - sd, ymax = means + sd), col = "grey50", size = 1.5, width = 0.1) +
  geom_jitter(data = reproDat, aes(x = metric, y = means), width = 0.2, size = 3) + 
  theme_classic() +
  scale_fill_manual(values = c(pal[1],pal[3],pal[2])) +
  ylim(0,6) +
  labs( x = NULL, y = "Number of pups [N]") + 
  theme(legend.position = "none")
reproStatsPlot

##### Lactation outs #####

lactOutC <- read_csv("Data/Verification/Reproduction/LactationOutsCosts.csv", skip = 18)
lactOutCF <- read_csv("Data/Verification/Reproduction/LactationOutsCostsFull.csv", skip = 18)
lactOutM <- read_csv("Data/Verification/Reproduction/LactationOutsMasses.csv", skip = 17)
lactOutMF <- read_csv("Data/Verification/Reproduction/LactationOutsMassesFull.csv", skip = 17)


LOCbmr <- 
  lactOutC %>% 
  select(x...1, y...2) %>% 
  rename(dsb = x...1, cost = y...2) %>% 
  mutate(name = "BMR")

LOCgrow <- 
  lactOutC %>% 
  select(x...5, y...6) %>% 
  rename(dsb = x...5, cost = y...6) %>% 
  mutate(name = "Growth")

LOCtot <- 
  lactOutC %>% 
  select(x...9, y...10) %>% 
  rename(dsb = x...9, cost = y...10) %>% 
  mutate(name = "Total")

LOCbmrF <- 
  lactOutCF %>% 
  select(x...1, y...2) %>% 
  rename(dsb = x...1, cost = y...2) %>% 
  mutate(name = "BMR-Full")

LOCgrowF <- 
  lactOutCF %>% 
  select(x...5, y...6) %>% 
  rename(dsb = x...5, cost = y...6) %>% 
  mutate(name = "Growth-Full")

LOCtotF <- 
  lactOutCF %>% 
  select(x...9, y...10) %>% 
  rename(dsb = x...9, cost = y...10) %>% 
  mutate(name = "Total-Full")

lactOutCF <- bind_rows(LOCbmr,LOCgrow,LOCtot)
lactOutCF <- bind_rows(LOCbmr,LOCgrow,LOCtot,LOCbmrF,LOCgrowF,LOCtotF)

ggplot(lactOutCF, aes(x = dsb, y = cost, col = name)) +
  geom_smooth(method = "gam") +
  labs(x = "Days since birth", y = "Cost [J 30min-1]", col = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 20, color = "grey30"),
        axis.line = element_line(color = "grey30"),
        legend.position = "top",
        legend.text = element_text(size = 12)) 

pal <- colorRampPalette(c("#BC4A53","#E4714E","#E4AA4E","#AABC4A","#278192","#3F3F7B"))
pal <- colorRampPalette(c("#9D6918","#E4AA4E","#EECB92"))

textSizeAxisTitle <- 35

ggplot(LOCtot, aes(x = round(dsb), y = cost, col = as.factor(round(dsb)))) + 
  geom_point(position = position_jitter(width = 0.2))+
  labs(x = "Days since birth", y = "Cost of lactation [J 30min-1]", col = element_blank()) +
  theme_classic() +
  scale_color_manual(values = rev(pal(22))) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = "none") 
#ggsave("Figures/Verification/LactationCosts.png", height = 4, width = 7)


LOCTM <- 
  lactOutM %>% 
  select(x...1, y...2) %>% 
  rename(dsb = x...1, mass = y...2) %>% 
  mutate(name = "Total mass")

LOCLM <- 
  lactOutM %>% 
  select(x...5, y...6) %>% 
  rename(dsb = x...5, mass = y...6) %>% 
  mutate(name = "Lean mass")


LOCTMF <- 
  lactOutMF %>% 
  select(x...1, y...2) %>% 
  rename(dsb = x...1, mass = y...2) %>% 
  mutate(name = "Total mass-Full")

LOCLMF <- 
  lactOutMF %>% 
  select(x...5, y...6) %>% 
  rename(dsb = x...5, mass = y...6) %>% 
  mutate(name = "Lean mass-Full")

lactOutM <- bind_rows(LOCTM,LOCLM,LOCTMF,LOCLMF)

ggplot(lactOutM, aes(x = dsb, y = mass, col = name)) +
  geom_smooth(method = "gam") +
  labs(x = "Days since birth", y = "Mass [g]", col = element_blank()) +
  theme_classic() +
  theme(text = element_text(size = 20, color = "grey30"),
        axis.line = element_line(color = "grey30"),
        legend.position = "top",
        legend.text = element_text(size = 12)) 
