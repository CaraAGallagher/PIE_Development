# Cara Gallagher
# March 5th, 2024
# Energy Budget with Individual Variation project
# Analyzing output files for NDVI scenarios 

##################################################

library(tidyverse)
library(showtext)
library(beepr)
library(patchwork)
library(ggfortify)


# load typeface and color palette
font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()
pal <- colorRampPalette(c("#3F3F7B","#278192","#00B089","#AABC4A", "#E5DC54","#E4AA4E","#E4714E","#BC4A53"))
textSizeAxisTitle <- 40
textSizeAxisText <- 30


##################################################

# read in outputs
NDVIOuts <- read.csv("../../MetabolicVariationLargeOuts/NDVI_IA_Shifted.csv", skip = 6)


NDVIOuts <- NDVIOuts %>%  select(c(1,8,10,20:23)) 


colnames(NDVIOuts) <- c("run.num", 
                        "site", 
                        "experiment", 
                        "final.TS", 
                        "day.outs", 
                        "year.outs", 
                        "abun.out")

NDVIOuts <- NDVIOuts %>%  filter(final.TS >= 800000) 

NDVIOuts <- NDVIOuts %>% 
  ungroup() %>% 
  mutate(site = case_when(
    site == 1 ~ "Asturias",
    site == 2 ~ "Bielowieza",
    site == 3 ~ "Calabria",
    site == 4 ~ "Evenstad",
    site == 5 ~ "Frýdek-Místek",
    site == 6 ~ "Havelaue",
    site == 7 ~ "Konnovesi",
    site == 8 ~ "Le Quartier",
    site == 9 ~ "Pallasjarvi",
    site == 10 ~ "Stroemsund"
  ))

NDVIOuts <- NDVIOuts %>% 
  ungroup() %>% 
  mutate(
    site = fct_relevel(
      site,
      c("Pallasjarvi", "Stroemsund", "Konnovesi", "Evenstad", "Havelaue", "Bielowieza", "Frýdek-Místek", "Le Quartier", "Asturias", "Calabria")
    )
  )

study_sites_lats <- data.frame(
  site = c("Stroemsund", 
           "Evenstad", 
           "Bielowieza", 
           "Pallasjarvi", 
           "Asturias", 
           "Le Quartier", 
           "Havelaue", 
           "Calabria", 
           "Frýdek-Místek", 
           "Konnovesi"),
  Latitude = c(64.083333,
               61.500000,
               52.666667,
               68.000000,
               43.000000,
               46.140000,
               52.840000,
               39.350000,
               49.700000,
               62.616667))


nam <- c(as.character(1:1100))

### useful functions ###

reshuffleDaily <- function(data, met) {
  
  data %>% 
    mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
    group_by(season) %>% 
    drop_na() %>% 
    mutate(mean = mean({{met}})) %>% 
    group_by(season, site) %>% 
    mutate({{met}} := mean({{met}})) %>%
    select(-run.num, -day) %>% 
    distinct() %>% 
    mutate(diff = (({{met}} - mean)/mean) * 100) %>%
    ungroup() %>% 
    mutate(
      season = fct_relevel(
        season,
        c("Spring", "Summer", "Fall")
      )
    ) 
  
}


reshuffleYearly <- function(data, met) {
  
  data %>%  
    group_by(site) %>% 
    drop_na() %>% 
    summarise(Skew = skewness({{met}}), 
              Kurtosis = kurtosis({{met}}), 
              Average = mean({{met}})) %>% 
    mutate(meanSkew = mean(Skew),
           meanKurt = mean(Kurtosis),
           mean = mean(Average)) %>% 
    mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
           Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
           Average = ((Average - mean)/mean) * 100) %>%
    select(-meanSkew, -meanKurt, -mean) %>% 
    pivot_longer(-site, names_to = "metric") %>% 
    mutate(
      metric = fct_relevel(
        metric,
        c("Average", "Skew", "Kurtosis")
      )
    )
}

plotInitYear <- function(data, lab, min, max) {
  
  ggplot(data, aes(x = season, y = diff)) + 
    geom_hline(yintercept = 0, col = "grey30") +
    geom_point(aes(fill = site), size = 2.5, position = position_dodge2(width = 0.8, reverse = TRUE), shape = 21, col = "grey30", stroke = 1.1) +
    labs(x = NULL, y = lab, col = "Site") + 
    scale_fill_manual(values = pal(10)) +
    ylim(min, max) +
    geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.05), label = paste0("rho=", cors)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") + 
    geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.2), label = paste0("p=", p)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") +
    theme_classic()+  
    theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.3),
          legend.position = "none")
  
}

plotMetsInitYear <- function(data, lab, min, max) {
  
  ggplot(data, aes(x = metric, y = value)) + 
    geom_hline(yintercept = 0, col = "grey30") +
    geom_point(aes(fill = site), size = 2.5, position = position_dodge2(width = 0.8, reverse = TRUE), shape = 21, col = "grey30", stroke = 1.1) +
    labs(x = NULL, y = lab, col = "Site") + 
    scale_fill_manual(values = pal(10)) +
    theme_classic()+  
    ylim(min, max) +
    geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.05), label = paste0("rho=", cors)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") + 
    geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.2), label = paste0("p=", p)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") +
    theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"), 
          legend.position = "none",
          axis.title.y = element_text(lineheight = 0.3))
}



# calculating spearman rank coefficient correlations 
seasons <- c("Spring", "Summer", "Fall") 
metrics <- c("Average", "Skew", "Kurtosis")
outputs <- c("LRS", "ageDeath", "age1stBirth", "LPY")

corFunct <- function(a, b, c) {
  cors <- tibble(
    season = as.character(),
    cors = as.numeric(),
    p = as.numeric() 
  )
  
  for (i in c) {
    t <- a %>% 
      filter(season == i) %>% 
      left_join(study_sites_lats, by = "site")
    
    c <- cor.test(t$Latitude, t[[b]],  method = "spearman")
    ce <- round(c$estimate, digits = 2)
    cp <- round(c$p.value, digits = 2)
    
    corsI <- tibble(season = i, cors = ce, p = cp)
    
    cors <- bind_rows(cors, corsI)
  }
  
  cors
}

corFunctMets <- function(a, b, c) {
  cors <- tibble(
    metric = as.character(),
    cors = as.numeric(),
    p = as.numeric() 
  )
  
  for (i in c) {
    t <- a %>% 
      filter(metric == i) %>% 
      left_join(study_sites_lats, by = "site")
    
    c <- cor.test(t$Latitude, t[[b]],  method = "spearman")
    ce <- round(c$estimate, digits = 2)
    cp <- round(c$p.value, digits = 2)
    
    corsI <- tibble(metric = i, cors = ce, p = cp)
    
    cors <- bind_rows(cors, corsI)
  }
  
  cors
}

#### Day-specific outputs ####

dayOuts <- NDVIOuts %>% select(run.num, site, day.outs)

dayOuts <- dayOuts %>% 
  separate(day.outs, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:11, times = (max(row_number()) / 11)), num = rep(1:(max(row_number()) / 11), each = 11)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(year = `1`, day = `2`, bodyMass = `3`, bodyCondition = `4`, FMR = `5`, loco = `6`, repro = `7`, growth = `8`, LSW = `9`, neoBM = `10`, weanBM = `11`) 

dayOuts <- dayOuts %>% 
  filter(year == 23)

#### Body mass ####

BM <- dayOuts %>%  select(run.num, site, day, bodyMass)

BM <- reshuffleDaily(BM, bodyMass)
  
# calculating spearman rank coefficient correlations 
cors <- corFunct(BM, "bodyMass", seasons)

# plot outputs

BMH <- plotInitYear(BM, "Body mass", min(BM$diff) - 1, max(BM$diff) + 5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())


#### Body condition ####

BC <- dayOuts %>%  select(run.num, site, day, bodyCondition)

BC <- reshuffleDaily(BC, bodyCondition)

# calculating spearman rank coefficient correlations 
cors <- corFunct(BC, "bodyCondition", seasons)

# plot outputs

BCH <- plotInitYear(BC, "bodyCondition", min(BC$diff) - 1, max(BC$diff) + 5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())



#### FMR ####

FMROut <- dayOuts %>%  select(run.num, site, day, FMR)

FMROut <- reshuffleDaily(FMROut, FMR)

# calculating spearman rank coefficient correlations 
cors <- corFunct(FMROut, "FMR", seasons)

# plot outputs

FMRH <- plotInitYear(FMROut, "FMR", min(FMROut$diff) - 1, max(FMROut$diff) + 5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())



#### Loco ####

LocoOut <- dayOuts %>%  select(run.num, site, day, loco)

LocoOut <- reshuffleDaily(LocoOut, loco)

# calculating spearman rank coefficient correlations 
cors <- corFunct(LocoOut, "loco", seasons)

# plot outputs

LocoH <- plotInitYear(LocoOut, "loco", min(LocoOut$diff) - 1,  max(LocoOut$diff) + 10) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())


#### Repro ####

ReproOut <- dayOuts %>%  select(run.num, site, day, repro)

ReproOut <- reshuffleDaily(ReproOut, repro)

# calculating spearman rank coefficient correlations 
cors <- corFunct(ReproOut, "repro", seasons)

# plot outputs

ReproH <- plotInitYear(ReproOut, "repro",  min(ReproOut$diff) - 1, max(ReproOut$diff) + 5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())


#### Growth ####

GrowthOut <- dayOuts %>%  select(run.num, site, day, growth)

GrowthOut <- reshuffleDaily(GrowthOut, growth)

# calculating spearman rank coefficient correlations 
cors <- corFunct(GrowthOut, "growth", seasons)

# plot outputs

GrowthH <- plotInitYear(GrowthOut, "growth", min(GrowthOut$diff) - 1, max(GrowthOut$diff) + 1) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())



#### Litter size at weaning ####

LSWOut <- dayOuts %>%  select(run.num, site, day, LSW)

LSWOut <- reshuffleDaily(LSWOut, LSW)

# calculating spearman rank coefficient correlations 
cors <- corFunct(LSWOut, "LSW", seasons)

# plot outputs

LSWH <- plotInitYear(LSWOut, "LSW", min(LSWOut$diff) - 1, max(LSWOut$diff) + 5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())


#### Neonate body mass ####

neoBMOut <- dayOuts %>%  select(run.num, site, day, neoBM)

neoBMOut <- reshuffleDaily(neoBMOut, neoBM)

# calculating spearman rank coefficient correlations 
cors <- corFunct(neoBMOut, "neoBM", seasons)

# plot outputs

neoBMH <- plotInitYear(neoBMOut, "neoBM", min(neoBMOut$diff) - 1, max(neoBMOut$diff) + 5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())


#### Weaning body mass ####

weanBMOut <- dayOuts %>% select(run.num, site, day, weanBM)

weanBMOut <- reshuffleDaily(weanBMOut, weanBM)

# calculating spearman rank coefficient correlations 
cors <- corFunct(weanBMOut, "weanBM", seasons)

# plot outputs
weanBMH <- plotInitYear(weanBMOut, "weanBM", min(weanBMOut$diff) - 1, max(weanBMOut$diff) + 5) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())


#### Combined daily plots #### 
BMH
BCH 
FMRH
LocoH
ReproH
GrowthH
LSWH
neoBMH
weanBMH

#### Year-specific outputs ####

nam <- c(as.character(1:1000))

yearOuts <- NDVIOuts %>%  select(run.num, site, year.outs)

yearOuts <- yearOuts %>% 
  separate(year.outs, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:5, times = (max(row_number()) / 5)), num = rep(1:(max(row_number()) / 5), each = 5)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(year = `1`, LRS = `2`, age1stBirth = `3`, LPY = `4`, ageDeath = `5`) 

yearOuts <- yearOuts %>% 
  filter(year == 23)

# other dist stats 
library(moments)


#### Lifetime reproductive success ####

# initial year 
LRSOuts <- yearOuts %>% select(run.num, site, LRS)

LRSmets <- reshuffleYearly(LRSOuts, LRS)

avgLRSH <- LRSOuts %>%  
  group_by(site) %>% 
  summarise(LRS = mean(LRS, na.rm = TRUE))

# for comb plot 
LRSOuts <- LRSmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(LRS = value)


#### Age at death ####

# initial year 
ageDeathOuts <- yearOuts %>%  select(run.num, site, ageDeath)

ageDeathmets <- reshuffleYearly(ageDeathOuts, ageDeath)

avgADH <- ageDeathOuts %>%  
  group_by(site) %>% 
  summarise(ageDeath = mean(ageDeath, na.rm = TRUE))

# for comb plot 
ageDeathOuts <- ageDeathmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(ageDeath = value)


#### Age at first birth ####

# initial year 
age1stBirthOuts <- yearOuts %>%  select(run.num, site, age1stBirth)

age1stBirthmets <- reshuffleYearly(age1stBirthOuts, age1stBirth)

avgA1BH <- age1stBirthOuts %>%  
  group_by(site) %>% 
  summarise(age1stBirth = mean(age1stBirth, na.rm = TRUE))

# for comb plot 
age1stBirthOuts <- age1stBirthmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(age1stBirth = value)


#### Litters per year ####

LPYOuts <- yearOuts %>%  select(run.num, site, LPY)

LPYmets <- reshuffleYearly(LPYOuts, LPY)

avgLPYH <- LPYOuts %>%  
  group_by(site) %>% 
  summarise(LPY = mean(LPY, na.rm = TRUE))

# for comb plot 
LPYOuts <- LPYmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(LPY = value)


#### Life history plots ####

LH_H <- avgLRSH %>% 
  left_join(avgADH, by = "site") %>% 
  left_join(avgA1BH, by = "site") %>% 
  left_join(avgLPYH, by = "site") 

LH_H <- LH_H %>%  
  pivot_longer(cols = 2:5, names_to = "metric", values_to = "value")

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(LH_H, "value", outputs)

LH_H <- LRSOuts %>% 
  left_join(ageDeathOuts, by = "site") %>% 
  left_join(age1stBirthOuts, by = "site") %>% 
  left_join(LPYOuts, by = "site") 

LH_H <- LH_H %>%  
  pivot_longer(cols = 2:5, names_to = "metric", values_to = "value")


# plot outputs
max <- 10
min <- -10

LHHplot <- ggplot(LH_H, aes(x = metric, y = value)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(aes(fill = site), size = 2, position = position_dodge2(width = 0.8, reverse = TRUE), shape = 21, col = "grey30", stroke = 1.1) +
  stat_smooth(method = "lm", se = FALSE) + 
  labs(x = NULL, y = "Difference from\naverage [%]", col = "Site") + 
  scale_fill_manual(values = pal(10)) +
  scale_x_discrete(labels = c("Age at\nfirst birth", "Longevity", "Litters\nper year", "Lifetime\nreproductive\nsuccess")) +
 # ylim(min, max) +
  geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.05), label = paste0("rho=", cors)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") + 
  geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.2), label = paste0("p=", p)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.3), 
        legend.position = "none")




#### Population abundance ####

abun <- NDVIOuts %>%  select(run.num, site, experiment, abun.out)

abun <- abun %>% 
  separate(abun.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site, -experiment) %>% 
  #  gather(key = "num", value = "value", -run.num, -site, -experiment, -model) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:4, times = (max(row_number()) / 4)), num = rep(1:(max(row_number()) / 4), each = 4)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, year = `2`, adults = `3`, offspring = `4`)

abun <- abun %>% 
  group_by(run.num, site, experiment) %>% 
  mutate(days = day + ((year - 1) * 365)) %>% 
  select(-run.num) %>% 
  pivot_longer(cols = c(adults, offspring), names_to = "state", values_to = "N") 


abunProjPlot <- abun %>% 
  #filter(state == "adults") %>% 
  group_by(run.num, site, year, days) %>% 
  summarise(tot = sum(N)) %>% 
  ggplot(aes(x = year, y = tot / 10, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Year", y = "Density [N ha-1]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
 # scale_x_continuous(labels = c(2020,2040,2060,2080,2100)) +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "top")


abun %>% 
  #filter(state == "adults") %>% 
  group_by(run.num, site, year, days) %>% 
  summarise(tot = sum(N)) %>% 
  ggplot(aes(x = year, y = tot / 10, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Year", y = "Density [N ha-1]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  # scale_x_continuous(labels = c(2020,2040,2060,2080,2100)) +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "top")

  
  
  



