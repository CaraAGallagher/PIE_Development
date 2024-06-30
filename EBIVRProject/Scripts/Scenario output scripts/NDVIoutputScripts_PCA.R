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
#NDVIOuts <- read.csv("../../MetabolicVariationLargeOuts/NDVI_585-10250.csv", skip = 6)
#NDVIOuts <- read.csv("../../MetabolicVariationLargeOuts/NDVI_245-10324.csv", skip = 6)
NDVIOuts <- read.csv("../../MetabolicVariationLargeOuts/NDVI_IA-10624.csv", skip = 6)

#NDVIOuts <- NDVIOuts %>%  select(c(1,17:47)) 
NDVIOuts <- NDVIOuts %>%  select(c(1,12,14,29:56)) 

colnames(NDVIOuts) <- c("run.num", 
                        "site", 
                        "experiment", 
                      #  "model", 
                        "final.TS", 
                        "BM.out.base", 
                        "BC.out.base", 
                        "LSW.out.base", 
                        "neo.BM.out.base", 
                        "wean.BM.out.base", 
                        "LRS.out.base", 
                        "age.at.death.out.base", 
                        "age.1st.birth.out.base", 
                        "LPY.out.base", 
                        "FMR.out.base", 
                        "Loco.out.base", 
                        "repro.out.base", 
                        "growth.out.base", 
                        
                        "BM.out", 
                        "BC.out", 
                        "LSW.out", 
                        "neo.BM.out", 
                        "wean.BM.out", 
                        "LRS.out", 
                        "age.at.death.out", 
                        "age.1st.birth.out", 
                        "LPY.out", 
                        "abun.out",
                        "FMR.out", 
                        "Loco.out", 
                        "repro.out", 
                        "growth.out" 
                        )

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

nam <- c(as.character(1:3000))
namOff <- c(as.character(1:10000))

# set up PCA table
PCAdat <- tibble(
  site = c("Pallasjarvi", "Stroemsund", "Konnovesi", "Evenstad", "Havelaue", "Bielowieza", "Frýdek-Místek", "Le Quartier", "Asturias", "Calabria")
)
PCAdat <- PCAdat %>% 
  mutate(
    site = fct_relevel(
      site,
      c("Pallasjarvi", "Stroemsund", "Konnovesi", "Evenstad", "Havelaue", "Bielowieza", "Frýdek-Místek", "Le Quartier", "Asturias", "Calabria")
    )
  )

# for projections
PCAdatProj <- tibble(
  site = c("Pallasjarvi", "Stroemsund", "Konnovesi", "Evenstad", "Havelaue", "Bielowieza", "Frýdek-Místek", "Le Quartier", "Asturias", "Calabria")
)
PCAdatProj <- PCAdatProj %>% 
  mutate(
    site = fct_relevel(
      site,
      c("Pallasjarvi", "Stroemsund", "Konnovesi", "Evenstad", "Havelaue", "Bielowieza", "Frýdek-Místek", "Le Quartier", "Asturias", "Calabria")
    )
  )

### useful functions ###

# calculating spearman rank coefficient correlations 
seasons <- c("Spring", "Summer", "Fall") 
metrics <- c("Average", "Skew", "Kurtosis")
outputs <- c("LRS", "AgeDeath", "Age1stBirth", "LPY")

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

# plotting functions
# initial year

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

plotFinalYear <- function(data, min, max) {
  
  ggplot(data, aes(x = season, y = diff)) + 
    geom_hline(yintercept = 0, col = "grey30") +
    geom_point(aes(fill = site), size = 2.5, position = position_dodge2(width = 0.8, reverse = TRUE), shape = 21, col = "grey30", stroke = 1.1) +
    labs(x = NULL, y = NULL, col = "Site") + 
    scale_fill_manual(values = pal(10)) +
    theme_classic()+  
    ylim(min, max) +
    geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.05), label = paste0("rho=", cors)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") + 
    geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.2), label = paste0("p=", p)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") +
    theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank())

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

plotMetsFinalYear <- function(data, min, max) {
  
  ggplot(data, aes(x = metric, y = value)) + 
    geom_hline(yintercept = 0, col = "grey30") +
    geom_point(aes(fill = site), size = 2.5, position = position_dodge2(width = 0.8, reverse = TRUE), shape = 21, col = "grey30", stroke = 1.1) +
    labs(x = NULL, y = NULL, col = "Site") + 
    scale_fill_manual(values = pal(10)) +
    theme_classic()+  
    ylim(min, max) +
    geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.05), label = paste0("rho=", cors)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") + 
    geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.2), label = paste0("p=", p)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") +
    theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"), 
          legend.position = "none",
          axis.title.y=element_blank(),
          axis.text.y = element_blank())
  
  }

NDVIOuts <- NDVIOuts %>% 
  group_by(site) %>%
  slice_sample(n = 100)


# go through outputs to process results

#### Body mass ####

BM <- NDVIOuts %>%  select(run.num, site, BM.out.base)

BM <- BM %>% 
  separate(BM.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, BM = `2`) 



# testing grouping by season and taking difference from average 
BMbins <- BM %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  drop_na() %>% 
  mutate(meanBM = mean(BM)) %>% 
  group_by(season, site) %>% 
  mutate(BM = mean(BM)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((BM - meanBM)/meanBM) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  ) 

# calculating spearman rank coefficient correlations 
cors <- corFunct(BMbins, "BM", seasons)

# plot outputs

BMH <- plotInitYear(BMbins, "Body mass", -7, 15) + 
  labs(title = "Year 2023") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())
  
# PCA outs 
avgBM <- BMbins %>% 
  select(-meanBM, -diff) %>% 
  pivot_wider(names_from = season, values_from = BM) %>% 
  select(-Summer, -Fall) %>% 
  rename(BM_Spring = Spring)

PCAdat <- PCAdat %>% 
  left_join(., avgBM, by = join_by(site))

# final year 

BM <- NDVIOuts %>%  select(run.num, site, BM.out)

BM <- BM %>% 
  separate(BM.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, BM = `2`) 

# testing grouping by season and taking difference from average 
BMbins <- BM %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanBM = mean(BM)) %>% 
  group_by(season, site) %>% 
  mutate(BM = mean(BM)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((BM - meanBM)/meanBM) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(BMbins, "BM", seasons)

# plot results
BMF <- plotFinalYear(BMbins, -7, 15) + 
  labs(title = "Year 2099") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())

# PCA outs 
avgBM <- BMbins %>% 
  select(-meanBM, -diff) %>% 
  pivot_wider(names_from = season, values_from = BM) %>% 
  select(-Summer, -Fall) %>% 
  rename(BM_Spring = Spring)

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgBM, by = join_by(site))

#### Body condition ####

# initial year
BC <- NDVIOuts %>%  select(run.num, site, BC.out.base)

BC <- BC %>% 
  separate(BC.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, BC = `2`) 

BCbins <- BC %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanBC = mean(BC)) %>% 
  group_by(season, site) %>% 
  mutate(BC = mean(BC)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((BC - meanBC)/meanBC) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(BCbins, "BC", seasons)

# plot outputs
BCH <- plotInitYear(BCbins, "Body fat", -10, 25) + theme(axis.text.x=element_blank())

# final year
BC <- NDVIOuts %>% select(run.num, site, BC.out)

BC <- BC %>% 
  separate(BC.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, BC = `2`) 


BCbins <- BC %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanBC = mean(BC)) %>% 
  group_by(season, site) %>% 
  mutate(BC = mean(BC)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((BC - meanBC)/meanBC) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(BCbins, "BC", seasons)

# plot outputs
BCF <- plotFinalYear(BCbins, -10, 25) + theme(axis.text.x=element_blank())


#### Litter size at weaning ####

# initial year

LSW <- NDVIOuts %>%  select(run.num, site, LSW.out.base)

LSW <- LSW %>% 
  separate(LSW.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, LSW = `2`) 

LSWbins <- LSW %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanLSW = mean(LSW)) %>% 
  group_by(season, site) %>% 
  mutate(LSW = mean(LSW)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((LSW - meanLSW)/meanLSW) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(LSWbins, "LSW", seasons)

# plot outputs
LSWH <- plotInitYear(LSWbins, "Litter size\nat weaning", -12, 12) + theme(axis.text.x=element_blank())

# PCA outs 
avgLSW <- LSWbins %>% 
  select(-meanLSW, -diff) %>% 
  pivot_wider(names_from = season, values_from = LSW) %>% 
  select(-Spring, -Fall) %>% 
  rename(LSW_Summer = Summer)

PCAdat <- PCAdat %>% 
  left_join(., avgLSW, by = join_by(site))


# final year 

LSW <- NDVIOuts %>%  select(run.num, site, LSW.out)

LSW <- LSW %>% 
  separate(LSW.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, LSW = `2`) 

LSWbins <- LSW %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanLSW = mean(LSW)) %>% 
  group_by(season, site) %>% 
  mutate(LSW = mean(LSW)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((LSW - meanLSW)/meanLSW) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(LSWbins, "LSW", seasons)

# plot outputs
LSWF <- plotFinalYear(LSWbins, -12, 12) + theme(axis.text.x=element_blank())

# PCA outs 
avgLSW <- LSWbins %>% 
  select(-meanLSW, -diff) %>% 
  pivot_wider(names_from = season, values_from = LSW) %>% 
  select(-Spring, -Fall) %>% 
  rename(LSW_Summer = Summer)

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgLSW, by = join_by(site))


#### Neonate body mass ####

# initial year
neoBM <- NDVIOuts %>% select(run.num, site, neo.BM.out.base)

neoBM <- neoBM %>% 
  separate(neo.BM.out.base, namOff, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>%
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, nBM = `2`) 

# neoPlotB <- ggplot(neoBM, aes(x = day, y = nBM, col = as.factor(site), fill = as.factor(site))) + 
#   geom_smooth() +
#   labs(x = "Day of year", y = "Neonate mass [g]", col = "Site", fill = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))

neoBMbins <- neoBM %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanneoBM = mean(nBM)) %>% 
  group_by(season, site) %>% 
  mutate(nBM = mean(nBM)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((nBM - meanneoBM)/meanneoBM) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(neoBMbins, "nBM", seasons)

# plot outputs
nBMH <- plotInitYear(neoBMbins, "Birth mass", -10, 25) + theme(axis.text.x=element_blank())

# PCA outs 
avgnBM <- neoBMbins %>% 
  select(-meanneoBM, -diff) %>% 
  pivot_wider(names_from = season, values_from = nBM) %>% 
  select(-Summer, -Fall) %>% 
  rename(nBM_Spring = Spring)

PCAdat <- PCAdat %>% 
  left_join(., avgnBM, by = join_by(site))


# final year

neoBM <- NDVIOuts %>%  select(run.num, site, neo.BM.out)

neoBM <- neoBM %>% 
  separate(neo.BM.out, namOff, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>%
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, nBM = `2`) 

neoBMbins <- neoBM %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanneoBM = mean(nBM)) %>% 
  group_by(season, site) %>% 
  mutate(nBM = mean(nBM)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((nBM - meanneoBM)/meanneoBM) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(neoBMbins, "nBM", seasons)

# plot outputs
nBMF <- plotFinalYear(neoBMbins, -10, 25) + theme(axis.text.x=element_blank())

# PCA outs 
avgnBM <- neoBMbins %>% 
  select(-meanneoBM, -diff) %>% 
  pivot_wider(names_from = season, values_from = nBM) %>% 
  select(-Summer, -Fall) %>% 
  rename(nBM_Spring = Spring)

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgnBM, by = join_by(site))


#### Offspring body mass at weaning ####

# initial year
weanBM <- NDVIOuts %>%  select(run.num, site, wean.BM.out.base)

weanBM <- weanBM %>% 
  separate(wean.BM.out.base, namOff, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, wBM = `2`) 

# weanPlotB <- ggplot(weanBM, aes(x = day, y = wBM, col = as.factor(site), fill = as.factor(site))) + 
#   geom_smooth() +
#   labs(x = "Day of year", y = "Weaning mass [g]", col = "Site", fill = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))

weanBMbins <- weanBM %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanweanBM = mean(wBM)) %>% 
  group_by(season, site) %>% 
  mutate(wBM = mean(wBM)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((wBM - meanweanBM)/meanweanBM) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(weanBMbins, "wBM", seasons)

# plot outputs
wBMH <- plotInitYear(weanBMbins, "Weaning mass", -10, 20) +
  theme(legend.position = 'bottom')

# PCA outs 
avgwBM <- weanBMbins %>% 
  select(-meanweanBM, -diff) %>% 
  pivot_wider(names_from = season, values_from = wBM) %>% 
  select(-Summer, -Fall) %>% 
  rename(wBM_Spring = Spring)

PCAdat <- PCAdat %>% 
  left_join(., avgwBM, by = join_by(site))


# final year 

weanBM <- NDVIOuts %>%  select(run.num, site, wean.BM.out)

weanBM <- weanBM %>% 
  separate(wean.BM.out, namOff, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, wBM = `2`) 

# weanPlotF <- ggplot(weanBM, aes(x = day, y = wBM, col = as.factor(site), fill = as.factor(site))) + 
#   geom_smooth() +
#   labs(x = "Day of year", y = "Weaning mass [g]", col = "Site", fill = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))

weanBMbins <- weanBM %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanweanBM = mean(wBM)) %>% 
  group_by(season, site) %>% 
  mutate(wBM = mean(wBM)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((wBM - meanweanBM)/meanweanBM) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(weanBMbins, "wBM", seasons)

# plot outputs
wBMF <- plotFinalYear(weanBMbins, -10, 20)

avgwBM <- weanBMbins %>% 
  select(-meanweanBM, -diff) %>% 
  pivot_wider(names_from = season, values_from = wBM) %>% 
  select(-Summer, -Fall) %>% 
  rename(wBM_Spring = Spring)

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgwBM, by = join_by(site))


#### Field metabolic rates ####

# initial year
FMR <- NDVIOuts %>%  select(run.num, site, FMR.out.base)

FMR <- FMR %>% 
  separate(FMR.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, FMRs = `2`) 

FMRbins <- FMR %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanFMRs = mean(FMRs)) %>% 
  group_by(season, site) %>% 
  mutate(FMRs = mean(FMRs)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((FMRs - meanFMRs)/meanFMRs) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(FMRbins, "FMRs", seasons)
 
# plot outputs
FMRH <- plotInitYear(FMRbins, "Field\nmetabolic rate", -5, 12) + theme(axis.text.x=element_blank())

# PCA outs 
avgFMR <- FMRbins %>% 
  select(-meanFMRs, -diff) %>% 
  pivot_wider(names_from = season, values_from = FMRs) %>% 
  select(-Summer, -Fall) %>% 
  rename(FMR_Spring = Spring)

PCAdat <- PCAdat %>% 
  left_join(., avgFMR, by = join_by(site))

# final year 

FMR <- NDVIOuts %>%  select(run.num, site, FMR.out)

FMR <- FMR %>% 
  separate(FMR.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, FMRs = `2`) 

FMRbins <- FMR %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanFMRs = mean(FMRs)) %>% 
  group_by(season, site) %>% 
  mutate(FMRs = mean(FMRs)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((FMRs - meanFMRs)/meanFMRs) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(FMRbins, "FMRs", seasons)

# plot outputs
FMRF <- plotFinalYear(FMRbins, -5, 12) + theme(axis.text.x=element_blank())

# PCA outs 
avgFMR <- FMRbins %>% 
  select(-meanFMRs, -diff) %>% 
  pivot_wider(names_from = season, values_from = FMRs) %>% 
  select(-Summer, -Fall) %>% 
  rename(FMR_Spring = Spring)

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgFMR, by = join_by(site))


#### Locomotive costs ####

# initial year
Loco <- NDVIOuts %>%  select(run.num, site, Loco.out.base)

Loco <- Loco %>% 
  separate(Loco.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, Locos = `2`) 

Locobins <- Loco %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanLocos = mean(Locos)) %>% 
  group_by(season, site) %>% 
  mutate(Locos = mean(Locos)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((Locos - meanLocos)/meanLocos) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(Locobins, "Locos", seasons)

# plot outputs
LocoH <- plotInitYear(Locobins, "Locomotion\ncosts", -30, 35) + theme(axis.text.x=element_blank())

# PCA outs 
# avgLoco <- Locobins %>% 
#   select(-meanLocos, -diff) %>% 
#   pivot_wider(names_from = season, values_from = Locos) %>% 
#   select(-Spring, -Fall) %>% 
#   rename(Loco_Summer = Summer)
# 
# PCAdat <- PCAdat %>% 
#   left_join(., avgLoco, by = join_by(site))

# final year 

Loco <- NDVIOuts %>%  select(run.num, site, Loco.out)

Loco <- Loco %>% 
  separate(Loco.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, Locos = `2`) 

Locobins <- Loco %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanLocos = mean(Locos)) %>% 
  group_by(season, site) %>% 
  mutate(Locos = mean(Locos)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((Locos - meanLocos)/meanLocos) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(Locobins, "Locos", seasons)

# plot outputs
LocoF <- plotFinalYear(Locobins, -30, 35) + theme(axis.text.x=element_blank())

#### Reproductive costs ####

# initial year
Repro <- NDVIOuts %>%  select(run.num, site, repro.out.base)

Repro <- Repro %>% 
  separate(repro.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, Repros = `2`) 

Reprobins <- Repro %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanRepros = mean(Repros)) %>% 
  group_by(season, site) %>% 
  mutate(Repros = mean(Repros)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((Repros - meanRepros)/meanRepros) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(Reprobins, "Repros", seasons)

# plot outputs
ReproH <- plotInitYear(Reprobins, "Reproductive\ncosts", -30, 35) + theme(axis.text.x=element_blank())

# PCA outs 
# avgRepro <- Reprobins %>% 
#   select(-meanRepros, -diff) %>% 
#   pivot_wider(names_from = season, values_from = Repros) %>% 
#   select(-Spring, -Fall) %>% 
#   rename(Repro_Summer = Summer)
# 
# PCAdat <- PCAdat %>% 
#   left_join(., avgRepro, by = join_by(site))

# final year 

Repro <- NDVIOuts %>%  select(run.num, site, repro.out)

Repro <- Repro %>% 
  separate(repro.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, Repros = `2`) 

Reprobins <- Repro %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanRepros = mean(Repros)) %>% 
  group_by(season, site) %>% 
  mutate(Repros = mean(Repros)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((Repros - meanRepros)/meanRepros) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(Reprobins, "Repros", seasons)

# plot outputs
ReproF <- plotFinalYear(Reprobins, -30, 35) + theme(axis.text.x=element_blank())

#### Growth costs #### 

# initial year
Growth <- NDVIOuts %>%  select(run.num, site, growth.out.base)

Growth <- Growth %>% 
  separate(growth.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, Growths = `2`) 

Growthbins <- Growth %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanGrowths = mean(Growths)) %>% 
  group_by(season, site) %>% 
  mutate(Growths = mean(Growths)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((Growths - meanGrowths)/meanGrowths) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(Growthbins, "Growths", seasons)

# plot outputs
GrowthH <- plotInitYear(Growthbins, "Growth\ncosts", -10, 20) + theme(axis.text.x=element_blank())

# PCA outs 
# avgGrowth <- Growthbins %>% 
#   select(-meanGrowths, -diff) %>% 
#   pivot_wider(names_from = season, values_from = Growths) %>% 
#   select(-Summer, -Fall) %>% 
#   rename(Growth_Spring = Spring)
# 
# PCAdat <- PCAdat %>% 
#   left_join(., avgGrowth, by = join_by(site))

# final year 

Growth <- NDVIOuts %>%  select(run.num, site, growth.out)

Growth <- Growth %>% 
  separate(growth.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, Growths = `2`) 

Growthbins <- Growth %>% 
  mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
  group_by(season) %>% 
  mutate(meanGrowths = mean(Growths)) %>% 
  group_by(season, site) %>% 
  mutate(Growths = mean(Growths)) %>%
  select(-run.num, -num, -day) %>% 
  distinct() %>% 
  mutate(diff = ((Growths - meanGrowths)/meanGrowths) * 100) %>%
  ungroup() %>% 
  mutate(
    season = fct_relevel(
      season,
      c("Spring", "Summer", "Fall")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunct(Growthbins, "Growths", seasons)

# plot outputs
GrowthF <- plotFinalYear(Growthbins, -10, 20) + theme(axis.text.x=element_blank())

#### Lifetime reproductive success ####

# initial year 
LRS <- NDVIOuts %>% select(run.num, site, LRS.out.base)

LRS <- LRS %>% 
  separate(LRS.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, lrs = `2`) 


# other dist stats 
library(moments)

LRSmets <- LRS %>%  
  filter(lrs != 0) %>% 
  group_by(site) %>% 
  summarise(Skew = skewness(lrs), 
            Kurtosis = kurtosis(lrs), 
            Average = mean(lrs)) %>% 
  mutate(meanSkew = mean(Skew),
         meanKurt = mean(Kurtosis),
         meanLRS = mean(Average)) %>% 
  mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
         Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
         Average = ((Average - meanLRS)/meanLRS) * 100) %>%
  select(-meanSkew, -meanKurt, -meanLRS) %>% 
  pivot_longer(-site, names_to = "metric") %>% 
  mutate(
    metric = fct_relevel(
      metric,
      c("Average", "Skew", "Kurtosis")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(LRSmets, "value", metrics)

# plot outputs
LRSH <- plotMetsInitYear(LRSmets, "Lifetime\nreproductive\nsuccess", -40, 35) +
  labs(title = "Year 2023") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())

# PCA outs 
avgLRSH <- LRS %>%  
  filter(lrs != 0) %>% 
  group_by(site) %>% 
  summarise(LRS = mean(lrs))

PCAdat <- PCAdat %>% 
  left_join(., avgLRSH, by = join_by(site))

# for comb plot 
LRSHouts <- LRSmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(LRS = value)

# final year 

LRS <- NDVIOuts %>%  select(run.num, site, LRS.out)

LRS <- LRS %>% 
  separate(LRS.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, lrs = `2`) 

LRSmets <- LRS %>%  
  filter(lrs != 0) %>% 
  group_by(site) %>% 
  summarise(Skew = skewness(lrs), 
            Kurtosis = kurtosis(lrs), 
            Average = mean(lrs)) %>% 
  mutate(meanSkew = mean(Skew),
         meanKurt = mean(Kurtosis),
         meanLRS = mean(Average)) %>% 
  mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
         Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
         Average = ((Average - meanLRS)/meanLRS) * 100) %>%
  select(-meanSkew, -meanKurt, -meanLRS) %>% 
  pivot_longer(-site, names_to = "metric") %>% 
  mutate(
    metric = fct_relevel(
      metric,
      c("Average", "Skew", "Kurtosis")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(LRSmets, "value", metrics)

# plot outputs
LRSF <- plotMetsFinalYear(LRSmets, -40, 35) +
  labs(title = "Year 2099") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_blank())

# PCA outs 
avgLRSF <- LRS %>%  
  filter(lrs != 0) %>% 
  group_by(site) %>% 
  summarise(LRS = mean(lrs))

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgLRSF, by = join_by(site))


# for comb plot 
LRSFouts <- LRSmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(LRS = value)

#### Age at death ####

# initial year 
ageDeath <- NDVIOuts %>%  select(run.num, site, age.at.death.out.base)

ageDeath <- ageDeath %>% 
  separate(age.at.death.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, age = `2`) 

ADmets <- ageDeath %>%  
  group_by(site) %>% 
  summarise(Skew = skewness(age), 
            Kurtosis = kurtosis(age), 
            Average = mean(age)) %>% 
  mutate(meanSkew = mean(Skew),
         meanKurt = mean(Kurtosis),
         meanAD = mean(Average)) %>% 
  mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
         Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
         Average = ((Average - meanAD)/meanAD) * 100) %>%
  select(-meanSkew, -meanKurt, -meanAD) %>% 
  pivot_longer(-site, names_to = "metric") %>% 
  mutate(
    metric = fct_relevel(
      metric,
      c("Average", "Skew", "Kurtosis")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(ADmets, "value", metrics)

# plot outputs
ADH <- plotMetsInitYear(ADmets, "Longevity", -40, 75) 

# PCA outs 
avgADH <- ageDeath %>%  
  group_by(site) %>% 
  summarise(AgeDeath = mean(age))

PCAdat <- PCAdat %>% 
  left_join(., avgADH, by = join_by(site))


# for comb plot 
ADHouts <- ADmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(AgeDeath = value)


# final year 
ageDeath <- NDVIOuts %>%  select(run.num, site, age.at.death.out)

ageDeath <- ageDeath %>% 
  separate(age.at.death.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, age = `2`) 

ADmets <- ageDeath %>%  
  group_by(site) %>% 
  summarise(Skew = skewness(age), 
            Kurtosis = kurtosis(age), 
            Average = mean(age)) %>% 
  mutate(meanSkew = mean(Skew),
         meanKurt = mean(Kurtosis),
         meanAD = mean(Average)) %>% 
  mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
         Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
         Average = ((Average - meanAD)/meanAD) * 100) %>%
  select(-meanSkew, -meanKurt, -meanAD) %>% 
  pivot_longer(-site, names_to = "metric") %>% 
  mutate(
    metric = fct_relevel(
      metric,
      c("Average", "Skew", "Kurtosis")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(ADmets, "value", metrics)

# plot outputs
ADF <- plotMetsFinalYear(ADmets, -40, 75) 

# PCA outs 
avgADF <- ageDeath %>%  
  group_by(site) %>% 
  summarise(AgeDeath = mean(age))

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgADF, by = join_by(site))

# for comb plot 
ADFouts <- ADmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(AgeDeath = value)


#### Age at first birth ####

# initial year 
age1stBirth <- NDVIOuts %>%  select(run.num, site, age.1st.birth.out.base)

age1stBirth <- age1stBirth %>% 
  separate(age.1st.birth.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, age = `2`) %>% 
  filter(age != 0)

ABmets <- age1stBirth %>%  
  group_by(site) %>% 
  summarise(Skew = skewness(age), 
            Kurtosis = kurtosis(age), 
            Average = mean(age)) %>% 
  mutate(meanSkew = mean(Skew),
         meanKurt = mean(Kurtosis),
         meanAB = mean(Average)) %>% 
  mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
         Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
         Average = ((Average - meanAB)/meanAB) * 100) %>%
  select(-meanSkew, -meanKurt, -meanAB) %>% 
  pivot_longer(-site, names_to = "metric") %>% 
  mutate(
    metric = fct_relevel(
      metric,
      c("Average", "Skew", "Kurtosis")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(ABmets, "value", metrics)

# plot outputs
ABH <- plotMetsInitYear(ABmets, "Age at\nfirst birth", -65, 130)  + theme(axis.text.x=element_blank())

# PCA outs 
avgABH <- age1stBirth %>%  
  group_by(site) %>% 
  summarise(Age1stBirth = mean(age))

PCAdat <- PCAdat %>% 
  left_join(., avgABH, by = join_by(site))


# for comb plot 
ABHouts <- ABmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(Age1stBirth = value)


# final year 
age1stBirth <- NDVIOuts %>%  select(run.num, site, age.1st.birth.out)

age1stBirth <- age1stBirth %>% 
  separate(age.1st.birth.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, age = `2`) %>% 
  filter(age != 0)

ABmets <- age1stBirth %>%  
  group_by(site) %>% 
  summarise(Skew = skewness(age), 
            Kurtosis = kurtosis(age), 
            Average = mean(age)) %>% 
  mutate(meanSkew = mean(Skew),
         meanKurt = mean(Kurtosis),
         meanAB = mean(Average)) %>% 
  mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
         Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
         Average = ((Average - meanAB)/meanAB) * 100) %>%
  select(-meanSkew, -meanKurt, -meanAB) %>% 
  pivot_longer(-site, names_to = "metric") %>% 
  mutate(
    metric = fct_relevel(
      metric,
      c("Average", "Skew", "Kurtosis")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(ABmets, "value", metrics)

# plot outputs
ABF <- plotMetsFinalYear(ABmets, -65, 130) + theme(axis.text.x=element_blank())

# PCA outs 
avgABF <- age1stBirth %>%  
  group_by(site) %>% 
  summarise(Age1stBirth = mean(age))

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgABF, by = join_by(site))

# for comb plot 
ABFouts <- ABmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(Age1stBirth = value)


#### Litters per year ####

# initial year 
LPY <- NDVIOuts %>%  select(run.num, site, LPY.out.base)

LPY <- LPY %>% 
  separate(LPY.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, nLitters = `2`) %>% 
  filter(nLitters != 0)

LPYmets <- LPY %>%  
  group_by(site) %>% 
  summarise(Skew = skewness(nLitters), 
            Kurtosis = kurtosis(nLitters), 
            Average = mean(nLitters)) %>% 
  mutate(meanSkew = mean(Skew),
         meanKurt = mean(Kurtosis),
         meanLPY = mean(Average)) %>% 
  mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
         Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
         Average = ((Average - meanLPY)/meanLPY) * 100) %>%
  select(-meanSkew, -meanKurt, -meanLPY) %>% 
  pivot_longer(-site, names_to = "metric") %>% 
  mutate(
    metric = fct_relevel(
      metric,
      c("Average", "Skew", "Kurtosis")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(LPYmets, "value", metrics)

# plot outputs
LPYH <- plotMetsInitYear(LPYmets, "Litters\nper year", -35, 35) + 
  theme(axis.text.x=element_blank()) 

# PCA outs 
avgLPYH <- LPY %>%  
  group_by(site) %>% 
  summarise(LPY = mean(nLitters))

PCAdat <- PCAdat %>% 
  left_join(., avgLPYH, by = join_by(site))

# for comb plot 
LPYHouts <- LPYmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(LPY = value)


# final year 
LPY <- NDVIOuts %>%  select(run.num, site, LPY.out)

LPY <- LPY %>% 
  separate(LPY.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, nLitters = `2`) %>% 
  filter(nLitters != 0)

LPYmets <- LPY %>%  
  group_by(site) %>% 
  summarise(Skew = skewness(nLitters), 
            Kurtosis = kurtosis(nLitters), 
            Average = mean(nLitters)) %>% 
  mutate(meanSkew = mean(Skew),
         meanKurt = mean(Kurtosis),
         meanLPY = mean(Average)) %>% 
  mutate(Skew = ((Skew - meanSkew)/meanSkew) * 100,
         Kurtosis = ((Kurtosis - meanKurt)/meanKurt) * 100,
         Average = ((Average - meanLPY)/meanLPY) * 100) %>%
  select(-meanSkew, -meanKurt, -meanLPY) %>% 
  pivot_longer(-site, names_to = "metric") %>% 
  mutate(
    metric = fct_relevel(
      metric,
      c("Average", "Skew", "Kurtosis")
    )
  )

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(LPYmets, "value", metrics)

# plot outputs
LPYF <- plotMetsFinalYear(LPYmets, -35, 35) + 
  theme(axis.text.x=element_blank())

# PCA outs 
avgLPYF <- LPY %>%  
  group_by(site) %>% 
  summarise(LPY = mean(nLitters))

PCAdatProj <- PCAdatProj %>% 
  left_join(., avgLPYF, by = join_by(site))

# for comb plot 
LPYFouts <- LPYmets %>% 
  filter(metric == "Average") %>% 
  select(-metric) %>% 
  rename(LPY = value)


#### Population abundance ####

#abun <- NDVIOuts %>%  select(run.num, site, experiment, model, abun.out)
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
#  group_by(run.num, site, experiment, model) %>% 
  mutate(days = day + ((year - 1) * 365)) %>% 
  select(-run.num) %>% 
  pivot_longer(cols = c(adults, offspring), names_to = "state", values_to = "N") 
  
abunProjPlot <- ggplot(subset(abun, experiment == 2), aes(x = days, y = N, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  facet_grid(cols = vars(state))  +
  labs(x = "Day of year", y = "Abundance [N]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))

abunProjPlot <- abun %>% 
  filter(experiment == 2) %>% 
 # filter(state == "adults") %>% 
  group_by(run.num, site, year, days) %>% 
  summarise(tot = sum(N)) %>% 
  ggplot(aes(x = year, y = tot / 10, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Year", y = "Density [N ha-1]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  scale_x_continuous(labels = c(2020,2040,2060,2080,2100)) +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "top")


#ggsave("Figures/ScenOutPlots/NDVI/08_AbunCC.png", height = 4, width = 10)

### Year 1 w/i year dynamics 

abunYr1 <- abun %>%
  filter(year == 23)

yr1Plot <- abun %>% 
  filter(year == 23) %>% 
#  filter(state == "adults") %>% 
  group_by(run.num, site, year, day) %>% 
  summarise(tot = sum(N)) %>% 
  ggplot(aes(x = day, y = tot / 10, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth(method = "loess") +
#  coord_cartesian(ylim=c(0,35)) + 
  labs(x = "Day of year", y = "Density [N ha-1]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none")

abun %>%
  filter(year == 30) %>%
  group_by(run.num, site, year, day) %>%
  summarise(tot = sum(N)) %>%
  ggplot(aes(x = day, y = tot / 10, col = as.factor(site), fill = as.factor(site))) +
  geom_smooth(method = "loess") +
  labs(x = "Day of year", y = "Density [N ha-1]", col = "Site", fill = "Site") +
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none")

### Year 100 w/i year dynamics 

yr100Plot <- abun %>% 
 # filter(year == 99, experiment == 2) %>% 
#  filter(year == 39, experiment == 1) %>% 
  filter(state == "adults") %>% 
  group_by(run.num, site, year, day) %>% 
  summarise(tot = sum(N)) %>% 
  ggplot(aes(x = day, y = tot / 10, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth(method = "loess") +
#  coord_cartesian(ylim=c(0,35)) + 
  labs(x = "Day of year", y = "Density [N ha-1]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank())


### VI throughout year 
# for insets
NDVIinsets <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/NDVI_hist_proj_245.csv")

NDVIinsets <- NDVIinsets %>% 
  mutate(
    site = fct_relevel(
      site,
      c("Pallasjarvi", "Stroemsund", "Konnovesi", "Evenstad", "Havelaue", "Bielowieza", "Frýdek-Místek", "Le Quartier", "Asturias", "Calabria")
    )
  )

yr1Inset <- ggplot(subset(NDVIinsets, year == 2022), aes(x = yday(floor_date), y = NDVI, col = site)) + 
  geom_smooth(size = 1.1, se = FALSE) +
  labs(x = "Day of year", y = "NDVI", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_y_continuous(breaks = c(0,0.5,1), limits = c(0,1)) +
  scale_x_continuous(breaks = c(150,200,250)) +
  theme_classic() +  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        plot.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

yr99Inset <- ggplot(subset(NDVIinsets, year == 2099), aes(x = yday(floor_date), y = NDVI, col = site)) + 
  #yr99Inset <- ggplot(subset(NDVIinsets, year == 2039), aes(x = yday(floor_date), y = NDVI, col = site)) + 
  geom_smooth(size = 1.1, se = FALSE) +
  labs(x = "Day of year", y = "NDVI", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_y_continuous(breaks = c(0,0.5,1), limits = c(0,1)) +
  scale_x_continuous(breaks = c(150,200,250)) +
  theme_classic() +  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        plot.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())


#### Life history plots ####

LH_H <- avgLRSH %>% 
  left_join(avgADH, by = "site") %>% 
  left_join(avgABH, by = "site") %>% 
  left_join(avgLPYH, by = "site") 

LH_H <- LH_H %>%  
  pivot_longer(cols = 2:5, names_to = "metric", values_to = "value")

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(LH_H, "value", outputs)

LH_H <- LRSHouts %>% 
  left_join(ADHouts, by = "site") %>% 
  left_join(ABHouts, by = "site") %>% 
  left_join(LPYHouts, by = "site") 

LH_H <- LH_H %>%  
  pivot_longer(cols = 2:5, names_to = "metric", values_to = "value")


# plot outputs
max <- 35
min <- -25

LHHplot <- ggplot(LH_H, aes(x = metric, y = value)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(aes(fill = site), size = 2, position = position_dodge2(width = 0.8, reverse = TRUE), shape = 21, col = "grey30", stroke = 1.1) +
  stat_smooth(method = "lm", se = FALSE) + 
  labs(x = NULL, y = "Difference from\naverage [%]", col = "Site") + 
  scale_fill_manual(values = pal(10)) +
  scale_x_discrete(labels = c("Age at\nfirst birth", "Longevity", "Litters\nper year", "Lifetime\nreproductive\nsuccess")) +
  ylim(min, max) +
  geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.05), label = paste0("rho=", cors)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") + 
  geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.2), label = paste0("p=", p)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.3), 
        legend.position = "none")

# projection
LH_F <- avgLRSF %>% 
  left_join(avgADF, by = "site") %>% 
  left_join(avgABF, by = "site") %>% 
  left_join(avgLPYF, by = "site") 

LH_F <- LH_F %>%  
  pivot_longer(cols = 2:5, names_to = "metric", values_to = "value")

# calculating spearman rank coefficient correlations 
cors <- corFunctMets(LH_F, "value", outputs)

LH_F <- LRSFouts %>% 
  left_join(ADFouts, by = "site") %>% 
  left_join(ABFouts, by = "site") %>% 
  left_join(LPYFouts, by = "site") 

LH_F <- LH_F %>%  
  pivot_longer(cols = 2:5, names_to = "metric", values_to = "value")

# plot outputs
LHFplot <- ggplot(LH_F, aes(x = metric, y = value)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(aes(fill = site), size = 2, position = position_dodge2(width = 0.8, reverse = TRUE), shape = 21, col = "grey30", stroke = 1.1) +
  labs(x = NULL, y = NULL, col = "Site") + 
  scale_fill_manual(values = pal(10)) +
  scale_x_discrete(labels = c("Age at\nfirst birth", "Longevity", "Litters\nper year", "Lifetime\nreproductive\nsuccess")) +
  ylim(min, max) +
  geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.05), label = paste0("rho=", cors)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") + 
  geom_text(data = cors, aes(y = max - ((max + abs(min)) * 0.2), label = paste0("p=", p)), size = textSizeAxisText / 2.75, color = "grey30", family = "Montserrat") +
  theme_classic()+  
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.3), 
        legend.position = "none",
        axis.text.y = element_blank())

#### PCA test #### 
colnames(PCAdat) <-c("site", "BM", "LSW", "nBM", "wBM", "FMR", "LRS", "AD", "A1B", "LPY")

pca_result <- prcomp(PCAdat[,-1], scale = TRUE)  

# Step 4: Visualization
# Biplot (sites and traits)
# biplot(pca_result)

# Scree Plot (variance explained by each principal component)
# plot(pca_result)


PCAplot <- autoplot(pca_result, data = PCAdat, colour = "grey30", fill="site", shape = 21, size = 3, stroke = 1.1,
         loadings = TRUE, loadings.colour = "grey30", loadings.label.repel = T, 
         loadings.label = TRUE, loadings.label.size = 10,  loadings.label.colour = "grey30")+
  theme_classic() + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey30", size = 0.5)+
  geom_vline(xintercept=0, linetype="dashed", colour= "grey30", size = 0.5)+
  scale_fill_manual(values = pal(10)) +
  ylim(-0.8, 0.7) +
  xlim(-0.7, 0.7) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        plot.background = element_blank(),
        legend.position = "none",
        panel.border = element_rect(colour = "grey30", fill=NA, linewidth=1.5))
PCAplot$layers[[1]]$aes_params$stroke <- 1.1
PCAplot

#ggsave("Figures/App 1/PCA1.png", PCAplot, height = 5, width = 7)

# for projections
colnames(PCAdatProj) <-c("site", "BM", "LSW", "nBM", "wBM", "FMR", "LRS", "AD", "A1B", "LPY")

pca_result_P <- prcomp(PCAdatProj[,-1], scale = TRUE)  

# Step 4: Visualization
# Biplot (sites and traits)
biplot(pca_result_P)

# Scree Plot (variance explained by each principal component)
plot(pca_result_P)


PCAplotP <- autoplot(pca_result_P, data = PCAdatProj, colour = "grey30", fill="site", shape = 21, size = 3,
                    loadings = TRUE, loadings.colour = "grey30", loadings.label.repel = T, 
                    loadings.label = TRUE, loadings.label.size = 10,  loadings.label.colour = "grey30")+
  theme_classic() + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey30", size = 0.5)+
  geom_vline(xintercept=0, linetype="dashed", colour= "grey30", size = 0.5)+
  scale_fill_manual(values = pal(10)) +
  ylim(-0.8, 0.7) +
  xlim(-0.7, 0.7) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        plot.background = element_blank(),
        legend.position = "none",
        panel.border = element_rect(colour = "grey30", fill=NA, linewidth=1.5))
PCAplotP$layers[[1]]$aes_params$stroke <- 1.1
PCAplotP

#ggsave("Figures/App 1/PCA2.png", PCAplotP, height = 5, width = 7)


#### Combined plot with PCA ####

fullPlot <- abunProjPlot / 
  plot_spacer() /
  ((yr1Plot + inset_element(yr1Inset, 0.01, 0.6, 0.4, 1.1)) + 
     plot_spacer() + 
     (yr100Plot + inset_element(yr99Inset, 0.01, 0.6, 0.4, 1.1))  + 
     plot_layout(widths = c(1,0.05,1))) /
  (LHHplot + plot_spacer() + LHFplot  + 
     plot_layout(widths = c(1,0.05,1))) /
  plot_spacer() /
  (PCAplot + plot_spacer() + PCAplotP + 
     plot_layout(widths = c(1,0.05,1))) + 
  plot_layout(axis_titles = "collect",
              heights = c(2.5,0.1,2,1.5,0.1,3))

#ggsave("Figures/App 1/Figure5IA.png", fullPlot, height = 11, width = 9)

#### Appendix plots ####

app1Plot <- 
  ((BMH + BMF) /
  (BCH + BCF) /
  (FMRH + FMRF) /
  (LocoH + LocoF) /
  (ReproH + ReproF) /
  (GrowthH + GrowthF) /
  (LSWH + LSWF) /
  (nBMH + nBMF) /
  (wBMH + wBMF) /
  guide_area()) + 
     plot_layout(guides = 'collect')

#ggsave("Figures/App 1/ScenAppen1IA.png", app1Plot, height = 14, width = 12)

app2Plot <- 
  (LRSH + LRSF) /
  (LPYH + LPYF) /
  (ABH + ABF) /
  (ADH + ADF + 
     plot_layout(guides = 'collect') & 
     theme(legend.position = 'bottom'))


#ggsave("Figures/App 1/ScenAppen2IA.png", app2Plot, height = 6.5, width = 12)

#### State-specific field metabolic rates ####
# 
# FMRbyS <- NDVIOuts %>%  select(run.num, site, FMR.byS.out)
# 
# FMRbyS <- FMRbyS %>% 
#   separate(FMR.byS.out, nam, sep = " ") %>% 
#   gather(key = "num", value = "value", -run.num, -site) %>% 
#   mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = as.numeric(value),
#          num = as.numeric(num)) %>% 
#   arrange(run.num, num) %>% 
#   drop_na() %>% 
#   group_by(run.num) %>% 
#   mutate(order = rep(1:3, times = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), each = 3)) %>% 
#   pivot_wider(names_from = order, values_from = value) %>% 
#   rename(day = `1`, state = `2`, FMR = `3`) 
# 
# # ggplot(FMRbyS, aes(x = as.factor(day), y = FMR, col = as.factor(site))) + 
# #   geom_boxplot() + 
# #   facet_grid(rows = vars(state), scales ="free")
# 
# FMRbySSum <- FMRbyS %>% 
#   group_by(day, site, state) %>% 
#   summarise(medFMRbyS = median(FMR), FMRbySHi = quantile(FMR, probs = 0.75), FMRbySLo = quantile(FMR, probs = 0.25)) %>% 
#   mutate(state = case_when(
#     state == 1 ~ "Juvenile", 
#     state == 2 ~ "Nonreproducing", 
#     state == 3 ~ "Pregnant", 
#     state == 4 ~ "Lactating", 
#     state == 5 ~ "Lactating and pregnant"
#   )) 
# 
# FMRbySSum$state <- factor(FMRbySSum$state, levels = c("Juvenile", "Nonreproducing", "Pregnant", "Lactating", "Lactating and pregnant"))
#   
# FMRplot <- ggplot(FMRbySSum, aes(x = as.factor(day), col = as.factor(site))) + 
#   geom_pointrange(aes(y = medFMRbyS / 1000, ymin = FMRbySLo / 1000, ymax = FMRbySHi / 1000), position = position_dodge2(width = 0.8, reverse = TRUE)) +
#   facet_grid(rows = vars(state))  +
#   labs(x = "Day of year", y = "Field metabolic rate [KJ day-1]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_bw()+  
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
# 


#### State-specific intake rates ####

# IRbyS <- NDVIOuts %>%  select(run.num, site, intake.byS.out)
# 
# IRbyS <- IRbyS %>% 
#   separate(intake.byS.out, nam, sep = " ") %>% 
#   gather(key = "num", value = "value", -run.num, -site) %>% 
#   mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = as.numeric(value),
#          num = as.numeric(num)) %>% 
#   arrange(run.num, num) %>% 
#   drop_na() %>% 
#   group_by(run.num) %>% 
#   mutate(order = rep(1:3, times = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), each = 3)) %>% 
#   pivot_wider(names_from = order, values_from = value) %>% 
#   rename(day = `1`, state = `2`, IR = `3`) 
# 
# # ggplot(IRbyS, aes(x = as.factor(day), y = IR, col = as.factor(site))) + 
# #   geom_boxplot() + 
# #   facet_grid(rows = vars(state), scales ="free")
# 
# IRbySSum <- IRbyS %>% 
#   group_by(day, site, state) %>% 
#   summarise(medIRbyS = median(IR), IRbySHi = quantile(IR, probs = 0.75), IRbySLo = quantile(IR, probs = 0.25))
# 
# # ggplot(IRbySSum, aes(x = as.factor(day), y = IR, col = as.factor(site))) + 
# #   geom_pointrange(aes(y = medIRbyS, ymin = IRbySLo, ymax = IRbySHi), position = position_dodge2(width = 0.8, reverse = TRUE)) +
# #   facet_grid(rows = vars(state), scales ="free")
# 
# IRbySSum <- IRbyS %>% 
#   group_by(day, site, state) %>% 
#   summarise(medIRbyS = median(IR), IRbySHi = quantile(IR, probs = 0.75), IRbySLo = quantile(IR, probs = 0.25)) %>% 
#   mutate(state = case_when(
#     state == 1 ~ "Juvenile", 
#     state == 2 ~ "Nonreproducing", 
#     state == 3 ~ "Pregnant", 
#     state == 4 ~ "Lactating", 
#     state == 5 ~ "Lactating and pregnant"
#   )) 
# 
# IRbySSum$state <- factor(IRbySSum$state, levels = c("Juvenile", "Nonreproducing", "Pregnant", "Lactating", "Lactating and pregnant"))
# 
# IRplot <- ggplot(IRbySSum, aes(x = as.factor(day), col = as.factor(site))) + 
#   geom_pointrange(aes(y = medIRbyS * 12.2811, ymin = IRbySLo * 12.2811, ymax = IRbySHi * 12.2811), position = position_dodge2(width = 0.8, reverse = TRUE)) +
#   facet_grid(rows = vars(state))  +
#   labs(x = "Day of year", y = "Daily food consumption [KJ day-1]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_bw()+  
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
# 
# 
# FMRplot + IRplot + plot_layout(guides = "collect")

#ggsave("Figures/ScenOutPlots/NDVI/03_FMR_IR.png", height = 7, width = 15)

# #### Activity ####
# 
# Act <- NDVIOuts %>%  select(run.num, site, activity.out.base)
# 
# Act <- Act %>% 
#   separate(activity.out.base, nam, sep = " ") %>% 
#   gather(key = "num", value = "value", -run.num, -site) %>% 
#   mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = as.numeric(value),
#          num = as.numeric(num)) %>% 
#   arrange(run.num, num) %>% 
#   drop_na() %>% 
#   group_by(run.num) %>% 
#   mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
#   pivot_wider(names_from = order, values_from = value) %>% 
#   rename(day = `1`, ActProp = `2`) 
# 
# ActSum <- Act %>% 
#   group_by(day, site) %>% 
#   summarise(medAct = median(ActProp), ActHi = quantile(ActProp, probs = 0.75), ActLo = quantile(ActProp, probs = 0.25))
#  
# ggplot(ActSum, aes(x = as.factor(day), col = as.factor(site))) + 
#   geom_pointrange(aes(y = medAct, ymin = ActLo, ymax = ActHi), position = position_dodge2(width = 0.8, reverse = TRUE)) +
#   labs(x = "Day of year", y = "Activity time [Prop]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"), 
#         legend.position = "bottom")
# 
# #ggsave("Figures/ScenOutPlots/NDVI/02_Activity.png", height = 4, width = 9)
# 
# # testing grouping by season and taking difference from average 
# # Actbins <- Act %>% 
# #   mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>% 
# #   group_by(season) %>% 
# #   mutate(meanActProp = median(ActProp)) %>% 
# #   group_by(season, site) %>% 
# #   mutate(ActProp = median(ActProp)) %>%
# #   select(-run.num, -num, -day) %>% 
# #   distinct() %>% 
# #   mutate(diff = ((ActProp - meanActProp)/meanActProp) * 100) %>%
# #   ungroup() %>% 
# #   mutate(
# #     site = fct_relevel(
# #       site,
# #       c("Pallasjarvi", "Stroemsund", "Konnovesi", "Evenstad", "Havelaue", "Bielowieza", "Frýdek-Místek", "Le Quartier", "Asturias", "Calabria")
# #     ),
# #     season = fct_relevel(
# #       season,
# #       c("Spring", "Summer", "Fall")
# #     )
# #   )
# # 
# # 
# # ggplot(Actbins, aes(x = season, y = diff, col = site)) + 
# #   geom_hline(yintercept = 0, col = "grey30") +
# #   geom_point(size = 2, position = position_dodge(width = 0.1)) +
# #   labs(x = "Season", y = "Difference in median body mass [%]", col = "Site") + 
# #   scale_color_manual(values = pal(10)) +
# #   theme_classic()+  
# #   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"), 
# #         legend.position = "none")
# 
# # final year 
# 
# Act <- NDVIOuts %>%  select(run.num, site, activity.out)
# 
# Act <- Act %>% 
#   separate(activity.out, nam, sep = " ") %>% 
#   gather(key = "num", value = "value", -run.num, -site) %>% 
#   mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = as.numeric(value),
#          num = as.numeric(num)) %>% 
#   arrange(run.num, num) %>% 
#   drop_na() %>% 
#   group_by(run.num) %>% 
#   mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
#   pivot_wider(names_from = order, values_from = value) %>% 
#   rename(day = `1`, ActProp = `2`) 
# 
# ActSum <- Act %>% 
#   group_by(day, site) %>% 
#   summarise(medAct = median(ActProp), ActHi = quantile(ActProp, probs = 0.75), ActLo = quantile(ActProp, probs = 0.25))
# 
# ggplot(ActSum, aes(x = as.factor(day), col = as.factor(site))) + 
#   geom_pointrange(aes(y = medAct, ymin = ActLo, ymax = ActHi), position = position_dodge2(width = 0.8, reverse = TRUE)) +
#   labs(x = "Day of year", y = "Activity time [Prop]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"), 
#         legend.position = "bottom")


# #### Interbirth interval ####
# 
# # initial year
# IBI <- NDVIOuts %>%  select(run.num, site, IBI.out.base)
# 
# IBI <- IBI %>%
#   separate(IBI.out.base, nam, sep = " ") %>%
#   gather(key = "num", value = "value", -run.num, -site) %>%
#   mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = as.numeric(value),
#          num = as.numeric(num)) %>%
#   arrange(run.num, num) %>%
#   drop_na() %>%
#   group_by(run.num) %>%
#   mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>%
#   pivot_wider(names_from = order, values_from = value) %>%
#   rename(day = `1`, IBI = `2`)
# 
# ggplot(IBI, aes(x = day, y = IBI, col = as.factor(site), fill = as.factor(site))) +
#   geom_smooth() +
#   labs(x = "Day of year", y = "Interbirth interval [days]", col = "Site", fill = "Site") +
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
# 
# #ggsave("Figures/ScenOutPlots/NDVI/06_IBI.png", height = 4, width = 6)
# 
# IBIbins <- IBI %>%
#   mutate(season = ifelse(day < 181, "Spring", ifelse(day < 242 & day >= 181, "Summer", "Fall"))) %>%
#   group_by(season) %>%
#   mutate(meanIBI = mean(IBI)) %>%
#   group_by(season, site) %>%
#   mutate(IBI = mean(IBI)) %>%
#   select(-run.num, -num, -day) %>%
#   distinct() %>%
#   mutate(diff = ((IBI - meanIBI)/meanIBI) * 100) %>%
#   ungroup() %>%
#   mutate(
#     season = fct_relevel(
#       season,
#       c("Spring", "Summer", "Fall")
#     )
#   )
# 
# ggplot(IBIbins, aes(x = season, y = diff, col = site)) +
#   geom_hline(yintercept = 0, col = "grey30") +
#   geom_point(size = 2, position = position_dodge2(width = 0.8, reverse = TRUE)) +
#   labs(x = "Season", y = "Difference in mean litter size at weaning [%]", col = "Site") +
#   scale_color_manual(values = pal(10)) +
#   ylim(-10, 10) +
#   theme_classic()+
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
#         legend.position = "none")
# 
# 
# # final year
# IBI <- NDVIOuts %>%  select(run.num, site, IBI.out)
# 
# IBI <- IBI %>%
#   separate(IBI.out, nam, sep = " ") %>%
#   gather(key = "num", value = "value", -run.num, -site) %>%
#   mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = as.numeric(value),
#          num = as.numeric(num)) %>%
#   arrange(run.num, num) %>%
#   drop_na() %>%
#   group_by(run.num) %>%
#   mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>%
#   #  mutate(order = rep(1:2, times = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), each = 2)) %>%
#   pivot_wider(names_from = order, values_from = value) %>%
#   rename(day = `1`, IBI = `2`)
# 
# ggplot(IBI, aes(x = day, y = IBI, col = as.factor(site), fill = as.factor(site))) +
#   geom_smooth() +
#   labs(x = "Day of year", y = "Interbirth interval [days]", col = "Site", fill = "Site") +
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
# 
# #ggsave("Figures/ScenOutPlots/NDVI/06_IBI.png", height = 4, width = 6)



