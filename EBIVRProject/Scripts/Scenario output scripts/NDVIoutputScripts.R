# Cara Gallagher
# March 5th, 2024
# Energy Budget with Individual Variation project
# Analyzing output files for NDVI scenarios 

##################################################

library(tidyverse)
library(showtext)
library(beepr)
library(patchwork)

# load typeface and color palette
font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()
pal <- colorRampPalette(c("#3F3F7B","#278192","#00B089","#AABC4A", "#E5DC54","#E4AA4E","#E4714E","#BC4A53"))


##################################################

# read in outputs
# NDVIOuts1 <- read.csv("Data/Scenarios/VI GCM scenarios/Outputs/NDVI-Out-1-9520.csv", skip = 6)
# NDVIOuts1 <- NDVIOuts1 %>%  select(c(1,17:39)) 
# 
# NDVIOuts2 <- read.csv("Data/Scenarios/VI GCM scenarios/Outputs/NDVI-Out-2-9520.csv", skip = 6)
# NDVIOuts2 <- NDVIOuts2 %>%  select(c(1,17:39)) 
# 
# NDVIOuts <- bind_rows(NDVIOuts1, NDVIOuts2)
# NDVIOuts <- NDVIOuts %>%  drop_na() 

NDVIOuts <- read.csv("../../MetabolicVariationLargeOuts/NDVI-9520.csv", skip = 6)
NDVIOuts <- NDVIOuts %>%  select(c(1,17:39)) 

colnames(NDVIOuts) <- c("run.num", "site", "experiment", "model", "final.TS", "BM.out.base", "activity.out.base", "BC.out.base", "LSW.out.base", "neo.BM.out.base", "wean.BM.out.base", "IBI.out.base", "LRS.out.base", "age.at.death.out.base", "BM.out", "activity.out", "BC.out", "LSW.out", "neo.BM.out", "wean.BM.out", "IBI.out", "LRS.out", "age.at.death.out", "abun.out")
NDVIOuts <- NDVIOuts %>%  filter(final.TS >= 883825) 

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


nam <- c(as.character(1:10000))

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
#  mutate(order = rep(1:2, times = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), each = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, BM = `2`) 


# ggplot(BM, aes(x = as.factor(day), y = BM, col = as.factor(site))) + 
#   geom_boxplot() 

# BMSum <- BM %>% 
#   group_by(day, site) %>% 
#   summarise(BMmed = median(BM), BMHi = quantile(BM, probs = 0.75), BMLo = quantile(BM, probs = 0.25))
# 
# ggplot(BMSum, aes(x = as.factor(day), col = as.factor(site))) + 
#   geom_pointrange(aes(y = BMmed, ymin = BMLo, ymax = BMHi), position = position_dodge(width = 0.5)) +
#   labs(x = "Day of year", y = "Body mass [g]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
#         legend.position = "bottom")

#ggsave("Figures/ScenOutPlots/NDVI/01_BodyMass.png", height = 4, width = 9)

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


BMH <- ggplot(BMbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Body mass", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-10,10) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text.x=element_blank())

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

BMSum <- BM %>% 
  group_by(day, site) %>% 
  summarise(BMmed = median(BM), BMHi = quantile(BM, probs = 0.75), BMLo = quantile(BM, probs = 0.25))

# ggplot(BMSum, aes(x = as.factor(day), col = as.factor(site))) + 
#   geom_pointrange(aes(y = BMmed, ymin = BMLo, ymax = BMHi), position = position_dodge(width = 0.5)) +
#   labs(x = "Day of year", y = "Body mass [g]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
#         legend.position = "bottom")

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


BMF <- ggplot(BMbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = NULL, col = "Site") + 
  scale_color_manual(values = pal(10)) +
  theme_classic()+  
  ylim(-10,10) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text=element_blank())


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


# BCSum <- BC %>% 
#   group_by(day, site) %>% 
#   summarise(BCmed = median(BC), BCHi = quantile(BC, probs = 0.75), BCLo = quantile(BC, probs = 0.25))

# ggplot(BCSum, aes(x = as.factor(day), col = as.factor(site))) + 
#   geom_pointrange(aes(y = BCmed, ymin = BCLo, ymax = BCHi), position = position_dodge(width = 0.5)) +
#   labs(x = "Day of year", y = "Body condition [Prop]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
#         legend.position = "bottom")

#ggsave("Figures/ScenOutPlots/NDVI/01_BodyMass.png", height = 4, width = 9)

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

BCH <- ggplot(BCbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Body fat", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  theme_classic()+  
  ylim(-10, 25) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text.x=element_blank())

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

# BCSum <- BC %>% 
#   group_by(day, site) %>% 
#   summarise(BCmed = median(BC), BCHi = quantile(BC, probs = 0.75), BCLo = quantile(BC, probs = 0.25))
# 
# ggplot(BCSum, aes(x = as.factor(day), col = as.factor(site))) + 
#   geom_pointrange(aes(y = BCmed, ymin = BCLo, ymax = BCHi), position = position_dodge(width = 0.5)) +
#   labs(x = "Day of year", y = "Body condition [Prop]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
#         legend.position = "bottom")

#ggsave("Figures/ScenOutPlots/NDVI/01_BodyMass.png", height = 4, width = 9)

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

BCF <- ggplot(BCbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = NULL, col = "Site") + 
  scale_color_manual(values = pal(10)) +
  theme_classic()+  
  ylim(-10, 25) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text=element_blank())


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
  # mutate(order = rep(1:2, times = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), each = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, LSW = `2`) 

# ggplot(LSW, aes(x = day, y = LSW, col = as.factor(site), fill = as.factor(site))) + 
#   geom_smooth() +
#   labs(x = "Day of year", y = "Litter size at weaning [N]", col = "Site", fill = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

#ggsave("Figures/ScenOutPlots/NDVI/04_LitterSizeWeaning.png", height = 4, width = 6)

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

LSWH <- ggplot(LSWbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Litter size\nat weaning", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-10, 10) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.title.y = element_text(lineheight = 0.3))

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
  # mutate(order = rep(1:2, times = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), each = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, LSW = `2`) 

# ggplot(LSW, aes(x = day, y = LSW, col = as.factor(site), fill = as.factor(site))) + 
#   geom_smooth() +
#   labs(x = "Day of year", y = "Litter size at weaning [N]", col = "Site", fill = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

#ggsave("Figures/ScenOutPlots/NDVI/04_LitterSizeWeaning.png", height = 4, width = 6)


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

LSWF <- ggplot(LSWbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = NULL, col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-10, 10) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

#### Neonate body mass ####

# initial year
neoBM <- NDVIOuts %>% select(run.num, site, neo.BM.out.base)

neoBM <- neoBM %>% 
  separate(neo.BM.out.base, nam, sep = " ") %>% 
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

neoPlotB <- ggplot(neoBM, aes(x = day, y = nBM, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Day of year", y = "Neonate mass [g]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

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

nBMH <- ggplot(neoBMbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Birth mass", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-10, 20) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text.x=element_blank())

# final year

neoBM <- NDVIOuts %>%  select(run.num, site, neo.BM.out)

neoBM <- neoBM %>% 
  separate(neo.BM.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>%
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  #  mutate(order = rep(1:2, times = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), each = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, nBM = `2`) 

neoPlotF <- ggplot(neoBM, aes(x = day, y = nBM, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Day of year", y = "Neonate mass [g]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


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

nBMF <- ggplot(neoBMbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = NULL, col = "Site") + 
  scale_color_manual(values = pal(10)) +
   ylim(-10, 20) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text=element_blank())

#### Offspring body mass at weaning ####

# initial year
weanBM <- NDVIOuts %>%  select(run.num, site, wean.BM.out.base)

weanBM <- weanBM %>% 
  separate(wean.BM.out.base, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
#  mutate(order = rep(1:2, times = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), each = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(day = `1`, wBM = `2`) 

weanPlotB <- ggplot(weanBM, aes(x = day, y = wBM, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Day of year", y = "Weaning mass [g]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


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

wBMH <- ggplot(weanBMbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Weaning mass", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-10, 20) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none")

# final year 

weanBM <- NDVIOuts %>%  select(run.num, site, wean.BM.out)

weanBM <- weanBM %>% 
  separate(wean.BM.out, nam, sep = " ") %>% 
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

weanPlotF <- ggplot(weanBM, aes(x = day, y = wBM, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Day of year", y = "Weaning mass [g]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


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

wBMF <- ggplot(weanBMbins, aes(x = season, y = diff, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = NULL, col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-10, 20) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text.y=element_blank())


#ggsave("Figures/ScenOutPlots/NDVI/05_Neonate_Weaning_BM.png", height = 4, width = 10)


#### Lifetime reproductive success ####

# initial year 
LRS <- NDVIOuts %>%  select(run.num, site, LRS.out.base)

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

# ggplot(subset(LRS, lrs != 0), aes(x = lrs, col = as.factor(site), fill = as.factor(site))) + 
#   geom_density(alpha = 0.5) +
#   labs(x = "Lifetime reproductive success [N]", y = "Density", col = "Site", fill = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

#ggsave("Figures/ScenOutPlots/NDVI/06_IBI.png", height = 4, width = 6)

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

LRSH <- ggplot(LRSmets, aes(x = metric, y = value, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Lifetime\nreproductive success", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-40, 20) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.title.y = element_text(lineheight = 0.3))


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

# ggplot(subset(LRS, lrs != 0), aes(x = lrs, col = as.factor(site), fill = as.factor(site))) + 
#   geom_density(alpha = 0.5) +
#   labs(x = "Lifetime reproductive success [N]", y = "Density", col = "Site", fill = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

#ggsave("Figures/ScenOutPlots/NDVI/06_IBI.png", height = 4, width = 6)

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

LRSF <- ggplot(LRSmets, aes(x = metric, y = value, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Lifetime reproductive success", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-40, 20) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.title.y=element_blank(),
        axis.text=element_blank())


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

ggplot(ageDeath, aes(x = age, col = as.factor(site), fill = as.factor(site))) + 
  geom_density(alpha = 0.5) +
  labs(x = "Age at death [days]", y = "Density", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

#ggsave("Figures/ScenOutPlots/NDVI/06_IBI.png", height = 4, width = 6)


ADmets <- ageDeath %>%  
  group_by(site) %>% 
  summarise(Skew = skewness(age), 
            Kurtosis = kurtosis(age), 
            Average = mean(age)) %>% 
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

ADH <- ggplot(ADmets, aes(x = metric, y = value, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Age at death", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-40, 65) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none")


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

# ggplot(ageDeath, aes(x = age, col = as.factor(site), fill = as.factor(site))) + 
#   geom_density(alpha = 0.5) +
#   labs(x = "Age at death [days]", y = "Density", col = "Site", fill = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   scale_fill_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

#ggsave("Figures/ScenOutPlots/NDVI/06_IBI.png", height = 4, width = 6)


ADmets <- ageDeath %>%  
  group_by(site) %>% 
  summarise(Skew = skewness(age), 
            Kurtosis = kurtosis(age), 
            Average = mean(age)) %>% 
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

ADF <- ggplot(ADmets, aes(x = metric, y = value, col = site)) + 
  geom_hline(yintercept = 0, col = "grey30") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Age at death", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  ylim(-40, 65) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank())


#### Population abundance ####

abun <- NDVIOuts %>%  select(run.num, site, experiment, model, abun.out)

abun <- abun %>% 
  separate(abun.out, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -site, -experiment, -model) %>% 
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
  group_by(run.num, site, experiment, model) %>% 
  mutate(days = day + ((year - 1) * 365)) %>% 
  select(-run.num) %>% 
  pivot_longer(cols = c(adults, offspring), names_to = "state", values_to = "N") 
  
ggplot(abun, aes(x = days, y = N, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  facet_grid(rows = vars(experiment), cols = vars(state))  +
  labs(x = "Day of year", y = "Abundance [N]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

abunProjPlot <- ggplot(subset(abun, experiment == 2), aes(x = days, y = N, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  facet_grid(cols = vars(state))  +
  labs(x = "Day of year", y = "Abundance [N]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

abunProjPlot <- abun %>% 
  filter(experiment == 2) %>% 
  group_by(run.num, site, year, days) %>% 
 # mutate(days = days - min(abun$days)) %>% 
  summarise(tot = sum(N)) %>% 
  ggplot(aes(x = year, y = tot / 10, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Simulation year", y = "Density [N ha-1]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "top")



#ggsave("Figures/ScenOutPlots/NDVI/08_AbunCC.png", height = 4, width = 10)

### Year 1 w/i year dynamics 

abunYr1 <- abun %>% 
  filter(year == 23) 
  

yr1Plot <- ggplot(abunYr1, aes(x = day, y = N, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  facet_grid(cols = vars(state))  +
  labs(x = "Day of year", y = "Abundance [N]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

#ggsave("Figures/ScenOutPlots/NDVI/07_AbunYr1.png", height = 4, width = 10)

yr1Plot <- abun %>% 
  filter(year == 23) %>% 
#  filter(year == 23, state == "adults") %>% 
  group_by(run.num, site, year, day) %>% 
  summarise(tot = sum(N)) %>% 
  ggplot(aes(x = day, y = tot / 10, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Day of year", y = "Density [N ha-1]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "none")

# abunYr1Sum <- abunYr1 %>% 
#   group_by(days, site, state) %>% 
#   summarise(Nmed = median(N), NHi = quantile(N, probs = 0.75), NLo = quantile(N, probs = 0.25))
# 
# ggplot(abunYr1Sum, aes(x = as.factor(days), col = as.factor(site))) + 
#   geom_pointrange(aes(y = Nmed, ymin = NLo, ymax = NHi), position = position_dodge(width = 0.5)) +
#   facet_grid(cols = vars(state))  +
#   labs(x = "Day of year", y = "Abundance [N]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
#         legend.position = "bottom")

### Year 100 w/i year dynamics 

abunYr100 <- abun %>% 
  filter(year == 99, experiment == 2)

yr100Plot <- ggplot(abunYr100, aes(x = days, y = N, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  facet_grid(cols = vars(state))  +
  labs(x = "Day of year", y = "Abundance [N]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


#ggsave("Figures/ScenOutPlots/NDVI/07_AbunYr100.png", height = 4, width = 10)

yr100Plot <- abun %>% 
  filter(year == 99, experiment == 2) %>% 
  group_by(run.num, site, year, day) %>% 
  summarise(tot = sum(N)) %>% 
  ggplot(aes(x = day, y = tot / 10, col = as.factor(site), fill = as.factor(site))) + 
  geom_smooth() +
  labs(x = "Day of year", y = "Density [N ha-1]", col = "Site", fill = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  theme_classic()+  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank())


### VI throughout year 
# for insets
NDVIinsets <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/NDVI_hist_proj.csv")

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
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        plot.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

yr99Inset <- ggplot(subset(NDVIinsets, year == 2099), aes(x = yday(floor_date), y = NDVI, col = site)) + 
  geom_smooth(size = 1.1, se = FALSE) +
  labs(x = "Day of year", y = "NDVI", col = "Site") + 
  scale_color_manual(values = pal(10)) +
  scale_y_continuous(breaks = c(0,0.5,1), limits = c(0,1)) +
  scale_x_continuous(breaks = c(150,200,250)) +
  theme_classic() +  
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        plot.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

#### Combined plot #### 

abunProjPlot / 
  plot_spacer() /
  ((yr1Plot + inset_element(yr1Inset, 0.01, 0.6, 0.4, 1.1)) + 
     yr100Plot + inset_element(yr99Inset, 0.01, 0.6, 0.4, 1.1)) /
  (BMH + BMF) /
  (BCH + BCF) /
  (LSWH + LSWF) /
  (nBMH + nBMF) /
  (wBMH + wBMF) /
  plot_spacer() /
  (LRSH + LRSF) /
  (ADH + ADF) + plot_layout(axis_titles = "collect",
                            heights = c(2,0.1,2,1,1,1,1,1,0.1,1,1))

ggsave("Figures/ScenOutPlots/NDVI/CombPlotTest.png", height = 14, width = 9)


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
#   geom_pointrange(aes(y = medFMRbyS / 1000, ymin = FMRbySLo / 1000, ymax = FMRbySHi / 1000), position = position_dodge(width = 0.5)) +
#   facet_grid(rows = vars(state))  +
#   labs(x = "Day of year", y = "Field metabolic rate [KJ day-1]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_bw()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
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
# #   geom_pointrange(aes(y = medIRbyS, ymin = IRbySLo, ymax = IRbySHi), position = position_dodge(width = 0.5)) +
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
#   geom_pointrange(aes(y = medIRbyS * 12.2811, ymin = IRbySLo * 12.2811, ymax = IRbySHi * 12.2811), position = position_dodge(width = 0.5)) +
#   facet_grid(rows = vars(state))  +
#   labs(x = "Day of year", y = "Daily food consumption [KJ day-1]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_bw()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
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
#   geom_pointrange(aes(y = medAct, ymin = ActLo, ymax = ActHi), position = position_dodge(width = 0.5)) +
#   labs(x = "Day of year", y = "Activity time [Prop]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
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
# #   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
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
#   geom_pointrange(aes(y = medAct, ymin = ActLo, ymax = ActHi), position = position_dodge(width = 0.5)) +
#   labs(x = "Day of year", y = "Activity time [Prop]", col = "Site") + 
#   scale_color_manual(values = pal(10)) +
#   theme_classic()+  
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
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
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
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
#   geom_point(size = 2, position = position_dodge(width = 0.5)) +
#   labs(x = "Season", y = "Difference in mean litter size at weaning [%]", col = "Site") +
#   scale_color_manual(values = pal(10)) +
#   ylim(-10, 10) +
#   theme_classic()+
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
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
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
# 
# #ggsave("Figures/ScenOutPlots/NDVI/06_IBI.png", height = 4, width = 6)



