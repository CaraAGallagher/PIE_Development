# Cara Gallagher
# February 26th, 2022
# Energy Budget with Individual Variation project
# Storage level output verification

##################################################
# Packages:
library(tidyverse)
library(readxl)
library(patchwork)
library(ggridges)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

pal <- colorRampPalette(c("#BC4A53","#3F3F7B","#278192","#00B089"))

##################################################

#### Storage #### 

# from Fedyk 1974
FedSL <- tibble(
  bodyMass = c(1.810,4.510,4.766,12.668,15.033,13.781,13.868,17.234,13.505,17.593), 
  fatMass = c(0.053,0.394,0.264,1.203,3.170,3.259,2.143,3.234,2.113,3.484)
  )

FedSL <- FedSL %>% 
  mutate(
    adiMass = fatMass / 0.881,
    fatPerc = adiMass / bodyMass
  )

FedSL <- FedSL %>% 
  select(fatPerc) %>% 
  mutate(
         ref = "g_Small litters", 
         source = "Fedyk 1974"
         )

FedLL <- tibble(
  bodyMass = c(1.967,3.875,5.172,11.545,12.382,14.154,12.930,14.768,13.777,12.671), 
  fatMass = c(0.070,0.253,0.177,1.375,1.966,3.592,1.126,2.461,2.080,1.581)
)

FedLL <- FedLL %>% 
  mutate(
    adiMass = fatMass / 0.881,
    fatPerc = adiMass / bodyMass
  )

FedLL <- FedLL %>% 
  select(fatPerc) %>% 
  mutate(
         ref = "f_Large litters", 
         source = "Fedyk 1974"
         )


# from Sawicka-Kapusta 1974
SKLab <- tibble(
  bodyMass = c(2.13,2.54,4.2,5.80,8.58,15.61,17.43),
  waterCon = c(0.845,0.816,0.782,0.746,0.730,0.661,0.609),
  fatConDryMass = c(0.248,0.299,0.315,0.349,0.316,0.445,0.489)
)

SKLab <- SKLab %>% 
  mutate(
    bodyMassDry = bodyMass * (1 - waterCon),
    fatMass = bodyMassDry * fatConDryMass,
    adiMass = fatMass / 0.881,
    fatPerc = adiMass / bodyMass
    )

SKLab <- SKLab %>% 
  select(fatPerc) %>% 
  mutate(
         ref = "c_Laboratory", 
         source = "Sawicka-Kapusta 1974"
         )

SKPen <- tibble(
  bodyMass = c(2.2,2.3,4.93,5.33,7.86,18.40),
  waterCon = c(0.832,0.825,0.752,0.725,0.709,0.637),
  fatConDryMass = c(0.285,0.276,0.411,0.423,0.397,0.435)
)

SKPen <- SKPen %>% 
  mutate(
    bodyMassDry = bodyMass * (1 - waterCon),
    fatMass = bodyMassDry * fatConDryMass,
    adiMass = fatMass / 0.881,
    fatPerc = adiMass / bodyMass
  )

SKPen <- SKPen %>% 
  select(fatPerc) %>% 
  mutate(
         ref = "d_Outdoor pen", 
         source = "Sawicka-Kapusta 1974"
         )


SKTerr <- tibble(
  bodyMass = c(14.0,17.9,19.1,19.3,15.8,15.5,17.3,20.8,21.3),
  waterCon = c(0.751,0.742,0.739,0.728,0.705,0.709,0.706,0.713,0.712),
  fatConDryMass = c(0.127,0.111,0.125,0.140,0.121,0.104,0.120,0.133,0.140)
)

SKTerr <- SKTerr %>% 
  mutate(
    bodyMassDry = bodyMass * (1 - waterCon),
    fatMass = bodyMassDry * fatConDryMass,
    adiMass = fatMass / 0.881,
    fatPerc = adiMass / bodyMass
    )

SKTerr <- SKTerr %>% 
  select(fatPerc) %>% 
  mutate(
    ref = "e_Terrain", 
    source = "Sawicka-Kapusta 1974"
    )

empDatFedSK <- bind_rows(
  FedSL,
  FedLL,
  SKLab,
  SKPen,
  SKTerr
)

#pal <- lacroix_palette("PassionFruit", type = "discrete")

# From Tidhar and Speakman 2007 

empDatTS <- tibble(
  mean = c(0.0971,0.114) / 0.881,  #  0.881 to account for water in adipose
  se = c(0.0129,0.0130) / 0.881,
  min = c(0.034,0.041) / 0.881, 
  max = c(0.257,0.224) / 0.881,
  source = "Tidhar and Speakman 2007",
  sex = c("b_Male","a_Female")
)

empDatTS <- empDatTS %>% 
  mutate(sd = se * sqrt(19))

empPlot <- 
  ggplot()+
  geom_linerange(data = empDatTS, aes(x = as.factor(sex), ymin = min, ymax = max, col = source), size = 1) + 
  geom_linerange(data = empDatTS, aes(x = as.factor(sex), ymin = mean - se, ymax = mean + se, col = source), size = 2) + 
  geom_point(data = empDatTS, aes(x = as.factor(sex), y = mean, col = source), size = 3) + 
  geom_jitter(data = empDatFedSK, aes(x = ref, y = fatPerc, col = source), width = 0.2, size = 2.5) + 
  scale_color_manual(values = pal(3)) +
  coord_flip() +
  ylim(-0.01,0.4) +
  labs(x = NULL, y = NULL, col = NULL) + 
  theme_classic() +
  theme(legend.position = c(0.85, 0.9))
empPlot

# model outputs

## Storage levels daily

modOut <- read_csv("Data/Verification/Storage/EBIVPrototype_StorageLevels.csv", skip = 17)

modOut <- modOut %>% 
  select(x...1, y...2) %>% 
  rename(storageLevel = x...1, count = y...2) 

tot <- sum(modOut$count)

modOut <- modOut %>% 
  mutate(perc = (count / tot)*100)

SLPlot1 <- ggplot(modOut, aes (x = storageLevel, y = perc)) + 
  geom_bar(stat = "identity", fill = "grey50") +
  labs(x = NULL, y = "Daily recordings [%]") +
  xlim(-0.01,0.4) +
  theme_classic() 
SLPlot1

# State dependent storage levels
modOut <- read_delim("Data/Verification/Storage/StorageLevelsStateRepoSeason.txt", delim = ", ", col_names = FALSE)

# fix bracketing issues
modOut$X1 <- str_replace(modOut$X1,"\\[","")
modOut$X3 <- str_replace(modOut$X3,"\\]","")

modOut <- modOut %>% 
  mutate(storageLevel = as.numeric(X1), age = as.numeric(X2), state = as.numeric(X3)) %>% 
  select(-X1, -X2, -X3) %>% 
  mutate(state = ifelse(state == 1, "Pup", ifelse(state == 2, "Juvenile", ifelse(state == 3, "Adult", ifelse(state == 4, "Pregnant", ifelse(state == 5, "Lactating", "Pregnant and Lactating")))))) %>% 
  mutate(state = factor(state, levels = c("Pup", "Juvenile", "Adult", "Pregnant","Lactating", "Pregnant and Lactating")))

ggplot(modOut, aes (x = storageLevel, fill = state)) + 
  geom_histogram(bins = 41, col = "white") +
  labs(x = "Body fat [%/100]", y = "Daily recordings [N]", fill = NULL) +
  xlim(-0.01,0.4) +
  theme_classic() +
  theme(legend.position = c(0.15, 0.8)) 


ggplot(modOut, aes (x = storageLevel, fill = state)) + 
  geom_histogram(bins = 41, col = "white") +
  labs(x = "Body fat [%/100]", y = "Daily recordings [N]", fill = NULL) +
  xlim(-0.01,0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(as.factor(state)), scales = "free_y")

## Storage levels at death

modOut <- read_csv("Data/Verification/Storage/EBIVPrototype_StorageLevelsAtDeath.csv", skip = 16)

modOut <- modOut %>% 
  select(x, y) %>% 
  rename(storageLevel = x, count = y) 

tot <- sum(modOut$count)

modOut <- modOut %>% 
  mutate(perc = (count / tot)*100)

SLPlot2 <- ggplot(modOut, aes (x = storageLevel, y = perc)) + 
  geom_bar(stat = "identity", fill = "grey50") +
  labs(x = "Storage level [%/100]", y = "Percent at death [%]") +
  xlim(-0.01,0.4) +
  theme_classic() 
SLPlot2


fullSLPlot1 <- SLPlot1 / empPlot / SLPlot2
fullSLPlot1
#ggsave("Figures/Verification/SLPlotOut.svg", fullSLPlot1 , width = 8, height = 8)
#ggsave("Figures/Verification/SLPlotOut.png", fullSLPlot1 , width = 8, height = 8)

### mobilization of fat and lean mass

nam <- c(as.character(1:100000))

fuelsOut <- read_csv("Data/Verification/EBIV_COT_activity_fuels.csv", skip = 6)

fuelsOut <- fuelsOut %>%  
  select('[run number]', 'fuel-use-list') %>% 
  rename(runNum = '[run number]', fuels = 'fuel-use-list')


fuelsOut <- fuelsOut %>% 
  separate(fuels, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -runNum) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(runNum, num) %>% 
  drop_na() %>% 
  group_by(runNum) %>% 
  mutate(order = rep(1:3, times = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), each = 3)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(SL = `1`, mLM = `2`, mAdi = `3`) %>% 
  mutate(SL = SL * 100, mLM = mLM * 1000, mAdi = mAdi * 1000) %>% 
  select(SL, mLM, mAdi) %>% 
  mutate(percLM = (mLM / (mLM + mAdi)) * 100, percAdi = (mAdi / (mLM + mAdi)) * 100) %>% 
  filter(mLM != 0)

pal <- c("#278192","#AABC4A")

fuelsOut1 <- fuelsOut %>% 
  mutate(SL = round(SL)) %>% 
  group_by(SL) %>% 
  summarise(mpLM = mean(percLM), sdpLM = sd(percLM), mpAdi = mean(percAdi), sdpAdi = sd(percAdi)) %>% 
  pivot_longer(cols = c(mpLM, mpAdi, sdpLM, sdpAdi), names_to = "name", values_to = "values")%>%
  mutate(metric = ifelse(name == "mpLM" | name == "mpAdi", "mean", "sd")) %>% 
  mutate(fuel = rep(c("LM", "Adi"), times = (max(row_number()) / 2))) %>% 
  select(-name) %>% 
  pivot_wider(names_from = metric, values_from = values)

FuelsPlot1 <- ggplot(fuelsOut1, aes (x = SL, y = mean)) + 
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = fuel), alpha = 0.25) +
  geom_line(aes(col = fuel), size = 2) +
  scale_color_manual(values = pal, labels = c("Adipose", "Lean mass")) +
  scale_fill_manual(values = pal, labels = c("Adipose", "Lean mass")) +
  labs(x = "Storage level [%]", y = "Percent fuel use [%]", col = NULL) +
  xlim(0,40) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"))
#FuelsPlot1

fuelsOut2 <- fuelsOut %>% 
  mutate(SL = round(SL)) %>% 
  group_by(SL) %>% 
  summarise(mpLM = mean(mLM), sdpLM = sd(mLM), mpAdi = mean(mAdi), sdpAdi = sd(mAdi)) %>% 
  pivot_longer(cols = c(mpLM, mpAdi, sdpLM, sdpAdi), names_to = "name", values_to = "values")%>%
  mutate(metric = ifelse(name == "mpLM" | name == "mpAdi", "mean", "sd")) %>% 
  mutate(fuel = rep(c("LM", "Adi"), times = (max(row_number()) / 2))) %>% 
  select(-name) %>% 
  pivot_wider(names_from = metric, values_from = values)
  

FuelsPlot2 <- ggplot(fuelsOut2, aes (x = SL, y = mean)) + 
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = fuel), alpha = 0.25) +
  geom_line(aes(col = fuel), size = 2) +
  scale_color_manual(values = pal, labels = c("Adipose", "Lean mass")) +
  scale_fill_manual(values = pal, labels = c("Adipose", "Lean mass")) +
  labs(x = "Storage level [%]", y = "Mass mobilized [g 30min-1]", col = NULL) +
  theme_classic() +
  xlim(0,40) +
  theme(legend.position = c(0.2, 0.9),
        text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "grey30"))+ guides(fill = "none")
FuelsPlot2


fullFuelsPlot <- FuelsPlot2 / FuelsPlot1  + 
  plot_layout(heights = c(2, 1)) + 
  plot_annotation(tag_levels = 'A')
fullFuelsPlot

#ggsave("Figures/Verification/FuelsPlotOut.png", fullFuelsPlot , width = 7, height = 7)



