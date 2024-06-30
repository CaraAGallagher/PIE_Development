# Cara Gallagher
# November 12th, 2022
# Energy Budget with Individual Variation project
# ABC calibration processing results from cluster

#------------------------------------------------#
####  Packages: #### 
library(tidyverse)
library(nlrx)
library(readxl)
library(data.table)
library(patchwork)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

#### Load dataset ####

fitsAll <- read.csv("Data/Calibration/ABCOutputsSelectedSets.csv")
runNums <- fitsAll$run.num

#### Loading  runs ####

#### plotting outputs
# Olympic, BryceCanyon, DeathValley !, Denali !,   Saguaro !, Volcanoes !, Yellowstone
#pal <- natparks.pals(name="Volcanoes",n=length(runNums),type="continuous")
#pal <- pal[1:length(runNums)]

pal <- colorRampPalette(c("#E4AA4E","#E4714E","#BC4A53","#3F3F7B"))
pal <- rev(pal(30))
textSizeAxisTitle <- 40
textSizeAxisText <- 30
  
####  Pattern 1. Fetal mass at birth  #### 

p1 <- fitsAll %>%  select(run.num, p1.FMAB)
nam <- c(as.character(1:10000))

p1 <- p1 %>% 
  separate(p1.FMAB, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na()

plot1 <- ggplot(p1) +
  annotate("rect", ymin=0.001 * 1000, ymax=0.0025 * 1000, xmin=-Inf, xmax=Inf, alpha=0.2, fill="black") +
  geom_boxplot(aes(x = 0.3, y = value * 1000),  col = pal[1], linewidth = 0.7,  width = 0.05, fill = pal[1], alpha = 0.25, outlier.shape = NA, notch = TRUE) +
  geom_pointrange(mapping = aes(x= 0, col = as.factor(run.num), y = value * 1000),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median, 
                  linewidth = 0.5, size = 0.3, position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = c(0,0.0005,0.0010,0.0015,0.0020,0.0025,0.0030) * 1000) +
  scale_x_continuous(breaks = c(0,0.3),limits = c(-0.25,0.325), labels = c("Ind. runs", "All")) +
  labs(y = "Neonate mass [g]", x = NULL) +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none",
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))

####  Pattern 3. Total body mass by age  #### 
GrowthDat <- read_excel("Data/Parameterization/Growth/GrowthCurveData.xlsx")
GrowthDat <-  GrowthDat %>%
  mutate(Perc_fat_R = ifelse((is.na(Perc_fat) & Environment == "Lab"), 
                           "E", 
                           "D")) %>% 
  mutate(Perc_fat = ifelse((is.na(Perc_fat) & Environment == "Lab"), 
                           (0.0258*(Mass_kg * 1000)^1.6571)/(Mass_kg * 1000), 
                           Perc_fat)) %>% 
  mutate(Mass_kg_lm = Mass_kg * (1-Perc_fat))

p3 <- fitsAll %>% select(run.num, p34.BMLMxA)

nam <- c(as.character(1:350000))

p3 <- p3 %>%
  separate(p34.BMLMxA, nam, sep = " ") %>%
  gather(key = "num", value = "value",-run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>%
  arrange(run.num, num) %>%
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:3, each = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), times = 3)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(BM = `1`, LM = `2`, age = `3`) %>%
  filter(age >= 21 & age <= 620) %>% 
  mutate(p3.pattern = 0.02483015*(1 - (1 - (0.005569125 / 0.02483015)^(1/3))*exp((-0.03913599*age)/3))^3,
         p4.pattern = 0.01930132*(1 - (1 - (0.004688886 / 0.01930132)^(1/3))*exp((-0.04845531*age)/3))^3)

### fitting growth curves to run outputs
fit.bertalanffy <- function(age, data) {                                 # create von bertalanffy fitting function
  d <- data.frame(age, data) 
  starting.values <- c(                                                  # starting values
    A=A1,                                                                # asymptotic starting mass set to max in dataset
    k=k1,
    Mo = Mo1)                                           
  formula.bertalanffy <- "data~A*(1 - (1 - (Mo / A)^(1/3))* exp((-k*age)/3))^3"               # fit using standard VB curve
  nls(formula.bertalanffy, d, starting.values)                           # use nls to fit curve using data and specified starting values
}


outputsMean <- data.frame(  
  runNum=numeric(),
  A=numeric(),
  Asd=numeric(),
  k=numeric(),
  ksd=numeric(),
  Mo=numeric(),
  Mosd=numeric(),
  AIC=numeric())

pots <- seq(0.000001, 0.020001, 0.0001)

pots <- seq(0.000001, 0.1, 0.0001)

n <- 1

for (h in unique(p3$run.num)) {

  for (i in pots) {
    run <- p3 %>% filter(run.num == h)
    
    tryCatch({
      k1 = i
      A1 =  max(run$BM)
      to1 = max(run$age)
      Mo1 = min(run$BM)
      fitMean <- fit.bertalanffy(run$age, run$BM)  # run formula
      # print(i)
    }, error = function(e) {
      # print("error")
    })
  }
  
  
  outputsMean[n, ] <-
    c(
      runNum = h,
      A = summary(fitMean)$coefficients[1, 1],
      Asd = summary(fitMean)$coefficients[1, 2],
      k = summary(fitMean)$coefficients[2, 1],
      ksd = summary(fitMean)$coefficients[2, 2],
      No = summary(fitMean)$coefficients[3, 1],
      Nosd = summary(fitMean)$coefficients[3, 2],
      AIC = AIC(fitMean)
    )
  n <- n + 1
}


p3alt <- tibble(age = seq(21, 620))
  
n <- 1

for (h in unique(outputsMean$runNum)) {
  
 # h <- unique(outputsMean$runNum)[1]
  
  p3alt <-  p3alt %>%  
    mutate(col = outputsMean[n,2]*(1 - (1 - (outputsMean[n,6] / outputsMean[n,2])^(1/3))*exp((-outputsMean[n,4]*age)/3))^3) %>% 
    rename(!!as.character(h) := col)
  

n <- n + 1
  
}

p3alt <- p3alt %>% 
  pivot_longer(cols = -age) %>% 
  rename(run.num = name, mean = value)

plot3 <- ggplot() +
  geom_point(data = p3, aes(x = age, y = BM * 1000), col = "grey80", size = 0.5) + 
  geom_line(data = p3alt, aes(y = mean * 1000, x = age, col = as.factor(run.num)), size = 0.7) +
  geom_line(data = p3, aes(y = p3.pattern * 1000, x = age), linewidth = 1, col = "grey20") +
  geom_point(data = GrowthDat, aes(x = Age_days, y = Mass_kg * 1000), col = "grey20", fill = "grey40", shape = 25, size = 0.7) + 
  labs(y = "Body mass [g]", x = "Age [days]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))

####  Patterns 5, 6, & 7  #### 


p567.pattern <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig1Dat.xlsx")
p567.pattern <- p567.pattern %>% rename(dsb = ageGest)

p5 <- fitsAll %>% select(run.num, p567.dsbxMMFILM)

nam <- c(as.character(1:100000))

p5 <- p5 %>%
  separate(p567.dsbxMMFILM, nam, sep = " ") %>%
  gather(key = "num", value = "value",-run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>%
  arrange(run.num, num) %>%
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:4, each = (max(row_number()) / 4)), num = rep(1:(max(row_number()) / 4), times = 4)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(dsb = `1`, MM = `2`, FI = `3`, LitM = `4`) %>%
  left_join(p567.pattern[which(p567.pattern$Metric == "Mean"),], by = "dsb") %>% 
  select(-Metric) %>% 
  mutate(bodyMass = bodyMass / 1000, foodCons = foodCons * 17.80, FI = FI * 12.2811, LitterMass = LitterMass / 1000)

####  Pattern 5. Lactating mother mass by pup age  #### 
p5Rib <- p567.pattern %>% 
  select(dsb, Metric, bodyMass) %>% 
  pivot_wider(names_from = Metric, values_from = bodyMass) %>% 
  filter(dsb != 0)


plot5 <- ggplot() +
  geom_point(data = p5, aes(x = dsb, y = MM * 1000), col = "grey80", size = 0.5, position = position_jitter(width = 0.2)) +
  geom_ribbon(data = p5Rib, aes(x = dsb, ymin = Lo, ymax = Hi), alpha = 0.3, fill = "grey20") + 
  geom_boxplot(data = p5, aes(x = dsb, y = MM * 1000, group = dsb), col = pal[1], width = 0.35, fill = pal[1], alpha = 0.25, lwd = 0.6, outlier.shape = NA, notch = TRUE) +
  geom_line(data = p5Rib, aes(x = dsb, y = Mean), size = 1,  col = "grey20") +
  labs(y = "Mother body\nmass [g]", x = "Pup age [days]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))

#plot5

####  Pattern 6. Lactating mother food intake by pup age  #### 
p6Rib <- p567.pattern %>% 
  select(dsb, Metric, foodCons) %>% 
  mutate(foodCons = foodCons * 17.80) %>% 
  pivot_wider(names_from = Metric, values_from = foodCons) %>% 
  filter(dsb != 0)

plot6 <- ggplot() +
  geom_point(data = p5, aes(x = dsb, y = FI), col = "grey80", size = 0.5, position = position_jitter(width = 0.2)) +
  geom_ribbon(data = p6Rib, aes(x = dsb, ymin = Lo, ymax = Hi), alpha = 0.3, fill = "grey20") + 
  geom_boxplot(data = p5, aes(x = dsb, y =FI, group = dsb), col = pal[1], width = 0.35, fill = pal[1], alpha = 0.25, lwd = 0.6, outlier.shape = NA, notch = TRUE) +
  geom_line(data = p6Rib, aes(x = dsb, y = Mean), size = 1, col = "grey20") +
  labs(y = "Mother food\nintake [kJ day-1]", x = "Pup age [days]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))

#plot6

####  Pattern 7. Total litter mass by pup age  #### 
p7Rib <- p567.pattern %>% 
  select(dsb, Metric, LitterMass) %>% 
  pivot_wider(names_from = Metric, values_from = LitterMass) %>% 
  filter(dsb != 0)

plot7 <- ggplot() +
  geom_point(data = p5, aes(x = dsb, y = LitM * 1000), col = "grey80", size = 0.5, position = position_jitter(width = 0.2)) +
  geom_ribbon(data = p7Rib, aes(x = dsb, ymin = Lo, ymax = Hi), alpha = 0.3, fill = "grey20") + 
  geom_boxplot(data = p5, aes(x = dsb, y = LitM * 1000, group = dsb), col = pal[1], width = 0.35, fill = pal[1], alpha = 0.25, lwd = 0.6, outlier.shape = NA, notch = TRUE) +
  geom_line(data = p7Rib, aes(x = dsb, y = Mean), size = 1, col = "grey20") +
  labs(y = "Total litter\nmass [g]", x = "Pup age [days]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))
#plot7


####  Patterns 8, 9, & 10  #### 

p8910.pattern <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig2Dat.xlsx")
p8910.pattern <- p8910.pattern %>% rename(LS = litterSize)

p8 <- fitsAll %>% select(run.num, p8910.LSxpFIEUMEO)

nam <- c(as.character(1:100000))


p8 <- p8 %>%
  separate(p8910.LSxpFIEUMEO, nam, sep = " ") %>%
  gather(key = "num", value = "value",-run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>%
  drop_na()  %>%
  group_by(run.num) %>% 
  mutate(order = rep(1:4, each = (max(row_number()) / 4)), num = rep(1:(max(row_number()) / 4), times = 4)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(LS = `1`, pFI = `2`, pEU = `3`, pMEO = `4`) %>%
  left_join(p8910.pattern, by = "LS") %>% 
  pivot_wider(names_from = Metric, values_from = Value) %>% 
  mutate(FC = FC * 17.80, pFI = pFI * 12.2811)


####  Pattern 8. Mother peak food intake by litter size  #### 

plot8 <- ggplot(p8) +
  geom_point(aes(x = as.factor(LS), y = pFI), col = "grey80", size = 0.5, position = position_jitter(width = 0.2)) +
  geom_boxplot(aes(x = as.factor(LS), y = pFI), col = pal[1], width = 0.35, fill = pal[1], alpha = 0.25, lwd = 0.6, outlier.shape = NA, notch = TRUE) +
  geom_line(aes(x = LS, y = FC), size = 1,  col = "grey20") +
  scale_x_discrete(breaks = seq(1:9)) +
  labs(y = "Mother peak food\nintake [kJ day-1]", x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 500)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))
#plot8

####  Pattern 9. Mother peak energy use by litter size  #### 

plot9 <- ggplot(p8) +
  geom_point(aes(x = as.factor(LS),y = pEU), col = "grey80", size = 0.5, position = position_jitter(width = 0.2)) +
  geom_boxplot(aes(x = as.factor(LS), y = pEU), col = pal[1], width = 0.35, fill = pal[1], alpha = 0.25, lwd = 0.6, outlier.shape = NA, notch = TRUE) +
  geom_line(aes(x = LS, y = ADMR), size = 1, col = "grey20") +
  scale_x_discrete(breaks = seq(1:9)) +
  labs(y = "Mother peak\nenergy use [kJ day-1]", x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  coord_cartesian(ylim=c(40, 150)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))

#plot9

####  Pattern 10. Mother peak milk transfer by litter size  #### 

plot10 <-  ggplot(p8) +  
  geom_point(aes(x = as.factor(LS), y = pMEO), col = "grey80", size = 0.5, position = position_jitter(width = 0.2)) +
  geom_boxplot(aes(x = as.factor(LS), y = pMEO), col = pal[1], width = 0.35, fill = pal[1], alpha = 0.25, lwd = 0.6, outlier.shape = NA, notch = TRUE) +
  geom_line(aes(x = LS, y = MEO), size = 1, col = "grey20") +
  scale_x_discrete(breaks = seq(1:9)) +
  labs(y = "Mother peak milk\ntransfer [kJ day-1]", x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  coord_cartesian(ylim=c(0, 250)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))
#plot10


####  Pattern 11. Pup mass at weaning by litter size  #### 

p11 <- fitsAll %>%  select(run.num, p11.PMxLS)

nam <- c(as.character(1:10000))

p11 <- p11 %>% 
  separate(p11.PMxLS, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(WM = `1`, LS = `2`) 

p11dat <- read_excel("Data/Calibration/Inputs/WeaningMass_LitterSize.xlsx")

plot11 <- ggplot(p11, aes(x = LS, y = WM * 1000)) +
  geom_smooth(aes(col = as.factor(run.num)),
              method = "lm",
              se = FALSE,
              size = 0.7) +
  geom_smooth(method = "lm",
              se = FALSE,
              col = pal[1],
              size = 1.5) +
  annotate("segment", x = p11dat$Mean_LS - p11dat$SE_LS, xend = p11dat$Mean_LS + p11dat$SE_LS, y = p11dat$Mean_WM, yend = p11dat$Mean_WM, colour = "grey20", linewidth = 0.5, size = 0.3) +
  annotate("pointrange", x = p11dat$Mean_LS, y = p11dat$Mean_WM, ymin = p11dat$Mean_WM - p11dat$SE_WM, ymax = p11dat$Mean_WM + p11dat$SE_WM, colour = "grey20", linewidth = 0.5, size = 0.3) +
  labs(y = "Weaning mass [g]", x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = seq(1:9)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))

####  Pattern 13. Litter size at weaning  #### 

p13 <- fitsAll %>%  select(run.num, p13.LSw)

nam <- c(as.character(1:10000))

p13 <- p13 %>% 
  separate(p13.LSw, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 


plot13 <- ggplot(p13) +
  annotate("rect", ymin=1.28, ymax=5.28, xmin=-Inf, xmax=Inf, alpha=0.2, fill="black") +
  geom_boxplot(aes(x = 0.3, y = value),  col = pal[1], linewidth = 0.7,  width = 0.05, fill = pal[1], alpha = 0.25, outlier.shape = NA, notch = TRUE) +
  geom_pointrange(mapping = aes(x= 0, col = as.factor(run.num), y = value),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median, 
                  linewidth = 0.5, size = 0.3, position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = seq(1,12)) +
  scale_x_continuous(breaks = c(0,0.3),limits = c(-0.25,0.325), labels = c("Ind. runs", "All")) +
  labs(y = "Weaning litter\nsize [N pups]", x = NULL) +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none",
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))


####  Pattern 14. Average / range of body fat %  #### 

p14 <- fitsAll %>%  select(run.num, p15.BF)

nam <- c(as.character(1:30000))

p14 <- p14 %>% 
  rename(p14.BF = p15.BF) %>% 
  separate(p14.BF, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 


plot14 <- ggplot(p14) +
  annotate("rect", ymin=3, ymax=29, xmin=-Inf, xmax=Inf, alpha=0.2, fill="black") +
  geom_boxplot(aes(x = 0.3, y = value * 100),  col = pal[1], linewidth = 0.7,  width = 0.05, fill = pal[1], alpha = 0.25, outlier.shape = NA, notch = TRUE) +
  geom_pointrange(mapping = aes(x= 0, col = as.factor(run.num), y = value * 100),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median, 
                  linewidth = 0.5, size = 0.3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = c(0,0.3),limits = c(-0.25,0.325), labels = c("Ind. runs", "All")) +
  labs(y = "Storage level\n[% body fat]", x = NULL) +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none",
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))

####  Pattern 15. Body fat % of living animals  #### 

p15 <- fitsAll %>% select(run.num, p16.BF)

nam <- c(as.character(1:30000))

p15 <- p15 %>% 
  rename(p15.BF = p16.BF) %>% 
  separate(p15.BF, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 

plot15 <- ggplot(p15) +
  annotate("rect", ymin=0, ymax=3, xmin=-Inf, xmax=Inf, alpha=0.2, fill="black") +
  geom_boxplot(aes(x = 0.3, y = value * 100),  col = pal[1], linewidth = 0.7,  width = 0.05, fill = pal[1], alpha = 0.25, outlier.shape = NA, notch = TRUE) +
  geom_pointrange(mapping = aes(x= 0, col = as.factor(run.num), y = value * 100),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median, 
                  linewidth = 0.5, size = 0.3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = c(0,0.3),limits = c(-0.25,0.325), labels = c("Ind. runs", "All")) +
  labs(y = "Storage level at\ndeath [% body fat]", x = NULL) +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8)) +
  theme_classic() +
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none",
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))

####  Pattern 16. Field metabolic rate by body mass #### 

p16 <- fitsAll %>%  select(run.num, p17.FMRxBM)

p16.pattern <- tibble(
  mass = seq(round(min(p3$BM)*1000),round(max(p3$BM)*1000),1),
  #mass = seq(3,48,1),
  Speakman1999 = exp(1.878) * mass^0.66,   
 # King1974 = 753 * (mass / 1000)^0.67,                 
  Nagy1999 = 5.48 * mass^0.71)

p16.pattern <- p16.pattern %>% 
  mutate(avgFMR = (Speakman1999 + Nagy1999) / 2) %>% 
  rename(BM = mass)

nam <- c(as.character(1:30000))

p16 <- p16 %>% 
  rename(p16.FMRxBM = p17.FMRxBM) %>% 
  separate(p16.FMRxBM, nam, sep = " ") %>% 
  gather(key = "num", value = "value",-run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>%
  arrange(run.num, num) %>%
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(BM = `1`, FMR = `2`) %>%
  mutate(BM = round(BM * 1000), FMR = FMR / 1000) %>% 
  left_join(p16.pattern, by = "BM") 

plot16 <-  ggplot(p16, aes(x = BM)) +
  geom_point(aes(y = FMR), size = 0.5, col = "grey80", position = position_jitter(width = 0.2)) +
  geom_ribbon(aes(ymin = Speakman1999, ymax = Nagy1999, group = 1), fill = "grey20", alpha = 0.25) +
  geom_line(aes(y = avgFMR, group = 1), size = 1, col = "grey20") +
  geom_boxplot(aes(y = FMR, group = as.factor(BM)), col = pal[1], width = 0.75, fill = pal[1], alpha = 0.25, lwd = 0.6, outlier.shape = NA) +
  labs(y = "Field metabolic rate [kJ day-1]", x = "Body mass [g]") +
  scale_color_manual(values = pal) +
  coord_cartesian(ylim=c(10, 130)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
#plot16

#### Resulting allocation curves ####


fitsPs <- read.csv("Data/Calibration/ABCParameterTable.csv")
fitsPs <- fitsPs %>% 
  mutate(run.num = row_number()) 


pal <- colorRampPalette(c("#E4AA4E","#E4714E","#BC4A53","#3F3F7B"))
pal <- rev(pal(30))

fitsPs <- fitsPs %>%
  mutate(run.num = row_number())

# Growth 
fitsGrow <- fitsPs %>% 
  select( "run.num","growth.lm.prob.const","growth.lm.prob.mid")


for (i in c(seq(0.0,0.4,0.01))) {
  n <- as.character(i)
  
  fitsGrow <- fitsGrow %>% 
    add_column(n = NA)
  
}

colnames(fitsGrow)[4:ncol(fitsGrow)] <- as.character(seq(0.0,0.4,0.01))

for (i in 1:nrow(fitsGrow)) {
  
  row <- fitsGrow[i,]
  
  for (ii in 1:length(seq(0.0,0.4,0.01))) {
    
    const <- as.numeric(row[2])
    mid <- as.numeric(row[3])
    sl <- as.numeric(colnames(row[ii + 3]))
    
    ind <- 1 / (1 + exp(-1 * const * (sl - mid)))
    
    fitsGrow[i,ii + 3] <- ind
    
  }
  
}

fitsGrow <- fitsGrow %>% 
  pivot_longer(4:ncol(fitsGrow), names_to = "storageLevel", values_to = "value") %>% 
  mutate(storageLevel = as.numeric(storageLevel))

#pal <- natparks.pals(name="Volcanoes",n=nrow(fitsPs),type="continuous")

growPlot <- ggplot(fitsGrow, aes(x = storageLevel, y = value)) +
  geom_smooth(aes(col = as.factor(run.num)), 
              method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              size = 0.7) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 1.5) +
  scale_color_manual(values = pal) +
  labs(x = "Storage level [% body fat]", y = "Growth allocation [%]") +
  theme_classic() +
  scale_x_continuous(labels = function(x) paste0(x*100)) +
  scale_y_continuous(labels = function(x) paste0(x*100)) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none")

# pregnancy 
fitsPreg <- fitsPs %>% 
  select( "run.num","preg.prob.const","preg.prob.mid")


for (i in c(seq(0.0,0.4,0.01))) {
  n <- as.character(i)
  
  fitsPreg <- fitsPreg %>% 
    add_column(n = NA)
  
}

colnames(fitsPreg)[4:ncol(fitsPreg)] <- as.character(seq(0.0,0.4,0.01))

for (i in 1:nrow(fitsPreg)) {
  
  row <- fitsPreg[i,]
  
  for (ii in 1:length(seq(0.0,0.4,0.01))) {
    
    const <- as.numeric(row[2])
    mid <- as.numeric(row[3])
    sl <- as.numeric(colnames(row[ii + 3]))
    
    ind <- 1 / (1 + exp(-1 * const * (sl - mid)))
    
    fitsPreg[i,ii + 3] <- ind
    
  }
  
}

fitsPreg <- fitsPreg %>% 
  pivot_longer(4:ncol(fitsPreg), names_to = "storageLevel", values_to = "value") %>% 
  mutate(storageLevel = as.numeric(storageLevel))


pregPlot <- ggplot(fitsPreg, aes(x = storageLevel, y = value)) +
  geom_smooth(aes(col = as.factor(run.num)), 
              method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              size = 0.7) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 1.5) +
  scale_color_manual(values = pal) +
  labs(x = "Storage level [% body fat]", y = "Pregnancy allocation [%]") +
  theme_classic() +
  scale_x_continuous(labels = function(x) paste0(x*100)) +
  scale_y_continuous(labels = function(x) paste0(x*100)) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none")


# lactation 
fitsLact <- fitsPs %>% 
  select( "run.num","lact.prob.const","lact.prob.mid")


for (i in c(seq(0.0,0.4,0.01))) {
  n <- as.character(i)
  
  fitsLact <- fitsLact %>% 
    add_column(n = NA)
  
}

colnames(fitsLact)[4:ncol(fitsLact)] <- as.character(seq(0.0,0.4,0.01))

for (i in 1:nrow(fitsLact)) {
  
  row <- fitsLact[i,]
  
  for (ii in 1:length(seq(0.0,0.4,0.01))) {
    
    const <- as.numeric(row[2])
    mid <- as.numeric(row[3])
    sl <- as.numeric(colnames(row[ii + 3]))
    
    ind <- 1 / (1 + exp(-1 * const * (sl - mid))) * 2
    
    fitsLact[i,ii + 3] <- ind
    
  }
  
}

fitsLact <- fitsLact %>% 
  pivot_longer(4:ncol(fitsLact), names_to = "storageLevel", values_to = "value") %>% 
  mutate(storageLevel = as.numeric(storageLevel))


lactPlot <- ggplot(fitsLact, aes(x = storageLevel, y = value)) +
  geom_smooth(aes(col = as.factor(run.num)), 
              method = "gam",
              #method.args = list(family = "binomial"),
              se = FALSE,
              size = 0.7) +
  geom_smooth(method = "gam",
              #method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 1.5) +
  scale_color_manual(values = pal) +
  labs(x = "Storage level [% body fat]", y = "Lactation allocation [%]") +
  theme_classic() +
  scale_x_continuous(labels = function(x) paste0(x*100)) +
  scale_y_continuous(labels = function(x) paste0(x*100)) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        legend.position = "none")


#### All plots ####
 
fullPlot <- 
  (plot3 | plot14 | plot15) /
  (plot5 | plot6 | plot7 ) /
  (plot8 | plot9 | plot10 ) /
  (plot13 | plot1 | plot11) /
  (plot16) /
  plot_spacer() /
  (growPlot | pregPlot | lactPlot)  + 
  plot_layout(heights = c(1,1,1,1,2,0.1,1.5)) + 
  plot_annotation(tag_levels = 'A', tag_suffix = '     ') & 
  theme(plot.tag = element_text(color = "grey30", family = "Montserrat"))
#fullPlot

ggsave("Figures/App 1/Figure2.png", fullPlot, height = 14, width = 10.5)

