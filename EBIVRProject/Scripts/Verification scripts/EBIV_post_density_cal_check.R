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
library(NatParksPalettes)


#### Load dataset ####

CalOuts <- read.csv("./../../MetabolicVariationLargeOuts/DensCal-5368.csv", skip = 6)
CalOuts <- read.csv("Data/Verification/CalCheckBMRlm.csv", skip = 6)

CalOuts <- CalOuts %>% 
  rename(run.num = X.run.number.) %>% 
  mutate(cal.param.row = run.num)



#### plotting outputs
# Olympic, BryceCanyon, DeathValley !, Denali !,   Saguaro !, Volcanoes !, Yellowstone
pal <- natparks.pals(name="Volcanoes", n=500, type="continuous")

####  Pattern 1. Fetal mass at birth  #### 

p1 <- CalOuts %>%  select(run.num, cal.param.row, p1.FMAB)
nam <- c(as.character(1:10000))

p1 <- p1 %>% 
  separate(p1.FMAB, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -cal.param.row) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>% 
  drop_na()

plot1a <- ggplot(p1, aes(x = value, fill = as.factor(run.num))) +
  geom_histogram(bins = 25, position="stack") +
  scale_x_continuous(breaks = c(0, 0.0005,0.0010,0.0015,0.0020,0.0025,0.0030), labels = c("0","0.5","1.0","1.5","2.0","2.5","3.0")) +
  annotate("rect", xmin=0.001, xmax=0.0025, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
  labs(y = "Count", x = "Neonate mass [g]") +
  scale_fill_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")

plot1b <- ggplot(p1) +
  geom_boxplot(aes(x = value, y = factor(0)), size = 1.1, col = pal[15]) +
  stat_summary(fun=mean, geom="point", aes(x = value, col = as.factor(run.num), y = factor(0.1)), size = 2, position = position_jitter(height = 0.2)) +
  scale_x_continuous(breaks = c(0,0.0005,0.0010,0.0015,0.0020,0.0025,0.0030)) +
  labs(y = NULL, x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10")) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

plot1 <- plot1b / plot1a + plot_layout(heights = c(0.2, 1)) 
plot1

####  Pattern 2. Birth mass by litter size  #### 

p2 <- CalOuts %>%  select(run.num, cal.param.row, p2.BMxLS)

p2 <- p2 %>% 
  separate(p2.BMxLS, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -cal.param.row) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(BM = `1`, LS = `2`) %>%
  group_by(run.num) %>%
  filter(!(n_distinct(BM) == 1))

plot2 <-  ggplot(p2, aes(x = LS, y = BM * 1000, col = as.factor(run.num))) +
  geom_jitter(alpha = 0.1, width  = 0.1)  +
  geom_smooth(method = "lm",
              se = FALSE,
              #col = "cadetblue", 
              size = 1.5) +
  labs(y = "Neonate mass [g]", x = "Litter size [N pups]") +
  scale_x_continuous(breaks = seq(1:9)) +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot2

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

p3 <- CalOuts %>% select(run.num, cal.param.row, p34.BMLMxA)

nam <- c(as.character(1:35000))

p3 <- p3 %>%
  separate(p34.BMLMxA, nam, sep = " ") %>%
  gather(key = "num", value = "value", -run.num, -cal.param.row) %>% 
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
         p4.pattern = 0.01930132*(1 - (1 - (0.004688886 / 0.01930132)^(1/3))*exp((-0.04845531*age)/3))^3) %>% 
  mutate(p3.fit = NA, p4.fit = NA)     

plot3 <- ggplot(p3, aes(x = age, col = as.factor(run.num))) +
  geom_point(aes(y = BM * 1000), alpha = 0.1)  +
  geom_line(aes(y = p3.pattern * 1000), size = 2, linetype = "dashed", col = "grey20") +
  geom_point(data = GrowthDat, aes(x = Age_days, y = Mass_kg * 1000), col = "grey20", fill = "grey40", shape = 25, size = 1.5) + 
  labs(y = "Body mass [g]", x = "Age [days]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot3

plot4 <- ggplot() +
  geom_point(data = p3, aes(x = age, col = as.factor(run.num), y = LM * 1000), alpha = 0.1)  +
  geom_line(data = p3, aes(x = age, col = as.factor(run.num), y = p4.pattern * 1000), size = 2, linetype = "dashed", col = "grey20") +
  geom_point(data = GrowthDat, aes(x = Age_days, y = Mass_kg_lm * 1000, fill = Perc_fat_R), col = "grey20", shape = 25, size = 1.5) +
  labs(y = "Lean body mass [g]", x = "Age [days]") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = c("grey40", "grey80")) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot4

####  Patterns 5, 6, & 7  #### 


p567.pattern <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig1Dat.xlsx")
p567.pattern <- p567.pattern %>% rename(dsb = ageGest)

p5 <- CalOuts %>% select(run.num, cal.param.row, p567.dsbxMMFILM)

nam <- c(as.character(1:100000))

p5 <- p5 %>%
  separate(p567.dsbxMMFILM, nam, sep = " ") %>%
  gather(key = "num", value = "value",-run.num, -cal.param.row) %>% 
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
 # geom_point(data = p5, aes(x = dsb, col = as.factor(run.num), y = MM * 1000), alpha = 0.05, size = 1, position = position_dodge(width = 0.75)) +
  geom_ribbon(data = p5Rib, aes(x = dsb, ymin = Lo, ymax = Hi), alpha = 0.4, fill = "grey20") + 
  geom_smooth(data = p5, aes(x = dsb, col = as.factor(run.num), y = MM * 1000), method = "gam", size = 1.5, se = FALSE) +
  geom_line(data = p5Rib, aes(x = dsb, y = Mean), size = 2, linetype = "dashed", col = "grey20") +
  labs(y = "Mother body mass [g]", x = "Pup age [days]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot5

####  Pattern 6. Lactating mother food intake by pup age  #### 
p6Rib <- p567.pattern %>% 
  select(dsb, Metric, foodCons) %>% 
  mutate(foodCons = foodCons * 17.80) %>% 
  pivot_wider(names_from = Metric, values_from = foodCons) %>% 
  filter(dsb != 0)

plot6 <- ggplot() +
#  geom_point(data = p5, aes(x = dsb, col = as.factor(run.num), y = FI), alpha = 0.05, size = 1, position = position_dodge(width = 0.75)) +
  geom_ribbon(data = p6Rib, aes(x = dsb, ymin = Lo, ymax = Hi), alpha = 0.4, fill = "grey20") + 
  geom_smooth(data = p5, aes(x = dsb, col = as.factor(run.num), y = FI), method = "gam", size = 1.5, se = FALSE) +
  geom_line(data = p6Rib, aes(x = dsb, y = Mean), size = 2, linetype = "dashed", col = "grey20") +
  labs(y = "Mother food intake [kJ]", x = "Pup age [days]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot6

####  Pattern 7. Total litter mass by pup age  #### 
p7Rib <- p567.pattern %>% 
  select(dsb, Metric, LitterMass) %>% 
  pivot_wider(names_from = Metric, values_from = LitterMass) %>% 
  filter(dsb != 0)

plot7 <- ggplot() +
 # geom_point(data = p5, aes(x = dsb, col = as.factor(run.num), y = LitM * 1000), alpha = 0.05, size = 1, position = position_dodge(width = 0.75)) +
  geom_ribbon(data = p7Rib, aes(x = dsb, ymin = Lo, ymax = Hi), alpha = 0.4, fill = "grey20") + 
  geom_smooth(data = p5, aes(x = dsb, col = as.factor(run.num), y = LitM * 1000), method = "gam", size = 1.5, se = FALSE) +
  geom_line(data = p7Rib, aes(x = dsb, y = Mean), size = 2, linetype = "dashed", col = "grey20") +
  labs(y = "Total litter mass [g]", x = "Pup age [days]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot7


####  Patterns 8, 9, & 10  #### 

p8910.pattern <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig2Dat.xlsx")
p8910.pattern <- p8910.pattern %>% rename(LS = litterSize)

p8 <- CalOuts %>% select(run.num, cal.param.row, p8910.LSxpFIEUMEO)

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
  geom_point(aes(x = as.factor(LS), col = as.factor(run.num), y = pFI), alpha = 0.2, size = 1, position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(x = as.factor(LS), y = pFI), col = pal[15], width = 0.35, fill = pal[15], alpha = 0.25, size = 1.2, outlier.shape = NA, notch = TRUE) +
  geom_line(aes(x = LS, y = FC), size = 2, linetype = "dashed", col = "grey20") +
  scale_x_discrete(breaks = seq(1:9)) +
  labs(y = "Mother food intake [kJ]", x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot8

####  Pattern 9. Mother peak energy use by litter size  #### 

plot9 <- ggplot(p8) +
  geom_point(aes(x = as.factor(LS), col = as.factor(run.num), y = pEU), alpha = 0.2, size = 1, position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(x = as.factor(LS), y = pEU), col = pal[15], width = 0.35, fill = pal[15], alpha = 0.25, size = 1.2, outlier.shape = NA, notch = TRUE) +
  geom_line(aes(x = LS, y = ADMR), size = 2, linetype = "dashed", col = "grey20") +
  scale_x_discrete(breaks = seq(1:9)) +
  labs(y = "Mother energy use [kJ]", x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot9

####  Pattern 10. Mother peak milk transfer by litter size  #### 

plot10 <-  ggplot(p8) +
  geom_point(aes(x = as.factor(LS), col = as.factor(run.num), y = pMEO), alpha = 0.2, size = 1, position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(x = as.factor(LS), y = pMEO), col = pal[15], width = 0.35, fill = pal[15], alpha = 0.25, size = 1.2, outlier.shape = NA, notch = TRUE) +
  geom_line(aes(x = LS, y = MEO), size = 2, linetype = "dashed", col = "grey20") +
  scale_x_discrete(breaks = seq(1:9)) +
  labs(y = "Mother milk transfer [kJ]", x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot10


####  Pattern 11. Pup mass at weaning by litter size  #### 

p11 <- CalOuts %>% select(run.num, cal.param.row, p11.PMxLS)

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

plot11 <- ggplot(p11, aes(x = LS, y = WM * 1000, col = as.factor(run.num))) +
  geom_jitter(alpha = 0.25, width  = 0.1)  +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1.5) +
  labs(y = "Weaning mass [g]", x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = seq(1:9)) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot11


####  Pattern 12. Litter size at birth  #### 

p12 <- CalOuts %>% select(run.num, cal.param.row, p12.LSb)

nam <- c(as.character(1:10000))

p12 <- p12 %>% 
  separate(p12.LSb, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 

plot12a <- ggplot(p12) +
  geom_histogram(aes(x = value, fill = as.factor(run.num)), binwidth = 1, position = "stack") +
  annotate("rect", xmin=3.6, xmax=6.1, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
  scale_x_continuous(breaks = seq(1:9)) +
  labs(y = "Count", x = "Litter size [N pups]") +
  scale_fill_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")

plot12b <- ggplot(p12) +
  geom_boxplot(aes(x = value, y = factor(0)), size = 1.1, col = pal[15]) +
  stat_summary(fun=mean, geom="point", aes(x = value, col = as.factor(run.num), y = factor(0.1)), size = 2, position = position_jitter(height = 0.2)) +
  scale_x_continuous(breaks = seq(1:9)) +
  labs(y = NULL, x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10")) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

plot12 <- plot12b / plot12a + plot_layout(heights = c(0.2, 1)) 
plot12


####  Pattern 13. Litter size at weaning  #### 

p13 <- CalOuts %>% select(run.num, cal.param.row, p13.LSw)

nam <- c(as.character(1:10000))

p13 <- p13 %>% 
  separate(p13.LSw, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 


plot13a <-  ggplot(p13, aes(x = value, fill = as.factor(run.num))) +
  geom_histogram(binwidth = 1, position = "stack") +
  annotate("rect", xmin=1.28, xmax=5.28, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
  scale_x_continuous(breaks = seq(1:9)) +
  scale_fill_manual(values = pal) +
  labs(y = "Count", x = "Litter size [N pups]") +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")

plot13b <- ggplot(p13) +
  geom_boxplot(aes(x = value, y = factor(0)), size = 1.1, col = pal[15]) +
  stat_summary(fun=mean, geom="point", aes(x = value, col = as.factor(run.num), y = factor(0.1)), size = 2, position = position_jitter(height = 0.2)) +
  scale_x_continuous(breaks = seq(1:9)) +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10")) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

plot13 <- plot13b / plot13a + plot_layout(heights = c(0.2, 1)) 
plot13

####  Pattern 14. Probability of weaning by mother body mass  #### 

p14 <- CalOuts %>% select(run.num, cal.param.row, p14.PwxBM)

p14.pattern <- read_excel("Data/Verification/Reproduction/JonssonetalData.xlsx")
p14.pattern <- p14.pattern %>% 
  rename(BM = mass)

nam <- c(as.character(1:10000))

p14 <- p14 %>% 
  separate(p14.PwxBM, nam, sep = " ") %>% 
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
  rename(pW = `1`, BM = `2`) %>% 
  mutate(BM = round(BM * 1000)) %>% 
  group_by(run.num, BM) %>% 
  summarise(pW = mean(pW, na.rm = TRUE)) 

plot14 <-  ggplot(p14, aes(x = BM, y = pW, col = as.factor(run.num))) +
  geom_point(alpha = 0.5)  +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              size = 1.5) +
  geom_line(data = p14.pattern, aes(x = BM, y = prob), size = 2, linetype = "dashed", col = "grey20") +
  labs(y = "Probability of weaning success", x = "Mother mass [g]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot14

####  Pattern 15. Average / range of body fat %  #### 

p15 <- CalOuts %>% select(run.num, cal.param.row, p15.BF)

nam <- c(as.character(1:30000))

p15 <- p15 %>% 
  separate(p15.BF, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 


plot15a <- ggplot(p15, aes(x = value * 100, fill = as.factor(run.num))) +
  geom_histogram(bins = 50, position = "stack") +
  annotate("rect", xmin=3, xmax=29, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
  labs(y = "Count", x = "Storage level [% body fat]") +
  scale_fill_manual(values = pal) +
  xlim(0,40) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")

plot15b <- ggplot(p15) +
  geom_boxplot(aes(x = value * 100, y = factor(0)), size = 1.1, col = pal[15]) +
  stat_summary(fun=mean, geom="point", aes(x = value * 100, col = as.factor(run.num), y = factor(0.1)), size = 2, position = position_jitter(height = 0.2)) +
  xlim(0,40) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10")) +
  scale_color_manual(values = pal) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

plot15 <- plot15b / plot15a + plot_layout(heights = c(0.1, 1)) 
plot15


####  Pattern 16. Body fat % of living animals  #### 

p16 <- CalOuts %>% select(run.num, cal.param.row, p16.BF)

nam <- c(as.character(1:30000))

p16 <- p16 %>% 
  separate(p16.BF, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 

plot16a <- ggplot(p16, aes(x = value * 100, fill = as.factor(run.num))) +
  geom_histogram(bins = 40, position = "stack") +
  annotate("rect", xmin=0, xmax=3, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
  xlim(0,40) +
  labs(y = "Count", x = "Storage level [% body fat]") +
  scale_fill_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")

plot16b <- ggplot(p16) +
  geom_boxplot(aes(x = value * 100, y = factor(0)), size = 1.1, col = pal[15]) +
  stat_summary(fun=mean, geom="point", aes(x = value * 100, col = as.factor(run.num), y = factor(0.1)), size = 2, position = position_jitter(height = 0.2)) +
  xlim(0,40) +
  labs(y = NULL, x = "Litter size [N pups]") +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10")) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

plot16 <- plot16b / plot16a + plot_layout(heights = c(0.2, 1)) 
plot16


####  Pattern 17. Field metabolic rate by body mass #### 

p17 <- CalOuts %>% select(run.num, cal.param.row, p16.FMRxBM)

p17.pattern <- tibble(
  mass = seq(1,60,1),
  King1974 = 753 * (mass / 1000)^0.67,                 
  Nagy1999 = 5.48 * mass^0.71)

p17.pattern <- p17.pattern %>% 
  mutate(avgFMR = (King1974 + Nagy1999) / 2) %>% 
  rename(BM = mass)

nam <- c(as.character(1:30000))

p17 <- p17 %>% 
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
  left_join(p17.pattern, by = "BM") 

plot17 <-  ggplot(p17) +
#  geom_point(data = p17, aes(x = as.factor(BM), col = as.factor(run.num), y = FMR), alpha = 0.05, size = 1, position = position_dodge(width = 0.9)) +
  geom_ribbon(data = p17.pattern, aes(x = BM, ymin = King1974, ymax = Nagy1999), fill = "grey20", alpha = 0.25) +
  geom_line(data = p17.pattern, aes(x = BM, y = avgFMR), size = 2, linetype = "dashed", col = "grey20") +
  geom_boxplot(data = p17, aes(x = as.factor(BM), y = FMR), col = pal[10], width = 0.75, fill = pal[10], alpha = 0.25, size = 1, outlier.shape = NA, fatten = 1) +
  labs(y = "Field metabolic rate [kJ day-1]", x = "Body mass [g]") +
  scale_color_manual(values = pal) +
  scale_x_discrete(breaks = seq(from = 5, to = 60, by = 5)) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10"),
        legend.position = "none")
plot17

#### All plots ####

# ggsave("Figures/CalOuts/WithoutP14/plot1.png", plot1)
# ggsave("Figures/CalOuts/WithoutP14/plot2.png", plot2)
# ggsave("Figures/CalOuts/WithoutP14/plot3.png", plot3)
# ggsave("Figures/CalOuts/WithoutP14/plot4.png", plot4)
# ggsave("Figures/CalOuts/WithoutP14/plot5.png", plot5)
# ggsave("Figures/CalOuts/WithoutP14/plot6.png", plot6)
# ggsave("Figures/CalOuts/WithoutP14/plot7.png", plot7)
# ggsave("Figures/CalOuts/WithoutP14/plot8.png", plot8)
# ggsave("Figures/CalOuts/WithoutP14/plot9.png", plot9)
# ggsave("Figures/CalOuts/WithoutP14/plot10.png", plot10)
# ggsave("Figures/CalOuts/WithoutP14/plot11.png", plot11)
# ggsave("Figures/CalOuts/WithoutP14/plot12.png", plot12)
# ggsave("Figures/CalOuts/WithoutP14/plot13.png", plot13)
# ggsave("Figures/CalOuts/WithoutP14/plot14.png", plot14)
# ggsave("Figures/CalOuts/WithoutP14/plot15.png", plot15)
# ggsave("Figures/CalOuts/WithoutP14/plot16.png", plot16)
# ggsave("Figures/CalOuts/WithoutP14/plot17.png", plot17, width = 10)
# 
plot1
plot2
plot3
plot4
plot5
plot6
plot7
plot8
plot9
plot10
plot11
plot12
plot13
plot14
plot15
plot16
plot17



