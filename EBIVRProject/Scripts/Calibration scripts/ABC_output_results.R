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

CalOutsHPC <- read.csv("Data/Calibration/CalAnalysisOuputs_20012023.csv")

CalOutsHPC <- CalOutsHPC %>%  
  drop_na()

vlines <- tibble(
  pattern = c("p1.fit","p2.fit","p3.fit","p4.fit","p5.fit","p6.fit","p7.fit","p8.fit","p9.fit","p10.fit","p11.fit","p12.fit","p13.fit","p15.fit","p16.fit","p17.fit"),
  xint = c(0,0,0.36,0.36,0.36-0.095, 0.36,0.36,0.36,0.36,0.36,0,0.36,0.36,0,0,0.36)
) 
vlines <- vlines %>% 
  mutate(pattern = factor(pattern, levels = c("p1.fit","p2.fit","p3.fit","p4.fit","p5.fit","p6.fit","p7.fit","p8.fit","p9.fit","p10.fit","p11.fit","p12.fit","p13.fit","p15.fit","p16.fit","p17.fit"))) 
  
pattern.labs <- c("p1.fit" = "Pattern 1","p2.fit" = "Pattern 2","p3.fit" = "Pattern 3","p4.fit" = "Pattern 4","p5.fit" = "Pattern 5","p6.fit" = "Pattern 6","p7.fit" = "Pattern 7","p8.fit" = "Pattern 8","p9.fit" = "Pattern 9","p10.fit" = "Pattern 10","p11.fit" = "Pattern 11","p12.fit" = "Pattern 12","p13.fit" = "Pattern 13","p15.fit" = "Pattern 14","p16.fit" = "Pattern 15","p17.fit" = "Pattern 16")

fitsPlot <- CalOutsHPC %>% 
  select(-X, -run.num, -p14.fit) %>% 
  pivot_longer(everything(), names_to = "pattern", values_to = "error") %>% 
  mutate(pattern = factor(pattern, levels = c("p1.fit","p2.fit","p3.fit","p4.fit","p5.fit","p6.fit","p7.fit","p8.fit","p9.fit","p10.fit","p11.fit","p12.fit","p13.fit","p15.fit","p16.fit","p17.fit"))) %>% 
  ggplot(aes(x = error)) +
  geom_vline(data = vlines, aes(xintercept = xint, group = pattern), col = "#78658B", size = 1.2, alpha = 0.75) + 
  geom_histogram(alpha = 0.5) +
  facet_wrap( ~ pattern, scales = "free", labeller = labeller(pattern = pattern.labs)) +
  labs( x = "Median absolute scaled error", y = "Count") + 
  theme_classic()  +
  theme(text = element_text(size = textSizeAxisTitle * 1.5, color = "grey30", family = "Montserrat"), 
        strip.background = element_rect(color=NA, fill="#78658B", size=1, linetype="solid"),
        strip.text.x = element_text(size = textSizeAxisTitle * 2, color = "white", family = "Montserrat")
  )
fitsPlot

#ggsave("Figures/CalOuts/FitsWThresh.png", fitsPlot, width = 12, height = 7.5)


thresh <- 0.360

fits <- CalOutsHPC %>%
  filter(round(p1.fit, digits = 3) == 0,
         round(p2.fit, digits = 3) == 0,
         round(p3.fit, digits = 3) <= thresh,
         round(p4.fit, digits = 3) <= thresh,
         round(p5.fit, digits = 3) <= thresh - 0.095,
         round(p6.fit, digits = 3) <= thresh,
         round(p7.fit, digits = 3) <= thresh,
         round(p8.fit, digits = 3) <= thresh,
         round(p9.fit, digits = 3) <= thresh,
         round(p10.fit, digits = 3) <= thresh,
         round(p11.fit, digits = 3) == 0,
         round(p12.fit, digits = 3) <= thresh,
         round(p13.fit, digits = 3) <= thresh,
         round(p14.fit, digits = 3) <= 0,
         round(p15.fit, digits = 3) <= 0,
         round(p16.fit, digits = 3) <= thresh)


### Pull from completed runs file. Then attach density values.
inputParams <- fread("./../../MetabolicVariationLargeOuts/CalOutsCompletedRuns.csv", select = c(2, 17:26, 39:40))


inputParams <- inputParams %>% 
  rename(run.num = X.run.number.)

fitsPs <- left_join(inputParams, fits[,1:2], by='run.num')
fitsPs <- fitsPs %>% drop_na()

#write.csv(fitsPs, "Data/Calibration/ABCParameterTableUpd.csv")

# check fits
runNums <- fits$run.num

cols <- fread("../../MetabolicVariationLargeOuts/CalOutsCompletedRuns.csv", nrows = 1)
cols <- colnames(cols)

rowIDs <- fread("../../MetabolicVariationLargeOuts/CalOutsCompletedRuns.csv", select = 2)
rowIDs <- rowIDs %>% 
  mutate(row.num = row_number()) %>% 
  filter(X.run.number. %in% runNums)


for (i in 1:length(runNums)) {
  fitsI <- fread("../../MetabolicVariationLargeOuts/CalOutsCompletedRuns.csv", skip = rowIDs$row.num[i], nrows = 1)
  colnames(fitsI) <- cols
  
  if (i == 1) {
    fitsAll <- fitsI
  } else {
    fitsAll <- bind_rows(fitsAll, fitsI)
  }
  
  print(i)
}

fitsAll <- fitsAll %>% rename(run.num = X.run.number.)

#### Densities ####
dens <- fitsAll %>% select(run.num, dens.mean, dens.med)

dens <- dens %>% filter(
  dens.mean >= 17.49 - 11.78 & dens.mean <= 17.49 + 11.78,
  dens.med >= 14.2 - 11.78 & dens.med <= 14.2 + 11.78
)

#### update to include only those with sufficient densities ####

fitsAll <- fitsAll %>% 
  filter(run.num %in% dens$run.num)

#write.csv(fitsAll, "Data/Calibration/ABCOutputsSelectedSets.csv")

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

# plot1a <- ggplot(p1, aes(x = value, fill = as.factor(run.num))) +
#   geom_histogram(bins = 25, position="stack") +
#   scale_x_continuous(breaks = c(0, 0.0005,0.0010,0.0015,0.0020,0.0025,0.0030), labels = c("0","0.5","1.0","1.5","2.0","2.5","3.0")) +
#   annotate("rect", xmin=0.001, xmax=0.0025, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
#   labs(y = "Count", x = "Neonate mass [g]") +
#   scale_fill_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30"),
#         legend.position = "none",
#         plot.margin = margin(0, 0, 0, 0),
#         axis.line = element_line(color = "grey30"))
# 
# plot1b <- ggplot(p1) +
#   geom_boxplot(aes(x = value, y = factor(0)), lwd = 0.6, col = pal[1], outlier.shape = NA) +
# #  stat_summary(fun=mean, geom="point", aes(x = value, col = as.factor(run.num), y = factor(0.1)), size = 0.5, position = position_jitter(height = 0.2)) +
#   stat_summary(fun=mean, geom="point", aes(x = value, col = as.factor(run.num), y = factor(0.1)), size = 0.5, position = position_dodge(width = 0.5)) +
#   stat_summary(fun.data = mean_sdl, geom = "errorbar", aes(x = value, col = as.factor(run.num), y = factor(0.1)), size = 0.5, position = position_dodge(width = 0.5)) +
#   scale_x_continuous(breaks = c(0,0.0005,0.0010,0.0015,0.0020,0.0025,0.0030)) +
#   scale_color_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30")) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank(),
#         plot.margin = margin(0, 0, 0, 0))
# 
# plot1 <- plot1b / plot1a + plot_layout(heights = c(0.2, 1)) 
# #plot1

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

####  Pattern 2. Birth mass by litter size  #### 

p2 <- fitsAll %>%  select(run.num, p2.BMxLS)

p2 <- p2 %>% 
  separate(p2.BMxLS, nam, sep = " ") %>% 
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
  rename(BM = `1`, LS = `2`) %>%
  group_by(run.num) %>%
  filter(!(n_distinct(BM) == 1))

plot2 <-  ggplot(p2, aes(x = LS, y = BM * 1000)) +
 # geom_jitter(alpha = 0.1, width  = 0.1, size = 0.7)  +
  geom_smooth(aes(col = as.factor(run.num)),
              method = "lm",
              se = FALSE,
              linewidth = 0.7) +
  geom_smooth(method = "lm",
              se = FALSE,
              col = pal[1], 
              linewidth = 1.5) +
  labs(y = "Neonate mass [g]", x = "Litter size [N pups]") +
  scale_x_continuous(breaks = seq(1:9)) +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_line(color = "grey30"),
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"))
#plot2

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
         p4.pattern = 0.01930132*(1 - (1 - (0.004688886 / 0.01930132)^(1/3))*exp((-0.04845531*age)/3))^3) #%>% 
  #mutate(p3.fit = NA, p4.fit = NA)     


# plot3 <- ggplot(p3, aes(x = round(age))) +
#  # geom_hline(yintercept = 13.66) +
# #  geom_hline(yintercept = 26.59) +
#   geom_point(aes(y = BM * 1000, col = as.factor(run.num)), size = 0.5, alpha = 0.3)  +
#  # geom_smooth(aes(y = BM * 1000), method = "loess", se = FALSE, col = pal[1], span = 0.6)  +
#   geom_line(aes(y = p3.pattern * 1000), size = 0.5, linetype = "dashed", col = "grey20") +
#   geom_point(data = GrowthDat, aes(x = Age_days, y = Mass_kg * 1000), col = "grey20", fill = "grey40", shape = 25, size = 0.7) + 
#   labs(y = "Body mass [g]", x = "Age [days]") +
#   scale_color_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30"),
#         legend.position = "none",
#         axis.line = element_line(color = "grey30"))
# plot3 <- plot3  + 
#   plot_annotation(title = 'B') & 
#   theme(title = element_text(size = textSizeAxisTitle, color = "grey30"))
# 
# plot4 <- ggplot() +
#   geom_point(data = p3, aes(x = age, col = as.factor(run.num), y = LM * 1000), alpha = 0.3, size = 0.7)  +
#   geom_line(data = p3, aes(x = age, col = as.factor(run.num), y = p4.pattern * 1000), size = 0.5, linetype = "dashed", col = "grey20") +
#   geom_point(data = GrowthDat, aes(x = Age_days, y = Mass_kg_lm * 1000, fill = Perc_fat_R), col = "grey20", shape = 25, size = 0.7) +
#   labs(y = "Lean body mass [g]", x = "Age [days]") +
#   scale_color_manual(values = pal) +
#   scale_fill_manual(values = c("grey40", "grey80")) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30"),
#         legend.position = "none",
#         axis.line = element_line(color = "grey30"))
#plot4


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


####  Pattern 4. Lean body mass by age  #### 

outputsMean <- data.frame(  
  runNum=numeric(),
  A=numeric(),
  Asd=numeric(),
  k=numeric(),
  ksd=numeric(),
  Mo=numeric(),
  Mosd=numeric(),
  AIC=numeric())

n <- 1

for (h in unique(p3$run.num)) {
  
  for (i in pots) {
    run <- p3 %>% filter(run.num == h)
    
    tryCatch({
      k1 = i
      A1 =  max(run$LM)
      to1 = max(run$age)
      Mo1 = min(run$LM)
      fitMean <- fit.bertalanffy(run$age, run$LM)  # run formula
      # print(i)
    }, error = function(e) {
      
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


p4alt <- tibble(age = seq(21, 620))

n <- 1

for (h in unique(outputsMean$runNum)) {
  
  # h <- unique(outputsMean$runNum)[1]
  
  p4alt <-  p4alt %>%  
    mutate(col = outputsMean[n,2]*(1 - (1 - (outputsMean[n,6] / outputsMean[n,2])^(1/3))*exp((-outputsMean[n,4]*age)/3))^3) %>% 
    rename(!!as.character(h) := col)
  
  
  n <- n + 1
  
}

p4alt <- p4alt %>% 
  pivot_longer(cols = -age) %>% 
  rename(run.num = name, mean = value)

plot4 <- ggplot() +
  geom_point(data = p3, aes(x = age, y = LM * 1000), col = "grey80", size = 0.5) + 
  geom_line(data = p4alt, aes(y = mean * 1000, x = age, col = as.factor(run.num)), size = 0.7) +
  geom_line(data = p3, aes(y = p4.pattern * 1000, x = age), linewidth = 1, col = "grey20") +
  geom_point(data = GrowthDat, aes(x = Age_days, y = Mass_kg_lm * 1000, fill = Perc_fat_R), col = "grey20", shape = 25, size = 0.7) +
  labs(y = "Lean mass [g]", x = "Age [days]") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = c("grey40", "grey80")) +
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
# plot11 <- plot11 + 
#   plot_annotation(title = 'K') & 
#   theme(title = element_text(size = textSizeAxisTitle, color = "grey30"))


####  Pattern 12. Litter size at birth  #### 

p12 <- fitsAll %>%  select(run.num, p12.LSb)

nam <- c(as.character(1:10000))

p12 <- p12 %>% 
  separate(p12.LSb, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 

# plot12a <- ggplot(p12) +
#   geom_histogram(aes(x = value, fill = as.factor(run.num)), binwidth = 1, position = "stack") +
#   annotate("rect", xmin=3.6, xmax=6.1, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
#   scale_x_continuous(breaks = seq(1:9)) +
#   labs(y = "Count", x = "Litter size [N pups]") +
#   scale_fill_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30"),
#         legend.position = "none",
#         plot.margin = margin(0, 0, 0, 0),
#         axis.line = element_line(color = "grey30"))
# 
# plot12b <- ggplot(p12) +
#   geom_boxplot(aes(x = value, y = factor(0)), lwd = 0.6, size = 0.5, col = pal[1], outlier.shape = NA) +
#   stat_summary(fun=mean, geom="point", aes(x = value, col = as.factor(run.num), y = factor(0.1)), size = 0.5, position = position_jitter(height = 0.2)) +
#   scale_x_continuous(breaks = seq(1:9)) +
#   labs(y = NULL, x = "Litter size [N pups]") +
#   scale_color_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30")) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank(),
#         plot.margin = margin(0, 0, 0, 0))
# 
# plot12 <- plot12b / plot12a + plot_layout(heights = c(0.2, 1)) 
# #plot12

plot12 <- ggplot(p12) +
  annotate("rect", ymin=3.6, ymax=6.1, xmin=-Inf, xmax=Inf, alpha=0.2, fill="black") +
  geom_boxplot(aes(x = 0.3, y = value),  col = pal[1], linewidth = 0.7,  width = 0.05, fill = pal[1], alpha = 0.25, outlier.shape = NA, notch = TRUE) +
  geom_pointrange(mapping = aes(x= 0, col = as.factor(run.num), y = value),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median, 
                  linewidth = 0.5, size = 0.3, position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = seq(1,12)) +
  scale_x_continuous(breaks = c(0,0.3),limits = c(-0.25,0.325), labels = c("Ind. runs", "All")) +
  labs(y = "Birth litter\nsize [N pups]", x = NULL) +
  scale_color_manual(values = pal) +
  theme_classic() +
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none",
        text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat", lineheight = 0.35))


####  Pattern 13. Litter size at weaning  #### 

p13 <- fitsAll %>%  select(run.num, p13.LSw)

nam <- c(as.character(1:10000))

p13 <- p13 %>% 
  separate(p13.LSw, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         num = as.numeric(num), value = as.numeric(value)) %>%
  drop_na() 


# plot13a <-  ggplot(p13, aes(x = value, fill = as.factor(run.num))) +
#   geom_histogram(binwidth = 1, position = "stack") +
#   annotate("rect", xmin=1.28, xmax=5.28, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
#   scale_x_continuous(breaks = seq(1:9)) +
#   scale_fill_manual(values = pal) +
#   labs(y = "Count", x = "Litter size [N pups]") +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30"),
#         legend.position = "none",
#         plot.margin = margin(0, 0, 0, 0),
#         axis.line = element_line(color = "grey30"))
# 
# plot13b <- ggplot(p13) +
#   geom_boxplot(aes(x = value, y = factor(0)), lwd = 0.6, size = 0.5, col = pal[1], outlier.shape = NA) +
#   stat_summary(fun=mean, geom="point", aes(x = value, col = as.factor(run.num), y = factor(0.1)), size = 0.5, position = position_jitter(height = 0.2)) +
#   scale_x_continuous(breaks = seq(1:9)) +
#   scale_color_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30")) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank(),
#         plot.margin = margin(0, 0, 0, 0))
# 
# plot13 <- plot13b / plot13a + plot_layout(heights = c(0.2, 1)) 
# #plot13


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


# plot14a <- ggplot(p14, aes(x = value * 100, fill = as.factor(run.num))) +
#   geom_histogram(bins = 50, position = "stack") +
#   annotate("rect", xmin=3, xmax=29, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
#   labs(y = "Count", x = "Storage level [% body fat]") +
#   scale_fill_manual(values = pal) +
#   xlim(0,40) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30"),
#         legend.position = "none",
#         plot.margin = margin(0, 0, 0, 0),
#         axis.line = element_line(color = "grey30"))
# 
# plot14b <- ggplot(p14) +
#   geom_boxplot(aes(x = value * 100, y = factor(0)), lwd = 0.6, size = 0.5, col = pal[1], outlier.shape = NA, notch = TRUE) +
#   stat_summary(fun=mean, geom="point", aes(x = value * 100, col = as.factor(run.num), y = factor(0.1)), size = 0.5, position = position_jitter(height = 0.2)) +
#   xlim(0,40) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30")) +
#   scale_color_manual(values = pal) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank(),
#         plot.margin = margin(0, 0, 0, 0))
# 
# plot14 <- plot14b / plot14a + plot_layout(heights = c(0.2, 1))
# #plot14


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

# plot15a <- ggplot(p15, aes(x = value * 100, fill = as.factor(run.num))) +
#   geom_histogram(bins = 40, position = "stack") +
#   annotate("rect", xmin=0, xmax=3, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
#   xlim(0,40) +
#   labs(y = "Count", x = "Storage level [% body fat]") +
#   scale_fill_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30"),
#         legend.position = "none",
#         plot.margin = margin(0, 0, 0, 0),
#         axis.line = element_line(color = "grey30"))
# 
# plot15b <- ggplot(p15) +
#   geom_boxplot(aes(x = value * 100, y = factor(0)), lwd = 0.6, size = 0.5, col = pal[1], outlier.shape = NA, notch = TRUE) +
#   stat_summary(fun=mean, geom="point", aes(x = value * 100, col = as.factor(run.num), y = factor(0.1)), size = 0.5, position = position_jitter(height = 0.2)) +
#   xlim(0,40) +
#   labs(y = NULL, x = "Litter size [N pups]") +
#   scale_color_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30")) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank(),
#         plot.margin = margin(0, 0, 0, 0))
# 
# plot15 <- plot15b / plot15a + plot_layout(heights = c(0.2, 1)) 
# #plot15

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
  left_join(p16.pattern, by = "BM") #%>% 
  #mutate(BM = factor(BM, levels = as.character(seq(round(min(p3$BM)*1000),round(max(p3$BM)*1000),1))))
         
# p16.pattern <- p16.pattern %>% 
#   mutate(BM = factor(BM, levels = as.character(seq(round(min(p3$BM)*1000),round(max(p3$BM)*1000),1))))


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

#### All plots ####
 
# ggsave("Figures/CalOuts/IndPatternPlots/plot1.png", plot1)
# ggsave("Figures/CalOuts/IndPatternPlots/plot2.png", plot2)
# ggsave("Figures/CalOuts/IndPatternPlots/plot3.png", plot3)
# ggsave("Figures/CalOuts/IndPatternPlots/plot4.png", plot4)
# ggsave("Figures/CalOuts/IndPatternPlots/plot5.png", plot5)
# ggsave("Figures/CalOuts/IndPatternPlots/plot6.png", plot6)
# ggsave("Figures/CalOuts/IndPatternPlots/plot7.png", plot7)
# ggsave("Figures/CalOuts/IndPatternPlots/plot8.png", plot8)
# ggsave("Figures/CalOuts/IndPatternPlots/plot9.png", plot9)
# ggsave("Figures/CalOuts/IndPatternPlots/plot10.png", plot10)
# ggsave("Figures/CalOuts/IndPatternPlots/plot11.png", plot11)
# ggsave("Figures/CalOuts/IndPatternPlots/plot12.png", plot12)
# ggsave("Figures/CalOuts/IndPatternPlots/plot13.png", plot13)
# ggsave("Figures/CalOuts/IndPatternPlots/plot14.png", plot14)
# ggsave("Figures/CalOuts/IndPatternPlots/plot15.png", plot15)
# ggsave("Figures/CalOuts/IndPatternPlots/plot16.png", plot16, width = 10)

fullPlot <- 
  (plot1 | plot3 | plot4 ) /
  (plot5 | plot6 | plot7 ) /
  (plot8 | plot9 | plot10 ) /
  (plot2 | plot11 | plot12 ) /
  (plot13 | plot14 | plot15 ) /
  (plot16)+ 
  plot_layout(heights = c(1,1,1,1,1,2)) + 
  plot_annotation(tag_levels = 'A', tag_suffix = '     ') & 
  theme(plot.tag = element_text(color = "grey30", family = "Montserrat"))
#fullPlot

#ggsave("Figures/CalOuts/fullOutPlotSimp.png", fullPlot, height = 14, width = 11)


#### Density plots ####

plotDensMean <- ggplot(fitsAll, aes(x = 0.5, y = dens.mean)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.4, ymax = 41.4, alpha = 0.25, col = "grey20", linetype = "dotted", fill = NA, size = 1) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 17.49 - 11.78, ymax = 17.49 + 11.78, alpha = 0.25, fill = "grey85") +
  geom_flat_violin(position = position_nudge(x = .2), lwd = 0.9, col = NA, fill = pal[4], alpha = 0.5) +
  geom_boxplot(aes(x = 0.6), width = .1, lwd = 0.6, col = pal[4], fill = pal[4], alpha = 0.5) + 
  geom_hline(yintercept = 17.5, size = 2, linetype = "dashed", col = "grey20") +
  coord_flip() +
  ylim(0,45) + 
  labs(y = "Mean density [Voles Ha-1]", x = NULL) + 
  theme_classic() +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        strip.text = element_text(color = "grey30", size = textSizeAxisText, family = "Montserrat"),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey30", fill=NA, size=1))

plotDensMed <- ggplot(fitsAll, aes(x = 0.5, y = dens.med)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 14.2 - 11.78, ymax = 14.2 + 11.78, alpha = 0.75, fill = "grey85") +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.4, ymax = 41.4, alpha = 0.25, col = "grey20", linetype = "dotted", fill = NA, size = 1) +
  geom_flat_violin(position = position_nudge(x = .2), lwd = 0.9, col =  NA, fill = pal[4], alpha = 0.5) +
  geom_boxplot(aes(x = 0.6), width = .1, lwd = 0.6, col = pal[4], fill = pal[4], alpha = 0.5) + 
  geom_hline(yintercept = 14.2, size = 2, linetype = "dashed", col = "grey20", family = "Montserrat") +
  coord_flip() +
  ylim(0,45) + 
  labs(y = "Median density [Voles Ha-1]", x = NULL) + 
  theme_classic() +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        strip.text = element_text(color = "grey30", size = textSizeAxisText, family = "Montserrat"),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey30", fill=NA, size=1))

plotDens <- plotDensMean /
  plotDensMed
plotDens

#ggsave("Figures/CalOuts/DensitiesFinal.png", plotDens)

#### Graveyard ####
# Previous pattern 14: Probability of weaning by mother body mass 
# 
# p14 <- fitsAll %>%  select(run.num, p14.PwxBM)
# 
# p14.pattern <- read_excel("Data/Verification/Reproduction/JonssonetalData.xlsx")
# p14.pattern <- p14.pattern %>% 
#   rename(BM = mass)
# 
# nam <- c(as.character(1:10000))
# 
# p14 <- p14 %>% 
#   separate(p14.PwxBM, nam, sep = " ") %>% 
#   gather(key = "num", value = "value", -run.num) %>% 
#   mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
#          value = as.numeric(value),
#          num = as.numeric(num)) %>% 
#   arrange(run.num, num) %>% 
#   drop_na() %>% 
#   group_by(run.num) %>% 
#   mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
#   pivot_wider(names_from = order, values_from = value) %>% 
#   rename(pW = `1`, BM = `2`) %>% 
#   mutate(BM = round(BM * 1000)) %>% 
#   group_by(run.num, BM) %>% 
#   summarise(pW = mean(pW, na.rm = TRUE)) 
# 
# plot14 <-  ggplot(p14, aes(x = BM, y = pW, col = as.factor(run.num))) +
#   geom_point(alpha = 0.5)  +
#   geom_smooth(method = "glm",
#               method.args = list(family = "binomial"),
#               se = FALSE,
#               size = 1.5) +
#   geom_line(data = p14.pattern, aes(x = BM, y = prob), size = 2, linetype = "dashed", col = "grey20") +
#   labs(y = "Probability of weaning success", x = "Mother mass [g]") +
#   scale_color_manual(values = pal) +
#   theme_classic() +
#   theme(text = element_text(size = textSizeAxisTitle, color = "grey30"),
#         legend.position = "none")
#plot14

#### geom_flat_violin ####


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }

#'geom_flat_violin_HELPER2
#'
#' Borrowed from
#' \href{https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R}{Ben Marwick}.
#' Original author David Robinson.
#'
#' @format NULL
#' @usage NULL
GeomFlatViolin <-
  ggplot2::ggproto(
    "GeomFlatViolin",
    ggplot2::Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = ggplot2::draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )


