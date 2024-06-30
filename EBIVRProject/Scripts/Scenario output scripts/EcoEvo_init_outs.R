# Cara Gallagher
# January 25th, 2024
# Energy Budget with Individual Variation project
# Initial eco-evo scenarios output analysis

##################################################
# Packages:
library(tidyverse)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

pal <- colorRampPalette(c("#BC4A53","#AABC4A","#E4AA4E","#95373F","#8D9C3A","#D48F20"))
pal2 <- colorRampPalette(c("#3F3F7B","#278192","#00B089","#AABC4A","#E5DC54","#E4AA4E","#E4714E","#BC4A53"))


##################################################

# read in outputs
EcoEvoOuts1 <- read.csv("Data/Scenarios/EcoEvo/EcoEvoOuts/eco_evo_init-9075.csv", skip = 6)
EcoEvoOuts2 <- read.csv("Data/Scenarios/EcoEvo/EcoEvoOuts/eco_evo_init-9096.csv", skip = 6)

EcoEvoOuts2$X.run.number. <- EcoEvoOuts2$X.run.number. + max(EcoEvoOuts1$X.run.number.)

EcoEvoOuts <- bind_rows(EcoEvoOuts1,EcoEvoOuts2)

#EcoEvoOuts <- read.csv("Data/Scenarios/EcoEvo/EcoEvoOuts/eco_evo_init-9096.csv", skip = 6)
EcoEvoOuts <- EcoEvoOuts %>%  select(c(1,12,13,18,20))
colnames(EcoEvoOuts) <- c("run.num", "version", "magnitude", "scenario", "all_outs")

nam <- c(as.character(1:100000))

EcoEvoOuts <- EcoEvoOuts %>% 
  separate(all_outs, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -run.num, -version, -magnitude, -scenario) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>%
  arrange(run.num, num) %>% 
  drop_na() %>% 
  group_by(run.num) %>% 
  mutate(order = rep(1:10, times = (max(row_number()) / 10)), num = rep(1:(max(row_number()) / 10), each = 10)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(year = `1`, day = `2`, ITVB = `3`, ITVR = `4`, ITVG = `5`, sdITVB = `6`, sdITVR = `7`, sdITVG = `8`, BM = `9`, abun = `10`) %>% 
  mutate(date = as.Date(paste(year, day, sep="-"), format="%Y-%j"))  %>% 
  mutate(scenario = ifelse(scenario == 0, "Control", 
                           ifelse(scenario == 1, "Positive pulses",
                                  ifelse(scenario == 2, "Negative pulses",
                                         ifelse(scenario == 3, "Positive and negative pulses", 
                                                scenario
                                         ))))) 

EcoEvoOuts$scenario <- factor(EcoEvoOuts$scenario, levels = c("Control", "Positive pulses", "Negative pulses", "Positive and negative pulses"))

n <- length(unique(EcoEvoOuts$run.num))

ggplot(EcoEvoOuts, aes(y = ITVR, x = date)) +
  geom_line(aes(col = interaction(version, run.num))) +
  geom_smooth(col = "grey5") +
  facet_grid(rows = vars(scenario), cols = vars(magnitude)) +
  theme(legend.position = "none") +
  theme_classic() +
  theme(text = element_text(size = 15, color = "grey30", family = "Montserrat"),
        axis.title = element_text(face="bold"),
        strip.text = element_text(face="bold"),
        legend.position = "none") +
  scale_color_manual(values = pal2(n))

ggplot(EcoEvoOuts, aes(y = BM, x = date)) +
  geom_line(aes(col = interaction(version, run.num))) +
  geom_smooth(col = "grey5") +
  facet_grid(rows = vars(scenario), cols = vars(magnitude))  +
  labs(y = "Body mass [g]", x = "Years since start") +
  theme_classic() +
  theme(text = element_text(size = 15, color = "grey30", family = "Montserrat"),
        axis.title = element_text(face="bold"),
        strip.text = element_text(face="bold"),
        legend.position = "none") +
  scale_color_manual(values = pal2(n))

ggplot(EcoEvoOuts, aes(y = abun, x = date)) +
  geom_line(aes(col = interaction(version, run.num))) +
  geom_smooth(col = "grey5") +
  facet_grid(rows = vars(scenario), cols = vars(magnitude)) +
  labs(y = "Population abundance [N]", x = "Years since start") +
  theme_classic() +
  theme(text = element_text(size = 15, color = "grey30", family = "Montserrat"),
        axis.title = element_text(face="bold"),
        strip.text = element_text(face="bold"),
        legend.position = "none") +
  scale_color_manual(values = pal2(n))



### Dot and whisker plot 

yr <- 225

EcoEvoMedians <- EcoEvoOuts %>% 
  filter(year >= yr) %>% 
  ungroup() %>% 
  filter(scenario == "Control") %>% 
  summarise(ITVB = median(ITVB),
            ITVR = median(ITVR), 
            ITVG = median(ITVG), 
            sdITVB = median(sdITVB),
            sdITVR = median(sdITVR), 
            sdITVG = median(sdITVG), 
            sdBM  = sd(BM), 
            sdabun = sd(abun), 
            BM  = median(BM), 
            abun = median(abun))

EcoEvoOutsLstYrs <- EcoEvoOuts %>% 
  filter(year >= yr) %>% 
  ungroup() %>% 
  select(-run.num, -version, -num, -year, -day, -date, -BM, -abun) %>% 
  mutate(ITVB = ITVB / EcoEvoMedians$ITVB,
         ITVR = 1 + ( 1 - (ITVR  / EcoEvoMedians$ITVR)), 
         ITVG = 1 + ( 1 - (ITVG / EcoEvoMedians$ITVG))) %>% 
  pivot_longer(cols = c("ITVB", "ITVR", "ITVG"), names_to = "output", values_to = "values") %>% 
  group_by(magnitude, scenario, output) %>% 
  summarise(stdev = sd(values),
            median = median(values),
            ci_up = quantile(values, probs = .75),
            ci_lo = quantile(values, probs = .25))

ggplot(subset(EcoEvoOutsLstYrs, scenario != "Control"), aes(x = output, linetype = as.factor(magnitude), col = interaction(output, magnitude))) +
  geom_hline(yintercept = 1, linewidth = 1, col = "grey30") +
  geom_pointrange(aes(y = median, ymin = ci_lo, ymax = ci_up), position = position_dodge(width = 0.5), size = 0.5, linewidth = 0.9) +
  scale_color_manual(values = pal(6), guide = "none") +
  facet_grid(cols = vars(scenario)) +
  scale_x_discrete(labels = c("Basal\nMetabolic rate", "Growth", "Reproduction")) + 
  labs(linetype = "Magnitude", y = "Relative median trait value", x = "Trait") +
  theme_classic() +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        strip.text.x = element_text(face="bold"),
        legend.key.height= unit(1.5, 'cm'),
        axis.text.x = element_text(lineheight = 0.3))
#ggsave("Figures/ScenOutPlots/TraitChanges.png", height = 3, width = 8)




EcoEvoOutsLstYrsSD <- EcoEvoOuts %>% 
  filter(year >= yr) %>% 
  ungroup() %>% 
  mutate(sdITVB = sdITVB / EcoEvoMedians$sdITVB,
         sdITVR = sdITVR  / EcoEvoMedians$sdITVR, 
         sdITVG = sdITVG / EcoEvoMedians$sdITVG) %>% 
  select(magnitude, scenario, sdITVB, sdITVR, sdITVG) %>% 
  pivot_longer(cols = c("sdITVB", "sdITVR", "sdITVG"), names_to = "output", values_to = "values") %>% 
  group_by(magnitude, scenario, output) %>% 
  summarise(stdev = sd(values),
            median = median(values),
            ci_up = quantile(values, probs = .75),
            ci_lo = quantile(values, probs = .25))



ggplot(subset(EcoEvoOutsLstYrsSD, scenario != "Control"), aes(x = output, linetype = as.factor(magnitude), col = interaction(output, magnitude))) +
  geom_hline(yintercept = 1, linewidth = 0.9, col = "grey30") +
  geom_pointrange(aes(y = median, ymin = ci_lo, ymax = ci_up), position = position_dodge(width = 0.5), size = 0.5, linewidth = 0.9) +
  scale_color_manual(values = pal(6), guide = "none") +
  facet_grid(cols = vars(scenario)) +
  scale_x_discrete(labels = c("Basal\nMetabolic rate", "Growth", "Reproduction")) + 
  labs(linetype = "Magnitude", y = "Relative trait SD", x = "Trait") +
  theme_classic() +
  theme(text = element_text(size = 15, color = "grey30", family = "Montserrat"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        strip.text.x = element_text(face="bold"),
        legend.key.height= unit(1.5, 'cm')) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        strip.text.x = element_text(face="bold"),
        legend.key.height= unit(1.5, 'cm'),
        axis.text.x = element_text(lineheight = 0.3))
#ggsave("Figures/ScenOutPlots/TraitSDChanges.png", height = 3, width = 8)

pal4 <- colorRampPalette(c("#5656A8","#E4AA4E","#3F3F7B","#CF8B20"))

EcoEvoOutsLstYrsBMAbun <- EcoEvoOuts %>% 
  filter(year >= yr) %>% 
  ungroup() %>% 
  select(-run.num, -version, -num, -year, -day, -date, -ITVB, -ITVR, -ITVG, -sdITVB, -sdITVR, -sdITVG) %>% 
  mutate(BM = BM / EcoEvoMedians$BM,
         abun = abun  / EcoEvoMedians$abun) %>%  
  pivot_longer(cols = c("BM", "abun"), names_to = "output", values_to = "values") %>% 
  group_by(magnitude, scenario, output) %>% 
  summarise(stdev = sd(values),
            median = median(values),
            ci_up = quantile(values, probs = .75),
            ci_lo = quantile(values, probs = .25))

ggplot(EcoEvoOutsLstYrsBMAbun, aes(x = output, linetype = as.factor(magnitude), col = interaction(output, magnitude))) +
  geom_hline(yintercept = 1, linewidth = 1, col = "grey30") +
  geom_pointrange(aes(y = median, ymin = ci_lo, ymax = ci_up), position = position_dodge(width = 0.5), size = 0.5, linewidth = 0.9) +
  scale_color_manual(values = pal4(4), guide = "none") +
  facet_grid(cols = vars(scenario)) +
  scale_x_discrete(labels = c("Abun","BM")) + 
  labs(linetype = "Magnitude", y = "Relative trait value", x = "Trait") +
  theme_classic() +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        strip.text.x = element_text(face="bold"),
        legend.key.height= unit(1.5, 'cm'),
        axis.text.x = element_text(lineheight = 0.3))


####

EcoEvoOutsLstYrsAbun <- EcoEvoOuts %>% 
  filter(year >= yr) %>% 
  ungroup() %>% 
  select(magnitude, scenario, day, date, abun) 

pal3 <- c("grey40", "#443983","#B46DB3", "#23898E")

ggplot(subset(EcoEvoOutsLstYrsAbun, magnitude == 0.3), aes(x = day, y = abun, col = scenario, fill = scenario)) +
  geom_smooth(method = "loess") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3) +
  labs(col = NULL, fill = NULL, y = "Population abundance [N]", x = "Day of year") +
  theme_classic() + 
  theme(text = element_text(size = 28, color = "grey30", family = "Montserrat"),
        legend.position = "bottom",
        legend.key.height= unit(0.5, 'cm'),
        legend.text = element_text(size = 20),
        legend.spacing.x = unit(0.2, 'cm')) +
  facet_grid(cols = vars(magnitude)) 
#ggsave("Figures/ScenOutPlots/PopAbunChanges.png", height = 4, width = 5)

ggplot(EcoEvoOutsLstYrsAbun, aes(x = date, y = abun, col = scenario, fill = scenario)) +
  geom_smooth(method = "loess", span = 0.9) +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3) +
  labs(col = "Scenario", fill = "Scenario", y = "Population abundance [N]", x = "Year") +
  theme_classic() +
  theme(text = element_text(size = 28, color = "grey30", family = "Montserrat"),
        legend.position = "bottom") +
  facet_grid(cols = vars(magnitude)) 

ggplot(EcoEvoOutsLstYrsAbun, aes(x = scenario, y = abun, col = scenario, fill = scenario)) +
  geom_boxplot(position = position_dodge(width = 0.5), alpha = 0.25) +
  scale_color_manual(values = pal3, guide = "none") +
  scale_fill_manual(values = pal3, guide = "none") +
  labs(col = "Scenario", fill = "Scenario", y = "Population abundance [N]", x = NULL) +
  theme_classic() +
  theme(text = element_text(size = 28, color = "grey30", family = "Montserrat"),
        legend.position = "bottom") +
  facet_grid(cols = vars(magnitude)) 


EcoEvoOutsLstYrsBM <- EcoEvoOuts %>% 
  filter(year >= yr) %>% 
  ungroup() %>% 
  select(magnitude, scenario, day, date, BM) 

ggplot(EcoEvoOutsLstYrsBM, aes(x = day, y = BM, col = scenario, fill = scenario)) +
  geom_smooth(method = "loess") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3) +
  labs(col = "Scenario", fill = "Scenario", y = "Body mass [g]", x = "Day of year") +
  theme_classic() +
  theme(text = element_text(size = 28, color = "grey30", family = "Montserrat"),
        legend.position = "bottom") +
  facet_grid(cols = vars(magnitude)) 
ggplot(EcoEvoOutsLstYrsBM, aes(x = date, y = BM, col = scenario, fill = scenario)) +
  geom_smooth(method = "loess") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3) +
  labs(col = "Scenario", fill = "Scenario", y = "Body mass [g]", x = "Year") +
  theme_classic() +
  theme(text = element_text(size = 28, color = "grey30", family = "Montserrat"),
        legend.position = "bottom") +
  facet_grid(cols = vars(magnitude)) 
#ggsave("Figures/ScenOutPlots/PopAbunChanges.png", height = 4, width = 7)


ggplot(subset(EcoEvoOutsLstYrsBM, magnitude == 0.3), aes(x = scenario, y = BM / EcoEvoMedians$BM, col = scenario, fill = scenario)) +
  geom_hline(yintercept = 1, linewidth = 1, col = "grey30") +
  geom_boxplot(width = 0.5, alpha = 0.25, linewidth = 1, notch = TRUE) +
  scale_color_manual(values = pal3, guide = "none") +
  scale_fill_manual(values = pal3, guide = "none") +
  labs(col = "Scenario", fill = "Scenario", y = "Body mass relative to control", x = NULL) +
  scale_x_discrete(breaks=unique(EcoEvoOutsLstYrsBM$scenario), 
                   labels=c("Control", 
                            "Positive\npulses",
                            "Negative\npulses",
                            "Positive and\nnegative pulses")) +
  theme_classic() +
  theme(text = element_text(size = 28, color = "grey30", family = "Montserrat"),
        legend.position = "bottom", 
        axis.text.x = element_text(lineheight = 0.3)) +
  facet_grid(cols = vars(magnitude)) 
#ggsave("Figures/ScenOutPlots/BMChanges.png", height = 4, width = 7)

