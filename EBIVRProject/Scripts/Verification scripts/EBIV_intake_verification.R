# Cara Gallagher
# February 28th, 2022
# Energy Budget with Individual Variation project
# Energy intake output verification

##################################################
# Packages:
library(tidyverse)
library(patchwork)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

##################################################


## maximum total energy intake
# 3.26 +- 0.23 kJ g-1 day-1 - maximum total energy consumption in Piątkowska & Weiner 1987 (19.6 g) -  Acta Theriologica
# 81.55 ± 6.19 kJ day-1 - Daily ingested energy for non-growing, non-reproducing voles (25 g) in Kaczmarski 1966  
# 97.06 ± 14.23, 100.18 ± 9.79, 111.90 ± 15.70  kJ day-1 - Daily food ingested for pregnant voles (25 g) in Kaczmarski 1966
# 124.29 ± 20.42, 170.01 ± 36.21, 184.14 ± 42.17  kJ day-1 - Daily food ingested for lactating voles (25 g) in Kaczmarski 1966
# 66.78 kJ day-1 - normal ingestion in Meese 1971
# 58.49 kJ day-1 - normal ingestion in Meese 1971
# 105.6 ± 2.64 kJ day-1 (6.0±0.15 g day-1 & 17.6 kJ g-1 for food) - plateau for females in Peacock et al. 2004
# 50.5 ± 1.06 kJ day-1 & 49.0 ± 1.91 kJ day-1 - normal ingestion for all animals (15.45g) in Peacock and Speakman 2001

# 90.07 ± 3.56 kJ day-1 - for unmated females in Sadowka et al. 2016
# 270.56 ± 5.34 kJ day-1 - for lactating females on day 15 in Sadowka et al.  2016

empDat <- tibble(
  mean = c(3.26 * 19.6, 81.55, 97.06, 100.18, 111.90, 124.29, 170.01, 184.14, 66.78, 58.49, 105.6, 50.5, 49.0, 90.07, 270.56),
  error = c(0.23 * 19.6, 6.19, 14.23, 9.79, 15.70, 20.42, 36.21, 42.17, NA, NA, 2.64, 1.06, 1.91, 3.56, 5.34),
  source = c("Piątkowska & Weiner 1987", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Kaczmarski 1966", "Meese 1971", "Meese 1971", "Peacock et al. 2004", "Peacock and Speakman 2001", "Peacock and Speakman 2001", "Sadowska et al. 2016", "Sadowska et al. 2016"),
  state = c("Nonreproducing", "Nonreproducing", "Pregnant", "Pregnant", "Pregnant", "Lactating", "Lactating", "Lactating", "Nonreproducing", "Nonreproducing", "Nonreproducing", "Juvenile", "Juvenile", "Nonreproducing", "Lactating")
)

empDat <- empDat %>% 
  mutate(state = fct_relevel(state, "Juvenile", "Nonreproducing", "Pregnant", "Lactating"))

pal <- lacroix_palette("PeachPear", type = "discrete")
pal <-c(pal[1],pal[3],pal[4],pal[5])

empPlot <- ggplot(empDat, aes(x = state, y = mean, group = mean, shape = source, col = state)) +
  geom_linerange(aes(ymin = mean - error, ymax = mean + error), position = position_dodge(width = 1), size = 1) +
  geom_point(position = position_dodge(width = 1), size = 2) +
  scale_color_manual(values = pal, guide = "none") +
  ylim(0,550) +
  theme_classic() +
  labs( x = NULL, y = "Daily food consumption [kJ day-1]", shape = NULL) +
  theme(legend.position = c(0.3, 0.8))
empPlot


# model outputs 

modOut <- read_csv("Data/Verification/Energy intake/EBIVPrototype_IngestionRates.csv", skip = 20)

modOut <- modOut %>% 
  select(y...2, y...6, y...10, y...14, y...18) %>% 
  rename("Nonreproducing" = y...2, "Lactating and pregnant" = y...6, "Lactating" = y...10, "Pregnant" = y...14, "Juvenile" = y...18) %>% 
  pivot_longer(everything(), names_to = "state", values_to = "foodCons")  %>% 
  mutate(state = fct_relevel(state, "Juvenile", "Nonreproducing", "Pregnant", "Lactating", "Lactating and pregnant"),
         foodCons = foodCons * 12.2811)

pal <- lacroix_palette("PeachPear", type = "discrete")
pal <-c(pal[1],pal[3],pal[4],pal[5],pal[6])

intakeVerPlot <- 
ggplot() + 
  geom_violin(data = modOut, aes(x = state, y = foodCons, fill = state), col = NA, alpha = 0.25) +
  geom_boxplot(data = modOut, aes(x = state, y = foodCons, col = state, fill = state), fill = "white", width = 0.1, alpha = 0.5) +
  geom_linerange(data = empDat, aes(x = state, y = mean, group = mean, col = state, ymin = mean - error, ymax = mean + error), position = position_dodge(width = 0.75), size = 1) +
  geom_point(data = empDat, aes(x = state, y = mean, group = mean, shape = source, col = state), position = position_dodge(width = 0.75), size = 2) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  theme_classic() +
  labs( x = NULL, y = "Daily food consumption [kJ day-1]", shape = NULL) +
  theme(legend.position = c(0.4, 0.975),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA))
intakeVerPlot


#### Ingestion rate & food debt check ####

IRFDOut <- read_csv("Data/Verification/Energy intake/IngestionFoodDebt.csv", skip = 17)

IRFD1 <- IRFDOut %>% 
  select(x...1, y...2) %>% 
  rename(timestep = x...1, value = y...2) %>% 
  mutate(metric = "Ingestion rate")

IRFD2 <-IRFDOut %>% 
  select(x...5, y...6) %>% 
  rename(timestep = x...5, value = y...6) %>% 
  mutate(metric = "Food debt")

textSizeAxisTitle <- 35

pal <- c("#8484C0", "#39396F")

t <- 500
text <- 48 * 7

IRFDOut <- bind_rows(IRFD1, IRFD2)

IRFDOut <- IRFDOut %>% 
  filter(timestep > t & timestep < t + text)

ggplot(IRFDOut, aes(x = timestep, y = value, fill = metric)) + 
  geom_area(col = NA) +
  labs(x = "Timestep [30 min]", y = "Food resources [g]", fill = NULL) +
  theme_classic() +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = seq(t, t + text, 100), labels = seq(0, text, 100)) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = c(0.15,0.15)) 
ggsave("Figures/Verification/IRFDplot.png", height = 4, width = 7)







