library(traitdata)
data("pantheria")
library(tidyverse)
library(showtext)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

pal <- colorRampPalette(c("#BC4A53","#E4714E","#E4AA4E","#AABC4A","#278192","#3F3F7B"))
textSizeAxisTitle <- 35

rodents <- pantheria %>% filter(Order == "Rodentia", AdultBodyMass_g <= 1000) %>% mutate(MperG = BasalMetRate_mLO2hr / AdultBodyMass_g)

BMRmsplot <- ggplot(rodents, aes(x = AdultBodyMass_g, y = MperG, col = Genus)) + 
  geom_point(size = 3.5) + 
  labs(x = "Adult body mass [g]", y = "Mass-specific basal metabolic rate [mL O2 hr-1 g-1]") +
  theme_classic() +
  scale_color_manual(values = pal(length(unique(rodents$Genus)))) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = "none") 
#ggsave("Figures/PanTHERIAPlots/BMRmsplot.png", BMRmsplot, width = 8, height = 6)

BMRplot <- ggplot(rodents, aes(x = AdultBodyMass_g, y = BasalMetRate_mLO2hr, col = Genus)) + 
  geom_point(size = 3.5) + 
  labs(x = "Adult body mass [g]", y = "Basal metabolic rate [mL O2 hr-1]") +
  theme_classic() +
  scale_color_manual(values = pal(length(unique(rodents$Genus)))) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = "none") 
#ggsave("Figures/PanTHERIAPlots/BMRplot.png", BMRplot, width = 8, height = 6)


SMrodents <- pantheria %>% filter(Order == "Rodentia", AdultBodyMass_g <= 100) %>% mutate(MperG = BasalMetRate_mLO2hr / AdultBodyMass_g)
BMRmsSmallplot <- ggplot(SMrodents, aes(x = AdultBodyMass_g, y = MperG, col = Genus)) + 
  geom_point(size = 3.5) + 
  labs(x = "Adult body mass [g]", y = "Mass-specific basal metabolic rate [mL O2 hr-1 g-1]") +
  theme_classic() +
  scale_color_manual(values = pal(length(unique(SMrodents$Genus)))) +
  theme(text = element_text(size = textSizeAxisTitle, color = "grey30", family = "Montserrat"),
        axis.line = element_line(color = "grey30"),
        legend.position = "none") 
#ggsave("Figures/PanTHERIAPlots/BMRmsSmallplot.png", BMRmsSmallplot, width = 8, height = 6)

ggplot(SMrodents, aes(x = AdultBodyMass_g, y = HuPopDen_Mean_n.km2, col = Genus)) + 
  geom_point() + 
  labs(x = "Adult body mass [g]", y = "Mass-specific basal metabolic rate [mL O2 hr-1 g-1]") +
  theme_classic() +
  theme(text = element_text(size = 20, color = "grey30"),
        axis.line = element_line(color = "grey30"),
        legend.position = "none") 

pantheriaVoles <- pantheria %>% filter(Species == "glareolus")

SMrodents <- pantheria %>% filter(Order == "Rodentia", AdultBodyMass_g <= 50) %>% mutate(MperG = BasalMetRate_mLO2hr / AdultBodyMass_g)

SMrodents %>% filter(MperG == max(SMrodents$MperG, na.rm = TRUE))
