# Cara Gallagher
# January 9th, 2024
# Energy Budget with Individual Variation project
# Fitting NDVI and GCM data for projection generation 

##################################################

library(tidyverse)
library(showtext)
library(beepr)

# load typeface and color palette
font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()
pal <- colorRampPalette(c("#3F3F7B","#278192","#00B089","#AABC4A", "#E5DC54","#E4AA4E","#E4714E","#BC4A53"))


#### import datasets #### 
NDVIdat <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/NDVIdata.csv")

GCMdat <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/GCMdataSPEI.csv")

#### prep for linking #### 
NDVIdat <- NDVIdat %>% 
  group_by(calendar_date, site) %>% 
  summarise(meanNDVI = mean(value)* 0.0001, sdNDVI = sd(value)* 0.0001) %>% 
  mutate(floor_date = floor_date(calendar_date, "month")) %>% 
  rename(VI_date = calendar_date) 


GCMdat <- GCMdat %>% 
  filter(date >= min(NDVIdat$VI_date) & date <= max(NDVIdat$VI_date)) %>% 
  dplyr::select(date, site, model, experiment, pr, tasmax, tasmin, rsds, spei, spi) %>% 
  mutate(floor_date = floor_date(date, "month")) %>% 
  rename(GCM_date = date)

# ggplot(GCMdat, aes(x = GCM_date, y = tasmin, col = site)) + 
#   geom_line(linewidth = 1.2) + 
#   facet_grid(rows = vars(site))

# cor(GCMdat$spei, GCMdat$spi)
# cor(GCMdat$pr, GCMdat$spei)
# cor(GCMdat$pr, GCMdat$spi)
# cor(GCMdat$tasmin, GCMdat$rsds)

#### combine datasets #### 

NDVI_GCM_dat <- left_join(GCMdat, NDVIdat, by = c("floor_date", "site"), relationship = "many-to-many")

#### visualize patterns #### 

NDVI_GCM_dat <- NDVI_GCM_dat %>% 
  ungroup() %>% 
  mutate(
    site = fct_relevel(
    site,
    "Pallasjarvi",
    "Stroemsund",
    "Konnovesi",
    "Evenstad",
    "Havelaue",
    "Bielowieza",
    "Frýdek-Místek",
    "Le Quartier",
    "Asturias",
    "Calabria"
  )
)

NDVI_GCM_dat <- NDVI_GCM_dat %>%  
  drop_na()
# NDVI
#speiVsNDVIPlot <- 
  ggplot(subset(NDVI_GCM_dat, month(GCM_date) >= 4 & month(GCM_date) <= 10), 
         aes(x = lag(tasmin, n = 0), y = meanNDVI, col = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(col = "black", linewidth = 1) +
  theme_classic() +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 10, color = "grey30", family = "Montserrat"),
        legend.position = "none") +
  facet_grid(rows = vars(site))
#ggsave("Figures/ScenInPlots/NDVIbyTempbySite.png", height = 9, width = 6)

ggplot(NDVI_GCM_dat, aes(x = pr, y = meanNDVI, col = site)) +
    geom_point(alpha = 0.5) +
    geom_smooth(col = "black", linewidth = 1)  +
    theme_classic() +
    scale_color_manual(values = pal(10)) +
    theme(text = element_text(size = 10, color = "grey30", family = "Montserrat"),
          legend.position = "none") 
  
#### checking for correlations #### 
# cor_mat_NDVI <- cor(NDVI_GCM_dat %>% dplyr::select(tasmin, rsds, pr, meanNDVI, sdNDVI) %>% drop_na())


#### fitting models ####
mean_tasmin <- mean(NDVI_GCM_dat$tasmin)
sd_tasmin <- sd(NDVI_GCM_dat$tasmin)
mean_pr <- mean(NDVI_GCM_dat$pr)
sd_pr <- sd(NDVI_GCM_dat$pr)
mean_rsds <- mean(NDVI_GCM_dat$rsds)
sd_rsds <- sd(NDVI_GCM_dat$rsds)

NDVI_GCM_dat_scaled <- NDVI_GCM_dat %>% 
  mutate(pr = (pr - mean_pr)/sd_pr,
         tasmin = (tasmin - mean_tasmin)/sd_tasmin,
         rsds = (rsds - mean_rsds)/sd_rsds,
         year = year(GCM_date))

library(lme4)
library(sjPlot)

# Original model -simple
modelSimple <- lmer(meanNDVI ~ pr + rsds + tasmin + (1 | site) + (1 | year), data = NDVI_GCM_dat_scaled)
# summary(modelSimple)
# AIC(modelSimple)
# tab_model(modelSimple)
# plot(residuals(modelSimple))

# Original model - complex
modelOrg <- lmer(meanNDVI ~ pr + pr^2 + rsds + tasmin + tasmin^2 + (pr | site) + (tasmin | site) + (1 | year), 
               data = NDVI_GCM_dat_scaled, 
               REML = TRUE, 
               control = lmerControl(optimizer = "bobyqa"))
# summary(modelOrg)
 AIC(modelOrg)
 tab_model(modelOrg)
# plot(residuals(modelOrg))



tNDVISimple <- NDVI_GCM_dat_scaled %>% 
  mutate(fit = predict(modelSimple, cur_data()))

p2Simple <- ggplot(tNDVISimple) +
  geom_line(aes(x = VI_date, y = meanNDVI), col = "grey5", linewidth = 1.2, alpha = 0.45) +
  geom_line(aes(x = GCM_date, y = fit, col = site), linewidth = 1) +
  theme_bw() +
  ylim(0,1) +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat")) +
  facet_grid(rows = vars(site), scales = "fixed") 

p1Simple <- ggplot(tNDVISimple) +
  geom_jitter(aes(x = as.factor(month(VI_date)), y = meanNDVI), col = "grey50", alpha = 0.45, width = 0.25) +
  geom_boxplot(aes(x = as.factor(month(GCM_date)), y = fit, col = site), linewidth = 1) +
  theme_bw() +
  ylim(0,1) +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat")) +
  facet_grid(rows = vars(site), scales = "fixed") 


tNDVISimple <- NDVI_GCM_dat_scaled %>% 
  mutate(fit = predict(modelSimple, cur_data()))

p2Simple <- ggplot(tNDVISimple) +
  geom_line(aes(x = VI_date, y = meanNDVI), col = "grey5", linewidth = 1.2, alpha = 0.45) +
  geom_line(aes(x = GCM_date, y = fit, col = site), linewidth = 1) +
  theme_bw() +
  ylim(0,1) +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat")) +
  facet_grid(rows = vars(site), scales = "fixed") 

p1Simple <- ggplot(tNDVISimple) +
  geom_jitter(aes(x = as.factor(month(VI_date)), y = meanNDVI), col = "grey50", alpha = 0.45, width = 0.25) +
  geom_boxplot(aes(x = as.factor(month(GCM_date)), y = fit, col = site), linewidth = 1) +
  theme_bw() +
  ylim(0,1) +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat")) +
  facet_grid(rows = vars(site), scales = "fixed") 


tNDVIOrg <- NDVI_GCM_dat_scaled %>% 
  mutate(fit = predict(modelOrg, cur_data()))

p2Org <- ggplot(tNDVIOrg) +
  geom_line(aes(x = VI_date, y = meanNDVI), col = "grey5", linewidth = 1.2, alpha = 0.45) +
  geom_line(aes(x = GCM_date, y = fit, col = site), linewidth = 1) +
  theme_bw() +
  labs(x = "Year", y = "Mean NDVI", col = "Site") +
  ylim(0,1) +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none") +
  facet_grid(rows = vars(site), scales = "fixed") 

#ggsave("Figures/ScenInPlots/PrevModel_NDVIyear.png", p2Org, height = 9, width = 7)

p1Org <- ggplot(tNDVIOrg) +
  geom_jitter(aes(x = as.factor(month(VI_date)), y = meanNDVI), col = "grey50", alpha = 0.45, width = 0.25) +
  geom_boxplot(aes(x = as.factor(month(GCM_date)), y = fit, col = site), linewidth = 1) +
  labs(x = "Month", y = "Mean NDVI", col = "Site") +
  theme_bw() +
  ylim(0,1) +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none") +
  facet_grid(rows = vars(site), scales = "fixed") 

#ggsave("Figures/ScenInPlots/PrevModel_NDVImonth.png", p1Org, height = 9, width = 7)


##################################################


#### generating projections ####

GCMdatFuture <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/GCMdata.csv")

GCMdatFuture <- GCMdatFuture %>% 
  filter(date >= max(NDVIdat$VI_date)) %>% 
  dplyr::select(date, site, model, experiment, pr, tasmin, rsds) %>% 
  mutate(floor_date = floor_date(date, "month")) %>% 
  rename(GCM_date = date) %>% 
  mutate(pr = (pr - mean_pr)/sd_pr,
         tasmin = (tasmin - mean_tasmin)/sd_tasmin,
         rsds = (rsds - mean_rsds)/sd_rsds,
      #   spei = (spei - mean_spei)/sd_spei,
         year = year(GCM_date)) # %>% 
  # group_by(site, model, experiment) %>% 
  # arrange(floor_date, .by_group = TRUE) %>% 
  # mutate(spei = ifelse(is.infinite(spei), NA, spei))
  
# GCMdatFuture <- GCMdatFuture %>% 
#   group_by(site, experiment, floor_date) %>% 
#   mutate(num_avg = mean(spei, na.rm=T))

# GCMdatFuture$spei[is.na(GCMdatFuture$spei)] <- GCMdatFuture$num_avg[is.na(GCMdatFuture$spei)]

GCMdatFuture <- GCMdatFuture %>% 
  group_by(site, model, experiment) %>% 
  arrange(floor_date, .by_group = TRUE) %>% 
  ungroup()

#predNDVISimple <- predict(modelSimple, newdata = GCMdatFuture, allow.new.levels = TRUE)
predNDVIOrg <- predict(modelOrg, newdata = GCMdatFuture, allow.new.levels = TRUE)
#predNDVISPEI <- predict(modelSPEI, newdata = GCMdatFuture, allow.new.levels = TRUE)

GCMdatFuture <- GCMdatFuture %>% 
  mutate(#predNDVISimple = predNDVISimple,
         predNDVIOrg = predNDVIOrg#,
         #predNDVISPEI = predNDVISPEI
         )

# results in a small number of values above one and below 0  
# clamped these values here, but should check  

GCMdatFuture <- GCMdatFuture %>% 
  mutate(#predNDVISimple = ifelse(predNDVISimple < 0, 0, ifelse(predNDVISimple > 1, 1, predNDVISimple)),
         predNDVIOrg = ifelse(predNDVIOrg < 0, 0, ifelse(predNDVIOrg > 1, 1, predNDVIOrg))#,
         #predNDVISPEI = ifelse(predNDVISPEI < 0, 0, ifelse(predNDVISPEI > 1, 1, predNDVISPEI))
         )
                                 
GCMdatFuture <- GCMdatFuture %>% 
  ungroup() %>% 
  mutate(
    site = fct_relevel(
      site,
      "Pallasjarvi",
      "Stroemsund",
      "Konnovesi",
      "Evenstad",
      "Havelaue",
      "Bielowieza",
      "Frýdek-Místek",
      "Le Quartier",
      "Asturias",
      "Calabria"
    )
  )
NDVIdat <- NDVIdat %>% 
  ungroup() %>% 
  mutate(
    site = fct_relevel(
      site,
      "Pallasjarvi",
      "Stroemsund",
      "Konnovesi",
      "Evenstad",
      "Havelaue",
      "Bielowieza",
      "Frýdek-Místek",
      "Le Quartier",
      "Asturias",
      "Calabria"
    )
  )

#### visualize projection results #### 

# ggplot(GCMdatFuture, aes(x = GCM_date, y = predNDVISPEI, col = experiment, linetype = model)) +
#   geom_line() +
#   geom_smooth(se = FALSE) +
#   facet_grid(rows = vars(site), scales = "fixed") +
#   theme_bw() +
#   scale_color_manual(values = c(pal(10)[1],pal(10)[10])) +
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
 
# ggplot(subset(GCMdatFuture, experiment == "ssp585"), aes(x = GCM_date, y = predNDVISPEI, linetype = model)) +
#   geom_line() +
#   geom_smooth(se = FALSE) +
#   facet_grid(rows = vars(site), scales = "fixed") +
#   theme_bw() +
#   scale_color_manual(values = c(pal(10)[1],pal(10)[10])) +
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))
 

ggplot(subset(GCMdatFuture, experiment == "ssp585"), aes(x = GCM_date, y = predNDVIOrg, col = site)) +
  geom_line(alpha = 0.5, linewidth = 0.4) +
  geom_smooth(se = FALSE, linewidth = 2) +
#  facet_grid(rows = vars(experiment), scales = "fixed") +
  theme_bw() +
  labs(x = "Year", y = "Projected NDVI") +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        strip.background = element_rect(fill="grey90"))
# ggsave("Figures/ScenInPlots/predNDVI.png", height = 3, width = 13.5)


# # seasonal trends


# p3JanSimple <- ggplot(subset(GCMdatFuture, month(GCM_date) == 1 & experiment == "ssp585"), aes(x = GCM_date, y = predNDVISimple, col = site)) +
#   geom_line(alpha = 0.5) +
#   geom_smooth(se = FALSE, linewidth = 1) +
#   facet_grid(rows = vars(experiment), scales = "fixed") +
#   theme_bw() +
#   ggtitle("January") +
#   scale_color_manual(values = pal(10)) +
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

p3JanOrg <- ggplot(subset(GCMdatFuture, month(GCM_date) == 1 & experiment == "ssp585"), aes(x = GCM_date, y = predNDVIOrg, col = site)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1) +
  facet_grid(rows = vars(experiment), scales = "fixed") +
  theme_bw() +
  labs(x = "Year", y = "Projected NDVI", col = "Site", title = "January") +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

# p3JanSPEI <- ggplot(subset(GCMdatFuture, month(GCM_date) == 1 & experiment == "ssp585"), aes(x = GCM_date, y = predNDVISPEI, col = site)) +
#   geom_line(alpha = 0.5) +
#   geom_smooth(se = FALSE, linewidth = 1) +
#   facet_grid(rows = vars(experiment), scales = "fixed") +
#   theme_bw() +
#   ggtitle("January") +
#   scale_color_manual(values = pal(10)) +
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))



# p4JulySimple <- ggplot(subset(GCMdatFuture, month(GCM_date) == 7 & experiment == "ssp585"), aes(x = GCM_date, y = predNDVISimple, col = site)) +
#   geom_line(alpha = 0.5) +
#   geom_smooth(se = FALSE, linewidth = 1) +
#   facet_grid(rows = vars(experiment), scales = "fixed") +
#   theme_bw() +
#   ggtitle("July") +
#   scale_color_manual(values = pal(10)) +
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

p4JulyOrg <- ggplot(subset(GCMdatFuture, month(GCM_date) == 7 & experiment == "ssp585"), aes(x = GCM_date, y = predNDVIOrg, col = site)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1) +
  facet_grid(rows = vars(experiment), scales = "fixed") +
  theme_bw() +
  labs(x = "Year", y = "Projected NDVI", col = "Site", title = "July") +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

# p4JulySPEI <- ggplot(subset(GCMdatFuture, month(GCM_date) == 7 & experiment == "ssp585"), aes(x = GCM_date, y = predNDVISPEI, col = site)) +
#   geom_line(alpha = 0.5) +
#   geom_smooth(se = FALSE, linewidth = 1) +
#   facet_grid(rows = vars(experiment), scales = "fixed") +
#   theme_bw() +
#   ggtitle("July") +
#   scale_color_manual(values = pal(10)) +
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


p3JanOrg / p4JulyOrg + plot_layout(guides = "collect")

#ggsave("Figures/ScenInPlots/PrevModel_JanJuly.png", height = 8, width = 6)

#### Generating scenario files ####

CRNM_245 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "CNRM-CM6-1-HR",
         experiment = "ssp245") %>% 
  rename(NDVI = meanNDVI) %>% 
  arrange(site, floor_date) %>% 
  group_by(site, model, experiment, floor_date) %>% 
  summarise(NDVI = mean(NDVI))

CRNM_585 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "CNRM-CM6-1-HR",
         experiment = "ssp585") %>% 
  rename(NDVI = meanNDVI) %>% 
  arrange(site, floor_date) %>% 
  group_by(site, model, experiment, floor_date) %>% 
  summarise(NDVI = mean(NDVI))

Earth3_245 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "EC-Earth3-CC",
         experiment = "ssp245") %>% 
  rename(NDVI = meanNDVI) %>% 
  arrange(site, floor_date) %>% 
  group_by(site, model, experiment, floor_date) %>% 
  summarise(NDVI = mean(NDVI))

Earth3_585 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "EC-Earth3-CC",
         experiment = "ssp585") %>%
  rename(NDVI = meanNDVI) %>% 
  arrange(site, floor_date) %>% 
  group_by(site, model, experiment, floor_date) %>% 
  summarise(NDVI = mean(NDVI))

AWI_245 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "AWI-CM-1-1-MR",
         experiment = "ssp245") %>% 
  rename(NDVI = meanNDVI) %>% 
  arrange(site, floor_date) %>% 
  group_by(site, model, experiment, floor_date) %>% 
  summarise(NDVI = mean(NDVI))

AWI_585 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "AWI-CM-1-1-MR",
         experiment = "ssp585") %>% 
  rename(NDVI = meanNDVI) %>% 
  arrange(site, floor_date) %>% 
  group_by(site, model, experiment, floor_date) %>% 
  summarise(NDVI = mean(NDVI))


Proj <- GCMdatFuture %>%  
  dplyr::select(floor_date, site, model, experiment, predNDVIOrg) %>% 
  rename(NDVI = predNDVIOrg)

yrStart <- 4
yrEnd <- 10

FullProjs <- bind_rows(
  CRNM_245,
  CRNM_585,
  Earth3_245,
  Earth3_585,
  AWI_245,
  AWI_585,
  Proj)


FullProjs <- FullProjs %>% 
  mutate(NDVI = ifelse(NDVI < 0, 0, 
                       ifelse(NDVI > 1, 1, NDVI))) %>% 
  filter(month(floor_date) >= yrStart & month(floor_date) <= yrEnd) %>% 
  mutate(NDVI = NDVI - 0.5) %>% 
  group_by(site, model, experiment) %>% 
  arrange(floor_date, .by_group = TRUE) 

ggplot(FullProjs, aes(x = floor_date, y = NDVI, col = experiment, linetype = model)) +
  geom_line(linewidth = 0.2, alpha = 0.75) +
  geom_smooth(se = FALSE, linewidth = 0.7) +
  facet_grid(rows = vars(site), scales = "fixed") +
  theme_bw() +
  labs(x = "Year", linetype = "Model", col = "SSP scenario") +
  scale_color_manual(values = c(pal(20)[5],pal(20)[19])) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "bottom", legend.spacing.y = unit(0.1, 'cm')) +
  guides(col=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=3,byrow=TRUE))

#ggsave("Figures/ScenInPlots/PrevModel_FullScenarios.png", height = 9, width = 7)


year2023_2099 <- FullProjs %>% 
  filter(year(floor_date) == 2000 | year(floor_date) == 2070 | year(floor_date) == 2099) %>% 
  filter(experiment == "ssp585") %>% 
  mutate(NDVI = NDVI + 0.5,
         year = year(floor_date))# %>% 
  #select(-days_mo)

ggplot(year2023_2099, aes(x = floor_date, y = NDVI, col = site)) + 
  geom_smooth(se=FALSE) +
  facet_grid(cols = vars(year), scales = "free_x")

#write_csv(year20FALSE#write_csv(year2023_2099, "Data/Scenarios/VI GCM scenarios/Inputs/NDVI_hist_proj.csv")

# ggplot(FullProjs, aes(x = floor_date, y = deltaNDVI, col = experiment, linetype = model)) +
#   geom_line() +
#   geom_smooth(se = FALSE) +
#   facet_grid(rows = vars(site), scales = "fixed") +
#   theme_bw() +
#   scale_color_manual(values = c(pal(10)[1],pal(10)[10])) +
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


# Interpolate NDVI to daily values 
FullProjs <- FullProjs %>% 
  mutate(days_mo = days_in_month(floor_date))  %>% 
  mutate(days_mo = ifelse(month(floor_date) == 2, 28, days_mo))

FullProjsDaily <- tibble(
  site = as.character(), 
  model = as.character(), 
  experiment = as.character(), 
  day = as.numeric(),
  year = as.numeric(),
#  deltaNDVI = as.numeric(),
  NDVI = as.numeric()
)
  

for (i in 1:(nrow(FullProjs) - 1)) {
 # i <- 5 
  
  year <- year(FullProjs$floor_date[i])
  startDate <- FullProjs$floor_date[i]
  endDate <- FullProjs$floor_date[i + 1]
  
  year(startDate) <- 2001 # setting to 2001 to avoid leap year troubles 
  year(endDate) <- 2001 # setting to 2001 to avoid leap year troubles 

  month <- month(startDate)
  startDay <- yday(startDate)
  endDay <- yday(endDate) - 1
  startNDVI <- FullProjs$NDVI[i]
  endNDVI <- FullProjs$NDVI[i + 1]
  
  siteSub <- FullProjs$site[i]
  modelSub <- FullProjs$model[i]
  experimentSub <- FullProjs$experiment[i]
  
  if (endDay > startDay) {
    
    days <- seq(startDay, endDay - 1, 1)
    
    sub <- tibble(
      site = siteSub, 
      model = modelSub, 
      experiment =experimentSub, 
      date = NA,
      day = days,
      year = year,
      NDVI = NA
    )
    
    sub <- sub %>% 
      mutate(NDVI = (startNDVI + ((endNDVI - startNDVI) * ((day - startDay)/(endDay - startDay))))) %>% 
      mutate(date = as.Date(paste(year, month, (1 + day - startDay), sep ="-")))

    FullProjsDaily <- bind_rows(FullProjsDaily, sub)
    
  }
}

beep() 

FullProjsDaily %>%
  dplyr::group_by(site, model, experiment, year, day) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

# Setting first day of year to flat NDVI value rather than change to initiate year with this value
FullProjsDaily <- FullProjsDaily  %>% 
  group_by(site, model, experiment) %>% 
  arrange(date, .by_group = TRUE) %>%
  mutate(deltaNDVI = 2 * (NDVI - lag(NDVI))) %>% 
  mutate(deltaNDVI = ifelse(day == min(FullProjsDaily$day), 2 * (NDVI + 0.5), deltaNDVI)) %>% 
  dplyr::select(-NDVI)

ggplot(FullProjsDaily, aes(x = date, y = deltaNDVI, col = experiment, linetype = model)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  facet_grid(rows = vars(site), scales = "fixed") +
  theme_bw() +
  scale_color_manual(values = c(pal(10)[1],pal(10)[10])) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

ggplot(FullProjsDaily, aes(x = date, y = NDVI, col = experiment, linetype = model)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  facet_grid(rows = vars(site), scales = "fixed") +
  theme_bw() +
  scale_color_manual(values = c(pal(10)[1],pal(10)[10])) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


ggplot(subset(FullProjsDaily, site == "Calabria" & year(date) <= 2010), aes(x = date, y = deltaNDVI, col = experiment, linetype = model)) +
  geom_line() +
  ylim(-0.1, 0.1) +
  facet_grid(rows = vars(site), scales = "fixed") +
  theme_bw() +
  scale_color_manual(values = c(pal(10)[1],pal(10)[10])) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


#### Write output files #### 

sites <- unique(FullProjsDaily$site)
models <- unique(FullProjsDaily$model)
experiments <- unique(FullProjsDaily$experiment)

filename <- "Data/Scenarios/VI GCM scenarios/Inputs/NDVIInputs/NDVIInputScenarios" 

for (i in sites) {
 # i <- sites[1]
  
  sub <- FullProjsDaily %>% 
    filter(site == i)
  
  for (ii in experiments) {
  #  ii <- experiments[1]
    
    sub_sub <- sub %>% 
      filter(experiment == ii) %>% 
      arrange(date) %>% 
      mutate(year = year(date) - 1999) %>% 
      ungroup() %>% 
      dplyr::select(year, day, model, deltaNDVI) %>% 
      pivot_wider(names_from = model, values_from = deltaNDVI) 
      
      
      write_csv(
        sub_sub,
        paste(
          filename,
          paste(
            "",
            as.character(i),
            as.character(ii),
            sep = "_"),
          ".csv",
          sep = ""
        )
      )
  }
}
      
      
      



