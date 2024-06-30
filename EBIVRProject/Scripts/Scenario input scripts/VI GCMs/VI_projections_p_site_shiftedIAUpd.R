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

sites <- c(
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

GCMdatFutureAllOuts <- tibble(GCM_date = as.Date(character()), 
                              site = as.character(), 
                              model = as.character(), 
                              experiment = as.character(), 
                              floor_date = as.Date(character()), 
                              year = as.numeric(), 
                              predNDVI = as.numeric())

NDVIAllOuts <- tibble(site = as.character(), 
                      VI_date = as.Date(character()),
                      meanNDVI = as.numeric(),
                      GCM_date = as.Date(character()), 
                      fit = as.numeric(),
                      AIC = as.numeric())


siteNo <- 1

for (siteNo in 1:10) {
  
  print(siteNo)
  
  #### import datasets #### 
  NDVIdat <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/NDVIdata.csv")
  
  GCMdat <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/GCMdata.csv")
  
  #### prep for linking #### 
  NDVIdat <- NDVIdat %>% 
    group_by(calendar_date, site) %>% 
    summarise(meanNDVI = mean(value)* 0.0001, sdNDVI = sd(value)* 0.0001) %>% 
    mutate(floor_date = floor_date(calendar_date, "month")) %>% 
    rename(VI_date = calendar_date) 
  
  ggplot(subset(NDVIdat, yday(VI_date) >= 80 & yday(VI_date) <= 300) , aes(x = VI_date, y = meanNDVI, col = site)) +
    #geom_line(linewidth = 1.2) +
    #geom_smooth() +
    geom_point() +
    facet_grid(rows = vars(site))
  
  
  GCMdat <- GCMdat %>% 
    filter(date >= min(NDVIdat$VI_date) & date <= max(NDVIdat$VI_date)) %>% 
    dplyr::select(date, site, model, experiment, pr, tasmin) %>% 
    mutate(floor_date = floor_date(date, "month")) %>% 
    rename(GCM_date = date)  
  
  ggplot(GCMdat, aes(x = GCM_date, y = pr, col = site)) +
    #geom_line(linewidth = 1.2) +
    geom_smooth() +
    facet_grid(rows = vars(model), col = vars(experiment))
  
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
    drop_na() %>% 
    filter(site == sites[siteNo]) #%>% 
  # filter(month(GCM_date) >= 4 & month(GCM_date) <= 10)
  
  # # NDVI
  #speiVsNDVIPlot <-
  ggplot(NDVI_GCM_dat, aes(x = tasmin, y = meanNDVI, col = site)) +
    geom_point(alpha = 0.5) +
    geom_smooth(col = "black", linewidth = 1) +
    theme_classic() +
    scale_color_manual(values = pal(10)) +
    theme(text = element_text(size = 10, color = "grey30", family = "Montserrat"),
          legend.position = "none") +
    facet_grid(rows = vars(site))
  # #ggsave("Figures/ScenInPlots/NDVIbyTempbySite.png", height = 9, width = 6)
  # 
  # ggplot(NDVI_GCM_dat, aes(x = prLag12, y = meanNDVI, col = site)) +
  #     geom_point(alpha = 0.5) +
  #     geom_smooth(col = "black", linewidth = 1)  +
  #     theme_classic() +
  #     scale_color_manual(values = pal(10)) +
  #     theme(text = element_text(size = 10, color = "grey30", family = "Montserrat"),
  #           legend.position = "none") 
  #   
  #### checking for correlations #### 
  # cor_mat_NDVI <- cor(NDVI_GCM_dat %>% dplyr::select(tasmin, pr, meanNDVI, sdNDVI) %>% drop_na())
  
  #### fitting models ####
  mean_tasmin <- mean(NDVI_GCM_dat$tasmin)
  sd_tasmin <- sd(NDVI_GCM_dat$tasmin)
  mean_pr <- mean(NDVI_GCM_dat$pr)
  sd_pr <- sd(NDVI_GCM_dat$pr)
  
  
  NDVI_GCM_dat_scaled <- NDVI_GCM_dat %>% 
    mutate(pr = (pr - mean_pr)/sd_pr,
           tasmin = (tasmin - mean_tasmin)/sd_tasmin,
           year = year(GCM_date))
  
  
  # Site specific model
  mod <- lmer(meanNDVI ~ pr + pr^2 + tasmin + tasmin^2 + (1 | year), 
              data = NDVI_GCM_dat_scaled, 
              REML = TRUE, 
              control = lmerControl(optimizer = "bobyqa"))
  # summary()
  AIC(mod)
  tab_model(mod)
  # plot(residuals(mod))
  
  tNDVI <- NDVI_GCM_dat_scaled %>% 
    mutate(fit = predict(mod, cur_data()))
  
  t_NDVIOuts <- tibble(site = tNDVI$site, 
                       VI_date = tNDVI$VI_date,
                       meanNDVI = tNDVI$meanNDVI,
                       GCM_date = tNDVI$GCM_date, 
                       fit = tNDVI$fit,
                       AIC = AIC(mod))
  
  NDVIAllOuts <- bind_rows(NDVIAllOuts, t_NDVIOuts)
  
  ##################################################
  
  
  #### generating projections ####
  
  GCMdatFuture <- read_csv("Data/Scenarios/VI GCM scenarios/Inputs/GCMdata.csv")
  
  GCMdatFuture <- GCMdatFuture %>% 
    filter(date >= max(NDVIdat$VI_date)) %>% 
    filter(site == sites[siteNo]) %>% 
    select(date, site, model, experiment, pr, tasmin) %>% 
    mutate(floor_date = floor_date(date, "month")) %>% 
    rename(GCM_date = date) %>% 
    group_by(site, model, experiment) %>% 
    mutate(pr = (pr - mean_pr)/sd_pr,
           tasmin = (tasmin - mean_tasmin)/sd_tasmin,
           year = year(GCM_date)) %>% 
    group_by(site, model, experiment) %>% 
    arrange(floor_date, .by_group = TRUE) %>% 
    ungroup() %>% 
    drop_na()
  
  predNDVI <- predict(mod, newdata = GCMdatFuture, allow.new.levels = TRUE)
  
  GCMdatFuture <- GCMdatFuture %>% 
    mutate(predNDVI = predNDVI)
  
  # results in a small number of values above one and below 0  
  # clamped these values here, but should check  
  
  GCMdatFuture <- GCMdatFuture %>% 
    mutate(predNDVI = ifelse(predNDVI < 0, 0, ifelse(predNDVI > 1, 1, predNDVI)))
  
  
  # combine outputs across sites 
  
  sub <- GCMdatFuture %>% 
    select(GCM_date, site, model, experiment, floor_date, year, predNDVI)
  
  GCMdatFutureAllOuts <- bind_rows(GCMdatFutureAllOuts, sub)
  
}

GCMdatFuture <- GCMdatFutureAllOuts                    


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

#### model outputs for historic data ####
NDVIAllOuts <- NDVIAllOuts %>% 
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

p1 <- ggplot(NDVIAllOuts) +
  geom_jitter(aes(x = as.factor(month(VI_date)), y = meanNDVI), col = "grey50", alpha = 0.45, width = 0.25) +
  geom_boxplot(aes(x = as.factor(month(GCM_date)), y = fit, col = site), linewidth = 1) +
  labs(x = "Month", y = "Mean NDVI", col = "Site") +
  theme_bw() +
  ylim(0,1) +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none") +
  facet_grid(rows = vars(site), scales = "fixed") 
p1
#ggsave("Figures/ScenInPlots/FinalModel_NDVImonth.png", p1, height = 9, width = 7)


p2 <- ggplot(NDVIAllOuts) +
  geom_smooth(aes(x = VI_date, y = meanNDVI), col = "grey5", linewidth = 1.2, alpha = 0.45) +
  geom_smooth(aes(x = GCM_date, y = fit, col = site), linewidth = 1) +
  #geom_line(aes(x = VI_date, y = meanNDVI), col = "grey5", linewidth = 1.2, alpha = 0.45) +
  #geom_line(aes(x = GCM_date, y = fit, col = site), linewidth = 1) +
  theme_bw() +
  labs(x = "Year", y = "Mean NDVI", col = "Site") +
  ylim(0,1) +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"), 
        legend.position = "none") +
  facet_grid(rows = vars(site), scales = "fixed") 
p2
#ggsave("Figures/ScenInPlots/FinalModel_NDVIyear.png", p2, height = 9, width = 7)


#### visualize projection results #### 

ggplot(subset(GCMdatFuture, experiment == "ssp585"), aes(x = GCM_date, y = predNDVI, col = site)) +
  # ggplot(subset(GCMdatFuture, experiment == "ssp585" & month(floor_date) >= 4 & month(floor_date) <= 10), aes(x = GCM_date, y = predNDVI, col = site)) +
  geom_line(alpha = 0.5, linewidth = 0.4) +
  geom_smooth(se = FALSE, linewidth = 2) +
  #  facet_grid(rows = vars(model), cols = vars(experiment), scales = "fixed") +
  theme_bw() +
  labs(x = "Year", y = "Projected NDVI") +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 50, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        strip.background = element_rect(fill="grey90"))
# ggsave("Figures/ScenInPlots/predNDVI.png", height = 3, width = 13.5)


# # seasonal trends

p3Jan <- ggplot(subset(GCMdatFuture, month(GCM_date) == 1 & experiment == "ssp585"), aes(x = GCM_date, y = predNDVI, col = site)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1) +
  facet_grid(rows = vars(experiment), scales = "fixed") +
  theme_bw() +
  labs(x = "Year", y = "Projected NDVI", col = "Site", title = "January") +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


p4July <- ggplot(subset(GCMdatFuture, month(GCM_date) == 7 & experiment == "ssp585"), aes(x = GCM_date, y = predNDVI, col = site)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1) +
  facet_grid(rows = vars(experiment), scales = "fixed") +
  theme_bw() +
  labs(x = "Year", y = "Projected NDVI", col = "Site", title = "July") +
  scale_color_manual(values = pal(10)) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))

p3Jan / p4July + plot_layout(guides = "collect")

#ggsave("Figures/ScenInPlots/FinalModel_JanJuly.png", height = 8, width = 6)



#### Generating scenario files ####

CRNM_245 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "CNRM-CM6-1-HR",
         experiment = "ssp245") %>% 
  rename(NDVI = meanNDVI, date = floor_date) %>% 
  arrange(site, date) %>% 
  group_by(site, model, experiment, date) %>% 
  summarise(NDVI = mean(NDVI))

CRNM_585 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "CNRM-CM6-1-HR",
         experiment = "ssp585") %>% 
  rename(NDVI = meanNDVI, date = floor_date) %>% 
  arrange(site, date) %>%  
  group_by(site, model, experiment, date) %>% 
  summarise(NDVI = mean(NDVI))

Earth3_245 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "EC-Earth3-CC",
         experiment = "ssp245") %>% 
  rename(NDVI = meanNDVI, date = floor_date) %>% 
  arrange(site, date) %>% 
  group_by(site, model, experiment, date) %>% 
  summarise(NDVI = mean(NDVI))

Earth3_585 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "EC-Earth3-CC",
         experiment = "ssp585") %>%
  rename(NDVI = meanNDVI, date = floor_date) %>% 
  arrange(site, date) %>% 
  group_by(site, model, experiment, date) %>% 
  summarise(NDVI = mean(NDVI))

AWI_245 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "AWI-CM-1-1-MR",
         experiment = "ssp245") %>% 
  rename(NDVI = meanNDVI, date = floor_date) %>% 
  arrange(site, date) %>% 
  group_by(site, model, experiment, date) %>% 
  summarise(NDVI = mean(NDVI))

AWI_585 <- NDVIdat %>%  
  dplyr::select(floor_date, site, meanNDVI) %>% 
  mutate(model = "AWI-CM-1-1-MR",
         experiment = "ssp585") %>% 
  rename(NDVI = meanNDVI, date = floor_date) %>% 
  arrange(site, date) %>% 
  group_by(site, model, experiment, date) %>% 
  summarise(NDVI = mean(NDVI))


Proj <- GCMdatFuture %>%  
  dplyr::select(floor_date, site, model, experiment, predNDVI) %>% 
  rename(NDVI = predNDVI, date = floor_date)

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
  filter(month(date) >= yrStart & month(date) <= yrEnd) %>% 
  #  mutate(NDVI = NDVI - 0.5) %>% 
  # mutate(NDVI = NDVI * 2) %>% 
  group_by(site, model, experiment) %>% 
  arrange(date, .by_group = TRUE) 

ggplot(FullProjs, aes(x = date, y = NDVI, col = experiment, linetype = model)) +
   geom_line(linewidth = 0.2, alpha = 0.75) +
  #geom_smooth(se = FALSE, linewidth = 0.7) +
  facet_grid(rows = vars(site), scales = "fixed") +
  theme_bw() +
  ylim(0,1) +
  labs(x = "Year", y = "NDVI", linetype = "Model", col = "SSP scenario") +
  scale_color_manual(values = c(pal(20)[5],pal(20)[19])) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "bottom", legend.spacing.y = unit(0.1, 'cm')) +
  guides(col=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=3,byrow=TRUE))

#ggsave("Figures/ScenInPlots/FinalModel_FullScenarios.png", height = 9, width = 7)

NDVIdatMo <- NDVIdat %>% 
  group_by(site, floor_date) %>% 
  filter(month(floor_date) >= yrStart & month(floor_date) <= yrEnd) %>% 
  summarise(meanNDVI = mean(meanNDVI))

# Interpolate proj to daily values  

FullProjsDaily <- tibble(
  site = as.character(), 
  date = as.Date(as.character()),
  day = as.numeric(),
  NDVI = as.numeric(),
  model = as.character(),
  experiment = as.character()
)

FullProjs <- FullProjs %>% 
  group_by(date, site, model, experiment) %>% 
  summarise(NDVI = mean(NDVI)) %>% 
  group_by(site, model, experiment) %>% 
  arrange(date, .by_group = TRUE)


for (i in 1:(nrow(FullProjs) - 1)) {
  # i <- 1 
  
  year <- year(FullProjs$date[i])
  startDate <- FullProjs$date[i]
  endDate <- FullProjs$date[i + 1]
  
  year(startDate) <- 2001 # setting to 2001 to avoid leap year troubles 
  year(endDate) <- 2001 # setting to 2001 to avoid leap year troubles 
  
  month <- month(startDate)
  startDay <- yday(startDate)
  endDay <- yday(endDate)
  startNDVI <- FullProjs$NDVI[i]
  endNDVI <- FullProjs$NDVI[i + 1]
  
  year(startDate) <- year # resetting to base year
  year(endDate) <- year 
  
  if (endDay > startDay) {
    
    days <- seq(startDay, endDay  - 1, 1)
    
    sub <- tibble(
      site = FullProjs$site[i], 
      date = seq.Date(startDate, endDate - 1, by = "days"),
      day = days,
      NDVI = NA,
      model = FullProjs$model[i],
      experiment = FullProjs$experiment[i]
    )
    
    sub <- sub %>% 
      mutate(NDVI = (startNDVI + ((endNDVI - startNDVI) * ((day - startDay)/(endDay - startDay)))))
    
    FullProjsDaily <- bind_rows(FullProjsDaily, sub)
    
  }
}

beep() 

FullProjsDaily <- FullProjsDaily %>% 
  filter(day > 90 & day <= 277)
  

# ggplot(subset(FullProjsDaily, day > 90 & day <= 277), aes(x = date, y = NDVI)) +
#   geom_point() +
#   geom_smooth(se = FALSE) +
#   facet_grid(rows = vars(site), scales = "fixed") +
#   theme_bw() +
#   scale_color_manual(values = c(pal(10)[1],pal(10)[10])) +
#   theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"))


### Determine average yearly values 

FullProjsYr <- FullProjsDaily %>%
  mutate(year = year(date)) %>% 
  group_by(site, experiment, year) %>% 
  summarise(meanNDVI = mean(NDVI))

ggplot(FullProjsYr, aes(x = year, y = meanNDVI, col = experiment)) +
  geom_line(linewidth = 0.2, alpha = 0.75) +
  geom_smooth(se = FALSE, linewidth = 0.7) +
  facet_grid(rows = vars(site), scales = "fixed") +
  theme_bw() +
  ylim(0,1) +
  labs(x = "Year", y = "NDVI", linetype = "Model", col = "SSP scenario") +
  scale_color_manual(values = c(pal(20)[5],pal(20)[19])) +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "bottom", legend.spacing.y = unit(0.1, 'cm')) +
  guides(col=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=3,byrow=TRUE))

ggplot(subset(FullProjsYr, experiment == "ssp585"), aes(x = year, y = meanNDVI, col = site)) +
  geom_line(linewidth = 0.2, alpha = 0.75) +
  geom_smooth(se = FALSE, linewidth = 0.7) +
  theme_bw() +
  scale_color_manual(values = pal(10)) +
  labs(x = "Year", y = "NDVI", linetype = "Model", col = "Site") +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "bottom", legend.spacing.y = unit(0.1, 'cm')) +
  guides(col=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=3,byrow=TRUE))


### Pull intraannual values from initial years 

centNDVI <- FullProjsDaily %>% 
  filter(year(date) <= max(year(NDVIdat$floor_date)) )


# Setting first day of year to flat NDVI value rather than change to initiate year with this value
centNDVIDaily <- centNDVI  %>% 
  select(-model, -experiment) %>% 
  distinct() %>% 
  mutate(year = year(date)) %>% 
  group_by(site) %>% 
  left_join(FullProjsYr, by = c("site", "year"), relationship = "many-to-many") %>% 
  select(-experiment) %>% 
  distinct() %>% 
  arrange(date, .by_group = TRUE) %>%
  mutate(NDVI = NDVI - 0.5) %>% 
  mutate(deltaNDVI = (NDVI - lag(NDVI))) %>% 
  mutate(deltaNDVI = ifelse(day == 91, NDVI + 0.5 - meanNDVI, deltaNDVI)) %>% 
  dplyr::select(-NDVI)


#### Write output files #### 

# yr outputs

sites <- unique(FullProjsYr$site)
experiments <- unique(FullProjsYr$experiment)

filename <- "Data/Scenarios/VI GCM scenarios/Inputs/NDVIInputs/IA/NDVIInputScenariosYr" 

for (i in sites) {
  # i <- sites[1]
  
  sub <- FullProjsYr %>% 
    filter(site == i)
  
  for (ii in experiments) {
    #  ii <- experiments[1]
    
    sub_sub <- sub %>% 
      filter(experiment == ii) %>% 
      arrange(year) %>% 
      mutate(year = year - 1999) %>% 
      ungroup() %>% 
      dplyr::select(year, meanNDVI) %>% 
      pivot_wider(names_from = year, values_from = meanNDVI)
    
    
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
      ), 
      col_names = FALSE
    )
  }
}


# daily outputs

filename <- "Data/Scenarios/VI GCM scenarios/Inputs/NDVIInputs/IA/NDVIInputScenariosIA" 

for (i in sites) {
  # i <- sites[1]
  
  sub <- centNDVIDaily %>% 
    filter(site == i)
  
  sub <- sub %>% 
    arrange(date) %>% 
    mutate(year = year(date) - 1999) %>% 
    ungroup() %>% 
    dplyr::select(year, day, deltaNDVI) %>% 
    pivot_wider(names_from = day, values_from = deltaNDVI) %>% 
    select(-1)
  
  
  write_csv(
    sub,
    paste(
      filename,
      paste(
        "",
        as.character(i),
        "",
        sep = "_"),
      ".csv",
      sep = ""
    ), 
    col_names = FALSE
  )
}

beep()



yr22plot <- FullProjsDaily %>% 
  filter(year(date) == 2022) %>% 
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
  ) %>% 
  ggplot(aes(x = date, y = NDVI, col = site)) + 
  geom_line(size = 1.5) +
  scale_color_manual(values = pal(10)) +
  ylim(0, 1) +
  facet_grid(rows = vars(site)) +
  theme_classic() +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "bottom", legend.spacing.y = unit(0.1, 'cm')) +
  guides(col=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=3,byrow=TRUE))

#ggsave("Figures/App 1/NDVI.png", yr22plot, height = 9, width = 4)

  

yr22plot <- FullProjsDaily %>% 
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
  ) %>% 
  filter(year(date) <= 2022) %>% 
  ggplot(aes(x = yday(date), y = NDVI, col = site, group = year(date))) + 
  geom_line(size = 0.5) +
  scale_color_manual(values = pal(10)) +
  ylim(0, 1) +
  facet_grid(rows = vars(site)) +
  theme_classic() +
  theme(text = element_text(size = 30, color = "grey30", family = "Montserrat"),
        legend.position = "bottom", legend.spacing.y = unit(0.1, 'cm')) +
  guides(col=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=3,byrow=TRUE))

#ggsave("Figures/App 1/NDVI_allyrs.png", yr22plot, height = 9, width = 4)

  



