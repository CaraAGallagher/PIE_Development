# Cara Gallagher
# December 1st, 2022
# Energy Budget with Individual Variation project
# Constructing parameter plots from ABC calibration results

#------------------------------------------------#
####  Packages: #### 
library(tidyverse)
library(showtext)
library(patchwork)
library(corrplot)


font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

#### Load results ####

fitsPs <- read.csv("Data/Calibration/ABCParameterTable.csv")
fitsPs <- fitsPs %>% 
  mutate(run.num = row_number()) 

# inputParams1 <- read.csv("Data/Calibration/ParamInputsFromNLRX05102022.csv")
# inputParams2 <- read.csv("Data/Calibration/ParamInputsFromNLRX20102022.csv")
# inputParams3 <- read.csv("Data/Calibration/ParamInputsFromNLRX19102022.csv")
# inputParams <- bind_rows(inputParams1, inputParams2, inputParams3)
# rm(inputParams1, inputParams2, inputParams3)

inputParams1 <- read.csv("Data/Calibration/Inputs/ParamInputs-3415.csv")
inputParams2 <- read.csv("Data/Calibration/Inputs/ParamInputs-3416.csv")
inputParams3 <- read.csv("Data/Calibration/Inputs/ParamInputs-lpt.csv")
inputParams <- bind_rows(inputParams1, inputParams2, inputParams3)
rm(inputParams1, inputParams2, inputParams3)

inputParams <- inputParams %>% 
  mutate(run.num = row_number()) 


densPlot <- inputParams %>% 
  select(-run.num) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  mutate(source = "priors") %>% 
  mutate(value = ifelse(parameter %in% c("growth.lm.prob.const","preg.prob.const","lact.prob.const"), (value - 10) / ((50 - 10)/ 2), 
                        ifelse(parameter == "surv.prob.const", (value - 20) / ((120 - 20) / 2),
                               ifelse(parameter %in% c("growth.lm.prob.mid","preg.prob.mid","lact.prob.mid"), value / (0.2 / 2), 
                                      ifelse(parameter == "surv.prob.mid", value / (0.1 / 2), value / (2 / 2)))))) %>% 
  filter(parameter == "growth.lm.prob.const") %>% 
  mutate(parameter = "priors")

densPlotO <- fitsPs %>% 
 # select(-run.num, -X) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  mutate(source = "posteriors") %>% 
  mutate(value = ifelse(parameter %in% c("growth.lm.prob.const","preg.prob.const","lact.prob.const"), (value - 10) / ((50 - 10)/ 2), 
                        ifelse(parameter == "surv.prob.const", (value - 20) / ((120 - 20) / 2),
                               ifelse(parameter %in% c("growth.lm.prob.mid","preg.prob.mid","lact.prob.mid"), value / (0.2 / 2), 
                                      ifelse(parameter == "surv.prob.mid", value / (0.1 / 2), value / (2 / 2))))))

densPlot <- densPlot %>% 
  bind_rows(densPlotO) %>% 
  mutate(parameter = factor(parameter, levels = c(
    "priors", 
    "growth.lm.prob.const",
    "growth.lm.prob.mid",
    "preg.prob.const",
    "preg.prob.mid",
    "lact.prob.const",
    "lact.prob.mid",
    "surv.prob.const",
    "surv.prob.mid",
    "surv.mod.embryo",
    "surv.mod.off"
  ))) %>% 
  drop_na()

pal1 <- colorRampPalette(c("#E4AA4E","#E4714E","#BC4A53","#3F3F7B"))
pal1 <- rev(pal1(10))
pal1 <- c(pal1[10],pal1[8],pal1[8],pal1[4],pal1[4],pal1[4],pal1[4],pal1[2],pal1[2],pal1[2],pal1[2])


priPostPlot <- ggplot(densPlot) +
  aes(x = 1,
      y = value,
      fill = parameter) +
  geom_flat_violin(position = position_nudge(x = .2), lwd = 0.9,  col = NA, alpha = 0.5) +
  #geom_boxplot(width = .1, alpha = 0.7) + 
  geom_boxplot(aes(col = parameter), width = .1, lwd = 0.9, alpha =0.55) + 
  coord_flip() +
  scale_fill_manual(values = pal1) +
  scale_color_manual(values = pal1) +
  theme_classic() +
  facet_wrap(. ~ parameter, ncol = 1, strip.position = "left") +
  labs(y = "scaled value", x = NULL) + 
  theme(text = element_text(size = 40, color = "grey30", family = "Montserrat"),
        legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        strip.background = element_blank())
priPostPlot
#ggsave("Figures/CalOuts/priPostPlot.png", priPostPlot, width = 5, height = 8)


#### correlation plot ####
# palL <- natparks.pals(name="Denali",n=6,type="continuous")
# palL <- palL[2:6]
# palH <- natparks.pals(name="Volcanoes",n=7,type="continuous")
# palH <- palH[2:6]
# 
# pal2 <- c(rev(palL),palH)

pal2 <- colorRampPalette(c("#3F3F7B","#278192","#00B089","#AABC4A","#E5DC54","#E4AA4E","#E4714E","#BC4A53"))
pal2 <- pal2(10)

res <- cor(fitsPs[,1:10])

#png(file = "Figures/CalOuts/paramCorr.png", width = 1800, height = 1300, pointsize = 23, res=300)
par(family="Montserrat")
corrplot(res, type = "upper", order = "original",
         tl.col = "black", tl.srt = 45,
         col = pal2)
#dev.off()



#### Resulting allocation curves ####
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
              size = 1.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 3) +
  scale_color_manual(values = pal) +
  labs(x = "Body fat", y = "Growth allocation") +
  theme_classic() +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme(text = element_text(size = 40, color = "grey30",family="Montserrat"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
  
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
              size = 1.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 3) +
  scale_color_manual(values = pal) +
  labs(x = "Body fat", y = "Pregnancy allocation") +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30",family="Montserrat"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())

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
              size = 1.5) +
  geom_smooth(method = "gam",
              #method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 3) +
  scale_color_manual(values = pal) +
  labs(x = "Body fat", y = "Lactation allocation") +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30",family="Montserrat"),
        legend.position = "none")

# survival 
fitsSurv <- fitsPs %>% 
  select( "run.num","surv.prob.const","surv.prob.mid")


for (i in c(seq(0.0,0.4,0.01))) {
  n <- as.character(i)
  
  fitsSurv <- fitsSurv %>% 
    add_column(n = NA)
  
}

colnames(fitsSurv)[4:ncol(fitsSurv)] <- as.character(seq(0.0,0.4,0.01))

for (i in 1:nrow(fitsSurv)) {
  
  row <- fitsSurv[i,]
  
  for (ii in 1:length(seq(0.0,0.4,0.01))) {
    
    const <- as.numeric(row[2])
    mid <- as.numeric(row[3])
    sl <- as.numeric(colnames(row[ii + 3]))
    
    ind <- 1 / (1 + exp(-1 * const * (sl - mid)))
    
    fitsSurv[i,ii + 3] <- ind
    
  }
  
}

fitsSurv <- fitsSurv %>% 
  pivot_longer(4:ncol(fitsSurv), names_to = "storageLevel", values_to = "value") %>% 
  mutate(storageLevel = as.numeric(storageLevel))


survPlot <- ggplot(fitsSurv, aes(x = storageLevel, y = value)) +
  geom_smooth(aes(col = as.factor(run.num)), 
              method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              size = 1.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 3) +
  scale_color_manual(values = pal) +
  labs(x = "Body fat", y = "Survival") +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30",family="Montserrat"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())


# survival for embryos

fitsSurvE <- fitsPs %>% 
  select( "run.num","surv.prob.const","surv.prob.mid","surv.mod.embryo")


for (i in c(seq(0.0,0.4,0.01))) {
  n <- as.character(i)
  
  fitsSurvE <- fitsSurvE %>% 
    add_column(n = NA)
  
}

colnames(fitsSurvE)[5:ncol(fitsSurvE)] <- as.character(seq(0.0,0.4,0.01))

for (i in 1:nrow(fitsSurvE)) {
  
  row <- fitsSurvE[i,]
  
  for (ii in 1:length(seq(0.0,0.4,0.01))) {
    
    const <- as.numeric(row[2])
    mid <- as.numeric(row[3])
    mod <- as.numeric(row[4])
    sl <- as.numeric(colnames(row[ii + 4]))
    
    ind <- 1 / (1 + exp((-1 * const * (1 + ( 1 - mod ))) * (sl - (mid * mod))))
    
    fitsSurvE[i,ii + 4] <- ind
    
  }
  
}

fitsSurvE <- fitsSurvE %>% 
  pivot_longer(5:ncol(fitsSurvE), names_to = "storageLevel", values_to = "value") %>% 
  mutate(storageLevel = as.numeric(storageLevel))


survEPlot <- ggplot(fitsSurvE, aes(x = storageLevel, y = value)) +
  geom_smooth(aes(col = as.factor(run.num)), 
              method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              size = 1.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 3) +
  scale_color_manual(values = pal) +
  labs(x = "Body fat", y = "Survival embryos") +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30",family="Montserrat"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())


# survival for offspring

fitsSurvO <- fitsPs %>% 
  select( "run.num","surv.prob.const","surv.prob.mid","surv.mod.off")


for (i in c(seq(0.0,0.4,0.01))) {
  n <- as.character(i)
  
  fitsSurvO <- fitsSurvO %>% 
    add_column(n = NA)
  
}

colnames(fitsSurvO)[5:ncol(fitsSurvO)] <- as.character(seq(0.0,0.4,0.01))

for (i in 1:nrow(fitsSurvO)) {
  
  row <- fitsSurvO[i,]
  
  for (ii in 1:length(seq(0.0,0.4,0.01))) {
    
    const <- as.numeric(row[2])
    mid <- as.numeric(row[3])
    mod <- as.numeric(row[4])
    sl <- as.numeric(colnames(row[ii + 4]))
    
    ind <- 1 / (1 + exp((-1 * const * (1 + ( 1 - mod ))) * (sl - (mid * mod))))
    
    fitsSurvO[i,ii + 4] <- ind
    
  }
  
}

fitsSurvO <- fitsSurvO %>% 
  pivot_longer(5:ncol(fitsSurvO), names_to = "storageLevel", values_to = "value") %>% 
  mutate(storageLevel = as.numeric(storageLevel))


survOPlot <- ggplot(fitsSurvO, aes(x = storageLevel, y = value)) +
  geom_smooth(aes(col = as.factor(run.num)), 
              method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              size = 1.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              col = pal[1],
              size = 3) +
  scale_color_manual(values = pal) +
  labs(x = "Body fat", y = "Survival offspring") +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30",family="Montserrat"),
        legend.position = "none")

allplot <-
(growPlot /
pregPlot /
lactPlot) |
(survPlot /
survEPlot /
survOPlot )
allplot

#ggsave("Figures/CalOuts/fitCurves.png", allplot, height = 9, width = 11)

#### Prior & posterior histograms ####

plot(density(inputParams$growth.lm.prob.const))
hist(fitsPs$growth.lm.prob.const, xlim = c(10,50), prob = TRUE, breaks = 30)
lines(density(fitsPs$growth.lm.prob.const))

plot(density(inputParams$preg.prob.const))
hist(fitsPs$preg.prob.const, xlim = c(10,50), prob = TRUE, breaks = 30)
lines(density(fitsPs$preg.prob.const))

plot(density(inputParams$lact.prob.const))
hist(fitsPs$lact.prob.const, xlim = c(10,50), prob = TRUE, breaks = 30)
lines(density(fitsPs$lact.prob.const))

plot(density(inputParams$surv.prob.const))
hist(fitsPs$surv.prob.const, xlim = c(20,120), prob = TRUE, breaks = 30)
lines(density(fitsPs$surv.prob.const))

plot(density(inputParams$growth.lm.prob.mid))
hist(fitsPs$growth.lm.prob.mid, xlim = c(0,0.2), prob = TRUE, breaks = 30)
lines(density(fitsPs$growth.lm.prob.mid))

plot(density(inputParams$preg.prob.mid))
hist(fitsPs$preg.prob.mid, xlim = c(0,0.2), prob = TRUE, breaks = 30)
lines(density(fitsPs$preg.prob.mid))

plot(density(inputParams$lact.prob.mid))
hist(fitsPs$lact.prob.mid, xlim = c(0,0.2), prob = TRUE, breaks = 30)
lines(density(fitsPs$lact.prob.mid))

plot(density(inputParams$surv.prob.mid))
hist(fitsPs$surv.prob.mid, xlim = c(0,0.1), prob = TRUE, breaks = 30)
lines(density(fitsPs$surv.prob.mid))

plot(density(inputParams$surv.mod.embryo))
hist(fitsPs$surv.mod.embryo, xlim = c(0,2), prob = TRUE, breaks = 30)
lines(density(fitsPs$surv.mod.embryo))

plot(density(inputParams$surv.mod.off))
hist(fitsPs$surv.mod.off, xlim = c(0,2), prob = TRUE, breaks = 30)
lines(density(fitsPs$surv.mod.off))



#### population size ####
popOuts <- read.csv("Data/Calibration/FitsPopSize.csv", skip = 6)

popOuts <- popOuts %>% 
  select("X.run.number.","X.step.","pop.list")

nam <- c(as.character(1:100000))

popOuts <- popOuts %>%
  separate(pop.list, nam, sep = " ") %>%
  rename(run.num = "X.run.number.") %>% 
  gather(key = "num", value = "value",-run.num, -X.step.) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num) * 10) %>%
  arrange(run.num, num) %>%
  drop_na() 

#### offspring ####
offOuts <- read.csv("Data/Calibration/FitsPopSize.csv", skip = 6)

offOuts <- offOuts %>% 
  select("X.run.number.","X.step.","off.list")

offOuts <- offOuts %>%
  separate(off.list, nam, sep = " ") %>%
  rename(run.num = "X.run.number.") %>% 
  gather(key = "num", value = "value",-run.num, -X.step.) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num) * 10) %>%
  arrange(run.num, num) %>%
  drop_na() 

allOuts <- left_join(offOuts %>% rename("off" = value),popOuts %>% rename("pop" = value), by = c("run.num", "X.step.", "num"))
allOuts <- as_tibble(allOuts)

allOuts <- allOuts %>% 
  mutate(off = off + pop)

popPlot <- ggplot(allOuts, aes(x = num, col = as.factor(run.num))) +
  geom_line(aes(y = off), alpha = 0.75, size = .5) +
 # geom_line(aes(y = pop), alpha = 0.75, size = 1) +
  scale_color_manual(values = pal) +
  labs(y = "Count animals", x = "Days") +
  theme_classic() +
  theme(text = element_text(size = 40, color = "grey30",family="Montserrat"),
        legend.position = "none")

#ggsave("Figures/CalOuts/popPlot.png", popPlot, width = 15, height = 3)





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




