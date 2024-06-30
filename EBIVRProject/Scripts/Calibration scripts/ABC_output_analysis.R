# Cara Gallagher
# October 14th, 2022
# Energy Budget with Individual Variation project
# ABC calibration with outputs generated using the nlrx package

#------------------------------------------------#

# specify the path from which to load R packages
.libPaths('/home/gallagher/calibration/analysis/Rpkgs')

####  Packages: #### 
install.packages("~/mypkg", lib = templib, type = "source", repos = NULL)


library(tidyverse)
library(readxl)
library(foreach)
library(doParallel)

#------------------------------------------------#


#setup parallel backend to use processors
n.cpus <- Sys.getenv("SLURM_CPUS_PER_TASK")
n.cpus <- as.numeric(n.cpus)

cl <- makeCluster(n.cpus)
registerDoParallel(cl)

####  Post processing ####  

AllOuts <- read.csv("../../MetabolicVariationLargeOuts/Cal-3190.csv", skip = 6, nrows= 100000)
AllOuts <- read.csv("../../MetabolicVariationLargeOuts/CalUpd-5533.csv", skip = 6)
AllOuts <- read.csv("../../MetabolicVariationLargeOuts/CalUpd-5547.csv", skip = 6)

completedRuns <- AllOuts %>% filter(X.step. == 38833)

completedRuns <- completedRuns %>% rename(run.num = X.run.number., step = X.step.)

rm(AllOuts)

listOne <- "[]"
listTwo <- "[[] []]"
listThree <- "[[] [] []]"
listFour <- "[[] [] [] []]"

options(dplyr.summarise.inform = FALSE)


calOuts <- foreach(n=1:nrow(completedRuns), .combine=rbind, .packages = c('tidyverse','readxl')) %dopar% {
  
  rowRuns <- completedRuns[n,]
  #rowRuns <- completedRuns[which(completedRuns$run.num == 7250),]
  
  p567.pattern <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig1Dat.xlsx")
  p567.pattern <- p567.pattern %>% rename(dsb = ageGest)
  
  p8910.pattern <- read_excel("Data/Verification/Reproduction/Sadowska2016/Fig2Dat.xlsx")
  p8910.pattern <- p8910.pattern %>% rename(LS = litterSize)
  
  
  ####  Pattern 1. Fetal mass at birth  #### 
  
  p1 <- rowRuns %>%  select(run.num, p1.FMAB)
  
  if (p1$p1.FMAB == listOne) {
    
    p1Out <- tibble(run.num = rowRuns$run.num, p1.fit = NA)
    
  } else {
    
    nam <- c(as.character(1:10000))
    
    p1 <- p1 %>% 
      separate(p1.FMAB, nam, sep = " ") %>% 
      gather(key = "num", value = "value", -run.num) %>% 
      mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             num = as.numeric(num), value = as.numeric(value)) %>% 
      drop_na()
    
    # ggplot(p1, aes(x = value)) +
    #   annotate("rect", xmin=0.001, xmax=0.0025, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
    #   geom_histogram(alpha = 0.75, fill = "cadetblue", bins = 40) +
    #   scale_x_continuous(breaks = c(0.0005,0.0010,0.0015,0.0020,0.0025,0.0030), labels = c("0.5","1.0","1.5","2.0","2.5","3.0")) +
    #   labs(y = "Count", x = "Neonate mass [g]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p1Out <- p1 %>% 
      arrange(run.num,num) %>% 
      mutate(p1.fit = ifelse(value <= 0.0025 & value >= 0.001, 0, ifelse(value >= 0.0025, (value - 0.0025) / (0.0025 - 0.001), (0.001 - value) / (0.0025 - 0.001)))) %>% 
      group_by(run.num) %>% 
      summarise(p1.fit = median(p1.fit, na.rm=TRUE)) %>% 
      select(run.num, p1.fit)
    
  }
  
  rm(p1)
  
  ####  Pattern 2. Birth mass by litter size  #### 
  
  p2 <- rowRuns %>%  select(run.num, p2.BMxLS)
  
  if (p2$p2.BMxLS == listTwo) {
    
    p2Out <- tibble(run.num = rowRuns$run.num, p2.fit = NA)
    
  } else {
    
    p2 <- p2 %>% 
      separate(p2.BMxLS, nam, sep = " ") %>% 
      gather(key = "num", value = "value", -run.num) %>% 
      mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             value = as.numeric(value),
             num = as.numeric(num)) %>% 
      arrange(run.num, num) %>% 
      drop_na() %>% 
      mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
      pivot_wider(names_from = order, values_from = value) %>% 
      rename(BM = `1`, LS = `2`) %>%
      group_by(run.num) %>%
      filter(!(n_distinct(BM) == 1))
    
    # ggplot(p2, aes(x = LS, y = BM * 1000)) +
    #   geom_jitter(alpha = 0.5, width  = 0.1, col = "grey50")  +
    #   geom_smooth(method = "lm",
    #               se = FALSE,
    #               col = "cadetblue", size = 2) +
    #   labs(y = "Neonate mass [g]", x = "Litter size [N pups]") +
    #   scale_x_continuous(breaks = seq(1:9)) +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    ifelse(
      nrow(p2) > 0,
      
      p2Out <- p2 %>%
        do(mod = lm(BM ~ LS, data = .)) %>%
        mutate(slope = summary(mod)$coeff[2]) %>%
        mutate(p2.fit = ifelse(slope < 0, 0, 1)) %>%
        select(run.num, p2.fit),
      
      p2Out <- tibble(run.num = p2$run.num[1], p2.fit = NA)
    )
    
  }
  
  rm(p2)
  
  ####  Pattern 3. Total body mass by age  #### 
  
  p3 <- rowRuns %>% select(run.num, p34.BMLMxA)
  
  if (p3$p34.BMLMxA == listThree) {
    
    p3Out <- tibble(run.num = rowRuns$run.num, p3.fit = NA)
    p4Out <- tibble(run.num = rowRuns$run.num, p4.fit = NA)
    
  } else {
    
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
      mutate(order = rep(1:3, each = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), times = 3)) %>% 
      pivot_wider(names_from = order, values_from = value) %>% 
      rename(BM = `1`, LM = `2`, age = `3`) %>%
      filter(age >= 21 & age <= 620) %>% 
      mutate(p3.pattern = 0.02483015*(1 - (1 - (0.005569125 / 0.02483015)^(1/3))*exp((-0.03913599*age)/3))^3,
             p4.pattern = 0.01930132*(1 - (1 - (0.004688886 / 0.01930132)^(1/3))*exp((-0.04845531*age)/3))^3) %>% 
      mutate(p3.fit = NA, p4.fit = NA)     
    
    # ggplot(p3, aes(x = age)) +
    #   geom_point(aes(y = BM * 1000), alpha = 0.5, col = "cadetblue")  +
    #   geom_line(aes(y = p3.pattern * 1000), size = 2, linetype = "dashed") +
    #   labs(y = "Body mass [g]", x = "Age [days]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p3Out <- p3 %>% 
      mutate(p3.fit = abs(BM - p3.pattern)/ p3.pattern) %>% 
      group_by(run.num) %>% 
      summarise(p3.fit = median(p3.fit, na.rm=TRUE)) %>% 
      select(run.num, p3.fit)
    
    ####  Pattern 4. Lean mass by age  #### 
    
    # ggplot(p3, aes(x = age)) +
    #   geom_point(aes(y = LM * 1000), alpha = 0.5, col = "cadetblue")  +
    #   geom_line(aes(y = p4.pattern * 1000), size = 2, linetype = "dashed") +
    #   labs(y = "Lean body mass [g]", x = "Age [days]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    
    p4Out <- p3 %>% 
      mutate(p4.fit = abs(LM - p4.pattern) / p4.pattern) %>% 
      group_by(run.num) %>% 
      summarise(p4.fit = median(p4.fit, na.rm=TRUE)) %>% 
      select(run.num, p4.fit)
    
  }
  
  rm(p3)
  
  ####  Patterns 5, 6, & 7  #### 
  
  p5 <- rowRuns %>% select(run.num, p567.dsbxMMFILM)
  
  if (p5$p567.dsbxMMFILM == listFour) {
    
    p5Out <- tibble(run.num = rowRuns$run.num, p5.fit = NA)
    p6Out <- tibble(run.num = rowRuns$run.num, p6.fit = NA)
    p7Out <- tibble(run.num = rowRuns$run.num, p7.fit = NA)
    
  } else {
    
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
      mutate(order = rep(1:4, each = (max(row_number()) / 4)), num = rep(1:(max(row_number()) / 4), times = 4)) %>% 
      pivot_wider(names_from = order, values_from = value) %>% 
      rename(dsb = `1`, MM = `2`, FI = `3`, LitM = `4`) %>%
      left_join(p567.pattern[which(p567.pattern$Metric == "Mean"),], by = "dsb") %>% 
      select(-Metric) %>% 
      mutate(bodyMass = bodyMass / 1000, foodCons = foodCons * 17.80, FI = FI * 12.2811, LitterMass = LitterMass / 1000)
    
    
    ####  Pattern 5. Lactating mother mass by pup age  #### 
    
    # ggplot(p5, aes(x = dsb)) +
    #   geom_jitter(aes(y = MM * 1000), alpha = 0.5, col = "grey50", width = 0.1)  +
    #   geom_smooth(aes(y = MM * 1000), method = "gam", size = 2, col = "cadetblue", se = FALSE) +
    #   geom_line(aes(y = bodyMass * 1000), size = 2, linetype = "dashed") +
    #   labs(y = "Mother body mass [g]", x = "Pup age [days]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p5Out <- p5 %>% 
      mutate(p5.fit = abs(MM - bodyMass) / bodyMass) %>% 
      group_by(run.num) %>% 
      summarise(p5.fit = median(p5.fit, na.rm=TRUE)) %>% 
      select(run.num, p5.fit)
    
    ####  Pattern 6. Lactating mother food intake by pup age  #### 
    
    # ggplot(p5, aes(x = dsb)) +
    #   geom_jitter(aes(y = FI), alpha = 0.5, col = "grey50", width = 0.1)  +
    #   geom_smooth(aes(y = FI), method = "gam", size = 2, col = "cadetblue", se = FALSE) +
    #   geom_line(aes(y = foodCons), size = 2, linetype = "dashed") +
    #   labs(y = "Mother food intake [kJ]", x = "Pup age [days]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p6Out <- p5 %>% 
      mutate(p6.fit = abs(FI - foodCons) / foodCons) %>% 
      group_by(run.num) %>% 
      summarise(p6.fit = median(p6.fit, na.rm=TRUE)) %>% 
      select(run.num, p6.fit)
    
    ####  Pattern 7. Total litter mass by pup age  #### 
    
    # ggplot(p5, aes(x = dsb)) +
    #   geom_jitter(aes(y = LitM * 1000), alpha = 0.5, col = "grey50", width = 0.1)  +
    #   geom_smooth(aes(y = LitM * 1000), method = "gam", size = 2, col = "cadetblue", se = FALSE) +
    #   geom_line(aes(y = LitterMass * 1000), size = 2, linetype = "dashed") +
    #   labs(y = "Total litter mass [g]", x = "Pup age [days]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p7Out <- p5 %>% 
      mutate(p7.fit = abs(LitM - LitterMass)/ LitterMass) %>% 
      group_by(run.num) %>% 
      summarise(p7.fit = median(p7.fit, na.rm=TRUE)) %>% 
      select(run.num, p7.fit)
    
  }
  
  rm(p5)
  
  ####  Patterns 8, 9, & 10  #### 
  
  p8 <- rowRuns %>% select(run.num, p8910.LSxpFIEUMEO)
  
  if (p8$p8910.LSxpFIEUMEO == listFour) {
    
    p8Out <- tibble(run.num = rowRuns$run.num, p8.fit = NA)
    p9Out <- tibble(run.num = rowRuns$run.num, p9.fit = NA)
    p10Out <- tibble(run.num = rowRuns$run.num, p10.fit = NA)
    
  } else {
    
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
      mutate(order = rep(1:4, each = (max(row_number()) / 4)), num = rep(1:(max(row_number()) / 4), times = 4)) %>% 
      pivot_wider(names_from = order, values_from = value) %>% 
      rename(LS = `1`, pFI = `2`, pEU = `3`, pMEO = `4`) %>%
      left_join(p8910.pattern, by = "LS") %>% 
      pivot_wider(names_from = Metric, values_from = Value) %>% 
      mutate(FC = FC * 17.80, pFI = pFI * 12.2811)
    
    
    ####  Pattern 8. Mother peak food intake by litter size  #### 
    
    # ggplot(p8, aes(x = LS)) +
    #   geom_boxplot(aes(x = as.factor(LS), y = pFI), col = "cadetblue", width = 0.5, size = 1.1, outlier.shape = NA) +
    #   geom_jitter(aes(y = pFI), alpha = 0.75, col = "grey50", width = 0.1)  +
    #   geom_line(aes(y = FC), size = 2, linetype = "dashed") +
    #   scale_x_discrete(breaks = seq(1:9)) +
    #   labs(y = "Mother food intake [kJ]", x = "Litter size [N pups]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p8Out <- p8 %>% 
      mutate(p8.fit = abs(pFI - FC) / FC) %>% 
      group_by(run.num) %>% 
      summarise(p8.fit = median(p8.fit, na.rm=TRUE)) %>% 
      select(run.num, p8.fit)
    
    
    ####  Pattern 9. Mother peak energy use by litter size  #### 
    
    # ggplot(p8, aes(x = LS)) +
    #   geom_boxplot(aes(x = as.factor(LS), y = pEU), col = "cadetblue", width = 0.5, size = 1.1, outlier.shape = NA) +
    #   geom_jitter(aes(y = pEU), alpha = 0.75, col = "grey50", width = 0.1)  +
    #   geom_line(aes(y = ADMR), size = 2, linetype = "dashed") +
    #   scale_x_discrete(breaks = seq(1:9)) +
    #   labs(y = "Mother energy use [kJ]", x = "Litter size [N pups]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p9Out <- p8 %>% 
      mutate(p9.fit = abs(pEU - ADMR) / ADMR) %>% 
      group_by(run.num) %>% 
      summarise(p9.fit = median(p9.fit, na.rm=TRUE)) %>% 
      select(run.num, p9.fit)
    
    ####  Pattern 10. Mother peak milk transfer by litter size  #### 
    
    # ggplot(p8, aes(x = LS)) +
    #   geom_boxplot(aes(x = as.factor(LS), y = pMEO), col = "cadetblue", width = 0.5, size = 1.1, outlier.shape = NA) +
    #   geom_jitter(aes(y = pMEO), alpha = 0.75, col = "grey50", width = 0.1)  +
    #   geom_line(aes(y = MEO), size = 2, linetype = "dashed") +
    #   scale_x_discrete(breaks = seq(1:9)) +
    #   labs(y = "Mother milk transfer [kJ]", x = "Litter size [N pups]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p10Out <- p8 %>% 
      mutate(p10.fit = abs(pMEO - MEO) / MEO) %>% 
      group_by(run.num) %>% 
      summarise(p10.fit = median(p10.fit, na.rm=TRUE)) %>% 
      select(run.num, p10.fit)
    
  }
  
  rm(p8)
  
  ####  Pattern 11. Pup mass at weaning by litter size  #### 
  
  p11 <- rowRuns %>%  select(run.num, p11.PMxLS)
  
  if (p11$p11.PMxLS == listTwo) {
    
    p11Out <- tibble(run.num = rowRuns$run.num, p11.fit = NA)
    
  } else {
    
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
      rename(WM = `1`, LS = `2`) %>%
      group_by(run.num) %>%
      filter(!(n_distinct(WM) == 1))
    
    # ggplot(p11, aes(x = LS, y = WM * 1000)) +
    #   geom_jitter(alpha = 0.5, width  = 0.1, col = "grey50")  +
    #   geom_smooth(method = "lm",
    #               se = FALSE,
    #               col = "cadetblue", size = 2) +
    #   labs(y = "Weaning mass [g]", x = "Litter size [N pups]") +
    #   scale_x_continuous(breaks = seq(1:9)) +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    ifelse(
      nrow(p11) > 0,
      
      p11Out <- p11 %>%
        do(mod = lm(WM ~ LS, data = .)) %>%
        mutate(slope = summary(mod)$coeff[2]) %>%
        mutate(p11.fit = ifelse(slope < 0, 0, 1)) %>%
        select(run.num, p11.fit),
      
      p11Out <- tibble(run.num = p11$run.num[1], p11.fit = NA)
    )
    
  }
  
  rm(p11)
  
  ####  Pattern 12. Litter size at birth  #### 
  
  p12 <- rowRuns %>%  select(run.num, p12.LSb)
  
  if (p12$p12.LSb == listOne) {
    
    p12Out <- tibble(run.num = rowRuns$run.num, p12.fit = NA)
    
  } else {
    
    nam <- c(as.character(1:10000))
    
    p12 <- p12 %>% 
      separate(p12.LSb, nam, sep = " ") %>% 
      gather(key = "num", value = "value", -run.num) %>% 
      mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             num = as.numeric(num), value = as.numeric(value)) %>%
      drop_na() 
    
    # ggplot(p12, aes(x = value)) +
    #   geom_histogram(alpha = 0.75, fill = "cadetblue", col = "white", size = 10, binwidth = 1) +
    #   annotate("rect", xmin=3.6, xmax=6.1, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
    #   scale_x_continuous(breaks = seq(1:9)) +
    #   labs(y = "Count", x = "Litter size [N pups]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p12Out <- p12 %>% 
      arrange(run.num,num) %>% 
      mutate(p12.fit = ifelse(value <= 6.1 & value >= 3.6, 0, ifelse(value >= 6.1, (value - 6.1) / (6.1 - 3.6), (3.6 - value) / (6.1 - 3.6)))) %>% 
      group_by(run.num) %>% 
      summarise(p12.fit = median(p12.fit, na.rm=TRUE)) %>% 
      select(run.num, p12.fit)
    
  }
  
  rm(p12)
  
  ####  Pattern 13. Litter size at weaning  #### 
  
  p13 <- rowRuns %>%  select(run.num, p13.LSw)
  
  if (p13$p13.LSw == listOne) {
    
    p13Out <- tibble(run.num = rowRuns$run.num, p13.fit = NA)
    
  } else {
    
    nam <- c(as.character(1:10000))
    
    p13 <- p13 %>% 
      separate(p13.LSw, nam, sep = " ") %>% 
      gather(key = "num", value = "value", -run.num) %>% 
      mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             num = as.numeric(num), value = as.numeric(value)) %>%
      drop_na() 
    
    
    # ggplot(p13, aes(x = value)) +
    #   geom_histogram(alpha = 0.75, fill = "cadetblue", binwidth = 1, col = "white", size = 10) +
    #   annotate("rect", xmin=1.28, xmax=5.28, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
    #   scale_x_continuous(breaks = seq(1:9)) +
    #   labs(y = "Count", x = "Litter size [N pups]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    
    p13Out <- p13 %>% 
      arrange(run.num,num) %>% 
      mutate(p13.fit = ifelse(value <= 5.28 & value >= 1.28, 0, ifelse(value >= 5.28, (value - 5.28) / (5.28 - 1.28), (1.28 - value) / (5.28 - 1.28)))) %>% 
      group_by(run.num) %>% 
      summarise(p13.fit = median(p13.fit, na.rm=TRUE)) %>% 
      select(run.num, p13.fit)
    
  }
  
  rm(p13)
  
  ####  Pattern 14. Probability of weaning by mother body mass  #### 
  
  p14 <- rowRuns %>%  select(run.num, p14.PwxBM)
  
  if (p14$p14.PwxBM == listTwo) {
    
    p14Out <- tibble(run.num = rowRuns$run.num, p14.fit = NA)
    
  } else {
    
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
      mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
      pivot_wider(names_from = order, values_from = value) %>% 
      rename(pW = `1`, BM = `2`) %>% 
      mutate(BM = round(BM * 1000)) %>% 
      group_by(run.num, BM) %>% 
      summarise(pW = mean(pW, na.rm = TRUE)) 
    
    # ggplot(p14, aes(x = BM, y = pW)) +
    #   geom_point(alpha = 0.5)  +
    #   geom_smooth(method = "glm",
    #               method.args = list(family = "binomial"),
    #               se = FALSE,
    #               col = "cadetblue", 
    #               size = 2) +
    #   geom_line(data = p14.pattern, aes(x = BM, y = prob), size = 2, linetype = "dashed") +
    #   labs(y = "Probability of weaning success", x = "Mother mass [g]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    
    p14Out <- p14 %>% 
      left_join(p14.pattern, by = "BM") %>% 
      mutate(p14.fit = abs(pW - prob) / 0.25) %>% 
      summarise(p14.fit = median(p14.fit, na.rm=TRUE)) %>% 
      select(run.num, p14.fit)
    
  }
  
  rm(p14)
  
  ####  Pattern 15. Average / range of body fat %  #### 
  
  p15 <- rowRuns %>%  select(run.num, p15.BF)
  
  if (p15$p15.BF == listOne) {
    
    p15Out <- tibble(run.num = rowRuns$run.num, p15.fit = NA)
    
  } else {
    
    nam <- c(as.character(1:30000))
    
    p15 <- p15 %>% 
      separate(p15.BF, nam, sep = " ") %>% 
      gather(key = "num", value = "value", -run.num) %>% 
      mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             num = as.numeric(num), value = as.numeric(value)) %>%
      drop_na() 
    
    
    # ggplot(p15, aes(x = value * 100)) +
    #   geom_histogram(alpha = 0.75, fill = "cadetblue", bins = 50, col = "white", size = 1) +
    #   annotate("rect", xmin=3, xmax=29, ymin=0, ymax=Inf, alpha=0.2, fill="black") +
    #   labs(y = "Count", x = "Storage level [% body fat]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    
    p15Out <- p15 %>% 
      arrange(run.num,num) %>% 
      mutate(p15.fit = ifelse(value <= 0.29 & value >= 0.03, 0, ifelse(value >= 0.29, (value - 0.29) / (0.29 - 0.03), (0.03 - value) / (0.29 - 0.03)))) %>% 
      group_by(run.num) %>% 
      summarise(p15.fit = median(p15.fit, na.rm=TRUE)) %>% 
      select(run.num, p15.fit)
    
  }
  
  rm(p15)
  
  ####  Pattern 16. Body fat % of living animals  #### 
  
  p16 <- rowRuns %>%  select(run.num, p16.BF)
  
  if (p16$p16.BF == listOne) {
    
    p16Out <- tibble(run.num = rowRuns$run.num, p16.fit = NA)
    
  } else {
    
    nam <- c(as.character(1:30000))
    
    p16 <- p16 %>% 
      separate(p16.BF, nam, sep = " ") %>% 
      gather(key = "num", value = "value", -run.num) %>% 
      mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             num = as.numeric(num), value = as.numeric(value)) %>%
      drop_na() 
    
    # ggplot(p16, aes(x = value * 100)) +
    #   geom_histogram(alpha = 0.5, fill = "cadetblue", binwidth = 0.1) +
    #   geom_vline(xintercept = 3, size = 2, linetype = "dashed") +
    #   labs(y = "Count", x = "Storage level [% body fat]") +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    
    p16Out <- p16 %>% 
      mutate(p16.fit = ifelse(value <= 0.03, 0, abs(value - 0.03) / 0.03)) %>% 
      group_by(run.num) %>% 
      summarise(p16.fit = median(p16.fit, na.rm=TRUE)) %>% 
      select(run.num, p16.fit)
    
  }
  
  rm(p16)
  
  ####  Pattern 17. Field metabolic rate by body mass #### 
  
  p17 <- rowRuns %>%  select(run.num, p17.FMRxBM)
  
  if (p17$p17.FMRxBM == listTwo) {
    
    p17Out <- tibble(run.num = rowRuns$run.num, p17.fit = NA)
    
  } else {
    
    p17.pattern <- tibble(
      mass = seq(1,60,1),
      Speakman1999 = exp(1.878) * mass^0.66,   
      # King1974 = 753 * (mass / 1000)^0.67,                 
      Nagy1999 = 5.48 * mass^0.71)
    
    p17.pattern <- p17.pattern %>% 
      mutate(avgFMR = (Speakman1999 + Nagy1999) / 2) %>% 
      select(mass, avgFMR) %>% 
      rename(BM = mass)
    
    nam <- c(as.character(1:30000))
    
    p17 <- p17 %>% 
      separate(p17.FMRxBM, nam, sep = " ") %>% 
      gather(key = "num", value = "value",-run.num) %>% 
      mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
             value = as.numeric(value),
             num = as.numeric(num)) %>%
      arrange(run.num, num) %>%
      drop_na() %>% 
      mutate(order = rep(1:2, each = (max(row_number()) / 2)), num = rep(1:(max(row_number()) / 2), times = 2)) %>% 
      pivot_wider(names_from = order, values_from = value) %>% 
      rename(BM = `1`, FMR = `2`) %>%
      mutate(BM = round(BM * 1000), FMR = FMR / 1000) %>% 
      left_join(p17.pattern, by = "BM") 
    
    # ggplot(p17, aes(x = as.factor(BM))) +
    #   geom_boxplot(aes(y = FMR), col = "cadetblue", size = 0.9, outlier.shape = NA) +
    #   geom_jitter(aes(y = FMR), col = "grey50", size = 0.1, alpha = 0.5, width =0.1) +
    #   geom_point(aes(y = avgFMR), size = 2) +
    #   labs(y = "Field metabolic rate [kJ day-1]", x = "Body mass [g]") +
    #   scale_x_discrete(breaks = seq(from = 5, to = 60, by = 5 )) +
    #   theme_classic() +
    #   theme(text = element_text(size = 16, color = "grey10"))
    
    p17Out <- p17 %>% 
      mutate(p17.fit = abs(FMR - avgFMR) / avgFMR) %>% 
      group_by(run.num) %>% 
      summarise(p17.fit = median(p17.fit, na.rm=TRUE)) %>% 
      select(run.num, p17.fit)
    
  }
  
  rm(p17)
  
  #### All together! ####
  
  calOutsRow <- 
    left_join(p1Out, p2Out, by='run.num') %>%
    left_join(., p3Out, by='run.num') %>%
    left_join(., p4Out, by='run.num') %>%
    left_join(., p5Out, by='run.num') %>%
    left_join(., p6Out, by='run.num') %>%
    left_join(., p7Out, by='run.num') %>%
    left_join(., p8Out, by='run.num') %>%
    left_join(., p9Out, by='run.num') %>%
    left_join(., p10Out, by='run.num') %>%
    left_join(., p11Out, by='run.num') %>%
    left_join(., p12Out, by='run.num') %>%
    left_join(., p13Out, by='run.num') %>%
    left_join(., p14Out, by='run.num') %>%
    left_join(., p15Out, by='run.num') %>%
    left_join(., p16Out, by='run.num') %>%
    left_join(., p17Out, by='run.num')  
  
  rm(p1Out, p2Out, p3Out, p4Out, p5Out, p6Out, p7Out, p8Out, p9Out, p10Out, p11Out, p12Out, p13Out, p14Out, p15Out, p16Out, p17Out)
  
  calOutsRow
  
}

#stop cluster
stopCluster(cl)










