# Cara Gallagher
# January 23th, 2024
# Energy Budget with Individual Variation project
# Initial eco-evo scenarios 

##################################################
# Packages:
library(tidyverse)
#library(profvis)
library(vctrs)

##################################################


baseline <-
  tibble(date = seq(as.Date("2010-01-01"), as.Date("2249-12-01"), by = "weeks"),
         base = 0)

baseline <- baseline %>%
  filter(yday(date) >= 116 + 5 &
           yday(date) <= 278 - 5) # start and end date of base model adjusted for 1 week duration of pulse

pulses_per_year <- tibble(year = seq(2010, 2249, 1),
                          n_pulses = rep(seq(1, 20, 1), each = 12))

days <- tibble(
  date = seq(as.Date("2010-01-01"), as.Date("2249-12-01"), by = "days"),
  pulse = 0,
  base = 0,
  pos_pulses = 0,
  neg_pulses = 0,
  pos_and_neg_pulses = 0
)

# create list of days
days <- days %>%
  filter(!(date %in% baseline$date))

seeds <- seq(1, 10, 1)

for (s in 1:10) {
  
  s <- 6
  
  set.seed(seeds[s])
  
  df <- tibble(
    date = Date(),
    pulse = numeric(),
    base = numeric(),
    pos_pulses = numeric(),
    neg_pulses = numeric(),
    pos_and_neg_pulses = numeric()
  )
  
  for (i in 1:240) {
    # i <- 240
    
    yr <- as.character(pulses_per_year[i, 1])
    
    temp <- baseline %>%
      filter(year(baseline$date) == yr) %>%
      mutate(pulse = 0)
    
    pulse_rows <-
      sample(1:nrow(temp), as.numeric(pulses_per_year[i, 2]), replace = FALSE)
    
    temp <- temp %>%
      mutate(pulse = ifelse(row_number() %in% pulse_rows, 1, 0))
    
    temp <- temp %>%
      rowwise() %>%
      mutate(
        pos_pulses = ifelse(pulse == 1, 1, 0),
        neg_pulses = ifelse(pulse == 1, -1, 0),
        pos_and_neg_pulses = ifelse(pulse == 1, 1, 0)
      )
    
    df <- vec_rbind(df, temp)
    
  }
  
  df <- bind_rows(df, days)
  df <- df %>% arrange(date)
  
  pulse_row_numbers <- which(df$pulse == 1)
  
  df <- df %>%
    mutate(pulse_effect = 0)
  
  # df$pulse_effect[pulse_row_numbers] <- 1
  # df$pulse_effect[c((pulse_row_numbers - 3), (pulse_row_numbers + 3))] <- 0.25
  # df$pulse_effect[c((pulse_row_numbers - 2), (pulse_row_numbers + 2))] <- 0.5
  # df$pulse_effect[c((pulse_row_numbers - 1), (pulse_row_numbers + 1))] <- 0.75
  

  for (ii in pulse_row_numbers) {
    #ii <- pulse_row_numbers[1]
    
    df$pulse_effect[c((ii - 3):(ii))] <- 0.25
    df$pulse_effect[c((ii + 1):(ii + 4))] <- -0.25
    
    df$pos_pulses[c((ii - 3):(ii + 4))] <- 1
    df$neg_pulses[c((ii - 3):(ii + 4))] <- -1

    t <- ifelse(sample(c(TRUE, FALSE), size = 1), 1,-1)
    
    df$pos_and_neg_pulses[c((ii - 3):(ii + 4))] <- t
    
    # print(df[seq(ii - 5, ii + 5, 1),])
  }
  
  
  df <- df %>%
    mutate(
      pos_pulses = pos_pulses * pulse_effect,
      neg_pulses = neg_pulses * pulse_effect,
      pos_and_neg_pulses = pos_and_neg_pulses * pulse_effect
    ) %>%
    select(-pulse_effect) %>%
    filter(yday(date) >= 116 & yday(date) <= 278) %>%
   # mutate(across(c(3:6),  ~ .x + 1)) %>%
    mutate(year = year(date) - 1999,  # to adjust for simulation rather than real year
           doy = yday(date))
  
  # ggplot(df, aes(x = date, y = pos_and_neg_pulses)) +
  #   geom_line() +
  #   scale_x_date(limits = c(as.Date("2226-12-01"), as.Date("2227-12-01")))
  
  df <- df %>%
    filter(pos_and_neg_pulses != 0)
  
  pos_pulses <- df[, c(7, 8, 4)]
  neg_pulses <- df[, c(7, 8, 5)]
  pos_and_neg_pulses  <- df[, c(7, 8, 6)]
  
  filename <-
    paste("EcoEvoInputScenarios",
          "vers",
          as.character(s),
          sep = "_")
  
  write_csv(
    pos_pulses,
    paste(
      "Data/Scenarios/EcoEvoInputs/",
      filename,
      "_pos_pulses",
      ".csv",
      sep = ""
    )
  )
  write_csv(
    neg_pulses,
    paste(
      "Data/Scenarios/EcoEvoInputs/",
      filename,
      "_neg_pulses",
      ".csv",
      sep = ""
    )
  )
  write_csv(
    pos_and_neg_pulses,
    paste(
      "Data/Scenarios/EcoEvoInputs/",
      filename,
      "_pos_and_neg_pulses",
      ".csv",
      sep = ""
    )
  )
  
  rm(df)
  rm(pos_pulses)
  rm(neg_pulses)
  rm(pos_and_neg_pulses)
  rm(temp)
  
  print(s)
}


#### Making plots of results ####


file_path <- "./Data/Scenarios/EcoEvo/EcoEvoInputs/"

filenames <- list.files(file_path, pattern="*.csv", full.names=TRUE)

ex1 <- read_csv(filenames[1])
ex2 <- read_csv(filenames[2])
ex3 <- read_csv(filenames[3])

