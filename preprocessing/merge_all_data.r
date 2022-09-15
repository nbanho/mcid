#### Libraries ####

library(tidyverse)


#### Clean data ####

cases <- read.csv("data-clean/redcap.csv")
palas <- read.csv("data-clean/palas-class.csv")


#### Merge data ####

df <- left_join(cases %>% select(-class), 
                palas %>% select(-week,-day,-intervention) %>% mutate(location = as.character(location)), 
                by = c("location", "date")) %>%
  select(location, class, date, week, day, time, no_class, intervention, airfilter, maskmandate, everything())


#### Compute infection risk ###

#' parameters:
#' - co2ppm = observed CO2 concentration in the indoor air per minute (ppm)
#' - Co = CO2 concentration in outdoor air per minute (ppm)
#' - Ca = Co2 concentration in exhaled air (ppm)
#' - f: proportion of rebreathed air 
#' - n_room: number of people in room
#' - f0: rebreathed proportion from other people
#' - p: minute respiratory volume (8l per minute)
#' - rav: rebtreathed air volume 
#' - V: average volume of gas exhaled per person (0.13 l/s per person)
#' - G: indoor CO2 generation rate
#' - Q: ventilation rate
#' - vol: volume of the room

# assumptions see analysis plan for tb transmission risk
Co <- 410 # alternatively could use morning level (i.e. level when noone was in the room)
Ca <- 39000
p <- 8
V <- 0.13

df <- df %>%
  mutate(f = (co2ppm - Co) / Ca,
         n_room = n_class - n_tot_absent,
         f0 = f * ((n_room - 1) / n_room),
         rav = p * f0)


write.csv(df, file = "data-clean/merged-data.csv", row.names = F)
