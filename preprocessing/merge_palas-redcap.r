#### Libraries ####

library(tidyverse)


#### Clean data ####

cases <- read.csv("data-clean/redcap-cases.csv")
absences <- read.csv("data-clean/redcap-absences.csv")
redcap_full <- readRDS("data-clean/redcap-full-range.rds")
palas <- read.csv("data-clean/palas.csv")


#### Merge Palas minute data ####

PM <- left_join(palas %>%
                  filter(!no_class) %>%
                  filter(!no_school), 
                absences %>% 
                  left_join(redcap_full %>% 
                              select(school, class, no_school)) %>%
                  filter(!no_school) %>%
                  filter(class != "Control") %>%
                  select(-no_school), 
                by = c("school", "class", "date")) 


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
Co <- 420 # 405 - 430 in the nearest area last year (https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/txt/co2_hpb_surface-flask_1_ccgg_month.txt)
Ca <- 39000 # Rundick and Milton, and Emmerich et al
p <- 8 # Emmeric et al

PM <- PM %>%
  mutate(f = (co2ppm - Co) / Ca,
         n_room = n_class - n_absent,
         f0 = f * ((n_room - 1) / n_room),
         rav = p * f0) %>%
  select(school, class, school_class, date, week, weekday, time, intervention, airfilter, maskmandate, n_class, n_absent, n_room, everything())


write.csv(PM, file = "data-clean/left-palas-redcap.csv", row.names = F)
