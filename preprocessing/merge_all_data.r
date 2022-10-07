#### Libraries ####

library(tidyverse)


#### Clean data ####

cases <- read.csv("data-clean/redcap.csv")
palas <- read.csv("data-clean/palas.csv")


#### Merge Palas minute data ####

PM <- left_join(palas %>%
                  filter(!no_class) %>%
                  filter(!no_school), 
                cases %>% 
                  filter(is_study_class) %>%
                  filter(!no_school) %>%
                  select("school", "class", "date", "n_class", "n_tot_absent"), 
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
         n_room = n_class - n_tot_absent,
         f0 = f * ((n_room - 1) / n_room),
         rav = p * f0) %>%
  select(school, class, date, week, weekday, time, no_class, intervention, airfilter, maskmandate, n_class, n_tot_absent, n_room, everything())


write.csv(PM, file = "data-clean/left-palas-redcap.csv", row.names = F)
