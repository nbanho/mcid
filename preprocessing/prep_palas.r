#### Libraries ####

library(tidyverse)
library(lubridate)
library(reshape2)


#### Load files ####

# read file, select and name variables, create additional meta variables

load_file <- function(school) {
  
  df <- read_delim(paste0("data-raw/palas/", school, ".csv"), delim = ";")[ ,1:33][ ,-c(1,4,5)] %>%
    select(-`SO2 [ppm]`, -`NO2 [ppm]`, -`O3 [ppm]`, -`CO [ppm]`) %>%
    mutate(school = ifelse(school == "olten", "School 1", "School 2")) %>%
    select(school, everything()) %>%
    set_names(c("school", "date", "time",
                "pm1mugm", "pm25mugm", "pm4mugm", "pm10mugm", "pmtotmugm",
                "co2ppm", "vocppb", "vocmgm", 
                "cn1cm", "m1mum", "m2mum", "m3mum", 
                "x10dcnmum", "x16dcnmum", "x50dcnmum", "x84dcnmum", "x90dcnmum",
                "cn1m3", "pia1m3", "tc", "phpa", "rh", "qipalas", "infectionrisk")) %>%
      mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
      mutate(weekday = weekdays(date)) %>%
      mutate(week = as.numeric(strftime(date, format = "%V"))) %>%
      filter(!(weekday %in% c("Saturday", "Sunday"))) %>%
      mutate(weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))) %>%
    select(school, date, week, weekday, time, everything())
  
  return(df)
}


olten <- load_file("olten") 

trimbach <- load_file("trimbach") 


# combine and add intervention info 
df <- rbind(olten, trimbach) %>%
  mutate(maskmandate = ifelse(school == "School 1" & week < 8, 1, 
                              ifelse(school == "School 2" & week < 9, 1, 0)),
         airfilter = ifelse(school == "School 1" & week >= 11, 1, 
                            ifelse(school == "School 2" & week >= 10 & week < 12, 1, 0)),
         no_school = ifelse(school == "School 1" & week %in% c(6,7), T, 
                            ifelse(school == "School 2" & week %in% c(6,12), T, F))) %>%
  mutate(intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air filter", "None")),
         intervention = factor(intervention, levels = c("Mask mandate", "None", "Air filter"))) 


#### Filter ####

# filter times where class should be in room
roomplan <- read_delim("data-raw/roomplan/roomplan.csv", delim = ";") %>%
  mutate(start_min = hour(start) * 60 + minute(start),
         end_min = hour(end) * 60 + minute(start)) 
filt_df_list <- list()
for (i in 1:nrow(roomplan)) {
  filt_df_list[[i]] <- df %>%
    mutate(time_min = hour(time) * 60 + minute(time)) %>%
    filter(school == roomplan$school[i]) %>%
    filter(weekday == roomplan$day[i]) %>%
    # TODO: How many minutes before or after start of lecture? Currently: 0min 
    # TODO: How many minutes before or after end of lecture? Currently: 0min
    filter(between(time_min, roomplan$start_min[i], roomplan$end_min[i])) %>%
    mutate(class = ifelse(roomplan$school[i] == "School 1",paste0("Study (", roomplan$class[i], ")"), "Study"))  
}
df_filt <- do.call(rbind, filt_df_list) %>%
  mutate(no_class = F) 

df_full <- df %>% 
  left_join(df_filt %>% select(school, class, date, time, no_class)) %>%
  complete(school, week, weekday, time) %>%
  mutate(time_min = hour(time) * 60 + minute(time)) %>%
  filter(time_min >= 7 * 60) %>%
  filter(time_min <= 17 * 60 + 30) %>%
  select(-time_min) %>%
  mutate(no_class = ifelse(is.na(no_class), T, F),
         class = ifelse(class == "Study (E3f)", "Study (A)",
                        ifelse(class == "Study (B3d)", "Study (B)", class))) 


ggplot(mapping = aes(x = time, y = co2ppm, color = factor(week))) +
  geom_line(data = df_full %>% 
              filter(!no_class) %>%
              complete(school, week, weekday, time), mapping = aes(color = factor(week)), size = 1 / cm(1)) +
  geom_line(data = df_full %>% 
              filter(no_class) %>%
              complete(school, week, weekday, time), mapping = aes(color = factor(week)), alpha = .25, size = 1 / cm(1)) +
  facet_wrap(~school + weekday) 


#### Variable Subset ####

# TODO: select variables for further analysis?

df_full <- df_full %>%
  select(school, class, date, week, weekday, time, no_school, no_class, maskmandate, airfilter, intervention, everything())

write.csv(df_full, "data-clean/palas.csv", row.names = F)
