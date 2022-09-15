#### Libraries ####

library(tidyverse)
library(lubridate)
library(reshape2)


#### Load files ####

# read file, select and name variables, create additional meta variables

load_file <- function(school) {
  
  df <- read_delim(paste0("data-raw/palas/", school, ".csv"), delim = ";")[ ,1:33][ ,-c(1,4,5)] %>%
    select(-`SO2 [ppm]`, -`NO2 [ppm]`, -`O3 [ppm]`, -`CO [ppm]`) %>%
    mutate(location = school) %>%
    select(location, everything()) %>%
    set_names(c("location", "date", "time",
                "pm1mugm", "pm25mugm", "pm4mugm", "pm10mugm", "pmtotmugm",
                "co2ppm", "vocppb", "vocmgm", 
                "cn1cm", "m1mum", "m2mum", "m3mum", 
                "x10dcnmum", "x16dcnmum", "x50dcnmum", "x84dcnmum", "x90dcnmum",
                "cn1m3", "pia1m3", "tc", "phpa", "rh", "qipalas", "infectionrisk")) %>%
      mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
      mutate(day = weekdays(date)) %>%
      mutate(week = as.numeric(strftime(date, format = "%V"))) %>%
      filter(!(day %in% c("Saturday", "Sunday"))) %>%
      mutate(day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))) %>%
    select(location, date, week, day, time, everything())
  
  return(df)
}


olten <- load_file("olten") %>%
  filter(!(week %in% c(6, 7))) %>% # filter vacation 
  mutate(maskmandate = ifelse(week < 8, 1, 0),
         airfilter = ifelse(week >= 11, 1, 0))

trimbach <- load_file("trimbach") %>%
  filter(!(week %in% c(6)))  %>% # filter vacation 
  mutate(maskmandate = ifelse(week < 9, 1, 0),
         airfilter = ifelse(week >= 10, 1, 0))


# combine and add intervention info 
df <- rbind(olten, trimbach) %>%
  mutate(intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air filter", "No")),
         intervention = factor(intervention, levels = c("Mask mandate", "No", "Air filter"))) %>%
  select(-maskmandate,-airfilter)


#### Filter ####

# filter times where class should be in room
roomplan <- read_delim("data-raw/roomplan/roomplan.csv", delim = ";") %>%
  mutate(start_min = hour(start) * 60 + minute(start),
         end_min = hour(end) * 60 + minute(start)) 
filt_df_list <- list()
for (i in 1:nrow(roomplan)) {
  filt_df_list[[i]] <- df %>%
    mutate(time_min = hour(time) * 60 + minute(time)) %>%
    filter(location == tolower(roomplan$location[i])) %>%
    filter(day == roomplan$day[i]) %>%
    # TODO: How many minutes before or after start of lecture? Currently: 0min 
    # TODO: How many minutes before or after end of lecture? Currently: 0min
    filter(between(time_min, roomplan$start_min[i], roomplan$end_min[i])) %>%
    mutate(class = roomplan$class[i]) 
}
df_filt <- do.call(rbind, filt_df_list) %>%
  mutate(no_class = F) 

df <- df %>% 
  left_join(df_filt %>% select(location, class, date, time, no_class)) %>%
  mutate(no_class = ifelse(is.na(no_class), T, F)) %>%
  complete(location, week, day, time) %>%
  mutate(time_min = hour(time) * 60 + minute(time)) %>%
  filter(time_min >= 7 * 60) %>%
  filter(time_min <= 17 * 60 + 30) %>%
  select(-time_min) 


ggplot(mapping = aes(x = time, y = co2ppm, color = factor(week))) +
  geom_line(data = df %>% 
              filter(!no_class) %>%
              complete(location, week, day, time), mapping = aes(color = factor(week)), size = 1 / cm(1)) +
  geom_line(data = df %>% 
              filter(no_class) %>%
              complete(location, week, day, time), mapping = aes(color = factor(week)), alpha = .25, size = 1 / cm(1)) +
  facet_wrap(~location + day) 


#### Variable Subset ####

# TODO: select variables for further analysis


write.csv(df, "data-clean/palas-all.csv", row.names = F)
write.csv(df %>% filter(!no_class), "data-clean/palas-class.csv", row.names = F)


#### Daily ####

df_class_daily <- df %>%
  filter(!no_class) %>%
  select(-no_class) %>%
  group_by(location, week, day, date, intervention) %>%
  summarize_all(mean, na.rm = T) %>%
  ungroup()

write.csv(df_class_daily, "data-clean/palas-class-daily-average.csv", row.names = F)
