#### Libraries ####

library(tidyverse)
library(lubridate)
library(reshape2)


#### Load files ####

# read file, select and name variables, create additional meta variables

load_file <- function(school) {
  
  df <- read_delim(paste0("data-raw/palas/", school, ".csv"), delim = ",")[ ,1:33][ ,-c(1,4,5)] %>%
    select(-`SO2 [ppm]`, -`NO2 [ppm]`, -`O3 [ppm]`, -`CO [ppm]`) %>%
    mutate(school = ifelse(school == "olten", "School 1", "School 2")) %>%
    select(school, everything()) %>%
    set_names(c("school", "date", "time",
                "pm1mugm", "pm25mugm", "pm4mugm", "pm10mugm", "pmtotmugm",
                "co2ppm", "vocppb", "vocmgm", 
                "cn1cm", "m1mum", "m2mum", "m3mum", 
                "x10dcnmum", "x16dcnmum", "x50dcnmum", "x84dcnmum", "x90dcnmum",
                "cn1m3", "pia1m3", "tc", "phpa", "rh", "qipalas", "infectionrisk")) %>%
      mutate(date = as.Date(date, format = "%d.%m.%y")) %>%
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
  mutate(intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air cleaner", "No intervention")),
         intervention = factor(intervention, levels = c("Mask mandate", "No intervention", "Air cleaner"))) 


#### Filter ####

lessons <- readRDS("data-clean/lesson-filter.rds") 

to_hhmm <- function(t) {
  h <- hour(t)
  if (nchar(h) == 1) {
    h <- paste0("0",h)
  }
  m <- floor(minute(t)/10) * 10
  if (nchar(m) == 1) {
    m <- "00"
  }
  hhmm <- paste0(h, ":", m)
}

df$hhmm <- sapply(df$time, to_hhmm) 
df <- df %>%
  left_join(lessons %>% mutate(date = as.Date(date)) %>% select(school, date, hhmm, lesson))
df_full <- df
df_filt <- df %>%
  filter(!no_school,
         lesson)

# filter times where class should be in room
# roomplan <- read_delim("data-raw/roomplan/roomplan.csv", delim = ";") %>%
#   mutate(start_min = hour(start) * 60 + minute(start),
#          end_min = hour(end) * 60 + minute(start)) 
# filt_df_list <- list()
# for (i in 1:nrow(roomplan)) {
#   filt_df_list[[i]] <- df %>%
#     mutate(time_min = hour(time) * 60 + minute(time)) %>%
#     filter(school == roomplan$school[i]) %>%
#     filter(weekday == roomplan$day[i]) %>%
#     # How many minutes before or after start of lecture? Currently: 0min 
#     # How many minutes before or after end of lecture? Currently: 0min
#     filter(between(time_min, roomplan$start_min[i], roomplan$end_min[i])) %>%
#     mutate(class = ifelse(roomplan$school[i] == "School 1",paste0("Study (", roomplan$class[i], ")"), "Study"))  
# }
# df_filt <- do.call(rbind, filt_df_list) %>%
#   dplyr::filter(!no_school) %>%
#   mutate(no_class = F) 
# 
# df_full <- df %>% 
#   left_join(df_filt %>% select(school, class, date, time, no_class)) %>%
#   complete(school, week, weekday, time) %>%
#   mutate(time_min = hour(time) * 60 + minute(time)) %>%
#   filter(time_min >= 7 * 60) %>%
#   filter(time_min <= 17 * 60 + 30) %>%
#   select(-time_min) %>%
#   mutate(no_class = ifelse(is.na(no_class), T, F),
#          class = ifelse(class == "Study (E3f)", "Study (A)",
#                         ifelse(class == "Study (B3d)", "Study (B)", class))) 



#### Outliers ####

plot_curves <- function(var = "co2ppm") {
  ggplot(mapping = aes(x = time, y = !! sym(var), color = factor(week))) +
    geom_line(data = df_full %>% 
                filter(lesson) %>%
                filter(!no_school) %>%
                complete(school, week, weekday, time), mapping = aes(color = factor(week)), size = 1 / cm(1)) +
    geom_line(data = df_full %>% 
                filter(!lesson) %>%
                complete(school, week, weekday, time), mapping = aes(color = factor(week)), alpha = .25, size = 1 / cm(1)) +
    facet_wrap(~school + weekday) 
}

# CO2

#' CO2 in the outdoor air is at least 400 based on measurements in a nearby area:
#' https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/txt/co2_hpb_surface-flask_1_ccgg_month.txt
#' However, we encounter CO2 levels even below 400 in the schools, so maybe near the schools the
#' CO2 levels were actually lower, or the measurements are inaccurate below 400.

co2ppm_out <- 400

df_full %>% filter(week == 6) %>% select(school, date, time, co2ppm) %>% ggplot(aes(x = time, color = school, y = co2ppm)) + facet_wrap(~date) + geom_point()
plot_curves()

#' The measurements during weeks without school/lessons are not consistent and
#' roughly within a range of 380 to 450.
#' The minimum in the filtered data yet is above the outdoor level.

df_filt %>%
  ggplot(aes(x = co2ppm, fill = school, color = school)) +
  geom_density(alpha = .5)

#' at the top the values are continuous --> probably no outliers 
#' --> keep even levels above 2,000


# Temperature

# hist(df_filt$tc)
# plot_curves("tc")

#' abnormal changes of more than 2 degrees Celsius within one minute
#' remove these changes

df_full <- df_full %>%
  group_by(school) %>%
  arrange(date, time) %>%
  mutate(delta_tc = tc - dplyr::lag(tc)) %>%
  ungroup() %>%
  mutate(tc2 = ifelse(!is.na(delta_tc) & abs(delta_tc) > 2, NA, tc))

plot_curves("tc2")

df_filt <- df_filt %>%
  left_join(df_full %>% select(school, date, time, tc2)) %>%
  select(-tc) %>%
  rename(tc = tc2)
df_full <- df_full %>% 
  select(-delta_tc, -tc) %>%
  rename(tc = tc2)


# Humidity

hist(df_filt$rh)
plot_curves("rh")

#' some almost vertical lines, i.e. sudden changes 
#' --> but only within unfiltered data

# df_full <- df_full %>%
#   group_by(school) %>%
#   arrange(date, time) %>%
#   mutate(delta_rh = rh - dplyr::lag(rh)) %>%
#   ungroup() %>%
#   mutate(rh2 = ifelse(!is.na(delta_rh) & abs(delta_rh) > 5, NA, rh))
# 
# plot_curves("rh2")
# 
# df_filt <- df_filt %>%
#   left_join(df_full %>% select(school, date, time, rh2)) %>%
#   select(-rh) %>%
#   rename(rh = rh2)
# df_full <- df_full %>% 
#   select(-delta_rh, -rh) %>%
#   rename(rh = rh2)


# particle number concentration

plot_curves("cn1cm")
hist(df_filt$cn1cm)


# particle matter concentration

#' note that there are two extreme spikes in large particles of sizes 4 and 10 
#' on Tuesday in week 5 and 7

plot_curves("pm1mugm")
hist(df_filt$pm1mugm)
plot_curves("pm25mugm")
hist(df_filt$pm25mugm)
plot_curves("pm4mugm")
hist(df_filt$pm4mugm)
plot_curves("pm10mugm")
hist(df_filt$pm10mugm)

#### Compute additional variables ###

# No. of students

absences <- read.csv("data-clean/redcap-absences.csv")
redcap_full <- readRDS("data-clean/redcap-full-range.rds")

no_stud <- absences %>% 
  left_join(redcap_full %>% 
              select(school, class, no_school)) %>%
  filter(!no_school) %>%
  filter(class != "Control") %>%
  mutate(n = n_class - n_absent + 1) %>%
  group_by(school, date) %>%
  summarize(n = mean(n)) %>%
  ungroup() %>%
  mutate(date = as.Date(date))

df_filt <- df_filt %>%
  left_join(no_stud, by = c("school", "date")) 

#' Ventilation and air exchange rate
#' Ventilation rate is measured in 1/h based on CO2 levels as
#' Q = 3600 * G / (co2ppm - co2ppm_out),
#' where:
#' - G is the CO2 generation rate
#' - co2ppm is the measured indoor CO2 level
#' - co2ppm_out is the CO2 level in outdoor air
#' G is taken from Persily et al. for age group 11 to 16, where it is ~0.004

G <- 0.004

#' The air exchange rate takes into account the number of people and volume of the room
#' and is computed as
#' AER = Q * n / V,
#' where:
#' - n is the number of people in the room and
#' - V is the volume of the room in m3

df_filt$V <- ifelse(df_filt$school == "School 1", 207, 233)

#' Rebreathed air volume
#' Measured in l/min per person as
#' RAV = p * f0,
#' where 
#' - p is the respiratory volume  (8l per minute)
#' - f0 is the rebreathed air fraction from other people
#' The rebreathed air fraction is computed from CO2 levels as
#' f = (co2ppm - co2ppm_out) / co2ppm_ex,
#' where in addition to before:
#' - co2ppm_ex is the Co2 concentration in exhaled air (ppm)
#' The CO2 in exhaled air is taken from Emmerich et al.
#' The rebreathed air fraction from other people then is
#' f0 = f * (n - 1) / n,

p <- 8
co2ppm_ex <- 39000

# Absolute humidity
#' Supposed to be a better predictor of viral survival.
#' https://royalsocietypublishing.org/doi/10.1098/rsif.2018.0298
#' Use formula 2.1 and compute it based on temperature and relative humidity.

df_filt <- df_filt %>%
  mutate(f = (co2ppm - co2ppm_out) / co2ppm_ex, 
         f0 = f * (n - 1) / n,
         rav = p * f0,
         Q = 3600 * G / ((co2ppm - co2ppm_out) / 1e3), 
         aer = Q * n / V,
         ah = 1322.7 * exp(17.625 * tc / (tc + 243.04)) * (rh / 100) / (tc + 273.15)) %>%
  select(school, date, week, weekday, time, intervention, airfilter, maskmandate, n, everything())

#### Variable Subset ####

df_filt <- df_filt %>%
  select(school, date, week, weekday, time, intervention, maskmandate, airfilter, 
         n, co2ppm, tc, rh, V, f, f0, rav, Q, aer, ah,
         cn1cm, pm1mugm, pm25mugm, pm4mugm, pm10mugm)

write.csv(df_filt, "data-clean/palas.csv", row.names = F)
