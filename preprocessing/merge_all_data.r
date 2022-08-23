#### Libraries ####

library(tidyverse)


#### Clean data ####

cases <- read.csv("data-clean/redcap.csv")
palas <- read.csv("data-clean/palas-daily-average.csv")


#### Merge data ####

df <- left_join(palas, cases %>% select(-week), by = c("location", "date")) 
