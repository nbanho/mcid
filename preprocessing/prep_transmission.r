library(tidyverse)

#### Read data ####

df <- read.csv("data-raw/transmission/CHE-estimates.csv")

#### Filter data ####

df_SO <- df %>%
  filter(country == "Switzerland") %>%
  filter(region == "SO") %>%
  filter(data_type == "Confirmed cases") %>%
  filter(estimate_type == "Cori_slidingWindow") %>%
  select(date, matches("median")) 

df_SO %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  select(n) %>%
  unlist() %>%
  summary

#### Computations ####

df_SO <- df_SO %>%
  # compute standard deviation to form prior in analysis
  mutate(median_R_SD = (median_R_highHPD - median_R_lowHPD) / (2 * qnorm(0.975))) %>%
  select(date, median_R_mean, median_R_SD) 


#### Save data ###

write.csv(df_SO, "data-clean/r-estimates.csv", row.names = F)
