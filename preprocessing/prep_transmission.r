library(tidyverse)
source("utils/plotting.r")

#### Read data ####

# reproduction number
df <- read.csv("data-raw/transmission/CHE-estimates.csv")

# covid cases
cases <- read.csv("data-raw/transmission/COVID19Cases_geoRegion.csv")

#### Filter data ####

df_SO <- df %>%
  filter(country == "Switzerland") %>%
  filter(region == "SO") %>%
  filter(data_type == "Confirmed cases") %>%
  filter(estimate_type == "Cori_slidingWindow") %>%
  select(date, matches("median")) 

cases_SO <- cases %>%
  filter(geoRegion == "SO") %>%
  select(datum, mean7d) %>%
  rename(date = datum,
         cases_mean7d = mean7d) %>%
  mutate(date = as.Date(date))

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


#### Plots ###

cases_and_R <- cases_SO %>%
  left_join(df_SO %>% mutate(date = as.Date(date)), by = "date") %>%
  filter(date >= "2022-01-24",
         date <= "2022-03-26") 

cases_and_R %>%
  ggplot(aes(x = date, y = cases_mean7d)) +
  geom_line() +
  labs(y - )
  scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b-%d") +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
  theme_bw2()
  

#### Save data ###

write.csv(df_SO, "data-clean/r-estimates.csv", row.names = F)
