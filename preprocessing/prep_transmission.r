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
  

#### Save data ###

write.csv(df_SO, "data-clean/r-estimates.csv", row.names = F)


#### Check tests ####

start_week <- as.numeric(paste0("20210", week(as.Date("2021-01-24"))))
end_week <- as.numeric(paste0("2021", week(as.Date("2021-03-18"))))
date <- seq.Date(as.Date("2021-01-24"), as.Date("2021-03-18"), by = "1 week") %m+% days(3)

tests <- read.csv("data-raw/transmission/COVID19Test_geoRegion_w.csv") %>%
  filter(geoRegion == "SO",
         datum <= end_week,
         datum >= start_week) %>%
  mutate(date = date) 


tests_pl <- tests %>%
  ggplot(aes(x = date, y = anteil_pos)) +
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = as.Date("2021-02-14"), linetype = "School 1", color = "End: Mask mandate")) +
  geom_vline(aes(xintercept = as.Date("2021-02-21"), linetype = "School 2", color = "End: Mask mandate")) +
  geom_vline(aes(xintercept = as.Date("2021-03-07") %m+% days(1), linetype = "School 1", color = "Start: Air cleaner")) +
  geom_vline(aes(xintercept = as.Date("2021-02-28") %m+% days(1), linetype = "School 2", color = "Start: Air cleaner")) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values = cbPalette, breaks = c("End: Mask mandate", "Start: Air cleaner")) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  labs(y = "Share positive tests (%)") +
  theme_bw2() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(),
        legend.position = "bottom", plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

tests_pl
save_plot(tests_pl, pdf_file = "results/transmssion-share-positive-tests.png", w = 12, h = 6)
