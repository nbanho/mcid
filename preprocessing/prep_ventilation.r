#### Libraries #### 

library(tidyverse)


#### Data ####

vent <- read.csv("data-raw/ventilation/ventilation-duration.csv")
lesson <- readRDS("data-clean/lesson-filter.rds")


#### Preprocessing ####

# aggregate ventilation data

vent_school <- vent %>%
  mutate(date = as.character(as.Date(date, format = "%d.%m.%y")),
         school = ifelse(school == "Olten", "School 1", "School 2")) %>%
  rename(duration = `duration.min.`) %>%
  group_by(school, date) %>%
  summarize(duration = sum(duration, na.rm = T)) %>%
  ungroup()

# compute total lesson time

lesson_school <- lesson %>%
  group_by(school, date) %>%
  summarize(lesson = sum(lesson) * 10) %>%
  ungroup()

# merge

vent_lesson <- left_join(vent_school, lesson_school, by = c("school", "date")) %>%
  filter(lesson > 0) %>%
  mutate(prop_vent = duration / lesson,
         date = as.Date(date),
         week = as.numeric(strftime(date, format = "%V")),
         weekday = weekdays(date)) %>%
  mutate(maskmandate = ifelse(school == "School 1" & week < 8, 1, 
                              ifelse(school == "School 2" & week < 9, 1, 0)),
         airfilter = ifelse(school == "School 1" & week >= 11, 1, 
                            ifelse(school == "School 2" & week >= 10 & week < 12, 1, 0)),
         no_school = ifelse(school == "School 1" & week %in% c(6,7), T, 
                            ifelse(school == "School 2" & week %in% c(6,12), T, F))) %>%
  mutate(intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air cleaner", "No intervention")),
         intervention = factor(intervention, levels = c("Mask mandate", "No intervention", "Air cleaner"))) %>%
  select(school, date, week, weekday, intervention, duration, lesson, prop_vent)


#### Data check ####

hist(vent_lesson$prop_vent)


#### Save ####

saveRDS(vent_lesson, "data-clean/ventilation-duration.rds")
