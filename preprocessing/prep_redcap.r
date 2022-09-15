#### Libraries ####

library(tidyverse)
library(lubridate)
library(reshape2)


#### Load data ####

df <- read.delim("data-raw/redcap/suspected-cases.csv", sep = ";") %>%
  rename(location = `Data.Access.Group`,
         date_start = `Datum_abwesend`,
         date_end = `Datum_zurück`) %>%
  mutate(location = tolower(ifelse(grepl("Olten", location), "Olten", location)),
         control = ifelse(grepl("Studienklasse", `Record.ID`), 0, 1),
         class = stringi::stri_extract(`Record.ID`, regex = "\\w\\d\\w"),
         date_start = as.Date(date_start, format = "%d.%m.%Y"),
         date_end = as.Date(date_end, format = "%d.%m.%Y"),
         # special case: teachers --> set date_start = date_end
         date_end = ifelse(is.na(date_end), as.character(date_start + days(1)), as.character(date_end)),
         date_end = as.Date(date_end),
         n_class = ifelse(!is.na(class) & class == "B3d", 14, 
                          ifelse(!is.na(class) & class == "E3f", 24,
                                 ifelse(is.na(class) & location == "trimbach", 18, 14))),
         # add 1 assuming there is always a teacher in class
         n_class = n_class + 1) %>%
  # special case: 0 missing day for one student --> remove entry
  filter(`Anzahl.Tage` != 0)
  

#### Filter data ####

# Filter total

df_tot <- df %>%
  select(location, control, class, n_class) %>%
  group_by(location, control, class) %>%
  slice(1) %>%
  ungroup()

# Filter absent

df_absent <- df  %>%
  select(location, control, class, date_start, date_end) %>%
  mutate(date = map2(as.character(date_start), as.character(date_end), 
                     function(x,y) seq(as.Date(x), to = as.Date(y) - days(1), by = "1 day"))) %>%
  select(location, control, class, date) %>%
  unnest() %>%
  group_by(location, control, class, date) %>%
  summarize(n_absent = n()) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) 
  

# Filter confirmed cases

df_confirmed <- df %>% 
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.` 
         == "Positiv") %>%
  select(location, control, class, date_start, date_end,
         `Der.Schüler...die.Schülerin.war.mit.den.oben.genannten.Symptomen.`,
         `Seit.wann.hat.der.Schüler...die.Schülerin.Symptome.`,
         `Falls.eine.mikrobiologische.Untersuchung.durchgeführt.wurde..wann.wurde.der.Test.gemacht.`) %>%
  set_names(c("location", "control", "class", "date_start", "date_end", "where_symptoms", "date_symptoms", "date_test")) %>%
  mutate(date_symptoms = as.Date(date_symptoms, format = "%d.%m.%Y"),
         date_test = as.Date(date_test, format = "%d.%m.%Y")) %>%
  mutate(date_symptoms = as.Date(ifelse(!is.na(date_symptoms), as.character(date_symptoms), 
                                as.character(min(date_start, date_test)))))
  

# Filter suspected cases

df_suspected <- df %>%
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.`
         == "") %>%
  filter(!grepl("Schnuppern", `Falls.anderes`)) %>%
  filter(!grepl("Persönliches", `Falls.anderes`)) %>%
  filter(!grepl("Unfall", `Falls.anderes`)) %>%
  filter(!grepl("Prüfungen", `Falls.anderes`)) %>%
  filter(!grepl("Berufsberatung", `Falls.anderes`)) %>%
  filter(!grepl("Magendarm", `Falls.anderes`)) %>%
  filter(!grepl("50% Krankgeschrieben", `Falls.anderes`)) 

df_suspected <- df_suspected %>%
  select(location, control, class, date_start, date_end, matches("Welche.Symptome.hat.der")) %>%
  set_names(c("location", "control", "class", "date_start", "date_end",
              "fever", "chills", "limb pain", "loss of taste", "loss of smell",
              "fatigue", "cough", "cold", "diarrhea", "sore throat", "headache",
              "breathing difficulties", "stomach",
              "other", "none")) %>%
  select(-other, -none) %>%
  mutate(across(-c(location, control, class, date_start, date_end), ~ ifelse(.x == "Checked", T, F))) %>%
  melt(c("location", "control", "class", "date_start", "date_end")) %>%
  group_by(location, control, class, date_start, date_end) %>%
  summarise(suspected = ifelse(any(value), "symptomatic", "unknown")) %>%
  ungroup()


#### Combine data ####

df_cases <- df_confirmed %>%
  select(location, control, class, date_start) %>%
  mutate(suspected = "confirmed") %>%
  rbind(df_confirmed %>%
          select(location, control, class, date_symptoms) %>%
          rename(date_start = date_symptoms) %>%
          mutate(suspected = "confirmed_by_onset")) %>%
  rbind(df_confirmed %>%
          select(location, control, class, date_end) %>%
          rename(date_start = date_end) %>%
          mutate(suspected = "recovered_from_confirmed")) %>%
  rbind(df_suspected %>%
          select(location, control, class, date_start, suspected)) %>%
  rbind(df_suspected %>%
          select(location, control, class, date_end, suspected) %>%
          rename(date_start = date_end) %>%
          mutate(suspected = paste0("recovered_from_", suspected))) %>%
  group_by(location, control, class, suspected, date_start) %>%
  summarize(new_cases = n()) %>%
  ungroup() %>%
  dcast(date_start + location + control + class ~ suspected) %>%
  mutate_at(vars(confirmed, confirmed_by_onset, symptomatic, unknown,
                 recovered_from_confirmed, recovered_from_symptomatic, recovered_from_unknown), 
            function(x) ifelse(is.na(x), 0, x))

df_cases <- data.frame(date_start = rep(seq.Date(min(df$date_start), 
                                                 max(df$date_start) + 2, # add 2 days for weekend
                                                 by = "1 day"), 5)) %>%
  mutate(location = rep(c("olten", "olten", "olten", "trimbach", "trimbach"), each = nrow(.) / 5),
         control = rep(c(0, 0, 1, 1, 0), each = nrow(.) / 5),
         class = rep(c("B3d", "E3f", NA, NA, NA), each = nrow(.) / 5)) %>%
  left_join(df_cases) %>%
  rename(date = date_start) %>%
  group_by(location, control, class) %>%
  arrange(date) %>%
  mutate_at(vars(confirmed, confirmed_by_onset, unknown, symptomatic,
                 recovered_from_confirmed, recovered_from_symptomatic, recovered_from_unknown), 
            function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(across(c(confirmed, confirmed_by_onset, unknown, symptomatic,
                  recovered_from_confirmed, recovered_from_symptomatic, recovered_from_unknown), 
                cumsum, .names = 'cum_{col}')) %>%
  ungroup() %>%
  mutate(week = as.numeric(strftime(date, format = "%V")))  %>%
  mutate(day = weekdays(date)) %>%
  mutate(maskmandate = ifelse(location == "olten" & week < 8, 1, ifelse(location == "trimbach" & week < 9, 1, 0)),
         airfilter = ifelse(location == "olten" & week >= 11, 1, ifelse(location == "trimbach" & week >= 10 & week < 12, 1, 0)),
         no_school = ifelse(location == "olten" & week %in% c(6,7), T, ifelse(location == "trimbach" & week %in% c(6,12), T, F))) %>%
  mutate(intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air filter", "No")),
         intervention = factor(intervention, levels = c("Mask mandate", "No", "Air filter")))  %>%
  left_join(df_absent) %>%
  left_join(df_tot) %>%
  mutate(n_absent = ifelse(!is.na(n_absent), n_absent, 0)) %>%
  rename(n_tot_absent = n_absent,
         new_confirmed = confirmed,
         new_confirmed_by_onset = confirmed_by_onset,
         new_unknown = unknown,
         new_symptomatic = symptomatic,
         new_recovered_from_confirmed = recovered_from_confirmed,
         new_recovered_from_symptomatic = recovered_from_symptomatic,
         new_recovered_from_unknown = recovered_from_unknown) %>%
  mutate(n_absent_other = n_tot_absent - new_confirmed - new_unknown - new_symptomatic) %>%
  mutate(class_comb = paste0(location, ": ", ifelse(control == 1, "control", "case"), 
                             ifelse(!is.na(class), paste0("(", class, ")"), "")),
         weekday = weekdays(date),
         weekend = ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0)) %>%
  select(location, control, class, class_comb, date, week, day, weekday, weekend, 
         intervention, airfilter, maskmandate, no_school,
         n_class, n_tot_absent, n_absent_other, 
         new_confirmed, new_confirmed_by_onset, new_unknown, new_symptomatic,
         cum_confirmed, cum_confirmed_by_onset, cum_unknown, cum_symptomatic,
         new_recovered_from_confirmed, new_recovered_from_symptomatic, new_recovered_from_unknown,
         cum_recovered_from_confirmed, cum_recovered_from_symptomatic, cum_recovered_from_unknown)


write.csv(df_cases, "data-clean/redcap.csv", row.names = F)
