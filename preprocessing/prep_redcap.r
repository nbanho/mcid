#### Libraries ####

library(tidyverse)
library(lubridate)
library(reshape2)


#### Load data ####

df <- read.delim("data-raw/redcap/suspected-cases.csv", sep = ";") %>%
  rename(school = `Data.Access.Group`,
         date_start = `Datum_abwesend`,
         date_end = `Datum_zurück`) %>%
  # Set school 1 to Olten
  mutate(school = ifelse(grepl("Olten", school), "School 1", "School 2"),
         is_study_class = ifelse(grepl("Studienklasse", `Record.ID`), T, F),
         classlabel = stringi::stri_extract(`Record.ID`, regex = "\\w\\d\\w"),
         class = ifelse(school == "School 1", ifelse(is_study_class, paste0("Study (", classlabel, ")"), "Control"), 
                        ifelse(is_study_class, "Study", "Control")),
         date_start = as.Date(date_start, format = "%d.%m.%Y"),
         date_end = as.Date(date_end, format = "%d.%m.%Y"),
         # special case: teachers --> set date_start = date_end
         date_end = ifelse(is.na(date_end), as.character(date_start + days(1)), as.character(date_end)),
         date_end = as.Date(date_end),
         n_class = ifelse(class == "Study (B3d)", 14, 
                          ifelse(class == "Study (E3f)", 24,
                                 ifelse(school == "School 1", 14, 
                                        ifelse(school == "School 2" & class == "Study", 20, 18))))) %>%
         # TODO: Should I add 1 assuming there is always a teacher in class or do that later during analysis?) %>%
  # special case: 0 missing day for one student --> remove entry
  filter(`Anzahl.Tage` != 0)
  

#### Filter data ####

# Filter total

df_tot <- df %>%
  select(school, class, n_class) %>%
  group_by(school, class) %>%
  slice(1) %>%
  ungroup()

# Filter absent

df_absent <- df  %>%
  select(school, is_study_class, class, date_start, date_end) %>%
  mutate(date = map2(as.character(date_start), as.character(date_end), 
                     function(x,y) seq(as.Date(x), to = as.Date(y) - days(1), by = "1 day"))) %>%
  select(school, is_study_class, class, date) %>%
  unnest() %>%
  group_by(school, is_study_class, class, date) %>%
  summarize(n_absent = n()) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) 
  

# Filter confirmed cases

df_confirmed <- df %>% 
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.` 
         == "Positiv") %>%
  select(school, is_study_class, class, date_start, date_end,
         `Der.Schüler...die.Schülerin.war.mit.den.oben.genannten.Symptomen.`,
         `Seit.wann.hat.der.Schüler...die.Schülerin.Symptome.`,
         `Falls.eine.mikrobiologische.Untersuchung.durchgeführt.wurde..wann.wurde.der.Test.gemacht.`) %>%
  set_names(c("school", "is_study_class", "class", "date_start", "date_end", "where_symptoms", "date_symptoms", "date_test")) %>%
  mutate(date_symptoms = as.Date(date_symptoms, format = "%d.%m.%Y"),
         date_test = as.Date(date_test, format = "%d.%m.%Y")) %>%
  mutate(date_symptoms = as.Date(ifelse(!is.na(date_symptoms), as.character(date_symptoms), 
                                as.character(pmin(date_start, date_test)))))
  

# Filter suspected cases

df_suspected <- df %>%
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.`
         == "" ) %>%
  filter(!grepl("Schnuppern", `Falls.anderes`)) %>%
  filter(!grepl("Persönliches", `Falls.anderes`)) %>%
  filter(!grepl("Unfall", `Falls.anderes`)) %>%
  filter(!grepl("Prüfungen", `Falls.anderes`)) %>%
  filter(!grepl("Berufsberatung", `Falls.anderes`)) %>%
  filter(!grepl("Magendarm", `Falls.anderes`)) %>%
  filter(!grepl("50% Krankgeschrieben", `Falls.anderes`)) 

df_suspected <- df_suspected %>%
  select(school, is_study_class, class, date_start, date_end, `Grund.für.die.Absenz....choice.Isolation.`, matches("Welche.Symptome.hat.der")) %>%
  set_names(c("school", "is_study_class", "class", "date_start", "date_end",
              "isolation",
              "fever", "chills", "limb pain", "loss of taste", "loss of smell",
              "fatigue", "cough", "cold", "diarrhea", "sore throat", "headache",
              "breathing difficulties", "stomach",
              "other", "none")) %>%
  select(-other, -none) %>%
  mutate(across(-c(school, is_study_class, class, date_start, date_end), ~ ifelse(.x == "Checked", T, F))) %>%
  melt(c("school", "is_study_class", "class", "date_start", "date_end", "isolation")) %>%
  group_by(school, is_study_class, class, date_start, date_end, isolation) %>%
  summarise(suspected = ifelse(any(value), "symptomatic", "unknown")) %>%
  ungroup() %>%
  # note here that isolated refers to isolated but unknown whether symptomatic
  mutate(suspected = ifelse(suspected == "symptomatic", "symptomatic", ifelse(isolation, "isolated", "unknown"))) 


#### Combine data ####

df_cases <- df_confirmed %>%
  select(school, is_study_class, class, date_start) %>%
  mutate(suspected = "confirmed") %>%
  rbind(df_confirmed %>%
          select(school, is_study_class, class, date_symptoms) %>%
          rename(date_start = date_symptoms) %>%
          mutate(suspected = "confirmed_by_onset")) %>%
  rbind(df_confirmed %>%
          select(school, is_study_class, class, date_end) %>%
          rename(date_start = date_end) %>%
          mutate(suspected = "recovered_from_confirmed")) %>%
  rbind(df_suspected %>%
          select(school, is_study_class, class, date_start, suspected)) %>%
  rbind(df_suspected %>%
          select(school, is_study_class, class, date_end, suspected) %>%
          rename(date_start = date_end) %>%
          mutate(suspected = paste0("recovered_from_", suspected))) %>%
  group_by(school, is_study_class, class, suspected, date_start) %>%
  summarize(new_cases = n()) %>%
  ungroup() %>%
  dcast(date_start + school + is_study_class + class ~ suspected) %>%
  mutate_at(vars(confirmed, confirmed_by_onset, symptomatic, isolated, unknown,
                 recovered_from_confirmed, recovered_from_symptomatic, recovered_from_isolated, recovered_from_unknown), 
            function(x) ifelse(is.na(x), 0, x))

df_cases <- data.frame(date_start = rep(seq.Date(min(df$date_start), 
                                                 max(df$date_start) + 2, # add 2 days for weekend
                                                 by = "1 day"), 5)) %>%
  mutate(school = rep(c("School 1", "School 1", "School 1", "School 2", "School 2"), each = nrow(.) / 5),
         is_study_class = rep(c(T, T, F, T, F), each = nrow(.) / 5),
         class = rep(c("Study (B3d)", "Study (E3f)", "Control", "Study", "Control"), each = nrow(.) / 5)) %>%
  left_join(df_cases) %>%
  rename(date = date_start) %>%
  group_by(school, is_study_class, class) %>%
  arrange(date) %>%
  mutate_at(vars(confirmed, confirmed_by_onset, symptomatic, isolated, unknown,
                 recovered_from_confirmed, recovered_from_symptomatic, recovered_from_isolated, recovered_from_unknown), 
            function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(across(c(confirmed, confirmed_by_onset, symptomatic, isolated, unknown,
                  recovered_from_confirmed, recovered_from_symptomatic, recovered_from_isolated, recovered_from_unknown), 
                cumsum, .names = 'cum_{col}')) %>%
  ungroup() %>%
  mutate(week = as.numeric(strftime(date, format = "%V")))  %>%
  mutate(day = weekdays(date)) %>%
  mutate(maskmandate = ifelse(school == "School 1" & week < 8, 1, 
                              ifelse(school == "School 2" & week < 9, 1, 0)),
         airfilter = ifelse(school == "School 1" & week >= 11, 1, 
                            ifelse(school == "School 2" & week >= 10 & week < 12, 1, 0)),
         no_school = ifelse(school == "School 1" & week %in% c(6,7), T, 
                            ifelse(school == "School 2" & week %in% c(6,12), T, F))) %>%
  mutate(intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air filter", "No")),
         intervention = factor(intervention, levels = c("Mask mandate", "No", "Air filter")))  %>%
  left_join(df_absent) %>%
  left_join(df_tot) %>%
  mutate(n_absent = ifelse(!is.na(n_absent), n_absent, 0)) %>%
  rename(n_tot_absent = n_absent,
         new_confirmed = confirmed,
         new_confirmed_by_onset = confirmed_by_onset,
         new_symptomatic = symptomatic,
         new_isolated = isolated,
         new_unknown = unknown,
         new_recovered_from_confirmed = recovered_from_confirmed,
         new_recovered_from_symptomatic = recovered_from_symptomatic,
         new_recovered_from_isolated = recovered_from_isolated,
         new_recovered_from_unknown = recovered_from_unknown) %>%
  # absent other is absent for unsuspected reasons of disease
  mutate(n_absent_other = n_tot_absent - new_confirmed - new_symptomatic - new_isolated - new_unknown) %>%
  mutate(weekday = weekdays(date),
         weekend = ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0)) %>%
  select(school, is_study_class, class, date, week, day, weekday, weekend, 
         intervention, airfilter, maskmandate, no_school,
         n_class, n_tot_absent, n_absent_other, 
         new_confirmed, new_confirmed_by_onset, new_symptomatic, new_isolated, new_unknown, 
         cum_confirmed, cum_confirmed_by_onset, cum_symptomatic, cum_isolated, cum_unknown, 
         new_recovered_from_confirmed, new_recovered_from_symptomatic, new_recovered_from_isolated, new_recovered_from_unknown,
         cum_recovered_from_confirmed, cum_recovered_from_symptomatic, cum_recovered_from_isolated, cum_recovered_from_unknown)


write.csv(df_cases, "data-clean/redcap.csv", row.names = F)
