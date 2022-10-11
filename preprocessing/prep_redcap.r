#### Libraries ####

library(tidyverse)
library(lubridate)
library(reshape2)


#### Load data ####

df <- read.delim("data-raw/redcap/suspected-cases.csv", sep = ";") %>%
  rename(school = `Data.Access.Group`,
         date_start = `Datum_abwesend`,
         date_end = `Datum_zurück`,
         date_symptoms = `Seit.wann.hat.der.Schüler...die.Schülerin.Symptome.`,
         date_test = `Falls.eine.mikrobiologische.Untersuchung.durchgeführt.wurde..wann.wurde.der.Test.gemacht.`) %>%
  # Set school 1 to Olten
  mutate(school = ifelse(grepl("Olten", school), "School 1", "School 2"),
         is_study_class = ifelse(grepl("Studienklasse", `Record.ID`), T, F),
         classlabel = stringi::stri_extract(`Record.ID`, regex = "\\w\\d\\w"),
         classlabel = ifelse(!is_study_class, classlabel, ifelse(classlabel == "B3d", "B", "A")),
         class = ifelse(school == "School 1", ifelse(is_study_class, paste0("Study (", classlabel, ")"), "Control"), 
                        ifelse(is_study_class, "Study", "Control")),
         is_teacher = ifelse(`Bitte.geben.Sie.das.Geburtsjahr.des.Schülers...der.Schülerin.an` <= 2000, T, F),
         date_start = as.Date(date_start, format = "%d.%m.%Y"),
         date_end = as.Date(date_end, format = "%d.%m.%Y"),
         date_symptoms = as.Date(date_symptoms, format = "%d.%m.%Y"),
         date_test = as.Date(date_test, format = "%d.%m.%Y"),
         # special case: teachers --> set date_start = date_end
         date_end = ifelse(is.na(date_end), as.character(date_start + days(1)), as.character(date_end)),
         date_end = as.Date(date_end),
         n_class = ifelse(class == "Study (B)", 14,
                          ifelse(class == "Study (A)", 24,
                                 ifelse(school == "School 1", 14, 
                                        ifelse(school == "School 2" & class == "Study", 20, 18))))) %>%
  filter(`Anzahl.Tage` != 0) %>%
  group_by(school, class) %>%
  arrange(date_start) %>%
  mutate(id = 1:n()) %>%
  ungroup()


# summary: absences

df %>%
  select(school, class, date_start, date_end, matches("Absenz")) %>%
  # set maximum date to end of study period
  mutate(date_end = as.Date(ifelse(date_end >= max(date_start), as.character(max(date_start)), as.character(date_end)))) %>%
  mutate(missing = as.numeric(date_end - date_start)) %>% 
  select(-date_start, -date_end) %>%
  melt(c("school", "class", "missing")) %>%
  mutate(value = ifelse(value == "Checked", T, F)) %>%
  mutate(variable = gsub("Grund.für.die.Absenz....choice.", "", variable, fixed = T),
         variable = gsub(".", "", variable, fixed = T)) %>%
  group_by(school, class, variable) %>%
  summarize(n = sum(missing[value])) %>%
  ungroup() %>%
  dcast(school + class ~ variable) %>%
  mutate(Total = `Quarantäne` + Isolation + Krankheit + Anderes) %>%
  select(school, class, Total, `Quarantäne`, Isolation, Krankheit, Anderes) %>%
  arrange(school, desc(class)) %>%
  as.matrix() %>%
  t()

#### Filter data ####

# Filter total

df_tot <- df %>%
  select(school, class, n_class) %>%
  group_by(school, class) %>%
  slice(1) %>%
  ungroup()

# Filter absent

df_absent <- df  %>%
  filter(!is_teacher) %>%
  select(school, is_study_class, class, date_start, date_end) %>%
  mutate(date = map2(as.character(date_start), as.character(date_end), 
                     function(x,y) seq(as.Date(x), to = as.Date(y) - days(1), by = "1 day"))) %>%
  select(school, is_study_class, class, date) %>%
  unnest() %>%
  group_by(school, class, date) %>%
  summarize(n_absent = n()) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) 
  

# Filter confirmed cases

df_confirmed <- df %>% 
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.` 
         == "Positiv") %>%
  select(school, class, is_teacher, date_symptoms, date_start, date_end) %>%
  mutate(suspected = "confirmed") 


# Filter isolated

df_isolated <- df %>%
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.`
         == "") %>%
  filter(`Grund.für.die.Absenz....choice.Isolation.` == "Checked") %>%
  select(school, class, is_teacher, date_symptoms, date_start, date_end) %>%
  mutate(suspected = "isolated")
  

# Filter suspected cases

df_suspected <- df %>%
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.`
         == "") %>%
  filter(`Grund.für.die.Absenz....choice.Krankheit.` == "Checked") %>%
  filter(!grepl("Schnuppern", `Falls.anderes`)) %>%
  filter(!grepl("Persönliches", `Falls.anderes`)) %>%
  filter(!grepl("Unfall", `Falls.anderes`)) %>%
  filter(!grepl("Prüfungen", `Falls.anderes`)) %>%
  filter(!grepl("Berufsberatung", `Falls.anderes`)) %>%
  filter(!grepl("Magendarm", `Falls.anderes`)) %>%
  filter(!grepl("50% Krankgeschrieben", `Falls.anderes`))  %>%
  select(school, class, id, is_teacher, date_start, date_end,  date_symptoms, matches("Welche.Symptome.hat.der")) %>%
  set_names(c("school", "class", "id", "is_teacher", "date_start", "date_end", "date_symptoms",
              "fever", "chills", "limb pain", "loss of taste", "loss of smell",
              "fatigue", "cough", "cold", "diarrhea", "sore throat", "headache",
              "breathing difficulties", "stomach",
              "other", "none")) %>%
  select(-other, -none) %>%
  mutate(across(-c(school, class, id, is_teacher, date_start, date_end, date_symptoms), ~ ifelse(.x == "Checked", T, F))) %>%
  melt(c("school", "class", "id", "is_teacher", "date_start", "date_end", "date_symptoms")) %>%
  group_by(school, class, id, is_teacher, date_start, date_end, date_symptoms) %>%
  summarise(suspected = ifelse(any(value), "symptomatic", "unknown")) %>%
  ungroup() %>%
  select(-id)

#' fix data error:
#' for one student, the symptom onset date (2022-01-21) is later than the date of absence (2022-02-21) by 30 days
#' but since both dates have the same date and the end date of the absence (2022-03-02), it can be assumed that this is 
#' a typing error in the month
idx_err <- which(df_suspected$date_start-df_suspected$date_symptoms==-31)
df_suspected$date_start[idx_err] <- df_suspected$date_symptoms[idx_err]

#### Combine data ####

col_order <- c("School 1 Study (A)", "School 1 Study (B)", "School 1 Control", "School 2 Study", "School 2 Control")
names(col_order) <- 1:5

df_cases <- rbind(df_confirmed, df_isolated, df_suspected) %>%
  mutate(school_class = factor(paste(school, class), levels = col_order))

write.csv(df_cases, "data-clean/redcap-cases.csv", row.names = F)

df_absences <- df_absent %>% left_join(df_tot) %>%
  mutate(school_class = factor(paste(school, class), levels = col_order))

write.csv(df_absences, "data-clean/redcap-absences.csv", row.names = F)

# df_cases <- df_confirmed_tot %>%
#   select(school, is_study_class, class, date_start) %>%
#   mutate(suspected = "confirmed") %>%
#   rbind(df_confirmed_tot %>%
#           select(school, is_study_class, class, date_end) %>%
#           rename(date_start = date_end) %>%
#           mutate(suspected = "recovered_from_confirmed")) %>%
#   rbind(df_suspected %>%
#           select(school, is_study_class, class, date_start, suspected)) %>%
#   rbind(df_suspected %>%
#           select(school, is_study_class, class, date_end, suspected) %>%
#           rename(date_start = date_end) %>%
#           mutate(suspected = paste0("recovered_from_", suspected))) %>%
#   group_by(school, is_study_class, class, suspected, date_start) %>%
#   summarize(new_cases = n()) %>%
#   ungroup() %>%
#   dcast(date_start + school + is_study_class + class ~ suspected) %>%
#   mutate_at(vars(confirmed, symptomatic, unknown,
#                  recovered_from_confirmed, recovered_from_symptomatic, recovered_from_unknown), 
#             function(x) ifelse(is.na(x), 0, x))
# 
# df_cases <- data.frame(date_start = rep(seq.Date(min(df$date_start), 
#                                                  max(df$date_start),
#                                                  by = "1 day"), 5)) %>%
#   mutate(school = rep(c("School 1", "School 1", "School 1", "School 2", "School 2"), each = nrow(.) / 5),
#          is_study_class = rep(c(T, T, F, T, F), each = nrow(.) / 5),
#          class = rep(c("Study (B3d)", "Study (E3f)", "Control", "Study", "Control"), each = nrow(.) / 5)) %>%
#   left_join(df_cases) %>%
#   rename(date = date_start) %>%
#   group_by(school, is_study_class, class) %>%
#   arrange(date) %>%
#   mutate_at(vars(confirmed, symptomatic, unknown,
#                  recovered_from_confirmed, recovered_from_symptomatic, recovered_from_unknown), 
#             function(x) ifelse(is.na(x), 0, x)) %>%
#   mutate(across(c(confirmed, symptomatic, unknown,
#                   recovered_from_confirmed, recovered_from_symptomatic, recovered_from_unknown), 
#                 cumsum, .names = 'cum_{col}')) %>%
#   ungroup() %>%
#   mutate(week = as.numeric(strftime(date, format = "%V")))  %>%
#   mutate(maskmandate = ifelse(school == "School 1" & week < 8, 1, 
#                               ifelse(school == "School 2" & week < 9, 1, 0)),
#          airfilter = ifelse(school == "School 1" & week >= 11, 1, 
#                             ifelse(school == "School 2" & week >= 10 & week < 12, 1, 0)),
#          airfilter = ifelse(class == "Control", 0, airfilter),
#          weekday = weekdays(date),
#          weekend = ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0),
#          #maskmandate = ifelse(weekend == 1, 0, maskmandate),
#          #airfilter = ifelse(weekend == 1, 0, airfilter),
#          no_school = ifelse(school == "School 1" & week %in% c(6,7), T, 
#                             ifelse(school == "School 2" & week %in% c(6,12), T, F)),
#          intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air filter", "None")),
#          intervention = factor(intervention, levels = c("Mask mandate", "None", "Air filter")))  %>%
#   left_join(df_absent) %>%
#   left_join(df_tot) %>%
#   mutate(n_absent = ifelse(!is.na(n_absent), n_absent, 0)) %>%
#   rename(n_tot_absent = n_absent,
#          new_confirmed = confirmed,
#          new_symptomatic = symptomatic,
#          new_unknown = unknown,
#          new_recovered_from_confirmed = recovered_from_confirmed,
#          new_recovered_from_symptomatic = recovered_from_symptomatic,
#          new_recovered_from_unknown = recovered_from_unknown) %>%
#   mutate(class = ifelse(class == "Study (E3f)", "Study (A)", 
#                         ifelse(class == "Study (B3d)", "Study (B)", class))) %>%
#   select(school, is_study_class, class, date, week, weekday, weekend, 
#          intervention, airfilter, maskmandate, no_school,
#          n_class, n_tot_absent, 
#          new_confirmed, new_symptomatic, new_unknown, 
#          cum_confirmed, cum_symptomatic, cum_unknown, 
#          new_recovered_from_confirmed, new_recovered_from_symptomatic, new_recovered_from_unknown,
#          cum_recovered_from_confirmed, cum_recovered_from_symptomatic, cum_recovered_from_unknown)
