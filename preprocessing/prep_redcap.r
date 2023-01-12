#### Libraries ####

library(tidyverse)
library(lubridate)
library(reshape2)


#### Load data ####

df <- read.delim("data-raw/redcap/suspected-cases.csv", sep = ",") %>%
  rename(school = `Data.Access.Group`,
         date_start = `Datum_abwesend`,
         date_end = `Datum_zurück`,
         date_symptoms = `Seit.wann.hat.der.Schüler...die.Schülerin.Symptome.`,
         date_test = `Falls.eine.mikrobiologische.Untersuchung.durchgeführt.wurde..wann.wurde.der.Test.gemacht.`) %>%
  # Set school 1 to Olten
  mutate(school = ifelse(grepl("Olten", school), "School 1", "School 2"),
         is_study_class = ifelse(grepl("Studienklasse", `Record.ID`), T, F),
         classlabel = stringi::stri_extract(`Record.ID`, regex = "\\w\\d\\w"),
         class = ifelse(school == "School 1", ifelse(is_study_class, ifelse(classlabel == "E3f", "A", "B"), "C"),
                             ifelse(is_study_class, "D", "E")),
         class = factor(class, levels = LETTERS[1:5]),
         is_teacher = ifelse(`Bitte.geben.Sie.das.Geburtsjahr.des.Schülers...der.Schülerin.an` <= 2000, T, F),
         date_start = as.Date(date_start, format = "%d.%m.%y"),
         date_end = as.Date(date_end, format = "%d.%m.%y"),
         date_symptoms = as.Date(date_symptoms, format = "%d.%m.%y"),
         date_test = as.Date(date_test, format = "%d.%m.%y"),
         # special case: teachers --> set date_start = date_end
         date_end = ifelse(is.na(date_end), as.character(date_start + days(1)), as.character(date_end)),
         date_end = as.Date(date_end),
         n_class = ifelse(class == "A", 24,
                          ifelse(class == "B", 14,
                                 ifelse(class == "C", 14, 
                                        ifelse(class == "D", 20, 18))))) %>%
  filter(`Anzahl.Tage` != 0) %>%
  group_by(school, class) %>%
  arrange(date_start) %>%
  mutate(id = 1:n()) %>%
  ungroup()


# summary: absences

df %>%
  # filter teachers
  filter(!is_teacher) %>%
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
  arrange(school, class) %>%
  bind_rows(data.frame(school = "Total", 
                       class = "Total",
                       Total = sum(.$Total),
                       `Quarantäne` = sum(.$Quarantäne),
                       Isolation = sum(.$Isolation),
                       Krankheit = sum(.$Krankheit),
                       Anderes = sum(.$Anderes))) %>%
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
  unnest(cols = date) %>%
  group_by(school, class, date) %>%
  summarize(n_absent = n()) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) 
  

# Filter confirmed cases

df_confirmed <- df %>% 
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.` 
         == "Positiv") 

# special cases
df_confirmed_teacher <- df_confirmed %>%
  filter(is_teacher)
# teacher first in quarantine then in isolation
df_confirmed_teacher[2,"date_start"] <- df_confirmed_teacher[1,"date_start"] 
df_confirmed_teacher <- df_confirmed_teacher[-1, ] 
# teacher in both control and study classes
df_confirmed_teacher <- df_confirmed_teacher[-3, ] 
# df_confirmed_teacher[2,"class"] <- "D|E" # i.e. the teacher actually teaches both classes

df_confirmed <- df_confirmed %>%
  filter(!is_teacher) %>%
  rbind(df_confirmed_teacher) %>%
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

col_order <- c("School 1 (A)", "School 1 (B)", "School 1 (C)", "School 2 (D)", "School 2 (E)")
names(col_order) <- 1:5

df_cases <- rbind(df_confirmed, df_isolated, df_suspected) %>%
  mutate(school_class = factor(paste0(school, " (", class, ")"), levels = col_order))

write.csv(df_cases, "data-clean/redcap-cases.csv", row.names = F)

df_absences <- df_absent %>% left_join(df_tot) %>%
  mutate(school_class = factor(paste0(school, " (", class, ")"), levels = col_order))

write.csv(df_absences, "data-clean/redcap-absences.csv", row.names = F)
