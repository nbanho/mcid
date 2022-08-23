#### Libraries ####

library(tidyverse)
library(lubridate)
library(reshape2)


#### Load data ####

df <- read.delim("data-raw/redcap/suspected-cases.csv", sep = ";") %>%
  filter(grepl("Studienklasse", `Record.ID`))  %>%
  rename(location = `Data.Access.Group`,
         date_start = `Datum_abwesend`,
         date_end = `Datum_zurück`) %>%
  mutate(location = tolower(ifelse(grepl("Olten", location), "Olten", location))) %>%
  mutate(date_start = as.Date(date_start, format = "%d.%m.%y")) %>%
  mutate(date_end = as.Date(date_end, format = "%d.%m.%y")) %>%
  # special case: teachers --> set date_start = date_end
  mutate(date_end = ifelse(is.na(date_end), as.character(date_start + days(1)), as.character(date_end))) %>%
  mutate(date_end = as.Date(date_end))
  

#### Filter data ####

# Filter absent

df_absent <- df  %>%
  select(c("location", "date_start", "date_end")) %>%
  mutate(date = map2(as.character(date_start), as.character(date_end), 
                     function(x,y) seq(as.Date(x), to = as.Date(y) - days(1), by = "1 day"))) %>%
  select(location, date) %>%
  unnest() %>%
  group_by(location, date) %>%
  summarize(n_absent = n()) %>%
  ungroup() %>%
  mutate(date = as.Date(date))
  

# Filter confirmed cases

df_confirmed <- df %>% 
  filter(`Falls.eine.mikrobiologische.Untersuchung.für.COVID.19.durchgeführt.wurde..was.ergab.das.Testresultat.` 
         == "Positiv")

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

w <- which(df_suspected %>% mutate(date_start = as.character(date_start),
                                   date_end = as.character(date_end)) =="Checked",arr.ind=TRUE)
df_suspected[w] <- names(df_suspected)[w[,"col"]]
df_suspected <- df_suspected %>%
  mutate_at(vars(matches("Welche.Symptome.hat.der.Schüler..die.Schülerin....choice.")), gsub,
            pattern = "Welche.Symptome.hat.der.Schüler..die.Schülerin....choice.", replacement = "") %>%
  mutate_at(vars(matches("Welche.Symptome.hat.der.Schüler..die.Schülerin....choice.")),
            function(x) ifelse(x == "Unchecked", NA, x)) %>%
  mutate(`Welche.Symptome.hat.der.Schüler..die.Schülerin....choice.Andere.` = 
           ifelse(!is.na(`Welche.Symptome.hat.der.Schüler..die.Schülerin....choice.Andere.`),
                         `Fall.andere.Symptome..Welche.`, 
                         `Welche.Symptome.hat.der.Schüler..die.Schülerin....choice.Andere.`)) %>%
  select(-`Welche.Symptome.hat.der.Schüler..die.Schülerin....choice.Keiner.dieser.Symptome.`) %>%
  unite(col = "Symptoms", matches("Welche.Symptome"), sep = ", ", remove = F, na.rm = T) %>%
  mutate(symptomatic = ifelse(Symptoms != "", "suspected-symptomatic", "suspected"))


#### Combine data ####

df_cases <- df_confirmed %>% 
  select(location, date_start) %>%
  mutate(symptomatic = "confirmed") %>%
  rbind(df_suspected %>%
          select(location, date_start, symptomatic)) %>%
  group_by(location, symptomatic, date_start) %>%
  summarize(new_cases = n()) %>%
  ungroup() %>%
  dcast(date_start + location ~ symptomatic) %>%
  mutate_at(vars(confirmed, suspected, `suspected-symptomatic`), function(x) ifelse(is.na(x), 0, x))

df_cases <- data.frame(date_start = rep(seq.Date(min(df$date_start), 
                                                 max(df$date_start), by = "1 day"), 2)) %>%
  mutate(location = rep(c("olten", "trimbach"), each = nrow(.) / 2)) %>%
  left_join(df_cases) %>%
  set_names(c("date", "location", c("confirmed", "unknown", "symptoms"))) %>%
  group_by(location) %>%
  arrange(date) %>%
  mutate_at(vars(confirmed, unknown, symptoms), function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(across(c(confirmed, unknown, symptoms), cumsum, .names = 'cum_{col}')) %>%
  ungroup() %>%
  mutate(week = as.numeric(strftime(date, format = "%V")))  %>%
  mutate(day = weekdays(date)) %>%
  filter(!(day %in% c("Samstag", "Sonntag"))) %>%
  mutate(maskmandate = ifelse(week < 9, 1, 0),
         airfilter = ifelse(week >= 12, 1, 0)) %>%
  mutate(intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air filter", "No")),
         intervention = factor(intervention, levels = c("Mask mandate", "No", "Air filter")))  %>%
  left_join(df_absent) %>%
  mutate(n_absent = ifelse(!is.na(n_absent), n_absent, 0)) %>%
  rename(n_tot_absent = n_absent,
         new_confirmed = confirmed,
         new_unknown = unknown,
         new_symptoms = symptoms) %>%
  mutate(n_absent_other = n_tot_absent - new_confirmed - new_unknown - new_symptoms) %>%
  select(location, date, week, day, 
         intervention, airfilter, maskmandate,
         n_tot_absent, n_absent_other, 
         new_confirmed, new_unknown, new_symptoms,
         cum_confirmed, cum_unknown, cum_symptoms) 
  

df_cases %>%
  filter(maskmandate == 1) %>%
  arrange(desc(date)) %>%
  slice(1)

df_cases %>%
  filter(airfilter == 1) %>%
  arrange(date) %>%
  slice(1)

df_cases %>%
  ggplot(aes(x = date, y = cases, color = symptomatic)) +
  facet_wrap(~ location) +
  geom_path() +
  geom_vline(aes(xintercept = as.Date("2020-02-23"))) +
  geom_vline(aes(xintercept = as.Date("2020-03-17"))) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave("cases.png", height = 10 / cm(1), width = 12 / cm(1))


write.csv(df_cases, "data-clean/redcap.csv", row.names = F)
