#### Libraries ####

library(tidyverse)
library(reshape2)


#### Load data ####

df <- read.csv("data-raw/lessons/lesson-time.csv") 


#### Prep ####

dfp <- df %>%
  melt(c("school", "class", "date", "weekday")) %>%
  mutate(variable = gsub("X", "", variable),
         value = ifelse(value == "l", T, F)) %>%
  rename(lesson = value) %>%
  rename(hhmm = variable) %>%
  mutate(hhmm = gsub(".", ":", hhmm, fixed = T),
         date = as.character(as.Date(date, format = "%d.%m.%y")),
         time = paste0(date, " ", hhmm, ":00"),
         time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(school = ifelse(school == "Olten", "School 1", "School 2")) %>%
  select(school, class, date, weekday, time, hhmm, lesson)

# check
dfp %>%
  mutate(time = format(time, format = "%H:%M:%S")) %>%
  ggplot(aes(y = date, x = time, fill = lesson)) +
  facet_wrap(~ school + class + weekday, scales = "free_y") +
  geom_tile()

# manual changes
#' - Monday 2022-03-21 in School 1 (B3d) there is a exceptional lesson in the classroom
#' due to an exam. The activity exam is different and may influence the environmental levels,
#' thus making this Monday less comparable to others in this school. We therefore set it to FALSE.

dfp$lesson[dfp$class=="B3d"&dfp$date=="2022-03-21"] <- F

#' - Wednesday 2022-02-02 in School 1 (B3d) there is an exceptional lesson very late in the 
#' afternoon, which could be a coding error. Set to False.

dfp$lesson[dfp$class=="B3d"&dfp$date=="2022-02-02"&format(dfp$time, "%H:%M")>"15:00"] <- F

#' - Wednesday afternoons in School 1 (E3f) there were project works in multiple weeks
#' in February and March, but not all, where students were only partially if at all 
#' in the classroom. These afternoons are thus not comparable and will be excluded, i.e. 
#' set to FALSE.

dfp$lesson[dfp$class=="E3f"&dfp$weekday=="Wednesday"&format(dfp$time, "%H:%M")>"12:00"] <- F

# check
dfp %>%
  mutate(time = format(time, format = "%H:%M:%S")) %>%
  ggplot(aes(y = date, x = time, fill = lesson)) +
  facet_wrap(~ school + class + weekday, scales = "free_y") +
  geom_tile()

#' Note that School 2 is generally more irregular, so exceptions are not clear.

# aggregate by schools
dfp <- dfp %>%
  group_by(school, date, weekday, time, hhmm) %>%
  summarize(lesson = ifelse(any(lesson), T, F)) %>%
  ungroup() 

# check

dfp %>%
  mutate(time = format(time, format = "%H:%M:%S")) %>%
  ggplot(aes(y = date, x = time, fill = lesson)) +
  facet_wrap(~ school + weekday, scales = "free_y") +
  geom_tile()

# is_short_lesson <- function(x) {
#   y <- logical(length(x))
#   y[1] <- F
#   for (i in 2:(length(x)-1)) {
#     if (x[i] & !x[i-1] & !x[i+1]) {
#       y[i] <- T
#     }
#   }
#   if (x[length(x)] & !x[length(x)-1]) {
#     y[length(x)] <- T
#   }
#   return(y)
# }
# 
# dfp <- dfp %>%
#   group_by(school) %>%
#   arrange(date) %>%
#   mutate(short_lesson = is_short_lesson(lesson),
#          lesson = ifelse(short_lesson, F, lesson)) %>%
#   ungroup() %>%
#   select(-short_lesson)
  

#### Save data ####

saveRDS(dfp, "data-clean/lesson-filter.rds")
