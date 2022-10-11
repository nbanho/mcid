#### Libraries ####

library(tidyverse)
library(lubridate)

source("utils/plotting.r")
source("utils/epi.r")


#### Data #### 

# order of school classes
col_order <- c("School 1 Study (A)", "School 1 Study (B)", "School 1 Control", "School 2 Study", "School 2 Control")
names(col_order) <- 1:5
J <- length(col_order)

# case data
df <- read_csv("data-clean/redcap-cases.csv") %>%
  filter(!is_teacher) %>% # ignore cases from teachers 
  select(-is_teacher)

# absences
absences_df <- read_csv("data-clean/redcap-absences.csv")

# R estimates
r_est <- read_csv("data-clean/r-estimates.csv")

# data range
dates <- seq(as.Date("2022-01-21"), as.Date("2022-03-25"), by = "d")
full_df <- data.frame(school_class = rep(col_order, each = length(dates)),
                      date = rep(dates, J)) %>%
  left_join(absences_df) %>%
  group_by(school_class) %>%
  fill(school, class, n_class, .direction = "updown") %>%
  arrange(date) %>%
  mutate(day = 1:n(),
         weekday = weekdays(date),
         n_absent = ifelse(is.na(n_absent), 0, n_absent),
         n_absent = ifelse(weekday %in% c("Saturday", "Sunday"), 0, n_absent),
         prop_absences = n_absent / n_class) %>%
  ungroup() %>%
  left_join(r_est %>% select(date, median_R_mean)) %>%
  mutate(week = as.numeric(strftime(date, format = "%V")),
         maskmandate = ifelse(school == "School 1" & week < 8, 1,
                              ifelse(school == "School 2" & week < 9, 1, 0)),
         airfilter = ifelse(school == "School 1" & week >= 11, 1,
                            ifelse(school == "School 2" & week >= 10 & week < 12, 1, 0)),
         airfilter = ifelse(class == "Control", 0, airfilter),
         weekend = ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0),
         no_school = ifelse(school == "School 1" & week %in% c(6,7), T,
                            ifelse(school == "School 2" & week %in% c(6,12), T, F)),
         intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air filter", "None")),
         intervention = factor(intervention, levels = c("Mask mandate", "None", "Air filter")),
         prop_absences = ifelse(no_school, 0, prop_absences)) %>%
  select(school, class, school_class, date, week, weekend, day, weekday, 
         no_school, intervention, maskmandate, airfilter,
         n_absent, n_class, prop_absences, median_R_mean) 

saveRDS(full_df, "data-clean/redcap-full-range.rds")


#### Settings ####

# path to files 
path <- "fitted-models/multiverse/"

# number of simulations
N <- 100

# Seeds
seeds <- 1:N

#### Symptom onset date ####

# determine probability weights for delay
timediff <- df %>%
  filter(!is.na(date_symptoms)) %>%
  mutate(d = as.numeric(date_start - date_symptoms)) %>%
  group_by(d) %>%
  summarise(n = n()) %>%
  bind_rows(data.frame(d = 2, n = 0)) %>%
  arrange(d)

# fit exponential decay
exp_fit <- nls(n~b0*exp(b1*d),data=timediff,start=list(b0=1,b1=1))

timediff <- timediff %>%
  mutate(n_fitted = fitted(exp_fit),
         p = n_fitted / sum(n_fitted),
         p_lab = paste0("Prob. = ", round(p , 2)))

# plot
time_diff_weights_pl <- timediff %>%
  ggplot(aes(x = d)) +
  geom_line(aes(y = n_fitted), color = "blue") +
  geom_point(aes(y = n_fitted), color = "blue", shape = 4) +
  geom_point(aes(y = n)) +
  geom_label(aes(y = n_fitted, label = p_lab), vjust = -.5, hjust = -.1, size = 8 / cm(1)) +
  scale_y_continuous(expand = expansion(add = c(.5, 2.5))) +
  scale_x_continuous(expand = expansion(add = c(0.1, 1)), breaks = seq(0, 4)) + 
  labs(y = "Frequency", x = "Days from symptom onset to absence") +
  theme_bw2() 

time_diff_weights_pl

save_plot(time_diff_weights_pl, pdf_file = "results/redcap-supp-delay-weights.pdf", w = 16, h = 10)


# sampling function

p.sympton_onset <- function(date_absence, seed) {
  set.seed(seed)
  delay <- sample(timediff$d, size = 1, prob = timediff$p)
  date_onset <- date_absence - days(delay)
  return(date_onset)
}


#### Suspected cases ####

# sampling distribution parameters
smpl_distr_pars <-  df %>%
  left_join(absences_df %>% group_by(school_class) %>% summarise(n_class = first(n_class))) %>%
  group_by(school_class) %>%
  summarize(total = n(),
            confirmed = sum(suspected == "confirmed" | suspected == "isolated"),
            suspected = sum(suspected == "symptomatic" | suspected == "unknown"),
            pop = max(n_class)) %>%
  ungroup()  %>%
  mutate(upper = pmin(pop - confirmed, suspected) / suspected,
         lower = 0) %>%
  rowwise() %>%
  mutate(mu = (upper + lower) / 2,
         sigma = (upper - lower) / (2*qnorm(0.95)))

# plot
dnorm.lim <- function(l, u, ...) {
  k <- seq(l, u, .01)
  data.frame(k=k, p=sapply(k, dnorm, ...))
}

exc_df <- smpl_distr_pars %>%
  rowwise() %>%
  mutate(draws = list(dnorm.lim(lower, upper, mu, sigma))) %>%
  unnest() 

exc_df_limits <- exc_df %>%
  group_by(school_class) %>%
  filter(k == min(k) | k == max(k)) %>%
  ungroup()

exc_df_pl <- exc_df %>%
  ggplot(aes(ymin = -Inf, ymax = p, x = k)) +
  facet_wrap(~ school_class, nrow = 1) +
  geom_ribbon(alpha = .1) +
  geom_line(aes(y = p)) +
  geom_segment(data = exc_df_limits, mapping = aes(x = k, xend = k, y = 0, yend = p), linetype = "dashed") +
  geom_vline(aes(xintercept = mu), linetype = "dotted", color = "blue") +
  scale_x_continuous(expand = expansion(add = c(0,0)), limits = c(0,1), labels = function(x) x * 100) +
  scale_y_continuous(expand = expansion(add = c(0., .01)), limits = c(0, NA)) +
  labs(x = "Proportion of suspected cases being actual cases of COVID-19 (%)", 
       y = "Density") +
  theme_bw2() +
  theme(plot.title = element_text(hjust = 0), panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(0.1, .5, 0.1, 0.1), "cm"))

exc_df_pl

save_plot(exc_df_pl, pdf_file = "results/redcap-prior-excessive-cases.pdf", w = 16, h = 4)

# sampling functions
rnorm.lim <- function(l, u, ...) {
  is_outside <- T
  while(is_outside) {
    d <- rnorm(1, ...)
    if (d >= l & d <= u) {
      is_outside <- F
    }
  }
  return(d)
}


#### Simulation ####

# cases with and without missing symptom onset
unknown_onset_df <- df %>%
  filter(is.na(date_symptoms))
known_onset_df <- df %>%
  filter(!is.na(date_symptoms))

# simulate datasets
for (i in 1:N) {
  # seed
  path_i <- paste0(path, i, "/")
  
  # sample symptom onset dates
  sample_onset_df <- unknown_onset_df %>%
    mutate(seed = i * 100 + 1:n()) %>%
    rowwise() %>%
    mutate(date_symptoms = p.sympton_onset(date_start, seed)) %>%
    ungroup() %>%
    select(-seed) %>%
    rbind(known_onset_df)
  
  # sample suspected cases
  sample_suspected_df <- sample_onset_df %>%
    filter(suspected %in% c("symptomatic", "unknown"))
  samples <- list()
  samples_p <- c()
  for (cl in col_order) {
    susp_cl <- filter(sample_suspected_df, school_class == cl)
    distr_pars_cl <- filter(smpl_distr_pars, school_class == cl)
    samples_p[cl] <- rnorm.lim(distr_pars_cl$lower, distr_pars_cl$upper, distr_pars_cl$mu, distr_pars_cl$sigma)
    samples[[cl]] <- slice_sample(susp_cl, prop = samples_p[[cl]])
  }
  samples_df <- do.call(rbind, samples) %>%
    rbind(sample_onset_df %>% filter(suspected %in% c("confirmed", "isolated")))
  
  dir.create(file.path(path_i), showWarnings = FALSE)
  saveRDS(data.frame(class = col_order, p = samples_p), paste0(path_i, "sampling_prop.rds"))
  saveRDS(samples_df, paste0(path_i, "sample.rds"))
  
  # create modeling data
  modeling_df <- full_df %>%
    left_join(samples_df %>% 
                select(school, class, school_class, date_symptoms, suspected) %>%
                rename(date = date_symptoms) %>%
                mutate(count = 1) %>%
                group_by(school, class, school_class, date) %>%
                summarize(new_cases = sum(count)) %>%
                ungroup()) %>%
    mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases))
  
  
  saveRDS(modeling_df, paste0(path_i, "sample-modeling-df.rds"))
  
  # combine with original data
  df_smpl <- df %>%
    left_join(samples) %>%
    mutate(new_suspected = ifelse(is.na(new_suspected), 0, new_suspected),
           new_cases = new_confirmed + new_suspected) %>%
    group_by(class) %>%
    arrange(day) %>%
    mutate(cum_cases = cumsum(new_cases)) %>%
    ungroup() 
  
  saveRDS(df_smpl, paste0(path_i, "sample.rds"))
  
  # create stan data
  sDF <- list(
    L = 5,
    D = nrow(df_smpl) / 5,
    S = round(exp(p_in_mu_m + p_in_sigma_m^2 / 2)) * 2,
    no_school = df_smpl %>%
      mutate(no_school = ifelse(week == 12 & school == "School 2", 0, no_school),
             no_school = ifelse(weekend == 1 | no_school, 1, 0)) %>%
      select(class, day, no_school) %>%
      dcast(day ~ class) %>%
      select(all_of(col_order)) %>%
      mutate_all(function(x) ifelse(x, 1, 0)) %>%
      as.matrix(),
    no_modeling_day = df_smpl %>%
      mutate(no_school = ifelse(weekend == 1 | no_school, 1, 0)) %>%
      select(class, day, no_school) %>%
      dcast(day ~ class) %>%
      select(all_of(col_order)) %>%
      mutate_all(function(x) ifelse(x, 1, 0)) %>%
      as.matrix(),
    new_cases = df_smpl %>%
      select(class, day, new_cases) %>%
      dcast(day ~ class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    population = smpl_distr_pars[match(smpl_distr_pars$class, col_order),"pop"]$pop,
    prop_absences = df_smpl %>%
      select(class, day, prop_absences) %>%
      dcast(day ~ class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    medianRest_mean = df_smpl %>%
      select(class, day, median_R_mean) %>%
      dcast(day ~ class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    maskmandate = df_smpl %>%
      select(class, day, maskmandate) %>%
      dcast(day ~ class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    airfilter = df_smpl %>%
      select(class, day, airfilter) %>%
      dcast(day ~ class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    p_in_mu_m = p_in_mu_m,
    p_in_mu_s = p_in_mu_s,
    p_in_sigma_m = p_in_sigma_m,
    p_in_sigma_s = p_in_sigma_s
  )
  
  # add data for seeding phase
  seed_date <- seq(df_smpl$date[1] - days(sDF$S), df_smpl$date[1] - days(1), by = "d")
  seed_weekend <- ifelse(weekdays(seed_date) %in% c("Saturday", "Sunday"), 1, 0)
  sDF$no_school <- rbind(matrix(rep(seed_weekend, 5), nrow = sDF$S, ncol = sDF$L), sDF$no_school)
  sDF$no_modeling_day <- rbind(matrix(rep(seed_weekend, 5), nrow = sDF$S, ncol = sDF$L), sDF$no_modeling_day)
  sDF$maskmandate <- rbind(matrix(1, nrow = sDF$S, ncol = sDF$L), sDF$maskmandate)
  sDF$airfilter <- rbind(matrix(0, nrow = sDF$S, ncol = sDF$L), sDF$airfilter)
  r_est_seed <- r_est %>% 
    filter(between(date, min(df_smpl$date) - sDF$S, min(df_smpl$date) - 1)) %>% 
    arrange(date) %>%
    select(median_R_mean) %>%
    as.matrix()
  sDF$medianRest_mean <- rbind(matrix(rep(r_est_seed, sDF$L), ncol = sDF$L), sDF$medianRest_mean)
}