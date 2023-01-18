#### Libraries ####

library(tidyverse)
library(reshape2)
library(lubridate)
library(LaplacesDemon)

source("utils/plotting.r")
source("utils/epi.r")
source("helper/settings.r")

if(!dir.exists('fitted-models/multiverse')) {
  dir.create('fitted-models/multiverse')
}

#### Data #### 

# order of school classes
col_order <- c("School 1 (A)", "School 1 (B)", "School 1 (C)", "School 2 (D)", "School 2 (E)")
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
         airfilter = ifelse(class %in% c("C", "E"), 0, airfilter),
         weekend = ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0),
         no_school = ifelse(school == "School 1" & week %in% c(6,7), T,
                            ifelse(school == "School 2" & week %in% c(6,12), T, F)),
         intervention = ifelse(maskmandate==1, "Mask mandate", ifelse(airfilter==1, "Air cleaner", "No intervention")),
         intervention = factor(intervention, levels = c("Mask mandate", "No intervention", "Air cleaner")),
         prop_absences = ifelse(no_school, 0, prop_absences)) %>%
  select(school, class, school_class, date, week, weekend, day, weekday, 
         no_school, intervention, maskmandate, airfilter,
         n_absent, n_class, prop_absences, median_R_mean) %>%
  group_by(school, class) %>%
  arrange(date) %>%
  # make last weekend from vacation count towards study period
  mutate(no_school = ifelse(!lead(no_school, n = 2) & weekend == 1, F, no_school),
         weekend = ifelse(no_school, 0, weekend)) %>%
  ungroup()

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
  geom_line(aes(y = n_fitted), color = "black") +
  geom_point(aes(y = n_fitted), color = "black") +
  geom_point(aes(y = n), color = "blue", shape = 4) +
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
  mutate(school_class = gsub("Study", "Intervention", school_class)) %>%
  ggplot(aes(ymin = -Inf, ymax = p, x = k)) +
  facet_wrap(~ school_class, nrow = 1) +
  geom_ribbon(alpha = .1) +
  geom_line(aes(y = p)) +
  geom_segment(data = exc_df_limits %>% mutate(school_class = gsub("Study", "Intervention", school_class)), 
               mapping = aes(x = k, xend = k, y = 0, yend = p), linetype = "dashed") +
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


# prior for alpha
S <- round(exp(p_in_mu_m + p_in_sigma_m^2 / 2)) * 2
Tdays <- 7 * 7 + S
#' (1 - x) ^ Tdays = mean_share
#' Tdays * log 1-x = log (mean_share)
#' log 1-x = log (mean_share) / Tdays
#' x = 1 - exp(log (mean_share) / Tdays)

share_of_susecptibles <- exc_df %>%
  mutate(share_end = 1 - (suspected * k + confirmed) / pop,
         daily_share = 1 - exp(log(share_end) / Tdays),
         logit_daily_share = logit(daily_share)) %>%
  select(school_class, mu, share_end, daily_share, logit_daily_share) 


mean_share <- mean(share_of_susecptibles$share_end)
mean_share

mean_daily_share = 1 - exp(log(mean_share) / Tdays)
logit(mean_daily_share)

daily_share_pl <- share_of_susecptibles %>%
  ggplot(aes(x = daily_share)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(share_of_susecptibles$daily_share)), linetype = "dotted", color = "blue") + 
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  scale_x_continuous(limits = c(0, .1), labels = function(x) x * 100, expand = c(0,0)) +
  labs(x = "Daily prop. of susceptibles\ngetting infected in school", y = "Density", title = "a") +
  theme_bw2() +
  theme(plot.title = element_text(hjust = 0))

daily_share_pl

prior_logit_share <- data.frame(x = seq(-8, 0, .1)) %>%
  mutate(y = LaplacesDemon::dst(x, mu = -4.1, sigma = 1.6, nu = 3))

logit_daily_share_pl <- share_of_susecptibles %>%
  ggplot(aes(x = logit_daily_share)) +
  geom_density() +
  annotate("text", x = -2.5, y = 0.4, label = "Data", color = "black", size = 8 / cm(1)) +
  geom_line(data = prior_logit_share, mapping = aes(x = x, y = y), color = "red") +
  annotate("text", x = -1.5, y = 0.15, label = "Prior", color = "red", size = 8 / cm(1)) +
  geom_vline(aes(xintercept = mean(share_of_susecptibles$logit_daily_share)), linetype = "dotted", color = "blue") + 
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "Logit of daily prop. of susceptibles\ngetting infected in school", y = "Density", title = "b") +
  theme_bw2() +
  theme(plot.title = element_text(hjust = 0), plot.margin = unit(c(5.5,9,5.5,5.5), "pt"))

logit_daily_share_pl

comb_prior_pl <- arrangeGrob(daily_share_pl, logit_daily_share_pl, widths = c(5, 5), ncol = 2)

ggsave(plot = comb_prior_pl, filename = "results/redcap-supp-prior-alpha.pdf", width = 10 / cm(1), height = 5 / cm(1))


#### Simulation ####

# cases with and without missing symptom onset
unknown_onset_df <- df %>%
  filter(is.na(date_symptoms))
known_onset_df <- df %>%
  filter(!is.na(date_symptoms))

# simulate datasets
for (i in 1:N) {
  # directory
  path_i <- paste0(path, i, "/")
  if(!dir.exists(path_i)) {
    dir.create(path_i)
  }
  
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
    set.seed(i)
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
  
  # create stan data
  sDF <- list(
    L = J,
    D = nrow(modeling_df) / 5,
    S = round(exp(p_in_mu_m + p_in_sigma_m^2 / 2)) * 2,
    no_school = modeling_df %>%
      select(school_class, day, no_school) %>%
      dcast(day ~ school_class) %>%
      select(all_of(col_order)) %>%
      mutate_all(function(x) ifelse(x, 1, 0)) %>%
      as.matrix(),
    weekend = modeling_df %>%
      select(school_class, day, weekend) %>%
      dcast(day ~ school_class) %>%
      select(all_of(col_order)) %>%
      mutate_all(function(x) ifelse(x, 1, 0)) %>%
      as.matrix(),
    new_cases = modeling_df %>%
      select(school_class, day, new_cases) %>%
      dcast(day ~ school_class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    population = absences_df %>%
      group_by(school_class) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(school_class = factor(school_class, levels = col_order)) %>%
      arrange(school_class) %>%
      select(n_class) %>%
      unlist(),
    prop_absences = modeling_df %>%
      select(school_class, day, prop_absences) %>%
      dcast(day ~ school_class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    medianRest_mean = modeling_df %>%
      select(school_class, day, median_R_mean) %>%
      dcast(day ~ school_class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    maskmandate = modeling_df %>%
      select(school_class, day, maskmandate) %>%
      dcast(day ~ school_class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    airfilter = modeling_df %>%
      select(school_class, day, airfilter) %>%
      dcast(day ~ school_class) %>%
      select(all_of(col_order)) %>%
      as.matrix(),
    p_in_mu_m = p_in_mu_m,
    p_in_mu_s = p_in_mu_s,
    p_in_sigma_m = p_in_sigma_m,
    p_in_sigma_s = p_in_sigma_s
  )
  
  # add data for seeding phase
  seed_date <- seq(min(modeling_df$date) - days(sDF$S), min(modeling_df$date) - days(1), by = "d")
  sDF$no_school <- rbind(matrix(rep(rep(0, length(seed_date)), 5), nrow = sDF$S, ncol = sDF$L), sDF$no_school)
  seed_weekend <- ifelse(weekdays(seed_date) %in% c("Saturday", "Sunday"), 1, 0)
  sDF$weekend <- rbind(matrix(rep(seed_weekend, 5), nrow = sDF$S, ncol = sDF$L), sDF$weekend)
  sDF$maskmandate <- rbind(matrix(1, nrow = sDF$S, ncol = sDF$L), sDF$maskmandate)
  sDF$airfilter <- rbind(matrix(0, nrow = sDF$S, ncol = sDF$L), sDF$airfilter)
  r_est_seed <- r_est %>% 
    filter(between(date, min(modeling_df$date) - sDF$S, min(modeling_df$date) - 1)) %>% 
    arrange(date) %>%
    select(median_R_mean) %>%
    as.matrix()
  sDF$medianRest_mean <- rbind(matrix(rep(r_est_seed, sDF$L), ncol = sDF$L), sDF$medianRest_mean)
  
  saveRDS(sDF, paste0(path_i, "sample-modeling-stan-datalist.rds"))
}
