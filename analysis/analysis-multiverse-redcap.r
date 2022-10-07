# Libraries
library(tidyverse)
library(tidybayes)
library(reshape2)
library(cmdstanr)
source("utils/epi.r")
source("utils/bayes.R")

# Model
tm <- cmdstan_model("models/tm_class.stan", cpp_options = list(stan_threads = T))

# Data 
col_order <- c("School 1 Study (A)", "School 1 Study (B)", "School 1 Control", "School 2 Study", "School 2 Control")
names(col_order) <- 1:5

r_est <- read_csv("data-clean/r-estimates.csv")

df <- read_csv("data-clean/redcap.csv") %>%
  filter(!no_school) %>%
  mutate(prop_absences = n_tot_absent / n_class) %>%
  rename(absences = n_tot_absent) %>%
  group_by(school, class) %>%
  arrange(date) %>%
  mutate(day = 1:n()) %>%
  ungroup() %>%
  left_join(r_est) %>%
  mutate(class = factor(paste(school, class), levels = col_order))

# suspected cases
suspected <- df %>%
  mutate(new_suspected = new_symptomatic + new_confirmed) %>%
  select(class, day, new_suspected) %>%
  uncount(new_suspected, .remove = F) %>%
  mutate(new_suspected = 1)

# sampling distribution parameters
smpl_distr_pars <-  df %>%
  select(class, n_class, cum_confirmed, cum_symptomatic, cum_unknown) %>%
  group_by(class) %>%
  summarize(all = max(cum_confirmed + cum_symptomatic + cum_unknown),
            confirmed = max(cum_confirmed),
            suspected = max(cum_symptomatic + cum_unknown),
            pop = max(n_class)) %>%
  ungroup()  %>%
  mutate(upper = pmin(pop - confirmed, suspected) / suspected,
         lower = 0) %>%
  rowwise() %>%
  mutate(mu = (upper + lower) / 2,
         sigma = (upper - lower) / (2*qnorm(0.95)))

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

# Number of runs
N <- 100
path <- "fitted-models/multiverse/"

for (i in 1:4) {
  # seed
  set.seed(i)
  path_i <- paste0(path, i, "/")
  
  # create sample
  samples <- list()
  samples_p <- c()
  for (cl in col_order) {
    susp_cl <- filter(suspected, class == cl)
    distr_pars_cl <- filter(smpl_distr_pars, class == cl)
    samples_p[cl] <- rnorm.lim(distr_pars_cl$lower, distr_pars_cl$upper, distr_pars_cl$mu, distr_pars_cl$sigma)
    samples[[cl]] <- slice_sample(susp_cl, prop = samples_p[[cl]])
  }
  samples <- do.call(rbind, samples) %>%
    group_by(class, day) %>%
    summarize(new_suspected = sum(new_suspected)) %>%
    ungroup()
  
  dir.create(file.path(path_i), showWarnings = FALSE)
  saveRDS(data.frame(class = col_order, p = samples_p), paste0(path_i, "sampling_prop.rds"))
  
  # combine with original data
  df_smpl <- df %>%
    left_join(samples) %>%
    mutate(new_suspected = ifelse(is.na(new_suspected), 0, new_suspected),
           new_cases = new_confirmed + new_suspected) %>%
    group_by(class) %>%
    arrange(day) %>%
    mutate(cum_cases = cumsum(new_cases)) %>%
    ungroup() %>%
    select(class, date, day, new_cases, cum_cases, airfilter, maskmandate, prop_absences, median_R_mean)
  
  saveRDS(df_smpl, paste0(path_i, "sample.rds"))
  
  # create stan data
  sDF <- list(
    L = 5,
    D = nrow(df_smpl) / 5,
    S = round(exp(p_in_mu_m + p_in_sigma_m^2 / 2)) * 2,
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
  sDF$maskmandate <- rbind(matrix(1, nrow = sDF$S, ncol = sDF$L), sDF$maskmandate)
  sDF$airfilter <- rbind(matrix(0, nrow = sDF$S, ncol = sDF$L), sDF$airfilter)
  r_est_seed <- r_est %>% 
    filter(between(date, min(df_smpl$date) - sDF$S, min(df_smpl$date) - 1)) %>% 
    arrange(date) %>%
    select(median_R_mean) %>%
    as.matrix()
  sDF$medianRest_mean <- rbind(matrix(rep(r_est_seed, sDF$L), ncol = sDF$L), sDF$medianRest_mean)
  
  
  # run model
  old_fits <- list.files(path_i, pattern = "tm_class", full.names = T)
  file.remove(old_fits)
  
  tmFit <- tm$sample(
    data = sDF,
    seed = 12345,
    chains = 4,
    iter_warmup = 1e3,
    iter_sampling = 1e3,
    parallel_chains = 4,
    threads_per_chain = 2,
    refresh = 200
  )
  
  if (i == 1) {
    tmFit$save_output_files(dir = path_i, basename = "tm_class")
  }
  
  # estimation results
  est_results <- tmFit$summary(c("phi_N", "alpha", "mu_p_in", "sigma_p_in", "theta_M", "tau", "theta_l",  "theta_A", "gamma")) 
  
  saveRDS(est_results, paste0(path_i, "estimation_results.rds"))
  
  # retrieve estimates
  new_I <- tidy_draws.CmdStanMCMC_mat(tmFit, "mu_new_infections") %>%
    rename(day = row,
           class = col) %>%
    mutate(class = recode(class, !!! col_order),
           class = factor(class, levels = col_order),
           day = day - sDF$S) %>%
    filter(day >= 1) %>%
    select(-draw) %>%
    group_by(class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Infections")
  new_N <- tidy_draws.CmdStanMCMC_mat(tmFit, "mu_new_cases") %>%
    rename(day = row,
           class = col) %>%
    mutate(class = recode(class, !!! col_order),
           class = factor(class, levels = col_order),
           day = day - sDF$S) %>%
    #filter(day >= 1) %>%
    select(-draw) %>%
    group_by(class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Cases")
  cum_I <- tidy_draws.CmdStanMCMC_mat(tmFit, "mu_cum_infections") %>%
    rename(day = row,
           class = col) %>%
    mutate(class = recode(class, !!! col_order),
           class = factor(class, levels = col_order),
           day = day - sDF$S) %>%
    filter(day >= 1) %>%
    select(-draw) %>%
    group_by(class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Cumulative infections")
  cum_N <- tidy_draws.CmdStanMCMC_mat(tmFit, "mu_cum_cases") %>%
    rename(day = row,
           class = col) %>%
    mutate(class = recode(class, !!! col_order),
           class = factor(class, levels = col_order),
           day = day - sDF$S) %>%
    filter(day >= 1) %>%
    select(-draw) %>%
    group_by(class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Cumulative cases")
  susceptibles <- tidy_draws.CmdStanMCMC_mat(tmFit, "susceptibles") %>%
    rename(day = row,
           class = col) %>%
    mutate(class = recode(class, !!! col_order),
           class = factor(class, levels = col_order),
           day = day - sDF$S) %>%
    filter(day >= 1) %>%
    select(-draw) %>%
    group_by(class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Susceptibles")
  estimated_inc <- rbind(new_I, new_N, cum_I, cum_N, susceptibles)
  
  saveRDS(estimated_inc, paste0(path_i, "incidence.rds"))
  
  avoided_I_masks <- tidy_draws.CmdStanMCMC_mat(tmFit, "avoided_infections_Masks")
  avoided_I_air <- tidy_draws.CmdStanMCMC_mat(tmFit, "avoided_infections_Air")
  
  avoided_I_masks_class <- avoided_I_masks %>%
    group_by(col, draw) %>%
    summarize(value = -sum(value)) %>%
    ungroup() %>%
    rename(class = col) %>%
    mutate(class = recode(class, !!! col_order),
           class = factor(class, levels = col_order),
           variable = "Masks")
  avoided_I_air_class <- avoided_I_air %>%
    group_by(col, draw) %>%
    summarize(value = -sum(value)) %>%
    ungroup() %>%
    rename(class = col) %>%
    mutate(class = recode(class, !!! col_order),
           class = factor(class, levels = col_order),
           variable = "Air")
  avoided_I <- rbind(avoided_I_masks_class, avoided_I_air_class)
  
  saveRDS(avoided_I, paste0(path_i, "avoided-infections.rds"))
  

}