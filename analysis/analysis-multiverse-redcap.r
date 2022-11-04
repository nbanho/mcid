# Libraries
library(tidyverse)
library(lubridate)
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

# Number of runs
N <- 100
path <- "fitted-models/multiverse/"

for (i in 1:N) {
  
  print(sprintf("Dataset %i", i))
  
  # seed
  set.seed(i)
  path_i <- paste0(path, i, "/")
  
  # stan data
  sDF <- readRDS(paste0(path_i, "sample-modeling-stan-datalist.rds"))
  
  # run model
  # old_fits <- list.files(path_i, pattern = "tm_class", full.names = T)
  # file.remove(old_fits)
  
  tmFit <- tm$sample(
    data = sDF,
    seed = 12345,
    chains = 4,
    iter_warmup = 1e3,
    iter_sampling = 1e3,
    parallel_chains = 4,
    threads_per_chain = 2,
    refresh = 0,
    show_messages = F
  )
  
  # if (i == 1) {
  #   tmFit$save_output_files(dir = path_i, basename = "tm_class")
  # }
  
  # estimation results
  est_results <- tmFit$summary(c("phi_N", "alpha", "omega", "beta", "mu_p_in", "sigma_p_in", "theta_M", "tau", "theta_l", "theta_A", "gamma")) 
  
  saveRDS(est_results, paste0(path_i, "estimation_results.rds"))
  
  # retrieve estimates
  new_I <- tidy_draws.CmdStanMCMC_mat(tmFit, "mu_new_infections") %>%
    rename(day = row,
           school_class = col) %>%
    mutate(school_class = recode(school_class, !!! col_order),
           school_class = factor(school_class, levels = col_order),
           day = day - sDF$S) %>%
    select(-draw) %>%
    group_by(school_class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Infections")
  new_N <- tidy_draws.CmdStanMCMC_mat(tmFit, "mu_new_cases") %>%
    rename(day = row,
           school_class = col) %>%
    mutate(school_class = recode(school_class, !!! col_order),
           school_class = factor(school_class, levels = col_order),
           day = day - sDF$S) %>%
    select(-draw) %>%
    group_by(school_class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Cases")
  est_new_N <- tidy_draws.CmdStanMCMC_mat(tmFit, "estimated_new_cases") %>%
    rename(day = row,
           school_class = col) %>%
    mutate(school_class = recode(school_class, !!! col_order),
           school_class = factor(school_class, levels = col_order),
           day = day - sDF$S) %>%
    select(-draw) %>%
    group_by(school_class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Estimated cases")
  cum_I <- tidy_draws.CmdStanMCMC_mat(tmFit, "mu_cum_infections") %>%
    rename(day = row,
           school_class = col) %>%
    mutate(school_class = recode(school_class, !!! col_order),
           school_class = factor(school_class, levels = col_order),
           day = day - sDF$S) %>%
    select(-draw) %>%
    group_by(school_class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Cumulative infections")
  cum_N <- tidy_draws.CmdStanMCMC_mat(tmFit, "mu_cum_cases") %>%
    rename(day = row,
           school_class = col) %>%
    mutate(school_class = recode(school_class, !!! col_order),
           school_class = factor(school_class, levels = col_order),
           day = day - sDF$S) %>%
    select(-draw) %>%
    group_by(school_class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Cumulative cases")
  susceptibles <- tidy_draws.CmdStanMCMC_mat(tmFit, "susceptibles") %>%
    rename(day = row,
           school_class = col) %>%
    mutate(school_class = recode(school_class, !!! col_order),
           school_class = factor(school_class, levels = col_order),
           day = day - sDF$S) %>%
    select(-draw) %>%
    group_by(school_class, day) %>%
    mean_qi() %>%
    ungroup() %>%
    mutate(variable = "Susceptibles")
  estimated_inc <- rbind(new_I, new_N, est_new_N, cum_I, cum_N, susceptibles)
  
  saveRDS(estimated_inc, paste0(path_i, "incidence.rds"))
  
  avoided_I_masks <- tidy_draws.CmdStanMCMC_mat(tmFit, "avoided_infections_Masks")
  avoided_I_air <- tidy_draws.CmdStanMCMC_mat(tmFit, "avoided_infections_Air")
  
  avoided_I_masks_class <- avoided_I_masks %>%
    group_by(col, draw) %>%
    summarize(value = -sum(value)) %>%
    ungroup() %>%
    rename(school_class = col) %>%
    mutate(school_class = recode(school_class, !!! col_order),
           school_class = factor(school_class, levels = col_order),
           variable = "Masks")
  avoided_I_air_class <- avoided_I_air %>%
    group_by(col, draw) %>%
    summarize(value = -sum(value)) %>%
    ungroup() %>%
    rename(school_class = col) %>%
    mutate(school_class = recode(school_class, !!! col_order),
           school_class = factor(school_class, levels = col_order),
           variable = "Air")
  avoided_I <- rbind(avoided_I_masks_class, avoided_I_air_class)
  
  saveRDS(avoided_I, paste0(path_i, "avoided-infections.rds"))

}