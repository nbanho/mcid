require(posterior)
require(cmdstanr)
require(tidybayes)
require(bayesplot)
require(LaplacesDemon)
require(loo)
library(rstan)


# Gather draws for posterior draws from read_cmdstan_csv
gather_draws_csv <- function(model, par, mi, ma) {
  model$post_warmup_draws[,,paste0(par, "[", mi:ma, "]")]%>% 
    as_draws_df() %>%
    data.frame() %>%
    reshape2::melt(id.vars = c(".chain", ".iteration", ".draw")) %>%
    mutate(variable = stringi::stri_extract(variable, regex = "\\d+")) 
}

gather_draws_csv2 <- function(model, par, mi1, ma1, mi2, ma2) {
  grid <- expand.grid(mi1:ma1, mi2:ma2)
  model$post_warmup_draws[,,paste0(par, "[", grid$Var1, ",", grid$Var2, "]")]%>% 
    as_draws_df() %>%
    data.frame() %>%
    reshape2::melt(id.vars = c(".chain", ".iteration", ".draw")) %>%
    mutate(row = stringi::stri_extract(variable, regex = "\\d+"),
           col = sapply(stringi::stri_extract_all(variable, regex = "\\d+"), function(x) x[2])) 
}

# To support tidybayes gather_draws for cmdstanr
tidy_draws.CmdStanMCMC <- function(model, ...) { return(as_draws_df(model$draws(...))) }

tidy_draws.CmdStanMCMC_mat <- function(model, ...) {
  tidy_draws.CmdStanMCMC(model, ...) %>%
    select(-`.chain`,-`.iteration`) %>%
    rename(draw = `.draw`) %>%
    melt("draw") %>%
    mutate(variable = stringi::stri_extract(variable, regex = "\\d+,\\d+"),
           variable_split = str_split(variable, ","),
           row = map_int(variable_split, function(x) as.integer(x[1])),
           col = map_int(variable_split, function(x) as.integer(x[2]))) %>%
    select(row, col, draw, value) 
}

# loo
loo.CmdStanMCMC.array <- function(model, ...) {
  loo_pars <- expand.grid(...) %>%
    unite("par", sep = ",") 
  loo <- model$post_warmup_draws[,,paste0("log_lik[",loo_pars$par,"]")] %>%
    loo()
  return(loo)
}

# Posterior summary
post_summary <- function(fit, pars) {
  fit$post_warmup_draws[,,pars] %>% 
    summarize_draws(mean, median, function(x) quantile2(x, probs = c(0.025, 0.975)), ess_bulk, Rhat) %>%
    mutate_if(is.numeric, round, 3)
}

# Sampler diagnostics
diagnostics <- function(fit) {
  np <- fit$post_warmup_sampler_diagnostics %>% 
    reshape2::melt() %>% 
    set_names(c("Iteration", "Chain", "Parameter", "Value")) %>%
    dplyr::select(Iteration, Parameter, Value, Chain)
  lp <- fit$post_warmup_draws[,,"lp__"] %>% reshape2::melt() %>% 
    dplyr::select(iteration, value, chain) %>% 
    set_names(c("Iteration", "Value", "Chain"))
  return(list(np = np, lp = lp))
}

# Plot posterior pairs
plot_post_pairs <- function(model, parameters) {
  thetas <- model$post_warmup_draws[,,parameters] %>%
    as_draws_df() %>%
    data.frame() %>%
    reshape2::melt(id.vars = c(".chain", ".iteration", ".draw")) %>%
    mutate(variable = stringi::stri_extract(variable, regex = "\\d")) %>%
    dplyr::select(variable, value, .draw) %>%
    spread(variable, value) %>%
    dplyr::select(-.draw)
  
  corr_pl <- ggpairs(thetas, 
                     lower = list(continuous = contours),
                     diag = list(continuous = wrap(hist_fun)),
                     upper = list(continuous = wrap(cor_fun, sz = text_size*5/14, stars = FALSE))) +
    theme_bw2()
  return(corr_pl)
}
