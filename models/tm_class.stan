functions {
  // Discretization of the cumulative lognormal distribution function
  real diff_lnorm(real x, real mu, real sigma) {
    if (x == 0) {
      return lognormal_cdf(0.5, mu, sigma);
    } else {
      return lognormal_cdf(x+0.5, mu, sigma) - lognormal_cdf(x-0.5, mu, sigma);
    }
  }
}


data {
  int<lower=1> L; // number of locations
  int<lower=1> D; // number of days
  int<lower=1> S; // number of seeding days
  int<lower=0> new_cases[D,L]; // number of new cases (confirmed + suspected)
  int<lower=1> population[L]; // number of students per school
  real<lower=0> prop_absences[D,L]; // proportion of students being absent = absences / population
  real<lower=0> medianRest_mean[D+S,5]; // mean of estimated median reproduction number R in Solothurn
  int<lower=0,upper=1> maskmandate[D+S,L]; // mask mandate intervention
  int<lower=0,upper=1> airfilter[D+S,L]; // air filter intervention (only affected the study classes)
  real multiplier[L];  // multiplier = fmax(total cases / population, 1)
  real p_in_mu_m; // prior: location hyperparameter m in mu^p_IN ~ Normal(m, s)
  real p_in_mu_s; // prior: scale hyperparameter s in mu^p_IN ~ Normal(m, s)
  real p_in_sigma_m; // prior: location hyperparameter m in sigma^p_IN ~ Normal(m, s)
  real p_in_sigma_s; /// prior: scale hyperparameter m in sigma^p_IN ~ Normal(m, s)
}

transformed data {
  int DS = D + S;
}


parameters {
  real<lower=0> invphi_N; // inverse over-dispersion parameter for negative binomial distr. of new cases
  real alpha; // inv_logit(alpha_0) constant proportion of infections
  real theta_M; // intervention effect of mask mandates
  real theta_A; // intervention effect of air filters
  real<lower=0> tau; // variation between classes 
  vector[L] theta_l; // class-specific effect of interventions
  real gamma[2]; // effects of control variables
  real mu_p_in; // log mean in p_IN ~ Lognormal(mu, sigma)
  real<lower=0> sigma_p_in; // log standard deviation in p_IN ~ Lognormal(mu, sigma)
}


transformed parameters {
  real<lower=0> phi_N = inv_square(invphi_N); // over-dispersion parameter for negative binomial distr. of new cases
  matrix<lower=0>[DS,L] mu_new_cases; // expected number of new cases
  matrix<lower=0>[DS,L] mu_new_infections; // expected number of new infections
  matrix[DS,L] logit_prop_infections; // proportion of new infections 
  matrix<lower=0>[DS,L] susceptibles; // number of new infections (latent / unobserved)
  matrix<lower=0>[DS,L] mu_cum_infections; // cumulative number of expected infections
  matrix<lower=0>[DS,L] mu_cum_cases; // cumulative number of expected cases
  vector<lower=0>[DS] p_in; // probability distribution for incubation period
  
  // Compute discretized p_IN distribution
    for (k in 1:DS) { 
      p_in[k] = diff_lnorm(DS-k, mu_p_in, sigma_p_in);
    }
  
    for (l in 1:L) {
      susceptibles[1,l] = population[l];
      logit_prop_infections[1,l] = alpha + (theta_M + theta_l[l]) * maskmandate[1,l] + theta_A * airfilter[1,l] + gamma[2] * medianRest_mean[1,l];
      mu_new_infections[1,l] = susceptibles[1,l] * inv_logit(logit_prop_infections[1,l]);
      mu_cum_infections[1,l] = mu_new_infections[1,l];
      mu_new_cases[1,l] = mu_new_infections[1,l] * p_in[DS];
      mu_cum_cases[1,l] = mu_new_cases[1,l];
      for (d in 2:S) {
        susceptibles[d,l] = population[l] - mu_cum_infections[d-1,l];
        logit_prop_infections[d,l] = alpha + (theta_M + theta_l[l]) * maskmandate[d,l] + theta_A * airfilter[d,l] + gamma[2] * medianRest_mean[d,l];
        mu_new_infections[d,l] = susceptibles[d,l] * inv_logit(logit_prop_infections[d,l]);
        mu_cum_infections[d,l] = mu_cum_infections[d-1,l] + mu_new_infections[d,l];
        mu_new_cases[d,l] = dot_product(mu_new_infections[1:d,l], tail(p_in, d));
        mu_cum_cases[d,l] = mu_cum_cases[d-1,l] + mu_new_cases[d,l];
      }
      for (d in (S+1):DS) {
        susceptibles[d,l] = population[l] - mu_cum_infections[d-1,l];
        logit_prop_infections[d,l] = alpha + (theta_M + theta_l[l]) * maskmandate[d,l] + theta_A * airfilter[d,l] + gamma[1] * prop_absences[d-S,l] + gamma[2] * medianRest_mean[d,l];
        mu_new_infections[d,l] = susceptibles[d,l] * inv_logit(logit_prop_infections[d,l]); 
        mu_cum_infections[d,l] = mu_cum_infections[d-1,l] + mu_new_infections[d,l];
        mu_new_cases[d,l] = dot_product(mu_new_infections[1:d,l], tail(p_in, d));
        mu_cum_cases[d,l] = mu_cum_cases[d-1,l] + mu_new_cases[d,l];
      }
    }
}


model {
  // priors
  invphi_N ~ normal(0., 1.);
  alpha ~ student_t(3., -3., 1.);
  mu_p_in ~ normal(p_in_mu_m, p_in_mu_s);
  sigma_p_in ~ normal(p_in_sigma_m, p_in_sigma_s);
  theta_M ~ student_t(7., 0., 2.5);
  theta_A ~ student_t(7., 0., 1.);
  tau ~ normal(0., 1.);
  theta_l ~ normal(0., tau);
  gamma ~ student_t(7., 0., 2.5);

  // likelihood and time-varying priors
  for (l in 1:L) {
    new_cases[1:D,l] ~ neg_binomial_2(mu_new_cases[(S+1):DS,l] * multiplier[l], phi_N);
  }
}

generated quantities {
  // infections
  matrix[DS,L] susceptibles_base = rep_matrix(0., DS, L);
  matrix[DS,L] logit_prop_infections_base = rep_matrix(0., DS, L);
  matrix[DS,L] mu_new_infections_base = rep_matrix(0., DS, L);
  matrix[DS,L] mu_cum_infections_base = rep_matrix(0., DS, L);
  // infections without masks
  matrix[DS,L] susceptibles_Masks = rep_matrix(0., DS, L);
  matrix[DS,L] logit_prop_infections_Masks = rep_matrix(0., DS, L);
  matrix[DS,L] mu_new_infections_Masks = rep_matrix(0., DS, L);
  matrix[DS,L] mu_cum_infections_Masks = rep_matrix(0., DS, L);
  matrix[DS,L] avoided_infections_Masks = rep_matrix(0., DS, L);

  // log-likelihood
  matrix[D,L] log_lik;
  for (l in 1:L) {
    for (d in 1:D) {
      log_lik[d,l] = neg_binomial_2_lpmf(new_cases[d,l] | mu_new_cases[S+d,l], phi_N);
    }
    susceptibles_base[1,l] = population[l];
    logit_prop_infections_base[1,l] = alpha + (theta_M + theta_l[l]) * maskmandate[1,l] + theta_A * airfilter[1,l] + gamma[2] * medianRest_mean[1,l];
    mu_new_infections_base[1,l] = susceptibles_base[1,l] * inv_logit(logit_prop_infections_base[1,l]);
    mu_cum_infections_base[1,l] = mu_new_infections_base[1,l];

    susceptibles_Masks[1,l] = population[l];
    logit_prop_infections_Masks[1,l] = alpha + theta_A * airfilter[1,l] + gamma[2] * medianRest_mean[1,l];
    mu_new_infections_Masks[1,l] = susceptibles_Masks[1,l] * inv_logit(logit_prop_infections_Masks[1,l]);
    mu_cum_infections_Masks[1,l] = mu_new_infections_Masks[1,l];
    
    avoided_infections_Masks[1,l] = mu_new_infections_base[1,l] - mu_new_infections_Masks[1,l];


    for (d in 2:S) {
      susceptibles_base[d,l] = population[l] - mu_cum_infections_base[d-1,l];
      logit_prop_infections_base[d,l] = alpha + (theta_M + theta_l[l]) * maskmandate[d,l] + theta_A * airfilter[d,l] + gamma[2] * medianRest_mean[d,l];
      mu_new_infections_base[d,l] = susceptibles_base[d,l] * inv_logit(logit_prop_infections_base[d,l]);
      mu_cum_infections_base[d,l] = mu_cum_infections_base[d-1,l] + mu_new_infections_base[d,l];

      susceptibles_Masks[d,l] = population[l] - mu_cum_infections_Masks[d-1,l];
      logit_prop_infections_Masks[d,l] = alpha + theta_A * airfilter[d,l] + gamma[2] * medianRest_mean[d,l];
      mu_new_infections_Masks[d,l] = susceptibles_Masks[d,l] * inv_logit(logit_prop_infections_Masks[d,l]);
      mu_cum_infections_Masks[d,l] = mu_cum_infections_Masks[d-1,l] + mu_new_infections_Masks[d,l];
      
      avoided_infections_Masks[d,l] = mu_new_infections_base[d,l] - mu_new_infections_Masks[d,l];
    }
    for (d in (S+1):DS) {
      susceptibles_base[d,l] = population[l] - mu_cum_infections_base[d-1,l];
      logit_prop_infections_base[d,l] = alpha + (theta_M + theta_l[l]) * maskmandate[d,l] + theta_A * airfilter[d,l] + gamma[1] * prop_absences[d-S,l] + gamma[2] * medianRest_mean[d,l];
      mu_new_infections_base[d,l] = susceptibles_base[d,l] * inv_logit(logit_prop_infections_base[d,l]);
      mu_cum_infections_base[d,l] = mu_cum_infections_base[d-1,l] + mu_new_infections_base[d,l];

      susceptibles_Masks[d,l] = population[l] - mu_cum_infections_Masks[d-1,l];
      logit_prop_infections_Masks[d,l] = alpha + theta_A * airfilter[d,l] + gamma[1] * prop_absences[d-S,l] + gamma[2] * medianRest_mean[d,l];
      mu_new_infections_Masks[d,l] = susceptibles_Masks[d,l] * inv_logit(logit_prop_infections_Masks[d,l]);
      mu_cum_infections_Masks[d,l] = mu_cum_infections_Masks[d-1,l] + mu_new_infections_Masks[d,l];

      avoided_infections_Masks[d,l] = mu_new_infections_base[d,l] - mu_new_infections_Masks[d,l];
    }
  }
}

