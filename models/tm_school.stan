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
  int<lower=0> new_cases[D,L]; // number of new cases
  int<lower=1> population[L]; // number of students per school
  real<lower=0> prop_contagious[D,L]; // number of potentially contagious = (population - recovered) / population
  real<lower=0> prop_absences[D,L]; // proportion of students being absent = absences / population
  real<lower=0> medianRest_mean[D+S,2]; // mean of estimated median reproduction number R in Solothurn
  real<lower=0> medianRest_sd[D+S,2]; // sd of estimated median reproduction number R in Solothurn
  int<lower=0,upper=1> maskmandate[D+S,L]; // mask mandate intervention
  int<lower=0,upper=1> airfilter[D+S,L]; // air filter intervention (only affected the study classes)
  int<lower=0,upper=1> weekend[D+S,L]; // binary indicator for weekend
  real p_in_mu_m; // prior: location hyperparameter m in mu^p_IN ~ Normal(m, s)
  real p_in_mu_s; // prior: scale hyperparameter s in mu^p_IN ~ Normal(m, s)
  real p_in_sigma_m; // prior: location hyperparameter m in sigma^p_IN ~ Normal(m, s)
  real p_in_sigma_s; /// prior: scale hyperparameter m in sigma^p_IN ~ Normal(m, s)
}

transformed data {
  int DS = D + S;
}


parameters {
  real alpha[L]; // inv_logit(alpha_0) constant proportion of infections
  real vega; // weekend effect
  real theta_M[L]; // intervention effect of mask mandates
  real theta_A[L]; // intervention effect of air filters
  real gamma[3]; // effects of control variables
  matrix<lower=0>[DS,L] medianR; // median reproduction number
  matrix[D,L] logit_prop_infections; // proportion of new infections
  real<lower=0> sigma; // sd of prop. of new infections
  real mu_p_in; // log mean in p_IN ~ Lognormal(mu, sigma)
  real<lower=0> sigma_p_in; // log standard deviation in p_IN ~ Lognormal(mu, sigma)
}


transformed parameters {
  matrix<lower=0>[DS,L] mu_new_cases; // expected number of new cases
  matrix<lower=0>[DS,L] mu_new_infections; // expected number of new infections
  matrix[DS,L] mu_logit_prop_infections; // proportion of new infections 
  matrix<lower=0>[DS,L] mu_susceptibles; // number of new infections (latent / unobserved)
  matrix<lower=0>[DS,L] mu_cum_infections; // cumulative number of expected infections
  matrix<lower=0>[DS,L] mu_cum_cases; // cumulative number of expected cases
  vector<lower=0>[DS] p_in; // probability distribution for incubation period
  
  // Compute discretized p_IN distribution
    for (k in 1:DS) { 
      p_in[k] = diff_lnorm(DS-k, mu_p_in, sigma_p_in);
    }
  
    for (l in 1:L) {
      mu_susceptibles[1,l] = population[l];
      mu_logit_prop_infections[1,l] = alpha[l] + vega * weekend[1,l] + theta_M[l] * maskmandate[1,l] + theta_A[l] * airfilter[1,l] + gamma[3] * (medianR[1,l]-1);
      mu_new_infections[1,l] = mu_susceptibles[1,l] * inv_logit(mu_logit_prop_infections[1,l]);
      mu_cum_infections[1,l] = mu_new_infections[1,l];
      mu_new_cases[1,l] = mu_new_infections[1,l] * p_in[DS];
      mu_cum_cases[1,l] = mu_new_cases[1,l];
      for (d in 2:S) {
        mu_susceptibles[d,l] = population[l] - mu_cum_infections[d-1,l];
        mu_logit_prop_infections[d,l] = alpha[l] + vega * weekend[d,l] + theta_M[l] * maskmandate[d,l] + theta_A[l] * airfilter[d,l] + gamma[3] * (medianR[d,l]-1);
        mu_new_infections[d,l] = mu_susceptibles[d,l] * inv_logit(mu_logit_prop_infections[d,l]);
        mu_cum_infections[d,l] = mu_cum_infections[d-1,l] + mu_new_infections[d,l];
        mu_new_cases[d,l] = dot_product(mu_new_infections[1:d,l], tail(p_in, d));
        mu_cum_cases[d,l] = mu_cum_cases[d-1,l] + mu_new_cases[d,l];
      }
      for (d in (S+1):DS) {
        mu_susceptibles[d,l] = population[l] - mu_cum_infections[d-1,l];
        mu_logit_prop_infections[d,l] = alpha[l] + vega * weekend[d,l] + theta_M[l] * maskmandate[d,l] + theta_A[l] * airfilter[d,l] + gamma[1] * prop_contagious[d-S,l] + gamma[2] * prop_absences[d-S,l] + gamma[3] * (medianR[d,l]-1);
        mu_new_infections[d,l] = mu_susceptibles[d,l] * inv_logit(logit_prop_infections[d-S,l]);
        mu_cum_infections[d,l] = mu_cum_infections[d-1,l] + mu_new_infections[d,l];
        mu_new_cases[d,l] = dot_product(mu_new_infections[1:d,l], tail(p_in, d));
        mu_cum_cases[d,l] = mu_cum_cases[d-1,l] + mu_new_cases[d,l];
      }
    }
}


model {
  // priors
  alpha ~ student_t(7., -4., 2.);
  vega ~ student_t(3., 0., 1.);
  theta_M ~ student_t(3., 0., 1.);
  theta_A ~ student_t(3., 0., 1.);
  gamma ~ student_t(3., 0., 1.);
  mu_p_in ~ normal(p_in_mu_m, p_in_mu_s);
  sigma_p_in ~ normal(p_in_sigma_m, p_in_sigma_s);
  sigma ~ student_t(5., 0., 1.);
  
  // likelihood and time-varying priors
  for (l in 1:L) {
    medianR[1:DS,l] ~ normal(medianRest_mean[1:DS,l], medianRest_sd[1:DS,l]);
    logit_prop_infections[1:D,l] ~ normal(mu_logit_prop_infections[(S+1):DS,l], sigma);
    new_cases[1:D,l] ~ poisson(mu_new_cases[(S+1):DS,l]);
  }
}

// generated quantities {
//   // log-likelihood
//   matrix[D,L] log_lik;
//   for (l in 1:L) {
//     for (d in 1:D) {
//       log_lik[d,l] = poisson_lpmf(new_cases[d,l] | mu_new_cases[S+d,l]);
//     }
//   }
// }

