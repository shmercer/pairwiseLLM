data {
  int<lower=1> N_cross;
  array[N_cross] int<lower=0, upper=1> y_spoke;
  vector[N_cross] hub_ref_cross;
  vector[N_cross] spoke_ref_cross;
  array[N_cross] int<lower=1> cross_hub_idx;
  array[N_cross] int<lower=1> cross_spoke_idx;
  vector[N_cross] beta_signed;
  real<lower=0, upper=1> epsilon;
  real beta_within;
  int<lower=0, upper=1> joint_used;
  int<lower=0, upper=1> estimate_hub;
  int<lower=0, upper=1> use_scale;
  int<lower=0> N_hub;
  int<lower=0> N_spoke;
  vector[N_hub] hub_ref;
  vector[N_spoke] spoke_ref;
  vector[N_hub] hub_prior_center;
  vector<lower=1e-8>[N_hub] hub_prior_sd;
  vector<lower=1e-8>[N_spoke] spoke_prior_sd;
  int<lower=0> N_within_hub;
  array[N_within_hub] int<lower=1> hub_within_A_idx;
  array[N_within_hub] int<lower=1> hub_within_B_idx;
  array[N_within_hub] int<lower=0, upper=1> hub_within_y_A;
  int<lower=0> N_within_spoke;
  array[N_within_spoke] int<lower=1> spoke_within_A_idx;
  array[N_within_spoke] int<lower=1> spoke_within_B_idx;
  array[N_within_spoke] int<lower=0, upper=1> spoke_within_y_A;
}

parameters {
  vector[estimate_hub * N_hub] theta_hub_free;
  vector[joint_used * N_spoke] theta_spoke_free;
  vector[use_scale] log_alpha_free;
  real delta;
}

transformed parameters {
  vector[N_hub] theta_hub = hub_ref;
  vector[N_spoke] theta_spoke = spoke_ref;
  real log_alpha = 0;
  real alpha = 1;

  if (estimate_hub == 1) {
    theta_hub = theta_hub_free;
  }
  if (joint_used == 1) {
    theta_spoke = theta_spoke_free;
  }
  if (use_scale == 1) {
    log_alpha = log_alpha_free[1];
    alpha = exp(log_alpha);
  }
}

model {
  delta ~ normal(0, 1);
  if (use_scale == 1) {
    log_alpha_free[1] ~ normal(0, 0.2);
  }
  if (estimate_hub == 1) {
    theta_hub ~ normal(hub_prior_center, hub_prior_sd);
  }
  if (joint_used == 1) {
    theta_spoke ~ normal(spoke_ref, spoke_prior_sd);
  }

  for (n in 1:N_cross) {
    real hub_n = estimate_hub == 1 ? theta_hub[cross_hub_idx[n]] : hub_ref_cross[n];
    real spoke_n = joint_used == 1 ? theta_spoke[cross_spoke_idx[n]] : spoke_ref_cross[n];
    real eta = delta + alpha * spoke_n - hub_n + beta_signed[n];
    real p = (1 - epsilon) * inv_logit(eta) + epsilon * 0.5;
    y_spoke[n] ~ bernoulli(p);
  }

  if (estimate_hub == 1) {
    for (n in 1:N_within_hub) {
      real eta = theta_hub[hub_within_A_idx[n]] - theta_hub[hub_within_B_idx[n]] + beta_within;
      real p = (1 - epsilon) * inv_logit(eta) + epsilon * 0.5;
      hub_within_y_A[n] ~ bernoulli(p);
    }
  }

  if (joint_used == 1) {
    for (n in 1:N_within_spoke) {
      real eta = alpha *
        (theta_spoke[spoke_within_A_idx[n]] - theta_spoke[spoke_within_B_idx[n]]) +
        beta_within;
      real p = (1 - epsilon) * inv_logit(eta) + epsilon * 0.5;
      spoke_within_y_A[n] ~ bernoulli(p);
    }
  }
}
