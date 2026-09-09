data {
  int<lower=1> N;
  int<lower=1> M;
  vector[N] prior_mean;
  vector<lower=0>[N] prior_sd;
  array[M] int<lower=1, upper=N> A;
  array[M] int<lower=1, upper=N> B;
  array[M] int<lower=0, upper=1> Y;
}
parameters {
  vector[N] theta_raw;
  real beta;
}
transformed parameters {
  vector[N] theta = theta_raw - mean(theta_raw);
}
model {
  theta_raw ~ normal(prior_mean, prior_sd);
  beta ~ normal(0, 0.3);
  Y ~ bernoulli_logit(theta[A] - theta[B] + beta);
}
