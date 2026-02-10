data {
  int<lower=1> N;
  int<lower=1> M;
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
  theta_raw ~ normal(0, 1);
  beta ~ normal(0, 0.3);
  Y ~ bernoulli_logit(theta[A] - theta[B] + beta);
}
