data {
  int<lower=1> N;
  int<lower=1> M;
  array[M] int<lower=1, upper=N> A;
  array[M] int<lower=1, upper=N> B;
  array[M] int<lower=0, upper=1> Y;
}
parameters {
  vector[N] theta_raw;
  real<lower=0, upper=1> epsilon;
  real beta;
}
transformed parameters {
  vector[N] theta = theta_raw - mean(theta_raw);
}
model {
  theta_raw ~ normal(0, 1);
  epsilon ~ beta(2, 20);
  beta ~ normal(0, 0.3);
  for (m in 1:M) {
    real d = theta[A[m]] - theta[B[m]] + beta;
    real p = (1 - epsilon) * inv_logit(d) + epsilon * 0.5;
    Y[m] ~ bernoulli(p);
  }
}
