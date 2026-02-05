data {
  int<lower=1> N;
  int<lower=1> K;
  array[K] int<lower=1, upper=N> A;
  array[K] int<lower=1, upper=N> B;
  array[K] int<lower=0, upper=1> Y;
}
parameters {
  vector[N] theta_raw;
  real<lower=0, upper=1> epsilon;
}
transformed parameters {
  vector[N] theta;
  theta = theta_raw - mean(theta_raw);
}
model {
  vector[K] p;
  theta_raw ~ normal(0, 1);
  epsilon ~ beta(2, 20);
  for (k in 1:K) {
    p[k] = (1 - epsilon) * inv_logit(theta[A[k]] - theta[B[k]]) + epsilon * 0.5;
  }
  Y ~ bernoulli(p);
}
