// E3 single-use joint-offset model; beta and epsilon are fixed data.
data {
  int<lower=1> N_H;
  int<lower=1> N_S;
  int<lower=0> M;
  matrix[N_H, N_H - 1] H_H;
  matrix[N_S, N_S - 1] H_S;
  array[M] int<lower=1, upper=N_H + N_S> A;
  array[M] int<lower=1, upper=N_H + N_S> B;
  array[M] int<lower=0, upper=1> Y;
  real beta;
  real<lower=0, upper=1> epsilon;
  real delta_mean;
  real<lower=0> delta_sd;
}
parameters {
  real delta;
  vector[N_H - 1] u_H;
  vector[N_S - 1] u_S;
}
transformed parameters {
  vector[N_H + N_S] theta;
  theta[1:N_H] = H_H * u_H;
  theta[(N_H + 1):(N_H + N_S)] = rep_vector(delta, N_S) + H_S * u_S;
}
model {
  delta ~ normal(delta_mean, delta_sd);
  u_H ~ std_normal();
  u_S ~ std_normal();
  if (M > 0) {
    for (m in 1:M) {
      real eta = (2 * Y[m] - 1) * (theta[A[m]] - theta[B[m]] + beta);
      if (epsilon == 0) {
        target += log_inv_logit(eta);
      } else if (epsilon == 1) {
        target += -log(2.0);
      } else {
        target += log_sum_exp(log1m(epsilon) + log_inv_logit(eta), log(epsilon) - log(2.0));
      }
    }
  }
}
