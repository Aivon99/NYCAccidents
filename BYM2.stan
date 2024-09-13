functions {
    real icar_normal_lpdf(vector phi, int N, int[] node1, int[] node2) {
        return -0.5 * dot_self(phi[node1] - phi[node2]) + normal_lpdf(sum(phi) | 0, 0.001 * N);
    }
}
data {
  int<lower=0> N;
  int<lower=0> N_edges;
  int<lower=1, upper=N> node1[N_edges]; 
  int<lower=1, upper=N> node2[N_edges]; 
  int<lower=0> y[N]; 
  vector<lower=0>[N] E;
  int<lower=1> K; 
  matrix[N, K] x;
  real<lower=0> scaling_factor;
}

transformed data {
  vector[N] log_E = log(E);
}

parameters {
  real beta0; 
  vector[K] betas;
  real logit_rho; 
  vector[N] phi; 
  vector[N] theta;
  real<lower=0> sigma; 
}
//(sqrt(ρ/s)φ + sqrt(1 − ρ)θ )σ

transformed parameters {
  
  real<lower=0, upper=1> rho = inv_logit(logit_rho);
  vector[N] convolved_re = sqrt(rho / scaling_factor) * phi + sqrt(1 - rho) * theta;
}

model {
  y ~ poisson_log(log_E + beta0 + x * betas + convolved_re * sigma);
  beta0 ~ normal(0, 1);
  betas ~ normal(0, 1);
  logit_rho ~ normal(0, 1);
  sigma ~ normal(0, 1);
  theta ~ normal(0, 1); //uno perchè grafico è pienamente connesso 
  phi ~ icar_normal(N, node1, node2);
}

generated quantities {

  vector[N] eta = log_E + beta0 + x * betas + convolved_re * sigma;
  vector[N] mu = exp(eta);
  int y_rep[N];
  
  if (max(eta) > 20) { 
    print("max eta too big: ", max(eta));
    for (n in 1:N) y_rep[n] = -1;
  } else {
    for (n in 1:N) y_rep[n] = poisson_log_rng(eta[n]);
  }
   
}
