data {
  array[8] int <lower=0>  ntrials;
  array[8, 2] int<lower=0>      y;
}
parameters {
  real<lower=0, upper=1> p_c;
  real<lower=0, upper=1> p_u;
  real<lower=0, upper=1> a;
  real<lower=0, upper=1> g;
}
transformed parameters {
  simplex[2] theta[8];
  theta[1, 1] = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*g;
  theta[1, 2] = (1-p_c)*(1-p_u)*(1-a)*(1-g); 
  theta[2, 1] = (1-p_c)*(1-p_u)*(1-a)*g;
  theta[2, 2] = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*(1-g);
  theta[3, 1] = (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*g;
  theta[3, 2] = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*(1-a)*(1-g);
  theta[4, 1] = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*(1-a)*g;
  theta[4, 2] = (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*(1-g);
  theta[5, 1] = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*g;
  theta[5, 2] = (1-p_c)*(1-p_u)*(1-a)*(1-g);
  theta[6, 1] = (1-p_c)*(1-p_u)*(1-a)*g;
  theta[6, 2] = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*(1-g);
  theta[7, 1] = p_c + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*g;
  theta[7, 2] = (1-p_c)*p_u + (1-p_c)*(1-p_u)*(1-a)*(1-g);
  theta[8, 1] = (1-p_c)*p_u + (1-p_c)*(1-p_u)*(1-a)*g;
  theta[8, 2] = p_c + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*(1-g);
}
model {
  target += beta_lpdf(p_c | 2, 2);
  target += beta_lpdf(p_u | 2, 4);
  target += beta_lpdf(a | 2, 4);
  target += beta_lpdf(g | 2, 2);
  for (i in 1:8) 
    target += multinomial_lpmf(y[i, ] | theta[i, ]);
}
generated quantities {
  array[8, 2] int sim_y;
  for (i in 1:8)
    sim_y[i, ] = multinomial_rng(theta[i, ], ntrials[i]);
}

