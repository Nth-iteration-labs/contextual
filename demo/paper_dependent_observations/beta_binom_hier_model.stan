data {
  int<lower=0> n_subjects;                          // items/subjects
  int<lower=0> n[n_subjects];                       // total trials
  int<lower=0> l[n_subjects];                       // total successes
}

parameters {
  real<lower=0, upper=1> phi;                       // population chance of success
  real<lower=1> kappa;                              // population concentration
  vector<lower=0, upper=1>[n_subjects] theta;       // chance of success
}

model {
  kappa ~ pareto(1, 1.5);                           // hyperprior
  theta ~ beta(phi * kappa, (1 - phi) * kappa);     // prior
  l ~ binomial(n, theta);                           // likelihood
}
