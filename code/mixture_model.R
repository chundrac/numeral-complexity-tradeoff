require(rstan)
require(loo)

df <- read.csv('complexity_measures.tsv',sep='\t')

model_code <- "data {
  int<lower=0> N;
  int lexicon[N];
  real msc[N];
  int<lower=1> K;
}
parameters {
  real beta_0[K];
  ordered[K] beta;
  real<lower=0> sigma[K];
}
transformed parameters {
  vector[K] log_lik_i[N];
  vector[N] log_lik;
  for (i in 1:N) {
    for (k in 1:K) {
      log_lik_i[i,k] = -log(K) + lognormal_lpdf(msc[i]|beta_0[k] + beta[k]*lexicon[i],sigma[k]);
    }
    log_lik[i] = log_sum_exp(log_lik_i[i,]);
  }
}
model {
  beta_0 ~ normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  target += sum(log_lik);
}
generated quantities {
  int<lower=1,upper=K> z[N];
  for (i in 1:N) {
    z[i] = categorical_logit_rng(log_lik_i[i,]);
  }
}"

n.iter = 2000

fits.broad <- list()

for (K in 1:2) {
  
  data.list <- list(
    N = nrow(df),
    lexicon = df$vocab_size_broad,
    msc = df$ms_complexity_broad,
    K = K
  )
  
  fit <- stan(model_code=model_code,data=data.list,iter=n.iter)
  
  while(max(na.omit(summary(fit)$summary[,'Rhat'])) > 1.1) {
    fit <- stan(model_code=model_code,data=data.list,iter=n.iter)
  }
  
  fits.broad[[K]] <- fit
  
}

loo(fits.broad[[1]])

loo(fits.broad[[2]])

loo_compare(loo(fits.broad[[1]]),loo(fits.broad[[2]]))

loo_model_weights(list(loo(fits.broad[[1]]),loo(fits.broad[[2]])))

fits.narrow <- list()

for (K in 1:2) {
  
  data.list <- list(
    N = nrow(df),
    lexicon = df$vocab_size_narrow,
    msc = df$ms_complexity_narrow,
    K = K
  )
  
  fit <- stan(model_code=model_code,data=data.list,iter=n.iter)
  
  while(max(na.omit(summary(fit)$summary[,'Rhat'])) > 1.1) {
    fit <- stan(model_code=model_code,data=data.list,iter=n.iter)
  }
  
  fits.narrow[[K]] <- fit
  
}

loo(fits.narrow[[1]])

loo(fits.narrow[[2]])

loo_compare(loo(fits.narrow[[1]]),loo(fits.narrow[[2]]))

loo_model_weights(list(loo(fits.narrow[[1]]),loo(fits.narrow[[2]])))

df$component_broad <- apply(extract(fits.broad[[2]])$z-1,2,mean)

df$component_narrow <- apply(extract(fits.narrow[[2]])$z-1,2,mean)

save.image('model_fits.Rstan')

write.csv(df, file='complexity_measures_w_component.tsv', row.names = F, quote = F, sep = '\t')