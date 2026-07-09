library(mgcv)
library(dplyr)

# data import
data = read.csv('data/untrimmed_data.csv')

# cutoff
t_cutoff = 30

# applying cutoff
data = data %>%
  mutate(n_td_true = n_td, observed = (t + d) <= t_cutoff,
         n_td = ifelse(observed, n_td, NA)) %>% filter(t <= t_cutoff)

# GAM
model = gam(
  n_td ~ s(t, k=40, bs='ps', m=1) + s(d, k=10, bs='ps', m=1) + ti(d, t),
  family=nb(link='log'),
  data=data,
  subset=observed,
  method='REML')

# predict
data$pred = predict(model,newdata=data,type="response")

# posterior
beta_hat = coef(model)
V_beta = vcov(model, unconditional = TRUE)

beta_samples = rmvn(1000, beta_hat, V_beta)
Xp = predict(model, newdata=data, type="lpmatrix")
mu_samples = exp(Xp %*% t(beta_samples))

theta = model$family$getTheta(TRUE)
count_samples = matrix(rnbinom(length(mu_samples), mu=mu_samples, size=theta), nrow=nrow(mu_samples))
stopifnot(nrow(count_samples) == nrow(data))
count_samples[data$observed, ] = data$n_td_true[data$observed]

weekly_samples = rowsum(count_samples, data$date)
N_hat = apply(weekly_samples, 1, mean)
N_lo = apply(weekly_samples, 1, quantile, 0.025)
N_hi = apply(weekly_samples, 1, quantile, 0.975)

# DIAGNOSTICS
gam.check(model)
summary(model)


