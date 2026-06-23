library(mgcv)
library(dplyr)

# importing arguments from python function
args = commandArgs(trailingOnly = TRUE)
t_cutoff = as.integer(args[2])
data_path = args[1]

# loading the data
data = read.csv(data_path)
data$date = as.Date(data$date)

# applying cutoff
data = data %>%
  mutate(n_td_true = n_td, observed = (t + d) <= t_cutoff,
         n_td = ifelse(observed, n_td, NA)) %>% filter(t <= t_cutoff)

# GAM
model = gam(
  n_td ~ s(t, k=10) + s(d, k=10) + ti(d, t),
  family=nb(link='log'),
  data=data,
  subset=observed,
  method='REML')

# predict
data$pred = predict(model,newdata=data,type="response")
output_path = file.path(dirname(data_path), paste0("gam_pred", t_cutoff, ".csv"))
write.csv(data, output_path, row.names = FALSE)

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

# export
nowcast = data.frame(date = as.Date(rownames(weekly_samples)), N_hat = N_hat, N_lo  = N_lo, N_hi  = N_hi)

write.csv(nowcast,file.path(dirname(data_path), paste0("gam_CI_", t_cutoff, ".csv")), row.names = FALSE)

write.csv(as.data.frame(weekly_samples), file.path(dirname(data_path), paste0("gam_posterior_", t_cutoff, ".csv")), row.names = TRUE)


