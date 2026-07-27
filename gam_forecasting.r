# packages
library('ggplot2')
library('dplyr')
library('mgcv')

# Data
model_data = read.csv('data/untrimmed_data.csv')
model_data = model_data %>%
  mutate(n_td_true = n_td,
         observed = (t + d) <= end_cutoff & (t + d) >= start_cutoff,
         n_td = ifelse(observed, n_td, NA)) %>%
  filter(t <= end_cutoff, t >= start_cutoff)

# GAM
# Original model
orig_model = gam(
  n_td ~ s(t, k=40, bs='ps', m=1) + s(d, k=10, bs='ps', m=1) + ti(d, t),
  family=nb(link='log'),
  data=model_data,
  subset=observed,
  method='REML')

# Knots
forecast_horizon = 4
buffer = 1
knot = list(t = c(min(model_data$t) - buffer,
                  min(model_data$t),
                  max(model_data$t),
                  max(model_data$t) + forecast_horizon))

# Forecast model
forecast_model = gam(
  n_td ~ s(t, k=40, bs='bs', m=c(3,2)) + s(d, k=10, bs='ps', m=1) + ti(d, t, bs=c('ps','bs'), m=list(1, c(3,2))),
  family=nb(link='log'),
  data=model_data,
  subset=observed,
  method='REML',
  knots=knot)

future_t = (max(model_data$t) + 1):(max(model_data$t) + forecast_horizon)
newdata = expand.grid(t = future_t, d = unique(model_data$d))

preds = predict(forecast_model, newdata, type = "response", se.fit = TRUE)
write.csv(preds, 'data/gam_forecast_pred.csv')
