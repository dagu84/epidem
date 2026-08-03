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
knot$week_of_year = c(1,52)

# Forecast model
forecast_model = gam(
  n_td ~ s(t, k=40, bs='bs', m=c(3,2)) + s(d, k=10, bs='ps', m=1) +
    ti(d, t, bs=c('ps','bs'), m=list(1, c(3,2))) + s(week_of_year, k=20, bs='cc'),
  family=nb(link='log'),
  data=model_data,
  subset=observed,
  method='REML',
  knots=knot)


# Future t and cycled week_of_year (no date column needed)
future_t <- (max(model_data$t) + 1):(max(model_data$t) + forecast_horizon)

last_t <- max(model_data$t)
last_week <- model_data$week_of_year[model_data$t == last_t][1]
offset <- future_t - last_t
future_week_of_year <- ((last_week - 1 + offset) %% 52) + 1

newdata <- expand.grid(t = future_t, d = unique(model_data$d)) %>%
  left_join(tibble::tibble(t = future_t, week_of_year = future_week_of_year), by = "t")

# Predict (link scale, exponentiate for CIs)
p_train <- predict(forecast_model, model_data, type = "link", se.fit = TRUE)
model_data <- model_data %>%
  mutate(fit = exp(p_train$fit),
         lwr = exp(p_train$fit - 2 * p_train$se.fit),
         upr = exp(p_train$fit + 2 * p_train$se.fit))

p_fc <- predict(forecast_model, newdata, type = "link", se.fit = TRUE)
newdata <- newdata %>%
  mutate(fit = exp(p_fc$fit),
         lwr = exp(p_fc$fit - 2 * p_fc$se.fit),
         upr = exp(p_fc$fit + 2 * p_fc$se.fit))

# Aggregate across d
train_by_t <- model_data %>%
  group_by(t) %>%
  summarise(observed = sum(n_td_true, na.rm = TRUE),
            fit = sum(fit), lwr = sum(lwr), upr = sum(upr), .groups = "drop")

fc_by_t <- newdata %>%
  group_by(t) %>%
  summarise(fit = sum(fit), lwr = sum(lwr), upr = sum(upr), .groups = "drop")

# Plot
ggplot() +
  geom_ribbon(data = train_by_t, aes(x = t, ymin = lwr, ymax = upr),
              fill = "steelblue", alpha = 0.2) +
  geom_line(data = train_by_t, aes(x = t, y = fit), colour = "steelblue") +
  geom_point(data = train_by_t, aes(x = t, y = observed), colour = "black", size = 1) +
  geom_ribbon(data = fc_by_t, aes(x = t, ymin = lwr, ymax = upr),
              fill = "tomato", alpha = 0.2) +
  geom_line(data = fc_by_t, aes(x = t, y = fit), colour = "tomato") +
  geom_vline(xintercept = max(model_data$t), linetype = "dashed") +
  labs(x = "t", y = "n_td", title = "Fitted vs forecast") +
  theme_minimal()