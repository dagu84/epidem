library(INLA)
library(dplyr)

# importing arguments from python function
args = commandArgs(trailingOnly = TRUE)
t_cutoff = as.integer(args[2])
data_path = args[1]

# loading the data
data = read.csv(data_path)
data$date = as.Date(data$date)

# apply cutoff
data = data %>%
  mutate(n_td_true=n_td, observed=(t + d) <= t_cutoff,
         n_td=ifelse(observed, n_td, NA), Y=n_td, Time=t, Delay=d, Time2=t, Delay2=d + 1,
         WeekOfYear=week_of_year) %>% filter(t <= t_cutoff)

# index of missing cells for INLA to predict
index.missing <- which(is.na(data$Y))

# half normal prior
half_normal_sd <- function(sigma) {
  paste("expression:
            sigma = ", sigma, ";
            precision = exp(log_precision);
            logdens = -0.5*log(2*pi*sigma^2) - 1.5*log_precision - 1/(2*precision*sigma^2);
            log_jacobian = log_precision;
            return(logdens + log_jacobian);", sep = "")}

# model formula
model_formula <- Y ~ 1 +
  f(Time, model = "rw1", constr = TRUE,
    hyper = list("prec" = list(prior = half_normal_sd(0.1)))) +
  f(Delay, model = "rw1", constr = TRUE,
    hyper = list("prec" = list(prior = half_normal_sd(1)))) +
  f(Time2, model = "rw1", constr = TRUE, replicate = Delay2,
    hyper = list("prec" = list(prior = half_normal_sd(0.1)))) +
  f(WeekOfYear, model = "rw2", cyclic = TRUE, constr = TRUE,
    hyper = list("prec" = list(prior = half_normal_sd(1))))

# fit model
output <- inla(
  model_formula,
  family = "nbinomial",
  data   = data,
  num.threads = 4,
  control.predictor = list(link = 1, compute = TRUE),
  control.compute   = list(config = TRUE),
  control.family    = list(
    hyper = list("theta" = list(prior = "loggamma", param = c(1, 0.1)))))

# posterior sampling — INLA version
n_sims <- 1000

# step 1: sample from approximate joint posterior
inla_samples_list <- inla.posterior.sample(n_sims, output)

# step 2: sample counts for missing cells from negative binomial
vector_samples <- lapply(
  inla_samples_list,
  function(x) {
    rnbinom(
      n    = length(index.missing),
      mu   = exp(x$latent[index.missing]),
      size = x$hyperpar[1])})

# step 3: reconstruct full grid and aggregate to weekly totals
weekly_list <- lapply(vector_samples, function(sample_vec) {
  data_aux                   <- data
  data_aux$Y[index.missing]  <- sample_vec
  data_aux %>%
    group_by(date) %>%
    summarise(Y = sum(Y), .groups = "drop")})

# build weekly samples matrix (weeks x simulations)
weekly_samples_mat <- sapply(weekly_list, function(df) df$Y)
rownames(weekly_samples_mat) <- as.character(weekly_list[[1]]$date)

# credible intervals
N_hat <- apply(weekly_samples_mat, 1, mean)
N_lo  <- apply(weekly_samples_mat, 1, quantile, probs = 0.025)
N_hi  <- apply(weekly_samples_mat, 1, quantile, probs = 0.975)

# summarise
inla_nowcast = data.frame(date  = as.Date(rownames(weekly_samples_mat)), N_hat = N_hat, N_lo  = N_lo, N_hi  = N_hi)

# export
output_dir = dirname(data_path)
write.csv(inla_nowcast, file.path(output_dir, paste0("inla_pred_", t_cutoff, ".csv")), row.names = FALSE)
write.csv(as.data.frame(weekly_samples_mat), file.path(output_dir, paste0("inla_posterior_", t_cutoff, ".csv")),
  row.names = TRUE)