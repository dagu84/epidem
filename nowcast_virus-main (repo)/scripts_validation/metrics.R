rm(list = ls())
gc()

if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('scales')) {install.packages('scales')};library('scales')
if(!require ('Metrics')) {install.packages('Metrics')};library('Metrics')
if(!require ('scoringutils')) {install.packages('scoringutils')};library('scoringutils')
if(!require ('readxl')) {install.packages('readxl')};library('readxl')

####reading last 4 weeks nowcst data

df<-read.csv("validation/all_models_last_4weeks_nowcast.csv")
  
df2<- df%>%
  select(metodo, epiyear, epiweek, agente, DS_UF_SIGLA, data_base, Median, LI, LS, LIb, LSb, obs)%>%
  dplyr::rename(observed=obs,
                "0.5"=Median,
                "0.025"=LI,
                "0.975"=LS,
                "0.25"=LIb,
                "0.75"=LSb) %>%
  pivot_longer(names_to="quantile_level", values_to = "predicted", cols = c(7:11))


df2$quantile_level<-as.numeric(df2$quantile_level)

modelo_full<- df2 %>% 
  as_forecast_quantile() 

df_score<-score(modelo_full, get_metrics(modelo_full, select=c("wis","overprediction","underprediction","dispersion", "ae_median"))) 

df_cov<- df %>%
  dplyr::  mutate(cov_50= case_when(obs>=LIb & obs<=LSb ~ 1,
                                    TRUE ~ 0),
                  cov_95= case_when(obs>=LI & obs<=LS ~ 1,
                                    TRUE ~ 0)) %>%
  select(metodo, agente, epiweek, epiyear, DS_UF_SIGLA, data_base, cov_50,cov_95) 

valida_join<-full_join(df_score, df_cov , c("metodo", "agente", "DS_UF_SIGLA", "epiyear", "epiweek", "data_base"))


write.csv(valida_join, "validation/metrics_nowcast_inla_gam.csv", row.names = FALSE)
