files <- list.files(
  path = "Dados/",
  pattern = "casos",
  full.names = TRUE
)

###lendo as bases

dados <- lapply(files, function(f) {
  vroom::vroom(f) %>%
    filter(fx_etaria == "Total", epiyear >= 2025) %>%
    select(SG_UF_NOT, DS_UF_SIGLA, epiyear, epiweek,
           SARS2, VSR, RINO, FLU_A, METAP, FLU_B)
}) %>%
  bind_rows(.id = "a") %>%
  mutate(dt_start = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  group_by(a) %>%
  mutate(dt_release = max(dt_start, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(dt_start>= min(dt_release)) %>%
  filter(!is.na(DS_UF_SIGLA)) %>%
  select(dt_start, dt_release,
         SARS2, VSR, RINO, FLU_A, METAP,FLU_B,
         SG_UF_NOT, DS_UF_SIGLA) %>%
  
  pivot_longer(
    cols = c(SARS2, VSR, RINO, FLU_A, FLU_B, METAP),
    names_to = "Virus",
    values_to = "cases"
  )


######Tratando dados

##parametros
wdw<-30
Dmax<-10
K<-0 ###Forecaster paremetr

uf_sigla <- sort(unique(dados$DS_UF_SIGLA))
virus<- sort(unique(dados$Virus))


lista<-list()
lista2<-list()

#i<-"SP"

  
for (i in uf_sigla) {
  
dataset<-dados %>%
        filter(DS_UF_SIGLA==i)

Tmax <- max(dataset |>
              dplyr::pull(var = dt_start))

data.inla <- dataset |> 
  dplyr::rename(date_release = dt_release,
                date_onset = dt_start,
                cases = cases,
                agente=Virus) |>
  dplyr::select(agente, date_onset,date_release,  cases) |>
  dplyr::filter(date_onset>=min(date_release)) |>
  dplyr::arrange(agente, date_onset, date_release) |>
  dplyr::group_by(agente, date_onset) |>
  dplyr::mutate(delay = row_number() - 1) |>  
  dplyr::arrange(delay, .by_group = TRUE) |>
  dplyr::mutate(
    Y = cases - lag(cases, default = 0),
    Y = pmax(Y, 0)
  ) |>
  dplyr::ungroup() |>
  ## Filter for dates
  dplyr::filter(date_onset >= Tmax - 7 * wdw,
                delay <= Dmax) 

#if(K==0){
  dates <- range(data.inla |>
                   dplyr::pull(var = date_onset))
#} else {
  ## This is done to explicitly say for the forecast part that its date of onset is the present date
 # date_k <- max(data.inla$date_onset) + 7*K
#  dates <- range(data.inla$date_onset, date_k)
#}
  
  ## To make an auxiliary date table with each date plus an amount of dates  to forecast
  tbl.date.aux <- tibble::tibble(
    date_onset = seq(dates[1], dates[2], by = 7)
  )  |>
    tibble::rowid_to_column(var = "Time")
  
  ## Joining auxiliary date tables
  data.inla <- data.inla  |>
    dplyr::left_join(tbl.date.aux)
  
  ## Time maximum to be considered
  Tmax.id <- max(data.inla$Time)
  
  #if(missing(age_col)){
    tbl.NA <-
      expand.grid(Time = 1:(Tmax.id+K),
                  delay = 0:Dmax,
                  agente=unique(data.inla$agente))  |>
      dplyr::left_join(tbl.date.aux, by = "Time")
#  } else{
#    tbl.NA <-
#      expand.grid(Time = 1:(Tmax.id+K),
  #                delay = 0:Dmax,
 #                 fx_etaria = unique(data.inla$fx_etaria),
  #                agente=unique(data.inla$agente)
  #    ) |>
 #     dplyr::left_join(tbl.date.aux, by = "Time")
 # }
  
  
  
  
  
  #if(missing(age_col)){
    data.inla <- data.inla  |>
      dplyr::full_join(tbl.NA) |>  #View()
      dplyr::mutate(
        Y = ifelse(Time + delay > Tmax.id, as.numeric(NA), Y),
        ## If Time + Delay is greater than Tmax, fill with NA
        Y = ifelse(is.na(Y) & Time + delay <= Tmax.id, 0, Y ),
        ## If Time + Delay is smaller than Tmax AND Y is NA, fill 0
      )  |>
      dplyr::arrange(agente, Time, delay) |>
      dplyr::rename(dt_event = date_onset) |>
      tidyr::drop_na(delay)
# 
  
dataset<-data.inla
####Fazendo o 
  
  index.missing <- which(is.na(dataset$Y))
  
  dataset <- dataset |>
   dplyr::mutate(
      agente.num = factor(agente))

  
  #output1 <- mgcv::gam(Y ~ 1 + s(Time, agente.num, bs = "fs") +
  #                       s(delay), ,
   #                    family = "nb", data = dataset )
  
  output0 <- mgcv::gam(Y ~ 1 + s(Time, agente.num, bs="fs") +
                         s(delay, agente.num, bs="fs"),
                       family = "nb", data = dataset)
  
  str(dataset)
  
AIC(output0)
#AIC(output1)

output <- list()

betas.p <- mgcv::rmvn(n = 1000, coef(output0), output0$Vp)


# Step 2: Get the design matrix (for the predictive values)
Xp <- predict(output0, type = "lpmatrix", newdata = dataset[index.missing,])

# Step 3: Get the samples from the linear terms
eta.samples <- Xp %*% t(betas.p)


n.missing = nrow(eta.samples)

# Step 4: Sampling the missing triangle (fixing the negative binomial hyperparameter)
theta.nb <- output0$family$getTheta(T) # NegBin hyper parameter

# Step 5: Do the same as we did in INLA (sampling the missing triangle)
vector.samples <- lapply(X = 1:1000,
                         FUN = function(x)
                           rnbinom(n = n.missing, mu = exp(eta.samples[,x]), size = theta.nb))

## Step 6: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}

gg.agente <- function(x, dados, idx){
  # Workaround check
  Y <- Time <- dt_event <- agente <- agente.num <- Delay <- NULL
  data.aux <- dados
  Tmin <- min(dados$Time[idx])
  data.aux$Y[idx] <- x
  data.aggregated <- data.aux |>
    ## Selecionando apenas os dias faltantes a partir
    ## do domingo da respectiva ultima epiweek
    ## com dados faltantes
    dplyr::filter(Time >= Tmin  ) |>
    dplyr::group_by(Time, dt_event, agente, agente.num) |>
    dplyr::summarise(
      Y = sum(Y), .groups = "keep"
    )
  data.aggregated
}

## Step 4: Applying the age aggregation on each posterior
tibble.samples.0 <- lapply( X = vector.samples,
                            FUN = gg.agente,
                            dados = dataset,
                            idx = index.missing)

srag.pred.0 <- dplyr::bind_rows(tibble.samples.0, .id = "sample")

output$sample <- srag.pred.0

output$summy <- srag.pred.0  |>
  dplyr::group_by(Time, dt_event, agente, agente.num) |>
  dplyr::summarise(Median = stats::median(Y, na.rm = T),
                   LI = stats::quantile(Y, probs = 0.025, na.rm = T),
                   LS = stats::quantile(Y, probs = 0.975, na.rm = T),
                   LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                   LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                   .groups = "drop")


nowcast_gam<-output$summy

nowcast_gam$DS_UF_SIGLA<-i

trajetorias_gam<-output$sample
trajetorias_gam$DS_UF_SIGLA<-i


lista[[length(lista) + 1]] <- nowcast_gam

lista2[[length(lista2) + 1]] <- trajetorias_gam

}
  


big_data_gam<-bind_rows(lista)
big_data2_gam<-bind_rows(lista2)


###SALVANDO 
last_week<-epiweek(max(dados$dt_release))
last_year<-epiyear(max(dados$dt_release))

save(big_data_gam, file = paste("output/GAM_nowcast_srag_por_virus_uf_sem_filtro_febre_", last_year, "_", last_week, ".RData"))
save(big_data2_gam, file = paste("output/GAM_trajetorias_srag_por_virus_uf_sem_filtro_febre", last_year, "_", last_week, ".RData"))

#####Plot exploratory:
#dados_virus <- read.csv2(file = "C:/Users/tatty/Documents/00_FIOCRUZ/Boletim_InfoGripe/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")


#data_uf <- teste %>% 
#  mutate(Semana.epidemiológica=epiweek(dt_event)) %>%
 # # filter(DS_UF_SIGLA!="BR") %>%
#  mutate(Ano.epidemiológico=epiyear(dt_event),
 #        data=dt_event,
 #        virus=agente) %>%
 # as.data.frame()

#last_week<-epiweek(max(data_uf$dt_event))
#min_epi<-min(data_uf$dt_event)

#dados_1<- dados_virus %>%
#  filter(Ano.epidemiológico>=2023, fx_etaria=="Total", DS_UF_SIGLA=="SP") %>%
#  mutate(data = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
#  filter(data < min_epi)%>%
#  select(SG_UF_NOT, data, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, SARS2, FLU_A, VSR, RINO) %>%
#  pivot_longer(cols = c("SARS2", "FLU_A", "VSR", "RINO"), names_to = "virus", values_to = "Median") %>%
#  rename(CO_UF=SG_UF_NOT) 

#dados_uf_virus<-bind_rows(dados_1, data_uf)


#df<- dados_uf_virus  %>%arrange(virus, Ano.epidemiológico, Semana.epidemiológica) %>%
#  group_by(virus) %>%  # agrupa por ano também
#  mutate(media_m = round(zoo::rollmean(Median, k = 3, fill = NA, align = "center"))) 

#source("fct/Graficos por UF.R")

#png(paste0("Plots/fig_SP_virus_2023_2026_gam.png"),height = 8, width = 10, units = 'in', res = 300)
#plot(plot_virus_uf(df, uf="SP"))
#grid::grid.raster(info.logo, x = 0.98, y = 1.0, just = c('right', 'top'), width = unit(.8, 'inches'))
#dev.off()
  