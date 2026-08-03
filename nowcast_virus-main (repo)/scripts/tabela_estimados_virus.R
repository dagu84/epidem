####Nowcast por UF
load("output/nowcast_srag_por_virus_uf_sem_filtro_febre.RData")
load("output/trajetorias_srag_por_virus_uf_sem_filtro_febre.RData")

total.summy <- big_data2 |>
  dplyr::group_by(Time, dt_event, sample, DS_UF_SIGLA, virus) |>
  dplyr::summarise(Y = sum(Y, na.rm = T)) |>
  dplyr::group_by(Time, dt_event, DS_UF_SIGLA, virus) |>
  dplyr::summarise(Median = stats::median(Y, na.rm = T),
                   LI = stats::quantile(Y, probs = 0.10, na.rm = T),
                   LS = stats::quantile(Y, probs = 0.90, na.rm = T),
                   LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                   LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                   .groups = "drop") %>%
  select(DS_UF_SIGLA, Time, dt_event, virus,  LI, LS)

big_data<- big_data %>%
  select(-LI, -LS)%>%
  left_join(total.summy, by=c("DS_UF_SIGLA", "Time", "dt_event", "virus"))

last_week<-epiweek(max(big_data$dt_event))

##Ddos virus
dados_virus <- read.csv2(file = "https://raw.githubusercontent.com/infogripe/Boletim_InfoGripe/main/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")


uf<-dados_virus %>%
  select(DS_UF_SIGLA, SG_UF_NOT) %>%
  unique() 

big_data_uf <- big_data %>% 
  mutate(Semana.epidemiológica=epiweek(dt_event),
         Ano.epidemiológico=epiyear(dt_event)) %>%
  mutate(casos.estimados=Median) %>%
  full_join(uf, by="DS_UF_SIGLA") %>%
  filter(!is.na(dt_event))

min_epi<-min(big_data_uf$dt_event)

canal_uf<- dados_virus %>%
  filter(Ano.epidemiológico>=2022, fx_etaria=="Total") %>%
  mutate(data = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(data < min_epi)%>%
  pivot_longer(cols = c("SARS2", "VSR", "FLU_A", "FLU_B", "RINO", "METAP"), names_to = "virus", values_to = "casos.estimados")%>%
  mutate(casos.registrados=casos.estimados)%>%
  select(DS_UF_SIGLA, SG_UF_NOT, casos.estimados,casos.registrados, Ano.epidemiológico, Semana.epidemiológica, virus) %>%
  bind_rows(big_data_uf) %>%
  arrange(SG_UF_NOT, Ano.epidemiológico, Semana.epidemiológica)%>%
  group_by(virus, DS_UF_SIGLA)%>%
  mutate(media.movel = round((rollmean(casos.estimados, k = 3, fill = NA, align = "center"))))%>%
  ungroup() %>%
  select(SG_UF_NOT, DS_UF_SIGLA, Ano.epidemiológico, Semana.epidemiológica, virus, Median, LI, LS, casos.registrados, casos.estimados, media.movel)
  
write.csv2(canal_uf, "C:/Users/tatty/Documents/00_FIOCRUZ/Boletim_InfoGripe/Dados/InfoGripe/estados_e_pais_serie_estimativas_virus_sem_filtro_febre.csv", row.names = FALSE)

