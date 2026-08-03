

###reding estimated data

conso <- read.csv2(file = "https://raw.githubusercontent.com/infogripe/Boletim_InfoGripe/main/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")

conso2<- conso %>%
      filter(epiyear>=2025, fx_etaria=="Total")%>%
      select(DS_UF_SIGLA, epiyear, epiweek, FLU_A, FLU_B, VSR, SARS2, RINO, METAP) %>%
      pivot_longer(FLU_A:METAP, values_to = "obs", names_to = "agente")


##reading estimative INLA

files <- list.files(
  path = "output/",
  pattern = "nowcast_srag_por_virus_uf_sem_filtro_febre",
  full.names = TRUE
)

cols <- c(
  "Time", "agente", "DS_UF_SIGLA", "dt_event",
  "Median", "LI", "LS", "LIb", "LSb", "vírus"
)

lista <- lapply(files, function(f) {
  
  env <- new.env()
  load(f, envir = env)
  
  df <- get(ls(env)[1], envir = env)
  
  # Padroniza o nome da coluna
  if ("virus" %in% names(df)) {
    names(df)[names(df) == "virus"] <- "agente"
  }
  
  if ("Agente" %in% names(df)) {
    names(df)[names(df) == "Agente"] <- "agente"
  }
  
  df <- df %>%
    select(any_of(cols))
  
  df$data_base <- max(df$dt_event, na.rm = TRUE)
  
  df
})

dados_inla <- bind_rows(lista)

dados_inla<- dados_inla %>% 
  group_by(DS_UF_SIGLA, agente, data_base) %>%
  slice_max(order_by = Time, n = 4, with_ties = FALSE) %>% ##Selecting the last four weeks
  ungroup() %>%
  filter(data_base<"2026-05-16") %>%
  mutate(metodo="INLA") %>%
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  select(DS_UF_SIGLA, agente, data_base, metodo, epiweek, epiyear, Median, LI, LS, LIb, LSb)


#####Estimates GAM

teste<-get(load("output/nowcast_srag_por_virus_uf_sem_filtro_febre_2025_ 38 .Rdata"))


###GAm multiplos vírus


gam_mult<-get(load("validation/GAM_nowcast_sari_viruses_uf_no_fever_filter_2025-07-05_a_2026-06-13.Rdata"))

gam_mult$data_base<-as.Date(gam_mult$data_base, origin = "1970-01-01")

gam_mult<- gam_mult %>% 
  group_by(data_base, DS_UF_SIGLA, agente, agente.num) %>%
  mutate(time=1:n())%>%
  ungroup() %>%
  filter(time %in% c(7:10)) %>% ##last four weeks
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  select(data_base, agente, DS_UF_SIGLA, epiyear, epiweek, Median, LI, LS, LIb, LSb)%>%
  mutate(metodo="GAM multiple virus") %>%
  bind_rows(dados_inla)%>%
  left_join(conso2, by=c("agente", "DS_UF_SIGLA", "epiyear", "epiweek")) %>%
  distinct(metodo,agente, DS_UF_SIGLA, data_base, epiyear, epiweek, .keep_all = TRUE)%>%
  filter(data_base<"2026-05-16")


#####GAM simple virus

gam_simple<-get(load("validation/GAM_nowcast_sari_simple_uf_no_fever_filter_2025-07-05_a_2026-06-27.Rdata"))


gam_simple<- gam_simple %>% 
  group_by(data_base, DS_UF_SIGLA, agente, agente.num) %>%
  mutate(time=1:n())%>%
  ungroup() %>%
  filter(time %in% c(7:10)) %>% ##last four weeks
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  select(data_base, agente, DS_UF_SIGLA, epiyear, epiweek, Median, LI, LS, LIb, LSb)%>%
  mutate(metodo="GAM one virus") %>%
  left_join(conso2, by=c("agente", "DS_UF_SIGLA", "epiyear", "epiweek")) %>%
  distinct(metodo,agente, DS_UF_SIGLA, data_base, epiyear, epiweek, .keep_all = TRUE)%>%
  filter(data_base<"2026-05-16")


df<- bind_rows(gam_mult, gam_simple)

write.csv(df, "validation/all_models_last_4weeks_nowcast.csv", row.names = FALSE)


