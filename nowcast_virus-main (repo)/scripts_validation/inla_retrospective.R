

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
           SARS2, VSR, RINO, FLU_A, FLU_B, METAP)
}) %>%
  bind_rows(.id = "a") %>%
  mutate(dt_start = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  group_by(a) %>%
  mutate(dt_release = max(dt_start, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(dt_start>= min(dt_release)) %>%
  filter(!is.na(DS_UF_SIGLA)) %>%
  select(dt_start, dt_release,
         SARS2, VSR, RINO, FLU_A, FLU_B, METAP,
         SG_UF_NOT, DS_UF_SIGLA) %>%
  
  pivot_longer(
    cols = c(SARS2, VSR, RINO, FLU_A, FLU_B, METAP),
    names_to = "Virus",
    values_to = "cases"
  )


uf_sigla <- sort(unique(dados$DS_UF_SIGLA))

virus<- sort(unique(dados$Virus))

datas<-sort(unique(dados$dt_release))
datas<-datas[-(1:10)]

#virus<-"VSR"
#uf_sigla<- c("PI")

#uf_sigla<-uf_sigla[19:28]


lista<-list()
lista2<-list()

j<-"VSR"
i<-"SP"
l<-datas[10]

#rm(j,i)

for (l in datas) {
  
  dados_sub<- dados %>% filter(dt_release<=l)
  
  print(l)

for (j in virus) {
  
  dados2<- dados_sub %>% filter(Virus==j)
  
  print(j)
  
  for (i in uf_sigla) {
    
    sub_dado<- dados2 %>% filter (DS_UF_SIGLA==i) 
    
      
      now_diff<- nowcasting_diff_inla(dataset= sub_dado,
                                      date_start = dt_start,
                                      date_release = dt_release,
                                      Dmax = 10,
                                      wdw=30,
                                      cases = cases,
                                      K=2,
                                      silent=F,
                                      trajectories = TRUE)
      
      
    nowcast_total<-now_diff$total
    
    
    #nowcast_total$tendencia.6s<-slope.estimate.quant(trajectories = now_diff$trajectories, window = 6)
    
    
    nowcast_total$DS_UF_SIGLA<-i
    nowcast_total$virus<-j
    nowcast_total$data_base<-l
    
    trajetorias<-now_diff$trajectories
    trajetorias$DS_UF_SIGLA<-i
    trajetorias$virus<-j
    trajetorias$data_base<-l
    
    
    lista[[length(lista) + 1]] <- nowcast_total
    
    lista2[[length(lista2) + 1]] <- trajetorias
    
  }
  
}
  
}

big_data<-bind_rows(lista)
big_data2<-bind_rows(lista2)

save(big_data_gam, file = paste("validation/IMLA_nowcast_sari_simple_uf_no_fever_filter_2025-07-05_a_2026-06-20.RData"))
save(big_data2_gam, file = paste("validation/trajectories_INLA_nowcast_sari_simple_uf_no_fever_filter__2025-07-05_a_2026-06-20.RData"))