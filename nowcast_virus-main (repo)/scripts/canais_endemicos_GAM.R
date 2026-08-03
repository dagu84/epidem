##Plotando canais endemicos com nowcast
library(zoo)

source("scripts-relatorio-semanal/scripts/plot_canais_endemicos.R")
source("scripts-relatorio-semanal/scripts/theme.publication.R")

#save(big_data, file = paste("C:/Users/tatty/Documents/00_FIOCRUZ/scripts-relatorio-semanal/dados/nowcast_srag_por_virus_uf_sem_filtro_febre.RData"))


####Nowcast por UF
load("nowcast_virus/output/GAM_nowcast_srag_por_virus_uf_sem_filtro_febre_ 2026 _ 21 .RData")
load("nowcast_virus/output/GAM_trajetorias_srag_por_virus_uf_sem_filtro_febre 2026 _ 21 .RData")

total.summy <- big_data2_gam |>
  mutate(virus=agente)|>
  dplyr::group_by(Time, dt_event, DS_UF_SIGLA, virus) |>
  dplyr::summarise(Median = stats::median(Y, na.rm = T),
                   LI = stats::quantile(Y, probs = 0.10, na.rm = T),
                   LS = stats::quantile(Y, probs = 0.90, na.rm = T),
                   LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                   LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                   .groups = "drop") %>%
  select(DS_UF_SIGLA, Time, dt_event, virus,  LI, LS)

big_data<- big_data_gam %>%
  select(-LI, -LS)%>%
  left_join(total.summy, by=c("DS_UF_SIGLA", "Time", "dt_event", "virus"))

last_week<-epiweek(max(big_data_gam$dt_event))

##Ddos virus
dados_virus <- read.csv2(file = "https://raw.githubusercontent.com/infogripe/Boletim_InfoGripe/main/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")

###Dados da pop já são baixados no script de tratamento de dados
pop <- import("infogripe_code/MEM_SRAG/populacao/projecoes_2024_tab1_idade_simples.xlsx", skip = 5)


uf<-dados_virus %>%
  filter(DS_UF_SIGLA!="BR") %>%
  select(DS_UF_SIGLA, SG_UF_NOT) %>%
  unique() 
  

load("scripts-relatorio-semanal/dados/bands_vsr_regiao.RData")
load("scripts-relatorio-semanal/dados/bands_flu_regiao.RData")

pop_regi<- pop %>% filter (LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul")) %>%
  filter(SEXO=="Ambos") %>%
  select(LOCAL, IDADE, '2023', '2024','2025','2026') %>%
  group_by(LOCAL)  %>%
  summarise(across('2023':'2026',\(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer('2023':'2026',names_to = "epiyear", values_to = "pop") %>%
  mutate(epiyear=as.double(epiyear)) %>%
  rename(regioes=LOCAL,
         Ano.epidemiológico=epiyear) 



big_data_regi <- big_data2_gam %>% 
  filter(DS_UF_SIGLA!="BR") %>%
  full_join(uf, by="DS_UF_SIGLA") %>%
  mutate(regioes=floor(as.integer(SG_UF_NOT)/10)) %>%
  mutate(Semana.epidemiológica=epiweek(dt_event),
         Ano.epidemiológico=epiyear(dt_event)) %>%
  filter(virus %in% c("VSR", "FLU_A")) %>%
  group_by(regioes, sample, Ano.epidemiológico, Semana.epidemiológica, virus) %>%
  summarise(Y=sum(Y, na.rm=T)) %>%
  group_by(virus, regioes,Ano.epidemiológico,  Semana.epidemiológica) %>%
  dplyr::summarise(Median = stats::median(Y, na.rm = T),
                   LI = stats::quantile(Y, probs = 0.10, na.rm = T),
                   LS = stats::quantile(Y, probs = 0.90, na.rm = T),
                   .groups = "drop") %>%
  mutate(casos=Median)


canal_regi<- dados_virus %>%
  filter(Ano.epidemiológico>=2023, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(data = MMWRweek::MMWRweek2Date(Ano.epidemiológico, Semana.epidemiológica) + 6) %>%
  filter(data < min_epi)%>%
  mutate(regioes=floor(as.integer(SG_UF_NOT)/10)) %>%
  select(regioes, Ano.epidemiológico, Semana.epidemiológica, VSR, FLU_A)%>%
  pivot_longer(cols = c("VSR", "FLU_A"), names_to = "virus", values_to = "casos")%>%
  group_by(virus, regioes, Ano.epidemiológico, Semana.epidemiológica) %>%
  summarise(casos=sum(casos, na.rm = TRUE)) %>%
  bind_rows(big_data_regi) %>%
  mutate(regioes=factor(regioes,
                        levels=c(1,2,5,3,4),
                        labels=c('Norte', 'Nordeste',
                                 'Centro-Oeste', 'Sudeste',
                                 'Sul'))) %>%
  left_join(pop_regi, by=c("regioes", "Ano.epidemiológico")) %>%
  mutate(inci=casos*100000/pop,
         LI=LI*100000/pop,
         LS=LS*100000/pop) %>%
  arrange(Ano.epidemiológico, Semana.epidemiológica)%>%
  group_by(virus, regioes)%>%
  mutate(media.movel = rollmean(inci, k = 3, fill = NA, align = "center")) %>%
  ungroup()

#####Arrumando os dados do nowcast

bands.flu.regiao$virus<-"FLU_A"
bands.flu.regiao$regioes <- str_replace(bands.flu.regiao$regioes, "Centro-oeste", "Centro-Oeste")
bands.vsr.regiao$regioes <- str_replace(bands.vsr.regiao$regioes, "Centro-oeste", "Centro-Oeste")

out<- bands.vsr.regiao %>% 
         mutate(virus="VSR") %>%
  bind_rows(bands.flu.regiao)%>%
  mutate(Ano.epidemiológico=2025)%>%
  left_join(pop_regi, by=c("regioes", "Ano.epidemiológico")) %>%
  mutate(`0.25quant`=`0.25quant`*100000/pop,
         `0.75quant`=`0.75quant`*100000/pop,
         `0.9quant`=`0.9quant`*100000/pop,
         `0.5quant`=`0.5quant`*100000/pop) %>%
  select(virus, Semana.epidemiológica, regioes, `0.25quant`, `0.75quant`,`0.9quant` , `0.5quant`)

canal_banda_regi<- canal_regi %>%
  full_join(out, by=c("Semana.epidemiológica", "regioes", "virus"))

data<-canal_banda_regi %>%
  filter(virus=="FLU_A")

###Por regiao

canal_banda_regi$regioes <- factor(canal_banda_regi$regioes, 
                       levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"))

cbr_flu_gam<-canal_banda_regi %>%
  filter(virus=="FLU_A") %>%
  # mutate(LS = ifelse(regioes %in% c( "Nordeste"), NA, LS),
  #        LI = ifelse(regioes %in% c("Nordeste"), NA, LI)) %>%
  plot.canais.endemicos(virus="Influenza A", UF=FALSE,last_week =last_week)

png(paste0('scripts-relatorio-semanal/plots/UF/GAM_Regiao_fluA.png'),height = 12, width = 14, units = 'in', res = 300)

plot(cbr_flu_gam)

grid::grid.raster(info.logo, x = 0.98, y = 1, just = c('right', 'top'), width = unit(1, 'inches'))
dev.off()

cbr_vsr_gam<-canal_banda_regi %>%
  filter(virus=="VSR") %>%
   mutate(LS = ifelse(regioes %in% c("Nordeste", "Norte"), NA, LS),
          LI = ifelse(regioes %in% c("Nordeste", "Norte"), NA, LI))%>%
  #mutate(LS = ifelse(DS_UF_SIGLA %in% c("PI"), NA, LS),
  #       LI = ifelse(DS_UF_SIGLA %in% c("PI"), NA, LI)) %>%
  plot.canais.endemicos(virus="VSR", UF=FALSE,last_week =last_week)

png(paste0('scripts-relatorio-semanal/plots/UF/GAM_Regiao_VSR.png'),height = 12, width = 14, units = 'in', res = 300)

plot(cbr_vsr_gam)

grid::grid.raster(info.logo, x = 0.98, y = 1, just = c('right', 'top'), width = unit(1, 'inches'))
dev.off()

