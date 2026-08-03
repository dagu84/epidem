

load("validation/GAM_nowcast_sari_viruses_uf_no_fever_filter_2025-07-05_a_2026-06-13.RData")
load("validation/trajectories_GAM_nowcast_sari_viruses_uf_no_fever_filter__2025-07-05_a_2026-06-13.RData")

unique(big_data_gam$data_base)

total.summy <- big_data2_gam |>
 # filter(data_base=="20617") |>
  mutate(virus=agente)|>
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

big_data_gam<- big_data_gam %>%
 # filter(data_base=="20617") |>
  mutate(virus=agente)%>%
         select(-LI, -LS)%>%
         left_join(total.summy, by=c("DS_UF_SIGLA", "Time", "dt_event", "virus"))  %>%
  filter(!is.na(dt_event))


last_week<- 24

covi <- big_data_gam%>% 
  mutate(Semana.epidemiológica=epiweek(dt_event),
         Ano.epidemiológico=epiyear(dt_event)) %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == 2025, Semana.epidemiológica, Semana.epidemiológica+53)) %>% ##mudar aqui depois
  filter(DS_UF_SIGLA!="BR") %>%
  filter(virus=="SARS2")


dados_virus <- read.csv2(file = "C:/Users/tatty/Documents/00_FIOCRUZ/Boletim_InfoGripe/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")

str(dados_virus)

dados_1<- dados_virus %>%
  filter(Ano.epidemiológico>=2025, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == max(Ano.epidemiológico)-1, Semana.epidemiológica, Semana.epidemiológica+53)) %>%
  mutate(Casos.semanais.reportados.até.a.última.atualização=SARS2) %>%
  full_join(covi, by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica")) %>%
  select(Casos.semanais.reportados.até.a.última.atualização, Ano.epidemiológico, Semana.epidemiológica, DS_UF_SIGLA, sequencia, Median, LI, LS)%>%
  filter(Ano.epidemiológico==2026)
library(zoo)

med<- dados_1 %>%
  filter(sequencia<min(covi$sequencia))%>%
  mutate(Median=Casos.semanais.reportados.até.a.última.atualização)%>%
  select(Median, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia)%>%
  bind_rows(covi) %>%
  arrange(sequencia)%>%
  group_by(DS_UF_SIGLA)%>%
  mutate(media.movel = round(rollmean(Median, k = 3, fill = NA, align = "center"))) %>%
  ungroup() %>%
  select(Ano.epidemiológico, Semana.epidemiológica, sequencia, DS_UF_SIGLA, media.movel) %>%
  filter(Ano.epidemiológico==2026)

dados_2<- dados_1 %>%
  left_join(med,  by=c("DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica", "sequencia")) %>%
  filter(Ano.epidemiológico==2026)#%>%
 # mutate(sequencia=Semana.epidemiológica)

#big_data<- big_data %>% filter (DS_UF_SIGLA!="BR")

##dados_1<- dados_1 %>%
 # filter(Ano.epidemiológico==2026)%>%
#  mutate(sequencia=Semana.epidemiológica)
# filter(DS_UF_SIGLA %in% c("CE", "PB", "RN", "SP", "RJ", "ES", "PR"))

#last_week<-20

#dados_2<- dados_2 %>%
#mutate(LS = ifelse(DS_UF_SIGLA %in% c( "RR", "PI"), NA, LS),
#      LI = ifelse(DS_UF_SIGLA %in% c("RR", "PI"), NA, LI))

pcovi<-ggplot(data = dados_2) +
  geom_col(
    aes(x = Semana.epidemiológica, y = Casos.semanais.reportados.até.a.última.atualização, fill = "Casos notificados"), 
    color = 'lightblue',
    alpha = 0.75) +
  geom_line(
    aes(x = Semana.epidemiológica, y = media.movel, color = "Média móvel"), 
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c("Média móvel" = 'black')
  ) +
  geom_ribbon(
    data = dados_2, 
    aes(x = Semana.epidemiológica, ymin = LI, ymax = LS, fill = "Casos estimados"), 
    alpha = 0.2) +
  scale_fill_manual(
    values = c("Casos estimados"='#184E77',
               "Casos notificados" = 'lightblue')
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  )+
 # scale_x_continuous(
  #  breaks = seq(1, max(dados_1$sequencia), by = 12),
  #  labels = function(x) {
      # Converter sequencia para semana epidemiológica
      # Se sequencia <= 52: mantém como está (ano 2024)
      # Se sequencia > 52: subtrai 52 (ano 2025)
  #    semana_epi = ifelse(x <= 52, x, x - 52)
  #    return(as.character(semana_epi))
  #  }
 # ) +
  labs(
    x = "Semana epidemiológica",
    y = 'Casos de SRAG por Covid-19 ', 
    color = '', 
    fill = ''
  ) +
  ggtitle(
    paste0("Casos estimados de SRAG por Covid-19 até a Semana ", last_week ," 2026")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    axis.title = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(), 
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size= unit(0.4, "cm"),
    legend.spacing = unit(0, "cm"),
    legend.title = element_text(face = "italic", size = rel(1)),
    legend.text = element_text(size = rel(.8)),
    plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
    strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    strip.text = element_text(face = "bold")
  ) +
  # facet_wrap(~ DS_UF_SIGLA, scale="free")+
  facet_geo(
    ~ DS_UF_SIGLA, 
    grid = 'br_states_grid1', 
    scale = 'free_y'
  ) +
  coord_cartesian(ylim = c(0, NA))+
  guides(fill = guide_legend(nrow = 1, title = NULL),
         color = guide_legend(nrow = 1, , title = NULL)) +
  theme(plot.caption = element_markdown())

png(paste0('Plots/GAM_fig_UFs_covid.png'),height = 10, width = 8, units = 'in', res = 300)
plot(pcovi)
#grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
dev.off()

#########################################                               
#########RINOVÍRUS##################
#########################################

big_data_RINO <- big_data_gam %>% 
  mutate(virus=agente)%>%
  mutate(Semana.epidemiológica=epiweek(dt_event),
         Ano.epidemiológico=epiyear(dt_event)) %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == 2025, Semana.epidemiológica, Semana.epidemiológica+53)) %>% ##mudar aqui depois
  filter(DS_UF_SIGLA!="BR") %>%
  filter(virus=="RINO")

dados_1<- dados_virus %>%
  filter(Ano.epidemiológico>=2025, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == max(Ano.epidemiológico)-1, Semana.epidemiológica, Semana.epidemiológica+53)) %>%
  group_by(SG_UF_NOT, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia) %>%
  summarise(Casos.semanais.reportados.até.a.última.atualização=sum(RINO, na.rm = TRUE)) %>%
  rename(CO_UF=SG_UF_NOT) %>%
  select(CO_UF, Casos.semanais.reportados.até.a.última.atualização, Ano.epidemiológico, Semana.epidemiológica, DS_UF_SIGLA, sequencia) %>%
  full_join(big_data_RINO, by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica")) %>%
  filter(Ano.epidemiológico==2026)

med<- dados_1 %>%
  filter(sequencia<min(covi$sequencia))%>%
  mutate(Median=Casos.semanais.reportados.até.a.última.atualização)%>%
  select(Median, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia)%>%
  bind_rows(big_data_RINO) %>%
  arrange(sequencia)%>%
  group_by(DS_UF_SIGLA)%>%
  mutate(media.movel = round(rollmean(Median, k = 3, fill = NA, align = "center"))) %>%
  ungroup() %>%
  select(Ano.epidemiológico, Semana.epidemiológica, sequencia, DS_UF_SIGLA, media.movel)

dados_1<- dados_1 %>%
  full_join(med,  by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica"))


#dados_1<- dados_1 %>%
 # mutate(LS = ifelse(DS_UF_SIGLA %in% c("PI"), NA, LS),
 #       LI = ifelse(DS_UF_SIGLA %in% c("PI"), NA, LI)) %>%
  #filter(Ano.epidemiológico==2026)#%>%
#  filter(Ano.epidemiológico==2026)%>%
#  mutate(sequencia=Semana.epidemiológica)

rino<-ggplot(data = dados_1) +
  geom_col(
    aes(x = Semana.epidemiológica, y = Casos.semanais.reportados.até.a.última.atualização, fill = "Casos notificados"), 
    color = 'lightblue',
    alpha = 0.75) +
  geom_line(
    aes(x = Semana.epidemiológica, y = media.movel, color = "Média móvel"), 
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c("Média móvel" = 'black')
  ) +
  geom_ribbon(
    data = dados_1, 
    aes(x = Semana.epidemiológica, ymin = LI, ymax = LS, fill = "Casos estimados"), 
    alpha = 0.2) +
  scale_fill_manual(
    values = c("Casos estimados"='#184E77',
               "Casos notificados" = 'lightblue')
  ) +
  scale_x_continuous(breaks = seq(1, 53, by=4))+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  )+
 # scale_x_continuous(
 #  breaks = seq(1, max(dados_1$sequencia), by = 12),
 #  labels = function(x) {
      # Converter sequencia para semana epidemiológica
      # Se sequencia <= 52: mantém como está (ano 2024)
      # Se sequencia > 52: subtrai 52 (ano 2025)
 #     semana_epi = ifelse(x <= 52, x, x - 52)
 #    return(as.character(semana_epi))
 #   }
 # ) +
  labs(
    x = "Semana epidemiológica",
    y = 'Casos de SRAG por Rinovírus ', 
    color = '', 
    fill = ''
  ) +
  ggtitle(
    paste0("Casos estimados de SRAG por Rinovírus até a Semana ", last_week ," 2026")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    axis.title = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(), 
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size= unit(0.4, "cm"),
    legend.spacing = unit(0, "cm"),
    legend.title = element_text(face = "italic", size = rel(1)),
    legend.text = element_text(size = rel(.8)),
    plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
    strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    strip.text = element_text(face = "bold")
  ) +
  # facet_wrap(~ DS_UF_SIGLA, scale="free")+
  facet_geo(
    ~ DS_UF_SIGLA, 
    grid = 'br_states_grid1', 
    scale = 'free_y'
  ) +
  guides(fill = guide_legend(nrow = 1, title = NULL),
         color = guide_legend(nrow = 1, , title = NULL)) +
  theme(plot.caption = element_markdown())

png(paste0('Plots/GAM_fig_UFs_rino.png'),height = 10, width = 8, units = 'in', res = 300)
plot(rino)
#grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
dev.off()


#########################################                               
#########VSR##################
#########################################

big_data_VSR<- big_data_gam %>% 
  mutate(virus=agente)%>%
  mutate(Semana.epidemiológica=epiweek(dt_event),
         Ano.epidemiológico=epiyear(dt_event)) %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == 2025, Semana.epidemiológica, Semana.epidemiológica+53)) %>% ##mudar aqui depois
  filter(DS_UF_SIGLA!="BR") %>%
  filter(virus=="VSR")

dados_1<- dados_virus %>%
  filter(Ano.epidemiológico>=2025, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == max(Ano.epidemiológico)-1, Semana.epidemiológica, Semana.epidemiológica+53)) %>%
  group_by(SG_UF_NOT, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia) %>%
  summarise(Casos.semanais.reportados.até.a.última.atualização=sum(VSR, na.rm = TRUE)) %>%
  rename(CO_UF=SG_UF_NOT) %>%
  select(CO_UF, Casos.semanais.reportados.até.a.última.atualização, Ano.epidemiológico, Semana.epidemiológica, DS_UF_SIGLA, sequencia) %>%
  full_join(big_data_VSR, by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica")) %>%
  filter(Ano.epidemiológico==2026)

med<- dados_1 %>%
  filter(sequencia<min(covi$sequencia))%>%
  mutate(Median=Casos.semanais.reportados.até.a.última.atualização)%>%
  select(Median, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia)%>%
  bind_rows(big_data_VSR) %>%
  arrange(sequencia)%>%
  group_by(DS_UF_SIGLA)%>%
  mutate(media.movel = round(rollmean(Median, k = 3, fill = NA, align = "center"))) %>%
  ungroup() %>%
  select(Ano.epidemiológico, Semana.epidemiológica, sequencia, DS_UF_SIGLA, media.movel)

dados_1<- dados_1 %>%
  full_join(med,  by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica"))


#dados_1<- dados_1 %>%
#mutate(LS = ifelse(DS_UF_SIGLA %in% c("PI", "AP"), NA, LS),
 #      LI = ifelse(DS_UF_SIGLA %in% c("PI", "AP"), NA, LI))%>%
#  filter(Ano.epidemiológico==2026)# %>%
  #filter(Ano.epidemiológico==2026)%>%
  #mutate(sequencia=Semana.epidemiológica)

vsr<-ggplot(data = dados_1) +
  geom_col(
    aes(x = Semana.epidemiológica, y = Casos.semanais.reportados.até.a.última.atualização, fill = "Casos notificados"), 
    color = 'lightblue',
    alpha = 0.75) +
  geom_line(
    aes(x = Semana.epidemiológica, y = media.movel, color = "Média móvel"), 
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c("Média móvel" = 'black')
  ) +
  geom_ribbon(
    data = dados_1, 
    aes(x = Semana.epidemiológica, ymin = LI, ymax = LS, fill = "Casos estimados"), 
    alpha = 0.2) +
  scale_fill_manual(
    values = c("Casos estimados"='#184E77',
               "Casos notificados" = 'lightblue')
  ) +
  scale_x_continuous(breaks = seq(1, 53, by=4))+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  )+
 #scale_x_continuous(
 #   breaks = seq(1, max(dados_1$sequencia), by = 12),
 #  labels = function(x) {
      # Converter sequencia para semana epidemiológica
      # Se sequencia <= 52: mantém como está (ano 2024)
      # Se sequencia > 52: subtrai 52 (ano 2025)
 #     semana_epi = ifelse(x <= 52, x, x - 52)
 #     return(as.character(semana_epi))
 #   }
 # ) +
  labs(
    x = "Semana epidemiológica",
    y = 'Casos de SRAG por VSR', 
    color = '', 
    fill = ''
  ) +
  ggtitle(
    paste0("Casos estimados de SRAG por VSR até a Semana ", last_week ," 2026")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    axis.title = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(), 
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size= unit(0.4, "cm"),
    legend.spacing = unit(0, "cm"),
    legend.title = element_text(face = "italic", size = rel(1)),
    legend.text = element_text(size = rel(.8)),
    plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
    strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    strip.text = element_text(face = "bold")
  ) +
  # facet_wrap(~ DS_UF_SIGLA, scale="free")+
  facet_geo(
    ~ DS_UF_SIGLA, 
    grid = 'br_states_grid1', 
    scale = 'free_y'
  ) +
  guides(fill = guide_legend(nrow = 1, title = NULL),
         color = guide_legend(nrow = 1, , title = NULL)) +
  theme(plot.caption = element_markdown())

png(paste0('Plots/GAM_fig_UFs_VSR.png'),height = 10, width = 8, units = 'in', res = 300)
plot(vsr)
#grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
dev.off()

#########################################
###############Influenza#################
##########################################

big_data_flu <- big_data_gam %>% 
  mutate(virus=agente)%>%
  mutate(Semana.epidemiológica=epiweek(dt_event),
         Ano.epidemiológico=epiyear(dt_event)) %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == 2025, Semana.epidemiológica, Semana.epidemiológica+53)) %>% ##mudar aqui depois
  filter(DS_UF_SIGLA!="BR") %>%
  filter(virus=="FLU_A")

dados_1<- dados_virus %>%
  filter(Ano.epidemiológico>=2025, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == max(Ano.epidemiológico)-1, Semana.epidemiológica, Semana.epidemiológica+53)) %>%
  group_by(SG_UF_NOT, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia) %>%
  summarise(Casos.semanais.reportados.até.a.última.atualização=sum(FLU_A, na.rm = TRUE)) %>%
  rename(CO_UF=SG_UF_NOT) %>%
  select(CO_UF, Casos.semanais.reportados.até.a.última.atualização, Ano.epidemiológico, Semana.epidemiológica, DS_UF_SIGLA, sequencia) %>%
  full_join(big_data_flu, by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica")) %>%
  filter(Ano.epidemiológico==2026)

med<- dados_1 %>%
  filter(sequencia<min(big_data_flu$sequencia))%>%
  mutate(Median=Casos.semanais.reportados.até.a.última.atualização)%>%
  select(Median, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia)%>%
  bind_rows(big_data_flu) %>%
  arrange(sequencia)%>%
  group_by(DS_UF_SIGLA)%>%
  mutate(media.movel = round(rollmean(Median, k = 3, fill = NA, align = "center"))) %>%
  ungroup() %>%
  select(Ano.epidemiológico, Semana.epidemiológica, sequencia, DS_UF_SIGLA, media.movel)

dados_1<- dados_1 %>%
  full_join(med,  by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica"))%>%
  filter(Ano.epidemiológico==2026)


#dados_1<- dados_1 %>%
 # mutate(LS = ifelse(DS_UF_SIGLA %in% c( "PE"), NA, LS),
 #       LI = ifelse(DS_UF_SIGLA %in% c("PE"), NA, LI)) #%>%
 # filter(Ano.epidemiológico==2026)%>%
 # mutate(sequencia=Semana.epidemiológica)


flua<-ggplot(data = dados_1) +
  geom_col(
    aes(x = Semana.epidemiológica, y = Casos.semanais.reportados.até.a.última.atualização, fill = "Casos notificados"), 
    color = 'lightblue',
    alpha = 0.75) +
  geom_line(
    aes(x = Semana.epidemiológica, y = media.movel, color = "Média móvel"), 
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c("Média móvel" = 'black')
  ) +
  geom_ribbon(
    data = dados_1, 
    aes(x = Semana.epidemiológica, ymin = LI, ymax = LS, fill = "Casos estimados"), 
    alpha = 0.2) +
  scale_fill_manual(
    values = c("Casos estimados"='#184E77',
               "Casos notificados" = 'lightblue')
  ) +
   scale_x_continuous(breaks = seq(1, 53, by=4))+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  )+
 # scale_x_continuous(
 #   breaks = seq(1, max(dados_1$sequencia), by = 12),
#    labels = function(x) {
      # Converter sequencia para semana epidemiológica
      # Se sequencia <= 52: mantém como está (ano 2024)
      # Se sequencia > 52: subtrai 52 (ano 2025)
  #    semana_epi = ifelse(x <= 52, x, x - 52)
  #    return(as.character(semana_epi))
 #   }
 # ) +
  labs(
    x = "Semana epidemiológica",
    y = 'Casos de SRAG por Influenza A ', 
    color = '', 
    fill = ''
  ) +
  ggtitle(
    paste0("Casos estimados de SRAG por FLU A até a Semana ", last_week ," 2026")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    axis.title = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(), 
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size= unit(0.4, "cm"),
    legend.spacing = unit(0, "cm"),
    legend.title = element_text(face = "italic", size = rel(1)),
    legend.text = element_text(size = rel(.8)),
    plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
    strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    strip.text = element_text(face = "bold")
  ) +
  # facet_wrap(~ DS_UF_SIGLA, scale="free")+
  facet_geo(
    ~ DS_UF_SIGLA, 
    grid = 'br_states_grid1', 
    scale = 'free_y'
  ) +
  guides(fill = guide_legend(nrow = 1, title = NULL),
         color = guide_legend(nrow = 1, , title = NULL)) +
  theme(plot.caption = element_markdown())

png(paste0('Plots/GAM_fig_UFs_FLU_A.png'),height = 10, width = 8, units = 'in', res = 300)
plot(flua)
#grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
dev.off()

##########################################################################################################
#################################FLU B ##################################################################
########################################################################################################


big_data_flub <- big_data_gam %>% 
  mutate(virus=agente)%>%
  mutate(Semana.epidemiológica=epiweek(dt_event),
         Ano.epidemiológico=epiyear(dt_event)) %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == 2025, Semana.epidemiológica, Semana.epidemiológica+53)) %>% ##mudar aqui depois
  filter(DS_UF_SIGLA!="BR") %>%
  filter(virus=="FLU_B")

dados_1<- dados_virus %>%
  filter(Ano.epidemiológico>=2025, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == max(Ano.epidemiológico)-1, Semana.epidemiológica, Semana.epidemiológica+53)) %>%
  group_by(SG_UF_NOT, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia) %>%
  summarise(Casos.semanais.reportados.até.a.última.atualização=sum(FLU_B, na.rm = TRUE)) %>%
  rename(CO_UF=SG_UF_NOT) %>%
  select(CO_UF, Casos.semanais.reportados.até.a.última.atualização, Ano.epidemiológico, Semana.epidemiológica, DS_UF_SIGLA, sequencia) %>%
  full_join(big_data_flub, by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica")) %>%
  filter(Ano.epidemiológico==2026)

med<- dados_1 %>%
  filter(sequencia<min(big_data_flub$sequencia))%>%
  mutate(Median=Casos.semanais.reportados.até.a.última.atualização)%>%
  select(Median, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia)%>%
  bind_rows(big_data_flub) %>%
  arrange(sequencia)%>%
  group_by(DS_UF_SIGLA)%>%
  mutate(media.movel = round(rollmean(Median, k = 3, fill = NA, align = "center"))) %>%
  ungroup() %>%
  select(Ano.epidemiológico, Semana.epidemiológica, sequencia, DS_UF_SIGLA, media.movel)
library(zoo)

dados_1<- dados_1 %>%
  full_join(med,  by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica"))%>%
  filter(Ano.epidemiológico==2026)


#dados_1<- dados_1 %>%
#mutate(LS = ifelse(DS_UF_SIGLA %in% c("RR", "AP", "PE", "AM", "MA", "PB", "RO", "TO"), NA, LS),
#       LI = ifelse(DS_UF_SIGLA %in% c("RR", "AP", "PE", "AM", "MA", "PB", "RO", "TO"), NA, LI)) #%>%
 #filter(Ano.epidemiológico==2026)%>%
 #mutate(sequencia=Semana.epidemiológica)



flub<-ggplot(data = dados_1) +
  geom_col(
    aes(x = Semana.epidemiológica, y = Casos.semanais.reportados.até.a.última.atualização, fill = "Casos notificados"), 
    color = 'lightblue',
    alpha = 0.75) +
  geom_line(
    aes(x = Semana.epidemiológica, y = media.movel, color = "Média móvel"), 
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c("Média móvel" = 'black')
  ) +
  geom_ribbon(
    data = dados_1, 
    aes(x = Semana.epidemiológica, ymin = LI, ymax = LS, fill = "Casos estimados"), 
    alpha = 0.2) +
  scale_fill_manual(
    values = c("Casos estimados"='#184E77',
               "Casos notificados" = 'lightblue')
  ) +
  scale_x_continuous(breaks = seq(1, 53, by=4))+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  )+
  # scale_x_continuous(
  #   breaks = seq(1, max(dados_1$sequencia), by = 12),
  #    labels = function(x) {
  # Converter sequencia para semana epidemiológica
  # Se sequencia <= 52: mantém como está (ano 2024)
  # Se sequencia > 52: subtrai 52 (ano 2025)
  #    semana_epi = ifelse(x <= 52, x, x - 52)
  #    return(as.character(semana_epi))
  #   }
  # ) +
  labs(
    x = "Semana epidemiológica",
    y = 'Casos de SRAG por Influenza B ', 
    color = '', 
    fill = ''
  ) +
  ggtitle(
    paste0("Casos estimados de SRAG por FLU B até a Semana ", last_week ," 2026")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    axis.title = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(), 
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size= unit(0.4, "cm"),
    legend.spacing = unit(0, "cm"),
    legend.title = element_text(face = "italic", size = rel(1)),
    legend.text = element_text(size = rel(.8)),
    plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
    strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    strip.text = element_text(face = "bold")
  ) +
  # facet_wrap(~ DS_UF_SIGLA, scale="free")+
  facet_geo(
    ~ DS_UF_SIGLA, 
    grid = 'br_states_grid1', 
    scale = 'free_y'
  ) +
  guides(fill = guide_legend(nrow = 1, title = NULL),
         color = guide_legend(nrow = 1, , title = NULL)) +
  theme(plot.caption = element_markdown())

png(paste0('Plots/GAM_fig_UFs_FLU_B.png'),height = 10, width = 8, units = 'in', res = 300)
plot(flub)
#grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
dev.off()


##########################################################################################################
#################################METAP ##################################################################
########################################################################################################


big_data_mt <- big_data_gam %>% 
  mutate(virus=agente)%>%
  mutate(Semana.epidemiológica=epiweek(dt_event),
         Ano.epidemiológico=epiyear(dt_event)) %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == 2025, Semana.epidemiológica, Semana.epidemiológica+53)) %>% ##mudar aqui depois
  filter(DS_UF_SIGLA!="BR") %>%
  filter(virus=="METAP")

dados_1<- dados_virus %>%
  filter(Ano.epidemiológico>=2025, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(sequencia = ifelse(Ano.epidemiológico == max(Ano.epidemiológico)-1, Semana.epidemiológica, Semana.epidemiológica+53)) %>%
  group_by(SG_UF_NOT, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia) %>%
  summarise(Casos.semanais.reportados.até.a.última.atualização=sum(METAP, na.rm = TRUE)) %>%
  rename(CO_UF=SG_UF_NOT) %>%
  select(CO_UF, Casos.semanais.reportados.até.a.última.atualização, Ano.epidemiológico, Semana.epidemiológica, DS_UF_SIGLA, sequencia) %>%
  full_join(big_data_mt, by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica")) %>%
  filter(Ano.epidemiológico==2026)

med<- dados_1 %>%
  filter(sequencia<min(big_data_mt$sequencia))%>%
  mutate(Median=Casos.semanais.reportados.até.a.última.atualização)%>%
  select(Median, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, sequencia)%>%
  bind_rows(big_data_mt) %>%
  arrange(sequencia)%>%
  group_by(DS_UF_SIGLA)%>%
  mutate(media.movel = round(rollmean(Median, k = 3, fill = NA, align = "center"))) %>%
  ungroup() %>%
  select(Ano.epidemiológico, Semana.epidemiológica, sequencia, DS_UF_SIGLA, media.movel)

dados_1<- dados_1 %>%
  full_join(med,  by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica"))%>%
  filter(Ano.epidemiológico==2026)


#dados_1<- dados_1 %>%
# mutate(LS = ifelse(DS_UF_SIGLA %in% c( "PE"), NA, LS),
#       LI = ifelse(DS_UF_SIGLA %in% c("PE"), NA, LI)) #%>%
# filter(Ano.epidemiológico==2026)%>%
# mutate(sequencia=Semana.epidemiológica)


mt<-ggplot(data = dados_1) +
  geom_col(
    aes(x = Semana.epidemiológica, y = Casos.semanais.reportados.até.a.última.atualização, fill = "Casos notificados"), 
    color = 'lightblue',
    alpha = 0.75) +
  geom_line(
    aes(x = Semana.epidemiológica, y = media.movel, color = "Média móvel"), 
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c("Média móvel" = 'black')
  ) +
  geom_ribbon(
    data = dados_1, 
    aes(x = Semana.epidemiológica, ymin = LI, ymax = LS, fill = "Casos estimados"), 
    alpha = 0.2) +
  scale_fill_manual(
    values = c("Casos estimados"='#184E77',
               "Casos notificados" = 'lightblue')
  ) +
  scale_x_continuous(breaks = seq(1, 53, by=4))+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  )+
  # scale_x_continuous(
  #   breaks = seq(1, max(dados_1$sequencia), by = 12),
  #    labels = function(x) {
  # Converter sequencia para semana epidemiológica
  # Se sequencia <= 52: mantém como está (ano 2024)
  # Se sequencia > 52: subtrai 52 (ano 2025)
  #    semana_epi = ifelse(x <= 52, x, x - 52)
  #    return(as.character(semana_epi))
  #   }
  # ) +
  labs(
    x = "Semana epidemiológica",
    y = 'Casos de SRAG por METAPNEUMOVÍRUS ', 
    color = '', 
    fill = ''
  ) +
  ggtitle(
    paste0("Casos estimados de SRAG por Metapneumovírus até a Semana ", last_week ," 2026")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    axis.title = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(), 
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size= unit(0.4, "cm"),
    legend.spacing = unit(0, "cm"),
    legend.title = element_text(face = "italic", size = rel(1)),
    legend.text = element_text(size = rel(.8)),
    plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
    strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    strip.text = element_text(face = "bold")
  ) +
  # facet_wrap(~ DS_UF_SIGLA, scale="free")+
  facet_geo(
    ~ DS_UF_SIGLA, 
    grid = 'br_states_grid1', 
    scale = 'free_y'
  ) +
  guides(fill = guide_legend(nrow = 1, title = NULL),
         color = guide_legend(nrow = 1, , title = NULL)) +
  theme(plot.caption = element_markdown())

png(paste0('Plots/GAM_fig_UFs_METAP.png'),height = 10, width = 8, units = 'in', res = 300)
plot(mt)
#grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
dev.off()

