

now_obt<- get(load("output/nowcast_obitos_srag_por_virus_uf_sem_filtro_febre.RData"))

load("output/trajetorias_obitos_srag_por_virus_uf_sem_filtro_febre.RData")


total.summy <- big_data2_obt |>
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

now_obt<- now_obt %>%
  select(-LI, -LS)%>%
  left_join(total.summy, by=c("DS_UF_SIGLA", "Time", "dt_event", "virus"))

###Limiares
limi_obt<-vroom("https://raw.githubusercontent.com/infogripe/Boletim_InfoGripe/main/Dados/InfoGripe/limiares_SRAG_obitos_UF.csv")

limi_obt<-limi_obt %>%
          mutate(DS_UF_SIGLA=regiao) %>%
         filter(escala=="obitos") 

#last_week<- 2

srag_obt <- now_obt %>% 
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(sequencia = ifelse(epiyear == 2025, epiweek, epiweek+53)) %>% ##mudar aqui depois
  filter(DS_UF_SIGLA!="BR") %>%
  filter(virus=="SRAG")


obt_virus <- read.csv2("https://raw.githubusercontent.com/infogripe/Boletim_InfoGripe/main/Dados/InfoGripe/obitos_semanais_fx_etaria_virus_sem_filtro_febre.csv")

str(obt_virus)

dados_1<- obt_virus %>%
  filter(epiyear>=2025, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(sequencia = ifelse(epiyear == max(epiyear)-1, epiweek, epiweek+53)) %>%
  mutate(Casos.semanais.reportados.até.a.última.atualização=SRAG) %>%
  full_join(srag_obt, by=c("sequencia", "DS_UF_SIGLA", "epiyear", "epiweek")) %>%
  select(Casos.semanais.reportados.até.a.última.atualização, epiyear, epiweek, DS_UF_SIGLA, sequencia, Median, LI, LS) %>%
  filter(epiyear==2026)

med<- dados_1 %>%
  filter(sequencia<min(srag_obt$sequencia))%>%
  mutate(Median=Casos.semanais.reportados.até.a.última.atualização)%>%
  select(Median, epiweek, epiyear, DS_UF_SIGLA, sequencia)%>%
  bind_rows(srag_obt) %>%
  arrange(sequencia)%>%
  group_by(DS_UF_SIGLA)%>%
  mutate(media.movel = round(rollmean(Median, k = 3, fill = NA, align = "center"))) %>%
  ungroup() %>%
  select(epiyear, epiweek, sequencia, DS_UF_SIGLA, media.movel)

dados_2<- dados_1 %>%
  left_join(med,  by=c("DS_UF_SIGLA", "epiyear", "epiweek", "sequencia")) %>%
  left_join(limi_obt, by="DS_UF_SIGLA")



dados_2<- dados_2 %>%
  mutate(LS = ifelse(DS_UF_SIGLA %in% c("AP"), NA, LS),
         LI = ifelse(DS_UF_SIGLA %in% c("AP"), NA, LI))

psrag<-ggplot(data = dados_2) +
  geom_col(
    aes(x = epiweek, y = Casos.semanais.reportados.até.a.última.atualização, fill = "Casos notificados"), 
    color = 'lightblue',
    alpha = 0.75) +
  geom_line(
    aes(x = epiweek, y = media.movel, color = "Média móvel"), 
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c("Média móvel" = 'black')
  ) +
  geom_ribbon(
    data = dados_2, 
    aes(x = epiweek, ymin = LI, ymax = LS, fill = "Casos estimados"), 
    alpha = 0.2) +
  scale_fill_manual(
    values = c("Casos estimados"='#184E77',
               "Casos notificados" = 'lightblue')
  ) +
  geom_hline(aes(yintercept = dados_2$baixo, linetype = "Baixo"), colour = "#018571") + 
  geom_hline(aes(yintercept = dados_2$moderado, linetype = "Moderado"), colour = "#dfc27d") + 
  geom_hline(aes(yintercept = dados_2$alto, linetype = "Alto"), colour = "#BE8B44") + 
  geom_hline(aes(yintercept = dados_2$muito_alto, linetype = "Muito alto"), colour = "#a6611a") + 
  scale_linetype_manual(name = "Limiares", 
                        values = c(2, 2,2,2),
                        labels= c("Baixo", "Moderado", "Alto", "Muito alto"),
                        guide = guide_legend(override.aes = list(color = c("#018571", "#dfc27d", "#BE8B44", "#a6611a" ))))+
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
  #  }
 # ) +
  labs(
    x = "Semana epidemiológica",
    y = 'Óbitos de SRAG', 
    color = '', 
    fill = ''
  ) +
  ggtitle(
    paste0("Óbitos estimados de SRAG até a Semana ", last_week ," 2026")
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
    legend.direction = "horizontal",
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

png(paste0('Plots/fig_UFs_obitos_srag.png'),height = 10, width = 8, units = 'in', res = 300)
plot(psrag)
grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
dev.off()

