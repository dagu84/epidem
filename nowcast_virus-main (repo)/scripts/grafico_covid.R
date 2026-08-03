
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


limiar_covid<-read.csv("C:/Users/tatty/Documents/00_FIOCRUZ/Boletim_InfoGripe/Dados/InfoGripe/limiares_COVID_casos_UF.csv")
  
limi_covid2<- limiar_covid %>%
            filter(escala=="casos") %>%
            mutate(DS_UF_SIGLA=regiao)
  
  
big_data <- big_data %>% 
  filter(virus=="SARS2")%>%
  mutate(Semana.epidemiológica=epiweek(dt_event)) %>%
  filter(DS_UF_SIGLA!="BR") %>%
  mutate(Ano.epidemiológico=epiyear(dt_event)) %>%
  mutate(Casos.semanais.reportados.até.a.última.atualização=Median)

last_week<- epiweek(max(big_data$dt_event))
min_epi<-min(big_data$dt_event)

dados_virus <- read.csv2(file = "C:/Users/tatty/Documents/00_FIOCRUZ/Boletim_InfoGripe/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")

dados_1<- dados_virus %>%
  filter(Ano.epidemiológico>=2023, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
  mutate(data = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  filter(data < min_epi)%>%
  group_by(SG_UF_NOT, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA) %>%
  summarise(Casos.semanais.reportados.até.a.última.atualização=sum(SARS2, na.rm = TRUE)) %>%
  rename(CO_UF=SG_UF_NOT) %>%
  select(CO_UF, Casos.semanais.reportados.até.a.última.atualização, Ano.epidemiológico,
         Semana.epidemiológica, DS_UF_SIGLA)# %>%
#  full_join(big_data, by=c("Semana.epidemiológica", "DS_UF_SIGLA", "Ano.epidemiológico", "Casos.semanais.reportados.até.a.última.atualização")) #%>%
#filter(Semana.epidemiológica>15)
dados_2<-bind_rows(dados_1, big_data)

dados_2<- dados_2 %>%arrange(Ano.epidemiológico, Semana.epidemiológica) %>%
  group_by(DS_UF_SIGLA) %>%  # agrupa por ano também
  mutate(media_m = round(zoo::rollmean(Casos.semanais.reportados.até.a.última.atualização, k = 3, fill = NA, align = "center"))) %>%
  ungroup() %>%
  full_join(limi_covid2, by=c("DS_UF_SIGLA"))


df<- dados_2 %>%
  mutate(LS = ifelse(DS_UF_SIGLA %in% c("PI", "AC", "RR", "AP"), NA, LS),
         LI = ifelse(DS_UF_SIGLA %in% c("PI", "AC", "RR", "AP"), NA, LI))

plot<-ggplot(df) +
  geom_line(data=df,
            aes(x = Semana.epidemiológica , y = media_m, color = as.factor(Ano.epidemiológico)), 
            linewidth = 0.8
  ) +
  scale_color_manual(values=colorblind_pal()(4)[c(4, 3, 2, 1)])+
  geom_ribbon(
    data = df %>%  filter(!is.na(LI), LS < 200), 
    #filter(!is.na(LI)), 
    aes(x = Semana.epidemiológica, ymin = LI, ymax = LS, fill = as.factor(Ano.epidemiológico)), 
    alpha = 0.4) +
  scale_fill_manual(
    values = c("Casos estimados"='#184E77')
  ) +
  scale_x_continuous(breaks = c(1, seq(9, 53, by = 8)))+
  geom_hline(aes(yintercept = df$baixo, linetype = "Baixo"), colour = "#018571") + 
  geom_hline(aes(yintercept = df$moderado, linetype = "Moderado"), colour = "#dfc27d") + 
  geom_hline(aes(yintercept = df$alto, linetype = "Alto"), colour = "#BE8B44") + 
  geom_hline(aes(yintercept = df$muito_alto, linetype = "Muito alto"), colour = "#a6611a") + 
  scale_linetype_manual(name = "Limiares", 
                        values = c(2, 2,2,2),
                        labels= c("Baixo", "Moderado", "Alto", "Muito alto"),
                        guide = guide_legend(override.aes = list(color = c("#018571", "#dfc27d", "#BE8B44", "#a6611a" ))))+
  labs(
    x = "Semana epidemiológica",
    y = 'Casos de SRAG por Covid-19 ', 
    color = '', 
    fill = ''
  ) +
  ggtitle(
    paste0("Casos estimados e notificados de SRAG por Covid-19 até a Semana ", last_week, " de 2026")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.0), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    axis.title = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text.x = element_text(angle=45, size=6),
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
  facet_geo(
    ~ DS_UF_SIGLA, 
    grid = 'br_states_grid1', 
    scale = 'free'
  ) +
  guides(fill = guide_legend(nrow = 1, title = NULL),
         color = guide_legend(nrow = 1, title = NULL)) +
  
  # labs(caption = "<img src='https://gitlab.fiocruz.br/lsbastos/infogripe_code/-/raw/main/MEM_SRAG/plots/infogripe.png' width='100'/>") +
  theme(plot.caption = element_markdown())


png(paste0('Plots/fig_UFs_covid_2023_2026.png'),height = 10, width = 8, units = 'in', res = 300)
plot(plot)
grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
dev.off()

