#devtools::install_version(package = "ggplot2", version = "3.5.2", 
 #                         repos = "http://cran.us.r-project.org")

load("output/nowcast_srag_por_virus_uf_sem_filtro_febre.RData")
load("output/trajetorias_srag_por_virus_uf_sem_filtro_febre.RData")

dados_virus <- read.csv2(file = "C:/Users/tatty/Documents/00_FIOCRUZ/Boletim_InfoGripe/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")


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
         left_join(total.summy, by=c("DS_UF_SIGLA", "Time", "dt_event", "virus"))%>%
  mutate(virus2=virus)%>%
  mutate(virus=case_when(
    virus2=="FLU_B" ~ "Influenza B",
    virus2=="FLU_A" ~ "Influenza A",
    virus2=="SARS2" ~ "SARS-CoV-2",
    virus2=="METAP" ~ "Metapneumovírus",
    virus2=="RINO" ~ "Rinovírus",
    virus2=="VSR" ~ "VSR"
  )) 


last_week<- epiweek(max(big_data$dt_event))
virus<-c(unique(big_data$virus2))

#i<-"SP"

for (i in virus) {
  
  
  covi <- big_data %>% 
    mutate(Semana.epidemiológica=epiweek(dt_event),
           Ano.epidemiológico=epiyear(dt_event)) %>%
    mutate(sequencia = ifelse(Ano.epidemiológico == 2025, Semana.epidemiológica, Semana.epidemiológica+53)) %>% ##mudar aqui depois
    filter(DS_UF_SIGLA!="BR") %>%
    filter(virus2==i) %>%
    as.data.frame()
  
  nome_virus<- unique(covi$virus)
  
  dados_1<- dados_virus %>%
    mutate(Casos.semanais.reportados.até.a.última.atualização=dados_virus[[i]]) %>%
    filter(Ano.epidemiológico>=2025, fx_etaria=="Total", DS_UF_SIGLA!="BR") %>%
    mutate(sequencia = ifelse(Ano.epidemiológico == max(Ano.epidemiológico)-1, Semana.epidemiológica, Semana.epidemiológica+53)) %>%
    full_join(covi, by=c("sequencia", "DS_UF_SIGLA", "Ano.epidemiológico", "Semana.epidemiológica")) %>%
    select(Casos.semanais.reportados.até.a.última.atualização, Ano.epidemiológico, Semana.epidemiológica, DS_UF_SIGLA, sequencia, Median, LI, LS)%>%
    filter(Ano.epidemiológico==2026)

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
  
  if(i=="FLU_B"){
    
    dados_2<- dados_2 %>%
      mutate(LS = ifelse(DS_UF_SIGLA %in% c( "RR", "PI", "AC", "RO", "TO", "AM"), NA, LS),
            LI = ifelse(DS_UF_SIGLA %in% c("RR", "PI", "AC", "RO", "TO", "AM"), NA, LI))  
    
  }else{
    
    dados_2<- dados_2 %>%
      mutate(LS = ifelse(DS_UF_SIGLA %in% c( "RR", "PI", "AP"), NA, LS),
             LI = ifelse(DS_UF_SIGLA %in% c("RR", "PI", "AP"), NA, LI))   
    
  }


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
      y = paste0('Casos de SRAG por ', nome_virus ), 
      color = '', 
      fill = ''
    ) +
    ggtitle(
      paste0("Casos estimados de SRAG por ", nome_virus, " até a Semana ", last_week ," 2026")
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
  
  png(paste('Plots/fig_UFs_', i ,'.png'),height = 10, width = 8, units = 'in', res = 300)
  plot(pcovi)
  grid::grid.raster(info.logo, x = 0.98, y = 0.98, just = c('right', 'top'), width = unit(.8, 'inches'))
  dev.off()
  
}

####Grafico por regiao, incluindo Brasil

UF<-as.list(unique(big_data$DS_UF_SIGLA))

for (i in UF) {
  
    data_uf <- big_data %>% 
      #filter(virus=="SARS2")%>%
      filter(DS_UF_SIGLA==i) %>%
      mutate(Semana.epidemiológica=epiweek(dt_event)) %>%
      # filter(DS_UF_SIGLA!="BR") %>%
      mutate(Ano.epidemiológico=epiyear(dt_event)) %>%
      as.data.frame() %>%
      mutate(LS = ifelse(DS_UF_SIGLA %in% c("PI", "AC", "RR"), NA, LS),
                     LI = ifelse(DS_UF_SIGLA %in% c("PI", "AC", "RR"), NA, LI)) %>%
      select(!virus)
    
    last_week<-epiweek(max(data_uf$dt_event))
    min_epi<-min(data_uf$dt_event)
    
    dados_1<- dados_virus %>%
      filter(Ano.epidemiológico>=2023, fx_etaria=="Total", DS_UF_SIGLA==i) %>%
      mutate(data = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
      filter(data < min_epi)%>%
      select(SG_UF_NOT, data, Semana.epidemiológica, Ano.epidemiológico, DS_UF_SIGLA, SARS2, FLU_A, VSR, RINO, FLU_B, METAP) %>%
      pivot_longer(cols = c("SARS2", "FLU_A", "VSR", "RINO", "FLU_B", "METAP"), names_to = "virus2", values_to = "Median") %>%
      rename(CO_UF=SG_UF_NOT) 
    
    
    dados_uf_virus<-bind_rows(dados_1, data_uf)
    
    
    df<- dados_uf_virus  %>%arrange(virus2, Ano.epidemiológico, Semana.epidemiológica) %>%
      group_by(virus2) %>%  # agrupa por ano também
      mutate(media_m = round(zoo::rollmean(Median, k = 3, fill = NA, align = "center"))) 
  
    
  png(paste0("Plots/fig_", i, "_virus_2023_2026.png"),height = 8, width = 10, units = 'in', res = 300)
  plot(plot_virus_uf(df, uf=i))
  grid::grid.raster(info.logo, x = 0.98, y = 1.0, just = c('right', 'top'), width = unit(.8, 'inches'))
  dev.off()
}

#  mutate(LS = ifelse(DS_UF_SIGLA %in% c("PI", "RR"), NA, LS),
 #        LI = ifelse(DS_UF_SIGLA %in% c("PI", "RR"), NA, LI))

