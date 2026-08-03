
plot_virus_uf<- function(data, uf){
  
  plot<-ggplot(data) +
    geom_ribbon(
      data = df %>% filter(!is.na(LI) & !is.na(LS)),
      aes(x = Semana.epidemiológica, ymin = LI, ymax = LS, fill = as.factor(Ano.epidemiológico)),
      alpha = 0.4
    )+
    scale_fill_manual(
      values = c("Casos estimados"='#184E77')
    ) +
    geom_line(data=data,
              aes(x = Semana.epidemiológica , y = media_m, color = as.factor(Ano.epidemiológico)), 
              linewidth = 0.8
    ) +
    scale_color_manual(values=colorblind_pal()(4)[c(4, 3, 2, 1)])+
    scale_x_continuous(breaks = c(1, seq(9, 53, by = 8)))+
    labs(
      x = "Semana epidemiológica",
      y = 'Casos de SRAG por vírus respiratório', 
      color = '', 
      fill = ''
    ) +
    ggtitle(
      paste0(uf, " - Casos estimados e notificados de SRAG até a Semana ", last_week ," de 2026")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5,  margin = margin(t= 8, b = 8)),
      text = element_text(),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold",size = 12),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text.x = element_text(angle=45, size=10),
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
      legend.title = element_text(face = "italic", size = 14),
      legend.text = element_text(size = 12),
      plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
      strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold", size=13)
    ) +
    facet_wrap(
      ~virus2,
      labeller = as_labeller(c(
        "FLU_A" = "Influenza A",
        "FLU_B" = "Influenza B",
        "SARS2" = "SARS-CoV-2",
        "METAP" = "Metapneumovírus",
        "VSR" = "VSR",
        "RINO" = "Rinovírus"
      )), scales="free"
    )+
    guides(fill = guide_legend(nrow = 1, title = NULL),
           color = guide_legend(nrow = 1, title = NULL)) +
    
    # labs(caption = "<img src='https://gitlab.fiocruz.br/lsbastos/infogripe_code/-/raw/main/MEM_SRAG/plots/infogripe.png' width='100'/>") +
    theme(plot.caption = element_markdown())
  
 return(plot)
}



