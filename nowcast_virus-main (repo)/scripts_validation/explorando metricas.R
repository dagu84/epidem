install.packages("hrbrthemes")
library(hrbrthemes)
library(viridis)
library(tidyverse)

metricas<-read.csv("validation/metrics_nowcast_inla_gam.csv")


wis<-metricas %>%
   filter(agente %in% c("FLU_A", "VSR", "SARS2", "RINO")) %>%
   group_by(metodo, agente, DS_UF_SIGLA) %>%
  dplyr::summarise(n= n(),
                   cob50=round(sum(cov_50*100)/n(),1),
                   cob95=round(sum(cov_95*100)/n(),1),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   Spread=round(median(dispersion, na.rm=TRUE),1),
                   Underprediction=round(median(underprediction, na.rm=TRUE),1),
                   Overprediction=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1)) %>%
  arrange(agente)

write.csv2(wis, "validation/metrics_UF.csv")


wis2<- wis %>%
  select(DS_UF_SIGLA, agente, metodo, Spread, Underprediction, Overprediction) %>%
  pivot_longer(Spread:Overprediction, names_to = "WIS", values_to = "components") 
  

wis3<- wis %>% filter(agente=="RINO")

png(paste0("validation/Plots/fig_coverage_RINO.png"),height = 10, width = 10, units = 'in', res = 300)
  
ggplot(wis3) +
      geom_bar(aes(y=cob95  ,x=metodo, fill="95%"), stat = 'identity') +
      geom_bar(aes(y=cob50  ,x=metodo, fill="50%"), stat = 'identity') +
      scale_fill_manual(values = c(
        "95%"= 'skyblue1',
        "50%"='dodgerblue4'))+
      labs(x="Method",
           y= "Coverage %",
           fill='Prediction Interval',
           title="RINO")+
  coord_cartesian(ylim = c(40, 100))+
      scale_x_discrete(guide = guide_axis(angle = 45))+
      geom_hline(yintercept = 95, col = "darkred", linetype='dotted', size=1)+
      geom_hline(yintercept = 50, col = "red", linetype='dotted', size=1)+
      theme_bw()+
      facet_wrap(~DS_UF_SIGLA) +
      #facet_wrap(~DRS, labeller=labeller(DRS=as_labeller(drs_label)))+
      theme(legend.position = "bottom", 
            axis.text=element_text(size=10),
            legend.text = element_text(size=14),
            axis.title = element_text(size=15),
            legend.title = element_text(size=14),
            plot.title = element_text(size=16, face="bold"),
            strip.text = element_text(face = "bold", size=16)
      )


dev.off()

head(metricas)

####Box plot dispersao
png(paste0("validation/Plots/box_plot_METAP.png"),height = 10, width = 20, units = 'in', res = 300)

metricas %>%
  filter(!DS_UF_SIGLA %in% c("RO", "PI", "AL", "TO", "RR", "AC", "AP", "BR")) %>%
  filter(agente=="METAP") %>%
  ggplot( aes(y=metodo, x=log(dispersion+0.0001), fill=metodo)) +
  #geom_boxplot() +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  facet_wrap(~DS_UF_SIGLA, scale="free")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("METAP") +
  xlab("")
  
  dev.off()

  
png(paste0("validation/Plots/violin_plot_FLU_A.png"),height = 10, width = 20, units = 'in', res = 300)
# Violin basic
metricas %>%
  filter(DS_UF_SIGLA!="BR")%>%
  filter(agente=="FLU_A") %>%
  ggplot( aes(y=metodo, x=dispersion, fill=metodo)) +
  geom_violin(outlier.shape = NA) +
  coord_cartesian(xlim = c(0, 10))+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  facet_wrap(~DS_UF_SIGLA, scale="free")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

dev.off()

png(paste0("validation/Plots/box_plot_AL_log.png"),height = 10, width = 20, units = 'in', res = 300)
# Violin basic
metricas %>%
  filter(DS_UF_SIGLA=="AL")%>%
  filter(!agente %in% c("METAP", "FLU_B")) %>%
  ggplot( aes(y=metodo, x=log(dispersion+0.0001), fill=metodo)) +
 # geom_violin() +
  geom_boxplot() +
 # coord_cartesian(xlim = c(0, 100))+
 # geom_violin(outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  facet_wrap(~agente, scale="free")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("AL") +
  xlab("")

dev.off()

