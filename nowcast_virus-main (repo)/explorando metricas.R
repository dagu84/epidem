
library(tidyverse)
library(ggplot2)

metricas<-read.csv("validation/metrics_nowcast_inla_gam.csv")

metricas <- metricas %>%
  mutate(
    regiao = case_when(
      DS_UF_SIGLA %in% c("RS", "PR", "SC") ~ "Sul",
      DS_UF_SIGLA %in% c("SP", "RJ", "ES", "MG") ~ "Sudeste",
      DS_UF_SIGLA %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      DS_UF_SIGLA %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
      DS_UF_SIGLA %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      TRUE ~ NA_character_
    )
  )%>%
  mutate(dt_start = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) 

###Plotando o observando com predito

m2<- metricas %>% filter(agente == "FLU_A") %>%
  filter(regiao=="Sul")

head(m2)

ggplot(m2)+
  geom_line(aes(x-dt_start, y=))


wis<-metricas %>%
   filter(agente %in% c("FLU_A", "VSR", "SARS2", "RINO")) %>%
   group_by(DS_UF_SIGLA, metodo, agente) %>%
  dplyr::summarise(n= n(),
                   cob50=round(sum(cov_50*100)/n(),1),
                   cob95=round(sum(cov_95*100)/n(),1),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   Spread=round(median(dispersion, na.rm=TRUE),1),
                   Underprediction=round(median(underprediction, na.rm=TRUE),1),
                   Overprediction=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1))


wis2<- wis %>%
  select(DS_UF_SIGLA, agente, metodo, Spread, Underprediction, Overprediction) %>%
  pivot_longer(Spread:Overprediction, names_to = "WIS", values_to = "components")




wis2 %>%
  filter(agente =="SARS2") %>%
  filter(DS_UF_SIGLA %in% c("BR", "SP", "RJ", "MG", "CE", "RS", "MS", "PI", 
                            "PR", "PA", "AM", "BA", "TO")) %>%
  # ggplot(aes(x = components, y = reorder(DRS, WIS), fill = WIS)) +
  ggplot(aes(x = metodo, y= components, fill = WIS)) +
  geom_bar(stat = "identity")+
  scale_fill_manual("Decomposition WIS",values=c('pink','lightpink3','lightpink4'))+
  theme_bw()+
  facet_wrap(~ DS_UF_SIGLA, scale="free", ncol=5)+
  #scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(legend.position = "top", axis.text=element_text(size=15),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.title = element_blank(),
        plot.title = element_text(size=18, face="bold")
  )+
  labs(x="Median WIS components", y="")

wis %>%
  filter(agente =="FLU_A") %>%
  filter(DS_UF_SIGLA %in% c("BR", "SP", "RJ", "MG", "CE", "RS", "MS", "PI", 
                            "PR", "PA", "AM", "BA", "TO")) %>%
ggplot(mapping = aes(x, y)) +
  geom_bar(aes(y=DS_UF_SIGLA  ,x=cob95, fill="95%"), stat = 'identity') +
  geom_bar( aes(y=DS_UF_SIGLA  ,x=cob50, fill="50%"), stat = 'identity') +
  scale_fill_manual(values = c(
    "95%"= 'skyblue1',
    "50%"='dodgerblue4'))+
  labs(fill = 'Prediction Interval', x="Coverage %", y="")+
  facet_wrap(~ DS_UF_SIGLA, scale="free", ncol=5)+
  # scale_x_discrete(guide = guide_axis(angle = 45))+
  # geom_xline(yintercept = 95, col = "darkred", linetype='dotted', size=1)+
  # geom_xline(yintercept = 50, col = "red", linetype='dotted', size=1)+
  theme_bw()+
  theme(legend.position = "top", axis.text=element_text(size=15),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        axis.title.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=18, face="bold")
  )
