

df<-read.csv("validation/all_models_last_4weeks_nowcast.csv")

df <- df %>%
  mutate(
    regiao = case_when(
      DS_UF_SIGLA %in% c("RS", "PR", "SC") ~ "Sul",
      DS_UF_SIGLA %in% c("SP", "RJ", "ES", "MG") ~ "Sudeste",
      DS_UF_SIGLA %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      DS_UF_SIGLA %in% c("AC", "AM", "AP", "PA") ~ "Norte",
      DS_UF_SIGLA %in% c("RO", "RR", "TO") ~ "Norte2",
      DS_UF_SIGLA %in% c("AL", "BA", "CE", "MA", "PB") ~ "Nordeste",
      DS_UF_SIGLA %in% c("PE", "PI", "RN", "SE") ~ "Nordeste2",
      DS_UF_SIGLA == "BR" ~ "Brasil",
      TRUE ~ NA_character_
    )
  )%>%
  mutate(dt_start = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) 

m2<- df %>% filter(agente == "FLU_A") %>%
  filter(regiao=="Sul")


region<-c(unique(df$regiao))
agent<-c(unique(df$agente))

for (i in region) {
  
  m2<- df %>%  filter(regiao==i)
  
  for (j in agent) {
    
  m3<- m2 %>% filter(agente==j)
  
  n_uf<-length(unique(m3$DS_UF_SIGLA))
  
 p<- ggplot(m3)+
    #geom_line(aes(x=dt_start, y=Median), linewidth = 0.8)+
    geom_ribbon(aes(x=dt_start,ymax= LS, ymin=LI), alpha=0.8, fill="lightblue")+
    geom_point(aes(x=dt_start, y=obs), size=0.5)+
    facet_wrap(~DS_UF_SIGLA+metodo, scale="free", nrow=n_uf)+
    theme_bw()+
    labs(
      x = "Epidemiological week",
      y = paste("ARI", j, 
                color = '', 
                fill = ''))
    
    png(paste0("validation/Plots/fig_", i, "_", j, ".png"),height = 8, width = 10, units = 'in', res = 300)
    
plot(p)
    
    dev.off()
    
  }
  
}




