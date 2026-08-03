get.intensity.info<- function(trajectories, region_code, window=2, escala_reg="UF", casos=TRUE){
  
  if(casos=="TRUE"){
    
    limi<-limiar %>%
      filter(escala=="casos") %>%
      filter(escala_reg == escala_reg) %>%
      mutate(across(1:5, as.numeric)) %>%
      filter(cod_regiao==region_code)  
  }else{
    
    limi<-limi_obito %>%
      filter(escala=="obitos") %>%
      mutate(across(1:5, as.numeric)) %>%
      filter(cod_regiao==region_code)  
    
    
  }
  
  ##calculando intensidade nas últimas 2 semanas
  inten<-  trajectories %>%
 #   filter(CO_UF==region_code)%>% ###caso for fazer a filtragem por UF dentro da função
   # mutate(dt_event = epiweek + ifelse(epiyear == max(epiyear)-1, 52, 0))%>%
    group_by(sample,Time,dt_event) %>%
    summarise(Y=sum(Y, na.rm = TRUE))%>%
    as.data.frame() %>%
    filter(dt_event>(max(dt_event)-window)) %>%
    group_by() %>%
    summarise("Baixo risco"=sum(Y<limi$baixo, na.rm=TRUE)*100/n(),
              "Segurança"=sum(Y>=limi$baixo & Y<limi$moderado, na.rm=TRUE)*100/n(),
              "Alerta"=sum(Y>=limi$moderado & Y<limi$alto, na.rm=TRUE)*100/n(),
              "Risco"=sum(Y>=limi$alto & Y<limi$muito_alto, na.rm=TRUE)*100/n(),
              "Alto risco"=sum(Y>=limi$muito_alto, na.rm=TRUE)*100/n()) %>%
    pivot_longer(
      everything(),
      names_to = "intensidade",
      values_to = "prob"
    ) %>%
    mutate(cum=cumsum(prob))
  
  inten_f<-inten %>% filter(cum>=50) %>%
    filter(cum==min(cum)) %>% 
    filter(prob>0) %>% ###excluir as regiões em que a prob == 0, caso tenha repetição de valor do cumsum
    select(intensidade) 
  
  return(inten_f)
  
}
