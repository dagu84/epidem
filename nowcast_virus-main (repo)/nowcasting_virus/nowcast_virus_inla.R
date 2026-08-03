library(dplyr)
library(vroom)
library(tidyverse)
library(geofacet)
library(zoo)
library(nowcaster)
library(INLA)
library(ggplot2)
library(rio)
library(tidyverse)

##function to run the nowcasting using the database difference method
source("fct/nowcasting_diff_inla.R")
#devtools::install_github("https://github.com/covid19br/nowcaster")

##reading the databases 
files <- list.files(
  path = "Dados/",
  pattern = "casos",
  full.names = TRUE
)


dados <- lapply(files, function(f) {
  vroom::vroom(f) %>%
    filter(fx_etaria == "Total", epiyear >= 2025) %>%
    select(SG_UF_NOT, DS_UF_SIGLA, epiyear, epiweek,
           SARS2, VSR, RINO, FLU_A)
}) %>%
  bind_rows(.id = "a") %>%
  mutate(dt_start = MMWRweek::MMWRweek2Date(epiyear, epiweek) + 6) %>%
  group_by(a) %>%
  mutate(dt_release = max(dt_start, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(dt_start>= min(dt_release)) %>%
  filter(!is.na(DS_UF_SIGLA)) %>%
  select(dt_start, dt_release,
         SARS2, VSR, RINO, FLU_A,
         SG_UF_NOT, DS_UF_SIGLA) %>%
  
  pivot_longer(
    cols = c(SARS2, VSR, RINO, FLU_A),
    names_to = "Virus",
    values_to = "cases"
  )


uf_sigla <- sort(unique(dados$DS_UF_SIGLA))

virus<- sort(unique(dados$Virus))


lista<-list()
lista2<-list()


###running the owcasting by virus type and region

for (j in virus) {
  
  dados2<- dados %>% filter(Virus==j)
  
  print(j)
  
for (i in uf_sigla) {
  
  sub_dado<- dados2 %>% filter (DS_UF_SIGLA==i) 
  
  
  freq_zero<- sub_dado %>% filter(dt_start>=(max(dt_start)-7*20))%>%
    summarise(freq_zero=sum(cases==0)/n()) %>%
    pull(freq_zero)
  
  print(i)
  
  if(freq_zero < 0.8){
    
    now_diff<- nowcasting_diff_inla(dataset= sub_dado,
                                    date_start = dt_start,
                                    date_release = dt_release,
                                    Dmax = 10,
                                    wdw=30,
                                    cases = cases,
                                    silent=F,
                                    trajectories = TRUE)
    
    
  }else{
    
    
    now_diff<- nowcasting_diff_inla(dataset= sub_dado,
                                    date_start = dt_start,
                                    date_release = dt_release,
                                    Dmax = 10,
                                    wdw=50,
                                    zero_inflated =TRUE,
                                    cases = cases,
                                    silent=F,
                                    trajectories = TRUE)
    
    
    
  }
  
  nowcast_total<-now_diff$total
  
  
  nowcast_total$tendencia.6s<-slope.estimate.quant(trajectories = now_diff$trajectories, window = 6)
  
  
  nowcast_total$DS_UF_SIGLA<-i
  nowcast_total$virus<-j
  
  trajetorias<-now_diff$trajectories
  trajetorias$DS_UF_SIGLA<-i
  trajetorias$virus<-j
  

  lista[[length(lista) + 1]] <- nowcast_total
  
  
  
  lista2[[length(lista2) + 1]] <- trajetorias
  
}

}

big_data<-bind_rows(lista)
big_data2<-bind_rows(lista2)


