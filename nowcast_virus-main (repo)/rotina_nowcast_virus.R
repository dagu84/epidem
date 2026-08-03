library(dplyr)
library(vroom)
library(tidyverse)
library(geofacet)
library(zoo)
library(nowcaster)
library(INLA)
library(ggtext)
library(ggplot2)
library(rio)
library(tidyverse)
library(geobr)
library(sf)
library(ggthemes)
library(gridExtra)
library(magick)
library(geomtextpath)
library(patchwork)


#devtools::install_github("https://github.com/covid19br/nowcaster")

#install.packages("INLA",
#                 repos=c(getOption("repos"),
  #                       INLA="https://inla.r-inla-download.org/R/stable"), 
  #               dep=TRUE)

#devtools::install_version(package = "ggplot2", version = "3.5.2", repos = "http://cran.us.r-project.org")

info.logo <- image_read("infogripe.png")

###Lendo bases novas e salvando
df1<-read.csv2("C:/Users/tatty/Documents/00_FIOCRUZ/Boletim_InfoGripe/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")

ano<-max(df1$Ano.epidemiológico)
sem<-max(df1$Semana.epidemiológica[df1$Ano.epidemiológico == ano], na.rm = TRUE)

write.csv2(df1, paste0("Dados/casos_semanais_fx_etaria_virus_sem_filtro_febre_",ano,"_",sem,".csv"))

df2<-read.csv2("C:/Users/tatty/Documents/00_FIOCRUZ/Boletim_InfoGripe/Dados/InfoGripe/obitos_semanais_fx_etaria_virus_sem_filtro_febre.csv")

ano<-max(df2$Ano.epidemiológico)
sem<-max(df2$Semana.epidemiológica[df2$Ano.epidemiológico == ano], na.rm = TRUE)

write.csv2(df2, paste0("Dados/obitos_semanais_fx_etaria_virus_sem_filtro_febre_",ano,"_",sem,".csv"))

source("fct/get.intensity.R")
source("fct/Graficos por UF.R")
source("fct/nowcasting_diff_inla.R")

##Nowcast por vírus
source("scripts/nowcast_virus.R")

##Gráficos casos vírus
source("scripts/graficos.R")

##Gráficos covid-19
source("scripts/grafico_covid.R")

##Gráfico óbitos SRAG
source("scripts/Graficos_obitos.R")

###Tabela estimados por vírus
source("scripts/tabela_estimados_virus.R")

