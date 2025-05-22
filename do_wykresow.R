library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dane_rozne_S_0<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_50_r_0.02_K_48_T_2_.csv",
                         header = T,sep = ",")


subset(dane_rozne_S_0,dane_rozne_S_0$wersja=="put") %>%
  ggplot(aes(x=  S_0,y = cena_opcji,col = opcja))+geom_line()
subset(dane_rozne_S_0,dane_rozne_S_0$wersja=="call"&dane_rozne_S_0$S_0 ==50)
