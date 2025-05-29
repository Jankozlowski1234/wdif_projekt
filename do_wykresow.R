library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

czas<-read.csv("./dane/dlugosc_liczenia_N_1000.csv",
               header = T,sep = ",")
czas$wersja<-paste(czas$opcja,czas$wersja,sep = ",")

ggplot(czas,aes(x= odw_t,y = sredni_czas,col = wersja))+geom_line()+
  labs(title="Sredni czas liczenia ceny opcji",
       subtitle = "dla różnych 1/t",x="1/t",y="sredni czas")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("sredni_czas.pdf",path  = "./wykresy")



d_r_ot<-read.csv("./dane/dane_odwr_t_od_2_do_98_sigma_0.3_S_0_50_r_0.02_K_48_T_2_.csv",
                header = T,sep = ",")
d_r_s<-read.csv("./dane/dane_odwr_t_12_sigma_od_0.1_do0.6500000000000001_S_0_od_30_do_79_r_0.02_K_48_T_2_.csv",
                header = T,sep = ",")
d_r_S0<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_od_30_do_79_r_0.02_K_48_T_2_.csv",
                 header = T,sep = ",")
d_r_r<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_50_r_od_-0.03_do_0.19000000000000003_K_48_T_2_.csv",
                header = T,sep = ",")
d_r_K<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_50_r_0.02_K_od_30_do_79_T_2_.csv",
                header = T,sep = ",")
d_r_T<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_50_r_0.02_K_48_T_od_1_do_99_.csv",
                header = T,sep = ",")


max_y <- max(max(d_r_ot$cena_opcji),max(d_r_s$cena_opcji),max(d_r_S0$cena_opcji),
            max(d_r_r$cena_opcji),max(d_r_K$cena_opcji),max(d_r_T$cena_opcji))
min_y <-min(min(d_r_ot$cena_opcji),min(d_r_s$cena_opcji),min(d_r_S0$cena_opcji),
            min(d_r_r$cena_opcji),min(d_r_K$cena_opcji),min(d_r_T$cena_opcji))
#d_r_ot

subset(d_r_ot,d_r_ot$wersja=="put") %>%
  ggplot(aes(x=  t,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put dla roznych t",x="t",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p_r_ot.pdf",path  = "./wykresy")

subset(d_r_ot,d_r_ot$wersja=="call") %>%
  ggplot(aes(x=  t,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji call dla roznych t",x="t",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("c_r_ot.pdf",path  = "./wykresy")

subset(d_r_ot) %>%
  ggplot(aes(x=  t,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put i call dla roznych t",x="t",y="cena opcji",
       subtitle = "z podziałem na opcje amerykańskie i europejskie")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+facet_wrap(~wersja)+
  ylim(min_y,max_y)
ggsave("d_r_ot.pdf",path  = "./wykresy")

#d_r_s
subset(d_r_s,d_r_s$wersja=="put") %>%
  ggplot(aes(x=  sigma,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put dla roznych sigma",x="sigma",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p_r_s.pdf",path  = "./wykresy")

subset(d_r_s,d_r_s$wersja=="call") %>%
  ggplot(aes(x=  sigma,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji call dla roznych sigma",x="sigma",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("c_r_s.pdf",path  = "./wykresy")

subset(d_r_s) %>%
  ggplot(aes(x=  sigma,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put i call dla roznych sigma",x="sigma",y="cena opcji",
       subtitle = "z podziałem na opcje amerykańskie i europejskie")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+facet_wrap(~wersja)+
  ylim(min_y,max_y)
ggsave("d_r_s.pdf",path  = "./wykresy")

#d_r_S0
subset(d_r_S0,d_r_S0$wersja=="put") %>%
  ggplot(aes(x=  S_0,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put dla roznych S_0",x="S_0",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p_r_S0.pdf",path  = "./wykresy")

subset(d_r_S0,d_r_S0$wersja=="call") %>%
  ggplot(aes(x=  S_0,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji call dla roznych S_0",x="S_0",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("c_r_S0.pdf",path  = "./wykresy")


subset(d_r_S0) %>%
  ggplot(aes(x=  S_0,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put i call dla roznych S_0",x="S_0",y="cena opcji",
       subtitle = "z podziałem na opcje amerykańskie i europejskie")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+facet_wrap(~wersja)+
  ylim(min_y,max_y)
ggsave("d_r_S0.pdf",path  = "./wykresy")

#d_r_r
subset(d_r_r,d_r_r$wersja=="put") %>%
  ggplot(aes(x=  r,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put dla roznych r",x="r",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p_r_r.pdf",path  = "./wykresy")

subset(d_r_r,d_r_r$wersja=="call") %>%
  ggplot(aes(x=  r,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji call dla roznych r",x="r",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("c_r_r.pdf",path  = "./wykresy")

subset(d_r_r) %>%
  ggplot(aes(x=  r,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put i call dla roznych r",x="r",y="cena opcji",
       subtitle = "z podziałem na opcje amerykańskie i europejskie")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+facet_wrap(~wersja)+
  ylim(min_y,max_y)
ggsave("d_r_r.pdf",path  = "./wykresy")

#d_r_K
subset(d_r_K,d_r_K$wersja=="put") %>%
  ggplot(aes(x=  K,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put dla roznych K",x="K",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p_r_K.pdf",path  = "./wykresy")

subset(d_r_K,d_r_K$wersja=="call") %>%
  ggplot(aes(x=  K,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji call dla roznych K",x="K",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("c_r_K.pdf",path  = "./wykresy")


subset(d_r_K) %>%
  ggplot(aes(x=  K,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put i call dla roznych K",x="K",y="cena opcji",
       subtitle = "z podziałem na opcje amerykańskie i europejskie")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+facet_wrap(~wersja)+
  ylim(min_y,max_y)
ggsave("d_r_K.pdf",path  = "./wykresy")

#d_r_T

subset(d_r_T,d_r_T$wersja=="put") %>%
  ggplot(aes(x=  T,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put dla roznych T",x="T",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p_r_T.pdf",path  = "./wykresy")

subset(d_r_T,d_r_T$wersja=="call") %>%
  ggplot(aes(x=  T,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji call dla roznych T",x="T",y="cena opcji")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("c_r_T.pdf",path  = "./wykresy")

subset(d_r_T) %>%
  ggplot(aes(x=  T,y = cena_opcji,col = opcja))+geom_line()+
  labs(title="Cena opcji put i call dla roznych T",x="T",y="cena opcji",
       subtitle = "z podziałem na opcje amerykańskie i europejskie")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+facet_wrap(~wersja)+
  ylim(min_y,max_y)
ggsave("d_r_T.pdf",path  = "./wykresy")



d_r_ot_r_s<-read.csv("./dane/dane_odwr_t_od_2_do_98_sigma_od_0.05_do0.3_S_0_50_r_0.02_K_48_T_2_.csv",
                 header = T,sep = ",")
d_r_ot_r_S0<-read.csv("./dane/dane_odwr_t_od_2_do_98_sigma_0.3_S_0_od_30_do_79_r_0.02_K_48_T_2_.csv",
                     header = T,sep = ",")
d_r_ot_r_r<-read.csv("./dane/dane_odwr_t_od_2_do_98_sigma_0.3_S_0_50_r_od_-0.03_do_0.19000000000000003_K_48_T_2_.csv",
                     header = T,sep = ",")
d_r_ot_r_K<-read.csv("./dane/dane_odwr_t_od_2_do_98_sigma_0.3_S_0_50_r_0.02_K_od_30_do_79_T_2_.csv",
                     header = T,sep = ",")
d_r_ot_r_T<-read.csv("./dane/dane_odwr_t_od_2_do_98_sigma_0.3_S_0_50_r_0.02_K_48_T_od_1_do_99_.csv",
                     header = T,sep = ",")

d_r_s_r_S0<-read.csv("./dane/dane_odwr_t_12_sigma_od_0.1_do0.6500000000000001_S_0_od_30_do_79_r_0.02_K_48_T_2_.csv",
                header = T,sep = ",")
d_r_s_r_K<-read.csv("./dane/dane_odwr_t_12_sigma_od_0.1_do0.6500000000000001_S_0_50_r_0.02_K_od_30_do_79_T_2_.csv",
                header = T,sep = ",")
d_r_s_r_r<-read.csv("./dane/dane_odwr_t_12_sigma_od_0.1_do0.6500000000000001_S_0_50_r_od_-0.03_do_0.19000000000000003_K_48_T_2_.csv",
                header = T,sep = ",")
d_r_s_r_T<-read.csv("./dane/dane_odwr_t_12_sigma_od_0.1_do0.6500000000000001_S_0_50_r_0.02_K_48_T_od_1_do_99_.csv",
                header = T,sep = ",")

d_r_S0_r_K<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_od_30_do_79_r_0.02_K_od_30_do_79_T_2_.csv",
                 header = T,sep = ",")
d_r_S0_r_r<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_od_30_do_79_r_od_-0.03_do_0.19000000000000003_K_48_T_2_.csv",
                 header = T,sep = ",")
d_r_S0_r_T<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_od_30_do_79_r_0.02_K_48_T_od_1_do_99_.csv",
                 header = T,sep = ",")

d_r_r_r_K<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_50_r_od_-0.03_do_0.19000000000000003_K_od_30_do_79_T_2_.csv",
                header = T,sep = ",")
d_r_r_r_T<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_50_r_od_-0.03_do_0.19000000000000003_K_48_T_od_1_do_99_.csv",
                    header = T,sep = ",")

d_r_K_r_T<-read.csv("./dane/dane_odwr_t_12_sigma_0.3_S_0_50_r_0.02_K_od_30_do_79_T_od_1_do_99_.csv",
                header = T,sep = ",")




my_list<-list(d_r_ot_r_s, d_r_ot_r_S0, d_r_ot_r_r,d_r_ot_r_K,d_r_ot_r_T,
              d_r_s_r_S0,d_r_s_r_K,d_r_s_r_r,d_r_s_r_T,
              d_r_S0_r_K,d_r_S0_r_r,d_r_S0_r_T,
              d_r_r_r_K,d_r_r_r_T,d_r_K_r_T)


my_list_nazwy<-list("d_r_ot_r_s", "d_r_ot_r_S0", "d_r_ot_r_r","d_r_ot_r_K","d_r_ot_r_T",
              "d_r_s_r_S0","d_r_s_r_K","d_r_s_r_r","d_r_s_r_T",
              "d_r_S0_r_K","d_r_S0_r_r","d_r_S0_r_T",
              "d_r_r_r_K","d_r_r_r_T","d_r_K_r_T")

i<-7
for (i in seq_along(my_list)) {
  dane<-my_list[[i]]
  pierwsza_zmienna<-NULL
  druga_zmienna<-NULL
  if(grepl("r_ot", my_list_nazwy[[i]])){
    if(is.null(pierwsza_zmienna)){
      pierwsza_zmienna<-"odw_t"
    }else{
      druga_zmienna<-"odw_t"
    }
  }
  if(grepl("r_s", my_list_nazwy[[i]])){
    if(is.null(pierwsza_zmienna)){
      pierwsza_zmienna<-"sigma"
    }else{
      druga_zmienna<-"sigma"
    }
  }
  if(grepl("r_S0", my_list_nazwy[[i]])){
    if(is.null(pierwsza_zmienna)){
      pierwsza_zmienna<-"S_0"
    }else{
      druga_zmienna<-"S_0"
    }
  }
  if(grepl("r_r", my_list_nazwy[[i]])){
    if(is.null(pierwsza_zmienna)){
      pierwsza_zmienna<-"r"
    }else{
      druga_zmienna<-"r"
    }
  }
  if(grepl("r_K", my_list_nazwy[[i]])){
    if(is.null(pierwsza_zmienna)){
      pierwsza_zmienna<-"K"
    }else{
      druga_zmienna<-"K"
    }
  }
  if(grepl("r_T", my_list_nazwy[[i]])){
    if(is.null(pierwsza_zmienna)){
      pierwsza_zmienna<-"T"
    }else{
      druga_zmienna<-"T"
    }
  }
  czy_jest_r<-FALSE
  if(pierwsza_zmienna=="r"){
    czy_jest_r<-TRUE
  }
  if(druga_zmienna=="r"){
    czy_jest_r<-TRUE
  }
  subset(dane,dane$wersja=="put") %>%
    ggplot(aes(x=  .data[[pierwsza_zmienna]],y = .data[[druga_zmienna]],fill = cena_opcji))+ 
    geom_tile()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom",
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"))+
    scale_fill_viridis_c()+facet_wrap(~opcja)+
    labs(title="Cena opcji put z podziałem",
         subtitle = "na opcje amerykanskie i europejskie")
  substr(my_list_nazwy[[i]], 1, 1) <- "p"
  ggsave(paste0(my_list_nazwy[[i]],".pdf"),path  = "./wykresy")
  if(czy_jest_r){
    subset(dane,dane$wersja=="call") %>%
      ggplot(aes(x=  .data[[pierwsza_zmienna]],y = .data[[druga_zmienna]],fill = cena_opcji))+ 
      geom_tile()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"))+
      scale_fill_viridis_c()+facet_wrap(~opcja)+
      labs(title="Cena opcji call z podziałem",
           subtitle = "na opcje amerykanskie i europejskie")
    substr(my_list_nazwy[[i]], 1, 1) <- "c"
    ggsave(paste0(my_list_nazwy[[i]],".pdf"),path  = "./wykresy")
  }else{
    subset(dane,dane$wersja=="call"&dane$opcja=="a") %>%
      ggplot(aes(x=  .data[[pierwsza_zmienna]],y = .data[[druga_zmienna]],fill = cena_opcji))+ 
      geom_tile()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"))+
      scale_fill_viridis_c()+
      labs(title="Cena opcji call z podziałem",
           subtitle = "na opcje amerykanskie i europejskie")
    substr(my_list_nazwy[[i]], 1, 1) <- "c"
    ggsave(paste0(my_list_nazwy[[i]],".pdf"),path  = "./wykresy")
    
  }
  
  
}




i<-7
for (i in seq_along(my_list)) {
  dane<-my_list[[i]]
  w_sumie<-merge(subset(dane,dane$wersja == "call"&dane$opcja == "a"),
        subset(dane,dane$wersja == "call"&dane$opcja == "e"), 
        by.x = c("sigma","S_0","K","T","r","odw_t"), 
        by.y =c("sigma","S_0","K","T","r","odw_t"))
  print(paste(my_list_nazwy[[i]]," suma roznic to ",sum(w_sumie$cena_opcji.x-w_sumie$cena_opcji.y)))
  
}


