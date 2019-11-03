library(brazilmaps)
library(tidyverse)
library(lubridate)
library(measurements)
library(stringr)
library(tidyr)

load("../oleogate.RData")
uf_code <- data.frame(ufsig = as.character(sort(unique(df_defeso$uf))), 
                      ufcode = as.character(
                        c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)),
                      stringsAsFactors = FALSE)
nordeste <- c("BA","SE","AL","PE","PB","RN","CE","PI","MA")
df_defeso <- df_defeso %>% 
  left_join(uf_code, by = c("uf" = "ufsig"))

png("../figures/defeso_nordeste_tempo.png",width=3200,height=1800,res=300)
df_defeso %>%
  group_by(mes_ano, ufcode, uf)%>%
  summarise(mean_valor_parcela = mean(valor_parcela),
            sum_valor_parcela = sum(valor_parcela),
            sd_valor_parcela = sd(valor_parcela),
            num_beneficios = n()) %>%
  ungroup()%>%
  filter(!is.na(uf)) %>%
  filter(uf %in% nordeste) %>%
  mutate(mes_ano = as.character(mes_ano),
         mes = substr(mes_ano, 5, 6),
         ano = substr(mes_ano, 1, 4),
         mes_ano = ymd(paste(ano, mes, "01",sep="-"))) %>%
  ggplot(aes(x=mes_ano, y= num_beneficios)) + geom_line() +
  facet_wrap(~uf, ncol = 3, scale = "free_y") + 
  xlab("Tempo (meses)")+
  ylab("valor médio da parcela")+
  ggtitle("Defeso - Nordeste")
dev.off()


df_defeso_nordeste <- df_defeso %>%
  group_by(ufcode, uf)%>%
  summarise(mean_valor_parcela = mean(valor_parcela),
            sum_valor_parcela = sum(valor_parcela),
            sd_valor_parcela = sd(valor_parcela),
            num_beneficios = n()) %>%
  ungroup()%>%
  filter(!is.na(uf)) %>%
  filter(uf %in% nordeste) %>%
  mutate(ufcode = as.numeric(ufcode))

png("../figures/mapa_defeso_anual.png",width=3200,height=1800,res=300)
get_brmap("State", geo.filter = list(Region = 2)) %>% 
  inner_join(df_defeso_nordeste %>% mutate(defeso_anual = signif(sum_valor_parcela/1000000,2)), 
             by =c("State"="ufcode")) %>%
  ggplot()+
  geom_sf(aes(fill = defeso_anual)) + 
  scale_fill_viridis_c(option = 2, begin = 0.2, end = 0.9)+
  geom_sf_label(aes(label = signif(sum_valor_parcela/1000000,2)))+
  theme_bw()+
  ggtitle(paste("Defeso Nordeste - 2018/2019 (milhões de Reais)"))
dev.off()


load("../economia_oleo_mun.RData")
png("../figures/mapa_cidades_atingidas.png",width=3200,height=1800,res=300)
get_brmap("City", geo.filter = list(Region = 2)) %>% 
  inner_join(df_dca_mun_sel_func, 
             c("City" = "cod_ibge")) %>% 
  ggplot() + geom_sf(aes(fill = populacao), 
                     # ajusta tamanho das linhas
                     colour = "transparent", size = 0.1) +
  geom_sf(data = get_brmap("State", geo.filter = list(Region = 2)),
          fill = "transparent",
          colour = "black", size = 0.5) +
  # muda escala de cores
  scale_fill_viridis_c(option = 1, begin = 0.1, end = 0.1) +
  # tira sistema cartesiano
  ggtitle("Cidades atingidas pela mancha de óleo")+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
dev.off()



load("../economia_NE.RData")
load("../economia_oleo_mun.RData")

names(PIB_Mun_sel)[40:45]<-c("PIB",  "População", "PIB_per_capita", "PIB_1","PIB_2","PIB_3")
names(PIB_Mun_NE)[43:45]<-c("PIB_1","PIB_2","PIB_3")

PIB_Mun_sel %>%
  filter(Ano==2016) %>%
  select(codigo_municipio, 43:45) %>%
  mutate(grupo =case_when(
    str_count(PIB_1,"pesca")>0 ~ "Pesca principal PIB",
    str_count(PIB_2,"pesca")>0 ~ "Pesca segundo PIB",
    str_count(PIB_3,"pesca")>0 ~ "PIB terceiro PIB",
    TRUE                       ~ "Pesca não pertence ao PIB 3"
  )) %>%
  ggplot(aes(x=grupo)) +
  geom_bar()+
  coord_flip()

PIB_Mun_sel$grupo_cidades <- rep("Cidades afetaads", nrow(PIB_Mun_sel))
PIB_Mun_NE$grupo_cidades <- rep("Demais Cidades NE", nrow(PIB_Mun_NE))
names(PIB_Mun_NE) <- names(PIB_Mun_sel)
all_PIB_Mun <- rbind(PIB_Mun_sel, PIB_Mun_NE)

get_brmap("City", geo.filter = list(Region = 2)) %>% 
  inner_join(all_PIB_Mun, 
             c("City" = "codigo_municipio",
               "State" = "Código da Unidade da Federação")) %>% 
  ggplot() + geom_sf(aes(fill = grupo_cidades), 
                     # ajusta tamanho das linhas
                     colour = "transparent", size = 0.1) +
  geom_sf(data = get_brmap("State", geo.filter = list(Region = 2)),
          fill = "transparent",
          colour = "black", size = 0.5) +
  # muda escala de cores
  # tira sistema cartesiano
  ggtitle("Cidades do Nordeste atingidas pela mancha de óleo")+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


#### PIB per capita


get_brmap("City", geo.filter = list(Region = 2)) %>% 
  inner_join(all_PIB_Mun, 
             c("City" = "codigo_municipio",
               "State" = "Código da Unidade da Federação")) %>% 
  ggplot() + geom_sf(aes(fill = PIB_per_capita), 
                     # ajusta tamanho das linhas
                     colour = "transparent", size = 0.1) +
  geom_sf(data = get_brmap("State", geo.filter = list(Region = 2)),
          fill = "transparent",
          colour = "black", size = 0.5) +
  # muda escala de cores
  # tira sistema cartesiano
  scale_fill_viridis_c(option = 1, begin = 0.3, end = 0.9) +
  ggtitle("PIB per capita de Cidades do Nordeste")+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "vertical") +
  facet_wrap(~grupo_cidades)


#### receita liquida

get_brmap("City", geo.filter = list(Region = 2)) %>% 
  inner_join(df_dca_mun_sel_RO_tidy %>%
               mutate(grupo="Cidades afetadas") %>%
               bind_rows(
                 df_dca_NE_RO_tidy %>%
                   anti_join(df_dca_mun_sel_RO_tidy) %>%
                   mutate(grupo="Demais Cidades NE") 
               ), 
             c("City" = "cod_ibge")) %>% 
  ggplot() + geom_sf(aes(fill = log10(receita_liquida_total)), 
                     # ajusta tamanho das linhas
                     colour = "transparent", size = 0.1) +
  geom_sf(data = get_brmap("State", geo.filter = list(Region = 2)),
          fill = "transparent",
          colour = "black", size = 0.5) +
  # muda escala de cores
  # tira sistema cartesiano
  scale_fill_viridis_c(option = 1, begin = 0.1, end = 0.9) +
  ggtitle("Receita líquida das Cidades do Nordeste")+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "vertical") 


#@@@@ Indice de dependencia


get_brmap("City", geo.filter = list(Region = 2)) %>% 
  inner_join(df_dca_mun_sel_RO_tidy %>%
               mutate(grupo="Cidades afetadas") %>%
               bind_rows(
                 df_dca_NE_RO_tidy %>%
                   anti_join(df_dca_mun_sel_RO_tidy) %>%
                   mutate(grupo="Demais Cidades NE") 
               ), 
             c("City" = "cod_ibge")) %>% 
  ggplot() + geom_sf(aes(fill = indice_dependencia), 
                     # ajusta tamanho das linhas
                     colour = "transparent", size = 0.1) +
  geom_sf(data = get_brmap("State", geo.filter = list(Region = 2)),
          fill = "transparent",
          colour = "black", size = 0.5) +
  geom_polygon(data = aes(x = long, y = lat, group = group, fill ="transparent"), colour = "black") +
  # muda escala de cores
  # tira sistema cartesiano
  scale_fill_viridis_c(option = 3) +
  ggtitle("Índice de dependência")+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "vertical") 


#####


df_dca_mun_sel_RO_tidy %>%
  mutate(grupo="Cidades afetadas") %>%
  bind_rows(
    df_dca_NE_RO_tidy %>%
      anti_join(df_dca_mun_sel_RO_tidy) %>%
      mutate(grupo="Demais Cidades NE") 
  ) %>%
  filter(grupo == "Cidades afetadas") %>%
  ggplot(aes(x = receita_liquida_total/1000, fill = "red")) + 
  theme_bw()+
  guides(fill=FALSE)+
  labs(title = "Cidades afetadas - Histograma de receita líquida total",
       x = "Receita líquida (Milhares de Reais)",
       y = "Número de cidades",
       fill = "")
