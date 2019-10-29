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
