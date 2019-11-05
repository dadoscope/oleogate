library(ggplot2)

library(dplyr)

library(stringr)
library(lares)

library(abjutils)




###Gráficos diversos para exploração de dados

df_defeso %>% 
  semi_join(ibama) %>%
  group_by(uf) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  ggplot(aes(x= reorder(uf, soma_valor), y= soma_valor, fill= uf)) +
  geom_col() +
  coord_flip()




df_defeso %>%
  filter(
    uf %in% ((ibama%>%
                distinct(uf))$uf)
  ) %>%
  #anti_join(ibama) %>%
  group_by(uf) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  ggplot(aes(x= reorder(uf, soma_valor), y= soma_valor, fill= uf)) +
  geom_col() +
  coord_flip()



df_defeso %>% 
  semi_join(ibama) %>%
  group_by(uf, Municipio) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  ggplot(aes(x= soma_valor)) +
  geom_histogram() +
  facet_grid(uf~.)


df_defeso %>% 
  semi_join(ibama) %>%
  group_by(uf,Municipio) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  mutate(dummy_12= "12M") %>%
  ggplot(aes(x= uf, y= soma_valor)) +
  geom_violin(aes(fill=uf)) 

df_defeso %>% 
  semi_join(ibama) %>%
  group_by(uf,Municipio) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  mutate(dummy_12= "12M") %>%
  ggplot(aes(x= uf, y= soma_valor)) +
  geom_boxplot(aes(fill=uf)) 



####GRárico de comparação do seguro defeso

df_defeso %>% 
  semi_join(ibama) %>%
  group_by(uf, Municipio) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  mutate(grupo= "Cidades afetadas") %>%
  bind_rows(
    df_defeso %>% 
      filter(
        uf %in% ((ibama%>%
                    distinct(uf))$uf)
      ) %>%
      anti_join(ibama) %>%
      group_by(uf, Municipio) %>%
      summarise(
        soma_valor = sum(valor_parcela)
      ) %>%  
      mutate(grupo= "Demais Cidades NE") 
  )%>%
  ggplot(aes(x= grupo, y= soma_valor)) +
  geom_violin(aes(fill=grupo)) +
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE),minor_breaks= NULL, breaks= c(10^3,10^4,10^5,1.3*10^6,10^7)) +
  theme_light()+
  theme(panel.border = element_blank())+
  labs(x="",
       y="Valor total em R$")


####GRárico de comparação de despesa com meio-ambiente


df_dca_mun_sel_func%>%
  filter(cod_interno=="18",
         coluna=="Despesas Liquidadas") %>%
  mutate(grupo="Cidades afetadas",
         desp_per_capita= valor/populacao)%>%
  bind_rows(
    df_dca_NE_func%>%
      filter(cod_interno=="18",
             coluna=="Despesas Liquidadas") %>%
      anti_join(df_dca_mun_sel_func) %>%
      mutate(grupo= "Demais Cidades NE",
             desp_per_capita = valor/populacao)
  ) %>%
  ggplot(aes(x= grupo, y= desp_per_capita)) +
  geom_violin(aes(fill=grupo)) +
  scale_y_log10(labels=function(x) format(x, decimal.mark=",", big.mark = ".", scientific = FALSE),minor_breaks= NULL, breaks=c(0.1,8,12,70,270,1000)) +
  theme_light()+
  theme(panel.border = element_blank())+
  labs(x="",
       y="Valor por habitante em R$")



### Gráfico de despesa com saúde


df_dca_mun_sel_func%>%
  filter(cod_interno=="10",
         coluna=="Despesas Liquidadas") %>%
  mutate(grupo="Cidades afetadas",
         desp_per_capita= valor/populacao)%>%
  bind_rows(
    df_dca_NE_func%>%
      filter(cod_interno=="10",
             coluna=="Despesas Liquidadas") %>%
      anti_join(df_dca_mun_sel_func) %>%
      mutate(grupo= "Demais Cidades NE",
             desp_per_capita = valor/populacao)
  ) %>%
  ggplot(aes(x= grupo, y= desp_per_capita)) +
  geom_violin(aes(fill=grupo))+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks= c(100,300,560,1000,3000)) +
  theme_light()+
  theme(panel.border = element_blank())+
  labs(x="",
       y="Valor por habitante em R$")


##### GRáfico de despesa com turismo

df_dca_mun_sel_func%>%
  filter(cod_interno=="23.695",
         coluna=="Despesas Liquidadas") %>%
  mutate(grupo="Cidades afetadas",
         desp_per_capita= valor/populacao)%>%
  bind_rows(
    df_dca_NE_func%>%
      filter(cod_interno=="23.695",
             coluna=="Despesas Liquidadas") %>%
      anti_join(df_dca_mun_sel_func) %>%
      mutate(grupo= "Demais Cidades NE",
             desp_per_capita = valor/populacao)
  ) %>%
  ggplot(aes(x= grupo, y= desp_per_capita)) +
  geom_violin(aes(fill=grupo))+
  scale_y_log10(labels=function(x) format(x,decimal.mark=",", big.mark = ".", scientific = FALSE), breaks= c(0.01,1,10,15,100), minor_breaks= NULL ) +
  theme_light()+
  theme(panel.border = element_blank())+
  labs(x="",
       y="Valor por habitante em R$")



##Gráfico de dependência de transferência intergovernamental
library(tidyr)


df_dca_mun_sel_RO_tidy<-
  df_dca_mun_sel_RO %>% 
  filter(cod_interno== "TotalReceitas")%>%
  select(cod_ibge, coluna, cod_interno, valor) %>%
  spread(coluna, valor) %>%
  rename_at(3:6,str_remove_all," ") %>%
  rename_at(3:6,str_remove_all,"-") %>%
  mutate(
    tipo_receita = "receita_liquida_total",
    receita_liquida = ReceitasBrutasRealizadas- 
      ifelse(is.na(DeduçõesFUNDEB),0,DeduçõesFUNDEB) -
      ifelse(is.na(DeduçõesTransferênciasConstitucionais),0,DeduçõesTransferênciasConstitucionais)-
      ifelse(is.na(OutrasDeduçõesdaReceita),0,OutrasDeduçõesdaReceita)) %>%
  select(1,7,8) %>%
  bind_rows(
    df_dca_mun_sel_RO %>% 
      filter(cod_interno== "1.7.0.0.00.0.0")%>%
      select(cod_ibge, coluna, cod_interno, valor) %>%
      spread(coluna, valor)%>%
      rename_at(3:6,str_remove_all," ") %>%
      rename_at(3:6,str_remove_all,"-") %>%
      mutate(
        tipo_receita = "transf_corrente_liq",
        receita_liquida = ReceitasBrutasRealizadas- 
          ifelse(is.na(DeduçõesFUNDEB),0,DeduçõesFUNDEB) -
          ifelse(is.na(DeduçõesTransferênciasConstitucionais),0,DeduçõesTransferênciasConstitucionais)-
          ifelse(is.na(OutrasDeduçõesdaReceita),0,OutrasDeduçõesdaReceita)) %>%
      select(1,7,8) 
  )%>%
  bind_rows(
    df_dca_mun_sel_RO %>% 
      filter(cod_interno== "2.4.0.0.00.0.0")%>%
      select(cod_ibge, coluna, cod_interno, valor) %>%
      spread(coluna, valor)%>%
      rename_at(3:4,str_remove_all," ") %>%
      rename_at(3:4,str_remove_all,"-") %>%
      mutate(
        tipo_receita = "transf_capital_liq",
        receita_liquida = ReceitasBrutasRealizadas- 
          ifelse(is.na(OutrasDeduçõesdaReceita),0,OutrasDeduçõesdaReceita)) %>%
      select(1,5,6)
  ) %>%
  spread(tipo_receita,receita_liquida) %>%
  mutate(transf_capital_liq= ifelse(is.na(transf_capital_liq),0,transf_capital_liq),
         indice_dependencia = (transf_capital_liq+transf_corrente_liq)/receita_liquida_total)


df_dca_NE_RO_tidy<-
  df_dca_NE_RO %>% 
  filter(cod_interno== "TotalReceitas")%>%
  select(cod_ibge, coluna, cod_interno, valor) %>%
  spread(coluna, valor) %>%
  rename_at(3:6,str_remove_all," ") %>%
  rename_at(3:6,str_remove_all,"-") %>%
  mutate(
    tipo_receita = "receita_liquida_total",
    receita_liquida = ReceitasBrutasRealizadas- 
      ifelse(is.na(DeduçõesFUNDEB),0,DeduçõesFUNDEB) -
      ifelse(is.na(DeduçõesTransferênciasConstitucionais),0,DeduçõesTransferênciasConstitucionais)-
      ifelse(is.na(OutrasDeduçõesdaReceita),0,OutrasDeduçõesdaReceita)) %>%
  select(1,7,8) %>%
  bind_rows(
    df_dca_NE_RO %>% 
      filter(cod_interno== "1.7.0.0.00.0.0")%>%
      select(cod_ibge, coluna, cod_interno, valor) %>%
      spread(coluna, valor)%>%
      rename_at(3:6,str_remove_all," ") %>%
      rename_at(3:6,str_remove_all,"-") %>%
      mutate(
        tipo_receita = "transf_corrente_liq",
        receita_liquida = ReceitasBrutasRealizadas- 
          ifelse(is.na(DeduçõesFUNDEB),0,DeduçõesFUNDEB) -
          ifelse(is.na(DeduçõesTransferênciasConstitucionais),0,DeduçõesTransferênciasConstitucionais)-
          ifelse(is.na(OutrasDeduçõesdaReceita),0,OutrasDeduçõesdaReceita)) %>%
      select(1,7,8) 
  )%>%
  bind_rows(
    df_dca_NE_RO %>% 
      filter(cod_interno== "2.4.0.0.00.0.0")%>%
      select(cod_ibge, coluna, cod_interno, valor) %>%
      spread(coluna, valor)%>%
      rename_at(3:6,str_remove_all," ") %>%
      rename_at(3:6,str_remove_all,"-") %>%
      mutate(
        tipo_receita = "transf_capital_liq",
        receita_liquida = ReceitasBrutasRealizadas- 
          ifelse(is.na(DeduçõesFUNDEB),0,DeduçõesFUNDEB) -
          ifelse(is.na(DeduçõesTransferênciasConstitucionais),0,DeduçõesTransferênciasConstitucionais)-
          ifelse(is.na(OutrasDeduçõesdaReceita),0,OutrasDeduçõesdaReceita)) %>%
      select(1,7,8)
  ) %>%
  spread(tipo_receita,receita_liquida) %>%
  mutate(transf_capital_liq= ifelse(is.na(transf_capital_liq),0,transf_capital_liq),
         indice_dependencia = (transf_capital_liq+transf_corrente_liq)/receita_liquida_total)


library(hrbrthemes)
df_dca_mun_sel_RO_tidy %>%
  mutate(grupo="Cidades afetadas") %>%
  bind_rows(
    df_dca_NE_RO_tidy %>%
      anti_join(df_dca_mun_sel_RO_tidy) %>%
      mutate(grupo="Demais Cidades NE") 
  ) %>%
  ggplot()+
  geom_violin(aes(x=grupo, y=indice_dependencia, fill= grupo))+
  scale_y_percent(breaks=c(0.25,0.50,0.75,0.83,0.9,1), minor_breaks = NULL)+
  theme_light()+
  theme(panel.border = element_blank())+
  labs(x="",
       y="Percentual de dependência")



##Mudança de nome de colunas
names(PIB_Mun_sel)[c(8,40:45)]<-c("Municipio","PIB",  "População", "PIB_per_capita", "PIB_1","PIB_2","PIB_3")
names(PIB_Mun_NE)[c(8,40:45)]<-c("Municipio","PIB",  "População", "PIB_per_capita", "PIB_1","PIB_2","PIB_3")

##Tratamento dos dados de seguro defeso
PIB_Mun_sel$Municipio <- str_to_upper(abjutils::rm_accent (PIB_Mun_sel$Municipio))


PIB_Mun_NE$Municipio <- str_to_upper(abjutils::rm_accent(PIB_Mun_NE$Municipio))

#GRáficos de barra com comparação entre cidades com pesca que pertence ao PIB3
df_defeso_NE<-
  df_defeso %>%
  inner_join(
    PIB_Mun_NE%>%
      select(Municipio, codigo_municipio ))


PIB_Mun_sel %>%
  filter(Ano==2016) %>%
  select(codigo_municipio, 43:45) %>%
  mutate(grupo =case_when(
    str_count(PIB_1,"pesca")>0 ~ "Pesca PIB 3",
    str_count(PIB_2,"pesca")>0 ~ "Pesca PIB 3",
    str_count(PIB_3,"pesca")>0 ~ "Pesca PIB 3",
    TRUE                       ~ "Pesca não pertence ao PIB 3"
  )) %>%
  ggplot(aes(x=grupo, fill =grupo)) +
  geom_bar()+
  coord_flip()+
  scale_y_comma()+
  theme_light()+
  labs(x="",
       y="Número de cidades")


#GRáficos de barra com comparação entre cidades com pesca que pertence ao PIB3 com os dois grupos de cidade
PIB_Mun_sel %>%
  filter(Ano==2016) %>%
  select(codigo_municipio, 43:45) %>%
  mutate( grupo_cidade = "Cidades Atingidas",
          grupo_PIB =case_when(
            str_count(PIB_1,"pesca")>0 ~ "Pesca PIB 3",
            str_count(PIB_2,"pesca")>0 ~ "Pesca PIB 3",
            str_count(PIB_3,"pesca")>0 ~ "Pesca PIB 3",
            TRUE                       ~ "Pesca não pertence ao PIB 3"
          )) %>%
  bind_rows(
    PIB_Mun_NE %>%
      anti_join(PIB_Mun_sel) %>%
      filter(Ano==2016) %>%
      select(codigo_municipio, 43:45) %>%
      mutate(grupo_cidade = "Demais Cidades NE",
             grupo =case_when(
               str_count(PIB_1,"pesca")>0 ~ "Pesca PIB 3",
               str_count(PIB_2,"pesca")>0 ~ "Pesca PIB 3",
               str_count(PIB_3,"pesca")>0 ~ "Pesca PIB 3",
               TRUE                       ~ "Pesca não pertence ao PIB 3"
             ))
  )  %>% 
  ggplot(aes(x=grupo, fill= grupo)) +
  geom_bar()+
  coord_flip()+
  scale_y_comma()+
  theme_light()+
  labs(x="",
       y="Número de cidades")+
  facet_grid(grupo_cidade~.)



#Gráfico de dispersão de população x renda per capita comparando as cidades com pensa no PIB 3 com as demais
PIB_Mun_sel %>%
  filter(Ano==2016) %>%
  select(codigo_municipio, 40:45) %>%
  mutate(grupo =case_when(
    str_count(PIB_1,"pesca")>0 ~ "Pesca PIB 3",
    str_count(PIB_2,"pesca")>0 ~ "Pesca PIB 3",
    str_count(PIB_3,"pesca")>0 ~ "Pesca PIB 3",
    TRUE                       ~ "Pesca não pertence ao PIB 3"
  )) %>%
  ggplot() +
  geom_point(aes(x=População, y=PIB_per_capita, color=grupo))+
  scale_x_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  theme_light()+
  labs(x="População",
       y="PIB per capita em R$")



###GRáfico de dispersão de população e renda per capita com os dois grupos de cidade

PIB_Mun_sel %>%
  filter(Ano==2016) %>%
  select(codigo_municipio, 40:45) %>%
  mutate( grupo = "Cidades atingidas") %>%
  
  bind_rows(
    PIB_Mun_NE %>%
      anti_join(PIB_Mun_sel) %>%
      filter(Ano==2016) %>%
      select(codigo_municipio, 40:45) %>%
      mutate( grupo = "Demais Cidades NE") 
  )  %>%
  ggplot() +
  geom_point(aes(x=População, y=PIB_per_capita, color=grupo), alpha= 0.5)+
  scale_x_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  theme_light()+
  theme(panel.border = element_blank())
  labs(x="População",
       y="PIB per capita em R$")
