library(ggplot2)
library(dplyr)

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



df_defeso %>% 
  semi_join(ibama) %>%
  group_by(Municipio) %>%
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
      group_by(Municipio) %>%
      summarise(
        soma_valor = sum(valor_parcela)
      ) %>%  
      mutate(grupo= "Demais Cidades NE") 
  )%>%
ggplot(aes(x= grupo, y= soma_valor)) +
  geom_boxplot(aes(fill=grupo)) +
  scale_y_log10()




glimpse(df_dca_mun_sel_func)



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
  geom_boxplot(aes(fill=grupo))+
  scale_y_log10()


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
  geom_boxplot(aes(fill=grupo))+
  scale_y_log10()


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
  geom_boxplot(aes(fill=grupo)) +
  scale_y_log10()


library(tidyr)
library(stringr)

1.7.0.0.00.0.0
2.4.0.0.00.0.0

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


df_dca_mun_sel_RO_tidy %>%
  mutate(grupo="Cidades afetadas") %>%
bind_rows(
  df_dca_NE_RO_tidy %>%
    anti_join(df_dca_mun_sel_RO_tidy) %>%
    mutate(grupo="Demais Cidades NE") 
) %>%
  ggplot()+
  geom_boxplot(aes(x=grupo, y=indice_dependencia, fill= grupo))


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

PIB_Mun_sel %>%
  filter(Ano==2016) %>%
  select(codigo_municipio, 43:45) %>%
  mutate(grupo =case_when(
    str_count(PIB_1,"pesca")>0 ~ "Pesca PIB 3",
    str_count(PIB_2,"pesca")>0 ~ "Pesca PIB 3",
    str_count(PIB_3,"pesca")>0 ~ "Pesca PIB 3",
    TRUE                       ~ "Pesca não pertence ao PIB 3"
  )) %>%
  ggplot(aes(x=grupo)) +
  geom_bar()+
  coord_flip()


PIB_Mun_NE %>%
  filter(Ano==2016) %>%
  select(codigo_municipio, 43:45) %>%
  mutate(grupo =case_when(
    str_count(PIB_1,"pesca")>0 ~ "Pesca principal PIB",
    str_count(PIB_2,"pesca")>0 ~ "Pesca segundo PIB",
    str_count(PIB_3,"pesca")>0 ~ "PIB terceiro PIB",
    TRUE                       ~ "Pesca não pertence ao PIB 3"
  )) %>%
  ggplot(aes(x=grupo)) +
  geom_point(aes(x=população, y=PIB, color=grupo))

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
  geom_point(aes(x=População, y=PIB, color=grupo))+
  scale_x_log10()+
  scale_y_log10()

  
  
    geom_bar()+
  coord_flip()



PIB_Mun_NE %>%
  filter(Ano==2016) %>%
  select(codigo_municipio, 40:45) %>%
  mutate(grupo =case_when(
    str_count(PIB_1,"pesca")>0 ~ "Pesca principal PIB",
    str_count(PIB_2,"pesca")>0 ~ "Pesca segundo PIB",
    str_count(PIB_3,"pesca")>0 ~ "PIB terceiro PIB",
    TRUE                       ~ "Pesca não pertence ao PIB 3"
  )) %>%
  ggplot(aes(x=grupo)) +
  geom_bar()+
  coord_flip()





    
         pesca_prim=if_else(str_count(PIB_1,"pesca")>0,1,0),
         pesca_sec=if_else(str_count(PIB_2,"pesca")>0,1,0),
         pesca_terc=if_else(str_count(PIB_3,"pesca")>0,1,0)) %>%
  ggplot()

  