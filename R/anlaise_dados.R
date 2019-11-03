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
ggplot(aes(x= grupo, y= log(soma_valor))) +
  geom_boxplot(aes(fill=grupo))




glimpse(df_dca_mun_sel_func)



df_dca_mun_sel_func%>%
  filter(cod_interno=="18",
         coluna=="Despesas Liquidadas") %>%
  mutate(grupo="Cidades afetadas",
         desp_per_capta= valor/populacao)%>%
  bind_rows(
    df_dca_NE_func%>%
      filter(cod_interno=="18",
             coluna=="Despesas Liquidadas") %>%
      anti_join(df_dca_mun_sel_func) %>%
      mutate(grupo= "Demais Cidades NE",
             desp_per_capta = valor/populacao)
  ) %>%
  ggplot(aes(x= grupo, y= log(desp_per_capta))) +
  geom_boxplot(aes(fill=grupo))


df_dca_mun_sel_func%>%
  filter(cod_interno=="10",
         coluna=="Despesas Liquidadas") %>%
  mutate(grupo="Cidades afetadas",
         desp_per_capta= valor/populacao)%>%
  bind_rows(
    df_dca_NE_func%>%
      filter(cod_interno=="10",
             coluna=="Despesas Liquidadas") %>%
      anti_join(df_dca_mun_sel_func) %>%
      mutate(grupo= "Demais Cidades NE",
             desp_per_capta = valor/populacao)
  ) %>%
  ggplot(aes(x= grupo, y= log(desp_per_capta))) +
  geom_boxplot(aes(fill=grupo))


df_dca_mun_sel_func%>%
  filter(cod_interno=="23.695",
         coluna=="Despesas Liquidadas") %>%
  mutate(grupo="Cidades afetadas",
         desp_per_capta= valor/populacao)%>%
  bind_rows(
    df_dca_NE_func%>%
      filter(cod_interno=="23.695",
             coluna=="Despesas Liquidadas") %>%
      anti_join(df_dca_mun_sel_func) %>%
      mutate(grupo= "Demais Cidades NE",
             desp_per_capta = valor/populacao)
  ) %>%
  ggplot(aes(x= grupo, y= log(desp_per_capta))) +
  geom_boxplot(aes(fill=grupo))


library(tidyr)
library(stringr)

df_dca_mun_sel_RO_tidy<-
df_dca_mun_sel_RO %>% 
  filter(cod_interno== "TotalReceitas")%>%
  select(cod_ibge, coluna, cod_interno, valor) %>%
  spread(coluna, valor) %>%
  rename_at(3:6,str_remove_all," ") %>%
  rename_at(3:6,str_remove_all,"-") %>%
  mutate(receita_liquida = ReceitasBrutasRealizadas- 
                           ifelse(is.na(DeduçõesFUNDEB),0,DeduçõesFUNDEB) -
                           ifelse(is.na(DeduçõesTransferênciasConstitucionais),0,DeduçõesTransferênciasConstitucionais)-
                           ifelse(is.na(OutrasDeduçõesdaReceita),0,OutrasDeduçõesdaReceita))



names(PIB_Mun_sel)[43:45]<-c("PIB_1","PIB_2","PIB_3")
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
  geom_bar()+
  coord_flip()


    
         pesca_prim=if_else(str_count(PIB_1,"pesca")>0,1,0),
         pesca_sec=if_else(str_count(PIB_2,"pesca")>0,1,0),
         pesca_terc=if_else(str_count(PIB_3,"pesca")>0,1,0)) %>%
  ggplot()

  