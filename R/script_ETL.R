library(tabulizer)
library(dplyr)

setwd("~/GitHub/oleogate/data")
tabulizer::extract_tables("ibama.pdf", output = "csv",outdir = getwd())

library(readr)
ibama <- read_csv("ibama-2.csv", locale = locale(encoding = "LATIN1"))

devtools::install_github("laresbernardo/lares")

library(lares)
library(stringr)
library(rlang)
library(readr)

names(ibama)[1]<- "Localidade"
names(ibama)[2]<-"Municipio"
names(ibama)[3]<-"data_avistamento"
names(ibama)[4]<-"uf"

ibama$Municipio<- str_to_upper(cleanText(ibama$Municipio))
ibama$Localidade<- str_to_upper(cleanText(ibama$Localidade))




#Percorre a lista de arquivos csv

files<-dir(path = "data",pattern = ".csv")[1:12]

library(purrr)

df_defeso<- map_df(files, function(arquivo){
  df<- read_delim(paste0("data/",arquivo), 
                  ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                              grouping_mark = ".", encoding = "LATIN1"), 
                  trim_ws = TRUE)
  
})


names(df_defeso)<-c("mes_ano", "uf", "cod_SIAFI", "Municipio", "cpf_favorecido", "nis_favorecido", "rgp_favorecido", "nome_favorecido", "valor_parcela")

library(ggplot2)

df_defeso %>% 
  semi_join(ibama) %>%
  group_by(uf) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  ggplot(aes(x= uf, y= soma_valor)) +
  geom_col() +
  coord_flip()
  



df_defeso %>%
  filter(
    uf %in% ((ibama%>%
           distinct(uf))$uf)
  ) %>%
  anti_join(ibama) %>%
  group_by(uf) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  ggplot(aes(x= uf, y= soma_valor)) +
  geom_col() +
  coord_flip()



df_defeso %>% 
  semi_join(ibama) %>%
  group_by(Municipio) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  ggplot(aes(x= soma_valor)) +
  geom_histogram()


df_defeso %>% 
  semi_join(ibama) %>%
  group_by(Municipio) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  mutate(dummy_12= "12M") %>%
  ggplot(aes(x= dummy_12, y= soma_valor)) +
  geom_violin()

df_defeso %>% 
  semi_join(ibama) %>%
  group_by(Municipio) %>%
  summarise(
    soma_valor = sum(valor_parcela)
  ) %>% 
  mutate(dummy_12= "12M") %>%
  ggplot(aes(x= dummy_12, y= soma_valor)) +
  geom_boxplot()

