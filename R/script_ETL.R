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



df_defeso %>%
  semi_join(ibama) %>%
  distinct(Municipio)

ibama %>%
  distinct(Municipio)


ibama%>%
  anti_join(df_defeso) %>%
  distinct(uf, Municipio)


ibama[ibama$Municipio== "PORTO DE PEDRA",2]<- "PORTO DE PEDRAS"
ibama[ibama$Municipio== "CEARAMIRIM",2]<- "CEARA-MIRIM"

ibama[ibama$Municipio== "SAO CRISTOVAO",4]<- "SE"
ibama[ibama$Municipio== "SIRINHAEM",4]<- "PE"
ibama[ibama$Municipio== "ILHA GRANDE",4]<- "PI"

save(list = c("df_defeso","ibama"),file = "oleogate.RData")


