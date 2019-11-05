load("oleogate.RData")

unique(ibama$Municipio)

devtools::install_github("tchiluanda/rsiconfi")

library(rsiconfi)

library(stringr)
library(abjutils)
library(dplyr)
library(readr)
library()

names(ibama)[1]<- "Localidade"
names(ibama)[2]<-"nome_municipio"
names(ibama)[3]<-"data_avistamento"
names(ibama)[4]<-"uf"


municipios_IBGE <- read_csv("Data/Municpios_IBGE - Sheet1.csv", 
                            col_types = cols(`COD. UF` = col_character()))

names(municipios_IBGE)<- c("uf","cod_uf","cod_mun","nome_municipio","pop_estimada")

municipios_IBGE <- municipios_IBGE%>%
  mutate(cod_mun =  paste0(cod_uf, cod_mun))



#Converte tudo em maíusculo e faz limpeza de texto para facilitar joins com outras tabelas
municipios_IBGE$nome_municipio<- str_to_upper(abjutils::rm_accent(municipios_IBGE$nome_municipio))




mun_sel<- (ibama%>%
  distinct(nome_municipio, uf)%>%
  inner_join(municipios_IBGE) %>%
  select(cod_mun))$cod_mun


write.csv2(ibama, file= "ibama.csv")


df_dca_NE_RO<- get_dca_mun_state( #busca informações de anexos de contas anuais para todos os municípios de um estado indicado
  year = c(2018), #ano 
  annex = "I-C", #anexo de receita orçamentária
  arg_cod_conta = c("TotalReceitas", "1.7.0.0.00.0.0", "2.4.0.0.00.0.0"), #Total de receita e receitas de transferências
  state = c(21:29), #estados do NE
  In_QDCC=FALSE)



df_dca_NE_func<- get_dca_mun_state( #busca informações de anexos de contas anuais para todos os municípios de um estado indicado
  year = c(2018), #ano 
  annex = "I-E", #anexo de despesa por função
  arg_cod_conta = c("10", "18", "23.695"),
  state = c(21:29), #estados do NE
  In_QDCC=FALSE)


df_dca_mun_sel_RO<- get_dca(year = c(2018), #ano de referência
                         annex = "I-C",  
                         entity = mun_sel, #código ibge de cidades selecionadas
                         arg_cod_conta = c("TotalReceitas", "1.7.0.0.00.0.0", "2.4.0.0.00.0.0"), #Total de receita e receitas de transferências
                         In_QDCC=FALSE)

df_conta_dca<- get_account_dca(c(2018), "I-E", c("2402600") )

df_dca_mun_sel_func<- get_dca(year = c(2018), #ano de referência
                         annex = "I-E",  
                         entity = mun_sel, #código ibge de uma cidade de MG
                         arg_cod_conta = c("10", "18", "23.695"), 
                         In_QDCC=FALSE)


library(readxl)
PIB_dos_Municipios_base_de_dados_2010_2016 <- read_excel("~/Analise_SICONFI/Data/PIB dos Municípios - base de dados 2010-2016.xls")


names(PIB_dos_Municipios_base_de_dados_2010_2016)[7]<- "codigo_municipio"

names(ibama)


mun_NE <- 
  municipios_IBGE%>%
  filter(cod_uf %in% c(21:29))
  

PIB_Mun_sel<- PIB_dos_Municipios_base_de_dados_2010_2016%>%
  filter(codigo_municipio %in% mun_sel)


PIB_Mun_NE<- PIB_dos_Municipios_base_de_dados_2010_2016%>%
  filter(codigo_municipio %in% mun_NE$cod_mun)


glimpse(PIB_dos_Municipios_base_de_dados_2010_2016)

save(list = c("df_dca_mun_sel_RO","df_dca_mun_sel_func","PIB_Mun_sel"), file = "economia_oleo_mun.RData")


save(list = c("df_dca_NE_RO","df_dca_NE_func","PIB_Mun_NE"), file = "economia_NE.RData")  

