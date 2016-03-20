library(readxl)
library(downloader)	
library(RCurl)	
library(dplyr)

rm(list = ls())
gc()

setwd('C:/Users/b2562360/Desktop/')


# iniciar o link do respositorio do IBGE da PNAD Contínua
tabelas_path <- "ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Mensal/Tabelas/"

# Ler as informações da pasta
tabelas <- readLines(textConnection(getURL(tabelas_path)))

# selecionar as tabelas disponíveis
tabelas <- gsub("(.*)(pnadc_[0-9]{6})", "\\2", tabelas)

# selecionar a tabela mais recente
tabelas <- tabelas[tabelas != ""]
tabela_att <- tabelas[length(tabelas)]

# Download da planilha mensal mais recente
tab_link <- paste0(tabelas_path, tabela_att)
download.file(tab_link, destfile = tabela_att)

# lendo a planilha baixada
planilhas <- excel_sheets(tabela_att)

# Lendo a planilha sumario
sumario <- read_excel(tabela_att, "Sumário", skip = 8)
names(sumario) <- c("del", "parte1", "indice", "descricao")
sumario <- select(sumario, -del)
sumario <- na.omit(sumario)


sumario$indice <- paste(sumario$parte1, sumario$indice)
sumario %>%
  select(-parte1) -> sumario

# População
populacao <- read_excel("pnadc_201512_mensal.xls", "4 - Tabela 1", skip = 7)
populacao <- populacao[ ,1:3] 
names(populacao) <- c("ano", "mes", "População")

# Ultima linha do arquivo - fonta da tabela
last_line <- grep("[Ff]onte.*", populacao$ano) - 2

# Ultimo ano analisado
last_year <- max(as.numeric(populacao$ano), na.rm = T)

# Cortar as linhas a mais
populacao <- populacao[1:last_line, ]
# Gerando os anos
populacao$ano <- c(rep(2012, 10), rep(2013:last_year, 12))

populacao$mes <-
  gsub("[a-z]{3}-[a-z]{3}-([a-z]{3})", "\\1", populacao$mes)
populacao$data <-
  paste0(populacao$mes,"/",substr(populacao$ano, 3,4))
