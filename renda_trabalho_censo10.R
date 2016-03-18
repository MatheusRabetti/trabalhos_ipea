# Série de pacotes a serem usados na rotina
library(bit64)
library(data.table)
library(descr)
library(dplyr)
library(stringr)
options(scipen=999) # disable scientific notation

rm(list = ls())
gc()

setwd("C:/Users/b2562360/Documents/Work/IVS/censo/renda_trabalho/")

# LEITURA -----------------------------------------------------------------
# Para mais informaçoes sobre as variáveis consultar o arquivo dicionario_ivs


dadosdom <- 
  fread(input = "../dados_brutos/censo2010_BRdom.CSV", sep = ',', 
        sep2 = 'auto', integer64 = 'double',
        select = c("v0300", "v0001", "v0002", "V1004", "v0011", "V1001", 
                   "v0010", "V4001", "V1006"))
dadospes <- 
  fread(input = "../dados_brutos/censo2010_BRpes.CSV", sep = ',', 
        sep2 = 'auto', integer64 = 'double',
        select = c("v0300", "V0601", "V0606", "V0502", 
                   "V0629", "V0633", "V0634", "V0628", "V6036",
                   "V0654", "V0641", "V0642", "V0643", "V0644",
                   "V0648", "V0650", "V6531", "V6527"))


# MERGE -------------------------------------------------------------------
# insira a variável V0300 (id_domicilio) como id
setkey(dadospes, v0300)
setkey(dadosdom, v0300)

# join data.table - mais rápido
banco <- dadospes[dadosdom, nomatch=0]
rm(dadospes, dadosdom)

# Tamanho na base
format(object.size(banco), units = "Gb") 

# FILTROS ----------------------------------------------------------------
# A manipulação vai utilizar ao máximo possível do pacote data.table
# excluindo pensionista, empregado domestico e parentes do empregado 
banco[V0502 >= 1 & V0502<=16] -> banco

# excluindo os domicilios coletivos
banco[V4001==1 | V4001==2] -> banco
# remover as variáveis para ganhar espaço na memória
banco[, V4001 := NULL]
banco[, V0502 := NULL]

# Adicionar uma coluna com apenas valores igual a 1
banco[, count := 1]

# Situaçao Domicilio Urbano - Rural
banco[, urbano := ifelse(V1006 == 1, 1, 0)]
banco[, V1006 := NULL]


# INDICADORES -------------------------------------------------------------

salario_min <- readRDS("../../salario_minimo.rds")
salario_min[salario_min$ano == 2010, ]$sal_min -> salar_min
rm(salario_min)

# Indicador 1 
# Renda domiciliar per capita - INPUT

renda_input <- fread(input = "../dados_brutos/renda_inputada2010.csv", sep = ",")
setkey(renda_input, iddom)
renda_input %>%
  select(iddom, rdpc_imput) -> renda_input

banco <- banco[renda_input, nomatch=0]
rm(renda_input)

banco[, ind1 := ifelse(rdpc_imput <= 255, 1, 0)]


# Indicador 2
# Desocupado
banco[,desocup := ifelse(V0641 %in% 2 & V0642 %in% 2 & V0643 %in% 2 & 
                           V0644 %in% 2 & V0654 %in% 1, 1, 0)]
# Populaçao economicamente ativa - desocupado ou ocupado
banco[,ocup:= ifelse(V0641 %in% 1 | V0642 %in% 1 |
                       V0643 %in% 1 | V0644 %in% 1, 1, 0)]
banco[,PEA := ifelse((desocup %in% 1) | (ocup %in% 1), 1, 0)]
banco[, c("V0641", "V0642", "V0643", "V0644", "V0654", "ocup") := NULL]

# PEA 10 a 14 e populacao 10 a 14
banco[, ind2 := ifelse(PEA %in% 1 & V6036 %in% 10:14, 1, 0)]
banco[, pop10a14 := ifelse(V6036 %in% 10:14, 1, 0)]


# Indicador 3
banco[, ind3 := ifelse(desocup %in% 1 & V6036 %in% 18:200, 1, 0)]
banco[, PEA18m := ifelse(PEA %in% 1 & V6036 %in% 18:200, 1, 0)]
banco[, c("PEA", "desocup") := NULL]
gc()

# Indicador 4
# Fundamental incompleto
banco[, fundin := ifelse(V0633 %in% c(1:3,5,6), 1, 0)]
banco[, fundin := ifelse(V0629 %in% 1:6, 1, fundin)]
banco[, fundin := ifelse((V0633 %in% c(4,7,8)) & (V0634 %in% 2), 1, fundin)]
banco[, fundin := ifelse(V0628 %in% 4, 1, fundin)]
banco[, c("V0633", "V0629", "V0634", "V0628") := NULL]

# Ocupaçao Informal
banco[, informal := ifelse(V0648 %in% 1:3 | 
                             (V0648 %in% 5:6 & V0650 %in% 1), 0, 1)]
banco[, c("V0648", "V0650") := NULL]

banco[, pop18m := ifelse(V6036 %in% 18:200, 1, 0)]
banco[, ind4 := ifelse(informal %in% 1 & V6036 %in% 18:200 &
                         fundin %in% 1, 1, 0)]
banco[, c("fundin", "informal") := NULL]
gc()

# Indicador 5
# idosos >= 65
banco[, adulto := ifelse(V6036 %in% 15:200, 1, 0)]

# Renda domicilio imputada e numero de adultos
banco[, lapply(.SD, sum), by = v0300,
      .SDcols = c("rdpc_imput", "adulto")] -> temp
setkey(temp, v0300)
setnames(temp, "rdpc_imput", "renda_dom")
setnames(temp, "adulto", "adulto_dom")

banco <- banco[temp, nomatch=0]
rm(temp); gc()

# Renda pessoal
banco[, rendap := ifelse(V6531 %in% 0 & rdpc_imput > 0 & V6036 %in% 0:14, 0,
                         ifelse(V6531 %in% 0 & rdpc_imput > 0 &
                                  V6036 %in% 14:200, renda_dom/adulto_dom, V6527))]
banco[, c("adulto_dom", "V6531", "V6527") := NULL]

# Idosos que tem mais da metade da renda do domicilio
banco[, max_rdpc_idoso := ifelse((rendap >= renda_dom*0.5) & 
                                   V6036 %in% 65:200, 1, 0)]
gc()

# Quantidade de idoso que recebe mais da metade da renda do dom por domicilio
banco[, lapply(.SD, sum), by = v0300,
      .SDcols = c("max_rdpc_idoso")] -> temp
setkey(temp, v0300)
setnames(temp, "max_rdpc_idoso", "dom_idoso")

banco <- banco[temp, nomatch=0]
rm(temp); gc()

# Vulneravel e com idoso com mais da metade da renda domiciliar
banco[, ind5 := ifelse(rdpc_imput <= salar_min/2 & dom_idoso >=1, 1, 0)]
banco[, c("rdpc_imput", "adulto", "renda_dom", 
          "rendap", "max_rdpc_idoso", "dom_idoso") := NULL]

# Idade
banco[, V6036 := NULL]



# AGREGAR -----------------------------------------------------------------
# Tabela mãe
# Manter apenas variaveis para construçao dos indicadores
# Método otimizado para soma ponderada pelo peso
banco[ ,lapply(.SD, function(x, w) sum(x*w), w = v0010),
      by=.(v0011, V0606, V0601, urbano, v0001, 
           v0002, V1004, V1001)] -> data_ivs
data_ivs[, v0300 := NULL]


data_ivs %>%
  mutate(sexo = ifelse(V0601 == 1, "homem", "mulher")) %>%
  mutate(raca = ifelse(V0606 == 1, "branca", 
                       ifelse(V0606 == 2, "preta", 
                              ifelse(V0606 == 3, "amarela", 
                                     ifelse(V0606 == 4, "parda",
                                            ifelse(V0606 == 5, "indígena", 
                                                   "sem declaraçao")))))) %>%
  mutate(sit_dom = ifelse(urbano == 1, "urbano", "rural")) -> data_ivs

data_ivs %>%
  select(-c(V0601, V0606, urbano)) -> data_ivs

# Enviar os indicadores
setnames(data_ivs, "v0011", "UDH")
setnames(data_ivs, "v0001", "UF")
setnames(data_ivs, "v0002", "Municipio")
setnames(data_ivs, "V1004", "Regiao_metropolitana")
setnames(data_ivs, "V1001", "Macroregiao")
setnames(data_ivs, "v0010", "Peso")


# Adicionar zeros à esquerda
data_ivs$Municipio <-
  str_pad(data_ivs$Municipio, width = 5, side = "left", pad = 0) 
# Juntando o prefixo de UF
data_ivs$Municipio <-
  paste0(data_ivs$UF, data_ivs$Municipio)

write.csv2(data_ivs, "renda_trabalho2010.csv", row.names = F)



# SAÍDA -------------------------------------------------------------------
# Indicador 1 tem valores absolutos
# Orgnaizar o denominador e o numerador das razões pra gerar os indicadores
data.frame(denominador = c("pop10a14", "PEA18m",
                           "pop18m", "count"),
           numerador = paste0("ind", 2:5)) -> razao
razao$denominador <- as.character(razao$denominador)
razao$numerador <- as.character(razao$numerador)

geo <- c("brasil", "UF", "UDH",
         "Municipio", "Regiao_metropolitana", "Macroregiao")

library(lazyeval)
export <- function(dados, pop, var, territorio) {
  saida = list()
  if(territorio == "brasil"){
    # Geral
    dados %>%
      group_by() %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent =  my_var / my_pop) %>%
      mutate(classe = "brasil") %>%
      select(-my_pop, -my_var) -> saida[[1]]
    
    # Sexo
    dados %>%
      group_by(sexo) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var / my_pop) %>%
      mutate(classe = sexo) %>%
      ungroup(.) %>%
      select(-my_pop, -my_var, -sexo) -> saida[[2]]
    
    # Raca
    dados %>%
      group_by(raca) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var/ my_pop) %>%
      mutate(classe = raca) %>%
      ungroup(.) %>%
      select(-my_pop, -my_var, -raca) -> saida[[3]]
  } else {
    
    # Geral
    dados %>%
      group_by_(territorio) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var/ my_pop) %>%
      mutate(classe = "brasil") %>%
      select(-my_pop, -my_var) -> saida[[1]]
    
    # Sexo
    dados %>%
      group_by_(quote(sexo), territorio) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var/ my_pop) %>%
      mutate(classe = sexo) %>%
      ungroup(.) %>%
      select(-my_pop, -my_var, -sexo) -> saida[[2]]
    
    # Raca
    dados %>%
      group_by_(quote(raca), territorio) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var/ my_pop) %>%
      mutate(classe = raca) %>%
      ungroup(.) %>%
      select(-my_pop, -my_var, -raca) -> saida[[3]]
  }
  saida <- rbindlist(saida)
  write.csv2(saida, paste0(var,"_",territorio,".csv"), row.names = F)
}

# Loop para gerar os indicadores
for(i in 2:5) {
  print(paste("Iniciando indicador", i, "..."))
  sapply(geo, function(x) export(data_ivs, pop = razao$denominador[i-1], 
                                 var = razao$numerador[i-1], x))
  print("Finalizado")
}
