# Série de pacotes a serem usados na rotina
library(bit64)
library(data.table)
library(descr)
library(dplyr)
library(stringr)
options(scipen=999) # disable scientific notation

rm(list = ls())
gc()

setwd("C:/Users/b2562360/Documents/Work/IVS/censo/capital_humano/")

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
                   "V0629", "V0633", "V0634", "V0628", "V6800", "V6036",
                   "V0627", "V0654", "V0641", "V0642", "V0643", "V0644"))


# MERGE -------------------------------------------------------------------
# insira a variável V0300 (id_domicilio) como id
setkey(dadospes, v0300)
setkey(dadosdom, v0300)

# join data.table - mais rápido
banco <- dadospes[dadosdom, nomatch=0]
rm(dadospes, dadosdom)

# Tamanho na base
format(object.size(banco), units = "Gb") # 1.6 Gb

# FILTROS ----------------------------------------------------------------
# A manipulação vai utilizar ao máximo possível do pacote data.table
# excluindo pensionista, empregado domestico e parentes do empregado 
banco[V0502 >= 1 & V0502<=16] -> banco

# excluindo os domicilios coletivos
banco[V4001==1 | V4001==2] -> banco
# remover as variáveis para ganhar espaço na memória
banco[, V4001 := NULL]

# Adicionar uma coluna com apenas valores igual a 1
banco[, count := 1]

# Situaçao Domicilio Urbano - Rural
banco[, urbano := ifelse(V1006 == 1, 1, 0)]
banco[, V1006 := NULL]


# INDICADORES -------------------------------------------------------------

# Indicador 1 - Adolescentes (10 a 17) com filhos
banco[, ind1 := ifelse(V6036 %in% 10:17 & V0601 %in% 2 & V6800 > 0, 1, 0)]
banco[, m10a17 := ifelse(V6036 %in% 10:17 & V0601 %in% 2, 1, 0)]
banco[, V6800 := NULL]

# Indicador 2
# Fundamental incompleto
banco[, fundin := ifelse(V0633 %in% c(1:3,5,6), 1, 0)]
banco[, fundin := ifelse(V0629 %in% 1:6, 1, fundin)]
banco[, fundin := ifelse((V0633 %in% c(4,7,8)) & (V0634 %in% 2), 1, fundin)]
banco[, fundin := ifelse(V0628 %in% 4, 1, fundin)]
banco[, c("V0633", "V0629", "V0634") := NULL]

# Mulher chefe de familia
banco[, mulchefe := ifelse(V0502 %in% 1 & V0601 %in% 2, 1, 0)]

# Mulher chefe de familia com fundamental incompleto
banco[, mulchefefundin := ifelse(mulchefe %in% 1 & fundin %in% 1, 1, 0)]

# Criança menor - Filho de 0 a 14 anos
banco[, filho0a14 := ifelse(V0502 %in% 4:5 & V6036 %in% 0:14, 1, 0)]

# Mulher chefe de familia com fundamental incompleto com filho menor
# Agregar informaçoes por domicilio
banco[, filho_menor := sum(filho0a14), by = v0300]

banco[, ind2 := ifelse(mulchefefundin %in% 1 & filho_menor > 0, 1, 0)]
banco[, c("mulchefefundin", "filho0a14", "filho_menor") := NULL]

# Indicador 3
# Criança - populaçao de 0 a 14 anos
banco[, crianca := ifelse(V6036 %in% 0:14, 1, 0)]

# Agregar informaçoes por domicilio
banco[, lapply(.SD, sum),by = v0300, .SDcols = c("count", "fundin")] -> temp
setkey(temp, v0300)
temp[, dom_fundin := as.numeric(count == fundin)]
temp[, c("fundin", "count") := NULL]

banco <- banco[temp, nomatch=0]
rm(temp); gc()
banco[, ind3 := ifelse(dom_fundin %in% 1 & V6036 %in% 0:14, 1, 0)]

banco[, c("fundin", "dom_fundin") := NULL]

# Indicador 4 e 5 - Criança fora da escola
banco[, pop0a5 := ifelse(V6036 %in% 0:5, 1, 0)]
banco[, ind4 := ifelse(V6036 %in% 0:5 & V0628 %in% 3:4, 1, 0)]
banco[, pop6a14 := ifelse(V6036 %in% 6:14, 1, 0)]
banco[, ind5 := ifelse(V6036 %in% 6:14 & V0628 %in% 3:4, 1, 0)]

# Base ficou muito grande - 3gb
# Gerar os dados agregados e trazer as outras informaçoes mais tarde

banco[ ,lapply(.SD, function(x, w) sum(x*w), w = v0010),
      by=.(v0011, V0606, V0601, urbano, v0001, 
           v0002, V1004, V1001)] -> data_ivs
data_ivs[, v0300 := NULL]

data_ivs %>%
  arrange(v0001, v0002, urbano, V0601, V0606) -> data_ivs

# Retirando as ultimas variaveis já salvas no data_ivs
banco %>%
  select(-c(ind1:ind5)) -> banco

# Indicador 6
# Vulneravel
# Renda Inputada e Salario Minimo
salario_min <- readRDS("../../salario_minimo.rds")
salario_min[salario_min$ano == 2010, ]$sal_min -> salar_min

renda_input <- fread(input = "../dados_brutos/renda_inputada2010.csv", sep = ",")
setkey(renda_input, iddom)
renda_input %>%
  select(iddom, rdpc_imput) -> renda_input

banco <- banco[renda_input, nomatch=0]
rm(renda_input)
banco[,vulner := ifelse(rdpc_imput <= salar_min/2, 1, 0)] -> banco

# Desocupado
banco[,desocup := ifelse(V0641 %in% 2 & V0642 %in% 2 & V0643 %in% 2 & 
                          V0644 %in% 2 & V0654 %in% 1, 1, 0)]
# Populaçao economicamente ativa - desocupado ou ocupado
banco[,PEA := ifelse((desocup %in% 1) | (V0641 %in% 1 & V0642 %in% 1 &
                                           V0643 %in% 1 & V0644 %in% 1), 1, 0)]
banco[, c("V0641", "V0642", "V0643", "V0644", "V0654") := NULL]

# Populaçao 15 a 24 anos
banco[, pop15a24 := ifelse(V6036 %in% 15:24, 1, 0)] 

banco[, ind6 := ifelse(pop15a24 %in% 1 & vulner %in% 1 & 
                         (desocup %in% 1 | PEA %in% 0) & V0628 %in% 3:4, 1, 0)]

# Indicador 7 - Analfabetismo 15 anos ou mais
banco[, pop15m := ifelse(V6036 %in% 15:200, 1, 0)]

banco[, ind7 := ifelse(pop15m %in% 1 & V0627 %in% 2, 1, 0)]

banco[ ,lapply(.SD, function(x, w) sum(x*w), w = v0010),
      by=.(v0011, V0606, V0601, urbano, v0001, 
           v0002, V1004, V1001)] -> data_ivs2
data_ivs2[, v0300 := NULL]

data_ivs2 %>%
  arrange(v0001, v0002, urbano, V0601, V0606) -> data_ivs2

# AGREGAR -----------------------------------------------------------------

# Manter apenas variaveis para construçao dos indicadores
data_ivs %>%
  select(-c(V0502:V0654)) -> data_ivs

data_ivs2 %>%
  select(-c(V0502:V0628, rdpc_imput:PEA)) -> data_ivs2

# Juntar as duas bases de dados já agregadas
inner_join(data_ivs, data_ivs2, by = c("v0011", "V0606", "V0601", "urbano",
                                       "v0001", "v0002", "V1004", "V1001", 
                                       "v0010", "count")) -> data_ivs
rm(data_ivs2)


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

write.csv2(data_ivs, "capital_humano2010.csv", row.names = F)



# SAÍDA -------------------------------------------------------------------

# Orgnaizar o denominador e o numerador das razões pra gerar os indicadores
data.frame(denominador = c("m10a17", "mulchefe", "crianca",
                           "pop0a5", "pop6a14", "pop15a24",
                           "pop15m"),
           numerador = paste0("ind", 1:7)) -> razao
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
for(i in 1:7) {
  print(paste("Iniciando indicador", i, "..."))
  sapply(geo, function(x) export(data_ivs, pop = razao$denominador[i], 
                                 var = razao$numerador[i], x))
  print("Finalizado")
}
  
