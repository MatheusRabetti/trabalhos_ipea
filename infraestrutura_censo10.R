# Série de pacotes a serem usados na rotina
library(bit64)
library(data.table)
library(descr)
library(dplyr)
library(stringr)
options(scipen=999) # disable scientific notation

rm(list = ls())
gc()

# Rotina de 9 MINUTOS

setwd("C:/Users/b2562360/Documents/Work/IVS/censo/infraestrutura/")

# LEITURA -----------------------------------------------------------------
# Para mais informaçoes sobre as variáveis consultar o arquivo dicionario_ivs


dadosdom <- 
  fread(input = "../dados_brutos/censo2010_BRdom.CSV", sep = ',', 
        sep2 = 'auto', integer64 = 'double',
        select = c("v0300", "v0001", "v0002", "V1004", "v0011", "V1001", 
                   "v0010", "V4001", "V1006", 
                   "V0210", "V0207", "V0209" ))
dadospes <- 
  fread(input = "../dados_brutos/censo2010_BRpes.CSV", sep = ',', 
        sep2 = 'auto', integer64 = 'double',
        select = c("v0300", "V0601", "V0606", "V0502", 
                   "V0662", "V0641", "V0642", "V0643", "V0644"))


# MERGE -------------------------------------------------------------------
# insira a variável V0300 (id_domicilio) como id
setkey(dadospes, v0300)
setkey(dadosdom, v0300)

# join data.table - mais rápido
banco <- dadospes[dadosdom, nomatch=0]
rm(dadospes, dadosdom)

# Tamanho na base
format(object.size(banco), units = "Gb") # 1.7 Gb

# FILTROS ----------------------------------------------------------------
# A manipulação vai utilizar ao máximo possível do pacote data.table
# excluindo pensionista, empregado domestico e parentes do empregado 
banco[V0502 >= 1 & V0502<=16] -> banco

# excluindo os domicilios coletivos
banco[V4001==1 | V4001==2] -> banco
# remover as variáveis para ganhar espaço na memória
banco[, c("V4001","V0502") := NULL]

# Adicionar uma coluna com apenas valores igual a 1
banco[, count := 1]

# Situaçao Domicilio Urbano - Rural
banco[, urbano := ifelse(V1006 == 1, 1, 0)]
banco[, V1006 := NULL]


# INDICADORES -------------------------------------------------------------

# Indicador 1
# Populacao ocupada 
banco[, ocupado := ifelse(V0641 %in% 1 |  V0642 %in% 1 | 
                            V0643 %in% 1 | V0644 %in% 1, 1, 0)]
banco[, c("V0641", "V0642", "V0643", "V0644") := NULL]  

# Vulneravel
# Renda Inputada e Salario Minimo
salario_min <- readRDS("../salario_minimo.rds")
salario_min[salario_min$ano == 2010, ]$sal_min -> salar_min

renda_input <- fread(input = "dados_brutos/renda_inputada2010.csv", sep = ",")
setkey(renda_input, iddom)
renda_input %>%
  select(iddom, rdpc_imput) -> renda_input

banco <- banco[renda_input, nomatch=0]
rm(renda_input)
banco[,vulner := ifelse(rdpc_imput <= salar_min/2, 1, 0)] -> banco

# Ocupado e vulneravel
banco[, ocup_vulner := ifelse(ocupado %in% 1 & vulner %in% 1, 1, 0)]
banco[, c("rdpc_imput", "ocupado") := NULL]

# Deslocamento para o trabalho
banco[, ind1 := ifelse(V0662 %in% 4:5 & vulner %in% 1, 1, 0)]
banco[, c("V0662", "vulner") := NULL]

# Indicador 2
# Foi considerado apenas domicílios urbanos
banco[, lixo := ifelse(V0210 %in% 1:2 & urbano %in% 1, 1, 0)]
banco[, V0210 := NULL]

# Indicador 3
banco[, agua_esgoto := ifelse(V0207 %in% 1:2 & V0209 %in% 1, 1, 0)]
banco[, c("V0207", "V0209") := NULL]


# AGREGAR -----------------------------------------------------------------
# Método otimizado para soma ponderada pelo peso
banco[ ,lapply(.SD, function(x, w) sum(x*w), w = v0010),
         by=.(v0011, V0606, V0601, urbano, v0001, 
              v0002, V1004, V1001)] -> data_ivs
data_ivs[, v0300 := NULL]

data_ivs[, ind2 := count - lixo]
data_ivs[, lixo := NULL]

data_ivs[, ind3 := count - agua_esgoto]
data_ivs[, agua_esgoto := NULL]

data_ivs %>%
  arrange(v0001, v0002, urbano, V0601, V0606) -> data_ivs

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

write.csv2(data_ivs, "infra_estrutura2010.csv", row.names = F)


# SAÍDA -------------------------------------------------------------------

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

# Indicador 1
export(data_ivs, pop = "ocup_vulner", var = "ind1", "brasil")
export(data_ivs, pop = "ocup_vulner", var = "ind1", "UF")
export(data_ivs, pop = "ocup_vulner", var = "ind1", "UDH")
export(data_ivs, pop = "ocup_vulner", var = "ind1", "Municipio")
export(data_ivs, pop = "ocup_vulner", var = "ind1", "Regiao_metropolitana")
export(data_ivs, pop = "ocup_vulner", var = "ind1", "Macroregiao")

data_ivs[sit_dom == 'urbano'] -> data_ivs_urbano
# Indicador 2
export(data_ivs_urbano, pop = "count", var = "ind2", "brasil")
export(data_ivs_urbano, pop = "count", var = "ind2", "UF")
export(data_ivs_urbano, pop = "count", var = "ind2", "UDH")
export(data_ivs_urbano, pop = "count", var = "ind2", "Municipio")
export(data_ivs_urbano, pop = "count", var = "ind2", "Regiao_metropolitana")
export(data_ivs_urbano, pop = "count", var = "ind2", "Macroregiao")

# Indicador 3
export(data_ivs, pop = "count", var = "ind3", "brasil")
export(data_ivs, pop = "count", var = "ind3", "UF")
export(data_ivs, pop = "count", var = "ind3", "UDH")
export(data_ivs, pop = "count", var = "ind3", "Municipio")
export(data_ivs, pop = "count", var = "ind3", "Regiao_metropolitana")
export(data_ivs, pop = "count", var = "ind3", "Macroregiao")

