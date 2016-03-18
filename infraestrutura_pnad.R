

# Série de pacotes a serem usados na rotina
library(bit64)
library(data.table)
library(descr)
library(survey)
library(dplyr)

getwd()
rm(list = ls())

setwd("C:/Users/b2562360/Documents/Work/IVS/infraestrutura_urbana/")

# diretorio com base de dados em csv
dir_dados <- "//Storage2/bases/NINSOC/Bases de dados/PNAD/CSV/"

## DOM
# V0218 = Coleta de lixo
# V0217 = Esgoto
# V0211 = Água canalizada

## PES
# V9001 = Trabalhou
# V9002 = Afastado temporariamente
# V9003 = Cultivo, pesca ou animais para consumo próprio

### FIXO ###
# Pes
# V0404 = Raça
# V0302 = Sexo
# V0401 = Parentesco chefe do domicílio

# Dom
# V4105 = Situaçao de domicilio // Rural ou Urbano
# V0201 = Especie de domicilio // Particular ou Coletivo
# V4611 = Peso
# V4617 = Estrato

pnad_list <- list()
for(ano in 2011:2014){
  print(ano)
  
  dadosdom <- 
    fread(input = paste0(dir_dados,'DOM',ano,'.csv'), sep = 'auto', 
          sep2 = 'auto', integer64 = 'double',
          select = c("DOMICILIOID","V0101", "UF","V0201","V4611", "V4105",
                     "V0218", "V0217", "V0211"))
  dadospes <- 
    fread(input = paste0(dir_dados,'PES',ano,'.csv'), sep = 'auto', 
          sep2 = 'auto', integer64 = 'double',
          select = c("DOMICILIOID","UF",
                     "V0404", "V0302", "V0401", "V9057",
                     "V9001", "V9002", "V9003"))
  banco = merge(dadosdom, dadospes, by = c("DOMICILIOID","UF"))
  rm(dadospes, dadosdom)
  
  id <- ano-2010
  pnad_list[[id]] <- banco
  rm(banco, id, ano)
}

banco <- rbindlist(pnad_list)
rm(pnad_list)
# Adicionar uma coluna com apenas valores igual a 1
banco$count = 1

# Adicionar coluna com as regioes geograficas
# Primeiro digito do cod uf
banco$macroreg <- as.integer(substring(banco$UF, 1, 1))

# Situaçao de domicilio - Urbano e rural
banco$urbano <- ifelse(banco$V4105 %in% 1:3, 1, 0)

# Excluir pensionista, empregado doméstico e parentes do empregado
# Excluir domicílios coletivos
banco %>%
  filter(V0401 %in% 1:5) %>%
  filter(V0201 == 1) -> banco

# Indicador 1
# Vulneravel
# Merge com a renda inputada e com salario minimo
# Salario minimo
salario_min <- readRDS("../salario_minimo.rds")
banco <- as.data.frame(banco)
left_join(banco, salario_min, by = c("V0101" = "ano")) -> banco

# Renda inputada
list.files("../", pattern = "renda.*csv")
renda_input <- data.frame()
for(year in 2011:2014){
  print(year)
  temp <- read.csv(paste0("../renda_inputada",year,".csv"), 
                   header = T, sep = ",")
  temp$ano <- year
  renda_input <- rbind(renda_input, temp)
  rm(temp, year)
}

# Transformar os valores 999999999999 em NA
# renda_input %>%
#   mutate(rdpc_input = ifelse(rdpc_input == 999999999999, 
#                              NA, rdpc_input)) -> renda_input

inner_join(banco, select(renda_input, DOMICILIOID, rdpc_input, ano),
           by = c("DOMICILIOID", "V0101" = "ano")) -> banco
banco %>%
  mutate(vulner = ifelse(rdpc_input <= sal_min/2, 1, 0)) -> banco
rm(renda_input)

# Populacao ocupada vulneravel
banco %>%
  mutate(ocupado = ifelse(V9001 %in% 1 |  V9002 %in% 2 | V9003 %in% 1, 
                          1, 0)) %>%
  mutate(ocup_vulner = ifelse(ocupado %in% 1 & vulner %in% 1, 1, 0)) -> banco

# Tempo de deslocamento
banco %>%
  mutate(desloc = ifelse(V9057 %in% 5:7, 1, 0)) %>%
  mutate(ind1 = ifelse(vulner %in% 1 & desloc %in% 1, 1, 0)) -> banco

# Indicador 2
# Foi considerado apenas domicílios urbanos
banco$lixo <- ifelse(banco$V0218 %in% 1:2 & banco$urbano %in% 1, 1, 0)

# Indicador 3
banco$agua_esgoto <- ifelse(banco$V0217 %in% 1:3 & 
                              banco$V0211 == 1, 1, 0)


# options(survey.lonely.psu = "adjust")
# sample.pnad <-
#   svydesign(
#     id = ~DOMICILIOID,
#     data = banco,
#     weights = ~V4611,
#     nest = TRUE) 
# 
# # Gerando 
# data_habit <- svyby(~ count + lixo + agua_esgoto, 
#                     by = ~V0101 + V0404 + V0302 + sit_dom + UF + macroreg, 
#                     sample.pnad, FUN = svytotal)
# # Retirando os rownames
# # rownames(data_habit) <- NULL
# 
# ### Organizando os dados pra saída para excel
# # primeiro select para retirar as colunas de erro (ex: se.count, se.lixo)
# # as duas linhas seguintes com o mutate estão transformando  as minhas variáveis
# # o último select retira as variáveis inicias que transformei
# data_habit %>%
#   select(-contains("se.")) %>%
#   mutate(sexo = ifelse(V0302 == 2, "homem", "mulher")) %>%
#   mutate(raca = ifelse(V0404 == 0, "indígena", 
#                        ifelse(V0404 == 2, "branca", 
#                               ifelse(V0404 == 4, "preta", 
#                                      ifelse(V0404 == 6, "amarela",
#                                             ifelse(V0404 == 8, "parda", "sem declaraçao")))))) %>%
#   select(-c(V0404, V0302)) -> data_habit

banco %>%
  mutate(sexo = ifelse(V0302 == 2, "homem", "mulher")) %>%
  mutate(raca = ifelse(V0404 == 0, "indígena", 
                       ifelse(V0404 == 2, "branca", 
                              ifelse(V0404 == 4, "preta", 
                                     ifelse(V0404 == 6, "amarela",
                                            ifelse(V0404 == 8, "parda", 
                                                   "sem declaraçao")))))) %>%
  mutate(sit_dom = ifelse(urbano == 1, "urbano", "rural")) %>%
  select(V0101, UF, V4611, count:sit_dom) -> data_ivs


data_ivs <- as.data.table(data_ivs)
# Método otimizado para soma ponderada pelo peso - pacote data.table
data_ivs[ ,lapply(.SD, function(x, w) sum(x*w), w = V4611),
         by=.(V0101, raca, sexo, sit_dom, UF, macroreg)] -> data_ivs

data_ivs %>%
  arrange(V0101, UF, sit_dom, sexo, raca) -> data_ivs

# Gerando a variavel de interesse
data_ivs %>%
  mutate(sem_lixo = count - lixo)  %>% 
  mutate(sem_agua_esgoto = count - agua_esgoto) -> data_ivs



# No caso do lixo só usaremos domicilios urbanos como populaçao
data_ivs %>%
  filter(sit_dom == 'urbano') -> data_ivs_urbano

### Brasil
library(lazyeval)
export <- function(dados, pop, var, territorio) {
  saida = list()
  if(territorio == "brasil"){
    # Geral
    dados %>%
      group_by(V0101) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent =  my_var / my_pop) %>%
      mutate(classe = "brasil") %>%
      select(-my_pop, -my_var) -> saida[[1]]
    
    # Sexo
    dados %>%
      group_by(V0101, sexo) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var / my_pop) %>%
      mutate(classe = sexo) %>%
      ungroup(.) %>%
      select(-my_pop, -my_var, -sexo) -> saida[[2]]
    
    # Raca
    dados %>%
      group_by(V0101, raca) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var/ my_pop) %>%
      mutate(classe = raca) %>%
      ungroup(.) %>%
      select(-my_pop, -my_var, -raca) -> saida[[3]]
  } else {
    
    # Geral
    dados %>%
      group_by_(quote(V0101), territorio) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var/ my_pop) %>%
      mutate(classe = "brasil") %>%
      select(-my_pop, -my_var) -> saida[[1]]
    
    # Sexo
    dados %>%
      group_by_(quote(V0101), quote(sexo), territorio) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var/ my_pop) %>%
      mutate(classe = sexo) %>%
      ungroup(.) %>%
      select(-my_pop, -my_var, -sexo) -> saida[[2]]
    
    # Raca
    dados %>%
      group_by_(quote(V0101), quote(raca), territorio) %>%
      summarise_(my_pop = interp(~sum(pop), pop = as.name(pop)),
                 my_var = interp(~sum(var), var = as.name(var))) %>%
      mutate(percent = my_var/ my_pop) %>%
      mutate(classe = raca) %>%
      ungroup(.) %>%
      select(-my_pop, -my_var, -raca) -> saida[[3]]
  }
  saida <- rbindlist(saida)
  setnames(saida, "V0101", "ano")
  write.csv2(saida, paste0(var,"_",territorio,".csv"), row.names = F)
}

# Indicador 1
export(data_ivs, pop = "ocup_vulner", var = "ind1", "brasil")
export(data_ivs, pop = "ocup_vulner", var = "ind1", "UF")
export(data_ivs, pop = "ocup_vulner", var = "ind1", "macroreg")

# Indicador 2
export(data_ivs_urbano, pop = "urbano", var = "sem_lixo", "brasil")
export(data_ivs_urbano, pop = "urbano", var = "sem_lixo", "UF")
export(data_ivs_urbano, pop = "urbano", var = "sem_lixo", "macroreg")

# Indicador 3
export(data_ivs, pop = "count", var = "sem_agua_esgoto", "brasil")
export(data_ivs, pop = "count", var = "sem_agua_esgoto", "UF")
export(data_ivs, pop = "count", var = "sem_agua_esgoto", "macroreg")




