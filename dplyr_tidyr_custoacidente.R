getwd()
rm(list=ls())
dev.off()
gc()
setwd("L:/# DIRUR #/ASMEQ/Acidentes de trânsito")

# Base inicial
acidentes <- readRDS("Dados Brutos/ano2007/acidentes07.rds") #2007
acidentes <- readRDS("Dados Brutos/ano2010/acidentes10.rds") #2010
acidentes <- readRDS("Dados Brutos/ano2014/acidentes14.rds") #2014

custo <- dget("custo_padrao.R")
componente <- dget("componente.R")
# Salvar trabalho
saveRDS(custoaci, "Material Final/2007/custoacidentes07.rds")
saveRDS(custoaci, "Material Final/2010/custoacidentes10.rds")
saveRDS(custoaci, "Material Final/2014/custoacidentes14.rds")

# Base atual
custoaci07 <- readRDS("Material Final/2007/custoacidentes07.rds")
custoaci10 <- readRDS("Material Final/2010/custoacidentes10.rds")
custoaci14 <- readRDS("Material Final/2014/custoacidentes14.rds")


library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

## Primeira leitura #####

# Acidentes 2007
acidentes <-
  read.table(file = "ano2007/2007-jan a jun.csv", header = T, sep = ";", 
             quote = "", stringsAsFactors = F, encoding = "Latin1")


temp <- 
  read.table(file = "ano2007/2007-jul a dez.csv", header = T, sep = ";", 
                   quote = "", stringsAsFactors = F, encoding = "Latin1")


acidentes <- rbind(acidentes, temp)
rm(temp)

acidentes <- tbl_df(acidentes)
## Salvando em outro arquivo
# saveRDS(acidentes, "Dados Brutos/ano2007/acidentes07.rds")

# Acidentes 2010

diretorio = "Dados Brutos/ano2010/"
readData <- function(arquivo){
  print(arquivo)
  read.table(file = paste0(diretorio,arquivo), header = T, sep = ";", 
             quote = "", stringsAsFactors = F, 
             encoding = "Latin1")
}

files = list.files("ano2010/", pattern = ".csv")
acidentes <- do.call("rbind", lapply(files, readData))

## Salvando em outro arquivo
# saveRDS(acidentes, "acidentes10.rds")

# Acidentes 2014
diretorio = "Dados Brutos/ano2014/"
files = list.files(diretorio, pattern = ".csv")
acidentes <- do.call("rbind", lapply(files, readData))

# saveRDS(acidentes, "Dados Brutos/ano2014/acidentes14.rds")

# Custo padrao
# Ctrl c na tabela de custos do Excel

custo <- read.table('clipboard', header = T, sep = "\t",
                     stringsAsFactors = F, dec = ",",
                     colClasses = c('character', rep('numeric', 3)))

# dput(custo, "custo_padrao.r")

#### Começando a analise ####
names(acidentes)
glimpse(acidentes)

# 2014

acidentes <-
  acidentes %>%
  arrange(Código.Ocorrência) %>%
  select(Código.Ocorrência, Classificação.Acidente,
         Qtd..Feridos.Leves, Qtd..Feridos.Graves, Qtd..Mortos, Qtd..Ilesos,
         Tipo.Veículo, UF, Tipo.Envolvido,
         BR, Causa.Acidente) 

# 2007 e 2010
acidentes <-
  acidentes %>%
  arrange(Número.da.Ocorrência) %>%
  select(Número.da.Ocorrência, Classificação.Acidente,
         Qtd.Ferido.Leve, Qtd.Ferido.Grave, Qtd.Morto,
         Tipo.do.Veículo, UF.Acidente, Descrição.Tipo.Envolvido,
         BR.do.Acidente, Descrição.Causa.Acidente) 

# 2014 VEM COM ILESO - 2007 E 2010 NÃO (apaga ileso e ctrl-z)
names(acidentes) <- 
  c("ocorrencia","classificacao","feridoleve",
    "feridograve",'morto','ileso','veiculo','uf','desc_envolvido',
    'br','causa')

#### Organizando os dados ####

### Arrumar a classificacao de gravidade - nao informados -> sem vitimas

acidentes <- 
  acidentes %>%
  mutate(classificacao =
           iconv(classificacao, to = 'ASCII//TRANSLIT')) %>% #tirar acentos
  mutate(classificacao =
           tolower(classificacao)) %>%
  mutate(classificacao = 
           ifelse(classificacao %in% c('ignorado'),
                  'sem vitimas', classificacao))  

### Preciso limpar os nomes de veiculo
table(acidentes$veiculo)

acidentes <- 
  acidentes %>%
  mutate(veiculo =
           iconv(veiculo, to = 'ASCII//TRANSLIT')) %>% #tirar acentos
  mutate(veiculo =
           toupper(veiculo))

acidentes %<>%
  mutate(veiculo = 
           ifelse(veiculo %in% 
                    c('CAMINHAO-TANQUE', 'CAMINHAO-TRATOR', 
                      'SEMI-REBOQUE'),
                  'CAMINHAO', veiculo)) %>%
  mutate(veiculo = 
           ifelse(veiculo %in% 
                    c('MICROONIBUS', 'MICRO?IBUS'),
                  'ONIBUS', veiculo)) %>%
  mutate(veiculo = 
           ifelse(veiculo %in% 
                    c('CAMIONETA', 'CAMINHONETE'),
                  'AUTOMOVEL', veiculo)) %>%
  mutate(veiculo = 
           ifelse(veiculo %in% 
                    c('MOTOCICLETAS', 'MOTONETA', 
                      'CICLOMOTOR', 'SIDE-CAR'),
                  'MOTOCICLETA', veiculo)) %>%
  mutate(veiculo = 
           ifelse(veiculo %in% 
                    c('TRICICLO', 'TRATOR DE RODAS', 
                      'TRATOR DE ESTEIRAS', 'TRATOR MISTO',
                      'CARRO-DE-MAO', 'REBOQUE',
                      'CHARRETE', 'CARROCA',
                      'BONDE / TREM', 'QUADRICICLO'),
                  'OUTROS', veiculo)) %>%
  mutate(veiculo = 
           ifelse(veiculo %in% 
                    c('NAO INFORMADO', 'NAO IDENTIFICADO', 
                      'NAO SE APLICA', ''),
                  'SEM_INFORMACAO', veiculo)) 
  
### Contar veiculos só com o condutor

# Olhando a relaçao da descricao do envolvidos com os veiculos
table(acidentes$desc_envolvido)

temp <- 
  acidentes %>%
  filter(desc_envolvido == 'Autor') %>%
  select(ocorrencia) 

acidentes %>%
  filter(ocorrencia %in% temp$ocorrencia) %>%
  select(-uf, -classificacao) %>%
  group_by(veiculo) %>%
  summarise(n())

# Nova variavel binaria para somar os veiculos
acidentes <-
  acidentes %>%  
  mutate(envolvimento = 
           ifelse(desc_envolvido == 'Condutor', 1, 0))

rm(temp)


#### Virando a base de cabeça para baixo

############# ##
## IMPORTANTE ##
############# ##
# 2014 retira o mutate ileso
custoaci <- 
acidentes %>%
  mutate(veiculo = tolower(veiculo)) %>%
#  mutate(ileso = 
#           ifelse(feridoleve == 0 & feridograve == 0 & morto == 0, 1, 0)) %>%
  group_by(ocorrencia, classificacao, uf, br, causa, veiculo) %>%
  summarise(qtd = sum(envolvimento), #contar o numero de vezes que aparece o veiculo (com condutor)
            ileso = sum(ileso),
            feridoleve = sum(feridoleve),
            feridograve = sum(feridograve),
            morto = sum(morto)) %>%
  mutate_each(funs(sum), -c(qtd,veiculo)) %>% #até aqui agrupou-se por ocorrencia e veiculos, o proximo passo spread ignora todos os valores menos o qtd para agrupar - (Ex. ocorrencia com 4 automoveis com 5 ilesos no total e 1 onibus com mais 5 ilesos, essa linha faz as linhas virarem 4 automoveis e 10 ilesos)
  ungroup() %>%
  spread(veiculo, qtd, fill = 0) #transformar a coluna com nome dos veiculos em varias recebendo o valor de qtd

custoaci %<>%
  group_by(ocorrencia, classificacao, uf, br, causa) %>% #fazer o próximo passo by ocorrencia
  summarise_each(funs(sum(., na.rm = TRUE))) %>% #aplicar sum em todas colunas
  select(ocorrencia:morto,automovel,motocicleta,bicicleta,
         utilitario,caminhao,onibus,outros,sem_informacao) #reordenando
  

#### Calculando custos #####
fator <- 1.077392 #2007
fator <- 1.260507 #2010
fator <- 1.60127  #2014

### IMPORTANTE ###
# Evitar multiplicar por cima mais de uma vez
custo <- dget("custo_padrao.R")
custo[ ,2:4] <-
  round(custo[ ,2:4] * fator, digits = 2)

### Adicionando custo de pessoa
custoaci <- 

  custoaci %>%
  
  mutate(c1 = #ileso
           ifelse(classificacao == 'sem vitimas', ileso * custo$sem_vitima[1], 
                  ifelse(classificacao == 'com vitimas feridas', ileso * custo$com_vitima[1], 
                         ifelse(classificacao == 'com vitimas fatais', ileso * custo$com_vitima_fatal[1],0)))
  ) %>%
  mutate(c2 = #feridoleve
           ifelse(classificacao == 'sem vitimas', feridoleve * custo$sem_vitima[2], 
                  ifelse(classificacao == 'com vitimas feridas', feridoleve * custo$com_vitima[2], 
                         ifelse(classificacao == 'com vitimas fatais', feridoleve * custo$com_vitima_fatal[2],0)))
  ) %>%
  mutate(c3 = #feridograve
           ifelse(classificacao == 'sem vitimas', feridograve * custo$sem_vitima[3], 
                  ifelse(classificacao == 'com vitimas feridas', feridograve * custo$com_vitima[3], 
                         ifelse(classificacao == 'com vitimas fatais', feridograve * custo$com_vitima_fatal[3],0)))
  ) %>%
  mutate(c4 = #morto
           ifelse(classificacao == 'sem vitimas', morto * custo$sem_vitima[4], 
                  ifelse(classificacao == 'com vitimas feridas', morto * custo$com_vitima[4], 
                         ifelse(classificacao == 'com vitimas fatais', morto * custo$com_vitima_fatal[4],0)))
  ) %>%
  
  group_by(ocorrencia) %>%
  summarise(custo_pessoa = sum(c1,c2,c3,c4)) %>%
  select(custo_pessoa) %>%
  cbind(custoaci, .)

### Adicionando custo de veiculo

chain_ifelse <- function(data, comp) {
  id <- which(custo$componente == comp)  
  ifelse(data$classificacao == 'sem vitimas', data[ ,comp] * custo$sem_vitima[id], 
         ifelse(data$classificacao == 'com vitimas feridas', data[ ,comp] * custo$com_vitima[id], 
                ifelse(data$classificacao == 'com vitimas fatais', data[ ,comp] * custo$com_vitima_fatal[id], 0)))
}

custoaci <- 
  
  custoaci %>%
  
  mutate(c1 = chain_ifelse(data = ., comp = 'automovel')) %>%
  mutate(c2 = chain_ifelse(data = ., comp = 'motocicleta')) %>%
  mutate(c3 = chain_ifelse(data = ., comp = 'bicicleta')) %>%
  mutate(c4 = chain_ifelse(data = ., comp = 'utilitario')) %>%
  mutate(c5 = chain_ifelse(data = ., comp = 'caminhao')) %>%
  mutate(c6 = chain_ifelse(data = ., comp = 'onibus')) %>%
  mutate(c7 = chain_ifelse(data = ., comp = 'outros')) %>%
     
  group_by(ocorrencia) %>%
  summarise(custo_veiculo = sum(c1,c2,c3,c4,c5,c6,c7)) %>%
  select(custo_veiculo) %>%
  cbind(custoaci, .)


### Adicionando custo de outros
custoaci <-
  custoaci %>% 
  mutate(custo_outro = 
           ifelse(classificacao == 'sem vitimas', 
                  yes = custo$sem_vitima[12], 
                  no = ifelse(classificacao == 'com vitimas feridas', 
                              yes = custo$com_vitima[12], 
                              no = ifelse(classificacao == 'com vitimas fatais', 
                                          yes = custo$com_vitima_fatal[12],
                                          no = 0)))
  )

### Calculando custo total

custoaci <-
  custoaci %>% 
  mutate(custo_total = custo_pessoa + custo_veiculo + custo_outro)


### Por fim - gerando nova variavel regiao geografica

custoaci %<>%
  mutate(regiao = ifelse(uf %in% c("AM","PA","RO","AC","RR","AP","TO"),
                         'norte',NA)) %>%
  mutate(regiao = ifelse(uf %in% c("MA","PI","CE","RN","PB","PE","AL",
                                   "SE","BA"),
                         'nordeste', regiao)) %>%
  mutate(regiao = ifelse(uf %in% c("MG","ES","RJ","SP"),
                         'sudeste', regiao)) %>%
  mutate(regiao = ifelse(uf %in% c("PR","SC","RS"),
                         'sul', regiao)) %>%
  mutate(regiao = ifelse(uf %in% c("MT","GO","DF","MS"),
                         'centro-oeste', regiao)) 


### Exportando em csv o resultado
#write.csv2(custoaci, 'Material Final/2007/custoacidentes07.csv', row.names = F)
#write.csv2(custoaci, 'Material Final/2010/custoacidentes10.csv', row.names = F)
#write.csv2(custoaci, 'Material Final/2014/custoacidentes14.csv', row.names = F)

### Analise Exploratoria ####
# Morte em moto
custoaci %>%
  filter(motocicleta > 0 & morto > 0) %>%
  select(uf) %>%
  group_by(uf) %>%
  summarise(total = n(),
            percent = round(total/nrow(.),2)) %>%
  View

# Custo e numero de acidentes por região e gravidade
custoaci %>%
  group_by(regiao, classificacao) %>%
  summarise(Custo = sum(custo_total),
            Quantidade = n())
  
custoaci %>%
  group_by(regiao, classificacao) %>%
  summarise(Custo = sum(custo_total),
            Quantidade = n()) %>%
  write.csv2(x = .,'Material Final/2014/temp.csv')
  
# Quantidade de acidentes e pessoas por gravidade 
custoaci %>%
  group_by(classificacao) %>%
  summarise(Quantidade = n(),
            Pessoas = sum(ileso, feridoleve, feridograve, morto))

# Número de pessoas por lesão e veiculos Nao identificado
custoaci %>%
  select(ileso:morto, sem_informacao) %>%
  summarise_each(funs(sum(., na.rm = TRUE))) 

# Quantidades de pessoas por lesao e gravidade
custoaci %>%
  group_by(classificacao) %>%
  select(ileso:morto) %>%
  summarise_each(funs(sum(., na.rm = TRUE)))

### Analise causa de acidente ####
custoaci07 <- tbl_df(custoaci07)


# Totais por causa de acidente
custoaci %>%
  group_by(causa) %>%
  summarise_each(funs(sum(., na.rm = T)), 
                 -c(ocorrencia, regiao,classificacao,uf,br)) %>%
  select(causa:morto,custo_pessoa,custo_veiculo,custo_total) %>%
  write.csv2('Material Final/2014/temp.csv')


# Quantidade de acidentes por br e uf
custoaci07 %>%
  group_by(br, uf) %>%
  tally() %>%
  arrange(desc(n)) 
# Total para br e uf - causa defeito na via
custoaci14 %>%
  group_by(br, uf) %>%
  filter(causa == 'Defeito na via') %>%
  summarise_each(funs(sum(., na.rm = T)), 
                 -c(causa, ocorrencia, regiao,classificacao,uf,br)) %>%
  select(br:morto,custo_pessoa,custo_veiculo,custo_total) %>%
  filter(custo_total > 1000000) %>%
  write.csv2('Material Final/2014/temp.csv')

#### Componentes de custo ####
fator <- 1.077392 #2007
fator <- 1.260507 #2010
fator <- 1.60127  #2014

custo <- dget("custo_padrao.R")
custo[ ,2:4] <-
  round(custo[ ,2:4] * fator, digits = 2)
componente <- dget("componente.R")

# Institucionais e danos à propriedades
sum(custoaci$custo_outro)/sum(custoaci$custo_total) * 100

data.frame(instituc_danospropried = sum(custoaci$custo_outro))

### Componentes pessoa
custo_lesao <- function(data, ano) {
  
  if(ano == 07) {
    fator <- 1.077392 #2007
  } else if(ano == 10) {
    fator <- 1.260507 #2010
  } else if(ano == 14) {
    fator <- 1.60127  #2014
  } else {
    stop('Escolha entre os anos 07, 10 e 14')
  }
  custo <- dget("custo_padrao.R")
  custo[ ,2:4] <-
    round(custo[ ,2:4] * fator, digits = 2)
  a <-
    data %>%
    group_by(classificacao) %>%
    filter(classificacao == 'sem vitimas') %>%
    summarise(ilesos = sum(ileso),
              custo_ile = custo[1,2] * ilesos,
              feridoleve = sum(feridoleve), 
              custo_ferleve = custo[2,2] * feridoleve,
              feridograve = sum(feridograve),
              custo_fergrave = custo[3,2] * feridograve,
              morto = sum(morto),
              custo_mort = custo[4,2] * morto,
              total = sum(custo_total))
  b <-
    data %>%
    group_by(classificacao) %>%
    filter(classificacao == 'com vitimas feridas') %>%
    summarise(ilesos = sum(ileso),
              custo_ile = custo[1,3] * ilesos,
              feridoleve = sum(feridoleve), 
              custo_ferleve = custo[2,3] * feridoleve,
              feridograve = sum(feridograve),
              custo_fergrave = custo[3,3] * feridograve,
              morto = sum(morto),
              custo_mort = custo[4,3] * morto,
              total = sum(custo_total))
  
  c <-
    data %>%
    group_by(classificacao) %>%
    filter(classificacao == 'com vitimas fatais') %>%
    summarise(ilesos = sum(ileso),
              custo_ile = custo[1,4] * ilesos,
              feridoleve = sum(feridoleve), 
              custo_ferleve = custo[2,4] * feridoleve,
              feridograve = sum(feridograve),
              custo_fergrave = custo[3,4] * feridograve,
              morto = sum(morto),
              custo_mort = custo[4,4] * morto,
              total = sum(custo_total))
  
  return(list(sem = a , comferido = b, comfatal = c))
  
}

quebrado <- 
  custo_lesao(custoaci, 14)
quebrado <- rbindlist(quebrado)


temp <- 
  componente$pessoa %>%
  select(sem_vitima:com_fatalidade)

pessoa <- 
  quebrado %>%
  summarise(perda_prod = 
              custo_mort[3] + 
              sum(temp[7, ] * custo_fergrave) +
              sum(temp[4, ] * custo_ferleve) +
              sum(temp[1, ] * custo_ile),
            outros_pessoa = 
              sum(temp[9, ] * custo_fergrave) +
              sum(temp[6, ] * custo_ferleve) +
              sum(temp[3, ] * custo_ile),
            hospitalar = 
              sum(temp[8, ] * custo_fergrave) +
              sum(temp[5, ] * custo_ferleve) +
              sum(temp[2, ] * custo_ile))


rm(temp)

# Escolher ano
componentes_df <-
  cbind(pessoa, 
        data.frame(instituc_danospropried = sum(custoaci$custo_outro)))

rm(pessoa, quebrado)
#### Componentes veiculos
custo_veic <- function(data, ano) {
  
  if(ano == 07) {
    fator <- 1.077392 #2007
  } else if(ano == 10) {
    fator <- 1.260507 #2010
  } else if(ano == 14) {
    fator <- 1.60127  #2014
  } else {
    stop('Escolha entre os anos 07, 10 e 14')
  }
  custo <- dget("custo_padrao.R")
  custo[ ,2:4] <-
    round(custo[ ,2:4] * fator, digits = 2)
  a <-
    data %>%
    group_by(classificacao) %>%
    filter(classificacao == 'sem vitimas') %>%
    summarise(automovel = sum(automovel),
              custo_auto = custo[5,2] * automovel,
              motocicleta = sum(motocicleta), 
              custo_moto = custo[6,2] * motocicleta,
              bicicleta = sum(bicicleta),
              custo_bike = custo[7,2] * bicicleta,
              utilitario = sum(utilitario),
              custo_util = custo[8,2] * utilitario,
              caminhao = sum(caminhao),
              custo_caminhao = custo[9,2] * caminhao,
              onibus = sum(onibus),
              custo_bus = custo[10,2] * onibus,
              outros = sum(outros),
              custo_outros = custo[11,2] * outros,
              total = sum(custo_total))
  b <-
    data %>%
    group_by(classificacao) %>%
    filter(classificacao == 'com vitimas feridas') %>%
    summarise(automovel = sum(automovel),
              custo_auto = custo[5,3] * automovel,
              motocicleta = sum(motocicleta), 
              custo_moto = custo[6,3] * motocicleta,
              bicicleta = sum(bicicleta),
              custo_bike = custo[7,3] * bicicleta,
              utilitario = sum(utilitario),
              custo_util = custo[8,3] * utilitario,
              caminhao = sum(caminhao),
              custo_caminhao = custo[9,3] * caminhao,
              onibus = sum(onibus),
              custo_bus = custo[10,3] * onibus,
              outros = sum(outros),
              custo_outros = custo[11,3] * outros,
              total = sum(custo_total))
  
  c <-
    data %>%
    group_by(classificacao) %>%
    filter(classificacao == 'com vitimas fatais') %>%
    summarise(automovel = sum(automovel),
              custo_auto = custo[5,4] * automovel,
              motocicleta = sum(motocicleta), 
              custo_moto = custo[6,4] * motocicleta,
              bicicleta = sum(bicicleta),
              custo_bike = custo[7,4] * bicicleta,
              utilitario = sum(utilitario),
              custo_util = custo[8,4] * utilitario,
              caminhao = sum(caminhao),
              custo_caminhao = custo[9,4] * caminhao,
              onibus = sum(onibus),
              custo_bus = custo[10,4] * onibus,
              outros = sum(outros),
              custo_outros = custo[11,4] * outros,
              total = sum(custo_total))
  
  return(list(sem = a , comferido = b, comfatal = c))
  
}

quebrado <- 
  custo_veic(custoaci, 14)
quebrado <- rbindlist(quebrado)


temp <- 
  componente$veiculo %>%
  select(sem_vitima:com_fatalidade)

veiculo <- 
  quebrado %>%
  summarise(danos_materiais = 
              sum(temp[1, ] * custo_auto) + 
              sum(temp[3, ] * custo_moto) +
              sum(temp[5, ] * custo_bike) +
              sum(temp[7, ] * custo_util) +
              sum(temp[9, ] * custo_caminhao) +
              sum(temp[11, ] * custo_bus) +
              sum(temp[13, ] * custo_outros),
            outros_veic = 
              sum(temp[2, ] * custo_auto) + 
              sum(temp[4, ] * custo_moto) +
              sum(temp[6, ] * custo_bike) +
              sum(temp[8, ] * custo_util) +
              sum(temp[10, ] * custo_caminhao) +
              sum(temp[12, ] * custo_bus) +
              sum(temp[14, ] * custo_outros))

rm(temp, quebrado)

componentes_df <-
  cbind(componentes_df,  veiculo)


rm(veiculo)


## Resultado
componentes_df %>%
  gather(componente, valor) %>%
  mutate(percentual = round(valor/sum(valor), 3)) %>%
  mutate(percentual2 = c(-percentual)/2) %>%
  mutate(cum_percent = cumsum(percentual)) %>%
  mutate(pos = cum_percent + percentual2) %>%
  mutate(percentual = scales::percent(percentual))  


componentes_df %>%
  gather(componente, valor) %>%
  mutate(percentual = round(valor/sum(valor), 3)) %>%
  mutate(percentual2 = c(-percentual)/2) %>%
  mutate(cum_percent = cumsum(percentual)) %>%
  mutate(pos = cum_percent + percentual2) %>%
 
  ggplot(data = ., aes(x = factor(1), y = percentual)) +
  geom_bar(aes(fill = factor(componente)), width = .5, 
           stat = 'identity')+
  geom_text(aes(y = pos,
                label = scales::percent(percentual),
                size = percentual)) + 
  scale_fill_brewer(palette = "Spectral", name = "Componente",
                    labels = c("Perda de Produção", 
                               "Outros associados às pessoas",
                               "Hospitalar", 
                               "Institucionais e danos a propriedades",
                               "Danos materiais - veículos",
                               "Outros associados aos veículos")) +
  scale_size(range = c(3,6), guide = 'none') +
  scale_x_discrete(labels = '', breaks = NULL) +
  ggtitle("Componentes de custo dos acidentes \n nas rodovias federais (2014)") +
  theme_minimal() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0)) +
  xlab('') +
  ylab('') 

pie(as.matrix(componentes_df), labels = names(componentes_df))


