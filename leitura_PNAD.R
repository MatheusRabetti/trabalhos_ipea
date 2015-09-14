getwd()
rm(list=ls())
dev.off()
gc()
setwd("C:/Users/b2562360/Desktop/Indicadores")

####### Pacotes #####


library(bit64)
library(data.table)
library(descr)
library(reshape2)
library(survey)
library(sqldf)
library(sas7bdat)

##### Dicion?rios ######
## Criando o dicion?rio a partir das tr?s primeiras colunas da planilha 
dicdom96 <- read.csv2(file = 'Dados Brutos/Dicion?rios/dicdom96.csv', header=F)
dicdom96 <- dicdom96[complete.cases(dicdom96),]
colnames(dicdom96) <- c('inicio', 'tamanho', 'variavel')

dicpes96 <- read.csv2(file = 'Dados Brutos/Dicion?rios/dicpes96.csv', header=F)
dicpes96 <- dicpes96[,-4]
dicpes96 <- dicpes96[complete.cases(dicpes96),]
colnames(dicpes96) <- c('inicio', 'tamanho', 'variavel')

dicdom2006 <- read.csv2(file = 'Dados Brutos/Dicion?rios/dicdom2006.csv', header=F)
dicdom2006 <- dicdom2006[complete.cases(dicdom2006),]
colnames(dicdom2006) <- c('inicio', 'tamanho', 'variavel')

dicpes2006 <- read.csv2(file = 'Dados Brutos/Dicion?rios/dicpes2006.csv', header=F)
dicpes2006 <- dicpes2006[complete.cases(dicpes2006),]
colnames(dicpes2006) <- c('inicio', 'tamanho', 'variavel')

dicdom2013=dicdom2013[,-4]
dicpes2013=dicpes2013[,-4]
colnames(dicdom2013) <- c('inicio', 'variavel','tamanho')
colnames(dicpes2013) <- c('inicio', 'variavel','tamanho')

## Par?metro com o final de cada campo
end_dom96 = dicdom96$inicio + dicdom96$tamanho - 1
end_pes96 = dicpes96$inicio + dicpes96$tamanho - 1

end_dom2006 = dicdom2006$inicio + dicdom2006$tamanho - 1
end_pes2006 = dicpes2006$inicio + dicpes2006$tamanho - 1

end_dom2013 = dicdom2013$inicio + dicdom2013$tamanho - 1
end_pes2013 = dicpes2013$inicio + dicpes2013$tamanho - 1

##### Convers?o dos bancos para csv #####
## Converte o microdado para um arquivo csv
fwf2csv(fwffile='Dados Brutos/D96BR', csvfile='dadosdom96.csv',
        names=dicdom96$variavel, begin=dicdom96$inicio, end=end_dom96)
fwf2csv(fwffile='Dados Brutos/P96BR', csvfile='dadospes96.csv',
        names=dicpes96$variavel, begin=dicpes96$inicio, end=end_pes96)

fwf2csv(fwffile='Dados Brutos/DOM2006.txt', csvfile='dadosdom2006.csv',
        names=dicdom2006$variavel, begin=dicdom2006$inicio, end=end_dom2006)
fwf2csv(fwffile='Dados Brutos/PES2006.txt', csvfile='dadospes2006.csv',
        names=dicpes2006$variavel, begin=dicpes2006$inicio, end=end_pes2006)

fwf2csv(fwffile='Dados Brutos/DOM2013.txt', csvfile='dadosdom2013.csv',
        names=dicdom2013$variavel, begin=dicdom2013$inicio, end=end_dom2013)
fwf2csv(fwffile='Dados Brutos/PES2013.txt', csvfile='dadospes2013.csv',
        names=dicpes2013$variavel, begin=dicpes2013$inicio, end=end_pes2013)

rm(list=ls(pattern="dic.*"))
rm(end_dom96,end_pes96)

#### Leitura Ano de 1996####
dadosdom <- fread(input='dadosdom96.csv', sep='auto', 
                  sep2='auto', integer64='double',
                  select = c("V 0102","V 0103","UF","V 4107","V 4611","V 4618","V 4617"))
names(dadosdom)<-gsub(names(dadosdom),pattern = "\\s",replacement = "")

# Incluir as variaveis psu e strat
amost96 = read.csv2("amost96.csv",colClasses=c("NULL",rep(NA,5)))
dadosdom = merge(x = amost96,y = dadosdom,by = c("V0102","V0103","UF"))
rm(amost96)

dadospes <- fread(input='dadospes96.csv', sep='auto', 
                  sep2='auto', integer64='double',
                  select = c("V 0102","V 0103","UF","V 0302","V 0404", # constantes
                             "V 9057"))

names(dadospes)<-gsub(names(dadospes),pattern = "\\s",replacement = "")

##### Leitura Anos de 2006 e 2013 ####
dadosdom <- fread(input='dadosdom2006.csv', sep='auto', 
                  sep2='auto', integer64='double',
                  select = c("V0102","V0103","UF","V4107","V4611","V4618","V4617")) # renda familia

dadospes <- fread(input='dadospes2006.csv', sep='auto', 
                  sep2='auto', integer64='double',
                  select = c("V0102","V0103","UF","V0302","V0404", # constantes
                             "V8005",
                             "V9001","V9002","V9003","V9004"))  # informalidade ,renda pess

#### Merge, cria?ao das var regiao e one e criacao da amostragem complexa ####
banco = merge(dadosdom,dadospes,by=c("V0102","V0103","UF"))

rm(dadospes,dadosdom)

options(survey.lonely.psu = "adjust")

# Adicionar uma coluna com apenas uns
banco$one = 1

##Adicionar regi?es ##
#Regiao Norte
temp = subset(banco, subset = grepl(banco$UF,pattern = "^1"))
temp$regiao = "Norte"
#Regiao Nordeste
temp2 = subset(banco, subset = grepl(banco$UF,pattern = "^2"))
temp2$regiao = "Nordeste"

temp = rbind(temp,temp2)
#Regiao Sudeste
temp2 = subset(banco, subset = grepl(banco$UF,pattern = "^3"))
temp2$regiao = "Sudeste"

temp = rbind(temp,temp2)
#Regiao Sul
temp2 = subset(banco, subset = grepl(banco$UF,pattern = "^4"))
temp2$regiao = "Sul"

temp = rbind(temp,temp2)
#Regiao Centro-Oeste
temp2 = subset(banco, subset = grepl(banco$UF,pattern = "^5"))
temp2$regiao = "Centro-Oeste"

temp = rbind(temp,temp2)

banco = temp
rm(temp,temp2)


#1996
sample.pnad <-
  svydesign(
    id = ~psu ,
    strata = ~strat ,
    data = banco ,
    weights = ~V4611 ,
    nest = TRUE ) 

#2013 e 2006
sample.pnad <-
  svydesign(
    id = ~V4618 ,
    strata = ~V4617,
    data = banco ,
    weights = ~V4611 ,
    nest = TRUE ) 

options(survey.lonely.psu = "adjust")

# Regi?o Metropolitana
regmet = subset(sample.pnad, subset = V4107 == 1)

porc=NULL
saida = function(original,selecao){
  # Brasil 
  a = svytotal(
    ~one,
    design = original)
  b = svytotal(
    ~one,
    design = selecao)
  
  porc[1] = 100* b[[1]]/a[[1]] ; porc
  print("Brasil")
  # Por Regiao 
  a = svyby(
    ~one,
    ~regiao,
    original,
    svytotal
  )
  b = svyby(
    ~one,
    ~regiao,
    selecao,
    svytotal
  )
  porc[2:6] = b[,2] *100/ a[,2] ;porc
  print("Regiao")
  # Por sexo - 4 ? feminino / 2 ? masculino
  a = svyby(
    ~one,
    ~V0302,
    original,
    svytotal
  )
  b = svyby(
    ~one,
    ~V0302,
    selecao,
    svytotal
  )
  porc[7] = b[2,2] *100/ a[2,2] ;porc
  print("Mulher")
  # Por ra?a - 4 e 8 negros
  a = svyby(
    ~one,
    ~V0404,
    original,
    svytotal
  )
  b = svyby(
    ~one,
    ~V0404,
    selecao,
    svytotal
  )
  porc[8] = (b[2,2]+b[5,2]) *100/ (a[2,2]+a[5,2]) ;porc
  print("Negro")
  
  assign(x = "porc",value = porc,envir = .GlobalEnv)
  porc = as.numeric(porc[1:8])
  write.csv2(porc,"output.csv")
  
  return(porc)
}
saida_media = function(selecao){
  # Brasil 
  a = svyby(
    ~V4720,
    ~V0302,
    selecao,
    svymean
  )
  porc[1]= a[1,2]/a[2,2]
  print("Brasil")
  
  # Por Regiao 
  a = svyby(
    ~V4720,
    ~sexreg,
    selecao,
    svymean
  )
  for(i in 1:5) porc[i+1] = a[i,2]/a[i+5,2]
  
  print("Regiao")
  # Por sexo - 4 ? feminino / 2 ? masculino
  a = svyby(
    ~V4720,
    ~sexnegro,
    selecao,
    svymean
  )
  
  porc[7] = (a[2,2]+a[5,2])/(a[7,2]+a[10,2])
  print("Negro")
  
  assign(x = "porc",value = porc,envir = .GlobalEnv)
  porc = as.numeric(porc[1:7])
  write.csv2(porc,"output.csv")
  
  return(porc)
}
saida_percent = function(selecao){
  # Brasil 
  a = svyby(
    ~one,
    ~V0302,
    selecao,
    svytotal
  )
  
  porc[1]= a[2,2] *100/ (a[2,2]+a[1,2])
  print("Brasil")
  
  # Por Regiao 
  a = svyby(
    ~one,
    ~sexreg,
    selecao,
    svytotal
  )
  for(i in 1:5) porc[i+1] = a[i+5,2] *100 / (a[i,2]+a[i+5,2])
  print(a)
  
  print("Regiao")
  # Por sexo - 4 ? feminino / 2 ? masculino
  a = svyby(
    ~one,
    ~sexnegro,
    selecao,
    svytotal
  )
  porc[7] = (a[7,2]+a[10,2])*100/(a[7,2]+a[10,2]+a[2,2]+a[5,2])
  print("Negro")
  
  assign(x = "porc",value = porc,envir = .GlobalEnv)
  porc = as.numeric(porc[1:7])
  write.csv2(porc,"output.csv")
  
  return(porc)
}



