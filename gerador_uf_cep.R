getwd()

setwd("Work/")

readLines("Lista_CNPJ_CPF_CEP.txt", 4)

galileo = read.table("Lista_CNPJ_CPF_CEP.txt", header = T, sep = "\t",
           quote = "", colClasses = rep("character",2))

summary(galileo)
names(galileo) = c("CPFCNPJ", "CEP")

# Nenhuma linha duplicada
sum(duplicated(galileo))

#### Gerador de UF a partir do CEP ####
galileo$cep3 = substr(galileo$CEP, start = 1, stop = 3)
galileo$UF = as.character(NA)

# Norte
galileo[grepl(x = galileo$cep3, pattern = "699"), "UF"] <- "AC"
galileo[grepl(x = galileo$cep3, pattern = "(6[6-7].)|(68[0-8])"), "UF"] <- "PA"
galileo[grepl(x = galileo$cep3, pattern = "689"), "UF"] <- "AP"
galileo[grepl(x = galileo$cep3, pattern = "69[0-8]"), "UF"] <- "AM"
galileo[grepl(x = galileo$cep3, pattern = "693"), "UF"] <- "RR"
galileo[grepl(x = galileo$cep3, pattern = "76[8-9]"), "UF"] <- "RO"

# Sudeste
galileo[grepl(x = galileo$cep3, pattern = "^[01]"), "UF"] <- "SP"
galileo[grepl(x = galileo$cep3, pattern = "^2[0-8]"), "UF"] <- "RJ"
galileo[grepl(x = galileo$cep3, pattern = "^29"), "UF"] <- "ES"
galileo[grepl(x = galileo$cep3, pattern = "3.."), "UF"] <- "MG"

# Nordeste
galileo[grepl(x = galileo$cep3, pattern = "4[0-8]."), "UF"] <- "BA"
galileo[grepl(x = galileo$cep3, pattern = "49."), "UF"] <- "SE"
galileo[grepl(x = galileo$cep3, pattern = "5[0-6]."), "UF"] <- "PE"
galileo[grepl(x = galileo$cep3, pattern = "57."), "UF"] <- "AL"
galileo[grepl(x = galileo$cep3, pattern = "58."), "UF"] <- "PB"
galileo[grepl(x = galileo$cep3, pattern = "59."), "UF"] <- "RN"
galileo[grepl(x = galileo$cep3, pattern = "6[0-3]."), "UF"] <- "CE"
galileo[grepl(x = galileo$cep3, pattern = "64."), "UF"] <- "PI"
galileo[grepl(x = galileo$cep3, pattern = "65."), "UF"] <- "MA"

# Centro-Oeste
galileo[grepl(x = galileo$cep3, pattern = "(7[0-2].)|(73[0-6])"), "UF"] <- "DF"
galileo[grepl(x = galileo$cep3, pattern = "(73[7-9])|(7[4-5].)|(76[0-7])"), "UF"] <- "GO"
galileo[grepl(x = galileo$cep3, pattern = "78[0-8]"), "UF"] <- "MT"
galileo[grepl(x = galileo$cep3, pattern = "79."), "UF"] <- "MS"
galileo[grepl(x = galileo$cep3, pattern = "77."), "UF"] <- "TO"

# Sul
galileo[grepl(x = galileo$cep3, pattern = "8[0-7]."), "UF"] <- "PR"
galileo[grepl(x = galileo$cep3, pattern = "8[8-9]."), "UF"] <- "SC"
galileo[grepl(x = galileo$cep3, pattern = "^9"), "UF"] <- "RS"


table(galileo$UF)
galileo[is.na(galileo$UF), ]

galileo$cep3 <- NULL

### Exportando galileo ####
write.table(galileo, "geocodificar.csv", sep = ";", quote = F, row.names = F)

### Lendo do galileo ####

lista = read.table("lista9julho.csv", header = T, quote = "", sep = ";")
