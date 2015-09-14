setwd("K:/Matheus/centro_eco/")
library(data.table)

rm(list = ls())
gc()

# Bancos de estab geocodificados
rais13.estab = readRDS(file = "rais13_kernel.rds")
# saveRDS(rais$estab2013, file = 'rais13_kernel.rds')
rais02.estab = readRDS(file = "rais02_kernel.rds")
# saveRDS(rais$estab2002, file = 'rais02_kernel.rds')
rais <- list(estab2013 = rais13.estab, 
             estab2002 = rais02.estab)
rm(rais02.estab, rais13.estab)

library(foreign)

# Vanessa e Bruna
rais13.estab = read.csv("rais13_kernel.csv")
rais02.estab = read.csv("rais02_kernel.csv")

# write.csv(rais$estab2013, "rais13_kernel.csv", row.names = F)
# write.csv(rais$estab2002, "rais02_kernel.csv", row.names = F)
# write.dta(rais$estab2013, "rais13_kernel.dta")
# write.dta(rais$estab2002, "rais02_kernel.dta")

#### Banco de dados RM ####
readLines("mun_RMs_2010.csv",5)
RM_codmun = read.table("mun_RMs_2010.csv", header = T, sep = ";", 
                       quote = "", colClasses = rep("character",3))
RM_codmun$X = NULL
RM_codmun$nome_mun = NULL
RM_codmun$Cod_municipio = substr(x = RM_codmun$Cod_municipio, start = 1, stop = 6)
names(RM_codmun) = c("codemun", "RM")
RM_codmun$codemun = as.integer(RM_codmun$codemun)
# merge dos dois bancos
rais13.estab = merge(x = rais13.estab, y = RM_codmun, by = "codemun", all.x = T)
rais10.estab = merge(x = rais10.estab, y = RM_codmun, by = "codemun", all.x = T)
rais02.estab = merge(x = rais02.estab, y = RM_codmun, by = "codemun", all.x = T)
rais06.estab = merge(x = rais06.estab, y = RM_codmun, by = "codemun", all.x = T)

#### Leitura do shape da ACP #####
library(rgdal)
library(spatstat)

shape.acp = readOGR("Areas_urbanizadas_do_Brasil_2005_shapes", "AreasUrbanizadas_MunicipiosACP_porMunicipio")

## Apagando acentos das ACP para evitar problemas com o readOGR
library(foreign)
acp <-
  read.dbf("Areas_urbanizadas_do_Brasil_2005_shapes/AreasUrbanizadas_MunicipiosACP_porMunicipio.dbf")
acp$ACP <- iconv(acp$ACP, to='ASCII//TRANSLIT')
write.dbf(acp, "Areas_urbanizadas_do_Brasil_2005_shapes/AreasUrbanizadas_MunicipiosACP_porMunicipio.dbf")
####

summary(shape.acp@data)
acp.BH = shape.acp[shape.acp$ACP == "Belo Horizonte", ]
plot(acp.BH, axes=TRUE, border="gray")

estab = subset(rais13.estab,
               subset = RM == "RM Belo Horizonte")
estab = estab[estab$PrecisionDepth != "1 Estrela", ]
estab = estab[!is.na(estab$Longitude), ]
estab = estab[estab$ind_rais_neg == 0, ]

estab = as.data.frame(estab)
coordinates(estab) = c("Longitude","Latitude")
proj4string(estab) = proj4string(acp.BH)
insideBH = !is.na(over(estab, as(acp.BH, "SpatialPolygons")))

mean(insideBH) # 98% dos pontos estão dentro os poligonos
raisBH = estab[insideBH, ]@data
raisBH$Longitude = coordinates(estab[insideBH, ])[ ,1]
raisBH$Latitude = coordinates(estab[insideBH, ])[ ,2]

#### Reduzindo a base de dados ####

# RM - NA
keep.vars = c("id","RM","qtd_vinc_ativos",
              "PrecisionDepth","Latitude","Longitude",
              "ind_rais_neg","nomemun","uf", "clas_cnae10")
rais02.estab = rais02.estab[!is.na(rais02.estab$RM), ]
rais02.estab = subset(rais02.estab, select = keep.vars)

rais06.estab = rais06.estab[!is.na(rais06.estab$RM), ]
rais06.estab = subset(rais06.estab, select = keep.vars)

rais10.estab = rais10.estab[!is.na(rais10.estab$RM), ]
rais10.estab = subset(rais10.estab, select = keep.vars)

rais13.estab = subset(rais.estab, select = keep.vars)
rais13.estab = rais13.estab[!is.na(rais13.estab$RM), ]


# Rais negativa = 0 

rais13.estab = rais13.estab[rais13.estab$ind_rais_neg == 0, ]

# Longitude  - NA

rais13.estab = rais13.estab[!is.na(rais13.estab$Longitude), ]
rais10.estab = rais10.estab[!is.na(rais10.estab$Longitude), ]
rais06.estab = rais06.estab[!is.na(rais06.estab$Longitude), ]
rais02.estab = rais02.estab[!is.na(rais02.estab$Longitude), ]

# Precisao = 1

rais13.estab = rais13.estab[rais13.estab$PrecisionDepth != "1 Estrela", ]
rais10.estab = rais10.estab[rais10.estab$PrecisionDepth != "1 Estrela", ]
rais06.estab = rais06.estab[rais06.estab$PrecisionDepth != "1 Estrela", ]
rais02.estab = rais02.estab[rais02.estab$PrecisionDepth != "1 Estrela", ]

saveRDS(rais02.estab, "rais02_kernel.rds")
saveRDS(rais06.estab, "rais06_kernel.rds")
saveRDS(rais10.estab, "rais10_kernel.rds")
saveRDS(rais13.estab, "rais13_kernel.rds")

#### Trazendo variáveis do banco ####
readLines('../galileo_geocoding/rais13_estab_geocoded.txt', 1)
readLines('../galileo_geocoding/rais02_estab_geocoded.txt', 1)

cnae13 = fread('../galileo_geocoding/rais13_estab_geocoded.txt', sep = "\t",
               select = c('id','cnpj_raiz','nome_logradouro','razao_social','cep'))
cnae13 = tbl_df(cnae13)

cnae02 = fread('../galileo_geocoding/rais02_estab_geocoded.txt', sep = "\t",
               select = c('EstabID', 'clas_cnae10'))
cnae02 = tbl_df(cnae02)


rais13.estab = left_join(rais13.estab, y = cnae13, by = 'id')
rais$estab2002 = left_join(rais$estab2002, y = cnae02, by = 'EstabID')
rm(cnae02)

#saveRDS(rais$estab2002, "rais02_kernel.rds")

#### Identificando problema no georeferenciamento ####


rais$estab2013 %>%
  filter(uf == 'RJ') %>% 
  group_by(NM_MUNICIP) %>% 
  summarise(tab = n(),
            vinculos = sum(qtd_vinc_ativos))


rais$estab2013 %>%
  filter(uf == 'MG') %>% 
  group_by(NM_MUNICIP) %>% 
  summarise(tab = n(),
            vinculos = sum(qtd_vinc_ativos)) %>% 
  data.frame(.)

####...

rais$estab2002 %>%
  filter(RM == 'RM Rio de Janeiro') %>% 
  group_by(NM_MUNICIP) %>% 
  summarise(tab = n(),
            vinculos = sum(qtd_vinc_ativos))

rais$estab2002 %>%
  filter(uf == 'MG') %>% 
  group_by(NM_MUNICIP) %>% 
  summarise(tab = n(),
            vinculos = sum(qtd_vinc_ativos)) %>% 
  data.frame(.)

names(rais$estab2013)

#### Empregos outliers - inputação ####

sum(rais$estab2002$qtd_vinc_ativos)
#12,93 milhões
sum(rais02.estab$vinc_ativos_corrigido)
#7,91 milhões
sum(rais$estab2013$qtd_vinc_ativos)
#20,99 milhões
sum(rais13.estab$vinc_ativos_corrigido)
#13,74 milhões
sum(rais$estab2013$vinc_ativos_corrigido)
sum(rais$estab2002$vinc_ativos_corrigido)


# Nova variável com dois primeiros dígitos da CNAE
rais13.estab <- 
  rais$estab2013 %>% 
  mutate(cnae = substring(clas_cnae10, 1, 2))
rais02.estab <- 
  rais$estab2002 %>% 
  mutate(cnae = substring(clas_cnae10, 1, 2))


rais13.estab %>% 
  filter(cnae == 75) %>% 
  summarise(soma = sum(qtd_vinc_ativos))
#3,87 milhões
rais13.estab %>% 
  filter(cnae == 75) %>% 
  summarise(soma = sum(vinc_ativos_corrigido))
# 0
rais02.estab %>% 
  filter(cnae == 75) %>% 
  summarise(soma = sum(qtd_vinc_ativos))
#3,09 milhões
rais02.estab %>% 
  filter(cnae == 75) %>% 
  summarise(soma = sum(vinc_ativos_corrigido))
#0

## Inputando pela mediana

`%ni%` <- Negate(`%in%`) #not in

# A busca grep
paste0('^',c(75,74,40,41,60,90,62103),'|', collapse = '')

#2002
adm.pub <-
  grep('^75', x = rais$estab2002$clas_cnae10)

cnae_xt <- 
  grep(pattern = 
         paste0(
           paste0('^',c(74,40,41,60,90),'|', collapse = ''), '^',
           62103),
       x = rais$estab2002$clas_cnae10)
#2013
adm.pub <-
  grep('^75', x = rais$estab2013$clas_cnae10)

cnae_xt <- 
  grep(pattern = 
         paste0(
           paste0('^',c(74,40,41,60,90),'|', collapse = ''), '^',
           62103),
       x = rais$estab2013$clas_cnae10)

# Nova variável com valores de inputação pela mediana dos 2 primeiros digitos cnae
rais13.estab <- 
  rais13.estab %>% 
  group_by(cnae) %>% 
  mutate(input = as.numeric(median(qtd_vinc_ativos))) 


rais02.estab <- 
  rais02.estab %>% 
  group_by(cnae) %>% 
  mutate(input = as.numeric(median(qtd_vinc_ativos))) 


# Inputando os valores extremos
rais13.estab %<>% 
  add_rownames('rowname') %>% 
  mutate(vinc_ativos_corrigido = 
           ifelse(rowname %in% cnae_xt, input, qtd_vinc_ativos)) %>% 
  mutate(vinc_ativos_corrigido = 
           ifelse(rowname %in% adm.pub, 0, vinc_ativos_corrigido))
rais13.estab$rowname <- NULL

rais02.estab %<>% 
  add_rownames('rowname') %>% 
  mutate(vinc_ativos_corrigido = 
           ifelse(rowname %in% cnae_xt, input, qtd_vinc_ativos)) %>% 
  mutate(vinc_ativos_corrigido = 
           ifelse(rowname %in% adm.pub, 0, vinc_ativos_corrigido))
rais02.estab$rowname <- NULL


#
rais$estab2013 <- left_join(rais$estab2013, 
                            select(rais13.estab, c(id, vinc_ativos_corrigido)), 
                            by = 'id')
rais$estab2002 <- left_join(rais$estab2002, 
                            select(rais02.estab, c(EstabID, vinc_ativos_corrigido)), 
                            by = 'EstabID')
rm(adm.pub, cnae_xt, rais02.estab, rais13.estab)

################################### ###

## Cnae
rais13.estab %>% 
  group_by(clas_cnae10) %>%
  summarise(quantidade = n(),
            media = mean(qtd_vinc_ativos),
            desvio = sd(qtd_vinc_ativos),
            q1 = quantile(qtd_vinc_ativos, 0.25),
            med = quantile(qtd_vinc_ativos, 0.5),
            q3 = quantile(qtd_vinc_ativos, 0.75),
            q90 = quantile(qtd_vinc_ativos, 0.9),
            q95 = quantile(qtd_vinc_ativos, 0.95),
            maxi = max(qtd_vinc_ativos),
            corte = med + 3 * desvio) %>% 
  arrange(desc(med, maxi)) %>% 
  View

rais13.estab %>% 
  mutate(cnae = substring(clas_cnae10, 1, 2)) %>% 
  group_by(cnae) %>%
  summarise(quantidade = n(),
            q1 = quantile(qtd_vinc_ativos, 0.25),
            med = quantile(qtd_vinc_ativos, 0.5),
            q3 = quantile(qtd_vinc_ativos, 0.75),
            q95 = quantile(qtd_vinc_ativos, 0.95),
            maxi = max(qtd_vinc_ativos)) %>% 
  arrange(desc(med, maxi)) %>% 
  View


# # Exportando para Vanessa os maiores valores e empregos
# rais13.estab %>% 
#   filter(qtd_vinc_ativos > 3440) %>% 
#   select(id, qtd_vinc_ativos, razao_social, nome_logradouro, 
#          NM_MUNICIP, clas_cnae10) %>% 
#   arrange(desc(qtd_vinc_ativos)) %>% 
#   write.csv(., 'maioresvinculos.csv')


# Observar inputs
rais13.noxtrm %>% 
  ungroup() %>% 
  select(clas_cnae10, input) %>% 
  arrange(desc(input)) 

rais02.noxtrm %>% 
  ungroup() %>% 
  dplyr::select(clas_cnae10, input) %>% 
  arrange(desc(input)) 
#60 e 75

# Observar mudancas
quantile(rais13.noxtrm$qtd_vinc_ativos, seq(0, 1, by = 0.05))
quantile(rais13.noxtrm$qtd_vinc_ativos, seq(.95, 1, by = 0.005))
quantile(rais13.noxtrm$qtd_vinc_ativos, seq(.99, 1, by = 0.001))

quantile(rais13.estab$qtd_vinc_ativos, seq(0, 1, by = 0.05))
quantile(rais13.estab$qtd_vinc_ativos, seq(.95, 1, by = 0.005))
quantile(rais13.estab$qtd_vinc_ativos, seq(.99, 1, by = 0.001))


### RM

rais13.noxtrm %>% 
  group_by(RM) %>%
  summarise(quantidade = n(),
            media = mean(qtd_vinc_ativos),
            q1 = quantile(qtd_vinc_ativos, 0.25),
            med = quantile(qtd_vinc_ativos, 0.5),
            q3 = quantile(qtd_vinc_ativos, 0.75),
            q90 = quantile(qtd_vinc_ativos, 0.9),
            q95 = quantile(qtd_vinc_ativos, 0.95),
            maxi = max(qtd_vinc_ativos)) %>% 
  arrange(desc(maxi, q3)) %>% 
  View


rais13.noxtrm = tbl_df(rais13.noxtrm)
rais13.noxtrm %>% 
  filter(RM == 'RM Rio de Janeiro') %>% 
  arrange(desc(qtd_vinc_ativos)) %>% 
  dplyr::select(qtd_vinc_ativos, razao_social, clas_cnae10) %>% 
  View

#### Empregos normalizados ####


rais$estab2013 <-
  rais$estab2013 %>% 
  mutate(empregosnorm = (qtd_vinc_ativos - min(qtd_vinc_ativos)) / 
           (max(qtd_vinc_ativos) - min(qtd_vinc_ativos))
  )

  
rais$estab2002 <-
  rais$estab2002 %>% 
  mutate(empregosnorm = (qtd_vinc_ativos - min(qtd_vinc_ativos)) / 
           (max(qtd_vinc_ativos) - min(qtd_vinc_ativos))
  )

#### Centro Vanessa - Bruna ####

centro12 = readOGR(dsn = "K:/Matheus/shape centros", 
                   layer = "Centros_12_RMs")
#2002
coordinates(rais$estab2002) = c("Longitude", "Latitude")
proj4string(rais$estab2002) = proj4string(centro12)

insidecentro = !is.na(over(rais$estab2002, as(centro12, "SpatialPolygons")))
mean(insidecentro) # 9,6% dos pontos estão dentro do centro

rais$estab2002 = as.data.frame(rais$estab2002)
rais$estab2002$in_centro = as.integer(as.vector(insidecentro))

#2003
rais$estab2013 <- as.data.frame(rais$estab2013)
coordinates(rais$estab2013) = c("Longitude", "Latitude")
proj4string(rais$estab2013) = proj4string(centro12)

insidecentro = !is.na(over(rais$estab2013, as(centro12, "SpatialPolygons")))
mean(insidecentro) # 7,8% dos pontos estão dentro do centro

rais$estab2013 = as.data.frame(rais$estab2013)
rais$estab2013$in_centro = as.integer(as.vector(insidecentro))

rm(insidecentro)

#### Centros lat e lon - google maps ####
readLines('clipboard', 3)
google.centro <- 
  read.table('clipboard', header = T, sep = "\t", 
             dec = ",", stringsAsFactors = F)

# dput(google.centro, 'google_centro.R')


### Análise  #####

library(KernSmooth)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(rgdal)
library(maptools)
library(spatstat)
library(dplyr)
library(scales)

shape.acp = readOGR("Areas_urbanizadas_do_Brasil_2005_shapes", 
                    "AreasUrbanizadas_MunicipiosACP_porMunicipio")

kernel.density = function(data, regiaometro, acp,
                          opcao = c("emprego","estab")){

  estab.ativo <- 
    data %>% 
    filter(RM == regiaometro) %>% 
    filter(PrecisionDepth != "1 Estrela") %>% 
    filter(!is.na(Longitude)) %>% 
    filter(ind_rais_neg == 0) 
  
  estab.ativo <- as.data.frame(estab.ativo)
    
  
  # Removendo outliers - estabelecimentos dentro da RM fora da ACP
  acp.rm = shape.acp[shape.acp$ACP == acp, ]
  coordinates(estab.ativo) = c("Longitude","Latitude")
  proj4string(estab.ativo) = proj4string(acp.rm)
  insiderm = !is.na(over(estab.ativo, as(acp.rm, "SpatialPolygons")))
  
  print(paste("Média de estabelecimentos dentro da ACP:", mean(insiderm)))
  temp = estab.ativo[insiderm, ]@data
  temp$Longitude = coordinates(estab.ativo[insiderm, ])[ ,1]
  temp$Latitude = coordinates(estab.ativo[insiderm, ])[ ,2]
  
  
  estab.ativo = temp
  
  # Calculando bandwith
  bandw = c(dpik(estab.ativo$Longitude),
            dpik(estab.ativo$Latitude))
  
  
  
  lat_range <- range(estab.ativo$Latitude)
  lon_range <- range(estab.ativo$Longitude)
  
  
  print("Calculando a densidade")
  if(opcao == "emprego") {
    
    # Retirando outliers de emprego
    adm.pub <-
      grep('^75', x = estab.ativo$clas_cnae10)
    
    cnae_xt <- 
      grep(pattern = 
             paste0(
               paste0('^',c(74,40,41,60,90),'|', collapse = ''), '^',
               62103),
           x = estab.ativo$clas_cnae10)
    
    estab.ativo <- 
      estab.ativo %>% 
      mutate(cnae = substring(clas_cnae10, 1, 2))
    
    
    estab.noxtrm <- 
      estab.ativo %>% 
      group_by(cnae) %>% 
      mutate(input = as.numeric(median(qtd_vinc_ativos))) 
    
    
    estab.noxtrm %<>% 
      add_rownames('rowname') %>% 
      mutate(qtd_vinc_ativos = 
               ifelse(rowname %in% cnae_xt, input, qtd_vinc_ativos)) %>% 
      mutate(qtd_vinc_ativos = 
               ifelse(rowname %in% adm.pub, 0, qtd_vinc_ativos))
    estab.noxtrm$rowname <- NULL
    
    # Repetindo a base pelo numero de empregos 
    df.expanded = 
      estab.noxtrm[rep(seq_len(nrow(estab.noxtrm)), estab.noxtrm$qtd_vinc_ativos), ]
    coord = data.frame(Longitude = df.expanded$Longitude,
                       Latitude = df.expanded$Latitude)
    # Calculando
    dens = bkde2D(x = coord, band = bandw,
                  gridsize = c(2400, 2400),
                  range.x = list(lon_range,lat_range))
      
  } else if(opcao == "estab"){
    
    coord = data.frame(Longitude = estab.ativo$Longitude,
                       Latitude = estab.ativo$Latitude)
    dens = bkde2D(x = coord, band = bandw,
                  gridsize = c(2400, 2400),
                  range.x = list(lon_range,lat_range))
    
  }
  
  print("Finalizado")
  
  kde_df <- expand.grid(
    lon = seq.int(lon_range[1], lon_range[2], length.out = 2400),
    lat = seq.int(lat_range[1], lat_range[2], length.out = 2400)
  )
  
  
  
  
  kde_df$density <- melt(dens$fhat)$value
  high = mean(kde_df$density) + 3 * sd(kde_df$density)
  # guardando informacoes densidade
  den_fill_scale <- scale_fill_gradient2(low = 'white', mid = 'darkgreen', 
                                           high = 'red', midpoint = high)
  den_fill_scale$range$train(kde_df$density)
  kde_df$density_s <- ggplot2:::scale_map(den_fill_scale,kde_df$density)
  kde_df$density_zeroone <- pmin(kde_df$density / max(kde_df$density), .9)
  
  # Descobrindo o ano
  if(names(data)[1] == 'EstabID') ano <- "02" else ano <- "13"
  
  return(list(kernel = kde_df, 
              ACP = acp,
              peso = opcao,
              ano = ano))
}


table(shape.acp@data$ACP)
table(rais10.estab$RM)



kde_st13 <- 
  kernel.density(data = rais$estab2002, regiaometro = "RM Rio de Janeiro",
                 acp = "Rio de Janeiro", opcao = "estab")

kde_em13 <- 
  kernel.density(data = rais$estab2002, regiaometro = "RM Rio de Janeiro",
                 acp = "Rio de Janeiro", opcao = "emprego")


kde_st13 <- 
  kernel.density(data = rais$estab2013, regiaometro = "RM Rio de Janeiro",
                 acp = "Rio de Janeiro", opcao = "estab")

kde_em13 <- 
  kernel.density(data = rais$estab2013, regiaometro = "RM Rio de Janeiro",
                 acp = "Rio de Janeiro", opcao = "emprego")

##### Gerando polígono da ACP e seu mapa de calor#####
kernel.poly = function(kdedata, acp, tit) {
  
  acp.rm = shape.acp[shape.acp$ACP == acp, ]
  coordinates(kdedata) = c("lon","lat")
  proj4string(kdedata) = proj4string(acp.rm)
  insiderm = !is.na(over(kdedata, as(acp.rm, "SpatialPolygons")))
  
  temp = kdedata[insiderm, ]@data
  temp$lon = coordinates(kdedata[insiderm, ])[ ,1]
  temp$lat = coordinates(kdedata[insiderm, ])[ ,2]
  
  
  kdedata = temp
  
  ggplot(data = kdedata) +
    geom_tile(aes(x = lon, y = lat, fill = density)) +
    scale_fill_distiller(palette = "YlOrBr", breaks = pretty_breaks(n = 10)) +
    labs(fill = '', x = '', y = '',
         title = paste(tit[2], 'para a RM de', tit[1],
                       sep = ' ')) +
    theme_map() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(family = "sans", color="#666666", 
                                    face="bold", size=18)) +
    coord_equal()
}

kernel.poly(kde_em13$kernel, 'Rio de Janeiro', tit = c('Rio de Janeiro', 'Empregos'))
kernel.poly(kde_st13$kernel, 'Rio de Janeiro', tit = c('Rio de Janeiro', 'Estabelecimentos'))

##### Gerando o mapa de calor com o google maps por trás - zoom melhor #####
kernel.graph = function(data, lupa){
  
  
  lat_range = range(data$lat)
  lon_range = range(data$lon)
  
  high = mean(data$density) + 3 * sd(data$density)
  highden = sapply(data[data$density  > high, c("lon", "lat")], median)
  
  
  assign("map.rm", get_map(location = c(lon = highden[1], 
                                        lat = highden[2]), 
                           source = "google", maptype = 'roadmap', 
                           messaging = F, color = "bw", zoom = lupa),
         envir = .GlobalEnv)
  
  
  
  
  ggmap(map.rm) +
    geom_tile(aes(x = lon, y = lat, fill = density_s, alpha = density_zeroone), 
              data = data) +
    scale_alpha(range = c(0, .7)) +
    scale_fill_identity() +
    labs(fill = '') +
    theme_nothing() +
    coord_equal()
  
}


kernel.graph(kde_st13, 11)
kernel.graph(kde_em13, 11)

##### Exportar rastter #####
library(raster)


r <- rasterFromXYZ(kde_st13$kernel[ ,1:3], 
              crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

writeRaster(r, filename="sp13_estab.tif", format="GTiff", overwrite=TRUE)

r <- rasterFromXYZ(kde_em13[ ,1:3], 
                   crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
writeRaster(r, filename="sp13_emprego.tif", format="GTiff", overwrite=TRUE)

##### Otimizando o calculo dos centros ######

gerador_centro = function(data, regiaometro, acp,
                          opcao = c("emprego","estab")){
  
  
  # Descobrindo o ano
  if(names(data)[1] == 'EstabID') ano <- "02" else ano <- "13"
  
  print(paste0("Ano de 20",ano))
  
  
  estab.ativo <- 
    data %>% 
    filter(RM == regiaometro) %>% 
    filter(PrecisionDepth != "1 Estrela") %>% 
    filter(!is.na(Longitude)) %>% 
    filter(ind_rais_neg == 0) 
  
  estab.ativo <- as.data.frame(estab.ativo)
  
  
  # Removendo outliers - estabelecimentos dentro da RM fora da ACP
  acp.rm = shape.acp[shape.acp$ACP == acp, ]
  coordinates(estab.ativo) = c("Longitude","Latitude")
  proj4string(estab.ativo) = proj4string(acp.rm)
  insiderm = !is.na(over(estab.ativo, as(acp.rm, "SpatialPolygons")))
  
  print(paste("Média de estabelecimentos dentro da ACP:", mean(insiderm)))
  temp = estab.ativo[insiderm, ]@data
  temp$Longitude = coordinates(estab.ativo[insiderm, ])[ ,1]
  temp$Latitude = coordinates(estab.ativo[insiderm, ])[ ,2]
  
  
  estab.ativo = temp
  
  # Calculando bandwith
  bandw = c(dpik(estab.ativo$Longitude),
            dpik(estab.ativo$Latitude))
  
  
  
  lat_range <- range(estab.ativo$Latitude)
  lon_range <- range(estab.ativo$Longitude)
  
  
  print("Calculando a densidade")
  if(opcao == "emprego") {
    
    # Retirando outliers de emprego
    adm.pub <-
      grep('^75', x = estab.ativo$clas_cnae10)
    
    cnae_xt <- 
      grep(pattern = 
             paste0(
               paste0('^',c(74,40,41,60,90),'|', collapse = ''), '^',
               62103),
           x = estab.ativo$clas_cnae10)
    
    estab.ativo <- 
      estab.ativo %>% 
      mutate(cnae = substring(clas_cnae10, 1, 2))
    
    
    estab.noxtrm <- 
      estab.ativo %>% 
      group_by(cnae) %>% 
      mutate(input = as.numeric(median(qtd_vinc_ativos))) 
    
    
    estab.noxtrm %<>% 
      add_rownames('rowname') %>% 
      mutate(qtd_vinc_ativos = 
               ifelse(rowname %in% cnae_xt, input, qtd_vinc_ativos)) %>% 
      mutate(qtd_vinc_ativos = 
               ifelse(rowname %in% adm.pub, 0, qtd_vinc_ativos))
    estab.noxtrm$rowname <- NULL
    
    # Repetindo a base pelo numero de empregos 
    df.expanded = 
      estab.noxtrm[rep(seq_len(nrow(estab.noxtrm)), estab.noxtrm$qtd_vinc_ativos), ]
    coord = data.frame(Longitude = df.expanded$Longitude,
                       Latitude = df.expanded$Latitude)
    # Calculando
    dens = bkde2D(x = coord, band = bandw,
                  gridsize = c(2400, 2400),
                  range.x = list(lon_range,lat_range))
    
  } else if(opcao == "estab"){
    
    coord = data.frame(Longitude = estab.ativo$Longitude,
                       Latitude = estab.ativo$Latitude)
    dens = bkde2D(x = coord, band = bandw,
                  gridsize = c(2400, 2400),
                  range.x = list(lon_range,lat_range))
    
  }
  
  print("Finalizado")
  
  kde_df <- expand.grid(
    lon = seq.int(lon_range[1], lon_range[2], length.out = 2400),
    lat = seq.int(lat_range[1], lat_range[2], length.out = 2400)
  )
  
  
  
  
  kde_df$density <- melt(dens$fhat)$value
  high = mean(kde_df$density) + 3 * sd(kde_df$density)
  # guardando informacoes densidade
  den_fill_scale <- scale_fill_gradient2(low = 'white', mid = 'darkgreen', 
                                         high = 'red', midpoint = high)
  den_fill_scale$range$train(kde_df$density)
  kde_df$density_s <- ggplot2:::scale_map(den_fill_scale,kde_df$density)
  kde_df$density_zeroone <- pmin(kde_df$density / max(kde_df$density), .9)
  
    kde <- list(kernel = kde_df, 
              ACP = acp,
              peso = opcao,
              ano = ano)
  
  
  acp.rm = shape.acp[shape.acp$ACP == kde$ACP, ]
  
  coordinates(kde$kernel) = c("lon","lat")
  proj4string(kde$kernel) = proj4string(acp.rm)
  
  insiderm = !is.na(over(kde$kernel, as(acp.rm, "SpatialPolygons")))
  
  print(paste0(round(mean(insiderm), digits = 2)*100,
               "% do grid está dentro da ACP"))
  
  
  
  temp = kde$kernel[insiderm, ]@data
  temp$lon = coordinates(kde$kernel[insiderm, ])[ ,1]
  temp$lat = coordinates(kde$kernel[insiderm, ])[ ,2]
  
  centro.eco <- temp[temp$density > mean(temp$density) + 3 * sd(temp$density), ]
  
  highden <- temp[which.max(temp$density), c('lon','lat')]
  
  assign("map.centro", get_map(location = c(lon = highden$lon, 
                                            lat = highden$lat), 
                               source = "google", maptype = 'roadmap', 
                               messaging = F, color = "bw", zoom = 11),
         envir = .GlobalEnv)
  
  centro <-
    ggmap(map.centro) +
    geom_tile(aes(x = lon, y = lat, fill = density_s), alpha = 0.5, 
              data = centro.eco) +
    scale_fill_identity() +
    theme_map() +
    theme(legend.position="none") + 
    coord_equal()
  
  ggsave(filename = 
           paste0("centros/",
                  tolower(sub('\\s', '', kde$ACP)),"_",kde$peso,kde$ano,".png"),
         plot = centro, width = 15, height = 12.45)
  
  return(centro)

  
}

#Sao Paulo
gerador_centro(data = rais13.estab, opcao = "emprego",
          regiaometro = "RM São Paulo", 
          acp = "Sao Paulo")
gerador_centro(data = rais13.estab, opcao = "estab",
          regiaometro = "RM São Paulo", 
          acp = "Sao Paulo")
gerador_centro(data = rais02.estab, opcao = "emprego",
          regiaometro = "RM São Paulo", 
          acp = "Sao Paulo")
gerador_centro(data = rais02.estab, opcao = "estab",
          regiaometro = "RM São Paulo", 
          acp = "Sao Paulo")



#Todas RM - menos DF
for(z in c("emprego","estab")) { 
  for(k in unique(rais$estab2002$RM)[-7]){ # todas RM's menos DF
    print(z)
    print(k)
    lapply(rais, function(x) {gerador_centro(data = x, opcao = z,
                                        regiaometro = k, 
                                        acp = substring(iconv(k, to='ASCII//TRANSLIT'), 4))}
    )
  }
}

#Brasilia
for(z in c("emprego","estab")) {
  print(z)
  lapply(rais, function(x) {gerador_centro(data = x, opcao = z,
                                      regiaometro = "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno", 
                                      acp = "Brasilia")}
  )
}

#### Sobrepondo centros  empregos e estabelecimentos ####

gerador_sobreposto = function(data, regiaometro, acp, lupa = 11){
  
  
  # Descobrindo o ano
  if(names(data)[1] == 'EstabID') ano <- "02" else ano <- "13"
  
  print(paste0("Ano de 20",ano))
  
  
  estab.ativo <- 
    data %>% 
    filter(RM == regiaometro) %>% 
    filter(PrecisionDepth != "1 Estrela") %>% 
    filter(!is.na(Longitude)) %>% 
    filter(ind_rais_neg == 0) 
  
  estab.ativo <- as.data.frame(estab.ativo)
  
  
  # Removendo outliers - estabelecimentos dentro da RM fora da ACP
  acp.rm = shape.acp[shape.acp$ACP == acp, ]
  coordinates(estab.ativo) = c("Longitude","Latitude")
  proj4string(estab.ativo) = proj4string(acp.rm)
  insiderm = !is.na(over(estab.ativo, as(acp.rm, "SpatialPolygons")))
  
  print(paste("Média de estabelecimentos dentro da ACP:", mean(insiderm)))
  temp = estab.ativo[insiderm, ]@data
  temp$Longitude = coordinates(estab.ativo[insiderm, ])[ ,1]
  temp$Latitude = coordinates(estab.ativo[insiderm, ])[ ,2]
  
  
  estab.ativo = temp
  
  assign(paste0("estab.ativo",ano),
         estab.ativo,
         envir = .GlobalEnv)
  
  
  # Calculando bandwith
  bandw = c(dpik(estab.ativo$Longitude),
            dpik(estab.ativo$Latitude))
  
  
  
  lat_range <- range(coordinates(acp.rm)[ ,2])
  lon_range <- range(coordinates(acp.rm)[ ,1])
  
  
  print("Calculando a densidade emprego")
  
    # Retirando outliers de emprego
    adm.pub <-
      grep('^75', x = estab.ativo$clas_cnae10)
    
    cnae_xt <- 
      grep(pattern = 
             paste0(
               paste0('^',c(74,40,41,60,90),'|', collapse = ''), '^',
               62103),
           x = estab.ativo$clas_cnae10)
    
    estab.ativo <- 
      estab.ativo %>% 
      mutate(cnae = substring(clas_cnae10, 1, 2))
    
    
    estab.noxtrm <- 
      estab.ativo %>% 
      group_by(cnae) %>% 
      mutate(input = as.numeric(median(qtd_vinc_ativos))) 
    
    
    estab.noxtrm %<>% 
      add_rownames('rowname') %>% 
      mutate(qtd_vinc_ativos = 
               ifelse(rowname %in% cnae_xt, input, qtd_vinc_ativos)) %>% 
      mutate(qtd_vinc_ativos = 
               ifelse(rowname %in% adm.pub, 0, qtd_vinc_ativos))
    estab.noxtrm$rowname <- NULL
    
    # Repetindo a base pelo numero de empregos 
    df.expanded = 
      estab.noxtrm[rep(seq_len(nrow(estab.noxtrm)), estab.noxtrm$qtd_vinc_ativos), ]
    coord = data.frame(Longitude = df.expanded$Longitude,
                       Latitude = df.expanded$Latitude)
    # Calculando
    dens_emp = bkde2D(x = coord, band = bandw,
                  gridsize = c(2400, 2400),
                  range.x = list(lon_range,lat_range))
    
    print("Calculando a densidade estab")
    
    coord = data.frame(Longitude = estab.ativo$Longitude,
                       Latitude = estab.ativo$Latitude)
    dens_est = bkde2D(x = coord, band = bandw,
                  gridsize = c(2400, 2400),
                  range.x = list(lon_range,lat_range))
  
  print("Finalizado")
  
  kde_emp <- expand.grid(
    lon = seq.int(lon_range[1], lon_range[2], length.out = 2400),
    lat = seq.int(lat_range[1], lat_range[2], length.out = 2400)
  )
  
  kde_emp$density <- melt(dens_emp$fhat)$value
  high = mean(kde_emp$density) + 3 * sd(kde_emp$density)
  # guardando informacoes densidade
  den_fill_scale <- scale_fill_gradient2(low = 'white', mid = 'darkgreen', 
                                         high = 'red', midpoint = high)
  den_fill_scale$range$train(kde_emp$density)
  kde_emp$density_s <- ggplot2:::scale_map(den_fill_scale,kde_emp$density)
  kde_emp$density_zeroone <- pmin(kde_emp$density / max(kde_emp$density), .9)
  
  
  kde_est <- expand.grid(
    lon = seq.int(lon_range[1], lon_range[2], length.out = 2400),
    lat = seq.int(lat_range[1], lat_range[2], length.out = 2400)
  )
  
  
  kde_est$density <- melt(dens_est$fhat)$value
  high = mean(kde_est$density) + 3 * sd(kde_est$density)
  # guardando informacoes densidade
  den_fill_scale <- scale_fill_gradient2(low = 'white', mid = 'darkgreen', 
                                         high = 'red', midpoint = high)
  den_fill_scale$range$train(kde_est$density)
  kde_est$density_s <- ggplot2:::scale_map(den_fill_scale,kde_est$density)
  kde_est$density_zeroone <- pmin(kde_est$density / max(kde_est$density), .9)
  
  kde <- list(emprego = kde_emp,
              estab = kde_est,
              ACP = acp,
              ano = ano)
  
  
  acp.rm = shape.acp[shape.acp$ACP == kde$ACP, ]
  
  # Emprego
  coordinates(kde$emprego) = c("lon","lat")
  proj4string(kde$emprego) = proj4string(acp.rm)
  
  # Estab
  coordinates(kde$estab) = c("lon","lat")
  proj4string(kde$estab) = proj4string(acp.rm)
  
  insiderm = !is.na(over(kde$estab, as(acp.rm, "SpatialPolygons")))
  
  print(paste0(round(mean(insiderm), digits = 2)*100,
               "% do grid está dentro da ACP"))
  
  
  # Emprego
  temp_emp = kde$emprego[insiderm, ]@data
  temp_emp$lon = coordinates(kde$emprego[insiderm, ])[ ,1]
  temp_emp$lat = coordinates(kde$emprego[insiderm, ])[ ,2]
  
  centro.emp <- 
    temp_emp[temp_emp$density > mean(temp_emp$density) + 3 * sd(temp_emp$density), ]
  
  
  # Estab
  temp_est = kde$estab[insiderm, ]@data
  temp_est$lon = coordinates(kde$estab[insiderm, ])[ ,1]
  temp_est$lat = coordinates(kde$estab[insiderm, ])[ ,2]
  
  
  centro.est <- 
    temp_est[temp_est$density > mean(temp_est$density) + 3 * sd(temp_est$density), ]
  
  if(acp == 'Brasilia') {
    localiza <- 
      (temp_emp[which.max(temp_emp$density), c('lon','lat')] +
         temp_emp[which.max(temp_emp$density), c('lon','lat')] ) / 2
    
  } else {
    
    google_centro <- dget("google_centro.R")
    localiza <- 
      google_centro %>% 
      filter(cidade == acp) %>% 
      dplyr::select(lon, lat)
  }
  
  # Intercessao
  centro.inter = inner_join(centro.emp, centro.est, by = c("lat","lon"))
  
  assign(paste0("centro.inter", kde$ano), 
                centro.inter,
                envir = .GlobalEnv)
  
  assign("map.centro", get_map(location = c(lon = localiza$lon, 
                                            lat = localiza$lat), 
                               source = "google", maptype = 'roadmap', 
                               messaging = F, color = "bw", zoom = lupa),
         envir = .GlobalEnv)
  
  centro <-
    ggmap(map.centro) +
    geom_tile(aes(x = lon, y = lat, fill = "brown"), alpha = 0.5, 
              data = centro.inter) +
    scale_fill_identity() +
    theme_map() +
    theme(legend.position="none") + 
    coord_equal()
  
  ggsave(filename = 
           paste0("centro_sobreposto/",
                  tolower(sub('\\s', '', kde$ACP)),kde$ano,".png"),
         plot = centro, width = 15, height = 12.45)
  
  return(centro)
}

lapply(rais, function(x) {gerador_sobreposto(data = x, 
                                             regiaometro = "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno", 
                                             acp = "Brasilia")}
)

for(k in unique(rais$estab2002$RM)[-7]){ # todas RM's menos DF
  print(k)
  lapply(rais, function(x) {gerador_sobreposto(data = x, regiaometro = k, 
                                               acp = 
                                                 substring(
                                                 iconv(k, to='ASCII//TRANSLIT'), 4))
    }
  )
}

#### Área e tamanho do pixel ####
# ver a quantidade de pixel's dentro do shape da acp
areas <- 
  data.frame(RM = c('Belém', 'Belo Horizonte', 'Recife', 
                    'Porto Alegre', 'Manaus', 'Goiânia', 'Fortaleza',
                    'Curitiba', 'Rio de Janeiro', 'Salvador', 
                    'São Paulo', 'RIDE'))
areas$RM = as.character(areas$RM)
areas$pixel_area_m2 <- 1
areas$lon_cellsize <- 1
areas$lat_cellsize <- 1

for(i in 1:11){
  acp = iconv(areas$RM[i], to='ASCII//TRANSLIT')
  acp.rm = shape.acp[shape.acp$ACP == acp, ]
  
  kde <- 
    kernel.density(data = rais$estab2002, 
                   regiaometro = paste("RM", areas$RM[i]),
                   acp = acp, opcao = "estab")
  
  coordinates(kde$kernel) = c("lon","lat")
  proj4string(kde$kernel) = proj4string(acp.rm)
  
  insiderm = !is.na(over(kde$kernel, as(acp.rm, "SpatialPolygons")))
  areas$pixel_area_m2[i] <- round(sum(acp.rm@data$Area_Km2) / sum(insiderm) *1000000,
                                  digits = 2)
  
  gridded(kde$kernel) <- TRUE
  areas$lon_cellsize[i] <- gridparameters(kde$kernel)$cellsize[1]
  areas$lat_cellsize[i] <- gridparameters(kde$kernel)$cellsize[2]
  
  rm(i, acp)
}
# Brasília
acp.rm = shape.acp[shape.acp$ACP == 'Brasilia', ]
kde <- 
  kernel.density(data = rais$estab2002, 
                 regiaometro = "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno",
                 acp = 'Brasilia', opcao = "estab")

coordinates(kde$kernel) = c("lon","lat")
proj4string(kde$kernel) = proj4string(acp.rm)

insiderm = !is.na(over(kde$kernel, as(acp.rm, "SpatialPolygons")))
areas$pixel_area_m2[12] <- round(sum(acp.rm@data$Area_Km2) / sum(insiderm) *1000000,
                                 digits = 2)

gridded(kde$kernel) <- TRUE
areas$lon_cellsize[12] <- gridparameters(kde$kernel)$cellsize[1]
areas$lat_cellsize[12] <- gridparameters(kde$kernel)$cellsize[2]

rm(acp.rm, insiderm)


dput(areas, 'RM_areagrid.R')
write.csv2(areas, 'RM_areagrid.csv', row.names= F)

#### Comparando dois anos centros ####

centro12 = readOGR(dsn = "K:/Matheus/shape centros", 
                   layer = "Centros_12_RMs")

comparador_sobreposto = function(data, regiaometro, acp, 
                                 graph = c(T, F), zoom = 11){
  

  lapply(data, function(x) {gerador_sobreposto(data = x, 
                                               regiaometro = regiaometro, 
                                               acp = acp, lupa = zoom)}
  )
  
  
  centro.fim <- inner_join(centro.inter13, centro.inter02, by = c("lon","lat"))

  
  if(graph == T) {
  
    
    centro12@data$acp <- c('Belem', 'Belo Horizonte', 'Brasilia', 'Recife', 
                           'Porto Alegre', 'Manaus', 'Goiania', 'Fortaleza',
                           'Curitiba', 'Rio de Janeiro', 'Salvador', 
                           'Sao Paulo')
    acp.rm = shape.acp[shape.acp$ACP == acp, ]
    
    
    centro.acp = centro12[centro12@data$acp == acp, ]
    centro.acp = spTransform(centro.acp, proj4string(acp.rm))
    centro.acp = fortify(centro.acp, region = 'acp') 
    

      
  centro02 <-
    ggmap(map.centro) +
    geom_tile(aes(x = lon, y = lat, fill = "#5A2E91"), alpha = 0.5, 
              data = centro.inter02) +
    geom_tile(aes(x = lon, y = lat, fill = "#8C4D36"), alpha = 0.7, 
              data = centro.fim) +
    geom_polygon(data= centro.acp, aes(x=long, y=lat, group=group), fill=NA, 
                 colour="yellow", size = 1 , alpha=1)  +
    scale_fill_identity() +
    theme_map() +
    theme(legend.position = "right") + 
    coord_equal()
  
  ggsave(filename = 
           paste0("comparacao_ano/",
                  tolower(sub('\\s', '', acp)),"saiu02.png"),
         plot = centro02, width = 15, height = 12.45)
  
  centro13 <-
    ggmap(map.centro) +
    geom_tile(aes(x = lon, y = lat, fill = "#51914E"), alpha = 0.5, 
              data = centro.inter13) +
    geom_tile(aes(x = lon, y = lat, fill = "#8C4D36"), alpha = 0.7, 
              data = centro.fim) +
    geom_polygon(data= centro.acp, aes(x=long, y=lat, group=group), fill=NA, 
                 colour="yellow", size = 1 , alpha=1)  +
    scale_fill_identity() +
    theme_map() +
    theme(legend.position = "right") + 
    coord_equal()
  
  ggsave(filename = 
           paste0("comparacao_ano/",
                  tolower(sub('\\s', '', acp)),"entrou13.png"),
         plot = centro13, width = 15, height = 12.45)
  
  
#   ggmap(map.centro) +
#     geom_tile(aes(x = lon, y = lat, fill = "#51914E"), alpha = 0.5, 
#               data = centro.inter13) +
#     geom_tile(aes(x = lon, y = lat, fill = "#5A2E91"), alpha = 0.5, 
#               data = centro.inter02) +
#     geom_tile(aes(x = lon, y = lat, fill = "#8C4D36"), alpha = 0.8, 
#               data = centro.fim) +
#     scale_fill_identity() +
#     theme_map() +
#     coord_equal()
  
  } else {
    
    centro.fim <- centro.fim[ ,c('lon', 'lat')]
    centro.inter02 <- centro.inter02[ ,c('lon','lat')]
    centro.inter13 <- centro.inter13[ ,c('lon','lat')]
    
    areas <- dget('RM_areagrid.R')
    pixel_acp <- 
      areas %>% 
      mutate(ACP =  iconv(RM, to='ASCII//TRANSLIT')) %>% 
      filter(ACP == acp)
    print('Gerando centralidade para 2002')
    # 2002
    acp.rm = shape.acp[shape.acp$ACP == acp, ]
    coordinates(estab.ativo02) = c("Longitude","Latitude")
    proj4string(estab.ativo02) = proj4string(acp.rm)
    
    excentro = anti_join(centro.inter02, centro.inter13, by = c("lat","lon"))
    coordinates(excentro) = c("lon", "lat")
    bb = bbox(excentro) 
    gt = GridTopology(c(bb[1,1],bb[2,1]), 
                      c(pixel_acp$lon_cellsize,pixel_acp$lat_cellsize),
                      round(
                        c(diff(bb[1,])/pixel_acp$lon_cellsize,
                          diff(bb[2,])/pixel_acp$lat_cellsize)) + 1)                
    excentro <- SpatialPixels(excentro, grid = gt, 
                              proj4string = proj4string(acp.rm)) 

    inside = !is.na(over(estab.ativo02, excentro)) # "vetor" de T ou F
    estab.ativo02@data$centralidade = ifelse((as.vector(inside)) == T, 1, 0) 

    
    novocentro = anti_join(centro.inter13, centro.inter02, by = c("lat","lon"))
    coordinates(novocentro) = c("lon", "lat")
    bb = bbox(novocentro) 
    gt = GridTopology(c(bb[1,1],bb[2,1]), 
                      c(pixel_acp$lon_cellsize,pixel_acp$lat_cellsize),
                      round(
                        c(diff(bb[1,])/pixel_acp$lon_cellsize,
                          diff(bb[2,])/pixel_acp$lat_cellsize)) + 1)             
    novocentro <- SpatialPixels(novocentro, grid = gt, 
                                proj4string = proj4string(acp.rm)) 
    
    inside = !is.na(over(estab.ativo02, novocentro)) # "vetor" de T ou F
    estab.ativo02@data$centralidade = 
      estab.ativo02@data$centralidade + 
      ifelse((as.vector(inside)) == T, 2, 0) 
    
    coordinates(centro.fim) = c("lon", "lat")
    bb = bbox(centro.fim) 
    gt = GridTopology(c(bb[1,1],bb[2,1]), 
                      c(pixel_acp$lon_cellsize,pixel_acp$lat_cellsize),
                      round(
                        c(diff(bb[1,])/pixel_acp$lon_cellsize,
                          diff(bb[2,])/pixel_acp$lat_cellsize)) + 1)            
    centro.fim <- SpatialPixels(centro.fim, grid = gt, 
                                proj4string = proj4string(acp.rm)) 
    
    inside = !is.na(over(estab.ativo02, centro.fim)) # "vetor" de T ou F
    estab.ativo02@data$centralidade = 
      estab.ativo02@data$centralidade + 
      ifelse((as.vector(inside)) == T, 3, 0) 
    
    estab.ativo02 = as.data.frame(estab.ativo02)
   
    print('Gerando centralidade para 2013')
    # 2013
    coordinates(estab.ativo13) = c("Longitude","Latitude")
    proj4string(estab.ativo13) = proj4string(acp.rm)
    
    
    inside = !is.na(over(estab.ativo13, excentro)) # "vetor" de T ou F
    estab.ativo13@data$centralidade = ifelse((as.vector(inside)) == T, 1, 0) 
    
    inside = !is.na(over(estab.ativo13, novocentro)) # "vetor" de T ou F
    estab.ativo13@data$centralidade = 
      estab.ativo13@data$centralidade + 
      ifelse((as.vector(inside)) == T, 2, 0) 
    
    inside = !is.na(over(estab.ativo13, centro.fim)) # "vetor" de T ou F
    estab.ativo13@data$centralidade = 
      estab.ativo13@data$centralidade + 
      ifelse((as.vector(inside)) == T, 3, 0) 
    
    estab.ativo13 = as.data.frame(estab.ativo13)
    
    estab.ativo13$centralidade <-
      ifelse(estab.ativo13$centralidade %in% c(4,5,6), 3, 
             estab.ativo13$centralidade)
    estab.ativo02$centralidade <-
      ifelse(estab.ativo02$centralidade %in% c(4,5,6), 3, 
             estab.ativo02$centralidade)
    
    return(list(estab13 = estab.ativo13,
                estab02 = estab.ativo02))
  }
  

}


for(k in grep(unique(rais$estab2002$RM), pattern = "^RM")){ # todas RM's menos DF
  k <- unique(rais$estab2002$RM)[k]
  print(k)
  comparador_sobreposto(data = rais, regiaometro = k, graph = T,
                        acp = 
                          substring(
                            iconv(k, to='ASCII//TRANSLIT'), 4))
  rm(k)
}

comparador_sobreposto(rais, acp = "Brasilia", graph = T,
                      regiaometro = "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno")

comparador_sobreposto(rais, acp = "Curitiba", graph = T,
                      regiaometro = "RM Curitiba")

comparador_sobreposto(rais, acp = "Sao Paulo", zoom = 10, graph = T,
                      regiaometro = "RM São Paulo")

comparador_sobreposto(rais, acp = "Belo Horizonte", zoom = 10, graph = T,
                      regiaometro = "RM Belo Horizonte")

#### Nova variável centralidade ####

# 3 - centro [dens > corte2013 e > corte 2002]
# 2 - novocentro [dens > corte2013 e < corte2002]
# 1 - excentro [dens < corte 2013 e > corte 2002]
# 0 - nuncacentro [dens < corte2013 e < corte 2002]
out <- 
  lapply(unique(rais$estab2002$RM)[-7], 
         function(k) {
           print(k)
           comparador_sobreposto(data = rais, regiaometro = k, graph = F,
                                 acp = 
                                   substring(
                                     iconv(k, to='ASCII//TRANSLIT'), 4))
           }
  )

bsb <- comparador_sobreposto(rais, acp = "Brasilia", graph = F,
                              regiaometro = "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno")

rm(centro.inter02, centro.inter13, estab.ativo02, estab.ativo13, map.centro)

#2013
estab2013 <- list()
for(i in 1:11){
  estab2013[[i]] <- as.data.frame(out[[i]]$estab13)
}
estab2013 <- rbindlist(estab2013)
estab2013 <- rbind(estab2013, bsb$estab13)

#2002
estab2002 <- list()
for(i in 1:11){
  estab2002[[i]] <- as.data.frame(out[[i]]$estab02)
}
estab2002 <- rbindlist(estab2002)
estab2002 <- rbind(estab2002, bsb$estab02)

rm(bsb, centro.inter13, estab.ativo02, estab.ativo13)
#saveRDS(out, 'backup_centralidade.R')

# Voltar a base
rais$estab2013 <- 
  left_join(rais$estab2013, 
            select(estab2013, c(id,centralidade)),
            by = 'id')

rais$estab2013$centralidade <-
  ifelse(is.na(rais$estab2013$centralidade), 0, rais$estab2013$centralidade)

rais$estab2002 <- 
  left_join(rais$estab2002, 
            select(estab2002, c(EstabID,centralidade)),
            by = 'EstabID')

rais$estab2002$centralidade <-
  ifelse(is.na(rais$estab2002$centralidade), 0, rais$estab2002$centralidade)

table(rais$estab2013$centralidade)
table(rais$estab2013$centralidade_corrigida)

rais$estab2013$centralidade = NULL
rais$estab2002$centralidade_corrigida = NULL

names(rais$estab2002)[15] <- 'centralidade_corrigida'

