library(rvest)
require(RCurl)
library(XML)
require(ggmap)
library(raster)


# Puxar a base de dados que contem as latitudes e longitudes
load(file = "L:/# DIRUR #/ASMEQ/MCMV/RJ3.Rdata")



getwd()
setwd("C:/Users/b2562360/Desktop/")

############## Come?ar a partir daqui ###################



samplecluster <-  function(banco, n){
  
  df<-data.frame(banco[complete.cases(banco),])
  dend<-hclust(dist(df),"complete")
  #plot(dend, hang=-1)
  groups <- cutree(dend, k=n)
  df$grupo<-groups
    
  df[,1]<-as.numeric(as.character(df[,1]))
  df[,2]<-as.numeric(as.character(df[,2]))
  
  print (paste("M?dia de estabelecimentos por cluster:",mean(table(groups))))
  t1 <- quantile(table(groups),probs = c(0.00,0.25,0.50,0.75,.95,.99,1.00))
  print(cat("Medidas Resumo das quantidades dos grupos:", "\n", attr(t1,"names"), "\n", t1))
  
  
  G1 <- data.frame("grupo"=1:n,
                   "mediaLAT"= tapply(df[,1], groups, mean))  
  G2 <- data.frame("grupo"=1:n,
                   "mediaLON"= tapply(df[,2], groups, mean))  # means of groups
  
  assign("destino",paste(G1$mediaLAT,",",G2$mediaLON,sep=""),
         envir=.GlobalEnv)

  
  df<-merge(df,G1,by="grupo")
  df<-merge(df,G2,by="grupo")
  assign(x = "df",df,envir = .GlobalEnv)
  

  distancia <- round(pointDistance( cbind(df[,2], df[,3]), cbind(df[,4],df[,5]), lonlat= T),2)
  t <- quantile(distancia,probs = c(0.00,0.25,0.50,0.75,.95,.99,1.00))
  print(cat("Medidas Resumo das distancias intra-grupo:", "\n", attr(t,"names"), "\n", t))
  
  f1 <- as.numeric(which(distancia == t[[7]] , distancia))
  latlon1 <- paste(df[f1,2],",", df[f1,3], sep="")
  latlon2 <- paste(df[f1,4],",", df[f1,5],sep="")
  url<-paste("https://www.google.com.br/maps?saddr=",latlon1,"&daddr=",
             latlon2,"&hl=pt-BR&sll=-22.944938,-43.358232&sspn=0.234274,0.259552&dirflg=r&ttype=dep&date=28%2F01%2F15&time=08:00&noexp=0&noal=0&sort=def&mra=ls&z=14&start=0",sep="")
  site <- html(url)
  xp <- "//*[@id=\"altroute_0\"]/div/div[1]"
  time <- xpathApply(site, xp,xmlValue, "class") [[1]]
  print(cat("Tempo m?ximo de deslocamento intra-grupo:", time))
  
 

}

load(file = "L:/# DIRUR #/ASMEQ/MCMV/destino4000_7000.Rdata")
samplecluster(destino4000_7000,n=1500)
destino4000_7000 <- destino
samplecluster(destino8000_13000, n=4000)
#### Distancia maxima para a media ####
#### Arrumar rotina ####




##############
a <- 1
for (i in 1:100)
{
  a <- a+i
  Sys.sleep(5)
  print(i)
}
  
temp_cont <- seq(1, 13392, by=40)
length(temp_cont)
destinosucupira_15000 <- destinosucupira_15000[-1]

for ( j in 1:length(temp_cont)){

  for (i in temp_cont[j]:temp_cont[j+1]) {
    
    
    url<-paste("https://www.google.com.br/maps?saddr=-22.89171,-43.24893&daddr=",
               destinosucupira_15000[i],"&hl=pt-BR&sll=-22.944938,-43.358232&sspn=0.234274,0.259552&dirflg=r&ttype=dep&date=28%2F01%2F15&time=08:00&noexp=0&noal=0&sort=def&mra=ls&z=14&start=0",sep="")
    
    site <- html(url)
    xp <- "//*[@id=\"altroute_0\"]/div/div[1]"
    xp2 <- "//*[@id=\"altroute_1\"]/div/div[1]"
    
     
    
    if (is.null(xpathApply(site, xp,xmlValue, "class") [[1]])  || is.null(xpathApply(site, xp2,xmlValue, "class") [[1]]) ) 
    {
      time1_sucupira15000[i] <- NA
      time2_sucupira15000[i] <- NA
      
    } else {
      time1[i] <- xpathApply(site, xp,xmlValue, "class") [[1]]
      time2[i] <- xpathApply(site, xp2,xmlValue, "class") [[1]]
    }
    
    
    print(i)
   
  }

    Sys.sleep(120)
  
 }






#SALVAR PAR?METROS 
save("time1_sucupira15000",file="L:/# DIRUR #/ASMEQ/MCMV/time1_sucupira15000.Rdata")
save("time2_sucupira15000",file="L:/# DIRUR #/ASMEQ/MCMV/time2_sucupira15000.Rdata")

tempoRJ_4000_7000_s <- data.frame(cbind(destino4000_7000,time1,time2))



# Os destinos 4548 e 4969 deram valores maior que 1h
# 1h:40 min pra ?rea mais perto aonde passa ?nibus seguido de mais 30 minutos de caminhada (DIAMETRO 7500-8000)



transf <- function(base){

if(dim(base)[2]!= 3) stop("N?mero de dimens?es da base n?o confere")
  
hora1<- grep("hora", base[,2])
base$min1 = 0
base$min1[hora1] <- substr(base[hora1,2],start=1,stop=1)
temp<-gsub(" min$","",x = base[,2])
temp<-gsub(". horas? ","",temp)
temp<-as.numeric(temp)
base$min1 = (as.numeric(base$min1)*60) + temp


hora2<- grep("hora", base[,3])
base$min2 = 0
base$min2[hora2] <- substr(base[hora2,3],start=1,stop=1)
temp<-gsub(" min$","",x = base[,3])
temp<-gsub(". horas? ","",temp)
temp<-as.numeric(temp)
base$min2 = (as.numeric(base$min2)*60) + temp

base$media <- apply(X = base[,c(4,5)],MARGIN = 1,FUN = mean,na.rm=T)
base$CONTROLE <- factor(ifelse(base$media<30, 1,0 ))
base$CONTROLE2 <- factor(ifelse(base$media<60, 1,0 ))

base$lat<-substr(x = base[,1],start = 1,stop = 17)
base$lon<-substr(x = base[,1],start = 19,stop = 40)                         
                         
return(base)

}


 
tempoRJ_7000_7500 <- transf(tempoRJ_7000_7500)
tempoRJ_8000_13000_s <- transf(tempoRJ_8000_13000_s)
save("tempoRJ_7000_7500",file="L:/# DIRUR #/ASMEQ/MCMV/tempoRJ_7000_7500.Rdata")
save("tempoRJ_7500_8000",file="L:/# DIRUR #/ASMEQ/MCMV/tempoRJ_7500_8000.Rdata")

###CASO PARTICULAR 8000-13000
df8000_13000[,4] <- as.character(df8000_13000[,4])
df8000_13000[,5] <- as.character(df8000_13000[,5])

tempoRJ_8000_13000 <- merge(df8000_13000, tempoRJ_8000_13000_s, 
                            by.x=c("mediaLAT","mediaLON"), by.y=c("lat","lon"), all.x=TRUE)

#tempoRJ_8000_13000 <- tempoRJ_8000_13000[,-c(1:3)]
save("tempoRJ_8000_13000",file="L:/# DIRUR #/ASMEQ/MCMV/tempoRJ_8000_13000.Rdata")





destino<- paste(G1,",",G2,sep="") # para jogar na url da programa?ao


a <- ggmap(map, extent = "device") +
  geom_point( aes(x=-43.248934, y=-22.891707),size=7 ,colour = "black",pch=3) +
  geom_point( aes(x=Longitude, y=Latitude), 
              colour= "lightblue",
              data=df, size=.5, alpha=.8) +
  geom_point( aes(x=mediaLON, y=mediaLAT), 
              data=df,size=1.5, alpha=1, colour= "blue") 

print(a)

