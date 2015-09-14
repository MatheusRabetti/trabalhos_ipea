rm(list=ls())
gc(reset=T)
setwd("/Users/leonardoaguirre/Downloads/")

#x<-readline(prompt=message("Informe o email para qual deverá ser enviado o arquivo contendo os resultados:"))


wazup<-readLines("WhatsApp.txt")

l<-NULL
for(i in 1:length(wazup)){
  a<-grep(pattern="[0-9]{2}/[0-9]{2}/[0-9]{2}",wazup[i])
  if(length(a)>0){l[i]<-1}
  if(length(a)==0){l[i]<-0}
  rm(a)
}

wazup<-data.frame(cbind(as.character(wazup),grupo=cumsum(l)))

teste<-aggregate(as.character(V1)~grupo,FUN="paste",collapse=' ',data=wazup)

names(teste)<-c("grupo","wazup")

teste$data<-as.Date(substr(teste$wazup,1,8),format="%d/%m/%y")
teste$horario<-substr(teste$wazup,10,17)
teste$hora<-substr(teste$wazup,10,11)
teste$min<-substr(teste$wazup,13,14)
teste$seg<-substr(teste$wazup,16,17)

require(timeDate)
msg_dia<-aggregate(horario~data,FUN="length",data=teste)
msg_dia$fds<-isWeekend(msg_dia$data)
msg_dia<-msg_dia[2:43,]
require(ggplot2)
ggplot(data=msg_dia, aes(x=data, y=horario)) + geom_line() + geom_point() + 
  geom_hline(yintercept=mean(msg_dia$horario),alpha=0.3) + 
  geom_text(data = NULL, x = as.numeric(as.Date(min(msg_dia$data))), y = mean(msg_dia$horario)+1, 
            label = paste(round(mean(msg_dia$horario))))
#média diária de msgs
mean(msg_dia$horario)

#máximo de msgs em um dia
max(msg_dia$horario)

#mínimo de msgs em um dia
min(msg_dia$horario)

#distribuição de msgs diárias
hist(msg_dia$horario)
plot(density(msg_dia$horario))


wd <- c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sáb")
n <- as.POSIXlt(teste$data, tz = "GMT")$wday + 1
teste$dia_semana<-wd[n]
teste$dia_semana<-as.factor(teste$dia_semana)
teste$dia_semana = factor(teste$dia_semana,levels(teste$dia_semana)[c(5,7,2,3,6,4,1)])

diahora<-aggregate(wazup~dia_semana+hora,FUN="length",data=teste)

ggplot(diahora, aes(dia_semana, hora)) + geom_tile(aes(fill = wazup), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(data=diahora,aes(x=dia_semana,y=hora,label=wazup),vjust=0.45,hjust=.45,col=rgb(.3,.3,.3))

require(sqldf)
dia_semana<-sqldf("select distinct data, dia_semana from teste")
a<-aggregate(wazup~data,FUN="length",data=teste)
a<-merge(a,dia_semana,by="data",all.x=T)

#número médio de mensagens por dia da semana
(b<-aggregate(wazup~dia_semana,FUN="mean",data=a))
qplot(x=dia_semana,y=wazup,data=b,geom="bar",stat="identity")




j<-grep(pattern="(alterou o assunto para)",teste$wazup)
teste$acao<-NA
for(i in 1:length(j)){
  teste$acao[j[i]]<-"alterou o assunto"
}

j<-grep(pattern="(<mídia omitida>)",teste$wazup)
for(i in 1:length(j)){
  teste$acao[j[i]]<-"enviou mídia"
}

teste$acao<-ifelse(is.na(teste$acao),"comentou",teste$acao)

table(teste$acao)


(z<-teste[5,2])
vai<-function(x,comeca,termina){
  trim(substr(x,str_locate(x,comeca)[,2]+1,
              str_locate(substr(x,str_locate(x,comeca)[,2]+1,nchar(as.character(x))),termina)[,2]+str_locate(x,comeca)[,2]-nchar(termina)))
}
require(gdata)
require(stringr)

teste$usuario<-ifelse(teste$acao=="alterou o assunto",vai(substr(teste$wazup,18,nchar(teste$wazup))," ","alterou"),NA)
teste$usuario<-ifelse(teste$acao!="alterou o assunto",vai(substr(teste$wazup,18,nchar(teste$wazup))," ",":"),teste$usuario)
teste$acao<-ifelse(is.na(teste$usuario),"entrou",teste$acao)
teste$usuario<-ifelse(teste$acao=="entrou",vai(substr(teste$wazup,18,nchar(teste$wazup))," ","entrou"),teste$usuario)


aggregate(wazup~usuario+acao,FUN="length",data=teste[teste$acao!="entrou",])
#hashtags
a<-grep("#\\S+",teste$wazup)

table(vai(teste$wazup[a],"#"," "))

#matriz de relacionamento
require(sqldf)
teste<-sqldf("select * from teste order by data,horario")


teste$time<-as.POSIXct(paste(teste$data,teste$horario), format="%Y-%m-%d %H:%M:%S")

#teste2<-subset(teste,acao!="entrou")
periodo<-5*60

ator<-list(NULL)
id<-NULL
for(i in 1:nrow(teste)){
  j<-i+1
  ator[[i]]<-teste$usuario[i]
  while(as.numeric(teste$time[j]-teste$time[i],units="secs")<=periodo & j<=nrow(teste)){
    ator[[i]][[j-i]]<-teste$usuario[j]
    j<-j+1
  }
  names(ator[[i]])<-c(rep(teste$usuario[i],length(ator[[i]])))
  id<-append(id,rep(i,length(ator[[i]])))
  print(i)
}
relac<-data.frame(unlist(ator,use.names=F))
relac$ator<-names(unlist(ator,use.names=T))
relac$id<-id




names(relac)<-c("respond","ator","id")

relac2<-subset(relac,respond!=ator & ator!='〽️anolos')
final<-sqldf("select ator,respond, count(distinct id) as n from relac2 group by ator,respond order by ator, n desc")

#final<-sqldf("select ator,respond, count(*) as n from relac2 group by ator, respond order by ator,n desc")
#matriz<-reshape(final,idvar="respond",timevar="ator",direction="wide")
#cor<-cor(matriz[2:ncol(matriz)],use="pairwise.complete.obs")
#rgb.palette <- colorRampPalette(c("yellow", "red"), space = "rgb")
#levelplot(cor, main="teste", xlab="", ylab="", col.regions=rgb.palette(120), cuts=5, at=seq(0,1,0.01))

temp<-sqldf("select ator, sum(n) as tot from final group by ator")
final<-merge(final,temp,by="ator",all.x=T)
final$prop<-100*final$n/final$tot
#final$rank_rel<-ave(final$prop,final$respond,FUN=rank)
#final<-sqldf("select * from final order by respond,rank_rel desc")

ggplot(final, aes(ator, respond)) + geom_tile(aes(fill = prop), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data=final,aes(x=ator,y=respond,label=round(prop,1)),vjust=0.45,hjust=.45,col=rgb(.3,.3,.3))
