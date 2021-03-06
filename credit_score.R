# Programação retirada do blog do Pedro Albuquerque
# Link para o post: http://pedrounb.blogspot.com.br/2015/09/credit-score-usando-o-r.html

#Limpa o Workspace
rm(list=ls())

#Importa os dados German.csv 
dados.df<-read.csv("https://dl.dropboxusercontent.com/u/36068691/Blog/german.csv")

#Apresenta as variáveis do DataFrame
names(dados.df)

#Apresenta a estrutura do DataFrame
str(dados.df)


#Transforma em fatores as variáveis categóricas e "dummies"
dados.df[,"CHK_ACCT"]     <-as.factor(dados.df[,"CHK_ACCT"])
dados.df[,"HISTORY"]      <-as.factor(dados.df[,"HISTORY"])
dados.df[,"NEW_CAR"]      <-as.factor(dados.df[,"NEW_CAR"])
dados.df[,"USED_CAR"]     <-as.factor(dados.df[,"USED_CAR"])
dados.df[,"FURNITURE"]    <-as.factor(dados.df[,"FURNITURE"])
dados.df[,"RADIO.TV"]     <-as.factor(dados.df[,"RADIO.TV"])
dados.df[,"EDUCATION"]    <-as.factor(dados.df[,"EDUCATION"])
dados.df[,"RETRAINING"]   <-as.factor(dados.df[,"RETRAINING"])
dados.df[,"SAV_ACCT"]     <-as.factor(dados.df[,"SAV_ACCT"])
dados.df[,"EMPLOYMENT"]   <-as.factor(dados.df[,"EMPLOYMENT"])
dados.df[,"MALE_DIV"]     <-as.factor(dados.df[,"MALE_DIV"])
dados.df[,"MALE_SINGLE"]  <-as.factor(dados.df[,"MALE_SINGLE"])
dados.df[,"MALE_MAR"]     <-as.factor(dados.df[,"MALE_MAR"])
dados.df[,"CO.APPLICANT"] <-as.factor(dados.df[,"CO.APPLICANT"])
dados.df[,"GUARANTOR"]    <-as.factor(dados.df[,"GUARANTOR"])
dados.df[,"TIME_RES"]     <-as.factor(dados.df[,"TIME_RES"])
dados.df[,"REAL_ESTATE"]  <-as.factor(dados.df[,"REAL_ESTATE"])
dados.df[,"PROP_NONE"]    <-as.factor(dados.df[,"PROP_NONE"])
dados.df[,"OTHER_INSTALL"]<-as.factor(dados.df[,"OTHER_INSTALL"])
dados.df[,"RENT"]         <-as.factor(dados.df[,"RENT"])
dados.df[,"OWN_RES"]      <-as.factor(dados.df[,"OWN_RES"])
dados.df[,"NUM_CREDITS"]  <-as.factor(dados.df[,"NUM_CREDITS"])
dados.df[,"JOB"]          <-as.factor(dados.df[,"JOB"])
dados.df[,"NUM_DEPEND"]   <-as.factor(dados.df[,"NUM_DEPEND"])
dados.df[,"TELEPHONE"]    <-as.factor(dados.df[,"TELEPHONE"])
dados.df[,"FOREIGN"]      <-as.factor(dados.df[,"FOREIGN"])

#Variável dependente
dados.df[,"RESPONSE"]     <-as.factor(dados.df[,"RESPONSE"])

#Transforma em numeric
dados.df[,"AMOUNT"]       <-as.numeric(dados.df[,"AMOUNT"])
dados.df[,"INSTALL_RATE"] <-as.numeric(dados.df[,"INSTALL_RATE"])
dados.df[,"AGE"]          <-as.numeric(dados.df[,"AGE"])
dados.df[,"NUM_DEPEND"]   <-as.numeric(dados.df[,"NUM_DEPEND"])
dados.df[,"DURATION"]     <-as.numeric(dados.df[,"DURATION"])


#Índices obtidos após a aleatorização
ordena <- sort(sample(nrow(dados.df), nrow(dados.df)*.6))

#Dados para o treinamento
treinamento<-dados.df[ordena,]

#Dados para a validação
validacao<-dados.df[-ordena,]


#Regressão Logística
modelo.completo <- glm(RESPONSE ~ . ,family=binomial,data=treinamento)

#Abordagem Stepwise para seleção de variáveis
stepwise <- step(modelo.completo,direction="both") 

#Modelo com as variáveis indicadas pelo Stepwise
stepwise <- glm(RESPONSE ~  JOB+NUM_CREDITS+EMPLOYMENT+RETRAINING+NEW_CAR+TELEPHONE+MALE_DIV+
                  FURNITURE+PROP_NONE+MALE_MAR+RENT+NUM_DEPEND+REAL_ESTATE+EDUCATION+FOREIGN+
                  TIME_RES, family=binomial,data=treinamento)

#Resume os resultados do modelo
summary(stepwise)

#Calcula a razão de chances
exp(cbind(OR = coef(stepwise), confint(stepwise)))

#Faz a previsão para a base de validação (probabilidade)
predito<-predict(stepwise,validacao,type="response")

#Escolhe quem vai ser "1" e quem vai ser "0"
predito<-ifelse(predito>=0.8,1,0)

#Compara os resultados
table(predito,validacao$RESPONSE)

# taxa de acerto
(101 + 104) / 400 
