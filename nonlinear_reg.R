# Autor: Matheus Rabetti
# Ultima Att: 30/03/2016
# Versao: 2.0


# PRIMEIROS PASSOS --------------------------------------------------------

rm(list = ls())
gc()

setwd("C:/Users/b2562360/Documents/Work/")

agric <- read.csv("eustaquio.csv", sep = ";", header = T, dec = ",", 
                  colClasses = rep("numeric", 4))

# Correcao dos 0's
agric$Milho[6] <- .0001
agric$Algodao[2] <- .0001

data.frame(
  Milho = c(.25,.26,.34,.40,.47,.52,.61,.73,.80,.85,.86,.88,.88,.90),
  Algodao = c(.61,.69,.71,.73,.76,.79,.83,.87,.86,.88,.93,.9,.94,.9),
  Soja = c(.54,.68,.75,.81,.85,.87,.89,.91,.92,.91,.93,.94,.93,.93),
  Ano = 2000:2013) -> agric_EUA


# COMANDOS ----------------------------------------------------------------

library(proto)
as.lm <- function(object, ...) UseMethod("as.lm")

as.lm.nls <- function(object, ...) {
  if (!inherits(object, "nls")) {
    w <- paste("expected object of class nls but got object of class:", 
               paste(class(object), collapse = " "))
    warning(w)
  }
  
  gradient <- object$m$gradient()
  if (is.null(colnames(gradient))) {
    colnames(gradient) <- names(object$m$getPars())
  }
  
  response.name <- if (length(formula(object)) == 2) "0" else 
    as.character(formula(object)[[2]])
  
  lhs <- object$m$lhs()
  L <- data.frame(lhs, gradient)
  names(L)[1] <- response.name
  
  fo <- sprintf("%s ~ %s - 1", response.name, 
                paste(colnames(gradient), collapse = "+"))
  fo <- as.formula(fo, env = as.proto.list(L))
  
  do.call("lm", list(fo, offset = substitute(fitted(object))))
  
}

linear <- function(data, item){
  
  data <- subset(data, subset = !is.na(get(item)))
  
  # Exponencial
  
  t <- 1:nrow(data)
  linear_exp <- lm(as.formula(paste0("log(1/(1-",item,")) ~ t + 0")),
                              data = data)
  summary(linear_exp)
  coef(linear_exp)
    
  # Regressao linear logistica
  linear_log <- lm(as.formula(paste0("I(log(",item,"/(1-",item,"))) ~ t")),
                   data = data)
  summary(linear_log)
  coef(linear_log)
  
  return(list(resumo_exp = summary(linear_exp),
              resumo_log = summary(linear_log)))
  
}

non_linear <- function(dados, item, plot = F, tipo){
  
  dados <- subset(dados, subset = !is.na(get(item)))
  
  t <- 1:nrow(dados)
  x <- t
  
  # EXP
  linear_exp <- lm(as.formula(paste0("log(1/(1-",item,")) ~ t + 0")),
                   data = dados)
  
  f_exp <- function(x, a) {1 - exp(-a*x)}
  fm_exp <- nls(as.formula(paste0(item," ~ f_exp(x, a)")), 
                data = dados, 
                start = c(a = as.numeric(coef(linear_exp))))
  
  assign("fm_exp", fm_exp, envir = globalenv())
  
  # LOG
  linear_log <- lm(as.formula(paste0("I(log(",item,"/(1-",item,"))) ~ t")),
                   data = dados)
  
  f_log <- function(x, b, k) {(1 + exp(-b*x - k) )^(-1)}
  fm_log <- nls(as.formula(paste0(item," ~ f_log(x, b, k)")),
                data = dados, 
                start = list(b = as.numeric(coef(linear_log)[2]),
                             k = as.numeric(coef(linear_log)[1])))
  
  assign("fm_log", fm_log, envir = globalenv())
  
  if(plot == T){
    resultsC <- data.frame(predict(as.lm.nls(fm_log), interval = "confidence"))
    resultsP <- data.frame(predict(as.lm.nls(fm_log), interval = "prediction"))
    
    
    with(dados, plot((100*Soja)~Ano, type="n", 
                     ylim = c(-20, 120), yaxt = "n",
                     ylab = "Adoção (%)"))
    axis(2, at = seq(0, 100, by = 20), las=2)
    title(main = item)
    # add fill
    polygon(c(dados$Ano, rev(dados$Ano)),
            c(100*resultsP$lwr, 100*rev(resultsP$upr)),
            col = "#D8EDFF", border = FALSE)
    polygon(c(dados$Ano, rev(dados$Ano)),
            c(100*resultsC$lwr, 100*rev(resultsC$upr)),
            col = "#A1C7D6", border = FALSE)
    
    with(dados, points((100*get(item))~Ano, col = "blue"))
    lines(dados$Ano, 100*predict(fm_log))
  }
  
  return(list(resumo_exp = fm_exp,
              resumo_log = fm_log))
}


# SIMULACAO ---------------------------------------------------------------


# Gráfico de simulacao 
# Modelo exponencial de fonte central
x = seq(0, 20, by = .05)
n = length(x)

a = 0.5
plot(curve(100 * (1 - exp(-a*x)), 1, NULL, n = n), 
     type = "l", xlim = c(0, 20), ylim = c(0, 100),
     xlab = "Período", ylab = "Y")

curve(100 * (1 - exp(-a*x)), 1, NULL, 
      n = n, add=T)
text(13.6, 97, expression(alpha[1] == 0.5), cex = .8)

a = 0.2
curve(100 * (1 - exp(-a*x)), 1, NULL, 
      n = n, add=T, lty = 2)
text(17, 93, expression(alpha[2] == 0.2), cex = .8)

a = 0.1
curve(100 * (1 - exp(-a*x)), 1, NULL, 
      n = n, add=T, lty = 3)
text(17.5, 79, expression(alpha[3] == 0.1), cex = .8)

a = 0.05
curve(100 * (1 - exp(-a*x)), 1, NULL, 
      n = n, add=T, lty = 6)
text(18.2, 55, expression(alpha[4] == 0.05), cex = .8)

legend(-0.5, 100, c(1:4), title = "Regiões",
       lty = c(1, 2, 3, 6))


# Modelo logístico de contágio
# k posiciona a curva =
# x é o tempo
# b é a taxa de adoção
# N é teto potencial de inovadores no mercado, tamanho da populacao N = 100
b = 1
plot(1,type = "l", xlim = c(0, 20), ylim = c(0, 100),
     xlab = "Período", ylab = "Y")

curve(100 * (1 + exp(-(b*x)+1.5))^(-1), 0.5, NULL, n = n, add=T)
text(12, 97, expression(beta[1] == 1), cex = .8)

b = 0.4
curve(100 * (1 + exp(-(b*x)+3))^(-1), 0.5, NULL, n = n, add=T, lty = 2)
text(17, 93, expression(beta[2] == 0.4), cex = .8)

b = 0.25
curve(100 * (1 + exp(-(b*x)+3.5))^(-1), 0.5, NULL, n = n, add=T, lty = 3)
text(18, 65, expression(beta[3] == 0.25), cex = .8)

b = 0.1
curve(100 * (1 + exp(-(b*x)+3.5))^(-1), 0.5, NULL, n = n, add=T, lty = 6)
text(17, 10, expression(beta[4] == 0.1), cex = .8)

legend(-0.5, 100, c(1:4), title = "Regiões",
       lty = c(1, 2, 3, 6))

# LINEAR ------------------------------------------------------------------


# Regressao linear exponencial - SOJA
t <- 1:length(na.omit(agric$Soja))
linear_exp <- lm(log(1/(1-Soja)) ~ t + 0, data = agric)
summary(linear_exp)
coef(linear_exp)



resultsC <- data.frame(1 - exp(-1 * predict(linear_exp, 
                                            interval = "confidence")))
resultsP <- data.frame(1 - exp(-1 * predict(linear_exp, 
                                            interval = "prediction")))

# Formula
# y = 1 - exp(coef(linear_exp)*t)

# Soja
# Plotando regressao linear exponencial
with(agric, plot(Soja~Ano, pch = 16))
lines(agric$Ano, resultsC$fit,lwd=2,
      col = "red", xlab = "Ano", ylab = "Soja")

lines(agric$Ano, resultsC$lwr,lwd=1,
      col = "blue", xlab = "Ano", ylab = "Soja")
lines(agric$Ano, resultsC$upr,lwd=1,
      col = "blue", xlab = "Ano", ylab = "Soja")

lines(agric$Ano, resultsP$lwr,lwd=1,
      col = "green", xlab = "Ano", ylab = "Soja")
lines(agric$Ano, resultsP$upr,lwd=1,
      col = "green", xlab = "Ano", ylab = "Soja")


# Regressao linear logistica
linear_log <- lm(I(log(Soja/(1-Soja))) ~ t , data = agric)
summary(linear_log)
coef(linear_log)


resultsC <- data.frame(1/ (1 + exp(-1 * predict(linear_log, 
                                                interval = "confidence"))))
resultsP <- data.frame(1/ (1 + exp(-1 * predict(linear_log, 
                                                interval = "prediction"))))

# Formula
# y = 1 / (1 + exp( (-1) * (Beta * t + k) ) )

# Soja
# Plotando regressao linear exponencial
with(agric, plot(Soja~Ano, pch = 16))
lines(agric$Ano, resultsC$fit, lwd=2,
      col = "red", xlab = "Ano", ylab = "Soja")

lines(agric$Ano, resultsC$lwr,lwd=1,
      col = "blue", xlab = "Ano", ylab = "Soja")
lines(agric$Ano, resultsC$upr,lwd=1,
      col = "blue", xlab = "Ano", ylab = "Soja")

lines(agric$Ano, resultsP$lwr,lwd=1,
      col = "green", xlab = "Ano", ylab = "Soja")
lines(agric$Ano, resultsP$upr,lwd=1,
      col = "green", xlab = "Ano", ylab = "Soja")

# NAO LINEAR --------------------------------------------------------------
# EXP
f <- function(x, a) {1 - exp(-a*x)}

x <- t
# fit a nonlinear model
fm <- nls(Soja ~ f(x, a), data = agric, 
          start = c(a = as.numeric(coef(linear_exp))))

coef(fm)
summary(as.lm.nls(fm))
summary(fm)

resultsC <- data.frame(predict(as.lm.nls(fm), interval = "confidence"))
resultsP <- data.frame(predict(as.lm.nls(fm), interval = "prediction"))


with(agric, plot((100*Soja)~Ano, type="n", 
                 ylim = c(-20, 100), yaxt = "n",
                 ylab = "Adoção (%)"))
axis(2, at = seq(0, 100, by = 20), las=2)
title(main = "Soja")
# add fill
polygon(c(agric$Ano, rev(agric$Ano)),
        c(100*resultsP$lwr, 100*rev(resultsP$upr)),
        col = "#D8EDFF", border = FALSE)
polygon(c(agric$Ano, rev(agric$Ano)),
        c(100*resultsC$lwr, 100*rev(resultsC$upr)),
        col = "#A1C7D6", border = FALSE)

with(agric, points((100*Soja)~Ano, col = "blue"))
lines(agric$Ano, 100*predict(fm))



# LOG
f <- function(x, b, k) {(1 + exp(-b*x - k) )^(-1)}
fm <- nls(Soja ~ f(x, b, k), data = agric, 
          start = list(b = as.numeric(coef(linear_log)[2]),
                       k = as.numeric(coef(linear_log)[1])))

summary(as.lm.nls(fm))

resultsC <- data.frame(predict(as.lm.nls(fm), interval = "confidence"))
resultsP <- data.frame(predict(as.lm.nls(fm), interval = "prediction"))


with(agric, plot((100*Soja)~Ano, type="n", 
                 ylim = c(-20, 100), yaxt = "n",
                 ylab = "Adoção (%)"))
axis(2, at = seq(0, 100, by = 20), las=2)
title(main = "Soja")
# add fill
polygon(c(agric$Ano, rev(agric$Ano)),
        c(100*resultsP$lwr, 100*rev(resultsP$upr)),
        col = "#D8EDFF", border = FALSE)
polygon(c(agric$Ano, rev(agric$Ano)),
        c(100*resultsC$lwr, 100*rev(resultsC$upr)),
        col = "#A1C7D6", border = FALSE)

with(agric, points((100*Soja)~Ano, col = "blue"))
lines(agric$Ano, 100*predict(fm))

# GERAR OUTPUTS ------------------------------------------------------------

# Itens:
# "Algodao" , "Milho" , "Soja"
# Bases:
# agric , agric_EUA

linear(agric_EUA, "Soja")

result <- non_linear(agric_EUA, "Soja")
summary(result$resumo_exp)
summary(as.lm.nls(result$resumo_exp))$r.squared
summary(result$resumo_log)
summary(as.lm.nls(result$resumo_log))$r.squared

non_linear(agric_EUA, "Soja", plot = T)







