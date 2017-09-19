## Programa para comparação entre estimador de máxima verossimilhança
## e estimador de Bayes

# Problema: Modelo Binomial com priori Beta

# Definindo semente
set.seed(3)

# Priori: Beta(a,b)
a <- 1
b <- 1

par(mfrow=c(1,3))
curve(dbeta(x, a, b), 0, 1, xlab = expression(theta), main = "Priori")

# Amostra
n <- 15
theta <- 0.5

n_simulacoes <- 2000

resultadosSimulacoes <- t(sapply(1:n_simulacoes, function(a){
  y <- rbinom(n, 1, theta)
  
  # Estimador de máxima verossimilhança
  thetaMV <- sum(y)/n
  
  # f <- function(x, n, somaY){
  #   ((x)^(somaY))*((1-x)^(n-somaY))
  # }
  # 
  # curve(f(x, n, sum(y)), 0, 1, xlab = expression(theta), 
  #       main = "Verossimilhança")
  # abline(v=thetaMV)
  # abline(v=theta, col=2)
  
  # Estimador de Bayes, segundo perda quadrática.
  thetaB <- (a + sum(y))/(a+b+n)
  
  ## Distribuição a posteriori
  # curve(dbeta(x, a + sum(y), b + n - sum(y)), 0, 1, 
  #       xlab = expression(theta), main = "Posteriori")
  # abline(v=thetaB)
  # abline(v=theta, col=2)
  
  viesMV <- theta - thetaMV
  viesB <- theta - thetaB
  
  EQM_MV <- (theta - thetaMV)^2
  EQM_B <- (theta - thetaB)^2
  
  c(viesMV, viesB, EQM_MV, EQM_B)
}))

resultadosDF <-data.frame(vicio = as.numeric(resultadosSimulacoes[,1:2]), 
                          EQM = as.numeric(resultadosSimulacoes[,3:4]),
                          abordagem = rep(c("MV", "Bayes"), 
                                            each = n_simulacoes))

library(ggplot2)

g <- ggplot(resultadosDF) + theme_bw()
g + geom_histogram(aes(x = vicio)) + facet_wrap(~abordagem)
g + geom_histogram(aes(x = EQM)) + facet_wrap(~abordagem)


## Modelo Poisson com priori gamma

# 