# Defining working directory
setwd('~/SpiderOak Hive/Pós Graduação/Seminários/7. Minicurso - Antofagasta, Dez-17/')

# Loading packages
library(quantreg)

# Defining seed for reproducibility
set.seed(1)

## Example of regression 1
n <- 100
x <- runif(n)*10
a <- 1
b <- 2
sigma <- 4
y <- a + b*x + rnorm(n, sd = sigma)

quant25 <- sigma * qnorm(0.25) + b*x
quant50 <- sigma * qnorm(0.50) + b*x
quant75 <- sigma * qnorm(0.75) + b*x

pdf('figuras/example1.pdf')
plot(x, y, bty = 'n')
lines(x, quant25, lwd = 2, col = "blue")
lines(x, quant50, lwd = 2, col = "blue")
lines(x, quant75, lwd = 2, col = "blue")
dev.off()

# Considering a scenario where the quantile is important to 
# define the regression parameters

n <- 200
x <- runif(n)*10
a1 <- 10
a2 <- 5
b2 <- 2
sigma <- 4
y <- ifelse(runif(n) < 0.5, 
            a1 + b2*x + rnorm(n, sd = sigma), 
            a2 + rnorm(n, sd = sigma))

coef.model <- lm(y~x)$coef
sigma.model <- summary(lm(y~x))$sigma

quant25 <- sigma.model * qnorm(0.25) + (coef.model[1] + coef.model[2]*x)
quant50 <- sigma.model * qnorm(0.50) + (coef.model[1] + coef.model[2]*x)
quant75 <- sigma.model * qnorm(0.75) + (coef.model[1] + coef.model[2]*x)

pdf('figuras/example2_data.pdf')
plot(x, y, bty = 'n')
dev.off()


pdf('figuras/example2.pdf')
plot(x, y, bty = 'n')
lines(x, quant25, lwd = 2, col = "blue")
lines(x, quant50, lwd = 2, col = "blue")
lines(x, quant75, lwd = 2, col = "blue")
dev.off()


# Estimating this scenario using quantile regression
model.qr <- rq(y~x, tau=1:3/4)$coef

pdf('figuras/example3.pdf')
plot(x, y, bty = 'n')
abline(a = model.qr[1,1], model.qr[2,1], lwd = 2, col = "red")
abline(a = model.qr[1,2], model.qr[2,2], lwd = 2, col = "red")
abline(a = model.qr[1,3], model.qr[2,3], lwd = 2, col = "red")
dev.off()


## Example using Engel data

data(engel)
pdf('figuras/example_RQ.pdf')
plot(foodexp ~ income, data = engel, cex= .5, col = "blue",
     xlab = "Household Income", ylab = "Food Expenditure")
z <- rq(foodexp ~ income, tau= .50, data = engel)
# "median line": L1 - regression
abline(z, col = "dark blue")
#the dreaded ols line
abline(lm(foodexp ~ income, data = engel), lty=2, col="red")
taus <- c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)) {
  abline(rq(foodexp~income, tau=taus[i], data = engel), col="gray")
}
dev.off()

pdf('figuras/resumo_modelo.pdf')
plot(summary(rq(foodexp~income, tau=1:19/20, data = engel)))
dev.off()

## Using package lqr 

library(lqr)

data(engel)
y <- engel$foodexp
X <- cbind(1, engel$income)

pdf('figuras/lqr_bestfit.pdf')
res = best.lqr(y, X, p = 0.50, criterion = "AIC")
dev.off()