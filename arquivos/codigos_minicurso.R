## ----calc1, echo=TRUE,eval=TRUE,include=TRUE-----------------------------
#adição
10 + 15
#subtração
10 - 2
#multiplicação
2*10
#divisão
30/2
#raiz quadrada
sqrt(4)
#potência
2^2


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
x <- 10/2
x
X


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
x <- banana 
x <- "banana"
x


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
banana <- 30
x <- banana
x


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
y <- "ola"
class(y)

x <- 2.5
class(x)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
x <- 20
x
remove(x)
x


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
x <- c(2, 3, 4)
x
y <- seq(1:10)
y
z <- rep(1, 10)
z
a <- 1:10
a
bicho <-c("macaco", "pato", "galinha", "porco")
bicho


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
bicho[2]


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
k <- x * 2
y <- c(x, k)
y


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
peso <- c(62, 70, 52, 98, 90, 70)
altura <- c(1.70, 1.82, 1.75, 1.94, 1.84, 1.61)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
imc <- peso/(altura^2)
imc


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
x <- matrix(seq(1:16), nrow=4,ncol=4)
x
x[2,3] #retorna o elemento na segunda linha e terceira coluna da matriz
x[3,  ]   # seleciona a 3ª linha
x[ , 2]   # seleciona a 2ª coluna
x[1,] <- c(13,15,19,30)  #substituir a primeira linha por (13,15,19,30)

x


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dim(x)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
length(x[1, ])

length(x[, 1])


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
v1 <- c(2, 20, 12, 34)
x2 <- rbind(x, v1)
x2


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
v2 <- c(25, 10, 15, 4) 
x3 <- cbind(x, v2)
x3


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
xa <- x2[1:2, 1:2]
xb <- matrix(rnorm(4), 2, 2)
xa * xb #multiplicacao ponto a ponto

xa %*% xb #multiplicacao matricial

solve(xa) #inversa de xa

diag(xa) #matriz diagonal


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
ID <- seq(1:6)
pes <- c(62, 70, 52, 98, 90, 70)
alt <- c(1.70, 1.82, 1.75, 1.94, 1.84, 1.61)
imc <- pes/(alt^2)
dados <- data.frame(ID = ID,
                    peso = pes,
                    altura = alt, 
                    imc = imc)
dados


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados$altura


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
gr <- c(rep(1,3),rep(2,3))
dados$grupo <- gr

dados


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
names(dados)
str(dados)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
mean(dados$imc)
sd(dados$imc)
summary(dados$imc)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
table(dados$grupo)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
sexo <- c("M", "H", "H", "H", "M", "M", "H")
sex <- as.factor(sexo)
sex
levels(sex)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dim3 <- array(rnorm(18),dim = c(3,3,2))
dim3


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
ls <- list(ls1 = 'a',
           ls2 = c(1, 2, 3),
           ls3 = matrix(rnorm(6), ncol = 3))
ls


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
10 == 11


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
10 != 11


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
x <- 15
x > 10 & x < 30

x < 10 & x < 30


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
x > 10 | x > 30



## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
x <- 15
!x<30


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
a <- 224
b <- 225
if (a == b) { 
  v <- 10
} else {
  v <- 15
}
v


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
a <- 224
b <- 225
if (a == b) { 
  v <- 10
} else if (a > b) {
  v <- 15
} else {
  v <- 25}
v


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
sexo <- c("H", "M", "H", "H", "M")


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
p <- character(length = length(sexo))
for (i in 1:length(sexo)){
  if (sexo[i] == "M") p[i] <- "Mulher"
  else p[i] <- "Homem"
}
p


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
f.soma <- function(x, y) {
  out <- x + y
  return(out)
}


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
f.soma(x = 10, y = 20)

f.soma(10, 20)


## ---- eval=FALSE,include=TRUE,echo=TRUE,error=TRUE-----------------------
## help(mean) #ou
## ?mean


## ----help, echo=FALSE,  out.width = '60%'--------------------------------
knitr::include_graphics("img/help_R.png")


## ---- eval=FALSE,include=TRUE,echo=TRUE,error=TRUE-----------------------
## library(readr) #pacote readr
## dados_csv <- read_csv(file = "dados1.csv")
## dados_txt <- read_delim(file = "caminho-para-o-arquivo/dados1.txt", delim = " ")


## ---- eval=FALSE,include=TRUE,echo=TRUE,error=TRUE-----------------------
## dados_txt2 <- read.table(file="dados1.txt",header=T)


## ---- eval=FALSE,include=TRUE,echo=TRUE,error=TRUE-----------------------
## write_csv(x = mtcars, path = "cars.csv")
## write_delim(x = mtcars, delim = " ", path = "cars.txt"))


## ---- eval=FALSE,include=TRUE,echo=TRUE,error=TRUE-----------------------
## library(readxl)
## dados_excel <- read_xls(path = "dados1.xls") #Leitura do arquivo .xls
## dados_excelx <- read_xlsx(path = "dados1.xlsx") #Leitura do arquivo .xlsx


## ---- eval=FALSE,include=TRUE,echo=TRUE,error=TRUE-----------------------
## library(readxl)
## dados_excel1 <- read_excel(path = "dados1.xls")
## dados_excelx1 <- read_excel(path = "dados1.xlsx")


## ---- eval=FALSE,include=TRUE,echo=TRUE,error=TRUE-----------------------
## library(haven)
## dados_stata <- read_stata("dados1.dta")
## dados_spss <- read_spss("dados1.sav")
## dados_sas <- read_sas("dados1.sas7bdat")


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
library(readxl)
dados <- read_excel(path = "microdados.xls", na = "-")


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE,message=FALSE,results="hide"----
library(tidyverse)
library(janitor)

names(dados)
# a função clean_names() para primeiro ajuste dos nomes das variaveis
dados <- clean_names(dados) 
names(dados)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados <- remove_empty(dados, "rows")


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE,results="hide"---------
dados$coluna_vazia <- ""
dados$coluna_NA <- NA
names(dados)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE,results="hide"---------
dados <- remove_empty(dados, "cols")
names(dados)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE,results="hide"---------
# Ver a estrutura dos dados
str(dados)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE,results="hide"---------
dados$data  <- as.Date(dados$data)
str(dados)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
#do pacote janitor
tabyl(dados, raca_cor) 


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
tabyl(dados, sexo) 


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE, results="hide"--------
library(forcats)
dados$sexo  <- as.factor(dados$sexo)

dados$sexo <- fct_recode(dados$sexo,
           Feminino = "F", 
           Masculino = "H", 
	   Ignorado = "I")

tabyl(dados, sexo)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados$raca_cor <- as.factor(dados$raca_cor)

dados$raca_cor <- fct_recode(dados$raca_cor,
                    Negra = "Parda",
                    Negra = "Preta")

tabyl(dados, raca_cor)

dados$raca_cor <- fct_relevel(dados$raca_cor, "Amarela", "Branca", "Negra",
                             "Indigena", "Ignorado")

tabyl(dados, raca_cor)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE,results="hide"---------
library(skimr)
kable(skim(dados)) 


## ---- eval=FALSE,include=TRUE,echo=TRUE,error=TRUE-----------------------
## dados_resumidos <- arrange(mutate(select(filter(dados, raca_cor == "Negra"), data, faixa_etaria, sexo, evolucao), sexo_idade = paste(sexo, faixa_etaria, sep = " - ")), desc(data))


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados_resumidos <- 
  arrange(
    mutate(
      select(
        filter(
	  dados, raca_cor == "Negra"
        ),
        data, faixa_etaria, sexo, evolucao
      ),
      sexo_idade = paste(sexo, faixa_etaria, sep = " - ")
    ),
    desc(data)
  )


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados_resumidos_pipe <- dados %>%
  filter(raca_cor == "Negra") %>%
  select(data, faixa_etaria, sexo, evolucao) %>%
  mutate(sexo_idade = paste(sexo, faixa_etaria, sep = " - ")) %>% 
  arrange(desc(data))


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
all.equal(dados_resumidos, dados_resumidos_pipe)


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados_comunidades_vitoria <- read.csv("lista_comunidades.csv", sep = ";")


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados_vitoria <- filter(dados, municipio == "VITORIA")


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados_vitoria <- dados_vitoria %>%
  mutate(bairro = iconv(bairro, from = 'UTF-8', to = 'ASCII//TRANSLIT'), 
	 bairro = str_trim(bairro), 
	 bairro = gsub("JESUS DE NAZARETH", "JESUS DE NAZARE", bairro), 
         bairro = gsub("JOANA DARC", "JOANA D'ARC", bairro))


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dados_comunidades_vitoria <- dados_comunidades_vitoria %>%
  mutate(nome_bairro = iconv(nome_bairro, from = 'UTF-8', to = 'ASCII//TRANSLIT'), 
	 nome_bairro = str_trim(nome_bairro),
	 nome_bairro = str_to_upper(nome_bairro))


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE, results="hide"--------
dados_completo <- left_join(dados_vitoria, dados_comunidades_vitoria, by = c("bairro" = "nome_bairro"))
dados_completo


## ---- eval=TRUE,include=TRUE,echo=TRUE,error=TRUE------------------------
dim(dados_completo)[1] == dim(dados_vitoria)[1]

