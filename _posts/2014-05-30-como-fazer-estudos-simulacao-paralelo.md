---
layout: post
lang: pt
title: Como fazer estudos de simulação utilizando computação em paralelo
---

Estudos de simulação são importantes quando temos o interesse de verificar a performance de um novo método de estimação, ou simplesmente para comparar dois métodos diferentes. Eventualmente, esses estudos podem ser um pouco demorados dependendo do custo computacional dos métodos. Uma alternativa de diminuir esse tempo de processamento é utilizar técnicas que permitem a computação em paralelo. Dessa forma, simplesmente utilizando dois núcleos do processador ao invés de um, o tempo de espera é diminuído pela metade. Pensando em casos extremos onde o tempo para terminar o estudo pode chegar a dias, digamos 20 dias, então distribuindo o estudo em 4 processadores, o que é razoavelmente simples tendo em vista os computadores atuais com até 8 núcleos, faria com que o analista obtivesse os seus resultados em “apenas” 5 dias ao invés dos 20 previstos inicialmente.

No R, isso é bastante fácil de ser realizado. Vou mostrar aqui um exemplo simples, deixando o código organizado para o caso em que estamos interessados em diferentes cenários, o que é bastante comum em casos de publicações, em que diferentes distribuições são utilizadas ou também diferentes tamanhos de amostra.

No meu código, eu não utilizo laços com “for”, pois o mesmo não apresenta boa performance no R e sempre que possível deve ser evitado. Uma opção é utilizar as funções da família “apply”. Dentre as funções disponíveis, “lapply” ou “sapply” são as minhas favoritas. O primeiro retorna um vetor, enquanto que o segundo é interessante quando retornamos objetos de diferentes tipos, o que sugere o uso de uma lista.

A ideia do exemplo feito aqui é bem simples, e o interesse está mais na organização do código e utilização das rotinas em paralelo do que nos resultados do estudo. Ao final de cada simulação vamos apenas calcular o viés um modelo de regressão linear simples com suposição de normalidade dos erros, quando na verdade os dados são gerados pela distribuição t-Student com três graus de liberdade ou Cauchy padrão, e também pela distribuição normal padrão.

Sobre o código, alguns comentários. Primeiro é definido o número de simulações do estudo. No exemplo, apenas para ilustração considerei 100 simulações, mas na literatura esse número varia usualmente entre 100 e 1000 simulações, dependendo do custo computacional do método. Eventualmente é possível encontrar casos de estudos de simulação com 50 repetições. Em segundo lugar, utilizo dois tamanhos de amostra apenas para definir cenários diferentes. Em seguida, defino as três distribuições a serem consideradas no exemplo.

Para possibilitar a computação em paralelo, considero o pacote ```multicore``` do R. Não é a única opção, porém considerando o uso da função ```lapply```, a adaptação para o caso em paralelo é muito simples. Não posso afirmar com certeza se esse pacote funciona no Windows, pois só tenho experiência com ele no Linux.

Então, no início do programa é necessário carregar o pacote.

```R
library(multicore)
```

Em seguida, algumas definições

```R
## Número de simulações no estudo.
num.sim <- 100

## Tamanho da amostras.
tam.amostras <- c(100, 500)

## Distribuições a serem consideradas.
distribuicoes <- c("Normal", "Student", "Cauchy")

## Função que define o erro no exemplo.
erro <- function(n, dist) {
    switch(dist, Normal = rnorm(n), Student = rt(n, df = 3), Cauchy = rcauchy(n))
}
```

Com o pacote carregado, vamos considerar a função ```mclapply``` para distribuir as simulações em paralelo somente no último nível dos casos, conforme o código a seguir.

```R
## Definindo a semente para garantir a reproducibilidade do estudo.
set.seed(123)

## Código para o estudo de simulação.
resultado.Simulacoes <- lapply(tam.amostras, function(a) {
    lapply(distribuicoes, function(b) {
        mclapply(1:num.sim, function(c) {
            x <- runif(a)
            y <- x + erro(a, b)

            # Calculando o viés da estimativa para o coeficiente da variável x
            as.numeric(coef(lm(y ~ x))[2]) - 1
        }, mc.cores = 4)  # Utilizando quatro núcleos do processador.
    })
})
```

Para fazer os gráficos, analisando os resultados utilizo o pacote ```ggplot2```. Então, primeiro devemos carregar o pacote no R.

```R
library(ggplot2)
```

Para utilizar as diversas facilidades que o pacote ```ggplot2``` proporciona, antes é necessário colocar os resultados da simulação em um objeto ```data.frame```, o que pode ser feito da seguinte forma.

```R
resultados <- data.frame(vies = unlist(resultado.Simulacoes), distribuicoes = rep(distribuicoes, 
    each = num.sim, times = length(tam.amostras)), tam.amostras = rep(tam.amostras, 
    each = num.sim * length(distribuicoes)))
```

Em seguida, podemos analisar o gráfico da distribuição do viés para cada cenário considerado, por exemplo fazendo

```R
g <- ggplot(resultados) + theme_bw()
g + geom_histogram(aes(x = vies, y = ..density..), fill = "grey75", colour = "black", 
    binwidth = 20/30) + facet_grid(tam.amostras ~ distribuicoes) + xlim(c(-10, 
    10))
```

![Gráfico com resultado das simulações](unnamed-chunk-6.png)

Porém, devido a grande variabilidade do viés quando a distribuição é Cauchy, os gráficos para as distribuições Normal e t-Student com três graus de liberdade ficam pouco informativos. Uma opção é fazer a análise em dois passos. Primeiramente somente com o gráfico dessas duas distribuições.

```R
g1 <- ggplot(subset(resultados, distribuicoes != "Cauchy")) + theme_bw()
g1 + geom_histogram(aes(x = vies, y = ..density..), fill = "grey75", colour = "black", 
    binwidth = 3/30) + facet_grid(tam.amostras ~ distribuicoes) + xlim(c(-1.5, 
    1.5))
```

![Gráficos com os resultados das simulações retirando a distribuição Cauchy](unnamed-chunk-7.png)

Em seguida, para a distribuição Cauchy.

```R
g2 <- ggplot(subset(resultados, distribuicoes == "Cauchy")) + theme_bw()
g2 + geom_histogram(aes(x = vies, y = ..density..), fill = "grey75", colour = "black", 
    binwidth = 1) + facet_grid(tam.amostras ~ distribuicoes) + xlim(c(-15, 15))
```

![Gráficos com os resultados das simulações somente com a distribuição Cauchy](unnamed-chunk-8.png)

Notem que eu fixei os limites de visualização no eixo x apenas para facilitar a visualização dos resultados. O mesmo para o tamanho das barras na função ```geom_histogram```.

Ainda sobre os resultados, usualmente não se reporta a distribuição desse viés, ou outra estatística qualquer que tenha sido utilizada no estudo de simulação, porém somente algumas medidas resumo. Isso pode ser feito utilizando o pacote ```doBy```.

```R
library(doBy)
```

E considerando a função summaryBy, podemos obter a média e o desvio padrão dos vieses para cada cenário considerado no estudo.

```R
summaryBy(vies ~ distribuicoes + tam.amostras, data = resultados, FUN = c(mean, sd))
```

A ideia desse post não era exatamente avaliar o viés dos modelos de regressão quando a variável resposta não é normalmente distribuída, mas sim mostrar algumas formas de organizar o seu código para obter resultados mais rápidos com a utilização da computação em paralelo. E também algumas maneiras de analisar os resultados do estudo.