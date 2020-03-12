
#' ### Dados sobre músicas do Raul Seixas no Spotify
#' 
#' Como primeira ilustração, considere os dados que podem ser obtidos sobre a plataforma digital Spotify, a partir de API disponibilizada pelos responsáveis por essa rede social de _streaming_ de músicas. A partir de uma obtenção de uma chave para acesso, é possível baixar diferentes variáveis de diferentes artistas, diferentes álbuns, as quais podem ser utilizadas para análise dos gostos musicais das pessoas, assim como dos diferentes estilos musicais e suas respectivas características, etc. Para essa ilustração consideremos dados do artista **Raul Seixas**, que é considerado por alguns como um dos precursores do rock no Brasil. Para obtenção de algumas informações pela API disponibilizada pelo Spotify, podemos obter os dados da seguinte maneira. Primeiramente, consideramos os seguintes pacotes, em que o primeiro nos permite o acesso às informações do Spotify de forma mais rápida, enquanto que o segundo nos permite organizar os dados no R de maneira mais ágil, e o terceiro nos possibilita o acesso dos dados da plataforma digital:
#' 
## ----packages, message=FALSE---------------------------------------------
library(Rspotify)
library(dplyr)
library(httr)

#' Em seguida, consideramos a variável `key` que deve ser obtida diretamente [aqui](https://developer.spotify.com/documentation/general/guides/authorization-guide/). Em posse dessa chave de acesso, podemos obter nossas informações de interesse:
#' 
## ----auth, message=FALSE, echo=FALSE-------------------------------------
library(httr)
spotifyEndpoint <- oauth_endpoint(NULL, "https://accounts.spotify.com/authorize", "https://accounts.spotify.com/api/token")

key = spotifyOAuth("r_bruno", 
                   client_id = "772e7ec6210f4ed2bd0b3a08caa45b04",
                   client_secret = "f4396c426579471d920eb71b789500c3")

 
## ----dados, message=FALSE------------------------------------------------
# Obtendo a id de identificação do artista
raul_seixas_id <- searchArtist("Raul+Seixas", token = key) %>%
  slice(1) %>%
  select(id)

# Obtendo o id e o nome de todos os álbuns do artista disponíveis na plataforma
rs_albums <- getAlbums(raul_seixas_id$id, token = key)

# Obtendo para cada um dos álbuns obtidos anteriormente, o id e o nome de 
# cada uma das músicas que compõem o álbum.
rs_album_songs <- lapply(rs_albums$id, getAlbum, token = key)

# Obtendo algumas informações adicionais sobre o album.
rs_album_songs_info <- lapply(rs_albums$id, getAlbumInfo, token = key)

 
#' Em seguida, precisamos obter todas as informações das músicas que queremos analisar. Isso é feita com ajuda das funções `lapply`, pois precisamos considerar os diferentes `id`'s dos álbuns e em seguida de cada uma das músicas. O seguinte código pode ser utilizado: 
#' 
## ----musicas, message=FALSE----------------------------------------------
lista_musicas_info <- lapply(rs_album_songs, function(a){
  musicas_album_a <- lapply(a$id, getFeatures, token = key)
  num_variaveis <- dim(musicas_album_a[[1]])[2]
  dados_musicas <- musicas_album_a %>% 
    unlist() %>%
    matrix(ncol = num_variaveis, byrow = TRUE) %>%
    as.data.frame() 
  names(dados_musicas) <- names(musicas_album_a[[1]])
  dados_musicas %>%
    select(danceability, energy, key, loudness, mode, speechiness, 
           acousticness, instrumentalness, liveness, valence, tempo,
           duration_ms, time_signature) %>%
    mutate(nome = a$name)
}) 

# Empilhando as músicas de todos os albuns em um mesmo banco de dados.
todas_musicas <- do.call(rbind.data.frame, lista_musicas_info)

# Preenchendo os dados com nome do album
todas_musicas$nome_album <- lapply(1:length(rs_album_songs), function(a){
  rep(rs_albums$name[a], length(rs_album_songs[[a]]$id))
}) %>% unlist()


# Obtendo data de lançamento
todas_musicas$data_lancamento <- lapply(1:length(rs_album_songs), function(a){
  rep(rs_album_songs_info[[a]]$release_date[1], length(rs_album_songs[[a]]$id))
}) %>% unlist()

# Criando variável ano de lançamento a partir de variável de texto
todas_musicas$ano_lancamento <- substr(todas_musicas$data_lancamento, 1, 4)

#' Para esse exemplo consideramos somente as músicas que apresentam ano de lançamento menor do que 1990, segundo informações obtidas na própria plataforma. 
#' 
## ----filtro--------------------------------------------------------------
musicas_raul_seixas <- filter(todas_musicas, ano_lancamento < "1990")

#' 
#' Agora podemos finalmente visualizar algumas das informações obtidas. Para essa parte, consideramos os seguintes pacotes. 
#' 
## ----pacotes2, message=FALSE---------------------------------------------
library(ggplot2)
library(ggridges)
library(rgdal)


#' Por exemplo, poderíamos estar interessados no tempo de duração das músicas ao longo dos anos. Primeiramente, consideramos a variável tempo de duração em sua forma quantitativa. O seguinte gráfico com a densidade estimada do tempo de duração das músicas para os diferentes anos de lançamento poderia ser obtido com o seguinte código
#' 
## ----graf1, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE----
musicas_raul_seixas %>% 
  mutate(duration = as.numeric(as.character(duration_ms))/1000) %>%
  ggplot(aes(y = ano_lancamento, x = duration, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, 
                               gradient_lwd = 1.) +
  labs(
    x = "Duração em s",
    y = "Ano do lançamento",
    title = "Distribuição da duração das músicas do Raul Seixas",
    subtitle = "Análise em função do ano de lançamento",
    caption = "Fonte: Spotify"
  ) +
  scale_fill_viridis_c() +
  theme_minimal() + 
  theme(legend.position = 'none') 

#' 
#' Nota-se que não é possível obter nenhuma indicação clara de relação entre a distribuição do tempo e o ano do lançamento. Poderíamos categorizar o tempo de duração para simplificar essa análise. Nesse caso talvez fosse interessante considerar uma categorização das músicas considerando somente 3 possibilidades: duração curta, média ou longa. Essa categorização poderia ser feita inclusive utilizando os quantis da própria variável utilizando a função `quantcut` do pacote `gtools`. O resultado dessa categorização pode ser vista na figura a seguir.
#' 
## ----graf2, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
musicas_raul_seixas %>% 
  mutate(duration = as.numeric(as.character(duration_ms))/1000, 
         duration_cat = 
           gtools::quantcut(duration, 3, 
                            labels = c("Curta", "Média", "Longa"))) %>%
  ggplot(aes(x = ano_lancamento, fill = duration_cat)) +
  geom_bar(position = 'fill') +
  labs(
    y = "",
    x = "Ano do lançamento",
    title = "Distribuição da duração das músicas do Raul Seixas",
    subtitle = "Análise em função do ano de lançamento",
    caption = "Fonte: Spotify"
  ) +
  scale_fill_brewer(palette = 'Greens', 
                    name = "Cat. duração") +
  theme_minimal()


#' > O tom geral estimado da música. Os valores nesse campo variam de 0 a 11, mapeando para os tons de acordo com uma notação clássica de tom (e.g., 0 = Dó, 1 = Dó#, ...). Se nenhum tom é detectado, o valor é igual a -1.
#' 
## ----graf3, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
musicas_raul_seixas %>% 
  mutate(tom = factor(as.numeric(as.character(key)), 
                      labels = c("Dó", "Dó#", "Ré", 
                                 "Ré#", "Mi", "Fá", "Fá#", "Sol", "Sol#", 
                                 "Lá", "Lá#", "Mi"))) %>%
  ggplot(aes(x = ano_lancamento, fill = tom)) +
  geom_bar(position = 'fill') +
  labs(y = "",
       x = "Ano do lançamento",
       title = "Distribuição do tom das músicas do Raul Seixas",
       subtitle = "Análise em função do ano de lançamento",
       caption = "Fonte: Spotify"
  ) +
  scale_fill_viridis_d(name = "Cat. duração") +
  theme_minimal() 

#' Outra análise que poderia ser feita seria avaliar a relação entre o tempo da música, considerando essa variável categorizada, e o tom utilizado. Para isso, em uma análise descritiva poderíamos considerar o gŕafico a seguir. 
#' 
## ----graf4, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
musicas_raul_seixas %>% 
  mutate(tom = factor(as.numeric(as.character(key)), 
                      labels = c("Dó", "Dó#", "Ré", 
                                 "Ré#", "Mi", "Fá", "Fá#", "Sol", "Sol#", 
                                 "Lá", "Lá#", "Mi")),
         duration = as.numeric(as.character(duration_ms))/1000, 
         duration_cat = gtools::quantcut(duration, 3, 
                                         labels = c("Curta", "Média", "Longa"))) %>%
  ggplot(aes(x = duration_cat, fill = tom)) +
  geom_bar(position = 'fill') +
  labs(y = "",
       x = "Duração da música (cat.)",
       title = "Distribuição do tom das músicas do Raul Seixas",
       subtitle = "Análise em função da duração da música",
       caption = "Fonte: Spotify"
  ) +
  scale_fill_viridis_d(name = "Tom") +
  theme_minimal() 

#' 
#' Porém a visualização dos resultados nesse caso, parece ser um pouco prejudicada, devido ao número muito maior de tons do que de categorias de duração da música. Por esse motivo, uma outra possibilidade é alterar o modo de visualização. Poderíamos observar a distribuição da duração da música em função dos tons da música, conforme figura a seguir.
#' 
## ----graf5, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
musicas_raul_seixas %>% 
  mutate(tom = factor(as.numeric(as.character(key)), 
                      labels = c("Dó", "Dó#", "Ré", 
                                 "Ré#", "Mi", "Fá", "Fá#", "Sol", "Sol#", 
                                 "Lá", "Lá#", "Mi")),
         duration = as.numeric(as.character(duration_ms))/1000, 
         duration_cat = gtools::quantcut(duration, 3, 
                                         labels = c("Curta", "Média", "Longa"))) %>%
  ggplot(aes(fill = duration_cat, x = tom)) +
  geom_bar(position = 'fill') +
  labs(y = "",
       x = "Tom da música",
       title = "Distribuição da duração das músicas do Raul Seixas",
       subtitle = "Análise em função do tom da música",
       caption = "Fonte: Spotify"
  ) +
  scale_fill_viridis_d(name = "Cat. duração") +
  theme_minimal()


#' Separemos inicialmente o tempo de hospitalização até 3 dias, entre 3 dias e até 7 dias e mais do que 7 dias. A depender do interesse, outras categorizações poderiam ser utilizadas para o efeito dessa análise. A tabela com as respectivas quantidades é apresentada em seguida:
#' 
## ----nice-tab, tidy=FALSE, echo=FALSE, message=FALSE, warning=FALSE------
data_to_look <- read.csv('../notas_livro/dataHospitals_df.csv') %>%
  mutate(tempo_internacao = factor(ifelse(DIAS_PERM <= 3, "Menor que 3 dias", 
                                   ifelse(DIAS_PERM <= 7, "Entre 3 e 7 dias", 
                                          "Maior que 7 dias")), 
                                   levels = c("Menor que 3 dias", 
                                              "Entre 3 e 7 dias", 
                                              "Maior que 7 dias")))

knitr::kable(
  table(data_to_look$UFSIGLA, data_to_look$tempo_internacao),
  booktabs = TRUE, format.args = list(big.mark = ".")
)

#' Se fizermos isso, o resultado no gráfico com os mapas ficariam da seguinte forma
#' 
## ----graf6, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
distBrasil <- readOGR(dsn='~/SpiderOak Hive/Brazil_data/Maps/estados_2010/', 
                      layer='estados_2010', verbose = FALSE)

distBrasil@data$id = rownames(distBrasil@data)
distBrasil.points = fortify(distBrasil, region="id")
distBrasil.df = inner_join(distBrasil.points, distBrasil@data, by="id")

data_summary <- data_to_look %>%
  group_by(UFSIGLA, tempo_internacao) %>%
  summarise(total = n()) %>%
  mutate(perc = total / sum(total))  

data_plot <- distBrasil.df %>%
    left_join(data_summary, by = c('sigla' = 'UFSIGLA'))
  
ggplot(data_plot) + theme_bw() +
  geom_polygon(aes(x = long, y = lat, group = group,  fill = perc)) +
  geom_path(aes(long, lat, group = group), color="black") + 
  coord_equal() + 
  theme_minimal() + 
  scale_fill_viridis_c("% de internação") + 
  facet_wrap(~ tempo_internacao)

#' 
#' Porém, note agora que devido a diferença de escala entre as três categorias, não é possível perceber a diferença entre os três mapas. No entanto, não é necessário observar essas três proporções, porque elas são complementares, isto é, a soma das três é igual a 1. Nesse caso, podemos focar nas duas categorias à direita que têm escalas muito mais próximas. 
#' 
## ----graf7, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
data_summary <- data_to_look %>%
  group_by(UFSIGLA, tempo_internacao) %>%
  summarise(total = n()) %>%
  mutate(perc = total / sum(total))  

data_plot <- distBrasil.df %>%
    left_join(data_summary, by = c('sigla' = 'UFSIGLA'))
  
ggplot(filter(data_plot, tempo_internacao != "Menor que 3 dias")) + theme_bw() +
  geom_polygon(aes(x = long, y = lat, group = group,  fill = perc)) +
  geom_path(aes(long, lat, group = group), color="black") + 
  coord_equal() + 
  theme_minimal() + 
  scale_fill_viridis_c("% de internação") + 
  facet_wrap(~ tempo_internacao) + 
  theme(legend.position = 'bottom')


#' Inicialmente, podemos identificar o resultado da partida como uma variável aleatória categórica, em que podemos classificar os seus possíveis valores como: "C", vitória do time da casa; "E", empate; "F", vitória do time visitante. Se observarmos a distribuição dessas variáveis ao longo dos anos, veremos o resultado na figura a seguir.
#' 
## ----graf8, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
## Exemplo Campeonato Brasileiro 
brasileirao_df <- read.csv('../notas_livro/brasileirao_df.csv')

## Removendo um jogo sem resultado (Chapecoense após o acidente de avião)
brasileirao_df <- brasileirao_df %>%
  filter(Res != "")

brasileirao_df %>% 
  mutate(res_pt = ifelse(Res == "A", "F", 
                         ifelse(Res == "D", "E", "C"))) %>%
  ggplot(aes(x = Season, fill = res_pt)) +
  geom_bar(position = 'fill') +
  labs(y = "Distribuição dos resultados",
       x = "Temporada",
       title = "Distribuição dos resultados do Camp. Brasileiro 2012-2019",
       subtitle = "Análise em função dos anos",
       caption = "Fonte: football-data.co.uk"
  ) +
  scale_fill_viridis_d(name = "Resultado") +
  theme_minimal() 

#' Uma outra análise simples que poderia ser feita seria a comparação do desempenho dos campeões com os demais participantes em cada um dos anos. Por exemplo, será que a distribuição dos resultados do campeão ao longo dos anos teve uma grande variação para esses certames analisados? Se fizermos isso, o resultado seria o seguinte: 
#' 
## ----graf9, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----

campeoes_df <- data.frame(Season = seq(2012, 2019, 1), 
                          campeao = c("Fluminense", "Cruzeiro", "Cruzeiro", 
                                   "Corinthians", "Palmeiras", "Corinthians", 
                                   "Palmeiras", "Flamengo RJ"))

brasileirao_df <- brasileirao_df %>%
  left_join(campeoes_df)

brasileirao_df <- brasileirao_df %>%
  mutate(campeao_dummy = ifelse(as.character(campeao) == as.character(Home) | 
                                  as.character(campeao) == as.character(Away), 
                                "Campeão", "Não campeão"))

brasileirao_df %>% 
  mutate(res_pt = ifelse(Res == "A", "F", 
                         ifelse(Res == "D", "E", "C"))) %>%
  ggplot(aes(x = Season, fill = res_pt)) +
  geom_bar(position = 'fill') +
  labs(y = "Distribuição dos resultados",
       x = "Temporada",
       title = "Distribuição dos resultados do Camp. Brasileiro 2012-2019",
       subtitle = "Análise em função dos anos",
       caption = "Fonte: football-data.co.uk"
  ) +
  scale_fill_viridis_d(name = "Resultado") +
  facet_wrap(~ campeao_dummy, ncol = 2) +
  theme_minimal() 

#' Para alguns valores fixados de $n$ e $p$ podemos utilizar o `R` para fazer o gráficos das respectivas funções de probabilidade. Por exemplo, se fizermos $n = 10$ e $p$ variando entre 0,2, 0,5 e 0,8, encontraríamos o seguinte:
#' 
## ----graf10, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE----
valores_p <- c(0.2, 0.5, 0.8)
n <- 10

valores_fp <- sapply(valores_p, function(x) dbinom(0:n, prob = x, size = n)) %>%
  as.numeric()

dados_figura <- data.frame(x = rep(0:n, length(valores_p)),
                           n = rep(n, length(valores_fp)), 
                           p = rep(valores_p, each = n + 1), 
                           valores_prob = valores_fp)

ggplot(dados_figura) + theme_minimal() + 
  geom_bar(aes(x = x, y = valores_prob, fill = factor(p)), 
           stat = 'identity', width = 0.5, position = 'dodge') + 
  scale_fill_viridis_d(name = "p = ") + 
  facet_wrap(~ p) +
  scale_x_continuous(breaks = 0:n) + 
  labs(title = "Distribuição Binomial para n = 10", 
       subtitle = "E p igual a 0,2, 0,5 ou 0,8",
       y = "P(W = w)",
       x = "w") +
  theme(legend.position = 'bottom', 
        strip.background = element_blank(), 
        strip.text.x = element_blank())

#' Ainda sobre a distribuição Binomial, um resultado importante que podemos citar se refere a aproximação dessa distribuição de probabilidade pela distribuição Normal, quando $n \rightarrow \infty$, isto é, quando $n$ cresce. Considere o gráfico anterior, por exemplo, quando consideramos $n = 100$, desconsiderando o valor de $p = 0,5$. 
#' 
## ----graf11, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
valores_p <- c(0.2, 0.8)
n <- 100

valores_fp <- sapply(valores_p, function(x) dbinom(0:n, prob = x, size = n)) %>%
  as.numeric()

dados_figura <- data.frame(x = rep(0:n, length(valores_p)),
                           n = rep(n, length(valores_fp)), 
                           p = rep(valores_p, each = n + 1), 
                           valores_prob = valores_fp)

ggplot(dados_figura) + theme_minimal() + 
  geom_bar(aes(x = x, y = valores_prob, fill = factor(p)), 
           stat = 'identity', width = 0.5, position = 'dodge') + 
  scale_fill_viridis_d(name = "p = ") + 
  scale_x_continuous(breaks = 0:10*10) + 
  labs(title = "Distribuição Binomial para n = 100", 
       subtitle = "E p igual a 0,2 ou 0,8",
       y = "P(W = w)",
       x = "w") +
  theme(legend.position = 'bottom', 
        strip.background = element_blank(), 
        strip.text.x = element_blank())

#' Consideremos o caso em que $k = 3$, então podemos visualizar o gráfico com essa função de probabilidade de forma mais simples. Considere o caso em que $n = 10$, então supondo o vetor de probabilidades $(p_1, p_2, p_3) = (0,4, 0,4, 0,2)$ teríamos o seguinte gráfico da distribuição de probabilidade 
#' 
## ----graf12, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE----
n <- 10

y_1 <- 0:n
y_2 <- 0:n

grid_y <- expand.grid(y_1, y_2)
names(grid_y) <- c("y_1", "y_2")

grid_y <- grid_y[apply(grid_y, 1, sum) <= n, ]

grid_y$y_3 <- n - apply(grid_y, 1, sum)

valores_prob_par <- c(0.4, 0.4, 0.2)
valores_prob <- apply(grid_y, 1, dmultinom, size = n, prob = valores_prob_par)

grid_y$valores_prob <- valores_prob  

ggplot(grid_y) + theme_minimal() + 
  geom_tile(aes(x = y_1, y = y_2, fill = valores_prob)) + 
  scale_fill_viridis_c(name = "Escala de valores para probabilidades") + 
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = 0:10) + 
  theme(legend.position = 'bottom') +
  coord_fixed() + 
  xlab(expression(y[1])) +
  ylab(expression(y[2]))

#' Note, por exemplo, que algumas combinações não são possíveis. Por exemplo, para $y_1 = 6$ e $y_2 = 5$ não é possível calcular nenhum valor de probabilidade, pois isso violaria a condição que $\sum_{i=1}^k n_i = n$. Se considerássemos outro valor para o vetor de probabilidades, como $(p_1, p_2, p_3) = (0,10, 0,25, 0,65)$ obteríamos o seguinte
#' 
## ----graf13, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE, results='hide'----
n <- 10

y_1 <- 0:n
y_2 <- 0:n

grid_y <- expand.grid(y_1, y_2)
names(grid_y) <- c("y_1", "y_2")

grid_y <- grid_y[apply(grid_y, 1, sum) <= n, ]

grid_y$y_3 <- n - apply(grid_y, 1, sum)

valores_prob_par <- c(0.1, 0.25, 0.65)
valores_prob <- apply(grid_y, 1, dmultinom, size = n, prob = valores_prob_par)

grid_y$valores_prob <- valores_prob  

ggplot(grid_y) + theme_minimal() + 
  geom_tile(aes(x = y_1, y = y_2, fill = valores_prob)) + 
  scale_fill_viridis_c(name = "Escala de valores para probabilidades") + 
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = 0:10) + 
  theme(legend.position = 'bottom') +
  coord_fixed() + 
  xlab(expression(y[1])) +
  ylab(expression(y[2]))

#' Nesse caso, temos que $E(Y) = a/(a+b)$ e $Var(Y) = (a+b)/[(a+b)^2(a+b+1)]$. Podemos usar o `R` para visualizar alguns exemplos dessa densidade fazendo
#' 
## ----graf14, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=TRUE----
numero_pontos <- 100
list_parametros <- list(c(1, 1), 
                        c(3, 1), 
                        c(1, 3), 
                        c(4, 4), 
                        c(10, 10))

densidade_beta <- lapply(list_parametros, 
                       function(x) dbeta(0:numero_pontos/numero_pontos, 
                                         x[1], x[2])) %>%
  unlist()

dados_beta <- data.frame(x = rep(0:numero_pontos/numero_pontos, 
                                 length(list_parametros)),
                              densidade = densidade_beta, 
                              parametros = rep(as.character(list_parametros), 
                                          each = numero_pontos + 1))


ggplot(dados_beta, aes(x = x, y = densidade, color = parametros)) +
  theme_minimal() + 
  geom_line() + 
  scale_color_discrete(name = "(a, b)")

#' 
#' Essa distribuição de probabilidade é particularmente importante para os métodos de inferência bayesiana que discutiremos na próxima seção. Para fazer o gráfico da densidade, há o problema da dimensionalidade, porém podemos ver alguns exemplos para o caso trivariado nas figuras, conforme variamos os valores de $\boldsymbol \alpha$. 
## ----graf15, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE, results='hide'----
x_1 <- seq(0.001, 0.999, length.out = 150)
x_2 <- seq(0.001, 0.999, length.out = 150)

grid_x <- expand.grid(x_1, x_2)
names(grid_x) <- c("x_1", "x_2")

grid_x <- grid_x[apply(grid_x, 1, sum) <= 1, ]

grid_x$x_3 <- 1 - apply(grid_x, 1, sum)

list_alpha <- list(c(1, 1, 1), 
                   c(3, 2, 1), 
                   c(1, 2, 3), 
                   c(3, 3, 3),
                   c(10, 10, 10))

densidade_dirichlet <- lapply(list_alpha, function(a) {
  gtools::ddirichlet(grid_x, a)
}) %>% unlist()

dados_dirichlet <- data.frame(x_1 = rep(grid_x$x_1, length(list_alpha)), 
                              x_2 = rep(grid_x$x_2, length(list_alpha)),
                              x_3 = rep(grid_x$x_3, length(list_alpha)),
                              densidade = densidade_dirichlet, 
                              alpha = rep(as.character(list_alpha), 
                                          each = dim(grid_x)[1]))

plot_dirichlet <- function(alpha){
  filtering <- dados_dirichlet$alpha == alpha & 
    is.finite(dados_dirichlet$densidade)
  plot3D::lines3D(x = dados_dirichlet$x_1[filtering],
                  z = dados_dirichlet$x_2[filtering], 
                  y = dados_dirichlet$x_3[filtering], 
                  colvar = dados_dirichlet$densidade[filtering], 
                  theta = 135, 
                  phi = 10, 
                  xlab = "y_1", 
                  ylab = "y_2", 
                  zlab = "y_3", 
                  main = paste0("Alpha = ", alpha))
}

lapply(as.character(list_alpha), plot_dirichlet)

#' Note que a equação acima está definida para diferentes valores de $p$. Inclusive podemos colocar essa informação em um gráfico, da seguinte forma:
#' 
## ----vero_bin, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=TRUE----
verossimilhanca_bin <- function(p, n, y){
  dbinom(y, size = n, prob = p) 
}

curve(verossimilhanca_bin(x, n = 6, y = 2), 
      ylab = "", xlab = "")


#' Para ilustrar algumas características desses estimadores, podemos discutir alguns resultados tendo em vista valores gerados da distribuição Binomial, com valor $p$ conhecido e fixado. Consideremos $p = 0,30$ e geramos valores de amostra com três tamanhos de amostra diferentes. No primeiro caso geramos 20 valores de $Y \sim Bin(n = 20, 0,3)$, enquanto no segundo caso geramos 100 valores dessa mesma variável aleatória e no terceiro 200 valores são gerados. Os resultados podem ser observados no gráfico a seguir. 
#' 
## ----vero_bin2, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=TRUE, warning=FALSE----
set.seed(42)

n <- c(20, 100, 200)
seq_p <- seq(0, 1, length.out = 250)

valores_verossimilhanca <- lapply(n, function(x){
  valores_amostrais <- rbinom(x, size = 20, prob = 0.3)
  
  vero_calc <- function(p){
    sapply(valores_amostrais, 
           dbinom, size = 20, prob = p, log = TRUE) %>%
      sum() %>%
      exp()
  }
  
  sapply(seq_p, vero_calc)
}) %>% unlist()

dados_verossimilhanca <- data.frame(valores = valores_verossimilhanca, 
                                    p = rep(seq_p, times = length(n)),
                                    tam_amostra = rep(n, each = length(seq_p)))

ggplot(dados_verossimilhanca, aes(p, valores_verossimilhanca)) + 
  theme_minimal() + 
  geom_line() + 
  facet_wrap(~ tam_amostra, scales = 'free') + 
  xlim(c(0.25, 0.35)) + ylab("Verossimilhança") + xlab("Valores de p") + 
  geom_vline(aes(xintercept = 0.3), linetype = 2, size = 0.5, color = 'grey50')

#' O resultado anterior indica que a distribuição a posteriori de $p$, quando consideramos uma distribuição a priori, $p \sim \mbox{Beta}(a, b)$, também tem distribuição Beta com parâmetros $y + a$ e $n - y + b$, quando consideramos para a verossimilhança $Y|p \sim Bin(n, p)$. Considerando novamente o cenário em que obtivemos 2 sucessos em 6 eventos independentes com distribuição Bernoulli, com probabilidade $p$,podemos comparar a distribuição a posteriori para diferentes valores de $a$ e $b$. No gráfico a seguir, a linha preta representa a função de verossimilhança, enquanto que as linhas coloridas representam as quantidades consideradas pela atualização bayesiana: a linha pontilhada colorida representa a informação a priori, enquanto que a respectiva linha cheia colorida representa a distribuição a priori. 
#' 
## ----posteriori_exemplo, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=TRUE, warning=FALSE----
ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  theme_minimal() +
  stat_function(fun = verossimilhanca_bin, args = list(n = 6, y = 2)) + 
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 5), 
                color = 'red') +
  stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 1), 
                color = 'red', linetype = 2) + 
  stat_function(fun = dbeta, args = list(shape1 = 4, shape2 = 6), 
                color = 'blue') +
  stat_function(fun = dbeta, args = list(shape1 = 2, shape2 = 2), 
                color = 'blue', linetype = 2) + 
  stat_function(fun = dbeta, args = list(shape1 = 5, shape2 = 6), 
                color = 'green') +
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 2), 
                color = 'green', linetype = 2)

#' 
#' Note que a depender da distribuição a priori, a distribuição a posteriori apresenta diferentes formas e também diferentes estimativas, a depender da função de perda de interesse. Compare agora com a situação supondo que o experimento independente Bernoulli foi repetido mais vezes, i.e., $n = 60$ e $y = 20$, porém observando ainda a mesma proporção de _sucessos_.  
#' 
## ----posteriori_exemplo2, out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, echo=FALSE, warning=FALSE----
ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  theme_minimal() +
  stat_function(fun = verossimilhanca_bin, args = list(n = 60, y = 20)) + 
  stat_function(fun = dbeta, args = list(shape1 = 21, shape2 = 41), 
                color = 'red') +
  stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 1), 
                color = 'red', linetype = 2) + 
  stat_function(fun = dbeta, args = list(shape1 = 22, shape2 = 42), 
                color = 'blue') +
  stat_function(fun = dbeta, args = list(shape1 = 2, shape2 = 2), 
                color = 'blue', linetype = 2) + 
  stat_function(fun = dbeta, args = list(shape1 = 23, shape2 = 42), 
                color = 'green') +
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 2), 
                color = 'green', linetype = 2)

