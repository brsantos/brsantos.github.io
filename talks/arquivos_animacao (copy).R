## Fazendo animação com geom_tile
library(gganimate)
library(ggplot2)
library(dplyr)

## Exemplo site
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

## Exemplo geom_tile
n_transitions <- 5

all_squares <- lapply(1:n_transitions, function(a){
  n_points <- 20
  x <- 1:n_points
  y <- 1:n_points
  
  x_y <- expand.grid(x, y)
  
  x_y$unif <- runif(n_points^2)
  x_y$transition <- a
  x_y
})

all_squares <- do.call(rbind.data.frame, all_squares)

p <- ggplot(all_squares, aes(x = Var1, y = Var2, colour = unif, fill = unif)) +
  geom_tile() + theme_bw() + 
  scale_fill_gradientn(colors = rainbow(7)) + 
  scale_colour_gradientn(colors = rainbow(7)) + 
  ylab("") + xlab("") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(), 
    legend.position = 'none'
  )
p

anim <- p +
  transition_states(transition,
                    transition_length = 1,
                    state_length = 1) 

anim_save("figuras/anim.gif", anim)


### Figura correlacionada
gaussian_correlation <- function(h, phi = 1) exp(-(h/phi)^2)

all_squares_cor <- lapply(1:n_transitions, function(a){
  n_points <- 20
  x <- 1:n_points
  y <- 1:n_points
  
  x_y <- expand.grid(x, y)
  
  mat_distances <- as.matrix(dist(x_y))
  
  ## Parameter of correlation function
  phi <- 2.5
  
  mat_correlation <- apply(mat_distances, 1, gaussian_correlation, phi = phi)
  
  y_draw <- MASS::mvrnorm(1, rep(0, n_points^2), Sigma = mat_correlation)
  
  x_y$values <- as.numeric(y_draw)
  x_y$transition <- a
  x_y
})

all_squares_cor <- do.call(rbind.data.frame, all_squares_cor)

p <- ggplot(all_squares_cor, 
            aes(x = Var1, y = Var2, colour = values, fill = values)) +
  geom_tile() + theme_bw() + 
  scale_fill_gradientn(colors = rainbow(7)) +
  scale_colour_gradientn(colors = rainbow(7)) +
  ylab("") + xlab("") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(), 
    legend.position = 'none'
  )


anim2 <- p +
  transition_states(transition,
                    transition_length = 1,
                    state_length = 1) 
anim2

anim_save("figuras/anim2.gif", anim2)


##  Fazendo gráfico para distribuições de probabilidades discretas e contínuas. 

x1 <- 0:20
y1 <- dbinom(x1, size = 20, prob = 0.3)
group1 <- rep(1, length(x1))

x2 <- 75:125
y2 <- dbinom(x2, size = 200, prob = 0.5)
group2 <- rep(2, length(x2))

dados_plot <- data.frame(x = c(x1, x2), 
                         y = c(y1, y2), 
                         group = c(group1, group2))

g <- ggplot(dados_plot, aes(x = x, y = y, group = group)) + 
  geom_bar(stat = 'identity', fill = 'royalblue', width = 0.30) + 
  theme_minimal() + ylab("") + xlab("") + 
  theme(axis.text.y = element_blank())
anim3 <- g + 
  transition_states(group, 
                    transition_length = 1,
                    state_length = 1) + 
  view_follow()

anim_save("figuras/anim3.gif", anim3, width = 200, height = 200)


## Caso contínuo
x <- seq(0.1, 8, by = 0.05)
y1 <- dnorm(x, mean = 3)
y2 <- dgamma(x, 2, 1)
y3 <- dgamma(x, 5, 1)
plot(x, y1, type = 'l')
plot(x, y2, type = 'l')
plot(x, y3, type = 'l')
groups <- rep(c(1, 2, 3), each = length(x))

dados_plot <- data.frame(x = c(x, x, x), 
                         y = c(y1, y2, y3), 
                         group = groups)

g <- ggplot(dados_plot, aes(x = x, y = y)) + 
  geom_line(colour = 'royalblue') + 
  theme_minimal() + ylab("") + xlab("") + 
  theme(axis.text.y = element_blank()) 
anim4 <- g + 
  transition_states(group, 
                    transition_length = 1,
                    state_length = 1) + 
  ease_aes("cubic-in-out")

anim_save("figuras/anim4.gif", anim4, width = 200, height = 200)


## Distribuição normal - exemplo

quantis <- 1:99/100
qnorm(quantis)

means <- c(20, 30, 40, 50, 60)

sds <- c(1, 2, 3, 4, 5)

normais_media <- lapply(means, function(a){
  x <- qnorm(quantis, mean = a)
  y <- dnorm(x, mean = a)
  data.frame(x = x, y = y, group = a)
})

normais_desv <- lapply(sds, function(a){
  x <- qnorm(quantis, sd = a)
  y <- dnorm(x, sd = a)
  data.frame(x = x, y = y, group = a)
})

values_norm <- do.call(rbind.data.frame, normais_media)
values_desv <- do.call(rbind.data.frame, normais_desv)


g <- ggplot(values_norm, aes(x = x, y = y)) + 
  geom_line(colour = 'royalblue') + 
  theme_classic() + ylab("") + xlab("") 
anim5 <- g + 
  transition_states(group, 
                    transition_length = 1,
                    state_length = 1) + 
  view_follow() + 
  ease_aes("cubic-in-out") + 
  shadow_mark(colour = 'grey75', size = 0.75) + 
  ggtitle('Média = {closest_state}, Variância = 1')

anim_save("figuras/anim5.gif", anim5, width = 350, height = 350)

g <- ggplot(values_desv, aes(x = x, y = y)) + 
  geom_line(colour = 'royalblue') + 
  theme_classic() + ylab("") + xlab("") 
anim6 <- g + 
  transition_states(group, 
                    transition_length = 1,
                    state_length = 1) + 
  view_follow() + 
  ease_aes("cubic-in-out") +
  shadow_mark(colour = 'grey75', size = 0.75) + 
  ggtitle('Média = 0, Variância = {closest_state}')

anim_save("figuras/anim6.gif", anim6, width = 350, height = 350)


#### Distribuição normal, tempo de incubação
n_points <- 250
x <- seq(0, 15, length = n_points)
y1 <- dnorm(x, mean = 7, sd = 2)
y2 <- dnorm(x, mean = 9, sd = 2)
type <- rep(c("Mulheres", "Homens"), each = n_points)

data_plot <- data.frame(x = rep(x, times = 2),
                        y = c(y2, y1), 
                        type = type)
g <- ggplot(data_plot)
g <- g + geom_line(aes(x, y, colour = type)) + theme_classic() + 
  scale_colour_manual(name = 'Sexo',
                      values = c('lightblue', 'darkviolet')) + 
  xlab("Tempo de incubação") + 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11, 13, 15),
                     labels = c("1", "3", "5", "mu_h", "mu_m", "11", "13", "15")) + 
  geom_vline(aes(xintercept = c(7)), linetype = 2, colour = 'grey75') +
  geom_vline(aes(xintercept = c(9)), linetype = 2, colour = 'grey75') +
  ylab("") + theme(axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank())
  
png('figuras/dist_tempo_sexo.png', width = 375, height = 375)
g
dev.off()

## Com variáveis preditoras 
n_points <- 250
x <- seq(0, 15, length = n_points)

values_age <- seq(20, 60, by = 5)

values_norm <- lapply(1:length(values_age), function(a){
  dnorm(x, mean = 5 + a*0.5, sd = 2)
}) %>% unlist()

data_plot <- data.frame(x = rep(x, times = length(values_age)), 
                        y = values_norm, 
                        age = rep(values_age, each = length(x)))

g <- ggplot(data_plot)
g <- g + geom_line(aes(x, y), colour = 'darkviolet') + theme_classic() + 
  xlab("Tempo de incubação") + 
  ylab("") + theme(axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank())
anim7 <- g + 
  transition_states(age, 
                    transition_length = 1,
                    state_length = 1) + 
  ease_aes("cubic-in-out") + 
  shadow_mark(colour = 'grey75', size = 0.75) + 
  ggtitle('Idade = {closest_state}')

anim_save("figuras/anim7.gif", anim7, width = 350, height = 350)


## Com efeito negativo
values_norm <- lapply(1:length(values_age), function(a){
  dnorm(x, mean = 10 - a*0.5, sd = 2)
}) %>% unlist()

data_plot <- data.frame(x = rep(x, times = length(values_age)), 
                        y = values_norm, 
                        age = rep(values_age, each = length(x)))

g <- ggplot(data_plot)
g <- g + geom_line(aes(x, y), colour = 'darkviolet') + theme_classic() + 
  xlab("Tempo de incubação") + 
  ylab("") + theme(axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank())
anim8 <- g + 
  transition_states(age, 
                    transition_length = 1,
                    state_length = 1) + 
  ease_aes("cubic-in-out") + 
  shadow_mark(colour = 'grey75', size = 0.75) + 
  ggtitle('Idade = {closest_state}')

anim_save("figuras/anim8.gif", anim8, width = 350, height = 350)


## Fazendo gráfico ao contrário
values_norm <- lapply(1:length(values_age), function(a){
  dnorm(x, mean = 10 - a*0.5, sd = 2)
}) %>% unlist()

x_first <- rep(x, times = length(values_age))
age_vector <- rep(values_age, each = length(x))
y2 <- values_norm + max(values_norm)*age_vector/5

data_plot <- data.frame(x = x_first, 
                        y = values_norm,
                        y2 = y2,
                        age = age_vector)

min_values <- group_by(data_plot, age) %>%
  summarise(minimos = min(y2)) %>%
  select(minimos) %>%
  unlist() %>%
  as.numeric()

max_values <- group_by(data_plot, age) %>%
  summarise(maximos = max(y2)) %>%
  select(maximos) %>%
  unlist() %>%
  as.numeric()

lower_values <- sapply(1:length(values_age), function(a){
  qnorm(0.01, mean = 10 - a*0.5, sd = 2)
})

upper_values <- sapply(1:length(values_age), function(a){
  qnorm(0.99, mean = 10 - a*0.5, sd = 2)
})

lower_bound <- rep(lower_values, each = length(x))
upper_bound <- rep(upper_values, each = length(x))

conditional_means <- 10 - 1:length(values_age)*0.5

data_plot2 <- data_plot %>%
  mutate(lower_bound = lower_bound, 
         upper_bound = upper_bound) %>%
  filter(x > lower_bound & x < upper_bound)

data_to_add <- data.frame(y = conditional_means, 
                          x = min_values,
                          yint = max_values,
                          age = values_age)


g <- ggplot()
g <- g + geom_path(data = data_plot2, 
                   aes(y2, x, group = age), 
                   colour = 'royalblue') + theme_classic() + 
  ylab("Tempo de incubação") + 
  xlab("Idade") +
  geom_line(data = data_to_add, aes(x, y)) + 
  geom_vline(data = data_to_add, aes(xintercept = x), linetype = 2) + 
  geom_hline(data = data_to_add, aes(yintercept = y),  linetype = 2) +
  scale_x_continuous(breaks = min_values,
                     labels = as.character(values_age))

anim9 <- g + 
  transition_reveal(age)  

anim_save("figuras/anim9.gif", anim9, width = 350, height = 350)


## Fazendo gráfico ao contrário - com efeito positivo
values_norm <- lapply(1:length(values_age), function(a){
  dnorm(x, mean = 5 + a*0.5, sd = 2)
}) %>% unlist()

x_first <- rep(x, times = length(values_age))
age_vector <- rep(values_age, each = length(x))
y2 <- values_norm + max(values_norm)*age_vector/5

data_plot <- data.frame(x = x_first, 
                        y = values_norm,
                        y2 = y2,
                        age = age_vector)

min_values <- group_by(data_plot, age) %>%
  summarise(minimos = min(y2)) %>%
  select(minimos) %>%
  unlist() %>%
  as.numeric()

max_values <- group_by(data_plot, age) %>%
  summarise(maximos = max(y2)) %>%
  select(maximos) %>%
  unlist() %>%
  as.numeric()

lower_values <- sapply(1:length(values_age), function(a){
  qnorm(0.01, mean = 5 + a*0.5, sd = 2)
})

upper_values <- sapply(1:length(values_age), function(a){
  qnorm(0.99, mean = 5 + a*0.5, sd = 2)
})

lower_bound <- rep(lower_values, each = length(x))
upper_bound <- rep(upper_values, each = length(x))

conditional_means <- 5 + 1:length(values_age)*0.5

data_plot2 <- data_plot %>%
  mutate(lower_bound = lower_bound, 
         upper_bound = upper_bound) %>%
  filter(x > lower_bound & x < upper_bound)

data_to_add <- data.frame(y = conditional_means, 
                          x = min_values,
                          yint = max_values,
                          age = values_age)


g <- ggplot()
g <- g + geom_path(data = data_plot2, 
                   aes(y2, x, group = age), 
                   colour = 'royalblue') + theme_classic() + 
  ylab("Tempo de incubação") + 
  xlab("Idade") +
  geom_line(data = data_to_add, aes(x, y)) + 
  geom_vline(data = data_to_add, aes(xintercept = x), linetype = 2) + 
  geom_hline(data = data_to_add, aes(yintercept = y),  linetype = 2) +
  scale_x_continuous(breaks = min_values,
                     labels = as.character(values_age))

anim10 <- g + 
  transition_reveal(age) + 
  ease_aes("cubic-in-out") 

anim_save("figuras/anim10.gif", anim10, width = 350, height = 350)


## Gráfico histograma Normal
set.seed(42)
n_cases <- 300
y_sample <- rnorm(n_cases, mean = 8.5, sd = 2)


g <- ggplot(data.frame(y = y_sample)) + theme_classic() +
  geom_histogram(aes(x = y, y = ..density..), 
                 fill = 'darkviolet', colour = 'grey75') + 
  ylab("densidade") + xlab("tempo de incubação")
png('figuras/hist_tempo.png', width = 350, height = 350)
g
dev.off()


sample_sizes <- seq(50, 350, by = 25)
mu_grid_values <- seq(7, 10, length = 200)

values_list_likelihood <- lapply(sample_sizes, function(a){
  y_sample_dif_sizes <- rnorm(a, mean = 8.5, sd = 2)
  likelihood_values <- sapply(mu_grid_values, function(a){
    exp(sum(dnorm(y_sample_dif_sizes, mean = a, sd = 2, log = TRUE)))
  })
  data.frame(x = mu_grid_values, 
             y = likelihood_values, 
             sample_size = a)
})

values_likelihood <- do.call(rbind.data.frame, values_list_likelihood)

g <- ggplot(values_likelihood) + theme_classic() + 
  geom_line(aes(x, y)) + 
  ylab("") + theme(axis.text.y = element_blank()) + 
  xlab(expression(mu)) + 
  geom_vline(aes(xintercept = 8.5), linetype = 2)

anim11 <- g + 
  transition_states(sample_size, 
                    transition_length = 1,
                    state_length = 1) + 
  ease_aes("linear") + 
  view_follow() + 
  ggtitle('Tamanho da amostra = {closest_state}')

anim_save("figuras/anim11.gif", anim11, width = 350, height = 350)


## Comparação priori e posteriori 
verossimilhanca_bin <- function(p, n, y){
  dbinom(y, size = n, prob = p) 
}

png('figuras/posteriori1.png', width = 350, height = 350)
ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  theme_minimal() +
  stat_function(fun = verossimilhanca_bin, args = list(n = 6, y = 2)) + 
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 5), 
                color = 'red') +
  stat_function(fun = dbeta, args = list(shape1 = 4, shape2 = 6), 
                color = 'blue') +
  stat_function(fun = dbeta, args = list(shape1 = 5, shape2 = 6), 
                color = 'green') +
  xlab(expression(theta)) + 
  labs(title = expression(paste("Posteriori de ", theta), "para diferentes prioris"), 
       subtitle = "Verossimilhança na linha preta")
dev.off()


png('figuras/priori1.png', width = 350, height = 350)
ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  theme_minimal() +
  stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 1), 
                color = 'red', linetype = 2) + 
  stat_function(fun = dbeta, args = list(shape1 = 2, shape2 = 2), 
                color = 'blue', linetype = 2) + 
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 2), 
                color = 'green', linetype = 2) +
  xlab(expression(theta)) + 
  labs(title = expression(paste("Diferentes prioris para  ", theta)))
dev.off()


png('figuras/posteriori2.png', width = 350, height = 350)
ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  theme_minimal() +
  stat_function(fun = verossimilhanca_bin, args = list(n = 60, y = 20)) + 
  stat_function(fun = dbeta, args = list(shape1 = 21, shape2 = 41), 
                color = 'red') +
  stat_function(fun = dbeta, args = list(shape1 = 22, shape2 = 42), 
                color = 'blue') +
  stat_function(fun = dbeta, args = list(shape1 = 23, shape2 = 42), 
                color = 'green') +
  xlab(expression(theta)) + 
  labs(title = expression(paste("Posteriori de ", theta), "para diferentes prioris"), 
       subtitle = "Verossimilhança na linha preta")
dev.off()


## Quantil 

df1 <- 5
df2 <- 10

x <- seq(0, 3.5, length = 1000)
y <- df(x, df1, df2)
dados <- data.frame(x = x, y = y)

break_5 <- 2.3


g <- ggplot(dados, aes(x = x, y = y)) + theme_classic()
g <- g + geom_line() + 
  ylab("") + xlab("") + 
  geom_ribbon(data = subset(dados, x < break_5),
              aes(ymax = y), ymin = 0, fill = "red", color = 'black', 
              alpha = 0.5) + 
  scale_x_continuous(breaks = break_5,
                     labels = "Q_y") 

png('figuras/quantis.png', width = 350, height = 350)
g
dev.off()




