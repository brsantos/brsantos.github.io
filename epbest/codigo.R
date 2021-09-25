library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)
library(quantreg)


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
  scale_colour_viridis_d(name = 'Genero') + 
  xlab("Milhares de reais") + 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11, 13, 15),
                     labels = c("1", "3", "5", "mu_h", "mu_m", "11", "13", "15")) + 
  geom_vline(aes(xintercept = c(7)), linetype = 2, colour = 'grey75') +
  geom_vline(aes(xintercept = c(9)), linetype = 2, colour = 'grey75') +
  ylab("") + theme(axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank())

png('img/dist_tempo_genero.png', width = 450, height = 450)
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

anim_save("img/anim_hor_pos.gif",anim7, duration = 10, fps = 15, 
          width = 450, height = 450, renderer = gifski_renderer())


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

anim_save("img/anim_hor_neg.gif",anim8, duration = 10, fps = 15, 
          width = 450, height = 450, renderer = gifski_renderer())


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

anim_save("img/anim_vert_neg.gif", anim9, duration = 10, fps = 15, 
          width = 450, height = 450, renderer = gifski_renderer())


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

anim_save("img/anim_vert_pos.gif", anim10, duration = 10, fps = 15, 
          width = 450, height = 450, renderer = gifski_renderer())


### Fazendo grafico

set.seed(1)
x_rand <- runif(100)*10
y_rand <- 1 + 2 * x_rand + rnorm(100, sd = 4)

g <- ggplot(data.frame(x = x_rand, y = y_rand)) +
  aes(x, y) + 
  geom_point() +
  theme_minimal() 

png("img/graf_dispersao.png")
g
dev.off()

png("img/graf_regressao.png")
g + geom_smooth(method = "lm", se = FALSE)
dev.off()

g + geom_abline(intercept = qnorm(1:9/10, sd = 2) + 2, slope = rep(2, 9), 
                color = "blue", linetype = 2)

quantis <- lapply(1:9/10, function(a) 4 * qnorm(a) + 2 * x_rand)

g_base <- ggplot() +
  geom_point(data = data.frame(x = x_rand, y = y_rand), aes(x, y)) +
  theme_minimal() 

for(i in 1:9) g_base <- 
  g_base + geom_line(data = data.frame(x = x_rand, yline = quantis[[i]]), 
            aes(x = x, y = yline), linetype = 2, color = "blue")


png("img/normal_regressao.png")
g_base
dev.off()



  
### Grafico - Normal quantis

g_norm <- 
  ggplot() + 
  theme_minimal() + 
  stat_function(fun = dnorm) + 
  xlim(c(-3, 3)) +
  labs(y = "densidade", 
       x = "y")

png("img/normal.png")
g_norm
dev.off()


png("img/normal_quantis.png")
g_norm + geom_segment(aes(y = 0, yend = dnorm(qnorm(1:9/10)), 
                          x = qnorm(1:9/10), xend = qnorm(1:9/10)), 
                      linetype = 2)
dev.off()


### Graficos da distribuição laplace assimétrica

x <- seq(-8, 8, len = 201)
y_25 <- ald::dALD(x, p = 0.25)
y_50 <- ald::dALD(x, p = 0.5)
y_75 <- ald::dALD(x, p = 0.75)

plot(x, y_50, type = 'l')
lines(x, y_25, type = 'l')

g_lap1 <- 
  ggplot(data.frame(x = c(x, x), 
                  y = c(y_25, y_50), 
                  tau = rep(c("tau = 0.25", "tau = 0.50"), 
                            each = length(x)))) +
  geom_line(aes(x, y, color = tau)) + 
  scale_color_viridis_d() + 
  theme_minimal() +
  labs(title = "Mu = 0, Sigma = 1")

g_lap2 <- 
  ggplot(data.frame(x = c(x, x), 
                    y = c(y_75, y_50), 
                    tau = rep(c("tau = 0.75", "tau = 0.50"), 
                              each = length(x)))) +
  geom_line(aes(x, y, color = tau)) + 
  scale_color_viridis_d(direction = -1) + 
  theme_minimal() +
  labs(title = "Mu = 0, Sigma = 1")


png("img/laplace_density1.png")
g_lap1
dev.off()

png("img/laplace_density2.png")
g_lap2
dev.off()

#### 

dados_pnad <- read.csv('dadosResumo_PNAD2015.csv', 
                       head = TRUE)

dados_paraiba <- dados_pnad %>% 
  filter(UF == 25) %>% 
  filter(CondicaoAtividade == 1) %>% 
  filter(Idade > 18) %>% 
  mutate(rendimento = ifelse(RendimentoTotal >= 9e11, NA, RendimentoTotal)) %>% 
  filter(!is.na(rendimento)) %>% 
  filter(rendimento > 0) %>% 
  mutate(etnia = ifelse(Etnia == 2, "Branca", "Não Branca")) %>% 
  mutate(genero = ifelse(Sexo == 2, "Masculino", "Feminino"))
    

png("img/disp_rendimento.png")
ggplot(dados_paraiba) +
  aes(y = rendimento, x = Idade) + 
  geom_point() +
  ylim(0, 7500) +
  theme_bw() +
  facet_grid(etnia ~ genero) +
  labs(title = "Dispersão dos valores de rendimento com idade") 
dev.off()

png("img/disp_anosEstudo.png")
ggplot(dados_paraiba) +
  aes(y = rendimento, x = AnosEstudo) + 
  geom_point() +
  ylim(0, 7500) +
  theme_bw() +
  facet_grid(etnia ~ genero) +
  labs(title = "Dispersão dos valores de rendimento com idade") 
dev.off()



png("img/hist_genero.png")
ggplot(dados_paraiba) +
  aes(x = rendimento, y = ..density..) + 
  geom_histogram(fill = "darkviolet", colour = "grey75") +
  xlim(0, 7500) +
  theme_minimal() +
  facet_wrap(~ genero) +
  labs(title = "Distribuição de rendimento", 
       y = "densidade") 
dev.off()

png("img/hist_etnia.png")
ggplot(dados_paraiba) +
  aes(x = rendimento, y = ..density..) + 
  geom_histogram(fill = "darkviolet", colour = "grey75") +
  xlim(0, 7500) +
  theme_minimal() +
  facet_wrap(~ etnia) +
  labs(title = "Distribuição de rendimento", 
       y = "densidade") 
dev.off()



model <- rq(rendimento ~ genero + etnia + Idade + AnosEstudo, 
   tau = 2:18/20, data = dados_paraiba)
resumo_freq <- summary(model, se = 'rank')
plot(resumo_freq)

coef_lm <- 
  coef(summary(lm(rendimento ~ genero + etnia + Idade + AnosEstudo, 
                  data = dados_paraiba))) %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>% 
  mutate(variable = rownames(coef_lm))

rownames(coef_lm) <- NULL

coef_lm <- coef_lm %>% 
  mutate(lower_bd = estimate + qnorm(0.025) * std_error, 
         upper_bd = estimate + qnorm(0.975) * std_error)


coef_models <- 
  lapply(resumo_freq, function(a) {
    data_info <- data.frame(a$coefficients)
    data_info$variable <- rownames(a$coefficients)
    rownames(data_info) <- NULL
    data_info$est_lm <- coef_lm$estimate
    data_info$lower_bd_lm <- coef_lm$lower_bd
    data_info$upper_bd_lm <- coef_lm$upper_bd
    data_info
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  mutate(tau = rep(2:18/20, each = 5)) %>% 
  janitor::clean_names() %>% 
  mutate(variable = factor(variable), 
         variable = forcats::fct_recode(variable, 
                                        "anos estudo" = "AnosEstudo", 
                                        "etnia = Nao Branca" = "etniaNão Branca", 
                                        "genero = Masculino" = "generoMasculino"))

g_rq <- ggplot(filter(coef_models, variable != "(Intercept)")) +
  theme_minimal() +
  aes(x = tau) +
  geom_line(aes(y = coefficients)) + 
  geom_line(aes(y = lower_bd), linetype = 2) + 
  geom_line(aes(y = upper_bd), linetype = 2) + 
  geom_line(aes(y = est_lm), color = "red") + 
  geom_line(aes(y = lower_bd_lm), linetype = 2, color = "red") + 
  geom_line(aes(y = upper_bd_lm), linetype = 2, color = "red") + 
  facet_wrap(~ variable, scales = "free")

png("img/coef_reg_quantilica.png", width = 900)
g_rq
dev.off()

faz_grafico_coeficiente <- function(nome_var){
  ggplot(filter(coef_models, variable == nome_var)) +
    theme_minimal() +
    aes(x = tau) +
    geom_line(aes(y = coefficients)) + 
    geom_line(aes(y = lower_bd), linetype = 2) + 
    geom_line(aes(y = upper_bd), linetype = 2) + 
    geom_line(aes(y = est_lm), color = "red") + 
    geom_line(aes(y = lower_bd_lm), linetype = 2, color = "red") + 
    geom_line(aes(y = upper_bd_lm), linetype = 2, color = "red") + 
    facet_wrap(~ variable, scales = "free")
  
}

g_rq2 <- faz_grafico_coeficiente(levels(coef_models$variable)[2]) 
g_rq3 <- faz_grafico_coeficiente(levels(coef_models$variable)[3]) 
g_rq4 <- faz_grafico_coeficiente(levels(coef_models$variable)[4]) 
g_rq5 <- faz_grafico_coeficiente(levels(coef_models$variable)[5]) 

png("img/coef_reg_quantilica2.png", width = 900)
g_rq2
dev.off()

png("img/coef_reg_quantilica3.png", width = 900)
g_rq3
dev.off()

png("img/coef_reg_quantilica4.png", width = 900)
g_rq4
dev.off()

png("img/coef_reg_quantilica5.png", width = 900)
g_rq5
dev.off()



### Estimacao bayesiana
model_bayes <- 
  bayesQR::bayesQR(rendimento ~ genero + etnia + Idade + AnosEstudo, 
            quantile = 2:18/20, data = dados_paraiba, ndraw = 110000, 
            keep = 100)

model_bayes_2 <- lapply(2:18/20, function(a){
  R2BayesX::bayesx(rendimento ~ genero + etnia + Idade + AnosEstudo, 
                   data = dados_paraiba,
                   iter = 110000, burnin = 10000,
                   step = 100, method = "MCMC",
                   family = "quantreg", quantile = a, 
                   control =
                     R2BayesX::bayesx.control(outfile = "/Users/macadmin/work_related/short_course_EPBEST/Slides/resultsBX/"))
})

resumo <- summary(model_bayes, burnin = 100, credint=c(.05,.95))
coef_models <- 
  lapply(resumo, function(a) {
    data_info <- data.frame(a$betadraw[, 1:3])
    data_info$variable <- rownames(a$betadraw)
    rownames(data_info) <- NULL
    data_info
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  mutate(tau = rep(2:18/20, each = 5)) %>% 
  janitor::clean_names() 

ggplot(filter(coef_models, variable != "(Intercept)")) +
  theme_minimal() +
  geom_line(aes(x = tau, y = bayes_estimate)) + 
  geom_line(aes(x = tau, y = lower), linetype = 2) + 
  geom_line(aes(x = tau, y = upper), linetype = 2) + 
  facet_wrap(~ variable, scales = "free")






##### Modelo bamlss

library(bamlss)

f <- rendimento ~ genero + etnia + Idade + AnosEstudo

b1 <- bamlss(f, family = "gamma", data = dados_paraiba)

resumo_bamlss <- summary(b1)

res <- lapply(resumo_bamlss$model.matrix, function(a){
  dim_x <- dim(a)[1]
  a[-dim_x, c(1,2,4)]
}) 

res[[1]] %>% knitr::kable('html', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("scale_down")) %>% 
  kableExtra::save_kable("img/tabela_coeficientes.png")


png("plot_bamlss.png")
plot(b1, which = "samples")
dev.off()

DIC(b1)


## Efeito polinomial
f2 <- rendimento ~ genero + etnia + poly(Idade, 3) + poly(AnosEstudo, 3)

b2 <- bamlss(f2, family = "gamma", data = dados_paraiba)

resumo_bamlss <- summary(b2)

##

nd <- data.frame(AnosEstudo = seq(1, 16, len = 100), 
                 Idade = seq(18, 80, len = 100))

nd$pIdade <- predict(b2, newdata = nd, 
                     model = "mu", term = "Idade",
        FUN = c95, intercept = FALSE)

nd$pAnosEstudo <- predict(b2, newdata = nd, 
                     model = "mu", term = "AnosEstudo",
                     FUN = c95, intercept = FALSE)

png("img/efeito_nao_linear.png", width = 960)
par(mfrow = c(1, 2))
ylim <- range(c(nd$pIdade, nd$pAnosEstudo))
plot2d(pIdade ~ Idade, data = nd, ylim = ylim)
plot2d(pAnosEstudo ~ AnosEstudo, data = nd, ylim = ylim)
dev.off()

## Ajustando vários modelos
b2_1 <- bamlss(f2, family = "gamma", data = dados_paraiba)

b2_2 <- bamlss(f2, family = "weibull", data = dados_paraiba)

b2_3 <- bamlss(f2, family = "lognormal", data = dados_paraiba)

b2_4 <- bamlss(f2, family = "gpareto", data = dados_paraiba)

DIC(b2_1, b2_2, b2_3, b2_4)

par(mfrow = c(2,2))
png("img/diag_gamma.png", width = 960)
plot(b2_1, which = c("hist-resid", "qq-resid"))
dev.off()
png("img/diag_weibull.png", width = 960)
plot(b2_2, which = c("hist-resid", "qq-resid"))
dev.off()
png("img/diag_lognormal.png", width = 960)
plot(b2_3, which = c("hist-resid", "qq-resid"))
dev.off()
png("img/diag_gpareto.png", width = 960)
plot(b2_4, which = c("hist-resid", "qq-resid"))
dev.off()

f3 <- list(rendimento ~ genero + etnia + poly(Idade, 3) + poly(AnosEstudo, 3), 
           sigma ~ genero + etnia + poly(Idade, 3) + poly(AnosEstudo, 3))

b3 <- bamlss(f3, family = "gamma", data = dados_paraiba)


DIC(b2_1, b3)

nd$pIdade <- predict(b3, newdata = nd, 
                     model = "mu", term = "Idade",
                     FUN = c95, intercept = FALSE)

nd$pAnosEstudo <- predict(b3, newdata = nd, 
                          model = "mu", term = "AnosEstudo",
                          FUN = c95, intercept = FALSE)


nd$pIdade_sigma <- predict(b3, newdata = nd, 
                     model = "sigma", term = "Idade",
                     FUN = c95, intercept = FALSE)

nd$pAnosEstudo_sigma <- predict(b3, newdata = nd, 
                          model = "sigma", term = "AnosEstudo",
                          FUN = c95, intercept = FALSE)


ylim <- range(c(nd$pIdade, nd$pAnosEstudo))
plot2d(pIdade ~ Idade, data = nd, ylim = ylim)
plot2d(pAnosEstudo ~ AnosEstudo, data = nd, ylim = ylim)

png("img/efeito_nao_linear_sigma.png", width = 960)
par(mfrow = c(1, 2))
ylim2 <- range(c(nd$pIdade_sigma, nd$pAnosEstudo_sigma))
plot2d(pIdade_sigma ~ Idade, data = nd, ylim = ylim2)
plot2d(pAnosEstudo_sigma ~ AnosEstudo, data = nd, ylim = ylim2)
dev.off()

resumo_bamlss3 <- summary(b3)

res3 <- lapply(resumo_bamlss3$model.matrix, function(a){
  dim_x <- dim(a)[1]
  a[-dim_x, c(1,2,4)]
}) 

res3[[2]] %>% knitr::kable('html', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("scale_down")) %>% 
  kableExtra::save_kable("img/tabela_coeficientes_3_sigma.png")


### Modelo com efeito não linear

f4 <- list(rendimento ~ genero + etnia + Idade + s(AnosEstudo), 
           sigma ~ genero + etnia + s(Idade) + s(AnosEstudo))

b4 <- bamlss(f4, family = "gamma", data = dados_paraiba, 
            n.iter = 25000, burnin = 5000, thin = 20)

nd <- data.frame(AnosEstudo = seq(1, 16, len = 100), 
                 Idade = seq(18, 80, len = 100))

nd$pAnosEstudo <- predict(b4, newdata = nd, 
                          model = "mu", term = "AnosEstudo",
                          FUN = c95, intercept = FALSE)

nd$pIdade_sigma <- predict(b4, newdata = nd, 
                                model = "sigma", term = "Idade",
                                FUN = c95, intercept = FALSE)

nd$pAnosEstudo_sigma <- predict(b4, newdata = nd, 
                                model = "sigma", term = "AnosEstudo",
                                FUN = c95, intercept = FALSE)


png("img/efeito_spline_sigma.png", width = 960)
par(mfrow = c(1, 3))
ylim <- range(c(nd$pAnosEstudo, nd$pIdade_sigma, nd$pAnosEstudo_sigma))
plot2d(pAnosEstudo ~ AnosEstudo, data = nd, ylim = ylim)
plot2d(pIdade_sigma ~ Idade, data = nd, ylim = ylim)
plot2d(pAnosEstudo_sigma ~ AnosEstudo, data = nd, ylim = ylim)
dev.off()


plot2d(pAnosEstudo_sigma ~ AnosEstudo, data = nd)


### Creating gif with images

## list file names and read in
library(magick)
imgs <- list.files("img/figuras_3d/animacao_3D/", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 4)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "img/gif_resultado.gif")


## Gráfico com dados
imgs <- list.files("img/figuras_3d/dados_3D/", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 4)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "img/gif_dados.gif")


#### Exemplo gráfico de regressão quantílica

set.seed(42)
taus_e <- 1:39/40


coeficientes_1 <- log(taus_e) - 0.5
upp_b <- coeficientes_1 + sqrt(abs(coeficientes_1))
low_b <- coeficientes_1 - sqrt(abs(coeficientes_1))

plot(taus_e, upp_b, type = "l", lty = 2, ylim = c(-7, 1))
lines(taus_e, coeficientes_1)
lines(taus_e, low_b, lty = 2)
abline(h = 0)

data_coef <- data.frame(tau = rep(taus_e, 3), 
                        coef = coeficientes_1, 
                        upp = upp_b, 
                        low = low_b)

g <- ggplot(data_coef) + 
  theme_minimal() + 
  aes(x = tau) + 
  geom_line(aes(y = coef)) +
  geom_line(aes(y = upp), linetype = 2) + 
  geom_line(aes(y = low), linetype = 2) + 
  labs(x = "Quantis", 
       y = "coeficientes da regressão quantílica") + 
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15))


png("img/exemplo_coeficientes.png", width = 800)
g
dev.off()

png("img/exemplo_coeficientes_2.png", width = 800)
g + geom_hline(yintercept = 0, size = 0.5, color = "blue")
dev.off()
