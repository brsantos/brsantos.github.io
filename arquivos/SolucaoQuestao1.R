##############################################
##### Possível solução para a Pergunta 1 #####
##############################################

#### Utilizando pacote dplyr
mutate(dados, rendimento = ifelse(RendimentoTotal == max(RendimentoTotal, na.rm = TRUE), 
                                  NA, RendimentoTotal)) %>%
  group_by(UF) %>%
  filter(Sexo == "Feminino", 
         Etnia == "Parda", 
         EstadoCivil == "Solteiro(a)", 
         Nasceu_UF == "Sim", 
         SabeLerEscrever == "Sim", 
         ContPrevidencia == "Sim") %>% 
  summarise(qtde = n(), 
            mediaRenda = mean(rendimento, na.rm = TRUE)) %>%
  select(UF, mediaRenda) %>%
  filter(mediaRenda %in% c(min(mediaRenda), max(mediaRenda)))


# Questão extra
resumoEstados <- mutate(dados, 
                         rendimento = ifelse(RendimentoTotal == max(RendimentoTotal, na.rm = TRUE), 
                                             NA, RendimentoTotal), 
                         regiao = factor(floor(sapply(as.numeric(dados$UF), function(a) 
                                                                           c(11, 12, 13, 14, 15, 16,
                                                                             17, 21, 22, 23, 24, 25,
                                                                             26, 27, 28, 29, 31, 32, 
                                                                             33, 35, 41, 42, 43, 50,
                                                                             51, 52, 53)[a])/10))) %>%
  group_by(UF, regiao) %>%
  filter(Sexo == "Feminino", 
         Etnia == "Parda", 
         EstadoCivil == "Solteiro(a)", 
         Nasceu_UF == "Sim", 
         SabeLerEscrever == "Sim", 
         ContPrevidencia == "Sim") %>% 
  summarise(qtde = n(), 
            mediaRenda = mean(rendimento, na.rm = TRUE))

levels(resumoEstados$regiao) <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
g <- ggplot(resumoEstados, aes(x = qtde, y = mediaRenda)) + 
  theme_bw() + 
  theme(legend.position = "bottom")
g + geom_point(aes(colour = regiao), size = 2) 
