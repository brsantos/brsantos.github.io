## Definindo níveis de fatores das variáveis

dados$UF <- factor(dados$UF)
levels(dados$UF) <- c("Rondônia", "Acre", "Amazonas",
                                         "Roraima", "Pará", "Amapá", 
                                         "Tocantins", "Maranhão", "Piauí", 
                                         "Ceará", "Rio Grande do Norte", "Paraíba",
                                         "Pernambuco", "Alagoas", "Sergipe", 
                                         "Bahia", "Minas Gerais", "Espírito Santo",
                                         "Rio de Janeiro", "São Paulo", "Paraná", 
                                         "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul",
                                         "Mato Grosso", "Goiás", "Distrito Federal")

dados$Sexo <- factor(dados$Sexo)
levels(dados$Sexo) <- c("Masculino", "Feminino")

dados$Etnia <- factor(dados$Etnia)
levels(dados$Etnia) <- c("Indígena", "Branca", "Preta", "Amarela", "Parda", 
                         "Sem declaração")

dados$EstadoCivil <- factor(dados$EstadoCivil)
levels(dados$EstadoCivil) <- c("Solteiro(a)", 
                               "Casado(a)", 
                               "Desquitado(a) ou separado(a) judicialmente",
                               "Divorciado(a)",
                               "Viúvo(a)")

dados$Nasceu_UF <- factor(dados$Nasceu_UF)
levels(dados$Nasceu_UF) <- c("Sim", "Não")

dados$LugarNascimento <- factor(dados$LugarNascimento)
levels(dados$LugarNascimento) <- c(levels(dados$UF), "País estrangeiro")

dados$SabeLerEscrever <- factor(dados$SabeLerEscrever)
levels(dados$SabeLerEscrever) <- c("Sim", "Não")

dados$UtilizaInternet <- factor(dados$UtilizaInternet)
levels(dados$UtilizaInternet) <- c("Sim", "Não")

dados$ContPrevidencia <- factor(dados$ContPrevidencia)
levels(dados$ContPrevidencia) <- c("Sim", "Não")

dados$Sindicato <- factor(dados$Sindicato)
levels(dados$Sindicato) <- c("Sim", "Não")

dados$AposentadoINSS <- factor(dados$AposentadoINSS)
levels(dados$AposentadoINSS) <- c("Sim", "Não")

dados$PensionistaINSS <- factor(dados$PensionistaINSS)
levels(dados$PensionistaINSS) <- c("Sim", "Não")

dados$TeveFilho <- factor(dados$TeveFilho)
levels(dados$TeveFilho) <- c("Sim", "Não")

dados$CondicaoAtividade <- factor(dados$CondicaoAtividade)
levels(dados$CondicaoAtividade) <- c("Economicamente ativas", 
                                     "Não economicamente ativas")

dados$GruposAnosEstudo <- factor(dados$GruposAnosEstudo)
levels(dados$GruposAnosEstudo) <- c("Sem instrução e menos de 1 ano",
                                    "1 a 3 anos",
                                    "4 a 7 anos",
                                    "8 a 10 anos",
                                    "11 a 14 anos",
                                    "15 anos ou mais",
                                    "Não determinados")

dados$SelecaoPerguntasTrabalho <- factor(dados$SelecaoPerguntasTrabalho)
levels(dados$SelecaoPerguntasTrabalho) <- "Selecionada"

dados$SatisfacaoSalario <- factor(dados$SatisfacaoSalario)
levels(dados$SatisfacaoSalario) <- c("Insatisfeito", 
                                     "Pouco satisfeito",
                                     "Indiferente",
                                     "Satisfeito",
                                     "Muito satisfeito")

dados$SatisfacaoOportunidades <- factor(dados$SatisfacaoOportunidades)
levels(dados$SatisfacaoOportunidades) <- c("Não cabe avaliar",
                                           "Insatisfeito",
                                           "Pouco satisfeito",
                                           "Indiferente",
                                           "Satisfeito",
                                           "Muito satisfeito")

dados$SelecaoPerguntasEsporte <- factor(dados$SelecaoPerguntasEsporte)
levels(dados$SelecaoPerguntasEsporte) <- c("Não selecionada", "Selecionada")

dados$PraticouEsporte <- factor(dados$PraticouEsporte)
levels(dados$PraticouEsporte) <- c("Sim", "Não")