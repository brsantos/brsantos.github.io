library(tidyverse)

##------------------------------------------------------- ##
## ---------------------- Slide 14 ---------------------- ##
dados <- tibble(var_x = 1:4, 
                var_y = seq(2, 8, 2), 
                var_grupo = c(rep("a", 3), "b")) 
dados

##------------------------------------------------------- ##
## ---------------------- Slide 15 ---------------------- ##
ggplot(dados)
  
##------------------------------------------------------- ##
## ---------------------- Slide 16 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y)) 
  
##------------------------------------------------------- ##
## ---------------------- Slide 17 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y)) +
  geom_point()
  
##------------------------------------------------------- ##
## ---------------------- Slide 18 ---------------------- ##
ggplot(dados) +
  geom_point(aes(x = var_x, y = var_y))
  

##------------------------------------------------------- ##
## ---------------------- Slide 19 ---------------------- ##
ggplot(dados) +
  geom_point(aes(x = var_x, y = var_y, 
                 color = var_grupo))
  
##------------------------------------------------------- ##
## ---------------------- Slide 20 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y)) +
  geom_point(aes(color = var_grupo)) 
  

##------------------------------------------------------- ##
## ---------------------- Slide 21 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y, 
                   color = var_grupo)) +
  geom_point()
  

##------------------------------------------------------- ##
## ---------------------- Slide 22 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y)) +
  geom_point(aes(color = var_grupo)) + 
  geom_line()


##------------------------------------------------------- ##
## ---------------------- Slide 23 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y, color = var_grupo)) +
  geom_point() + 
  geom_line()


##------------------------------------------------------- ##
## ---------------------- Slide 24 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y, 
                   color = var_grupo)) +
  geom_point() +
  facet_grid(. ~ var_grupo)
 
 
##------------------------------------------------------- ##
## ---------------------- Slide 25 ---------------------- ##
 ggplot(dados, aes(x = var_x, y = var_y, 
                   color = var_grupo)) +
  geom_label(aes(label = var_x)) +
  facet_grid(. ~ var_grupo)
  
 
##------------------------------------------------------- ##
## ---------------------- Slide 26 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y, 
                   color = var_grupo,
                   label = var_x)) +
  geom_point() +
  geom_label() +
  facet_grid(. ~ var_grupo)
  

##------------------------------------------------------- ##
## ---------------------- Slide 27 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y, 
                   color = var_grupo,
                   label = var_x)) +
  geom_label() +
  geom_point() +
  facet_grid(. ~ var_grupo)
  

##------------------------------------------------------- ##
## ---------------------- Slide 28 ---------------------- ##
ggplot(dados, aes(x = var_x, y = var_y, 
                   color = var_grupo,
                   label = var_x)) +
  geom_label() +
  facet_grid(. ~ var_grupo) +
  theme_dark()
  

##------------------------------------------------------- ##
## ---------------------- Slide 29 ---------------------- ##
p <- ggplot(dados, aes(x = var_x, y = var_y, 
                   color = var_grupo,
                   label = var_x)) +
  geom_point() +
  geom_label() +
  facet_grid(. ~ var_grupo) +
  theme_dark()

p + facet_grid(var_grupo ~ .)
  

##------------------------------------------------------- ##
## ---------------------- Slide 31 ---------------------- ##
##   install.packages("tidyverse")
library(tidyverse)

##   install.packages("MASS")
library(MASS)


df <- Cars93 %>% 
  select(Horsepower, Type, AirBags)

glimpse(df)


##------------------------------------------------------- ##
## ---------------------- Slide 32 ---------------------- ##
ggplot(df, aes(x = Horsepower)) 


##------------------------------------------------------- ##
## ---------------------- Slide 33 ---------------------- ##
ggplot(df, aes(x = Horsepower)) +
  geom_histogram()


##------------------------------------------------------- ##
## ---------------------- Slide 34 ---------------------- ##
ggplot(df, aes(x = Horsepower)) +
  geom_histogram(binwidth = 25)


##------------------------------------------------------- ##
## ---------------------- Slide 35 ---------------------- ##
ggplot(df, aes(x = Horsepower)) +
  geom_histogram(binwidth = 10)


##------------------------------------------------------- ##
## ---------------------- Slide 36 ---------------------- ##
ggplot(df, aes(x = Horsepower)) +
  geom_histogram(binwidth = 10,
                 color = "black",
                 fill = "darkorchid4")


##------------------------------------------------------- ##
## ---------------------- Slide 37 ---------------------- ##
ggplot(df, aes(x = Horsepower)) +
  geom_histogram(binwidth = 10,
                 color = "black",
                 fill = "darkorchid4") +
  theme_bw() 


##------------------------------------------------------- ##
## ---------------------- Slide 38 ---------------------- ##
ggplot(df, aes(x = Horsepower)) +
  geom_histogram(binwidth = 10,
                 color = "black",
                 fill = "darkorchid4") +
  theme_bw() +
  labs(x = "Nome eixo x", 
       y = "Nome eixo y", 
       title = "Título",
       subtitle = "Subtítulo",
       caption = "Fonte de Dados")


##------------------------------------------------------- ##
## ---------------------- Slide 39 ---------------------- ##
ggplot(df, aes(x = Horsepower)) +
  geom_histogram(bins = 10, 
                 color = "black", 
                 fill = "darkorchid4") +
  theme_bw() +
  labs(x = "Frequencia", 
       y = "Horsepower", 
       title = "Potencia")


##------------------------------------------------------- ##
## ---------------------- Slide 41 ---------------------- ##
ggplot(df, aes(x = Type))



##------------------------------------------------------- ##
## ---------------------- Slide 42 ---------------------- ##
ggplot(df, aes(x = Type)) +
  geom_bar()


##------------------------------------------------------- ##
## ---------------------- Slide 43 ---------------------- ##
ggplot(df, aes(x = Type)) +
  geom_bar(fill = "darkorchid4")


##------------------------------------------------------- ##
## ---------------------- Slide 44 ---------------------- ##
ggplot(df, aes(x = Type)) +
  geom_bar(fill = "darkorchid4") +
  labs(x = "Tipo de carro", y = "Contagem",
       title = "Tipo de carro") 


##------------------------------------------------------- ##
## ---------------------- Slide 45 ---------------------- ##
ggplot(df, aes(x = Type)) +
  geom_bar(fill = "darkorchid4") +
  labs(x = "Tipo de carro", y = "Contagem",
       title = "Tipo de carro") +
  theme_minimal()


##------------------------------------------------------- ##
## ---------------------- Slide 46 ---------------------- ##
ggplot(df, aes(x = Type)) +
  geom_bar(fill = "darkorchid4") +
  labs(x = "Tipo de carro", y = "Contagem",
       title = "Tipo de carro") +
  theme_minimal() +
  facet_grid( ~ AirBags)

##------------------------------------------------------- ##
## ---------------------- Slide 47 ---------------------- ##
ggplot(df, aes(x = Type)) +
  geom_bar(fill = "darkorchid4") +
  labs(x = "Tipo de carro", y = "Contagem",
       title = "Tipo de carro") +
  theme_minimal() +
  facet_grid( ~ AirBags) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##------------------------------------------------------- ##
## ---------------------- Slide 48 ---------------------- ##
ggplot(df, aes(x = Type, fill = AirBags)) +
  geom_bar(position = "dodge", 
               color = "black") +
  theme_minimal()


##------------------------------------------------------- ##
## ---------------------- Slide 49 ---------------------- ##
ggplot(df, aes(x = Type, fill = AirBags)) +
  geom_bar(position = "dodge", 
               color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Purples", 
                    direction = 1)


##------------------------------------------------------- ##
## ---------------------- Slide 50 ---------------------- ##
ggplot(df, aes(x = Type, fill = AirBags)) +
  geom_bar(position = "dodge", 
               color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Greens", 
                    direction = 1)


##------------------------------------------------------- ##
## ---------------------- Slide 51 ---------------------- ##
ggplot(df, aes(x = Type, fill = AirBags)) +
  geom_bar(position = "dodge", 
               color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Reds", 
                    direction = 1)


##------------------------------------------------------- ##
## ---------------------- Slide 52 ---------------------- ##
ggplot(df, aes(x = Type, fill = AirBags)) +
  geom_bar(position = "fill", 
               color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Purples", 
                    direction = 1)


##------------------------------------------------------- ##
## ---------------------- Slide 53 ---------------------- ##
ggplot(df, aes(x = Type, fill = AirBags)) +
  geom_bar(position = "stack", 
               color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Purples", 
                    direction = 1)


##------------------------------------------------------- ##
## ---------------------- Slide 54 ---------------------- ##
ggplot(df, aes(x = Type, fill = AirBags)) +
  geom_bar(position = "stack", 
               color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Purples", 
                    direction = 1) +
  coord_flip()


##------------------------------------------------------- ##
## ---------------------- Slide 56 ---------------------- ##
##   install.packages("tidyverse")
##   library(tidyverse)

dfhair <- data.frame(HairEyeColor)
glimpse(dfhair)


##------------------------------------------------------- ##
## ---------------------- Slide 57 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq)) +
  theme_minimal() 


##------------------------------------------------------- ##
## ---------------------- Slide 58 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq)) +
  theme_minimal() +
  geom_boxplot()


##------------------------------------------------------- ##
## ---------------------- Slide 59 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq, 
                   fill = Freq)) +
  theme_minimal() +
  geom_boxplot()



##------------------------------------------------------- ##
## ---------------------- Slide 60 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq, 
                   fill = Sex)) +
  theme_minimal() +
  geom_boxplot() + 
  theme(legend.position = 'none')


##------------------------------------------------------- ##
## ---------------------- Slide 61 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq, 
                   color = Sex)) +
  theme_minimal() +
  geom_boxplot() +
  scale_x_discrete(labels = c("Homem", "Mulher")) +
  xlab("Sexo") +
  ylab("Frequencia") +
  scale_color_manual(values = 
                       c("darkorchid4", "brown2"))


##------------------------------------------------------- ##
## ---------------------- Slide 62 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq, 
                   color = Sex)) +
  theme_minimal() +
  geom_boxplot(
    outlier.colour = "black",
    outlier.shape = 8,
    outlier.size = 4
  ) +
  scale_x_discrete(labels = c("Homem", "Mulher")) +
  xlab("Sexo") +
  ylab("Frequencia") +
  scale_color_manual(values = 
                       c("darkorchid4", "brown2"))


##------------------------------------------------------- ##
## ---------------------- Slide 63 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq, 
                   color = Sex)) +
  theme_minimal() +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    dotsize = 1,
    binwidth = 2
  ) +
  scale_x_discrete(labels = c("Homem", "Mulher")) +
  xlab("Sexo") +
  ylab("Frequencia") +
  scale_color_manual(values = 
                       c("darkorchid4", "brown2"))


##------------------------------------------------------- ##
## ---------------------- Slide 64 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq, 
                   color = Sex)) +
  theme_minimal() +
  geom_boxplot(
    outlier.colour = "black",
    outlier.shape = 8,
    outlier.size = 4
  ) +
  scale_x_discrete(labels = c("Homem", "Mulher")) +
  labs(
    title = "Boxplot",
    x = "Sexo",
    y = "Frequência",
    subtitle = "HairEyeColor"
  )


##------------------------------------------------------- ##
## ---------------------- Slide 65 ---------------------- ##
ggplot(dfhair, aes(x = Sex, y = Freq, 
                   fill = Sex)) +
  theme_minimal() +
  geom_boxplot(
    outlier.colour = "black",
    outlier.shape = 8,
    outlier.size = 4
  ) +
  scale_x_discrete(labels = c("Homem", "Mulher")) +
  labs(
    title = "Boxplot",
    x = "Sexo",
    y = "Frequência",
    subtitle = "HairEyeColor"
  ) +
  scale_fill_manual(values = 
                      c("darkorchid4", "brown2"))


##------------------------------------------------------- ##
## ---------------------- Slide 67 ---------------------- ##
#   install.packages("tidyverse"); library(tidyverse)

data(mtcars)
glimpse(mtcars)


##------------------------------------------------------- ##
## ---------------------- Slide 68 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point()


##------------------------------------------------------- ##
## ---------------------- Slide 69 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados")


##------------------------------------------------------- ##
## ---------------------- Slide 70 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point(aes(size = drat)) +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados")


##------------------------------------------------------- ##
## ---------------------- Slide 71 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point(aes(size = cyl)) +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados")


##------------------------------------------------------- ##
## ---------------------- Slide 72 ---------------------- ##
ggplot(mtcars, aes(mpg, drat, colour = cyl)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados",
    colour = "Cilindros")


##------------------------------------------------------- ##
## ---------------------- Slide 73 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados") +
  geom_vline(xintercept = 20, col="red") 


##------------------------------------------------------- ##
## ---------------------- Slide 74 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados") +
  geom_hline(yintercept = 4, col="red") 


##------------------------------------------------------- ##
## ---------------------- Slide 75 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados") +
  geom_vline(xintercept = 10:15, col="red") 


##------------------------------------------------------- ##
## ---------------------- Slide 76 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados") +
  geom_smooth(method = "lm", se = FALSE, col="red") 


##------------------------------------------------------- ##
## ---------------------- Slide 77 ---------------------- ##
ggplot(mtcars, aes(mpg, drat)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Meu gráfico :)",
    subtitle = "Gráfico de Dispersão",
    x = "MPG",
    y = "Drat",
    caption = "Fonte de dados") +
  geom_smooth(method = "lm", se = TRUE, col="red")



##------------------------------------------------------- ##
## ---------------------- Slide 79 ---------------------- ##
## install.packages("BatchGetSymbols")
library(BatchGetSymbols)


#Definindo ações para extração de dados
empresas <- c('PETR4.SA', 'CIEL3.SA')

#Definindo a minha série de tempo
first.date <- Sys.Date() - 90
last.date <- Sys.Date()

#Inserindo os paramentros para extrair o meu dataframe
temp <- BatchGetSymbols(tickers = empresas,
                         first.date = first.date,
                         last.date = last.date, 
                         do.cache = FALSE)

##------------------------------------------------------- ##
## ---------------------- Slide 80 ---------------------- ##
meudataset <- temp$df.tickers

glimpse(meudataset)


##------------------------------------------------------- ##
## ---------------------- Slide 81 ---------------------- ##
ggplot(meudataset, 
       aes(x = ref.date,
           y = volume)) +
  geom_line()


##------------------------------------------------------- ##
## ---------------------- Slide 82 ---------------------- ##
ggplot(meudataset, 
       aes(x = ref.date, 
           y = volume, 
           color = ticker)) +
  geom_line()


##------------------------------------------------------- ##
## ---------------------- Slide 83 ---------------------- ##
ggplot(meudataset, 
       aes(x = ref.date, 
           y = volume, 
           color = ticker)) +
  geom_line() +
  labs(
    title = "Série Temporal",
    subtitle = "Ações",
    x = "Data",
    y = "Volume de Ações",
    caption = "Fonte: Bovespa"
  ) +
  theme_minimal()


##------------------------------------------------------- ##
## ---------------------- Slide 84 ---------------------- ##
ggplot(meudataset, 
       aes(x = ref.date, 
           y = volume, 
           color = ticker)) +
  geom_line() +
  labs(
    title = "Série Temporal",
    subtitle = "Ações",
    x = "Data",
    y = "Volume de Ações",
    caption = "Fonte: Bovespa"
  ) +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "2 week")


##------------------------------------------------------- ##
## ---------------------- Slide 86 ---------------------- ##
##   install.packages("GGally"); 
library(GGally)
 
## #Pacote com paleta de cores otimizada
##   install.packages("viridis"); 
library(viridis)


glimpse(happy)


##------------------------------------------------------- ##
## ---------------------- Slide 87 ---------------------- ##
happy %>% 
  count(age, year) %>% 
  ggplot(aes(age, year, fill = n)) +
    geom_tile() +
    scale_fill_viridis() 


##------------------------------------------------------- ##
## ---------------------- Slide 88 ---------------------- ##
happy %>% 
  count(age, year) %>% 
  group_by(year) %>%
  mutate(total = sum(n),
         prop = n / total) %>%
  ggplot(aes(age, year, fill = prop)) +
    geom_tile() +
    scale_fill_viridis()


##------------------------------------------------------- ##
## ---------------------- Slide 89 ---------------------- ##
happy %>% 
  drop_na(happy) %>% 
  count(age, year, happy) %>% 
  group_by(year, happy) %>%
  mutate(total = sum(n),
         prop = n / total) %>%
  ggplot(aes(age, year, fill = prop)) +
    geom_tile() +
    scale_fill_viridis() +
    facet_grid(. ~ happy)


##------------------------------------------------------- ##
## ---------------------- Slide 90 ---------------------- ##
library(corrplot)

m <- cor(mtcars)
corrplot(m, method = "circle")



##------------------------------------------------------- ##
## ---------------------- Slide 91 ---------------------- ##
corrplot(m, method = "color",
         addCoef.col = "black")


##------------------------------------------------------- ##
## ---------------------- Slide 92 ---------------------- ##
mtcars %>% 
  ggpairs()


##------------------------------------------------------- ##
## ---------------------- Slide 93 ---------------------- ##
mtcars %>% 
  select(drat:am) %>% 
  ggpairs()


##------------------------------------------------------- ##
## ---------------------- Slide 94 ---------------------- ##
mtcars %>% 
  select(drat:am) %>% 
  mutate_at(c("vs", "am"), as_factor) %>% 
  ggpairs()


##------------------------------------------------------- ##
## ---------------------- Slide 95 ---------------------- ##
##   install.packages("plotly"); 
library(plotly)


set.seed(100)
d <- diamonds %>% filter(cut %in% c("Very Good", "Premium", "Ideal")) %>% sample_frac(0.05)
d %>% glimpse


##------------------------------------------------------- ##
## ---------------------- Slide 96 ---------------------- ##
p <- ggplot(d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_grid(.~cut)
ggplotly(p)


##------------------------------------------------------- ##
## ---------------------- Slide 99 ---------------------- ##
## install.packages("esquisse")
library(esquisse)
esquisser()


##-------------------------------------------------------- ##
## ---------------------- Slide 101 ---------------------- ##
library(gganimate)
library(gapminder)
theme_set(theme_bw())

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
  ) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "PIB per capita", y = "Expectativa de vida (anos)")
p + transition_time(year) +
  labs(title = "Ano: {frame_time}")


##------------------------------------------------------- ##
## ---------------------- Slide 102 ---------------------- ##
## install.packages("highcharter")
library(highcharter)
## 
## # data science
## install.packages("tidyverse")
## library(tidyverse)

# prepare data
data(gapminder, package = "gapminder")

asia <- gapminder %>%
  filter(continent == "Americas") %>%
  dplyr::select(year, country, lifeExp)

# convert to long to wide format
plotdata <- spread(asia, country, lifeExp)

# generate graph
highchart() %>%
  hc_xAxis(categories = plotdata$year) %>%
  hc_add_series(name = "Brasil",
                data = plotdata$Brazil) %>%
  hc_add_series(name = "Argentina",
                data = plotdata$Argentina) %>%
  hc_add_series(name = "Paraguai",
                data = plotdata$Paraguay) %>%
  hc_add_series(name = "Uruguai",
                data = plotdata$Uruguay) %>%

  hc_title(text = "Expectativa de vida por País - Mercosul - 
           <center><b> Fonte: <a href='https://rkabacoff.github.io/datavis/Interactive.html'>Adaptado de Datavis with R </a></b></center>",
           margin = 20,
           align = "left",
           style = list(color = "steelblue")) %>%
  hc_subtitle(text = "1952 à 2007",
              align = "left",
              style = list(color = "#2b908f",
                           fontWeight = "bold")) %>%
  hc_credits(enabled = TRUE, # add credits
             text = "Gapminder Data",
             href = "http://gapminder.com") %>%
  hc_legend(align = "left",
            verticalAlign = "top",
            layout = "vertical",
            x = 0,
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE,
             backgroundColor = "#FCFFC5",
             shared = TRUE,
             borderWidth = 4) %>%
  hc_exporting(enabled = TRUE)
