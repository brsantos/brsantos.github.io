xaringanExtra::use_scribble()
xaringanExtra::use_share_again()

knitr::opts_chunk$set(echo = FALSE, 
                      fig.align = "center", 
                      message = FALSE, 
                      warning = FALSE, 
                      comment = "  #>", 
                      collapse = T)

library(tidyverse)
library(MASS) 
library(skimr)
library(viridis)
library(gganimate)
library(highcharter)

knitr::include_graphics("img/new_allison/ggplot2_masterpiece.png")

knitr::include_graphics("img/rlogos/hex-ggplot2.png")

## ?plot

## ?par

x <- 3:15
plot(x)

x <- 3:15
y <- 6:18
plot(x, y)

x <- 3:15
y <- 6:18
plot(y, x)

x <- 3:15
y <- 6:18
plot(y = y, x = x)

x <- 3:15
y <- 6:18
plot(y = y, x = x, type = 'l')

x <- 3:15
y <- 6:18
plot(y = y, x = x, type = 'b')

x <- 3:15
y <- 6:18
plot(y = y, x = x, type = 'h')

x <- 3:15
y <- 6:18
plot(y = y, x = x, type = 'h', 
     col = 2, 
     lty = 2)

x <- 3:15
y <- 6:18
plot(y = y, x = x, type = 'b', 
     col = 4, 
     lty = 4,
     pch = 16)

x <- 3:15
y <- 6:18

par(mfrow = c(1, 2))

plot(y = y, x = x, type = 'b', 
     col = 4, 
     lty = 4,
     pch = 16)

plot(y = y, x = x, type = 'h', 
     col = 4, 
     lty = 4)

x <- 3:15
y <- 6:18

par(mfrow = c(2, 2))

plot(y = y, x = x, type = 'b', 
     col = 4, lty = 4, pch = 16)

plot(y = y, x = x, type = 'h', 
     col = 4, lty = 4)

plot(y = y, x = x, type = 'l', 
     col = "darkviolet", lty = 5)

plot(y = y, x = x, type = 'o', 
     col = "royalblue", lty = 3)

x <- 3:15
y <- 6:18
z <- y + 3
w <- z + 3

plot(x, y, type = 'l')

lines(x, z, col = 2)

points(x, w, col = 4)

x <- 3:15
y <- 6:18
z <- y + 3
w <- z + 3

plot(x, y, type = 'l', 
     ylim = c(5, 24)) #<<

lines(x, z, col = 2)

points(x, w, col = 4)

boxplot(mtcars$mpg ~ mtcars$cyl, 
        ylab = "Miles per gallon",
        xlab = "Cylinders")

## ggplot(data) +
##   aesthetics +
##   geometries

knitr::include_graphics("img/communication/communication_11_GrammarGraphicsGgplot_sintaxe.png")

## ggplot(data) +
##   aesthetics +
##   geometries +
##   facets +
##   coordinates +
##   theme

knitr::include_graphics("img/communication/communication_11_GrammarGraphicsGgplot.png")

d <- tibble(var_x = 1:4,
            var_y = seq(2, 8, 2),
            var_group = c(rep("a", 3), "b"))
d

ggplot(d)

ggplot(d, aes(x = var_x, y = var_y))

ggplot(d) + 
  aes(x = var_x, y = var_y)

ggplot(d) + 
  aes(x = var_group, y = var_y)

ggplot(d) + 
  aes(x = var_x, y = var_y) + 
  geom_point()

ggplot(d) + 
  aes(x = var_x, y = var_y) + 
  geom_line()

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_point(color = "darkviolet")

ggplot(d) +  
  geom_point(aes(x = var_x, y = var_y, 
                 color = var_group))

ggplot(d) +  
  geom_line(aes(x = var_x, y = var_y, 
                 color = var_group))

d <- tibble(var_x = 1:5,
            var_y = seq(2, 10, 2),
            var_group = c("a", "a", "a",
                          "b", "b"))

ggplot(d) +  
  geom_line(aes(x = var_x, y = var_y, 
                 color = var_group))

ggplot(d) +  
  geom_line(aes(x = var_x, y = var_y, 
                 linetype = var_group))

ggplot(d) +  
  geom_line(aes(x = var_x, y = var_y, 
                 color = var_group)) +
  geom_point(aes(x = var_x, y = var_y))

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(aes(color = var_group)) +
  geom_point()

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_point() + 
  geom_line(aes(color = var_group))

ggplot(d) +  
  aes(x = var_x, y = var_y, color = var_group) +
  geom_point() + 
  geom_line()

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line() + 
  geom_point(aes(color = var_group)) 

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(aes(color = var_group)) 

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") 

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") + 
  facet_wrap(~ var_group) #<<

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") + 
  facet_wrap(~ var_group, 
             scales = "free_x") #<<

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") + 
  facet_wrap(~ var_group, 
             scales = "free_y") #<<

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") + 
  facet_wrap(~ var_group, 
             scales = "free") #<<

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") + 
  facet_wrap(~ var_group) + 
  coord_equal() #<<

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") + 
  facet_wrap(~ var_group) + 
  coord_equal() + 
  theme_bw() #<<

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") + 
  facet_wrap(~ var_group) + 
  coord_equal() + 
  theme_minimal() #<<

ggplot(d) +  
  aes(x = var_x, y = var_y) +
  geom_line(color = "yellow") + 
  geom_point(color = "darkviolet") + 
  facet_wrap(~ var_group) + 
  coord_equal() + 
  theme_classic() #<<

## library(ggplot2)
## data(diamonds)
## ?diamonds

## library(skimr)
## skim(diamonds)

library(janitor)
tabyl(diamonds$cut)

library(janitor)
tabyl(diamonds, cut, color)

ggplot(diamonds) +  
  aes(x = cut) +
  geom_bar()

pie(table(diamonds$cut))

knitr::include_graphics("img/funny-pie-charts.jpeg")

ggplot(diamonds) +  
  aes(x = cut) +
  geom_bar(fill = "royalblue")

ggplot(diamonds) +  
  aes(x = cut) +
  geom_bar(color = "royalblue")

cont_table <- tabyl(diamonds, cut)
cont_table

ggplot(cont_table) +  
  aes(x = cut, y = n) +
  geom_bar(fill = "royalblue", 
           stat = "identity") #<<

ggplot(cont_table) +  
  theme_bw() + 
  aes(x = cut, y = n) +
  geom_bar(fill = "royalblue", 
           stat = "identity")  + 
  geom_text(aes(label = n), #<<
            color = "royalblue") #<<

ggplot(cont_table) +  
  theme_bw() + 
  aes(x = cut, y = n) +
  geom_bar(fill = "royalblue", 
           stat = "identity")  + 
  geom_text(aes(label = n), 
            color = "royalblue", 
            nudge_y = 1000) #<<

ggplot(cont_table) +  
  theme_bw() + 
  aes(x = cut, y = n) +
  geom_bar(fill = "royalblue", 
           stat = "identity")  + 
  geom_text(aes(label = n), 
            color = "royalblue", 
            nudge_y = 1000) + 
  geom_text(aes(label = round(percent, 2)), 
            color = "grey25", 
            nudge_y = 2000)

ggplot(cont_table) +  
  theme_bw() + 
  aes(x = cut, y = n) +
  geom_bar(fill = "royalblue", 
           stat = "identity")  + 
  geom_text(aes(label = n), 
            color = "royalblue", 
            nudge_y = 1000) + 
  geom_text(aes(label = round(percent, 2)), 
            color = "grey25", 
            nudge_y = 2000) + 
  coord_flip() #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut) +
  geom_bar(fill = "royalblue")  + 
  facet_wrap(~ color)

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut) +
  geom_bar(fill = "royalblue")  + 
  facet_wrap(~ color, scales = "free_y", 
             ncol = 4) #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + #<<
  geom_bar()  

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "fill")  #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "fill")  + 
  labs(y = "Percentage of clarity", #<<
       title = "Distribution of clarity", #<<
       subtitle = "by different cuts") #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "dodge")  #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "dodge") +  
  facet_wrap( ~ cut, scales = "free") #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = color, fill = color) + #<<
  geom_bar(position = "dodge") +  
  facet_wrap( ~ cut, scales = "free") 

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = color, fill = color) + 
  geom_bar(position = "dodge") +  
  facet_wrap( ~ cut, scales = "free") + 
  theme(legend.position = "none") #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price) + 
  geom_point() #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price) + 
  geom_point(alpha = 0.1) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price, 
      colour = cut) + #<<
  geom_point(alpha = 0.1) 

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price, 
      colour = cut) + 
  geom_point() + 
  facet_wrap(~ clarity) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price, 
      color = color) + 
  geom_point() + 
  facet_grid(cut ~ clarity) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price) + 
  geom_histogram() #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price) + 
  geom_histogram(fill = "darkviolet", #<<
                 color = "grey75") #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price, y = ..density..) + #<<
  geom_histogram(fill = "darkviolet", 
                 color = "grey75") + 
  geom_density() #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price, y = ..density..) + 
  geom_histogram(fill = "darkviolet", 
                 color = "grey75") + 
  geom_density(color = "royalblue") #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price, y = ..density..) + 
  geom_density(aes(color = clarity)) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price, y = ..density..) + 
  geom_density(aes(fill = clarity)) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price, y = ..density..) + 
  geom_density(aes(fill = clarity), 
               alpha = 0.5) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price, y = ..density..) + 
  geom_histogram(fill = "darkviolet", 
                 color = "grey75") + 
  facet_wrap(~ clarity) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = price, y = ..density..) + 
  geom_histogram(fill = "darkviolet", 
                 color = "grey75") + 
  facet_wrap(~ clarity, 
             scales = "free_y") #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(y = price) + 
  geom_boxplot(fill = "darkviolet", #<<
                 color = "black") #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(y = price, x = cut) + 
  geom_boxplot(fill = "darkviolet", 
                 color = "black") 

ggplot(diamonds) +  
  theme_bw() +
  aes(y = price, x = cut) + 
  geom_boxplot(aes(fill = clarity)) 

# Just for purposes of illustration
ggplot(diamonds) +  
  theme_bw() +
  aes(y = price, x = cut) + 
  geom_boxplot(aes(fill = clarity), 
               outlier.alpha = 0) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price) + 
  geom_point() + 
  scale_x_continuous(breaks = c(0, 1.2, 1.4, 2, 3, 4.5, 5)) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price) + 
  geom_point() + 
  scale_x_continuous(breaks = c(0, 1.2, 1.4, 2, 3, 4.5, 5)) + 
  scale_y_continuous(breaks = seq(0, 2000, by = 250)) #<<

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price) + 
  geom_point() + 
  geom_vline(xintercept = c(1.2, 1.4, 2, 3, 4.5, 5), #<<
              color = "red", linetype = 2) + #<<
  scale_y_continuous(breaks = seq(0, 20000, by = 2500)) 

ggplot(diamonds) +  
  theme_bw() +
  aes(x = carat, y = price) + 
  geom_point() + 
  geom_vline(xintercept = c(0, 1.2, 1.4, 2, 3, 4.5, 5), 
                 color = "red", linetype = 2) + 
  geom_hline(yintercept = c(7500, 12600), #<<
             color = "blue", #<<
             linetype = "dotted") + #<<
  scale_y_continuous(breaks = seq(0, 20000, by = 2500)) 

# Using a different data set
library(gapminder)
data(gapminder)

gapminder_2007 <- subset(gapminder, 
                         year == 2007)

ggplot(gapminder_2007) +  
  theme_bw() +
  aes(x = gdpPercap, y = lifeExp) + 
  geom_point(aes(size = pop, color = continent))

# Using a different data set
library(gapminder)
data(gapminder)

gapminder_2007 <- subset(gapminder, 
                         year == 2007)

ggplot(gapminder_2007) +  
  theme_bw() +
  aes(x = gdpPercap, y = lifeExp) + 
  geom_point(aes(size = pop, color = continent)) +
  scale_size(name = "Population") + 
  scale_color_viridis_d(name = "Continent")

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  scale_fill_brewer(palette = "Purples", #<<
                    direction = 1) #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  scale_fill_brewer(palette = "Purples", #<<
                    direction = -1) #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  scale_fill_brewer(palette = "Reds", #<<
                    direction = -1) #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  scale_fill_grey() #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  scale_fill_viridis_d(option = "B", #<<
                       direction = -1) #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut) +
  geom_bar(fill = "royalblue")  + 
  facet_wrap(~ color)

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut) +
  geom_bar(fill = "royalblue")  + 
  facet_wrap(~ color) + 
  theme(axis.text.x = element_text(angle = 45)) #<<

ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut) +
  geom_bar(fill = "royalblue")  + 
  facet_wrap(~ color) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5), #<<
        axis.title.y = element_text(size = 15, #<<
                                    color = "red"))   #<<

## ?ggsave

## g <- ggplot(diamonds) +
##   theme_bw() +
##   aes(x = cut, fill = color) +
##   geom_bar(position = "fill") +
##   coord_flip() +
##   scale_fill_viridis_d(option = "B", direction = -1)
## 
## ggsave("my_plot.png", plot = g)

x <- 3:15
y <- 6:18

par(mfrow = c(2, 2))

plot(y = y, x = x, type = 'b', 
     col = 4, lty = 4, pch = 16)

plot(y = y, x = x, type = 'h', 
     col = 4, lty = 4)

plot(y = y, x = x, type = 'l', 
     col = "darkviolet", lty = 5)

plot(y = y, x = x, type = 'o', 
     col = "royalblue", lty = 3)

library(COVID19)
italy <- covid19(country = "Italy", verbose = FALSE)
us <- covid19(country = "US", verbose = FALSE)
uk <- covid19(country = "United Kingdom", verbose = FALSE)

ggplot() +  
  theme_bw() + 
  geom_line(data = italy, color = "blue") + 
  geom_line(data = us, color = "orange") + 
  geom_line(data = uk, color = "red") + 
  aes(x = date, y = deaths)

g1 <- ggplot() +  
  theme_bw() + 
  geom_line(data = italy, color = "blue") + 
  geom_line(data = us, color = "orange") + 
  geom_line(data = uk, color = "red") + 
  aes(x = date, y = deaths) + 
  scale_x_date(date_breaks = "3 months") + 
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5))
g1

library(season)
?schz

g2 <- ggplot(schz, aes(year, month, 
                       fill = SczBroad)) + 
  geom_tile(colour = "gray20", 
            size = 1.5, 
            stat = "identity") + 
  scale_fill_viridis(option = "A") +
  scale_y_continuous(breaks = 1:12, 
                     labels = month.abb[1:12]) + 
  xlab("") + 
  ylab("") +
  ggtitle("Total Australian Schizophrenics Born By Month and Year") + 
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(), 
    panel.background = element_blank()
  )

library(patchwork)
g <- ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = color) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  scale_fill_viridis_d(option = "B", direction = -1)
 
(g + g1) / g2

library(patchwork)
g4 <- ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = clarity) + 
  geom_bar(position = "dodge") + 
  coord_flip() + 
  scale_fill_brewer(palette = "Reds", 
                    direction = -1)
 
g + g1 + g4 + g2 + 
  plot_layout(ncol = 3)

library(patchwork)
g4 <- ggplot(diamonds) +  
  theme_bw() + 
  aes(x = cut, fill = clarity) + 
  geom_bar(position = "dodge") + 
  coord_flip() + 
  scale_fill_brewer(palette = "Reds", 
                    direction = -1)
 
g + g1 + g4 + g2 + 
  plot_layout(widths = c(1, 2))

p1 <- ggplot(mtcars) + 
  geom_point(aes(mpg, disp)) + 
  ggtitle('Plot 1')

p2 <- ggplot(mtcars) + 
  geom_boxplot(aes(gear, disp, group = gear)) + 
  ggtitle('Plot 2')

p1 + inset_element(p2, 
                   left = 0.6, 
                   bottom = 0.6, 
                   right = 1, 
                   top = 1)

g5 <- ggplot(diamonds) + 
  geom_bar(aes(x = clarity, fill = cut))

g6 <- ggplot(diamonds) + 
  geom_bar(aes(x = color, fill = cut))

g5 / g6 + plot_layout(guides = 'collect')

## library(rgdal)
## path_to_maps <- "SP_Municipios_2020/"
## 
## sp_map <-
##   readOGR(dsn = path_to_maps,
##           layer = 'SP_Municipios_2020',
##           verbose = FALSE)
## 
## plot(sp_map)

library(rgdal)
library(maptools)
path_to_maps <- "SP_Municipios_2020/"

sp_map <- 
  readOGR(dsn = path_to_maps, 
          layer = 'SP_Municipios_2020', 
          verbose = FALSE)

plot(sp_map)

data_plot <- read.csv("sp_data.csv") 
data_plot <- data_plot %>% 
  mutate(code = as.character(code_city))

## skim(data_plot)

sp_map@data$id <- rownames(sp_map@data)
gpclibPermit()
sp_map_points <- fortify(sp_map, region = "id")

map_sp_df <- dplyr::inner_join(sp_map_points, 
                              sp_map@data, 
                              by = "id")

map_sp_df <- map_sp_df %>% 
  left_join(data_plot, 
            by = c("CD_MUN" = "code"))

ggplot(map_sp_df) +
  theme_minimal() +
  geom_polygon(aes(x = long, y = lat,
                   group = group,
                   fill = life_exp)) +
  geom_path(aes(long, lat, group = group),
            color = "black") +
  facet_wrap(~ year, ncol = 1) +
  coord_equal() +
  theme_minimal() +
  scale_fill_viridis_c("Life Expectancy") +
  theme(legend.position = 'bottom')

ggplot(filter(map_sp_df, year == 2010)) +
  theme_minimal() +
  geom_polygon(aes(x = long, y = lat,
                   group = group,
                   fill = year_edu)) +
  geom_path(aes(long, lat, group = group),
            color = "black") +
  coord_equal() +
  theme_minimal() +
  scale_fill_viridis_c("Years of Education") +
  theme(legend.position = 'bottom')

plot_map <- function(var_name, title){
  ggplot(filter(map_sp_df, year == 2010)) +
  theme_minimal() +
  geom_polygon(aes_string(x = "long", y = "lat",
                   group = "group",
                   fill = var_name)) +
  geom_path(aes(long, lat, group = group),
            color = "black") +
  coord_equal() +
  theme_minimal() +
  scale_fill_viridis_c(title) +
  theme(legend.position = 'bottom')
}

plot_map("hdi", "Human Development Index")

gapminder_2007 <- subset(gapminder, 
                         year == 2007)

ggplot(gapminder_2007) +  
  theme_bw() +
  aes(x = gdpPercap, y = lifeExp) + 
  geom_point(aes(size = pop, color = continent))

library(gganimate)
p <- ggplot(gapminder, 
            aes(gdpPercap, 
                lifeExp, 
                size = pop, 
                colour = country)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  theme(legend.position = 'none') +
  labs(title = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) + #<<
  ease_aes('linear') #<<

animate(p, 100, 10)



library(highcharter)
mpgman2 <- count(mpg, manufacturer, year)

hchart(
  mpgman2, 
  "bar",
  hcaes(x = manufacturer, y = n, group = year),
  color = c("#7CB5EC", "#F7A35C"),
  name = c("Year 1999", "Year 2008")
  )
