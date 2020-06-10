library(gganimate)
library(dplyr)


n_points <- 250
x <- seq(0, 15, length = n_points)

values_age <- seq(20, 60, by = 5)

## Moving along the x-axis
values_norm <- lapply(1:length(values_age), function(a){
  dnorm(x, mean = 5 + a*0.5, sd = 2)
}) %>% unlist()

data_plot <- data.frame(x = rep(x, times = length(values_age)), 
                        y = values_norm, 
                        age = rep(values_age, each = length(x)))

g <- ggplot(data_plot)
g <- g + geom_line(aes(x, y), colour = 'royalblue') + theme_classic() + 
  xlab("time") + 
  ylab("") + theme(axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank())
cond_normal_animation <- g + 
  transition_states(age, 
                    transition_length = 1,
                    state_length = 1) + 
  ease_aes("cubic-in-out") + 
  shadow_mark(colour = 'grey75', size = 0.75) + 
  ggtitle('Age = {closest_state}')

anim_save("cond_normal_animation.gif", cond_normal_animation, 
          width = 250, height = 250)


x_first <- rep(x, times = length(values_age))
age_vector <- rep(values_age, each = length(x))
y2 <- values_norm + max(values_norm) * age_vector/5

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
  ylab("Y") + 
  xlab("X") +
  geom_line(data = data_to_add, aes(x, y)) + 
  geom_vline(data = data_to_add, aes(xintercept = x), linetype = 2) + 
  geom_hline(data = data_to_add, aes(yintercept = y),  linetype = 2) +
  scale_x_continuous(breaks = min_values,
                     labels = paste0("x_", 1:length(values_age)))

regression_animation <- g + 
  transition_reveal(age)  

anim_save("regression_animation.gif", 
          regression_animation, width = 250, height = 250)


