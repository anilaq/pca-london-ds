# Figures for Slides

library(tidyverse)

hiphop <- read_csv("hiphop.csv")

hiphop %>%
  filter(points < 50) %>%
  ggplot(aes(x = year, y = points, color = as.factor(mode))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
 

iris %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species)) +
  theme_minimal() +
  labs(title = "Petal Length vs Width", 
       x = "Petal Length", 
       y = "Petal Width")

iris %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(aes(color = Species)) +
  theme_minimal() +
  labs(title = "Petal Length vs Width", 
       x = "Petal Length", 
       y = "Petal Width")
  

library(tidymodels)

library(psych)

iris %>%
  as_tibble() %>%
  select(starts_with("P")) -> iris_subset

model_1 <- pca(iris_subset,2)

model_1$scores

iris_subset %>%
  cbind(model_1$scores) %>%
  mutate(z_length= scale(Petal.Length)) %>%
  mutate(z_width = scale(Petal.Width)) %>%
  cbind(iris$Species) %>%
  mutate(Species = `iris$Species`) %>%
  as_tibble() -> my_iris

my_iris %>%
  ggplot(aes(x = z_length, y = z_width)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Petal Length vs Width", 
       x = "Petal Length", 
       y = "Petal Width")

my_iris %>%
  ggplot(aes(x = RC1, y = RC2)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Petal Length vs Width", 
       x = "PC1", 
       y = "PC2")

