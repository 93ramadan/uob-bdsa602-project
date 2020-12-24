install.packages("tidyverse")
install.packages("broom")
library(tidyverse)
library(broom)

mtcars %>% 
  nest(-am) %>% 
  mutate(am = factor(am, labels = am),
         fit = map(data, ~ lm(mpg ~ hp, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = mpg, y = .fitted)) +
  #geom_abline(intercept = 3, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(am ~ .) +
  labs(x = "Miles Per Gallon", y = "Predicted Value") +
  theme_bw()


COVID19 %>% 
  nest(-country) %>% 
  mutate(country = factor(country, labels = country),
         fit = map(data, ~ lm(total_cases ~ date, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = total_cases, y = .fitted)) +
  #geom_abline(intercept = 3, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(country ~ .) +
  labs(x = "Miles Per Gallon", y = "Predicted Value") +
  theme_bw()

library(nlme)
model = lmList(new_cases ~ date | country, data = COVID19 )
