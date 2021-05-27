#call up libraries
library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)

#create variable 
lm_bd <- penguins %>%
  #create regression of body mass ~ bill depth 
  lm(body_mass_g ~ bill_depth_mm,
     data =.)

#look at this data in tidy tibble instead of weird list
tidy(lm_bd)

#could also use R base function to do this
summary(lm_bd)

#glance at different model fit metrics
glance(lm_bd)

#do this again but with flipper length
#create variable of regression
lm_fl <- penguins %>%
  lm(body_mass_g ~ flipper_length_mm,
     data=.)
#look at this in tibble
tidy(lm_fl)

#again  but with bill length
lm_bl<- penguins %>%
  lm(body_mass_g ~ bill_length_mm,
     data=.)
#tibble time
tidy(lm_bl)

#flipper length look at r squared
glance(lm_fl)
#about 76%

#bill length r squared
glance(lm_bl)
#about 35%

#create regression of both variables together
lm_fl_bl <- penguins %>%
  lm(body_mass_g ~ flipper_length_mm + bill_length_mm,
     data=.)

#look at r squared
glance(lm_fl_bl)
#76%

#why so? look at lm intercepts
tidy(lm_fl_bl)
#bill length does not have significant unique contribution if flipper length is constant

#plot flipper length vs body mass w bill depth of each point
penguins %>%
  ggplot(aes(flipper_length_mm, y=body_mass_g, color=bill_length_mm)) +
  geom_point() +
  scale_colour_viridis_b(option = "plasma")