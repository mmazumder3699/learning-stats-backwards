#simple regression

#import library about penguin study and other needed stuff

install.packages("palmerpenguins")
library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)

#plot penguin data

penguins %>%
  ggplot(aes(x=flipper_length_mm, y=body_mass_g)) +
  geom_point()

#create linear model of whether flipper length
#predicts body mass

penguins %>%
  lm(body_mass_g ~ flipper_length_mm,
     data = .)%>%
  tidy()

#look at lm of how bill length predicts body mass
#plot relationship

penguins %>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g)) +
  geom_point() 

#lm test

penguins %>%
  lm(body_mass_g ~ bill_length_mm,
     data = .) %>%
  tidy()

#predict mass of penguin that has 40 mm beak

x<-362 + 87.4*40
#x is 3858 g

#plot average body masses across species
penguins %>%
  filter(species %in% c("Adelie", "Gentoo")) %>%
  group_by(species) %>%
  summarise(body_mass_g = mean(body_mass_g, na.rm=TRUE)) %>%
  ggplot(aes(x=species, y=body_mass_g)) +
  geom_bar(stat = "identity")

#lm of this
penguins %>%
  filter(species %in% c("Adelie", "Gentoo")) %>%
  lm(body_mass_g ~ species,
     data = .) %>%
  tidy()

#plot adelie gentoo and chinstrap
penguins %>%
  filter(species %in% c("Adelie", "Gentoo", "Chinstrap")) %>%
  group_by(species) %>%
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE)) %>%
  ggplot(aes(x=species, y=body_mass_g)) +
  geom_bar(stat = "identity")

#regression to test for significant differences
penguins %>%
  filter(species %in% c("Adelie", "Gentoo", "Chinstrap")) %>%
  lm(body_mass_g ~ species,
     data = .) %>%
  tidy()

#looks like adelie and chinstrap are not sig different but gentoo is?

#change reference category to chinstrap and run regression
penguins %>%
filter(species %in% c("Adelie", "Gentoo", "Chinstrap")) %>%
  mutate(species= fct_relevel(species, "Adelie", after =1)) %>%
  lm(body_mass_g ~ species,
     data = .) %>%
  tidy()
#shows that gentoo and chinstrap are also significantly diff

#change reference category to gentoo
penguins %>%
  filter(species %in% c("Adelie", "Gentoo", "Chinstrap")) %>%
  mutate(species= fct_relevel(species, "Gentoo", after =0)) %>%
  lm(body_mass_g ~ species,
     data = .) %>%
  tidy()
#gentoo is sig diff from both species

#correlation bt flipper length and body mass
with(penguins, cor.test(flipper_length_mm, body_mass_g)) %>%
  tidy()
#this is basically the slope of the regression but standarized by giving same sd to each variable

#t tests!!
#compare adelie and gentoo body mass
with(penguins %>% filter(species %in% c("Adelie", "Gentoo")),
     t.test(body_mass_g ~species, var.equal = TRUE)) %>%
  tidy()
#same answer as original regression w sign flip

#practice w covid intervention data
covid<- here("Data", "covid_intervention.csv") %>%
  read_csv() %>%
  mutate(keep = if_else(sub %% 2== 0, "threat", "prosocial")) %>%
  filter(keep ==intervention) %>%
  select(sub, intervention, willingness, change, valence, arousal)

  