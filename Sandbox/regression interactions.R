
#load libraries
library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)
library(lubridate)
library(janitor)

#additive linear model of bill length/depth
lm_bl_bd_add<- penguins %>%
  lm(body_mass_g ~ bill_length_mm + bill_depth_mm,
     data=.)

#visualize this
expand_grid(
  bill_length_mm = seq(30, 60, 1),
  bill_depth_mm = seq(13, 22, 1)
) %>%
  mutate(predicted_body_mass_g = predict(lm_bl_bd_add, newdata = .)) %>%
  ggplot(aes(x=bill_length_mm,
             y= predicted_body_mass_g,
             color=bill_depth_mm,
             group=bill_depth_mm)) +
  geom_point() +
  geom_line()

#create model with interaction terms
lm_bl_bd_int <- penguins %>%
  lm(body_mass_g ~ bill_length_mm * bill_depth_mm,
     data=.)

#visualize
expand_grid(
  bill_length_mm = seq(30, 60, 1),
  bill_depth_mm = seq(13, 22, 1)
) %>%
  mutate(predicted_body_mass_g = predict(lm_bl_bd_int, newdata=.)) %>%
  ggplot(aes(x=bill_length_mm,
             y=predicted_body_mass_g,
             color=bill_depth_mm,
             group=bill_depth_mm)) +
  geom_point() +
  geom_line()

#line of best fit for effect of species * bill length on body mass
penguins %>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g, color=species)) +
  geom_point() +
  geom_smooth(method = "lm")

#look at the table for this interaction
penguins %>%
  lm(body_mass_g ~ bill_length_mm * species,
     data=.) %>%
  tidy() %>%
  kable()

#categorical by categorical
penguins %>%
  select(body_mass_g, species, sex) %>%
  drop_na() %>%
  ggplot(aes(x=species, y=body_mass_g, color=sex)) +
  geom_point(alpha = 0.5,
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.5)) +
  stat_summary(geom = "crossbar", fun = mean,
               width = 0.75, position = position_dodge(width = 0.5))

#effect of sex when interacting bt different species
penguins %>%
  lm(body_mass_g ~ sex * species,
     data=.) %>%
  tidy() %>%
  kable()

#continuous by continuous

#graph some data
penguins %>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g, color=bill_depth_mm)) +
  geom_point() +
  scale_colour_viridis_b(option = "plasma", end = 0.8) +
  geom_smooth(method = "lm")

#better visualize interaction bt bill depth and bill length
penguins %>%
  mutate(bill_depth_groups = cut_number(bill_depth_mm, n=2)) %>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g, color=bill_depth_groups)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE)

#same visualization idea but break into smaller groups
penguins %>%
  mutate(bill_depth_groups = cut_number(bill_depth_mm, n=5)) %>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g, color=bill_depth_groups)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE)

#actual interaction analysis
penguins %>%
  lm(body_mass_g ~ bill_length_mm * bill_depth_mm,
     data=.) %>%
  tidy() %>%
  kable()

#exercises

#load in bike riding data
pioneer_riders <- here("Data", "pioneer_riders.csv") %>%
  read_csv() %>%
  mutate(season = case_when(
    date %within% interval(ymd("2005-03-20"), ymd("2005-06-19")) ~ "Spring",
    date %within% interval(ymd("2005-06-20"), ymd("2005-09-21")) ~ "Summer",
    date %within% interval(ymd("2005-09-22"), ymd("2005-12-20")) ~ "Fall"
  )) %>%
  select(riders, season, day, hi, lo, precip, clouds, weekday)

#plot effect of warmth in different seasons
pioneer_riders %>%
  ggplot(aes(x=hi, y=riders, color=season)) +
  geom_point() +
  geom_smooth(method = "lm")

#visually looks like hi has strongest effect on riders in spring, then fall, then summer no effect

#perform regression
pioneer_riders %>%
  lm(riders ~ hi *season,
     data=.) %>%
  tidy() %>%
  kable()
#there is positive interaction bt hi and spring and not summer, so follows
#not significant tho?


#willingness to self isolate
covid <- here("Data", "covid_intervention.csv") %>%
  read_csv() %>%
  mutate(keep = if_else(sub %% 2 == 0, "threat", "prosocial")) %>%
  filter(keep == intervention) %>%
  select(-keep) %>%
  mutate(intervention = fct_relevel(intervention, "threat"))

#valence and prosocial vs threat interaction visual
covid %>%
  ggplot(aes(x=valence, y=willingness, color=intervention)) +
  geom_point() +
  geom_smooth(method = "lm")
#increased valence does seem to matter w prosocial and not threat

#regression of this
covid %>%
  lm(willingness ~ valence *intervention,
     data=.) %>%
  tidy() %>%
  kable()
#same trend, small positive effect of valence*prosocial

#increased arousal w prosocial vs threat
covid %>%
  ggplot(aes(x=arousal, y=willingness, color=intervention)) +
  geom_point() +
  geom_smooth(method = "lm")
#visually no difference

#test regression
covid %>%
  lm(willingness ~ arousal *intervention,
     data=.) %>%
  tidy() %>%
  kable()
#ya it is not significant



