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

#create variable of lm with bill length and depth together
lm_bl_bd <- penguins %>%
  lm(body_mass_g ~ bill_length_mm + bill_depth_mm,
     data = .)

#look at beta values
tidy(lm_bl_bd)
#have significant individual contributions according to p values

#look at r squared
glance(lm_bl_bd)
#account for 47% of variance together
#this is more than by themselves

#exercises
#read in covid intervention data and clean/tidy
covid_intervention <- here("Data", "covid_intervention.csv") %>%
  read_csv() %>%
  mutate(keep = if_else(sub %% 2 == 0, "threat", "prosocial")) %>%
  filter(keep == intervention) %>%
  select(sub, willingness, valence, arousal, extraversion = bfi_extraversion, neuroticism = bfi_neuroticism)

#willingness = beta 0 + beta1(valence) +beta2(arousal)
#beta0 tells you level of willingness with zero valence and arousal
#beta1 tells you the change in willingness with a one unit change in valence, when arousal is constant
#beta2 is like the same thing but with arousal changing and valence constant

#create the lm for this set up
lm_val_aro <- covid_intervention %>%
  lm(willingness ~ valence + arousal,
     data=.)

#look at beta values
tidy(lm_val_aro)
#looks like both are affecting willingness significantly but the magnitude of effect is small
#i would say this doesn't really support hypothesis, negative valence part is just wrong
#arousal effect looks small and would have to consider

#calculate mean arousal value
covid_intervention %>%
  summarise(mean(arousal))
#min arousal
covid_intervention %>%
  summarise(min(arousal))
#max arousal
covid_intervention %>%
  summarise(max(arousal))
#standard deviation arousal
covid_intervention %>%
  summarise(sd(arousal))
#look at r squared
glance(lm_val_aro)
#only explains 4% of the variation, again not v convincing/predictive

#create individual lms and look at r squared
lm_val <- covid_intervention %>%
  lm(willingness ~ valence,
     data=.)
#beta for valence
tidy(lm_val)

#rsquared for valence
glance(lm_val)

lm_aro <- covid_intervention %>%
  lm(willingness ~ arousal,
     data=.)

#beta for arousal
tidy(lm_aro)

#rsquared for arousal
glance(lm_aro)

#conclusions: they both are significant on their own (based on p values) and explain the data better together
#than apart (rsquared values). however, hypothesis about effect of valence is incorrect, and i'm not sure how
#to interpret effect of arousal bc beta is pretty small? but also give how high willingness is to begin
#with it could be seen as a sizable effect. 

#effects of personality

#hypothesis(es): as people become more extroverted and neurotic they become less willing to self isolate
# to test: willingness = beta0 + beta1extra +beta2neuro

#shared lm
lm_ex_ne<- covid_intervention %>%
  lm( willingness ~ extraversion + neuroticism,
      data=.)
#beta and p
tidy(lm_ex_ne)
#neither shows significant p values- even if they were beta shows opposite of hyp. for extraversions effect

#r squared
glance(lm_ex_ne)
#explains very little of the variance, .23%

#individual lms
lm_ex <- covid_intervention %>%
  lm( willingness ~ extraversion,
      data=.)

tidy(lm_ex)
#still not significant!

glance(lm_ex)
#.20% of the variance

lm_ne<- covid_intervention %>%
  lm( willingness ~ neuroticism,
      data=.)

tidy(lm_ne)
#still not significant!

glance(lm_ne)
#.13% of variance

#conclusions: neither extraversion or neuroticism significantly impact willingness, share a fair bit of the variance

#control for personality by including in lm with emotion variables. if variance is shared could impact beta and p values of
#emotion dimensions and make them not significant, though less likely bc personality explains v little on its own

#emotion and personality lm
lm_emo_bfi <- covid_intervention %>%
  lm(willingness ~ valence + arousal + extraversion + neuroticism,
     data=.)

#look at beta and p
tidy(lm_emo_bfi)
#beta for all seem relatively similar to individual estimates before, valence and arousal still significant

#look at rsquared
glance(lm_emo_bfi)
#4.09% compared to 3.89% so no/little shared variance imo




