#call up libraries
library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)
library(lubridate)
library(janitor)

#simple categorical test- difference contrasts bt reference and other
#penguin species body mass difference contrast

penguins %>%
  lm(body_mass_g ~ species,
     data=.) %>%
  tidy()

#do this but no intercept in model
penguins %>%
  lm(body_mass_g ~ 0 + species,
     data=.) %>%
  tidy()

#plot species and sex against body mass
penguins %>%
  group_by(species, sex) %>%
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE), .groups = "drop") %>%
  drop_na() %>%
  ggplot(aes(x= species, y= body_mass_g, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge())

#regression
penguins %>%
  lm(body_mass_g ~ species +sex,
     data=.) %>%
  tidy()
#body mass difference bt male adelie and male chinstrap
26.9 g more
#absolute terms of male chinstrap
3372 + 668 +26.9= 4066.9

#islands as predictors of body mass alone
penguins %>%
  lm(body_mass_g ~ island,
     data=.) %>%
  tidy()
#conclude: biscoe sig more mass than the other islands

#add species as a predictor as well
penguins %>%
  lm(body_mass_g ~ island + species,
     data=.) %>%
  tidy()
#conclude: penguins do not differ sig across islands, still do across species

#visualize the data
penguins %>%
  group_by(species, island) %>%
  summarise(body_mass_g = mean(body_mass_g, na.rm =T),.groups = "drop") %>%
  drop_na() %>%
  ggplot(aes(x=species, y=body_mass_g, fill=island)) +
  geom_bar(stat= "identity", position = position_dodge())

#looks like there isnt data for all 3 on every island
#basically just representing more gentoo on biscoe in island model
#should remove island bc not helpful/unique variance

#read in ma walking path data for contrasts
pioneer_riders <- here("Data", "pioneer_riders.csv") %>%
  read_csv() %>%
  select(date, day, riders) %>%
  mutate(season = case_when(
    date %within% interval(ymd("2005-03-20"), ymd("2005-06-19")) ~ "Spring",
    date %within% interval(ymd("2005-06-20"), ymd("2005-09-21")) ~ "Summer",
    date %within% interval(ymd("2005-09-22"), ymd("2005-12-20")) ~ "Fall"
  )) %>%
  mutate(part_of_week = case_when(day == "Friday" ~ "Friday",
                                  day %in% c("Saturday", "Sunday") ~ "Weekend",
                                  TRUE ~ "Weekday")) %>%
  mutate(season = fct_relevel(season, "Spring", "Summer", "Fall"),
         part_of_week = fct_relevel(part_of_week, "Weekday", "Friday", "Weekend"))

#test whether saturdays are more popular
pioneer_riders %>%
  mutate(day = fct_relevel(day, "Saturday", after= 0)) %>%
  lm(riders ~ day,
     data=.) %>%
  tidy()
#conclude: sig more than on wednesday, comparable with every other day

#helmert contrast: look at friday against weekeday and weekend against weekday and friday
pioneer_riders %>%
  lm(riders ~ part_of_week,
     contrasts = list(part_of_week =contr.helmert),
     data = .) %>%
  tidy()
#conclusion: weekday not diff from friday, weekend not sig more than friday and weekday
#intercept is grand mean across categories

pioneer_riders %>%
  summarise(riders = mean(riders))

#successive differences across seasons
pioneer_riders %>%
  lm(riders ~ season,
     contrasts = list(season = MASS::contr.sdif),
     data = .) %>%
  tidy()
#riding goes down from summer to fall sig

#look at ridership day of the week and season together, general patterns

#simple regression comparing ridership each day of the week
#deviation contrast (compare to grand mean)

pioneer_riders %>%
  mutate(day = fct_relevel( day,
                            "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>%
  lm(riders ~ day,
     contrasts = list(day = contr.sum),
     data=.) %>%
  tidy()

#do again but account for season successively
pioneer_riders %>%
  mutate(day = fct_relevel( day,
                            "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>%
  lm(riders ~ day + season,
     contrasts = list(day = contr.sum,
                      season = MASS::contr.sdif),
     data=.) %>%
  tidy()
#monday sig more across seasons, wed sig less? i am getting v confused
# visualized data
pioneer_riders %>%
  mutate(day = fct_relevel(day,
                           "Monday", "Tuesday", "Wednesday", "Thursday",
                           "Friday", "Saturday", "Sunday")) %>%
  group_by(day, season) %>%
  summarise(riders = mean(riders), .groups = "drop") %>%
  ggplot(aes(x=day, y=riders, fill=day)) +
  facet_grid(cols = vars(season)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

#exercises
#tidy up education and race data
us_regions <- tibble(state_name = state.name,
                     state_abb = state.abb,
                     state_region = state.region)

school <- here("Data", "school_diversity.csv") %>%
  read_csv() %>%
  clean_names() %>%
  rename(school_id = leaid,
         district_id = lea_name,
         state = st,
         district_type = d_locale_txt,
         native = aian,
         total_students = total) %>%
  select(school_id:total_students) %>%
  left_join(us_regions, by = c("state"="state_abb")) %>%
  separate(district_type, into = c("urbanicity", "district_size")) %>%
  filter(school_year == "2016-2017") %>%
  mutate(urbanicity = fct_relevel(urbanicity, "rural", "town", "suburban"))

#create plot to visualize all the data

#are there fewer black students in the west?
school %>%
  mutate(state_region = fct_relevel(state_region, "West", after=0)) %>%
  lm(black ~ state_region,
     data=.) %>%
  tidy()
#conclude: yes, sig more black student proportion in other regions

#are there more asian students in the west than other regions?
school %>%
  mutate(state_region = fct_relevel(state_region, "West", after=0)) %>%
  lm(asian ~ state_region,
     data=.) %>%
  tidy()
#more than south and north central, comparable to northeast

#account for increasing urbanicity in previous two questions
#black
school %>%
  mutate(state_region = fct_relevel(state_region, "West", after=0)) %>%
  lm(black ~ state_region + urbanicity,
     contrasts = list(urbanicity = MASS::contr.sdif),
     data=.) %>%
  tidy()
#still sig more in regions other than the west, proportion increases with urbanicity

#asian
school %>%
  mutate(state_region = fct_relevel(state_region, "West", after=0)) %>%
  lm(asian ~ state_region + urbanicity,
     contrasts = list(urbanicity = MASS::contr.sdif),
     data=.) %>%
  tidy()
#sig less in all regions compared to west now, urbancity increases w proportion

#native trends w regions and urbanicity
school %>%
  lm(native ~ state_region + urbanicity,
     contrasts = list(urbanicity = MASS::contr.sdif),
     data=.) %>%
  tidy()
#proportion decreases with urbanicity increase

#white trends with regions and urbanicity
school %>%
  lm(white ~ state_region + urbanicity,
     contrasts = list(urbanicity = MASS::contr.sdif),
     data=.) %>%
  tidy()
#also decreased proportion as urbanicity increases


#ok back to ridership, playing around with visualization
pioneer_riders %>%
  group_by(part_of_week) %>%
  summarise(riders = mean(riders, na.rm = T)) %>%
  ggplot(aes(x = part_of_week, y = riders)) +
  geom_bar(stat = "identity")

