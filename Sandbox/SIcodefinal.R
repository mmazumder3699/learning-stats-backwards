#read in data and clean it
video_ <- here("Data", "21-008-SIPilot-T1.csv") %>%
  read_csv() %>%
  clean_names() %>%
  filter(marker_name %in% c("North West", "South East")) %>%
  select("marker_name", "time_stamp", "time_stamp2" ) %>%
  #add column for bout duration
  group_by(marker_name) %>%
  mutate(bout_duration = time_stamp2-time_stamp) %>%
  #add column for total bouts at each side
  mutate(new_bout = case_when(
    marker_name == "North West" ~ "1",
    TRUE ~ "1"
  )
  ) %>%
  mutate(total_bouts = cumsum(new_bout)) %>%
  ungroup() %>%
  #add time bin markers
  mutate(bin = cut(time_stamp, breaks = c(0, 60, 120, 180, 240, 300, 360), labels = c("1", "2", "3", "4", "5", "6"))) %>%
  group_by(marker_name) %>%
  mutate(total_bout_duration= cumsum(bout_duration))

video_ %>%
  ggplot(aes(x=marker_name, y= bout_duration)) +
  geom_bar(stat ="identity")


#export
video_%>%
  write_csv(here("data output", "21-008-SIPilot-T1-Final.csv"))

