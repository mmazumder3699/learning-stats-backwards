#read in data and clean it
video_sample <- here("Data", "21-006-SIPilot-T1-01.csv") %>%
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
  mutate(bin = cut(time_stamp, breaks = c(0, 60, 120, 180, 240), labels = c("1", "2", "3", "4"))) %>%
  arrange(time_stamp)

#export
video_sample %>%
  write_csv(here("data output", "21-006Sample.csv"))
