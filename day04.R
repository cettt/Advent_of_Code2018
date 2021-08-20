library(dplyr)
data04 <- read.table("input/day04.txt", sep = "_", comment.char = "", col.names = "x") %>%
  as_tibble() %>%
  transmute(
    daytme = as.POSIXlt(sub("\\[(.*)\\].*", "\\1", x)),
    day = as.Date(daytme),
    tme = format(daytme, "%H:%M"),
    status = sub("\\[.*\\] ", "", x),
    guard_id = if_else(grepl("\\d", status), sub("\\D*(\\d+) .*", "\\1", status), NA_character_),
  ) %>%
  arrange(daytme) %>%
  mutate(x = cumsum(!is.na(guard_id))) %>%
  group_by(x) %>%
  mutate(
    guard_id = first(guard_id),
    day = if_else(tme > "23:00", day + 1, day),
    tme = if_else(tme > "23:00", "00:00", tme),
  ) %>%
  ungroup()

data_complete <- data04 %>% 
  tidyr::expand(
    tidyr::nesting(day, guard_id),
    tme = paste0("00:", stringr::str_pad(0:59, 2, "left", 0)), 
  ) %>%
  left_join(data04, by = c("day", "guard_id", "tme")) %>%
  arrange(day, tme) %>%
  group_by(day, guard_id) %>%
  mutate(x = cumsum(!is.na(status))) %>%
  group_by(x) %>%
  mutate(
    status = first(status),
    asleep = if_else(grepl("sleep", status), 1, 0),
  ) 

#part1-------------         
data_complete %>%
  group_by(guard_id) %>%
  summarise(asleep = sum(asleep), .groups = "drop") %>%
  top_n(1, asleep) %>%
  ungroup() %>%
  semi_join(data_complete, ., by = "guard_id") %
  group_by(guard_id, tme) %>% 
  summarise(asleep = sum(asleep), .groups = "drop") %>%
  top_n(1, asleep) %>% 
  transmute(res = as.integer(guard_id) * as.integer(sub("00:", "", tme)))


#part2---------
data_complete %>%
  group_by(guard_id, tme) %>%
  summarise(x = sum(asleep), .groups = "drop") %>%
  top_n(1, x) %>%
  transmute(res = as.integer(guard_id) * as.integer(sub("00:", "", tme)))
