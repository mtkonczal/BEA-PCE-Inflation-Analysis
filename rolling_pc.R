library(fredr)
fredr_set_key(fredKey)

##### PUTTING THIS HERE FOR NOW:
# Load the long one dude
pce_headline <-  c("PCE excluding food and energy","PCE goods excluding food and energy","PCE services excluding energy")

data <- pce %>% filter(LineDescription %in% pce_headline) %>%
  group_by(LineDescription) %>%
  mutate(YoY = DataValue/lag(DataValue,12)-1) %>%
  filter(month(date) == 10) %>%
  ungroup() %>%
  filter(!is.na(YoY)) %>%
  select(date, LineDescription, YoY) %>%
  pivot_wider(names_from = LineDescription, values_from = YoY) %>%
  clean_names()


current_date <- as.Date(Sys.time())
unrate <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1900-01-01"),
  observation_end = as.Date(current_date)
) %>%
  select(date, unrate = value) %>%
  mutate(unrate = unrate/100)

nrou <- fredr(
  series_id = "NROU",
  observation_start = as.Date("1900-01-01"),
  observation_end = as.Date(current_date)
) %>%
  select(date, nrou = value) %>%
  mutate(nrou = nrou/100)

df <- data %>%
  left_join(unrate, by="date") %>%
  left_join(nrou, by="date")

df

