df <- read.csv("data/merge/W220_D6FC80.csv")
df$date <- as.POSIXct(df$date, "%Y-%m-%d %H:%M", tz="Asia/Seoul")

library(dplyr)
library(lubridate)
df_10 <- df %>% filter(date >= as.POSIXct("2023-10-01")) %>%
  mutate(
    total_door = coalesce(door1, 0) + coalesce(door2, 0) + coalesce(door3, 0),
    total_motion = coalesce(motion1, 0) + coalesce(motion2, 0)
  ) %>%
  select(-time_of_day)

df_3days <- df_10 %>%
  mutate(hour_group = floor_date(date, "3 days")) %>%
  group_by(mac_address, hour_group) %>%
  summarise(
    temperature = mean(temperature, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    dew_point = mean(dew_point, na.rm = TRUE),
    door1 = sum(door1, na.rm = TRUE),
    door2 = sum(door2, na.rm = TRUE),
    door3 = sum(door3, na.rm = TRUE),
    total_door = sum(total_door, na.rm=TRUE),
    motion1 = sum(motion1, na.rm = TRUE),
    motion2 = sum(motion2, na.rm = TRUE),
    total_motion = sum(total_motion, na.rm=TRUE),
    plug = sum(plug, na.rm = TRUE)
  ) %>%
  ungroup()

df_grouped <- df_10 %>%
  mutate(time_slot = case_when(
    hour(date) >= 0 & hour(date) < 2 ~ "0~2",
    hour(date) >= 2 & hour(date) < 4 ~ "2~4",
    hour(date) >= 4 & hour(date) < 6 ~ "4~6",
    hour(date) >= 6 & hour(date) < 8 ~ "6~8",
    hour(date) >= 8 & hour(date) < 10 ~ "8~10",
    hour(date) >= 10 & hour(date) < 12 ~ "10~12",
    hour(date) >= 12 & hour(date) < 14 ~ "12~14",
    hour(date) >= 14 & hour(date) < 16 ~ "14~16",
    hour(date) >= 16 & hour(date) < 18 ~ "16~18",
    hour(date) >= 18 & hour(date) < 20 ~ "18~20",
    hour(date) >= 20 & hour(date) < 22 ~ "20~22",
    hour(date) >= 22 & hour(date) < 24 ~ "22~24"
  )) %>%
  group_by(time_slot) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE),
    avg_humidity = mean(humidity, na.rm = TRUE),
    total_door = sum(door1 + door2 + door3, na.rm = TRUE),
    total_motion = sum(motion1 + motion2, na.rm = TRUE),
    total_plug = sum(plug, na.rm = TRUE)
  )

# 시각화
library(ggplot2)

ggplot(df_10, aes(x=date)) +
  geom_line(aes(y = total_door), color="blue") +
  geom_line(aes(y = total_motion), color="red")

ggplot(df_grouped, aes(x = time_slot)) +
  geom_bar(aes(y = total_door), stat = "identity", fill = "blue") +
  geom_bar(aes(y = total_motion), stat = "identity", fill = "red") +
  labs(title = "Time Slot-wise Activity", x = "Time Slot", y = "Total Activity")
