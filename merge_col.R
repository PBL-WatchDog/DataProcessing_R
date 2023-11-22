gateway_df <- read.csv("data/gateway/W220_D6FC80.csv")
str(gateway_df)

gateway_df$date <- as.POSIXct(gateway_df$date, "%Y-%m-%d %H:%M:%S", tz="Asia/Seoul")
str(gateway_df)
colSums(is.na(gateway_df))

door_df <- read.csv("data/door/W220_D6FC80_door.csv")
motion_df <- read.csv("data/motion/W220_D6FC80_motion.csv")
plug_df <- read.csv("data/plug/W220_D6FC80_plug.csv")

door_df$date <- as.POSIXct(door_df$date, "%Y-%m-%d %H:%M:%S", tz="Asia/Seoul")
motion_df$date <- as.POSIXct(motion_df$date, "%Y-%m-%d %H:%M:%S", tz="Asia/Seoul")
plug_df$date <- as.POSIXct(plug_df$date, "%Y-%m-%d %H:%M:%S", tz="Asia/Seoul")
colSums(is.na(door_df))
colSums(is.na(motion_df))
colSums(is.na(plug_df))

nrow(gateway_df)
nrow(door_df)
nrow(motion_df)
nrow(motion_df)
nrow(plug_df)

# left join으로 df 결합합
library(dplyr)
df <- left_join(gateway_df, door_df, by=c("date", "mac_address"))
df <- left_join(df, motion_df, by=c("date", "mac_address"))
df <- left_join(df, plug_df, by=c("date", "mac_address"))

# 디바이스 등록 전 날짜의 값들은 결측치로 처리리
df <- df %>%
  mutate(door1 = if_else(date < as.Date("2023-10-10"), NA_real_, door1))
df <- df %>%
  mutate(door2 = if_else(date < as.Date("2023-10-10"), NA_real_, door2))
df <- df %>%
  mutate(door3 = if_else(date < as.Date("2023-08-23"), NA_real_, door3))

df <- df %>%
  mutate(motion1 = if_else(date < as.Date("2023-10-06"), NA_real_, motion1))
df <- df %>%
  mutate(motion2 = if_else(date < as.Date("2023-10-29") | date > as.Date("2023-11-10"), NA_real_, motion2))

df <- df %>%
  mutate(plug = if_else(date < as.Date("2023-10-10"), NA_real_, plug))

df$date <- format(df$date, "%Y-%m-%d %H:%M:%S")
write.csv(df, file="data/merge/W220_D6FC80.csv", row.names=FALSE)
