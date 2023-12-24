gateway <- "W220_894FAC"

gateway_df <- read.csv(paste0("data/gateway/", gateway, ".csv"))
gateway_df$date <- as.POSIXct(gateway_df$date, "%Y-%m-%d %H:%M:%S", tz="Asia/Seoul")
str(gateway_df)
colSums(is.na(gateway_df))

door_df <- read.csv(paste0("data/door/", gateway, "_door.csv"))
motion_df <- read.csv(paste0("data/motion/", gateway, "_motion.csv"))
plug_df <- read.csv(paste0("data/plug/", gateway, "_plug.csv"))

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

library(dplyr)
# left join으로 df 결합
df <- left_join(gateway_df, door_df, by=c("date", "mac_address"))
df <- left_join(df, motion_df, by=c("date", "mac_address"))
df <- left_join(df, plug_df, by=c("date", "mac_address"))

library(lubridate)
df <- df %>%
  mutate(date = date + hours(9))

# # 디바이스 등록 전 날짜의 값들은 결측치로 처리리
# df <- df %>%
#   mutate(door1 = if_else(date < as.Date("2023-10-10"), NA_real_, door1))
# df <- df %>%
#   mutate(door2 = if_else(date < as.Date("2023-10-10"), NA_real_, door2))
# df <- df %>%
#   mutate(door3 = if_else(date < as.Date("2023-08-23"), NA_real_, door3))
# 
# df <- df %>%
#   mutate(motion1 = if_else(date < as.Date("2023-10-06"), NA_real_, motion1))
# df <- df %>%
#   mutate(motion2 = if_else((date >= as.Date("2023-10-29") & date <= as.Date("2023-10-31")) | date >= as.Date("2023-11-21"), motion2, NA_real_))
# 
# df <- df %>%
#   mutate(plug = if_else(date < as.Date("2023-10-10"), NA_real_, plug))

df$date <- format(df$date, "%Y-%m-%d %H:%M:%S")
write.csv(df, file=paste0("data/merge/", gateway, ".csv"), row.names=FALSE)
