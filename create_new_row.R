df <- read.csv("data/merge/W220_D6FC80.csv")
str(df)
df$date <- as.POSIXct(df$date, "%Y-%m-%d %H:%M", tz="Asia/Seoul")

#library(imputeTS)


library(ggplot2)
ggplot() +
  geom_line(data=door_df, aes(x=date, y=door3), color="blue") +
  geom_line(data=door_df, aes(x=date, y=door2), color="red" ) +
  geom_line(data=door_df, aes(x=date, y=door1), color="green")


# 달이 넘어가는 부분에 대한 보간 -> create_new_row 함수로는 감지 못 함
library(dplyr)
interpolate_row <- function(df){
  # 날짜 범위 설정
  start_date <- min(df$date, na.rm = TRUE)
  end_date <- max(df$date, na.rm = TRUE)
  
  # 30분 단위로 날짜 생성
  all_dates <- seq(from = start_date, to = end_date, by = "30 min")
  
  # 모든 날짜를 포함하도록 데이터프레임 확장
  new_df <- data.frame(
    date = all_dates,
    mac_address = unique(df$mac_address),
    temperature = NA,
    humidity = NA,
    dew_point = NA,
    door1 = NA,
    door2 = NA,
    door3 = NA,
    motion1 = NA,
    motion2 = NA,
    plug = NA
  )
  new_df$date <- as.POSIXct(new_df$date, origin='1970-01-01') # date type 변환
  
  # 원래 데이터와 병합
  df <- merge(new_df, df, by = c("date", "mac_address"), all.x = TRUE)
  
  # coalesce를 사용해 컬럼 통합
  df <- df %>%
    mutate(
      temperature = coalesce(temperature.x, temperature.y),
      humidity = coalesce(humidity.x, humidity.y),
      dew_point = coalesce(dew_point.x, dew_point.y),
      door1 = coalesce(door1.x, door1.y),
      door2 = coalesce(door2.x, door2.y),
      door3 = coalesce(door3.x, door3.y),
      motion1 = coalesce(motion1.x, motion1.y),
      motion2 = coalesce(motion2.x, motion2.y),
      plug = coalesce(plug.x, plug.y),
      
    ) %>%
    select(-contains(".x"), -contains(".y")) # 불필요한 컬럼 제거
  
  return (df)
}

df_temp <- interpolate_row(df)
str(df_temp)
df_temp$date <- format(df_temp$date, "%Y-%m-%d %H:%M:%S")
write.csv(df_temp, file="data/merge/W220_D6FC80_nan.csv", row.names=FALSE)

na_percentages <- sapply(df_temp, function(x) sum(is.na(x)) / length(x)) * 100
na_df <- data.frame(variable = names(na_percentages), na_percentage = na_percentages)
ggplot(na_df, aes(x = variable, y = na_percentage)) + 
  geom_bar(stat = "identity") +
  labs(y = "결측치 비율 (%)", x = "변수") +
  theme_minimal() +
  coord_flip()


df_10 <- df_temp %>% filter(date >= as.POSIXct("2023-10-01"))

str(df_10)
colSums(is.na(df_10))
summary(df_10)

quantile(df_10$plug, na.rm = TRUE)
