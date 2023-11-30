df <- read.csv("data/merge/W220_D6FC80.csv")
str(df)
df$date <- as.POSIXct(df$date, "%Y-%m-%d %H:%M", tz="Asia/Seoul")

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

df <- interpolate_row(df)

# 시간대를 분류하는 함수 정의
get_time_of_day <- function(hour) {
  if (hour >= 6 & hour < 12) {
    return("Morning")
  } else if (hour >= 12 & hour < 18) {
    return("Afternoon")
  } else if (hour >= 18 & hour < 24) {
    return("Evening")
  } else {
    return("Night")
  }
}

# 파생 변수 추가
df <- df %>%
  mutate(
    weekday = wday(date, label = TRUE, abbr = TRUE), # 요일 (영어 전체 이름)
    is_weekend = wday(date) %in% c(1, 7), # 주말 여부 (토요일, 일요일)
    time_of_day = sapply(hour(date), get_time_of_day) # 시간대
  )

weekdays_eng <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
df$weekday <- weekdays_eng[wday(df$date)]

df$date <- format(df$date, "%Y-%m-%d %H:%M:%S")
write.csv(df, file="data/merge/W220_D6FC80.csv", row.names=FALSE)
