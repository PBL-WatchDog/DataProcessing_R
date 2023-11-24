df <- read.csv("data/merge/W220_D6FC80.csv")
str(df)
df$date <- as.POSIXct(df$date, "%Y-%m-%d %H:%M", tz="Asia/Seoul")

### 데이터 관측
na_percentages <- sapply(df, function(x) sum(is.na(x)) / length(x)) * 100
na_df <- data.frame(variable = names(na_percentages), na_percentage = na_percentages)
ggplot(na_df, aes(x = variable, y = na_percentage)) + 
  geom_bar(stat = "identity") +
  labs(y = "결측치 비율 (%)", x = "변수") +
  theme_minimal() +
  coord_flip()

df_10 <- df %>% filter(date >= as.POSIXct("2023-10-01"))

str(df_10)
colSums(is.na(df_10))
summary(df_10)
quantile(df_10$plug, na.rm = TRUE)

# 첨도, 왜도, 빈도수
library(psych)
kurtosi(df$plug)
skew(df$plug)

library(descr)
freq(df$door3, plot=T)

boxplot(df$plug)


### 스케일링
sensor_cols <- c("temperature", "humidity", "dew_point", "plug")

# 각 컬럼에 대해 z-score 표준화 수행
df[sensor_cols] <- lapply(df[sensor_cols], function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
})
summary(df)

df_10 <- df %>% filter(date >= as.POSIXct("2023-10-01"))

df_10$activity_index_morning = ifelse(df_10$time_of_day == "Morning", rowSums(df_10[, c("door1", "door2", "door3", "motion1", "motion2", "plug")], na.rm = TRUE), NA)
df_10$activity_index_evening = ifelse(df_10$time_of_day == "Evening", rowSums(df_10[, c("door1", "door2", "door3", "motion1", "motion2", "plug")], na.rm = TRUE), NA)
df_10$activity_index_afternoon = ifelse(df_10$time_of_day == "Afternoon", rowSums(df_10[, c("door1", "door2", "door3", "motion1", "motion2", "plug")], na.rm = TRUE), NA)
df_10$activity_index_Night = ifelse(df_10$time_of_day == "Night", rowSums(df_10[, c("door1", "door2", "door3", "motion1", "motion2", "plug")], na.rm = TRUE), NA)

# 정규화
df_10 <- df %>% filter(date >= as.POSIXct("2023-10-01"))

library(scales)
library(dplyr)
df_10 <- df_10 %>%
  mutate(
    temperature = rescale(temperature, to = c(0, 1)),
    humidity = rescale(humidity, to = c(0, 1)),
    dew_point = rescale(dew_point, to = c(0, 1)),
    plug = rescale(plug, to = c(0, 10))
  )

weights = c(door1 = 1, door2 = 1, door3 = 1, motion1 = 1, motion2 = 1, plug = 1)
df_10$activity_index = rowSums(sweep(df_10[, c("door1", "door2", "door3", "motion1", "motion2", "plug")], 2, weights, "*"), na.rm = TRUE)


library(ggplot2)
ggplot() +
  geom_line(data=df_10, aes(x=date, y=activity_index))
