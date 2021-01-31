df = read.csv("C:/Users/adm/Desktop/이수현/201204_robot_log.csv")

library(dplyr)
library(plyr)
library(leaflet)
library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)
library(reshape2)

time1 = paste(as.character.Date(df$YYYY), 
              as.character.Date(df$MM), 
              as.character.Date(df$DD),
              as.character.Date(df$HH),
              as.character.Date(df$MM.1),
              as.character.Date(df$ss.sss))
df <- df %>% mutate(time = ymd_hms(time1)) 
str(df)

df_manual <- df
df_following <- df
df_auto <- df
df_setting <- df
str(df_manual)

# operation모드에 따라 speed NULL값 처리
df_manual$speed <- ifelse(df_manual$operationMode == 1, df_manual$speed, NA)
df_following$speed <- ifelse(df_following$operationMode == 2, df_following$speed, NA)
df_auto$speed <- ifelse(df_auto$operationMode == 3, df_auto$speed, NA)
df_setting$speed <- ifelse(df_setting$operationMode == 4, df_setting$speed, NA)



### speed 주행모드별로 ###

# speed 원본 데이터 시각화
ggplot(df, aes(x = time, y = speed/10)) +
  geom_point(size = 0.7) +
  labs(x = "time (s)", y = "speed (km/h)")
ggsave(file="C:/Users/adm/Desktop/이수현/speed(원본).png")

# 데이터 전처리1: 테스트 준비중(operationMode = 4)인 데이터 분석에서 제외
df$speed <- ifelse(df$operationMode == 4, NA, df$speed)
ggplot(df, aes(x = time, y = speed/10)) +
  geom_point(size = 0.7, na.rm = TRUE) +
  labs(x = "time (s)", y = "speed (km/h)")
ggsave(file="C:/Users/adm/Desktop/이수현/speed(전처리1).png")


## 모드별 배터리 분석

# 모드별 battery Status만 합친 df 만들기
ms <- (df_manual$speed)/10
fs <- (df_following$speed)/10
as <- (df_auto$speed)/10
time <- df_manual$time

df_speed <- data.frame(ms, fs, as, time)
View(df_speed)


# 통계
df_speed_m <- df_speed %>% filter(!is.na(ms))
mean(df_speed_m$ms)
max(df_speed_m$ms)
min(df_speed_m$ms)
df_speed_f <- df_speed %>% filter(!is.na(fs))
View(df_speed_f)
mean(df_speed_f$fs)
max(df_speed_f$fs)
min(df_speed_f$fs)
df_speed_a <- df_speed %>% filter(!is.na(as))
mean(df_speed_a$as)
max(df_speed_a$as)
min(df_speed_a$as)


# 그래프 
gspeed <- ggplot(df_speed, aes(x = time)) + 
  geom_point(aes(y = ms, colour = "Manual", group = 1), size = 0.6, na.rm = TRUE) +
  geom_point(aes(y = fs, colour = "Following", group = 2), size = 0.6, na.rm = TRUE) +
  geom_point(aes(y = as, colour = "Autonomous", group = 3), size = 0.6, na.rm = TRUE) +
  geom_line(aes(y = ms, colour = "Manual", group = 1), size = 0.5, na.rm = TRUE) +
  geom_line(aes(y = fs, colour = "Following", group = 2), size = 0.5, na.rm = TRUE) +
  geom_line(aes(y = as, colour = "Autonomous", group = 3), size = 0.5, na.rm = TRUE) +
  scale_colour_manual(values = c("#228B22", "#4682B4", "darkred"))+
  scale_y_continuous(breaks = c(-5, 0, 5, 10)) +
  labs(x = "time (s)", y = "speed (km/h)", color = "Parameter") +
  theme(legend.position = 'bottom')

gspeed
ggsave(file="C:/Users/adm/Desktop/이수현/speed(주행모드).png")


#Manual speed
gspeed_m <- ggplot(df_speed, aes(x = time)) + 
  geom_point(aes(y = ms, colour = "Manual", group = 1),size = 0.6, na.rm = TRUE) +
  geom_line(aes(y = ms, colour = "Manual", group = 1),size = 0.5, na.rm = TRUE) +
  scale_colour_manual(values = c("darkred"))+
  #  lims(y = c(-5,10)) +
  labs(x = "time (s)", y = "speed (km/h)", color = "Parameter") +
  theme(legend.position = 'bottom')
gspeed_m
ggsave(file="C:/Users/adm/Desktop/이수현/speed(Manual).png")

#Following speed
gspeed_f <- ggplot(df_speed, aes(x = time)) + 
  geom_point(aes(y = fs, colour = "Following", group = 2), size = 0.6, na.rm = TRUE) +
  geom_line(aes(y = fs, colour = "Following", group = 2), size = 0.5, na.rm = TRUE) +
  scale_colour_manual(values = c("#4682B4"))+
  scale_y_continuous(breaks = c(0, 2.5, 5)) +
  labs(x = "time (s)", y = "speed (km/h)", color = "Parameter") +
  theme(legend.position = 'bottom')
gspeed_f
ggsave(file="C:/Users/adm/Desktop/이수현/speed(Following).png")

# speed < 0 인 횟수 카운트
cnt = 0
for(i in 2:nrow(df_speed_f)){
  if(df_speed_f$fs[i] < 0 & df_speed_f$fs[i-1] > 0){
    cnt = cnt + 1
  }
}
cnt

#Autonomous speed
gspeed_a <- ggplot(df_speed, aes(x = time)) + 
  geom_point(aes(y = as, colour = "Autonomous", group = 3), size = 0.6, na.rm = TRUE) +
  geom_line(aes(y = as, colour = "Autonomous", group = 3), size = 0.5, na.rm = TRUE) +
  scale_colour_manual(values = c("#228B22"))+
  scale_y_continuous(breaks = c(0, 2.5, 5, 7.5)) +
  labs(x = "time (s)", y = "speed (km/h)", color = "Parameter") +
  theme(legend.position = 'bottom')
gspeed_a
ggsave(file="C:/Users/adm/Desktop/이수현/speed(Autonomous).png")

# speed < 0 인 횟수 카운트
cnt = 0
for(i in 2:nrow(df_speed_a)){
  if(df_speed_a$as[i] < 0 & df_speed_a$as[i-1] > 0){
    cnt = cnt + 1
  }
}
cnt


mode_chg = c()
for(i in 2:nrow(df)){
  if(df$operationMode[i] == df$operationMode[i-1]){
    mode_chg[i] = 0
  }  
  else{
    mode_chg[i] = 1
  }
}
df$mode_chg <- mode_chg
