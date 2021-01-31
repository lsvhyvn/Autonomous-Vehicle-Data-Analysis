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


### 배터리 변화량 분석 ###

## 원본 데이터 (전처리x)
ggplot(df, aes(x = time, y = batteryStatus)) + 
  geom_line() +
  labs(x = "time (s)", y = "batteryStatus (%)")
ggsave(file="C:/Users/adm/Desktop/이수현/배터리 원본.png")


## 전처리 1 : batteryStatus < 80인 데이터 이상치로 판단하고 전처리
table(is.na(df$batteryStatus))
for(i in 2:nrow(df)){
  if(df$batteryStatus[i] < 80)
    df$batteryStatus[i] = df$batteryStatus[i-1]
}
View(df)

ggplot(df, aes(x = time, y = batteryStatus)) + geom_line() +
  labs(x = "time (s)", y = "batteryStatus (%)") + lims(y = c(85,100))
ggsave(file="C:/Users/adm/Desktop/이수현/배터리 전처리1.png")


## 전처리 2 : outlier 제거(위아래로 왔다갔다하며 튀는 값 정제)
b_itv = c()

for(j in 1:5){
  for(i in 2:nrow(df)){
    b_itv[i] = df$batteryStatus[i-1] - df$batteryStatus[i]
    if(b_itv[i] > 0.4 | b_itv[i] < -0.4){
      df$batteryStatus[i] = (df$batteryStatus[i-1] + df$batteryStatus[i+1])/2
      b_itv[i] = df$batteryStatus[i-1] - df$batteryStatus[i]
    }
  }
}
b_itv[1] = 0

df$b_itv <- b_itv
View(df)

ggplot(df, aes(x =  time)) + geom_line(aes(y = batteryStatus), na.rm = T) +
  labs(x = "time (s)", y = "batteryStatus (%)") +
  lims(y = c(85,100))
ggsave(file="C:/Users/adm/Desktop/이수현/배터리 전처리2.png")


## 전처리 3: 모드별 배터리 잔량 확인
df_manual <- df
df_following <- df
df_auto <- df
df_setting <- df

# operation모드에 따라 배터리 NULL값 처리
df_manual$batteryStatus <- ifelse(df_manual$operationMode == 1, df_manual$batteryStatus, NA)
df_following$batteryStatus <- ifelse(df_following$operationMode == 2, df_following$batteryStatus, NA)
df_auto$batteryStatus <- ifelse(df_auto$operationMode == 3, df_auto$batteryStatus, NA)
df_setting$batteryStatus <- ifelse(df_setting$operationMode == 4, df_setting$batteryStatus, NA)

# 모드별 battery Status만 합친 df 만들기
mb <- df_manual$batteryStatus
fb <- df_following$batteryStatus
ab <- df_auto$batteryStatus
sb <- df_setting$batteryStatus
time <- df_manual$time

df_bstatus <- data.frame(mb, fb, ab, sb, time)
View(df_bstatus)

# 그래프
gbattery <- ggplot(df_bstatus, aes(x = time)) + 
  geom_line(aes(y = mb, colour = "Manual", group = 1), na.rm = TRUE) +
  geom_line(aes(y = fb, colour = "Following", group = 2), na.rm = TRUE) +
  geom_line(aes(y = ab, colour = "Autonomous", group = 3), na.rm = TRUE) +
  geom_line(aes(y = sb, colour = "Setting", group = 4), na.rm = TRUE) +
  scale_y_continuous(limits = c(80, 100)) +
  scale_colour_manual(values = c("#228B22", "#4682B4", "darkred" , "#4B0082"))+
  labs(x = "time (s)", y = "batteryStatus (%)", color = "Parameter") +
  lims(y = c(85,100)) +
  theme(legend.position = 'bottom')

gbattery
ggsave(file="C:/Users/adm/Desktop/이수현/배터리 잔량(전처리3-모드).png")




############################################################################################


# 시간당 배터리 변화량 분석 위해 각 데이터에 대해 time interval, batteryStatus interval 계산
time_itv = c()
for(i in 2:nrow(df_bstatus)){
  time_itv[i] = df_bstatus$time[i] - df_bstatus$time[i-1]
}
time_itv[1] = 0
df_bstatus$time_itv <- time_itv

mb_itv <- ifelse(df$operationMode == 1, df$b_itv, NA)
fb_itv <- ifelse(df$operationMode == 2, df$b_itv, NA)
ab_itv <- ifelse(df$operationMode == 3, df$b_itv, NA)

df_bstatus$mb_itv <- mb_itv
df_bstatus$fb_itv <- fb_itv
df_bstatus$ab_itv <- ab_itv

View(df_bstatus)


## Manual 모드
bstatus_m <- df_bstatus %>% filter(!is.na(mb))

tmt = sum(bstatus_m$time_itv)
tmb = sum(bstatus_m$mb_itv, na.rm = T)
tmb/tmt

gbattery_m <- ggplot(df_bstatus, aes(x = time)) + 
  geom_line(aes(y = mb, colour = "Manual", group = 1), na.rm = TRUE) +
  scale_y_continuous(limits = c(80, 100)) +
  scale_colour_manual(values = c("darkred", "#228B22", "#4682B4", "#4B0082"))+
  labs(x = "time (s)", y = "batteryStatus (%)", color = "Parameter") +
  lims(y = c(85,100)) +
  theme(legend.position = 'bottom')

gbattery_m
ggsave(file="C:/Users/adm/Desktop/이수현/배터리 잔량(Manual).png")



#Following 모드
bstatus_f <- df_bstatus %>% filter(!is.na(fb))

tft = sum(bstatus_f$time_itv)
tfb = sum(bstatus_f$fb_itv, na.rm = T)
tfb
tfb/tft



gbattery_f <- ggplot(df_bstatus, aes(x = time)) + 
  geom_line(aes(y = fb, colour = "Following", group = 1), na.rm = TRUE) +
  scale_y_continuous(limits = c(80, 100)) +
  scale_colour_manual(values = c("#4682B4", "darkred", "#228B22", "#4B0082"))+
  labs(x = "time (s)", y = "batteryStatus (%)", color = "Parameter") +
  lims(y = c(85,100)) +
  theme(legend.position = 'bottom')

gbattery_f
ggsave(file="C:/Users/adm/Desktop/이수현/배터리 잔량(Following).png")



#Autonomous 모드
bstatus_a <- df_bstatus %>% filter(!is.na(ab))

tat = sum(bstatus_a$time_itv)
tab = sum(bstatus_a$ab_itv, na.rm = T)
tab/tat


gbattery_a <- ggplot(df_bstatus, aes(x = time)) + 
  geom_line(aes(y = ab, colour = "Autonomous", group = 1), na.rm = TRUE) +
  scale_y_continuous(limits = c(80, 100)) +
  scale_colour_manual(values = c("#228B22", "#4682B4", "darkred", "#4B0082"))+
  labs(x = "time (s)", y = "batteryStatus (%)", color = "Parameter") +
  lims(y = c(85,100)) +
  theme(legend.position = 'bottom')

gbattery_a
ggsave(file="C:/Users/adm/Desktop/이수현/배터리 잔량(Autonomous).png")



#Setting (테스트 준비중일 때)
bstatus_s <- df_bstatus %>% filter(!is.na(sb))

titv_s = c()
for(i in 1:nrow(bstatus_s)){
  if(i != 1){
    titv_s[i] = bstatus_s$time[i] - bstatus_s$time[i-1]
  }
}
bstatus_s$titv_s <- titv_s

tst = sum(bstatus_s$titv_s, na.rm = T)
tsb = sum(bstatus_s$sb_itv, na.rm = T)
tsb/tst

