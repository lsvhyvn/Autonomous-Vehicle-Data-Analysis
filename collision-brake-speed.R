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



### collision - brake - speed 상관관계 분석 & 시각화 ###

# collision, braking 데이터 결측치 있는지 확인
table(is.na(df$collision))
table(is.na(df$braking))

fs <- (df_following$speed)/10
as <- (df_auto$speed)/10
time <- df$time
opM <- df$operationMode
col <- df$collision
brake <-df$braking

df_cbs <- data.frame(fs, as, opM, col, brake, time)
View(df_cbs)


## 전체구간에서의 collision - brake - speed 시각화 => 향후 구간별 분석에 활용할 데이터 추출용

# collision, brake 데이터 following, autonomous 모드에서만 의미있으므로 따로 추출
df_cbs$col <- ifelse(df_cbs$opM == 2 | df_cbs$opM == 3, df_cbs$col, NA)
df_cbs$brake <- ifelse(df_cbs$opM == 2 | df_cbs$opM == 3, df_cbs$brake, NA)

gcb_fa <- ggplot(df_cbs, aes(x = time)) + 
  geom_line(aes(y = fs,color = "speed(following)"), na.rm = T, alpha = 2) +
  geom_line(aes(y = as,color = "speed(autonomous)"), na.rm = T, alpha = 2) +
  geom_point(aes(y = brake*7, color = "brake")) +
  geom_point(aes(y = col*7.1, color = "collision")) +
  scale_y_continuous(sec.axis = sec_axis(~./ 7, breaks = c(0,1), name = "collision, brake"))+
  scale_colour_manual(values = c( "blue", "red", "#228B22", "#4682B4"))+
  labs(x = "time (s)", y = "speed (km/h)", colour = "Parameter") +
  theme(legend.position = 'bottom')

gcb_fa
ggsave(file="C:/Users/adm/Desktop/이수현/전체구간(모드).png")


## 구간1 (11시 43분 ~ 11시 44분, 자율주행 모드)
df_1 <- df %>% filter(df$HH == 11 & ((df$MM.1 > 42 & df$MM.1 < 44) | (df$MM.1 == 44 & df$ss.sss < 9.2)))
df_1_a <- df_1 %>% filter(df_1$operationMode == 3)

# collision -> brake 시간 구하기
# cchg: collision 변화시 1, 그대로면 0 표시 / bchg: braking 변화시 1, 그대로면 0 표시
cchg <- c()
bchg <- c()
for(i in 2:nrow(df_1_a)){
  if(df_1_a$collision[i] == 1){
    if(df_1_a$collision[i-1] == 1){
      cchg[i] = 0
    } else {
      cchg[i] = 1
    }
  }
  else{
    if(df_1_a$collision[i-1] == 1){
      cchg[i] = 1
    } else {
      cchg[i] = 0
    }
  }
}
for(i in 2:nrow(df_1_a)){
  if(df_1_a$braking[i] == 1){
    if(df_1_a$braking[i-1] == 1){
      bchg[i] = 0
    } else {
      bchg[i] = 1
    }
  }
  else{
    if(df_1_a$braking[i-1] == 1){
      bchg[i] = 1
    } else {
      bchg[i] = 0
    }
  }
}

df_1_a$cchg <- cchg
df_1_a$bchg <- bchg
View(df_1_a)

for(i in 2:nrow(df_1_a)){
  if(df_1_a$cchg[i] == 1){
    t1 = df_1_a$time[i]
    print(i)
  }
}
for(i in 2:nrow(df_1_a)){
  if(df_1_a$bchg[i] == 1){
    t2 = df_1_a$time[i]
    print(i)
  }
}

t2 - t1
df_1_a$time[559] - df_1_a$time[553]


#brake-speed 관계 구하기
speed_itv = c()
for(i in 2:nrow(df_1_a)){
  speed_itv[i] = df_1_a$speed[i] - df_1_a$speed[i-1]
}
speed_itv[1] = 0
df_1_a$speed_itv <- speed_itv

time_itv = c()
for(i in 2:nrow(df_1_a)){
  time_itv[i] = df_1_a$time[i] - df_1_a$time[i-1]
}
time_itv[1] = 0
df_1_a$time_itv <- time_itv

speed_sum1 = 0
time_sum1 = 0
for(i in 1:nrow(df_1_a)){
  if(df_1_a$braking[i] == 1) {
    speed_sum1 = speed_sum1 + df_1_a$speed_itv[i]
    time_sum1 = time_sum1 + df_1_a$time_itv[i]
  }
}
View(df_1_a)

speed_sum1
time_sum1
speed_sum1/time_sum1


#그래프그리기
sf <- max((df_1_a$speed)/10)/max(df_1_a$collision)

gcb_1_a <- ggplot(df_1_a, aes(x = time)) + 
  geom_line(aes(y = speed/10,,colour = "speed", group = 1)) +
  geom_line(aes(y = braking*(max(speed)/30), color = "braking", group = 1), alpha = 2) +
  geom_line(aes(y = collision*(max(speed)/30), color = "collision", group = 1), alpha = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./(sf/3), breaks = c(0,1), name = "collision, braking"))+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(x = "time (s)", y = "speed (km/h)", colour = "Parameter") +
  theme(legend.position = 'bottom')
gcb_1_a
ggsave(file="C:/Users/adm/Desktop/이수현/구간1.png")


#지도에 구간 나타내기
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_1_a)){
  vehLat2[i] = df_1_a$vehLat[i] + 0.00008
  vehLng2[i] = df_1_a$vehLng[i] - 0.00005
}


df_1_a$operationMode <- factor(df_1_a$operationMode,
                               levels = c(1,2,3,4),
                               labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/88/88945.svg?token=exp=1611643164~hmac=29fe91fb529702e990f3851fb14f90ea",
                 iconWidth = 35, iconHeight = 70)


leaflet(df_1_a) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 1,
             popup = ~operationMode, radius = 1, color =~fpal(operationMode)) %>%
  addMarkers(lng = c(126.74882049079, 126.74925890779, 126.75018891625, 126.75193100899, 126.75218850106, 126.75210267037,
                     126.75183176458, 126.75009410779, 126.74909725079, 126.74949019879, 126.74992001179, 126.751150524,
                     126.7517539832, 126.75179689854, 126.75099635579, 126.75041065279, 126.7496404025), 
             lat = c(37.69694458842, 37.69666670472, 37.69666467341, 37.69688221273, 37.69698408383, 37.69737034384, 37.69751890696,
                     37.69771781011, 37.69735805762, 37.69808131165, 37.69798856351, 37.6981093051, 37.69834022958, 37.6986437142,
                     37.6986827303, 37.698713971, 37.69860975794),
             icon = aptIcon,
             label = c("401동","402동","403동","404동","405동","406동","407동","408동","409동","410동",
                       "411동","412동","413동","414동","415동","416동","417동"),
             labelOptions = labelOptions(noHide = T, direction = 'top', textsize = "13px", textOnly = T)) %>%
  addLegend("bottomright", pal = fpal, values = ~operationMode)




## 구간2 (11시 49분 35초 ~ 11시 54분, 자율주행 모드)
df_2 <- df %>% filter(df$HH == 11 & ((df$MM.1 > 49 & df$MM.1 < 54)|(df$MM.1 == 49 & df$ss.sss > 35)))
df_2_a <- df_2 %>% filter(df_2$operationMode == 3)

str(df_2_a)

#collision 1되었다0 되기까지 시간구하기
cchg <- c()
bchg <- c()
for(i in 2:nrow(df_2_a)){
  if(df_2_a$collision[i] == 1){
    if(df_2_a$collision[i-1] == 1){
      cchg[i] = 0
    } else {
      cchg[i] = 1
    }
  }
  else{
    if(df_2_a$collision[i-1] == 1){
      cchg[i] = 1
    } else {
      cchg[i] = 0
    }
  }
}
for(i in 2:nrow(df_2_a)){
  if(df_2_a$braking[i] == 1){
    if(df_2_a$braking[i-1] == 1){
      bchg[i] = 0
    } else {
      bchg[i] = 1
    }
  }
  else{
    if(df_2_a$braking[i-1] == 1){
      bchg[i] = 1
    } else {
      bchg[i] = 0
    }
  }
}

df_2_a$cchg <- cchg
df_2_a$bchg <- bchg
View(df_2_a)

for(i in 2:nrow(df_2_a)){
  if(df_2_a$cchg[i] == 1){
    t1 = df_2_a$time[i]
    print(i)
  }
}
for(i in 2:nrow(df_2_a)){
  if(df_2_a$bchg[i] == 1){
    print(i)
    t2 = df_2_a$time[i]
    
  }
}

t1
df_2_a$time[2383] - df_2_a$time[2369]
df_2_a$time[2423] - t1

# 그래프 그리기
sf2 <- max((df_2_a$speed)/10)/max(df_2_a$collision)

gcb_2_a <- ggplot(df_2_a, aes(x = time)) + 
  geom_line(aes(y = speed/10,,colour = "speed", group = 1)) +
  geom_line(aes(y = braking*(max(speed)/30), color = "braking", group = 1), alpha = 2) +
  geom_line(aes(y = collision*(max(speed)/30), color = "collision", group = 1), alpha = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./(sf2/3), breaks = c(0,1), name = "collision, braking"))+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(x = "time (s)", y = "speed (km/h)", colour = "Parameter") +
  theme(legend.position = 'bottom')
gcb_2_a
ggsave(file="C:/Users/adm/Desktop/이수현/구간2.png")

#지도에 구간 나타내기
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_2_a)){
  vehLat2[i] = df_2_a$vehLat[i] + 0.00008
  vehLng2[i] = df_2_a$vehLng[i] - 0.00005
}


df_2_a$operationMode <- factor(df_2_a$operationMode,
                               levels = c(1,2,3,4),
                               labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/88/88945.svg?token=exp=1611643164~hmac=29fe91fb529702e990f3851fb14f90ea",
                 iconWidth = 35, iconHeight = 70)


m2<- leaflet(df_2_a) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 1,
             popup = ~operationMode, radius = 1, color =~fpal(operationMode)) %>%
  addMarkers(lng = c(126.74882049079, 126.74925890779, 126.75018891625, 126.75193100899, 126.75218850106, 126.75210267037,
                     126.75183176458, 126.75009410779, 126.74909725079, 126.74949019879, 126.74992001179, 126.751150524,
                     126.7517539832, 126.75179689854, 126.75099635579, 126.75041065279, 126.7496404025), 
             lat = c(37.69694458842, 37.69666670472, 37.69666467341, 37.69688221273, 37.69698408383, 37.69737034384, 37.69751890696,
                     37.69771781011, 37.69735805762, 37.69808131165, 37.69798856351, 37.6981093051, 37.69834022958, 37.6986437142,
                     37.6986827303, 37.698713971, 37.69860975794),
             icon = aptIcon,
             label = c("401동","402동","403동","404동","405동","406동","407동","408동","409동","410동",
                       "411동","412동","413동","414동","415동","416동","417동"),
             labelOptions = labelOptions(noHide = T, direction = 'top', textsize = "13px", textOnly = T)) %>%
  addLegend("bottomright", pal = fpal, values = ~operationMode)
m2




## 구간3 (12시 7분 20.5초 ~ 12시 9분 59.3초, 자율주행 모드)
df_3 <- df %>% filter(df$HH == 12 & (df$MM.1 == 8|(df$MM.1 == 7 & df$ss.sss > 20.5)|(df$MM.1 == 9 & df$ss.sss < 59.3)))
df_3_a <- df_3 %>% filter(df_3$operationMode == 3)


cchg <- c()
bchg <- c()
for(i in 2:nrow(df_3_a)){
  if(df_3_a$collision[i] == 1){
    if(df_3_a$collision[i-1] == 1){
      cchg[i] = 0
    } else {
      cchg[i] = 1
    }
  }
  else{
    if(df_3_a$collision[i-1] == 1){
      cchg[i] = 1
    } else {
      cchg[i] = 0
    }
  }
}
for(i in 2:nrow(df_3_a)){
  if(df_3_a$braking[i] == 1){
    if(df_3_a$braking[i-1] == 1){
      bchg[i] = 0
    } else {
      bchg[i] = 1
    }
  }
  else{
    if(df_3_a$braking[i-1] == 1){
      bchg[i] = 1
    } else {
      bchg[i] = 0
    }
  }
}

df_3_a$cchg <- cchg
df_3_a$bchg <- bchg
View(df_3_a)

for(i in 2:nrow(df_3_a)){
  if(df_3_a$cchg[i] == 1){
    print(i)
  }
}
for(i in 2:nrow(df_3_a)){
  if(df_3_a$bchg[i] == 1){
    print(i)
    t2 = df_3_a$time[i]
    
  }
}
df_3_a$time[243] - df_3_a$time[236]
df_3_a$time[849] - df_3_a$time[842]
df_3_a$time[263] - df_3_a$time[262]
df_3_a$time[865] - df_3_a$time[862]
df_3_a$time[262] - df_3_a$time[236]
df_3_a$time[862] - df_3_a$time[842]

df_3_a$time[263] - df_3_a$time[243]


#brake-speed 관계 구하기
speed_itv3 = c()
for(i in 2:nrow(df_3_a)){
  speed_itv3[i] = df_3_a$speed[i] - df_3_a$speed[i-1]
}
speed_itv3[1] = 0
df_3_a$speed_itv3 <- speed_itv3

time_itv3 = c()
for(i in 2:nrow(df_3_a)){
  time_itv3[i] = df_3_a$time[i] - df_3_a$time[i-1]
}
time_itv3[1] = 0
df_3_a$time_itv3 <- time_itv3

speed_sum3 = 0
time_sum3 = 0
for(i in 400:nrow(df_3_a)){
  if(df_3_a$braking[i] == 1) {
    speed_sum3 = speed_sum3 + df_3_a$speed_itv3[i]
    time_sum3 = time_sum3 + df_3_a$time_itv3[i]
  }
}
View(df_3_a)

speed_sum3
time_sum3
speed_sum3/time_sum3

# collision - brake - speed 그래프 그리기
sf3 <- max((df_3_a$speed)/10)/max(df_3_a$collision)

gcb_3_a <- ggplot(df_3_a, aes(x = time)) + 
  geom_line(aes(y = speed/10,,colour = "speed", group = 1)) +
  geom_line(aes(y = braking*(max(speed)/30), color = "braking", group = 1), alpha = 2) +
  geom_line(aes(y = collision*(max(speed)/30), color = "collision", group = 1), alpha = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./(sf3/3), breaks = c(0,1), name = "collision, braking"))+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(x = "time (s)", y = "speed (km/h)", colour = "Parameter") +
  theme(legend.position = 'bottom')

gcb_3_a
ggsave(file="C:/Users/adm/Desktop/이수현/구간3.png")

#지도에 구간 나타내기
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_3_a)){
  vehLat2[i] = df_3_a$vehLat[i] + 0.00008
  vehLng2[i] = df_3_a$vehLng[i] - 0.00005
}


df_3_a$operationMode <- factor(df_3_a$operationMode,
                               levels = c(1,2,3,4),
                               labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)


leaflet(df_3_a) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 1,
             popup = ~operationMode, radius = 1, color =~fpal(operationMode)) %>%
  addMarkers(lng = c(126.74882049079, 126.74925890779, 126.75018891625, 126.75193100899, 126.75218850106, 126.75210267037,
                     126.75183176458, 126.75009410779, 126.74909725079, 126.74949019879, 126.74992001179, 126.751150524,
                     126.7517539832, 126.75179689854, 126.75099635579, 126.75041065279, 126.7496404025), 
             lat = c(37.69694458842, 37.69666670472, 37.69666467341, 37.69688221273, 37.69698408383, 37.69737034384, 37.69751890696,
                     37.69771781011, 37.69735805762, 37.69808131165, 37.69798856351, 37.6981093051, 37.69834022958, 37.6986437142,
                     37.6986827303, 37.698713971, 37.69860975794),
             icon = aptIcon,
             label = c("401동","402동","403동","404동","405동","406동","407동","408동","409동","410동",
                       "411동","412동","413동","414동","415동","416동","417동"),
             labelOptions = labelOptions(noHide = T, direction = 'top', textsize = "13px", textOnly = T)) %>%
  addLegend("bottomright", pal = fpal, values = ~operationMode)




## 구간4 (13시 53분 54.6초 ~ 13시 54분 5.5초, 자율주행 모드)
df_4 <- df %>% filter(df$HH == 13 & ((df$MM.1 == 53 & df$ss.sss > 54.6)|(df$MM.1 == 54 & df$ss.sss < 5.5)))
df_4_a <- df_4 %>% filter(df_4$operationMode == 3)


cchg <- c()
bchg <- c()
for(i in 2:nrow(df_4_a)){
  if(df_4_a$collision[i] == 1){
    if(df_4_a$collision[i-1] == 1){
      cchg[i] = 0
    } else {
      cchg[i] = 1
    }
  }
  else{
    if(df_4_a$collision[i-1] == 1){
      cchg[i] = 1
    } else {
      cchg[i] = 0
    }
  }
}
for(i in 2:nrow(df_4_a)){
  if(df_4_a$braking[i] == 1){
    if(df_4_a$braking[i-1] == 1){
      bchg[i] = 0
    } else {
      bchg[i] = 1
    }
  }
  else{
    if(df_4_a$braking[i-1] == 1){
      bchg[i] = 1
    } else {
      bchg[i] = 0
    }
  }
}

df_4_a$cchg <- cchg
df_4_a$bchg <- bchg
View(df_4_a)

for(i in 2:nrow(df_4_a)){
  if(df_4_a$cchg[i] == 1){
    print(i)
  }
}
for(i in 2:nrow(df_4_a)){
  if(df_4_a$bchg[i] == 1){
    print(i)
    t2 = df_4_a$time[i]
    
  }
}
df_4_a$time[66] - df_4_a$time[59]
df_4_a$speed[90]


#brake-speed 관계 구하기
speed_itv4 = c()
for(i in 2:nrow(df_4_a)){
  speed_itv4[i] = df_4_a$speed[i] - df_4_a$speed[i-1]
}
speed_itv4[1] = 0
df_4_a$speed_itv4 <- speed_itv4

time_itv = c()
for(i in 2:nrow(df_4_a)){
  time_itv[i] = df_4_a$time[i] - df_4_a$time[i-1]
}
time_itv[1] = 0
df_4_a$time_itv <- time_itv

speed_sum4 = 0
time_sum4 = 0
for(i in 60:108){
  if(df_4_a$braking[i] == 1) {
    speed_sum4 = speed_sum4 + df_4_a$speed_itv4[i]
    time_sum4 = time_sum4 + df_4_a$time_itv[i]
  }
}
View(df_4_a)

speed_sum4
time_sum4
speed_sum4/time_sum4


# collision - braking - speed 그래프 그리기
sf4 <- max((df_4_a$speed)/10)/max(df_4_a$collision)

gcb_4_a <- ggplot(df_4_a, aes(x = time)) + 
  geom_line(aes(y = speed/10,,colour = "speed", group = 1)) +
  geom_line(aes(y = braking*(max(speed)/30), color = "braking", group = 1), alpha = 2) +
  geom_line(aes(y = collision*(max(speed)/30), color = "collision", group = 1), alpha = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./(sf4/3), breaks = c(0,1), name = "collision, braking"))+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(x = "time (s)", y = "speed (km/h)", colour = "Parameter") +
  theme(legend.position = 'bottom')

gcb_4_a
ggsave(file="C:/Users/adm/Desktop/이수현/구간4.png")

#지도에 구간 나타내기
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_4_a)){
  vehLat2[i] = df_4_a$vehLat[i] + 0.00008
  vehLng2[i] = df_4_a$vehLng[i] - 0.00005
}


df_4_a$operationMode <- factor(df_4_a$operationMode,
                               levels = c(1,2,3,4),
                               labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)


leaflet(df_4_a) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 1,
             popup = ~operationMode, radius = 1, color =~fpal(operationMode)) %>%
  addMarkers(lng = c(126.74882049079, 126.74925890779, 126.75018891625, 126.75193100899, 126.75218850106, 126.75210267037,
                     126.75183176458, 126.75009410779, 126.74909725079, 126.74949019879, 126.74992001179, 126.751150524,
                     126.7517539832, 126.75179689854, 126.75099635579, 126.75041065279, 126.7496404025), 
             lat = c(37.69694458842, 37.69666670472, 37.69666467341, 37.69688221273, 37.69698408383, 37.69737034384, 37.69751890696,
                     37.69771781011, 37.69735805762, 37.69808131165, 37.69798856351, 37.6981093051, 37.69834022958, 37.6986437142,
                     37.6986827303, 37.698713971, 37.69860975794),
             icon = aptIcon,
             label = c("401동","402동","403동","404동","405동","406동","407동","408동","409동","410동",
                       "411동","412동","413동","414동","415동","416동","417동"),
             labelOptions = labelOptions(noHide = T, direction = 'top', textsize = "13px", textOnly = T)) %>%
  addLegend("bottomright", pal = fpal, values = ~operationMode)




## 구간5 (14시 15분 47초 ~ 14시 16분 40.2초, 추종 모드)
df_5 <- df %>% filter(df$HH == 14 & ((df$MM.1 == 15 & df$ss.sss > 47)|(df$MM.1 == 16 & df$ss.sss < 40.2)))
df_5_f <- df_5 %>% filter(df_5$operationMode == 2)


cchg <- c()
bchg <- c()
for(i in 2:nrow(df_5_f)){
  if(df_5_f$collision[i] == 1){
    if(df_5_f$collision[i-1] == 1){
      cchg[i] = 0
    } else {
      cchg[i] = 1
    }
  }
  else{
    if(df_5_f$collision[i-1] == 1){
      cchg[i] = 1
    } else {
      cchg[i] = 0
    }
  }
}
for(i in 2:nrow(df_5_f)){
  if(df_5_f$braking[i] == 1){
    if(df_5_f$braking[i-1] == 1){
      bchg[i] = 0
    } else {
      bchg[i] = 1
    }
  }
  else{
    if(df_5_f$braking[i-1] == 1){
      bchg[i] = 1
    } else {
      bchg[i] = 0
    }
  }
}

df_5_f$cchg <- cchg
df_5_f$bchg <- bchg
View(df_5_f)

for(i in 2:nrow(df_5_f)){
  if(df_5_f$cchg[i] == 1){
    print(i)
  }
}
for(i in 2:nrow(df_5_f)){
  if(df_5_f$bchg[i] == 1){
    print(i)
    t2 = df_5_f$time[i]
    
  }
}
df_5_f$time[469] - df_5_f$time[464]


# collision - braking - speed 그래프 그리기
sf5 <- max((df_5_f$speed)/10)/max(df_5_f$collision)

gcb_5_f <- ggplot(df_5_f, aes(x = time)) + 
  geom_line(aes(y = speed/10,,colour = "speed", group = 1)) +
  geom_line(aes(y = braking*(max(speed)/30), color = "braking", group = 1), alpha = 2) +
  geom_line(aes(y = collision*(max(speed)/30), color = "collision", group = 1), alpha = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./(sf5/3), breaks = c(0,1), name = "collision, braking"))+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(x = "time (s)", y = "speed (km/h)", colour = "Parameter") +
  theme(legend.position = 'bottom')

gcb_5_f
ggsave(file="C:/Users/adm/Desktop/이수현/구간5.png")

#지도에 구간 나타내기
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_5_f)){
  vehLat2[i] = df_5_f$vehLat[i] + 0.00008
  vehLng2[i] = df_5_f$vehLng[i] - 0.00005
}


df_5_f$operationMode <- factor(df_5_f$operationMode,
                               levels = c(1,2,3,4),
                               labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)


leaflet(df_5_f) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 1,
             popup = ~operationMode, radius = 1, color =~fpal(operationMode)) %>%
  addMarkers(lng = c(126.74882049079, 126.74925890779, 126.75018891625, 126.75193100899, 126.75218850106, 126.75210267037,
                     126.75183176458, 126.75009410779, 126.74909725079, 126.74949019879, 126.74992001179, 126.751150524,
                     126.7517539832, 126.75179689854, 126.75099635579, 126.75041065279, 126.7496404025), 
             lat = c(37.69694458842, 37.69666670472, 37.69666467341, 37.69688221273, 37.69698408383, 37.69737034384, 37.69751890696,
                     37.69771781011, 37.69735805762, 37.69808131165, 37.69798856351, 37.6981093051, 37.69834022958, 37.6986437142,
                     37.6986827303, 37.698713971, 37.69860975794),
             icon = aptIcon,
             label = c("401동","402동","403동","404동","405동","406동","407동","408동","409동","410동",
                       "411동","412동","413동","414동","415동","416동","417동"),
             labelOptions = labelOptions(noHide = T, direction = 'top', textsize = "13px", textOnly = T)) %>%
  addLegend("bottomright", pal = fpal, values = ~operationMode)




## 구간6 (14시 24분 19.2초 ~ 14시 25분 24초, 자율주행 모드)
df_6 <- df %>% filter(df$HH == 14 & ((df$MM.1 == 24 & df$ss.sss > 19.2)|(df$MM.1 == 25 & df$ss.sss < 24)))
df_6_a <- df_6 %>% filter(df_6$operationMode == 3)


cchg <- c()
bchg <- c()
for(i in 2:nrow(df_6_a)){
  if(df_6_a$collision[i] == 1){
    if(df_6_a$collision[i-1] == 1){
      cchg[i] = 0
    } else {
      cchg[i] = 1
    }
  }
  else{
    if(df_6_a$collision[i-1] == 1){
      cchg[i] = 1
    } else {
      cchg[i] = 0
    }
  }
}
for(i in 2:nrow(df_6_a)){
  if(df_6_a$braking[i] == 1){
    if(df_6_a$braking[i-1] == 1){
      bchg[i] = 0
    } else {
      bchg[i] = 1
    }
  }
  else{
    if(df_6_a$braking[i-1] == 1){
      bchg[i] = 1
    } else {
      bchg[i] = 0
    }
  }
}

df_6_a$cchg <- cchg
df_6_a$bchg <- bchg
View(df_6_a)

for(i in 2:nrow(df_6_a)){
  if(df_6_a$cchg[i] == 1){
    print(i)
  }
}
for(i in 2:nrow(df_6_a)){
  if(df_6_a$bchg[i] == 1){
    print(i)
    t2 = df_6_a$time[i]
    
  }
}
df_6_a$time[297] - df_6_a$time[291]
df_6_a$time[335] - df_6_a$time[333]
df_6_a$time[441] - df_6_a$time[438]
df_6_a$time[333] - df_6_a$time[291]
df_6_a$time[438] - df_6_a$time[409]
df_6_a$time[335] - df_6_a$time[297]
df_6_a$time[441] - df_6_a$time[409]

#brake-speed 관계 구하기
speed_itv5 = c()
for(i in 2:nrow(df_6_a)){
  speed_itv5[i] = df_6_a$speed[i] - df_6_a$speed[i-1]
}
speed_itv5[1] = 0
df_6_a$speed_itv5 <- speed_itv5

time_itv5 = c()
for(i in 2:nrow(df_6_a)){
  time_itv5[i] = df_6_a$time[i] - df_6_a$time[i-1]
}
time_itv5[1] = 0
df_6_a$time_itv5 <- time_itv5

speed_sum5 = 0
time_sum5 = 0
for(i in 361:473){
  if(df_6_a$braking[i] == 1) {
    speed_sum5 = speed_sum5 + df_6_a$speed_itv5[i]
    time_sum5 = time_sum5 + df_6_a$time_itv5[i]
  }
}

View(df_6_a)

speed_sum5
time_sum5
speed_sum5/time_sum5

# collision - braking - speed 그래프 그리기
sf6 <- max((df_6_a$speed)/10)/max(df_6_a$collision)

gcb_6_a <- ggplot(df_6_a, aes(x = time)) + 
  geom_line(aes(y = speed/10,,colour = "speed", group = 1)) +
  geom_line(aes(y = braking*(max(speed)/30), color = "braking", group = 1), alpha = 2) +
  geom_line(aes(y = collision*(max(speed)/30), color = "collision", group = 1), alpha = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./(sf6/3), breaks = c(0,1), name = "collision, braking"))+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(x = "time (s)", y = "speed (km/h)", colour = "Parameter") +
  theme(legend.position = 'bottom')

gcb_6_a
ggsave(file="C:/Users/adm/Desktop/이수현/구간6.png")

#지도에 구간 나타내기
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_6_a)){
  vehLat2[i] = df_6_a$vehLat[i] + 0.00008
  vehLng2[i] = df_6_a$vehLng[i] - 0.00005
}


df_6_a$operationMode <- factor(df_6_a$operationMode,
                               levels = c(1,2,3,4),
                               labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)


leaflet(df_6_a) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 1,
             popup = ~operationMode, radius = 1, color =~fpal(operationMode)) %>%
  addMarkers(lng = c(126.74882049079, 126.74925890779, 126.75018891625, 126.75193100899, 126.75218850106, 126.75210267037,
                     126.75183176458, 126.75009410779, 126.74909725079, 126.74949019879, 126.74992001179, 126.751150524,
                     126.7517539832, 126.75179689854, 126.75099635579, 126.75041065279, 126.7496404025), 
             lat = c(37.69694458842, 37.69666670472, 37.69666467341, 37.69688221273, 37.69698408383, 37.69737034384, 37.69751890696,
                     37.69771781011, 37.69735805762, 37.69808131165, 37.69798856351, 37.6981093051, 37.69834022958, 37.6986437142,
                     37.6986827303, 37.698713971, 37.69860975794),
             icon = aptIcon,
             label = c("401동","402동","403동","404동","405동","406동","407동","408동","409동","410동",
                       "411동","412동","413동","414동","415동","416동","417동"),
             labelOptions = labelOptions(noHide = T, direction = 'top', textsize = "13px", textOnly = T)) %>%
  addLegend("bottomright", pal = fpal, values = ~operationMode)



## 구간7 (14시 32분 39.6초 ~ 14시 34분 10초, 추종 모드)
df_7 <- df %>% filter(df$HH == 14 & ((df$MM.1 == 33)|(df$MM.1 == 32 & df$ss.sss > 39.6)|(df$MM.1 == 34 & df$ss.sss < 10)))
df_7_f <- df_7 %>% filter(df_7$operationMode == 2)

# collision - braking - speed 그래프 그리기
gcb_7_f <- ggplot(df_7_f, aes(x = time)) + 
  geom_line(aes(y = speed/10,,colour = "speed", group = 1)) +
  geom_line(aes(y = braking, color = "braking", group = 1), alpha = 2) +
  geom_line(aes(y = collision, color = "collision", group = 1), alpha = 2) +
  scale_y_continuous(limits = c(0,3), breaks = c(0, 1, 2, 3), sec.axis = sec_axis(~./1, breaks = c(0,1), name = "collision, braking"))+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(x = "time (s)", y = "speed (km/h)", colour = "Parameter") +
  theme(legend.position = 'bottom')

gcb_7_f
ggsave(file="C:/Users/adm/Desktop/이수현/구간7.png")

#지도에 구간 나타내기
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_7_f)){
  vehLat2[i] = df_7_f$vehLat[i] + 0.00008
  vehLng2[i] = df_7_f$vehLng[i] - 0.00005
}


df_7_f$operationMode <- factor(df_7_f$operationMode,
                               levels = c(1,2,3,4),
                               labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)


leaflet(df_7_f) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 1,
             popup = ~operationMode, radius = 1, color =~fpal(operationMode)) %>%
  addMarkers(lng = c(126.74882049079, 126.74925890779, 126.75018891625, 126.75193100899, 126.75218850106, 126.75210267037,
                     126.75183176458, 126.75009410779, 126.74909725079, 126.74949019879, 126.74992001179, 126.751150524,
                     126.7517539832, 126.75179689854, 126.75099635579, 126.75041065279, 126.7496404025), 
             lat = c(37.69694458842, 37.69666670472, 37.69666467341, 37.69688221273, 37.69698408383, 37.69737034384, 37.69751890696,
                     37.69771781011, 37.69735805762, 37.69808131165, 37.69798856351, 37.6981093051, 37.69834022958, 37.6986437142,
                     37.6986827303, 37.698713971, 37.69860975794),
             icon = aptIcon,
             label = c("401동","402동","403동","404동","405동","406동","407동","408동","409동","410동",
                       "411동","412동","413동","414동","415동","416동","417동"),
             labelOptions = labelOptions(noHide = T, direction = 'top', textsize = "13px", textOnly = T)) %>%
  addLegend("bottomright", pal = fpal, values = ~operationMode)
