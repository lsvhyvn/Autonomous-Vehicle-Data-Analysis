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


### 로봇 주행 궤적 ###

# 빈 지도에 테스트배드(아파트) 위치만 표시
leaflet(df) %>%
  addTiles() %>%
  addGraticule() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addMarkers(lng = c(126.74882049079, 126.74925890779, 126.75018891625, 126.75193100899, 126.75218850106, 126.75210267037,
                     126.75183176458, 126.75009410779, 126.74909725079, 126.74949019879, 126.74992001179, 126.751150524,
                     126.7517539832, 126.75179689854, 126.75099635579, 126.75041065279, 126.7496404025), 
             lat = c(37.69694458842, 37.69666670472, 37.69666467341, 37.69688221273, 37.69698408383, 37.69737034384, 37.69751890696,
                     37.69771781011, 37.69735805762, 37.69808131165, 37.69798856351, 37.6981093051, 37.69834022958, 37.6986437142,
                     37.6986827303, 37.698713971, 37.69860975794),
             icon = aptIcon,
             label = c("401동","402동","403동","404동","405동","406동","407동","408동","409동","410동",
                       "411동","412동","413동","414동","415동","416동","417동"),
             labelOptions = labelOptions(noHide = T, direction = 'top', textsize = "13px", textOnly = T))


# 데이터 전처리: vehLat, vehLng 0인 데이터 결측치로 처리, 분석에서 제외
df$vehLat <- ifelse(df$vehLat==0, NA, df$vehLat)
df$vehLng <- ifelse(df$vehLng==0, NA, df$vehLng)

df_ll_nomiss <- na.omit(df)

# 지도상에 표시되는 위치가 실제 위치와 차이가 있어, 계산한 OFFSET값 적용
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_ll_nomiss)){
  vehLat2[i] = df_ll_nomiss$vehLat[i] + 0.00008
  vehLng2[i] = df_ll_nomiss$vehLng[i] - 0.00005
}

df_ll <-  mutate(df_ll_nomiss, as.data.frame(vehLat2), as.data.frame(vehLng2))


# 지도에 로봇 주행궤적 그리기
df_ll$operationMode <- factor(df_ll$operationMode,
                              levels = c(1,2,3,4),
                              labels = c("Manual","Following", "Autonomous", "Setting"))

fpal <- colorFactor("Set1", df_ll$operationMode, n=4)
aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)

leaflet(df_ll) %>%
  addTiles() %>%
  addGraticule() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 0.2,
             popup = ~operationMode, radius = 0.2, color = ~fpal(operationMode)) %>%
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




## 모드별로 각각 주행 궤적 나타내기 ##
df_manual <- df
df_following <- df
df_auto <- df
df_setting <- df
str(df_manual)

# operation모드에 따라 배터리 NULL값 처리
df_manual$batteryStatus <- ifelse(df_manual$operationMode == 1, df_manual$batteryStatus, NA)
df_following$batteryStatus <- ifelse(df_following$operationMode == 2, df_following$batteryStatus, NA)
df_auto$batteryStatus <- ifelse(df_auto$operationMode == 3, df_auto$batteryStatus, NA)
df_setting$batteryStatus <- ifelse(df_setting$operationMode == 4, df_setting$batteryStatus, NA)

# operation모드에 따라 위경도 NULL값 처리
df_manual$vehLat <- ifelse(df_manual$operationMode != 1 | df_manual$vehLat == 0, NA, df_manual$vehLat)
df_following$vehLat <- ifelse(df_following$operationMode != 2 | df_following$vehLat == 0, NA, df_following$vehLat)
df_auto$vehLat <- ifelse(df_auto$operationMode != 3 | df_auto$vehLat == 0, NA, df_auto$vehLat)
df_setting$vehLat <- ifelse(df_setting$operationMode != 4 | df_setting$vehLat == 0, NA, df_setting$vehLat)

df_manual$vehLng <- ifelse(df_manual$operationMode != 1 | df_manual$vehLng == 0, NA, df_manual$vehLng)
df_following$vehLng <- ifelse(df_following$operationMode != 2 | df_following$vehLng == 0, NA, df_following$vehLng)
df_auto$vehLng <- ifelse(df_auto$operationMode != 3 | df_auto$vehLng == 0, NA, df_auto$vehLng)
df_setting$vehLng <- ifelse(df_setting$operationMode != 4 | df_setting$vehLng == 0, NA, df_setting$vehLng)



# 지도에 그리기 - Following(추종모드)

df_following_ll <- na.omit(df_following)
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_following_ll)){
  vehLat2[i] = df_following_ll$vehLat[i] + 0.00008
  vehLng2[i] = df_following_ll$vehLng[i] - 0.00005
}

df_following_ll$operationMode <- factor(df_following_ll$operationMode,
                                        levels = c(1,2,3,4),
                                        labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)

leaflet(df_following_ll) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 0.2,
             popup = ~operationMode, radius = 0.2, color =~fpal(operationMode)) %>%
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


# 지도에 그리기 - Autonomous(자율주행모드)

df_auto_ll <- na.omit(df_auto)
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_auto_ll)){
  vehLat2[i] = df_auto_ll$vehLat[i] + 0.00008
  vehLng2[i] = df_auto_ll$vehLng[i] - 0.00005
}

df_auto_ll$operationMode <- factor(df_auto_ll$operationMode,
                                   levels = c(1,2,3,4),
                                   labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)

leaflet(df_auto_ll) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 0.2,
             popup = ~operationMode, radius = 0.2, color =~fpal(operationMode)) %>%
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



# 지도에 그리기 - Manual(매뉴얼모드)

df_manual_ll <- na.omit(df_manual)
vehLat2 = c()
vehLng2 = c()
for(i in 1:nrow(df_manual_ll)){
  vehLat2[i] = df_manual_ll$vehLat[i] + 0.00008
  vehLng2[i] = df_manual_ll$vehLng[i] - 0.00005
}

df_manual_ll$operationMode <- factor(df_manual_ll$operationMode,
                                     levels = c(1,2,3,4),
                                     labels = c("Manual","Following", "Autonomous", "Setting"))


aptIcon <- icons("https://www.flaticon.com/svg/vstatic/svg/2400/2400342.svg?token=exp=1610943640~hmac=01a0ce8ab0243bbbaeed9fde0af3a2d1",
                 iconWidth = 35, iconHeight = 70)

leaflet(df_manual_ll) %>%
  addTiles() %>%
  setView(lng = 126.7502, lat = 37.69705, zoom = 21) %>%
  addCircles(lng = ~vehLng2, lat = ~vehLat2, weight = 0.2,
             popup = ~operationMode, radius = 0.2, color =~fpal(operationMode)) %>%
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
