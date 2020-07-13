############################
# Individual Feature Visualization
############################
pacman::p_load('ggplot2', 
               'dplyr', 
               'lubridate', 
               'grid',
               'leaflet',
               'stringr',
               'tidyr')



### reformating features ###
air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))

air_reserve <- air_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

hpg_reserve <- hpg_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

air_store <- air_store %>%
  mutate(air_genre_name = as.factor(air_genre_name),
         air_area_name = as.factor(air_area_name))

hpg_store <- hpg_store %>%
  mutate(hpg_genre_name = as.factor(hpg_genre_name),
         hpg_area_name = as.factor(hpg_area_name))

holidays <- holidays %>%
  mutate(holiday_flg = as.logical(holiday_flg),
         date = ymd(calendar_date),
         calendar_date = as.character(calendar_date))

############################
############################

  ### 1. Air Visits
    # 날짜별 총 방문객
p1 <- air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(col = "blue") +
  labs(x = "All visitors", y = "Date")

    # 방문객 수 분포 
p2 <- air_visits %>%
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = "orange") +
  geom_histogram(fill = "blue", bins = 30) +
  scale_x_log10() # 로그 변환을 통해 시각성 확보 

    # 요일별 방문객 중위수 
p3 <- air_visits %>%
  mutate(wday = wday(visit_date, label = TRUE, week_start = 1)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Median visitors") +
  scale_fill_hue()

    # 월별 방문객 중위수
p4 <- air_visits %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Median visitors")

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

    # Time-series 상에서 흥미로운 장기 흐름 구조가 있는 것 같다. 
    # 이건 신규 레스토랑이 데이터 베이스에 추가된 것과 연관이 있어 보인다. 
    # 게다가, 주별 흐름에 연동된 기간 패턴을 볼 수 있었다.
    # 
    # 2번째 차트는 매장별 일 방문 고객수가 20명 근처(오랜지색 선)인 곳이 
    # 가장 많은 것으로 보인다. 
    # 드문 경우이지만 일 방문 고객 수가 100명이 넘는 곳도 간혹 있다.
    # 
    # 예상대로 금요일과 주말이 가장 인기있는 날로 보인다. 
    # 월.화는 방문 고객수의 중위값이 가장 낮다.
    # 
    # 연 기준 차트를 보면 특정 변동이 보인다. 
    # 12월이 가장 인기있는 달로 보이고, 3 ~ 5월은 꾸준하게 바빠보인다.

air_visits %>%
  filter(visit_date > ymd("2016-04-15") & visit_date < ymd("2016-06-15")) %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) + 
  geom_line() + 
  geom_smooth(method = "loess", color = "blue", span = 1/7) +
  labs(y = "All visitors", x = "Date")
    # 검은 선은 날짜이며, 파란 선은 회색 신뢰 영역에 해당하는 smoothing fit이다. 
    # 여기서 2016년 4월 29일 ~ 5월 5일까지의 골든 위크의 영향을 다시 볼 수 있다.

############################
############################

  ### 2. Air Reservation
    # 예약 / 실제 방문 고객수 비교
      # foo에 날짜, 시간, 요일 및 예약/실제 방문 날짜/시간을 
      # 나눠서 저장
foo <- air_reserve %>% 
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = lubridate::wday(reserve_datetime, label = TRUE),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = lubridate::wday(visit_datetime, label = TRUE),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = 'hour'),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = 'day')
  )
      # 날짜별 총 예약 인원수 
p1 <- foo %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() + 
  labs(x = "'air' visit date")

      # 방문 시간대별 예약 인원 수
p2 <- foo %>% 
  group_by(visit_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = 'blue') 

      # 예약/방문 시간 차이별 방문객 수 
p3 <- foo %>% 
  filter(diff_hour < 24*5) %>% # 5일 이내의 차이
  group_by(diff_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(diff_hour, all_visitors)) + 
  geom_col(fill = 'blue') +
  labs(x = "Time from reservation to visit [hours]")

layout <- matrix(c(1,1,2,3),2,2,byrow = TRUE)
multiplot(p1, p2, p3, layout = layout)

# 1. 2016년의 예약은 많이 저조해보이고, 
    # 심지어 긴시간 없는 구간도 보인다. 
    # 16년 연말 동안만 예약이 늘었다. 
    # 2017년의 고객수는 강하게 머물러 있고, 
    # 1분기 이후의 인위적인 감소는 training time이 끝나는 시기의 
    # 이런 예약과 연관되어 보인다. 
    # 이건 장기 예약이 data set에 미포함된걸 의미한다.

# 2. 일반적으로 저녁 식사를 하기 위해 직전에 예약이 이루어 진다.

# 3. 예약 후 방문까지의 시간은 24시간 주기를 보인다. 
    # 방문 몇 시간 전 예약하는 경우가 가장 많지만, 
    # 예약이 불가능하면 가능한 다른 날을 예약하는걸로 보인다. 
    # 이 차트는 더 긴 시간의 패턴을 보이기 위해 끝이 잘린다. 
    # 예약 후 방문까지의 시간이 긴 것은 예외적인게 아니다. 
    # 가장 긴 시간은 1년이 넘는 시간이 있다

foo %>% 
  arrange(desc(diff_day)) %>% 
  select(reserve_datetime, visit_datetime, diff_day, air_store_id) %>% 
  head(5)

# 가장 긴 시간을 정렬해보면 top5에 단지 2식당만이 있다. 
# 이건 정말로 핫플레이스거나 아니면 데이터 입력의 오류일 것이다.

############################
############################

  ### 3. HPG Reservation

foo <- hpg_reserve %>% 
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = 'hour'),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = 'day')
  )

      # 방문날짜 / 방문객
p1 <- foo %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() +
  labs(x = "'hpg' visit date")

      # 방문 시간 / 방문객 
p2 <- foo %>% 
  group_by(visit_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = 'red') 

      # 예약에서 실방문까지의 시간
p3 <- foo %>% 
  filter(diff_hour < 24*5) %>% 
  group_by(diff_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = 'red') +
  labs(x = "Time from reservation to visit [hours]")

layout <- matrix(c(1,1,2,3),2,2,byrow = TRUE)
multiplot(p1, p2, p3, layout=layout)

# 1. 예약 방문 고객은 2016년 12월에 많아지는 패턴을 보이며, 
  # 위의 ‘air’ data와 같이 time frame의 끄트머리는 수치가 
  # 떨어지는걸 볼 수 있다.

# 2. 방문 시간은 위와 동일하게 저녁대가 가장 많지만,
  # 예약 후 방문까지의 시간은 또 다른 24시간 패턴을 볼 수 있다. 
  # 몇 시간 전의 예약이 하루나 이틀전의 예약보다 
  # 특출나게 많지가 않다. 이건 ‘air’ data와는 
  # 완연한 대조를 보여준다.

############################
############################

  ### 4. Air Store
    # 식당 위치를 지도위에 나타내기
leaflet(air_store) %>% 
  addTiles() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addMarkers(~longitude, ~latitude, popup= ~air_store_id, label = ~air_genre_name,
             clusterOptions = markerClusterOptions())

    # 식당 타입 분포 + 최빈 레스토랑 위치 
p1 <- air_store %>% 
  group_by(air_genre_name) %>% 
  count() %>% 
  ggplot(aes(reorder(air_genre_name, n, FUN = min), n, fill = air_genre_name)) +
  geom_col() +
  coord_flip() + 
  theme(legend.position = 'none') +
  labs(x = "Type of cuisine (air_genre_name)", y = "Number of air restaurants")

p2 <- air_store %>% 
  group_by(air_area_name) %>% 
  count() %>% 
  ungroup() %>% 
  top_n(15, n) %>% 
  ggplot(aes(reorder(air_area_name, n, FUN = min), n, fill = air_area_name)) +
  geom_col() +
  theme(legend.position = 'none') +
  coord_flip() +
  labs(x = "Top 15 areas (air_area_name)", y = "Number of air restaurants")

layout <- matrix(c(1,2),2,1, byrow = TRUE)
multiplot(p1, p2, layout=layout)

############################
############################

  ### 5. HPG Store
    # 식당 위치를 지도위에 나타내기
leaflet(hpg_store) %>% 
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(~longitude, ~latitude, popup = ~hpg_store_id, label = ~hpg_genre_name,
             clusterOptions = markerClusterOptions())

p1 <- hpg_store %>%
  group_by(hpg_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(hpg_genre_name, n, FUN = min), n, fill = hpg_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine (hpg_genre_name)", y = "Number of hpg restaurants")

p2 <- hpg_store %>%
  mutate(area = str_sub(hpg_area_name, 1, 20)) %>% # 1 ~ 20번째 문자만 선택 
  group_by(area) %>%
  count() %>%
  ungroup() %>%
  top_n(15, n) %>%
  ggplot(aes(reorder(area, n, FUN = min) ,n, fill = area)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 15 areas (hpg_area_name)", y = "Number of hpg restaurants")

layout <- matrix(c(1,2),1,2,byrow= TRUE)
multiplot(p1, p2, layout = layout)

############################
############################

  ### 6. Holidays
foo <- holidays %>% 
  mutate(wday = wday(date)) # 일요일을 1, 월요일을 2.. 같은 식으로 바꿈

    # 공휴일 여부
p1 <- foo %>% 
  ggplot(aes(holiday_flg, fill = holiday_flg)) +
  geom_bar() +
  theme(legend.position = 'none')

    # 2016년 공휴일 여부 비교 
p2 <- foo %>% 
  filter(date > ymd('2016-04-15') & date < ymd('2016-06-01')) %>% 
  ggplot(aes(date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = 'none') +
  labs(x = '2016 date')

    # 2017년 공휴일 여부 비교
p3 <- foo %>% 
  filter(date > ymd('2017-04-15') & date < ymd('2017-06-01')) %>% 
  ggplot(aes(date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = 'none') +
  labs(x = '2017 date')

layout <- matrix(c(1,1,2,3),2,2,byrow=FALSE)
multiplot(p1, p2, p3, layout = layout)

holidays %>% 
  group_by(holiday_flg) %>% 
  count() %>% 
  spread(holiday_flg,n) %>% 
  mutate(frac = `TRUE`/(`TRUE`+`FALSE`))



foo <- air_visits %>% 
  rename(date = visit_date) %>% 
  distinct(visit_date) %>% 
  mutate(dset = 'train')
