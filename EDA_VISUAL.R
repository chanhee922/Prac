############################
# Individual Feature Visualization
############################
  ### 1. Air Visits
    # 날짜별 총 방문객
p1 <- air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
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
    # 2번째 차트는 매장별 일 방문 고객수가 20명 근처(오랜지색 선)인 곳이 가장 많은 것으로 보인다. 
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

  ### 2. Air Reservation

