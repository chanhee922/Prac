############################
# Feature relations
############################

pacman::p_load('dplyr',
               'ggridges',
               'ggplot2',
               'grid')

############################
############################
  ### 1. Visitors per genre

foo <- air_visits %>% 
  left_join(air_store, by = 'air_store_id')

foo %>% 
  group_by(visit_date, air_genre_name) %>% 
  summarise(mean_visitors = mean(visitors)) %>% 
  ungroup() %>% # group_by로 인한 오류 방지용
  ggplot(aes(visit_date, mean_visitors, color = air_genre_name)) +
  geom_line() +
  labs(y = 'air의 평균 방문객 수', x = '날짜') +
  theme(legend.position = 'none') +
  scale_y_log10() +
  facet_wrap(~air_genre_name)

      ### 결과:
          # 일별 평균 방문객은 10~100명 사이에 분포한다.
        # 각 카테고리내에서 장기 추세는 안정적으로 보인다. 
        # ‘Creative Cuisine’, ‘Okonomiyaki’ 외에도 2016년 후반부터 
        # ‘Asian’ 식당의 인기가 감소하는 추세이다.
          # ‘Karaoke’, ‘Asina’과 같은 적은 수의 time series는 다른 타입보다 
        # 변동성이 심해 보인다. ’Asian’ restaurants은 그들의 희소성에도 불구하고 
        # 매우 인기가 있어 보인다.

    ### 장르별, 요일별 평균 방문객
        # ‘Ridgeline plot’은 ’semi-overlapping’(밀도) 곡선을 빠르게 비교할 수 있다. 
        # 장르별 일 방문자의 분포를 나태는 차트를 그리고, y축은 하나만 나타내되 
        # 두 차트 모두에서 공유한다.

p1 <- foo %>% 
  mutate(wday = lubridate::wday(visit_date, label=T)) %>% 
  group_by(wday, air_genre_name) %>% 
  summarise(mean_visitors = mean(visitors)) %>% 
  ggplot(aes(air_genre_name, mean_visitors, color=wday)) +
  geom_point(size=4) +
  theme(legend.position = 'left', axis.text.y = element_blank(),
        plot.title = element_text(size=14)) +
  coord_flip() +
  labs(x = '') +
  scale_x_discrete(position = 'top') +
  ggtitle('air_genre_name') +
  scale_color_hue()

p2 <- foo %>% 
  ggplot(aes(visitors, air_genre_name, fill = air_genre_name)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = 'none') +
  labs(x = "") +
  scale_fill_cyclical(values = c('blue', 'red')) #장르, 요일별 차트와 햇갈려 색을 달리 했다.

layout <- matrix(c(1,1,2,2,2),1,5,byrow= TRUE)
multiplot(p1, p2, layout = layout)


    # 결과:
      #   주중과 주말의 가장 큰 차이는 ’Karaoke’에서 보인다. 
      # 차이는 크지 않지만 유사한 추세로 ’International’이 있다.

      # 주말이 주중보다 방문객 수가 많은 추세를 뒤집는 data는 없다. 
      # 주중.주말 갭이 가장 적은 곳은 ‘Other’, ’Japanese food’이고, 
      # ’Korean food’는 금요일이 평균 방문객이 가장 많다. 
      #’Bar/Cocktail’은 전반적으로 방문객이 적다.

      # 밀도 분포는 주간 분포에서 얻은 인상을 확인한다.: 
      # ‘Asian’ 레스토랑은 평균 방문객수가 10명 미만이 거의 없고, 
      # ’Karaoke’는 강한 주말의 영향으로 분포가 넓다.

############################
############################
  ### 2. The impact of holidays
      ## 휴일과 평일의 차이 확인

foo <- air_visits %>% 
  mutate(calendar_date = as.character(visit_date)) %>% 
  left_join(holidays, by = 'calendar_date')

p1 <- foo %>% 
  ggplot(aes(holiday_flg, visitors, color = holiday_flg)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position =  'none')

p2 <- foo %>% 
  mutate(wday = lubridate::wday(date, label=T)) %>% 
  group_by(wday, holiday_flg) %>% 
  summarise(mean_visitors = mean(visitors)) %>% 
  ggplot(aes(wday, mean_visitors, color = holiday_flg)) +
  geom_point(size = 4) +
  theme(legend.position = 'none') +
  labs(y = '평균방문객수')

layout <- matrix(c(1,2),1,2,byrow = TRUE)
multiplot(p1, p2, layout=layout)

      # 왼쪽 차트를 보면 휴일 유무에 따른 방문객 수는 큰 차이가 없어 보인다. 
      # 숨겨진 세부 정보가 있다.
      # 
      # 오른쪽 차트를 보면 휴일이 주말인 경우는 
      # 방문객 수에 큰 영향을 미치지 않고 감소한 경우도 있다. 
      # 하지만 휴일이 주중인 경우는 큰 영향을 미치는 걸로 보인다. 
      # 특히 월요일과 화요일은.

############################
############################
  ### 3. Restaurants per area and the effect on visitor numbers
    ## 휴일과 평일의 차이 확인


