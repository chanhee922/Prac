# reference
https://www.kaggleusercontent.com/kf/28233006/eyJhbGciOiJkaXIiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0..ZbDEXP7yXmYB5tizgWtfOg.QT0M0Ar12LstGRqwqg-3HJDef1mvtnIx5XP0DpqTOy2NbmqpevPZ3ezHmOquzkBv-gXhCve8sAt9WHd5izFyKLEYIj-Njt0pSBFFI2OUiAUF50m5r7M_FVMEVzGN1whAXl781d7ivnSXP8ZRYwOLPRBqGQgQbg-ZifZAY2AOLE2FbdEDFSJsrIH2t4sIwj2NTCRUnc8Kc5_r71lDeoJK71nLn1DkandxLdHbqzrYePK6LkfYpT3Yh5oS9n2PGOSHETq8SjXL1lXJJp3_pcp90VhX91-QW8k8RADr9Uoo8x034DSROUVflA_y74UW6mOt0sX_B_EJAfPpqZpRCqpAPbcY5OGTvPWffYnJ4M-KbBxmaVBNN06ayGRpOfKLT8fy8h01bEOKTLli9lWg1GR93A1kEbzrHzBp8FoZVnTvnxs4CEGxi_9UKUQYQ832ryHuWmN9wBSrPjtSoadsqJlQv_8v9oTDWWQFpPmRIoGMGD9EPXNbpAOyb73c-37mhDuZrWEXMTRRKMRiBWDKhFIEGhwSdtzT3ntI48juZ17tE72ile4KuWi2bLVIp-vJrnZ2rSINluYH5Qd4CflcYvdd6dScmdoUNTp_iylRIdobVM7njs0KGhq35VcTM8Ifmsele41cucCsNHpPmdGwg4mdsKkgJWT2itihNV5lELObsI1Mjmjsoh4qekewuAsTedE1.25RR_ymksLNkooYg7iewfQ/__results__.html#


############################
#   About Datasets
############################
#
# air_visit_data.csv: 
# historical visit data for the air restaurants. 
# This is essentially the main training data set.
# 
# air_reserve.csv / hpg_reserve.csv: 
# reservations made through the air / hpg systems.
# 
# air_store_info.csv / hpg_store_info.csv: 
# details about the air / hpg restaurants including genre and location.
# 
# store_id_relation.csv: 
# connects the air and hpg ids
# 
# date_info.csv: 
# essentially flags the Japanese holidays.
# 
# sample_submission.csv: 
# serves as the test set. 
# The id is formed by combining the air id with the visit date.

############################
# load libraries
############################


# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# specific visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('ggExtra') # visualisation
library('ggforce') # visualisation
library('viridis') # visualisation

# specific data manipulation
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # string manipulation

# Date plus forecast
library('lubridate') # date and time
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('prophet') # time series analysis
library('timetk') # time series analysis

# Maps / geospatial
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps

############################
# load data
############################
air_visits <- fread('../air_visit_data.csv')
air_reserve <- fread('../air_reserve.csv')
hpg_reserve <- fread('../hpg_reserve.csv')
air_store <- fread('../air_store_info.csv')
hpg_store <- fread('../hpg_store_info.csv')
holidays <- fread('../date_info.csv')
store_ids <- fread('../store_id_relation.csv')
test <- fread('../sample_submission.csv')

############################
# Overview
############################

### 1. Air Visits
head(air_visits)
summary(air_visits)
str(air_visits)
air_visits %>% distinct(air_store_id) %>% nrow()
    # 829
### 2. Air Reserve
head(air_reserve)
summary(air_reserve)
str(air_reserve)
air_reserve %>% distinct(air_store_id) %>% nrow()
    # 314
### 3. HPG Reserve
head(hpg_reserve)
summary(hpg_reserve)
str(hpg_reserve)
hpg_reserve %>% distinct(hpg_store_id) %>% nrow()
    # 13325
### 4. Air Store
head(air_store)
summary(air_store)
str(air_store)
### 5. HPG Store
head(hpg_store)
summary(hpg_store)
str(hpg_store)

### 6. Holidays
head(holidays)
summary(holidays)
str(holidays)
### 7. Store IDs
head(store_ids)
summary(store_ids)
str(store_ids)

### 8. Test data
head(test)
summary(test)
str(test)
### 9. Missing values
sum(is.na(air_visits))
# ...
sum(is.na(test))

### 10. reformating features
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
# Individual Feature Visualization
############################













