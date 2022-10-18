# MAKES RDUW DATA FRAME 

library(tidyverse)
library(dplyr)

warnings <- read_csv('warnings.csv')
rdu <- read_csv('weather_station_data_20220915/rdu_station_data.csv')

# get year, month, day from date 
rdu %>%
  mutate(date = lubridate::mdy(date),
         year = factor(lubridate::year(date)),
         month = factor(lubridate::month(date)), 
         day = factor(lubridate::day(date))) %>%
  mutate(round_time = hms::as_hms(
    format(
      lubridate::floor_date(
        as.POSIXct(time), unit='hour'), format = "%H:%M:%S")))-> rdu

# add extreme heat condition var 
# where it's negative switch issued and expired 
warnings %>%
  filter(ph_name %in% c('Excessive Heat', 'Heat' )) %>%
  select(issued, expired, name) %>%
  mutate(issued = lubridate::floor_date(issued, unit = "hour"),
         expired = lubridate::ceiling_date(expired, unit = "hour"),
         diff = as.integer(expired - issued))  -> warnings_c

warnings_c %>%
  filter(diff < 0 ) %>%
  rename(expired = issued, issued = expired) %>%
  mutate(diff = as.integer(expired - issued)) -> warnings_neg

# join 
warnings_fix <- rbind(warnings_c[warnings_c$diff >= 0,], warnings_neg)

# get day month year hour 
warnings_dates <- data.frame()

for (i in 1:nrow(warnings_fix)){
  x <- warnings_fix[i,]
  num <- x$diff
  num1 <- num + 1
  date_i <- rep(x$issued + 3600 * 0:num)
  warnings_i <- rep(x$name, num1)
  
  df_i <- data.frame(date_i, warnings_i)
  names(df_i) <- c('date', 'name')
  
  warnings_dates <- rbind(warnings_dates, df_i)
}


# make is heat bool var
warnings_dates %>% 
  mutate(is_heat = TRUE, 
         time = hms::as_hms(format(as.POSIXct(date), format = "%H:%M:%S")), 
         date = lubridate::date(date)) -> warnings_dates

# join warnings and rdu data 
left_join(rdu, warnings_dates, by=c('date', 'round_time'='time')) -> rduw


rduw %>%
  mutate(is_heat = ifelse(is.na(is_heat), FALSE, is_heat),
         is_heat_day = ifelse(date %in% unique(rduw$date[rduw$is_heat == TRUE]), TRUE, FALSE)) -> rduw

readr::write_csv(rduw, 'rdu_warnings.csv')





