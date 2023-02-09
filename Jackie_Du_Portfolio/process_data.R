library(dplyr)
library(stats)
library(stringr)
library(magrittr)

# read in weather data 
df <- read.table('era5land_1980_2022.txt', sep=',', header = TRUE)

# filter for years and months and lat / lon
df |>
  mutate(year = as.numeric(str_sub(datetime, 1, 4))) |>
  # subset for 2005 onward 
  filter(year >= 2005) |>
  mutate(month = as.numeric(str_sub(datetime, 6,7))) |>
  # subset for summer months
  filter(month %in% c(5,6,7,8,9)) |>
  # filter lat / long - 79, 78.8 ; 35.9, 36.1
  filter(lat %in% c(35.9, 36, 36.1), lon %in% c(-78.8, -78.9, -79)) |>
  # make month indicator variables 
  mutate(month5 = ifelse(month == 5, 1, 0),
         month6 = ifelse(month == 6, 1, 0), 
         month7 = ifelse(month == 7, 1, 0),
         month8 = ifelse(month == 8, 1, 0),
         month9 = ifelse(month == 9, 1,0)) |>
  # make date and select columns
  mutate(date = as.POSIXct(str_sub(datetime, 1, 10))) |>
  select(date, air_temp = Tair, pressure = pres, solar, dewpoint, 
         humidity = relhum, wind_10m:hi, month:date) -> df

# ----------------------------------------------------------------------------#
#                               GET LAG DATA 
# ----------------------------------------------------------------------------#

# get lag day 1-5 dates 
df |>
  select(date) |>
  distinct() |>
  mutate(lag1 = lag(date, 1),
         lag2 = lag(date, 2),
         lag3 = lag(date, 3),
         lag4 = lag(date, 4),
         lag5 = lag(date, 5)) |>
  filter(!is.na(lag5)) -> lag_dates

# function to process lag data 
process_lag_data <- function(df) {
  df %>%
    group_by(date) %>%
    summarise_if(is.numeric, list(min, max, mean, var), na.rm = TRUE) %>%
    setNames(gsub("_fn1","_min",names(.))) %>%
    setNames(gsub("_fn2","_max",names(.))) %>%
    setNames(gsub("_fn3","_mean",names(.))) %>%
    setNames(gsub("_fn4","_var",names(.)))  -> df_lag
  
  return(df_lag)
}

# function to get data by lag day 
get_lag_data <- function(start, stop) {
  
  final_df <- data.frame()
  
  for(i in start:stop){
    name <- paste0('lag', i)
    df |>
      select(-c(month:month9)) |>
      inner_join(lag_dates[, c('date', name)], by=c('date'=name)) |>
      select(-date, date = date.y) -> sub_df
    
    final_df <- rbind(final_df, sub_df)
  }
  return(final_df)
}

# one day lag 
##### CHECK 
df_lag1 <- process_lag_data(df |> select(-c(month:month9)))


# three day lag
df3 <- get_lag_data(1, 3)
df_lag3 <- process_lag_data(df3) %>%
  setNames(paste0('lag3_', names(.))) 

# five day lag
df5 <- get_lag_data(1, 5)
df_lag5 <- process_lag_data(df5) %>%
  setNames(paste0('lag5_', names(.))) 

# ----------------------------------------------------------------------------#
#                               GET WARNING DATA 
# ----------------------------------------------------------------------------#

warn <- readxl::read_xlsx('vtec_93.6530W_41.5300N_19860101_20221122 (1).xlsx')

warn |>
  filter(ph_name %in% c('Excessive Heat', 'Heat' )) |>
  mutate(date_issued = as.POSIXct(str_sub(issued, 1, 10)),
         date_expired = as.POSIXct(str_sub(expired, 1, 10)),
         diff = as.integer(date_expired - date_issued)) -> warn

warn |>
  filter(diff < 0 ) |>
  rename(date_expired = date_issued, date_issued = date_expired) |>
  mutate(diff = as.integer(date_expired - date_issued)) -> warn_neg

warn |>
  filter(diff >= 0) |>
  bind_rows(warn_neg) -> warn

dates <- c()
for (i in 1:nrow(warn)){
  n <- seq(warn$date_issued[i], warn$date_expired[i], by='days')
  dates <- c(dates, n)
}

warning_dates <- as.POSIXct(dates, origin = "1970-01-01")

# make dataframe and add indicator column
data.frame(date = warning_dates) |>
  distinct() |>
  mutate(is_heat = TRUE) -> warn_fin

# ----------------------------------------------------------------------------#
#                           MERGE FEATURES & OUTCOME
# ----------------------------------------------------------------------------#

# bind aggs 
cbind(df_lag3, df_lag5) |>
  select(-c(lag3_date), date=lag5_date) |>
  inner_join(df_lag1, by='date') |>
  # left join onto warnings data 
  left_join(warn_fin, by = 'date') |>
  mutate(is_heat = ifelse(is.na(is_heat), FALSE, is_heat)) |>
  select(date, is_heat, air_temp_min:hi_var, lag3_air_temp_min:lag5_hi_var)-> df_fin

# write to csv 
write.csv(df_fin, "processed-data-02-07.csv")




