library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(timetk)
library(purrr)

fill_dates <- function(data) {
  
  min_dt = max(data$admission_date)
  max_dt = max(data$date_of_release)
  
  dates <- tibble(
    ds = tk_make_timeseries(min_dt, max_dt, by = "month")
  )

  return(dates)
}

Iowa_Prison_Admissions <- read_csv("data/Iowa_Prison_Admissions.csv", 
                                   col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"), 
                                                    `Date of Release` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() %>% 
  mutate(admission_month = floor_date(admission_date, unit = 'month')) 



dates <- tibble(
  ds = tk_make_timeseries(min(Iowa_Prison_Admissions$admission_date), max(Iowa_Prison_Admissions$date_of_release), by = "month")
)


offender_dates <- Iowa_Prison_Admissions %>% 
  # head() %>% 
  filter(!is.na(admission_date),
         !is.na(date_of_release)) %>% 
  group_by(offender_number, record_id) %>%
  summarise(min_date = min(admission_date),
            max_date = max(date_of_release))  %>% 
  ungroup() %>% 
  group_by(offender_number, record_id) %>% 
  mutate(dates_admitted = list(tibble(
    ds = tk_make_timeseries(min_date, max_date, by = "month")
  )))

offender_dates

