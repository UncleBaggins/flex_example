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

tibble(
  ds = tk_make_timeseries("2019-03-20", "2020-04-20", by = "month")
)


offender_dates <- Iowa_Prison_Admissions %>% 
  # head() %>% 
  filter(!is.na(admission_date),
         !is.na(date_of_release)) %>% 
  group_by(offender_number, record_id) %>%
  summarise(min_date = floor_date(min(admission_date), 'month'),
            max_date = floor_date(max(date_of_release), 'month'))  %>% 
  ungroup() %>% 
  group_by(offender_number, record_id) %>% 
  mutate(dates_admitted = list(tibble(
    ds = tk_make_timeseries(min_date, max_date, by = "month")
  )))

offender_dates %>% 
  head(1) %>% 
  tidyr::unnest() %>% 
  mutate(ds = floor_date(ds, 'month'))
