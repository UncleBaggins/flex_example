library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(timetk)
library(purrr)

## ---- Read Data ---- ##
released_admissions <- read_csv("data/Iowa_Prison_Admissions.csv", 
                                   col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"), 
                                                    `Date of Release` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() 


current_admissions <- read_csv("data/Current_Iowa_Correctional_System_Prison_Population (1).csv", 
                                                                  col_types = cols(`Prison Start Date` = col_date(format = "%m/%d/%Y"), 
                                                                                   `Report Date` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() 



## ---- Expload Offender Dates in Prison (Monthly) ---- ##
offenders_active_dates <- current_admissions %>% 
  mutate(min_date = floor_date(prison_start_date, unit = 'month'),
         max_date = floor_date(report_date, 'month')) %>% 
  select(offender_number, record_id, min_date, max_date) %>% 
  group_by(offender_number, record_id) %>% 
  mutate(dates_admitted = list(tibble(
    ds = tk_make_timeseries(min_date, max_date, by = "month")
  )))


offender_release_dates <- released_admissions %>% 
  filter(!is.na(admission_date),
         !is.na(date_of_release)) %>% 
  mutate(min_date = floor_date(admission_date, 'month'),
            max_date = floor_date(date_of_release, 'month'))  %>% 
  select(offender_number, record_id, min_date, max_date) %>%
  group_by(offender_number, record_id) %>% 
  mutate(dates_admitted = list(tibble(
    ds = tk_make_timeseries(min_date, max_date, by = "month")
  )))


## ---- Combine Exploaded Datasets ---- ##
unioned_df <- offender_release_dates %>%
  select(offender_number, record_id, dates_admitted) %>% 
  tidyr::unnest(cols = c(dates_admitted)) %>% 
  ungroup() %>% 
  bind_rows(offenders_active_dates %>%
              select(offender_number, record_id, dates_admitted) %>% 
              tidyr::unnest(cols = c(dates_admitted))) %>% 
  ungroup()


## ---- Aggregate by Month ---- ##
monthly_prison_pop <- unioned_df %>% 
  ungroup() %>% 
  filter(ds >= as.Date('2010-01-01')) %>% 
  count(ds)

## ---- Save .rda for Modeling ---- ##
save(monthly_prison_pop, file = "./data/monthly_prison_pop.rda")
