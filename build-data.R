library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(timetk)
library(purrr)


Iowa_Prison_Admissions <- read_csv("data/Iowa_Prison_Admissions.csv", 
                                   col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"), 
                                                    `Date of Release` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() %>% 
  mutate(admission_month = floor_date(admission_date, unit = 'month')) 




offender_dates <- Iowa_Prison_Admissions %>% 
  # head() %>% 
  filter(!is.na(admission_date),
         !is.na(date_of_release)) %>% 
  # group_by(offender_number, record_id) %>%
  mutate(min_date = floor_date(admission_date, 'month'),
            max_date = floor_date(date_of_release, 'month'))  %>% 
  # ungroup() %>% 
  group_by(offender_number, record_id) %>% 
  mutate(dates_admitted = list(tibble(
    ds = tk_make_timeseries(min_date, max_date, by = "month")
  )))

offender_dates_by_month <- offender_dates %>%
  select(offender_number, record_id, dates_admitted) %>% 
  tidyr::unnest(cols = c(dates_admitted))

monthly_prison_pop_released <- offender_dates_by_month %>% 
  ungroup() %>% 
  count(ds)

save(monthly_prison_pop_released, file = "./data/monthly_prison_pop_released.rda")
