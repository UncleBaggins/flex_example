library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(timetk)
library(purrr)

## ---- Read Data ---- ##
admissions <- read_csv("data/Iowa_Prison_Admissions.csv", 
                       col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"), 
                                        `Date of Release` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() 


releases <- read_csv("data/Offenders_Released_from_Iowa_Prisons.csv", 
                     col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"),
                                      `Release Date` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() 



## ---- Format data to be unioned ---- ##

admission_dates <- admissions %>% 
  select(offender_number, admission_date, date_of_release)

release_dates <- releases %>% 
  rename(date_of_release = release_date) %>% 
  select(offender_number, admission_date, date_of_release) 


## ---- Expload combined datasets and unnest ---- ##
unioned_df <- admission_dates %>%
  bind_rows(release_dates) %>%
  mutate(start_date = floor_date(admission_date, 'month'),
         end_date = floor_date(date_of_release, 'month'),
         visit_id = paste0(offender_number, as.integer(start_date))) %>% 
  select(offender_number, start_date, end_date, visit_id) %>% 
  group_by(visit_id) %>%
  summarise(min_date = min(start_date),
            max_date = max(end_date)) %>%
  ungroup() %>%
  group_by(visit_id) %>%
  mutate(dates_admitted = list(
    tibble(
      ds = tk_make_timeseries(min_date, max_date, by = "month")
      )
    )) %>% 
  tidyr::unnest(cols = c(dates_admitted))



## ---- Aggregate by Month ---- ##
monthly_prison_pop <- unioned_df %>% 
  ungroup() %>% 
  filter(ds >= as.Date('2010-01-01')) %>% 
  count(ds)

## ---- Save .rda for Modeling ---- ##
save(monthly_prison_pop, file = "./data/monthly_prison_pop.rda")


# offenders_active_dates <- current_admissions %>% 
#   mutate(min_date = floor_date(prison_start_date, unit = 'month'),
#          max_date = floor_date(report_date, 'month')) %>% 
#   select(offender_number, record_id, min_date, max_date) %>% 
#   group_by(offender_number, record_id) %>% 
#   mutate(dates_admitted = list(tibble(
#     ds = tk_make_timeseries(min_date, max_date, by = "month")
#   )))
# 
# 
# offender_release_dates <- released_admissions %>% 
#   filter(!is.na(admission_date),
#          !is.na(date_of_release)) %>% 
#   mutate(min_date = floor_date(admission_date, 'month'),
#             max_date = floor_date(date_of_release, 'month'))  %>% 
#   select(offender_number, record_id, min_date, max_date) %>%
#   group_by(offender_number, record_id) %>% 
#   mutate(dates_admitted = list(tibble(
#     ds = tk_make_timeseries(min_date, max_date, by = "month")
#   )))
