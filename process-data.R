library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(timetk)

## ---- Read Data ---- ##
admissions <- read_csv("data/Iowa_Prison_Admissions.csv", 
                       col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"), 
                                        `Date of Release` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() 


releases <- read_csv("data/Offenders_Released_from_Iowa_Prisons.csv", 
                     col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"),
                                      `Release Date` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() 


current <- read_csv("data/Current_Iowa_Correctional_System_Prison_Population.csv", 
                    col_types = cols(`Prison Start Date` = col_date(format = "%m/%d/%Y"), 
                                     `Report Date` = col_date(format = "%m/%d/%Y"))) %>% 
  clean_names() 

## ---- Format data to be unioned ---- ##

current_dates <- current %>% 
  rename(admission_date = prison_start_date) %>% 
  select(offender_number, admission_date, report_date, sex, race_ethnicity, offense_code, offense_classification, offense_type, offense_subtype) 

admission_dates <- admissions %>% 
  select(offender_number, admission_date, date_of_release, sex, race_ethnicity, offense_code, offense_classification, offense_type, offense_subtype)

release_dates <- releases %>% 
  rename(date_of_release = release_date) %>% 
  select(offender_number, admission_date, date_of_release, sex, race_ethnicity, offense_code, offense_classification, offense_type, offense_subtype) 


## ---- Expload combined datasets and unnest ---- ##
unioned_df <- admission_dates %>%
  
  # Union releases and admissions
  bind_rows(release_dates) %>%
  
  # Join current prisoners, accounting for duplicates. Offenders not
  # in admissions/releases will not have a `date_of_release` 
  full_join(current_dates, by = c('offender_number', 'admission_date', 'sex', 'race_ethnicity', 'offense_code', 'offense_classification', 'offense_type', 'offense_subtype')) %>% 
  
  # `in_system_through_date` checks to see if there's a date of release. If not,
  # they are considered active as of this year-month
  mutate(
    in_system_through_date = case_when(
      is.na(date_of_release) ~ floor_date(today(), 'month'),
      TRUE ~ date_of_release
    )) %>% 
  distinct() %>%
  
  # Handles situtations where offenses are split into separate release periods
  group_by(offender_number, admission_date) %>% 
  filter(in_system_through_date == max(in_system_through_date)) %>%
  ungroup() %>% 
  
  # collects offense codes into lists where offenders had multiple charges and serving them during the same period.
  # group_by(offender_number, admission_date, in_system_through_date, sex, race_ethnicity) %>% 
  # summarise(offense_codes = list(offense_code),
  #           offense_classifications = list(offense_classification),
  #           offense_types = list(offense_type),
  #           offense_subtypes = list(offense_subtype)) %>% 
  # ungroup() %>% 
  
  # Truncates dates to year-month for aggregation purposes
  # creates `visit_id` which is a unique identifier for each offender visit
  mutate(start_date = floor_date(admission_date, 'month'),
       end_date = floor_date(in_system_through_date, 'month'),
       visit_id = paste0(offender_number, as.integer(start_date))) %>% 
  select(offender_number, start_date, end_date, visit_id, sex, race_ethnicity, offense_code, offense_classification, offense_type, offense_subtype) %>% 
  
  # grouping visit_id and finding the min/max start dates 
  # sets the stage for exploading months in prison
  group_by(visit_id, sex, race_ethnicity) %>%
  summarise(min_date = min(start_date),
            max_date = max(end_date),
            offense_codes = list(offense_code),
            offense_classifications = list(offense_classification),
            offense_types = list(offense_type),
            offense_subtypes = list(offense_subtype)) %>%
  ungroup() %>% 
  
  # expload dates in nested dataframe
  group_by(visit_id) %>%
  mutate(dates_admitted = list(
    tibble(
      ds = tk_make_timeseries(min_date, max_date, by = "month")
      )
    )) %>% 
  
  #  unnest exploaded dates
  tidyr::unnest(cols = c(dates_admitted))



## ---- Aggregate by Month ---- ##
monthly_prison_pop <- unioned_df %>% 
  ungroup() %>% 
  filter(ds >= as.Date('2010-01-01')) %>% 
  tidyr::unnest(cols = c(offense_codes,offense_classifications,offense_types,offense_subtypes)) %>% 
  group_by(ds,sex, race_ethnicity, offense_codes,offense_classifications,offense_types,offense_subtypes) %>% 
  summarise(n = n()) %>% 
  ungroup()

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
