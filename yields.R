
# Setup -----------------------------------------------------------------------------------------------------------

librarian::shelf(tidyverse, lubridate, ggbrookings)
# Figure 1: 30-day Treasury notes ---------------------------------------------------------------------------------

cur_yield_raw <- readr::read_csv('https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/all/202203?type=daily_treasury_yield_curve&field_tdr_date_value_month=202203&page&_format=csv')


# Tidy
cur_yield <-
  cur_yield |>
  pivot_longer(-Date,
               names_to = 'period',
               values_to = 'yield') |>
  janitor::clean_names() |>
  mutate(date = mdy(date))
