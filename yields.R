# What is this? ---------------------------------------------------------------------------------------------------

# Code to update yield curve explainer
# https://www.brookings.edu/blog/up-front/2018/12/05/the-hutchins-center-explains-the-yield-curve-what-it-is-and-why-it-matters/

# Setup -----------------------------------------------------------------------------------------------------------

librarian::shelf(tidyverse, lubridate, ggbrookings, glue)
update_geom_defaults('line',
                     list(size = 0.75,
                          color = "#003A79"))
theme_set(theme_brookings(base_size = 9))

plot_options <-
  list(coord_cartesian(clip = 'off'))
# Figure 1: 30-day Treasury notes ---------------------------------------------------------------------------------

cur_yield_raw <- readr::read_csv('https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/all/202203?type=daily_treasury_yield_curve&field_tdr_date_value_month=202203&page&_format=csv')


# Tidy
cur_yield <-
  cur_yield_raw |>
  pivot_longer(-Date,
               names_to = 'maturity',
               values_to = 'yield') |>
  janitor::clean_names() |>
  mutate(date = mdy(date),
         maturity = factor(maturity, levels = c("1 Mo",
                                                "2 Mo",
                                                "3 Mo",
                                                "6 Mo",
                                                "1 Yr",
                                                "2 Yr",
                                                "3 Yr",
                                                "5 Yr",
                                                "7 Yr",
                                                "10 Yr",
                                                "20 Yr",
                                                "30 Yr"))) |>
  filter(date == max(date))



cur_yield |>
  ggplot(aes(x = maturity, y = yield, group = date)) +
  geom_line() +
  scale_x_discrete(expand = expansion(0, 0.2)) +
  scale_y_continuous(expand = expansion(),
                     labels = scales::label_percent(scale = 1,accuracy = 0.1)) +
  labs(x = NULL,
       y = NULL,
       title = 'The Yield Curve',
       caption = '**Source**: U.S. Treasury Department.<br>') +
  coord_cartesian(clip = 'off')

# Export
fig_name <- '01-cur-yield'
brookings_save(glue('figures/{fig_name}.png'))
plogo <- add_logo(glue('figures/{fig_name}.png'), logo_path = 'hc',
        height_padding = 0.02)
magick::image_write(plogo, path = glue('figures/{fig_name}.png'))



# Figure 2 --------------------------------------------------------------------------------------------------------

yield_2009 <-
  readr::read_csv("https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/2009/all?type=daily_treasury_yield_curve&field_tdr_date_value=2009&page&_format=csv") |>
  mutate(Date = mdy(Date)) |>
  filter(Date == min(Date))
yield_2010 <- read_csv("https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/2010/all?type=daily_treasury_yield_curve&field_tdr_date_value=2010&page&_format=csv") |>
  mutate(Date = mdy(Date)) |>
  filter(Date == min(Date))
cur_yield_raw <- readr::read_csv('https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/all/202203?type=daily_treasury_yield_curve&field_tdr_date_value_month=202203&page&_format=csv') |>
  mutate(Date = mdy(Date)) |>
  filter(Date == min(Date))



yields <-
  yield_2009 |>
  full_join(yield_2010) |>
  full_join(cur_yield_raw) |>
  pivot_longer(-Date,
               names_to = 'maturity',
               values_to = 'yield') |>
  janitor::clean_names() |>
  mutate(year = as.character(year(date)),
         maturity = factor(maturity, levels = c("1 Mo",
                                                "2 Mo",
                                                "3 Mo",
                                                "6 Mo",
                                                "1 Yr",
                                                "2 Yr",
                                                "3 Yr",
                                                "5 Yr",
                                                "7 Yr",
                                                "10 Yr",
                                                "20 Yr",
                                                "30 Yr"))) |>
  filter(maturity != '2 Mo')

yields |>
  ggplot(mapping = aes(x = maturity, y = yield,  color = year, group = year)) +
  geom_line() +
  scale_color_brookings() +
  scale_x_discrete(expand = expansion(0, 0.1)) +
  scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = NULL,
       title = "The Yield Curve",
       subtitle = "The Yield Curve is much flatter now than it was at the end of the Great Recession",
       caption = '**Source**: U.S. Treasury Department.<br>')

# Export
fig_name <- '02-yields'
brookings_save(glue('figures/{fig_name}.png'))
plogo <- add_logo(glue('figures/{fig_name}.png'), logo_path = 'hc',
                  height_padding = 0.02)
magick::image_write(plogo, path = glue('figures/{fig_name}.png'))

