# Library
library(tidyverse)
library(nycflights13)

# Dataset
nycflights13::flights
glimpse(flights)

# Pipe operator
flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

## filter()
# Find all flights that departed more than 120 minutes (two hours) late:
flights  |> 
  filter(dep_delay > 120)

# Flights that departed on January 1
flights  |>  
  filter(month == 1 & day == 1)

# Flights that departed in January or February
flights  |>  
  filter(month == 1 | month == 2)

# A shorter way to select flights that departed in January or February
flights  |>  
  filter(month %in% c(1, 2))

# Saving the result
jan1 <- flights  |>  
  filter(month == 1 & day == 1)

## arrange()
# The following code sorts by the departure time, which is spread over four columns. 
# We get the earliest years first, then within a year, the earliest months
flights  |>  
  arrange(year, month, day, dep_time)

# Flights from most to least delayed:
flights  |>  
  arrange(desc(dep_delay))

## distict()
# Remove duplicate rows, if any
flights |>
  distinct()

# Find all unique origin and destination pairs
flights |>
  distinct(origin, dest)

# To keep the other columns when filtering for unique rows
flights |>
  distinct(origin, dest, .keep_all = TRUE)

# Find the number of occurrences in descending order of the number of occurrences
flights |>
  count(origin, dest, sort = TRUE)

## mutate()
# compute the gain, how much time a delayed flight made up in the air, 
# and the speed in miles per hour:
flights  |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )

# Adds new columns on the right-hand side of the dataset
flights  |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

# Add the new variables after 'day':
flights  |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )

# .keep = "used" specifies that we only keep the columns that 
# were involved or created in the mutate() step. 
flights  |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )


## select()
# Select columns by name:
flights  |> 
  select(year, month, day)

# Select all columns between year and day (inclusive):
flights  |> 
  select(year:day)

# Select all columns except those from year to day (inclusive):
flights |>
  select(!year:day)

# Select all columns that are characters:
flights |>
  select(where(is.character))
# There are a number of helper functions you can use within select():
# starts_with("abc"): matches names that begin with “abc”.
# ends_with("xyz"): matches names that end with “xyz”.
# contains("ijk"): matches names that contain “ijk”.
# num_range("x", 1:3): matches x1, x2 and x3.

# Rename variables as you select() them by using =. 
# The new name appears on the left-hand side of the =, 
# and the old variable appears on the right-hand side:

flights |>
  select(tail_num = tailnum)

## rename()
# Keep all the existing variables and just want to rename a few
flights |>
  rename(tail_num = tailnum)
# If you have a bunch of inconsistently named columns 
# check out janitor::clean_names() which provides some useful automated cleaning

## relocate()
# By default relocate() moves variables to the front:
flights |>
  relocate(time_hour, air_time)

# specify where to put them using the .before and .after arguments, 
# just like in mutate():
flights |> 
  relocate(year:dep_time, .after = time_hour)

flights |>
  relocate(starts_with("arr"), .before = dep_time)

## Pipe
# find the fastest flights to Houston’s IAH airport: 
# you need to combine filter(), mutate(), select(), and arrange():
flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

## group_by()
# divide your dataset into groups 
flights |> 
  group_by(month)

# summarize() computes the average departure delay by month:
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

# n() returns the number of rows in each group:
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

## slice_ functions
# There are five handy functions that allow you to extract specific rows within each group:
# df |> slice_head(n = 1) takes the first row from each group.
# df |> slice_tail(n = 1) takes the last row in each group.
# df |> slice_min(x, n = 1) takes the row with the smallest value of column x.
# df |> slice_max(x, n = 1) takes the row with the largest value of column x.
# df |> slice_sample(n = 1) takes one random row.

# find the flights that are most delayed upon arrival at each destination:
flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(dest)

## grouping by multiple variables
# make a group for each date
daily <- flights |>  
  group_by(year, month, day)
daily

daily_flights <- daily |> 
  summarize(n = n())
#> `summarise()` has grouped output by 'year', 'month'. You can override using
#> the `.groups` argument.

# explicitly request it in order to suppress the message:
daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )

## ungrouping
# remove grouping from a data frame without using summarize()
daily |> 
  ungroup()

# what happens when you summarize an ungrouped data frame?
daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
  )
#get a single row back because dplyr treats all the rows in an ungrouped data frame 
# as belonging to one group

## .by
# use the .by argument to group within a single operation:
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )

# group by multiple variables:
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )


# baseball data from the Lahman package:
# compare what proportion of times a player gets a hit (H) vs. the number of times 
# they try to put the ball in play (AB):

batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters

#  plot the skill of the batter (measured by the batting average, performance) against 
# the number of opportunities to hit the ball (measured by times at bat, n)
batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) + 
  geom_smooth(se = FALSE)

### Summary ###

# manipulate the rows: filter(), arrange()
# manipulate the columns: select(), mutate()
# manipulate groups: group_by(), summarize()


