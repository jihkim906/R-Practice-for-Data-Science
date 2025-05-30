library(tidyverse)

# Dataset
table1
glimpse(table1)

# Tidy data
# Compute rate per 10,000
table1 |>
  mutate(rate = cases / population * 10000)

# Compute total cases per year
table1 |> 
  group_by(year) |> 
  summarize(total_cases = sum(cases))


# Visualize changes over time
ggplot(table1, aes(x = year, y = cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country, shape = country)) +
  scale_x_continuous(breaks = c(1999, 2000)) # x-axis breaks at 1999 and 2000

# Lengthening data
# Dataset
billboard 

# Tidy data
billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank"
  )

# get rid of them by setting values_drop_na = TRUE:
billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  )

# converting values of week from character strings to numbers using mutate() 
# and readr::parse_number()
billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )

# visualize how song ranks vary over time
billboard_longer |> 
  ggplot(aes(x = week, y = rank, group = track)) + 
  geom_line(alpha = 0.25) + 
  scale_y_reverse()

# How does pivoting work?
# small tibbles
df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)

# new dataset to have three variables: id (already exists), 
# measurement (the column names), and value (the cell values)
df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

# Many variables in column names
who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"), 
    names_sep = "_",
    values_to = "count"
  )

# Data and variable names in the column headers
household |> 
  pivot_longer(
    cols = !family, 
    names_to = c(".value", "child"), 
    names_sep = "_", 
    values_drop_na = TRUE
  )

# Widening data
cms_patient_experience

# set of values for measure_cd and measure_title by using distinct():
cms_patient_experience |> 
  distinct(measure_cd, measure_title)

# not good, still multiple rows for each organization
cms_patient_experience |> 
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  )

cms_patient_experience |> 
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

# How does pivot_wider() work?
df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)

# widening
df |> 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

# first figure out what will go in the rows and columns. 
# The new column names will be the unique values of measurement
df |> 
  distinct(measurement) |> 
  pull()

# the rows in the output are determined by all the variables 
# that aren’t going into the new names or values. These are called the id_cols
df |> 
  select(-measurement, -value) |> 
  distinct()

# pivot_wider() then combines these results to generate an empty data frame:
df |> 
  select(-measurement, -value) |> 
  distinct() |> 
  mutate(x = NA, y = NA, z = NA)


