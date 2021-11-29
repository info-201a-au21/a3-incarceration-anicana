# First load all libraries and packages
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(styler)
library(maps)
library(mapproj)
library(lintr)
lint("analysis.R")
style_file("analysis.R")

# Load the prison data from the a3 website
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Calculate 5 values from the data to center my report on
# Which county in the most recent year had the most jail admission count and
# what was the value?
most_admission <- incarceration_trends %>%
  filter(year == 2018) %>%
  select(county_name, total_jail_adm) %>%
  slice_max(total_jail_adm) %>%
  select(county_name, total_jail_adm)

most_admission_county <- most_admission$county_name
count_most_admission <- most_admission$total_jail_adm

# In 2018, how many of incarcerated individuals in Los Angeles were
# caucasian and what percentage is that?
la_white <- incarceration_trends %>%
  filter(year == 2018, county_name == "Los Angeles County") %>%
  select(total_jail_pop, white_jail_pop)

count_la_white <- la_white$white_jail_pop
percent_white <- strtrim(
  ((la_white$white_jail_pop / la_white$total_jail_pop) * 100), 5
)

# Since 1970, what is the maximum number of people incarcerated. When and
# where did this take place?
max_incarcerated <- incarceration_trends %>%
  slice_max(total_jail_adm) %>%
  select(year, county_name, total_jail_adm)

count_max_incarcerated <- max_incarcerated$total_jail_adm
max_incarcerated_year <- max_incarcerated$year
max_incarcerated_county <- max_incarcerated$county

# Which county had the highest percentage of incarcerated White
# individuals as of 2018?
highest_county_white <- incarceration_trends %>%
  filter(year == 2018) %>%
  select(county_name, state, total_jail_pop, white_jail_pop) %>%
  unite(location, county_name, state, sep = ", ", remove = T) %>%
  mutate(percent_white = (white_jail_pop / total_jail_pop) * 100) %>%
  filter(percent_white <= 100) %>%
  slice_max(percent_white)

# Chart 1 : Trend over Time
options(scipen = 999)

white_pop <- incarceration_trends %>%
  select(year, white_jail_pop) %>%
  filter(white_jail_pop > 0) %>%
  group_by(year) %>%
  summarize(num_ppl = sum(white_jail_pop))

black_pop <- incarceration_trends %>%
  select(year, black_jail_pop) %>%
  filter(black_jail_pop > 0) %>%
  group_by(year) %>%
  summarize(num_ppl = sum(black_jail_pop))

aapi_pop <- incarceration_trends %>%
  select(year, aapi_jail_pop) %>%
  filter(aapi_jail_pop > 0) %>%
  group_by(year) %>%
  summarize(num_ppl = sum(aapi_jail_pop))

latinx_pop <- incarceration_trends %>%
  select(year, latinx_jail_pop) %>%
  filter(latinx_jail_pop > 0) %>%
  group_by(year) %>%
  summarize(num_ppl = sum(latinx_jail_pop))

native_pop <- incarceration_trends %>%
  select(year, native_jail_pop) %>%
  filter(native_jail_pop > 0) %>%
  group_by(year) %>%
  summarize(num_ppl = sum(native_jail_pop))

race_in_jail <- ggplot() +
  geom_line(
    data = white_pop, aes(year, num_ppl, color = "White")
  ) +
  geom_line(data = aapi_pop, aes(year, num_ppl, color = "Asian American/Pacific Islander")) +
  geom_line(data = black_pop, aes(year, num_ppl, color = "Black")) +
  geom_line(data = latinx_pop, aes(year, num_ppl, color = "Latinx")) +
  geom_line(data = native_pop, aes(year, num_ppl, color = "Native American")) +
  ggtitle("Number of Incarcerated Individuals Categorized by Race") +
  labs(x = "Year", y = "Number of Individuals", color = "Race") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000))

# Chart 2 - Variable Comparison
white_metro <- incarceration_trends %>%
  filter(year == 2018) %>%
  select(metro_area, white_jail_pop) %>%
  arrange(metro_area, na.rm = TRUE)

metro_jail_pop <- ggplot(white_metro, aes(metro_area, white_jail_pop)) +
  geom_point(color = "#6BAED6", alpha = 0.7) +
  ggtitle("Metropolitan Area vs Incarcerated Whites in 2018") +
  labs(x = "Metropolitan Area", y = "Whites in Jail") +
  scale_x_continuous(breaks = c(
    10000, 15000, 20000, 25000, 30000, 35000,
    40000, 45000, 50000
  )) +
  scale_y_log10() +
  theme_minimal()

# Chart 3 - Map
white_county <- incarceration_trends %>%
  filter(year == 2018) %>%
  select(fips, state, county_name, white_jail_pop, total_jail_pop) %>%
  mutate(percent_white = (white_jail_pop / total_jail_pop) * 100) %>%
  filter(percent_white <= 100)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(white_county, by = "fips")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

white_pop_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = percent_white),
    color = "gray", size = 0.3
  ) +
  scale_fill_continuous(
    limits = c(0, max(map_data$percent_white)),
    na.value = "white", low = "#F7FCF0", high = "#084081"
  ) +
  ggtitle("Percentage of Incarcerated Whites in Each County") +
  labs(fill = "Percentage") +
  coord_map() +
  blank_theme
