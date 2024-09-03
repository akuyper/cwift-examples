# load packages
library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)

# load data
county_cwift <- read_delim("data/teacher-wage-index-data/EDGE_ACS_CWIFT2021/EDGE_ACS_CWIFT2021_County.txt")
state_cwift <- read_delim("data/teacher-wage-index-data/EDGE_ACS_CWIFT2021/EDGE_ACS_CWIFT2021_State.txt")

ca_data <- tigris::counties(cb = TRUE, year = 2021)

ca_data |> 
  inner_join(
    county_cwift |> 
      select(CNTY_FIPS, CNTY_CWIFTEST, CNTY_CWIFTSE),
    by = c("GEOID" = "CNTY_FIPS")
  ) |>
  mutate(STATE_NAME = fct_reorder(factor(STATE_NAME), CNTY_CWIFTEST, .fun = max, .na_rm = TRUE)) |> 
  ggplot(aes(x = STATE_NAME, CNTY_CWIFTEST)) +
  geom_boxplot() +
  coord_flip()

haha <- ca_data |> 
  left_join(
    county_cwift |> 
      select(CNTY_FIPS, CNTY_CWIFTEST, CNTY_CWIFTSE),
    by = c("GEOID" = "CNTY_FIPS")
  ) |> 
  ggplot(aes(fill = CNTY_CWIFTEST)) +
  geom_sf() 


ggsave(filename = "test.pdf", plot = haha, width = 11.5, height = 8)

usmap::us_map(regions = "counties") |> 
  left_join(
    county_cwift |> 
      select(CNTY_FIPS, CNTY_CWIFTEST, CNTY_CWIFTSE),
    by = c("fips" = "CNTY_FIPS")
  ) |> 
  ggplot(aes(fill = CNTY_CWIFTEST)) +
  geom_sf() 

counties_sf |> 
  left_join(
    county_cwift |> 
      select(CNTY_FIPS, CNTY_CWIFTEST, CNTY_CWIFTSE),
    by = c("county_fips" = "CNTY_FIPS")
  ) |>
ggplot(aes(fill = CNTY_CWIFTEST)) +
  geom_sf() 


county_cwift |> 
  skimr::skim_without_charts(CNTY_CWIFTEST)
