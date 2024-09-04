# 

# load packages ----
library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)

# load data ----

# import 2021 CWIFT data
county_cwift <- read_tsv("data/EDGE_ACS_CWIFT2021_County.txt") |> 
  janitor::clean_names() |> 
  mutate(
    cnty_name = iconv(cnty_name, "latin1"),
    statefp = str_extract(cnty_fips, "^\\d\\d")
    )

# county map data
county_map_data <- tigris::counties(year = 2021, cb = TRUE) |> 
  shift_geometry() |> 
  janitor::clean_names() |> 
  filter(statefp %in% unique(county_cwift$statefp))

# explore data ----

# quick summary of data
county_cwift |> 
  skimr::skim_without_charts()

# inspect county CWIFT estimates
county_cwift |> 
  # slice_sample(prop = 0.02) |> 
  arrange(cnty_cwiftest) |> 
  mutate(x_index = row_number()) |> 
  ggplot(aes(x = x_index, cnty_cwiftest)) +
  geom_errorbar(
    aes(
      ymin = cnty_cwiftest - 2*cnty_cwiftse,
      ymax = cnty_cwiftest + 2*cnty_cwiftse,
      )
    ) + 
  geom_hline(yintercept = 1) +
  coord_flip()

county_cwift |> 
  ggplot(aes(cnty_cwiftest)) +
  ggdist::geom_dots() 

county_cwift |> 
  ggplot(aes(cnty_cwiftest)) +
  geom_boxplot() 

county_cwift |> 
  ggplot(aes(cnty_cwiftest)) +
  geom_density() 

county_cwift |> 
  mutate(st_name = fct_reorder(factor(st_name), cnty_cwiftest, .fun = median, .na_rm = TRUE)) |> 
  ggplot(aes(x = st_name, cnty_cwiftest)) +
  geom_boxplot() +
  coord_flip()

county_cwift_map_data <- county_map_data |> 
  left_join(
    county_cwift |> 
      select(cnty_fips, cnty_cwiftest, cnty_cwiftse),
    by = c("geoid" = "cnty_fips")
  ) 

haha <- county_cwift_map_data |> 
  ggplot(aes(fill = cnty_cwiftest), color = NA) +
    geom_sf() +
    scale_fill_distiller(
      type = "seq",
      direction = 1,
      palette = "Greys", limits = c(0.5, 1.5)
    ) +
    theme_void()

county_cwift_map_data |> 
  filter(
    stusps %in% c("MA")
  ) |> 
  ggplot(aes(fill = cnty_cwiftest), color = NA) +
  geom_sf() +
  scale_fill_distiller(
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.5, 1.5)
  ) +
  theme_void()

county_cwift_map_data |> 
  filter(
    stusps %in% c("SD")
  ) |> 
  ggplot(aes(fill = cnty_cwiftest), color = NA) +
  geom_sf() +
  scale_fill_distiller(
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.5, 1.5)
  ) +
  theme_void()

county_cwift_map_data |> 
  filter(
    stusps %in% c("NJ", "NY", "PA")
  ) |> 
  ggplot(aes(fill = cnty_cwiftest), color = NA) +
  geom_sf() +
  scale_fill_distiller(
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.5, 1.5)
  ) +
  theme_void()

county_cwift_map_data |> 
  filter(
    stusps %in% c("TX")
  ) |> 
  ggplot(aes(fill = cnty_cwiftest), color = NA) +
  geom_sf() +
  scale_fill_distiller(
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.5, 1.5)
  ) +
  theme_void()


ggsave(filename = "test.pdf", plot = haha, width = 11.5, height = 8)


income_data <- get_acs(
  geography = "county",    # Specify the geography level
  year = 2021,             # Specify the year
  survey = "acs5",         # Specify the 5-year ACS survey
  variables = "B19013_001" # The ACS variable for median household income
  ) |> 
  janitor::clean_names()

income_data |> 
  inner_join(
    county_cwift |> 
      select(cnty_fips, cnty_cwiftest, cnty_cwiftse),
    by = c("geoid" = "cnty_fips")
  ) |>
  ggplot(aes(x = estimate, cnty_cwiftest)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

county_cwift |> 
  ggplot(aes(cnty_cwiftest, 1)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.2)

county_cwift |> 
  ggplot(aes(cnty_cwiftest, y = st_name)) +
  ggdist::geom_dots(layout = "hex", side = "both") +
  theme_classic()
