# State CWIFT exploration ----

# load packages ----
library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)
library(statebins)
library(patchwork)

# load data ----

# import 2021 CWIFT data
state_cwift <- read_tsv("data/EDGE_ACS_CWIFT2021_State.txt") |> 
  janitor::clean_names()

# top 5 states
state_cwift |> 
  slice_max(st_cwiftest, n = 5)

# bottom 5 states
state_cwift |> 
  slice_min(st_cwiftest, n = 5)

# state map data
state_map_data <- tigris::states(year = 2021, cb = TRUE) |> 
  shift_geometry() |> 
  janitor::clean_names() |> 
  filter(statefp %in% unique(state_cwift$st_fips))

# join state CWIFT data with mapping/geographic data
state_cwift_map_data <- state_map_data |> 
  left_join(
    state_cwift |> 
      select(st_fips, st_cwiftest, st_cwiftse),
    by = c("geoid" = "st_fips")
  ) 

# state household income data from ACS
income_data <- get_acs(
  geography = "state",    
  year = 2021,             
  survey = "acs5",         
  variables = "B19013_001" 
  ) |> 
  janitor::clean_names()

# Explore CWIFT data ----

# quick summary of data
state_cwift |> 
  skimr::skim_without_charts()

# inspect county CWIFT estimates
(cwift_estimates <- state_cwift |> 
  mutate(
    st_name = fct_reorder(
      factor(st_name), 
      st_cwiftest, 
      .fun = median, 
      .na_rm = TRUE)
    ) |> 
  ggplot(aes(st_cwiftest, st_name)) +
  geom_errorbar(
    mapping = aes(
      xmin = st_cwiftest - 2*st_cwiftse,
      xmax = st_cwiftest + 2*st_cwiftse
    ),
    width = 0.3
    ) + 
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "State CWIFT",
    y = NULL
  ))

# univariate graphs fo state CWIFT

# dot density plot
(dot_density_cwift <- state_cwift |> 
  ggplot(aes(st_cwiftest)) +
  ggdist::geom_dots() +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("State CWIFT"))

# box plot
(box_plot_cwift <- state_cwift |> 
  ggplot(aes(st_cwiftest)) +
  geom_boxplot())

# density plot
(density_plot_cwift <-state_cwift |> 
  ggplot(aes(st_cwiftest)) +
  geom_density())

# CWIFT state heat map
(state_cwift_heat_map <- state_cwift_map_data |> 
  ggplot(aes(fill = st_cwiftest), color = NA) +
  geom_sf() +
  scale_fill_distiller(
    name = "State\nCWIFT",
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.75, 1.5)
  ) +
  theme_void())

# Heat map with states in Census defined West region
# list of states in Census defined west region
census_west_region_states <- tibble(state_abb = state.abb, region = as.character(state.region)) |> 
  filter(region == "West") |> 
  pull(state_abb)

# heat map
(west_state_cwift_heat_map <- state_cwift_map_data |> 
  filter(
    stusps %in% c(census_west_region_states)
  ) |> 
  ggplot(aes(fill = st_cwiftest), color = NA) +
  geom_sf() +
  geom_sf_label(aes(label = stusps)) +
  scale_fill_distiller(
    name = "State\nCWIFT",
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.75, 1.5)
  ) +
  theme_void())

# statebins CWIFT heat map
(st_cwift_bins_plot <- state_cwift |> 
    ggplot(aes(state = st_name, fill = st_cwiftest)) +
    geom_statebins() +
    scale_fill_distiller(
      name = "State\nCWIFT",
      type = "seq",
      direction = 1,
      palette = "Greys", limits = c(0.75, 1.5)
    ) +
    theme_void())

# state cwift by houeshold income
(cwift_by_income <- income_data |> 
  inner_join(
    state_cwift |> 
      select(st_fips, st_cwiftest, st_cwiftse),
    by = c("geoid" = "st_fips")
  ) |>
  ggplot(aes(x = estimate, st_cwiftest)) +
  geom_point() +
  scale_x_continuous(
    name = "Median Household Income", 
    labels = scales::label_currency()
  ) +
  geom_smooth(se = FALSE) +
  ylab("State CWIFT") +
  theme_minimal())

# arranging plots & writing/saving plots out for book chapter

# Example CWIFT plots
(state_plot_collection <- cwift_estimates + 
  (density_plot_cwift / box_plot_cwift  / dot_density_cwift) +
  plot_annotation(tag_levels = "A"))

# CWIFT heat maps
(state_heat_maps <- (state_cwift_heat_map + st_cwift_bins_plot) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom"))

# write out/save images
ggsave(
  filename = "plots/state_plot_collection.png",
  plot = state_plot_collection,
  height= 7,
  width = 6,
  units = "in"
  )

ggsave(
  filename = "plots/state_heatmaps.png",
  plot = state_heat_maps,
  height= 3,
  width = 8,
  units = "in"
)
