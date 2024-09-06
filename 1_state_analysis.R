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

# top 5 cwift states
state_cwift |> 
  slice_max(st_cwiftest, n = 5)

# bottom 5 cwift states
state_cwift |> 
  slice_min(st_cwiftest, n = 5)

# inspect state CWIFT estimates
(cwift_estimates <- state_cwift |>
  arrange(st_cwiftest)  |> 
  mutate(st_name = fct_inorder(factor(st_name))) |> 
  ggplot(aes(st_cwiftest, st_name)) +
  geom_errorbar(
    mapping = aes(
      xmin = st_cwiftest - qnorm(0.975) * st_cwiftse,
      xmax = st_cwiftest + qnorm(0.975) * st_cwiftse
    ),
    width = 0.3
    ) + 
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "State CWIFT",
    y = NULL
  ))

# univariate graphs for state CWIFT

# dot density plot
(dot_density_cwift <- state_cwift |> 
  ggplot(aes(st_cwiftest)) +
  ggdist::geom_dots() +
  theme_classic() +
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
  ggplot(aes(fill = st_cwiftest)) +
  geom_sf() +
  scale_fill_distiller(
    name = "State\nCWIFT",
    type = "seq",
    direction = 1,
    palette = "Greys", 
    limits = c(0.75, 1.5)
  ) +
  theme_void())

# Heat map with states in Census defined West region
# list of states in Census defined west region
census_west_region_states <- tibble(
    state_abb = state.abb, 
    region = as.character(state.region)
  ) |> 
  filter(region == "West") |> 
  pull(state_abb)

# heat map
(west_state_cwift_heat_map <- state_cwift_map_data |> 
  filter(
    stusps %in% c(census_west_region_states)
  ) |> 
  ggplot(aes(fill = st_cwiftest)) +
  geom_sf() +
  geom_sf_label(aes(label = stusps)) +
  scale_fill_distiller(
    name = "State\nCWIFT",
    type = "seq",
    direction = 1,
    palette = "Greys", 
    limits = c(0.75, 1.5)
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
      palette = "Greys",
      limits = c(0.75, 1.5)
    ) +
    theme_void())

# state cwift by household income
(cwift_by_income <- income_data |> 
  inner_join(
    state_cwift |> 
      select(st_fips, st_cwiftest, st_cwiftse),
    by = c("geoid" = "st_fips")
  ) |>
  ggplot(aes(x = estimate, st_cwiftest)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(labels = scales::label_currency()) +
  labs(
    title = "State CWIFT by State Median Household Income",
    x = NULL,
    y = NULL
  ) +
  theme_minimal())

# arranging plots & writing/saving plots out for book chapter ----

# Example CWIFT plots
(state_plot_collection <- cwift_estimates + 
  (density_plot_cwift / box_plot_cwift  / dot_density_cwift) +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")")) &
  theme(plot.tag = element_text(size = 12))

# CWIFT heat maps
(state_heat_maps <- (state_cwift_heat_map + st_cwift_bins_plot) +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 12)
  ))

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

ggsave(
  filename = "plots/state_cwift_by_income.png",
  plot = cwift_by_income,
  height= 5,
  width = 6,
  units = "in"
)