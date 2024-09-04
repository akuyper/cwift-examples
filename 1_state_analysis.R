# State CWIFT exploration ----

# load packages ----
library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)
library(statebins)

# load data ----

# import 2021 CWIFT data
state_cwift <- read_tsv("data/EDGE_ACS_CWIFT2021_State.txt") |> 
  janitor::clean_names()

# county map data
state_map_data <- tigris::states(year = 2021, cb = TRUE) |> 
  shift_geometry() |> 
  janitor::clean_names() |> 
  filter(statefp %in% unique(state_cwift$st_fips))

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
(cwfti_estimates <- state_cwift |> 
  mutate(st_name = fct_reorder(factor(st_name), st_cwiftest, .fun = median, .na_rm = TRUE)) |> 
  ggplot(aes(st_cwiftest, st_name)) +
  geom_errorbar(
    aes(
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


state_cwift |> 
  ggplot(aes(st_cwiftest)) +
  ggdist::geom_dots() +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("State CWIFT") 

state_cwift |> 
  ggplot(aes(st_cwiftest)) +
  geom_boxplot() 

state_cwift |> 
  ggplot(aes(st_cwiftest)) +
  geom_density() 

state_cwift_map_data <- state_map_data |> 
  left_join(
    state_cwift |> 
      select(st_fips, st_cwiftest, st_cwiftse),
    by = c("geoid" = "st_fips")
  ) 

haha <- state_cwift_map_data |> 
  ggplot(aes(fill = st_cwiftest), color = NA) +
  geom_sf() +
  scale_fill_distiller(
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.75, 1.5)
  ) +
  theme_void()

state_cwift_map_data |> 
  filter(
    stusps %in% c("NJ", "NY", "PA")
  ) |> 
  ggplot(aes(fill = st_cwiftest), color = NA) +
  geom_sf() +
  scale_fill_distiller(
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.75, 1.5)
  ) +
  theme_void()


ggsave(filename = "test.pdf", plot = haha, width = 11.5, height = 8)



# state cwfti by houeshold income
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

# statebins CWIFT map
(st_wwift_bins_plot <- state_cwift |> 
  ggplot(aes(state = st_name, fill = st_cwiftest)) +
  geom_statebins() +
  scale_fill_distiller(
    name = "State\nCWIFT",
    type = "seq",
    direction = 1,
    palette = "Greys", limits = c(0.75, 1.5)
  ) +
  theme_statebins() )
  
