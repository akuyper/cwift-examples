# County CWIFT exploration ----

# load packages ----
library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)
library(patchwork)

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

# join county CWIFT data with mapping/geographic data
county_cwift_map_data <- county_map_data |> 
  left_join(
    county_cwift |> 
      select(cnty_fips, cnty_cwiftest, cnty_cwiftse),
    by = c("geoid" = "cnty_fips")
  ) 

# county household income data from ACS
income_data <- get_acs(
  geography = "county",    
  year = 2021,             
  survey = "acs5",         
  variables = "B19013_001" 
  ) |> 
  janitor::clean_names()

# Explore CWIFT data ----

# quick summary of data
county_cwift |> 
  skimr::skim_without_charts()

# summary of CWIFTs per state
county_cwift |> 
  group_by(st_name) |> 
  skimr::skim_without_charts(cnty_cwiftest)

# number of counties per state
county_cwift |> 
  count(st_name, name = "num_counties", sort = TRUE)

# top 5 cwift counties
county_cwift |> 
  slice_max(cnty_cwiftest, n = 5)

# max cwift county per state
county_cwift |> 
  slice_max(cnty_cwiftest, n = 1, by = st_name)

# bottom 5 cwift counties
county_cwift |> 
  slice_min(cnty_cwiftest, n = 5)

# min cwift county per state
county_cwift |> 
  slice_min(cnty_cwiftest, n = 1, by = st_name)

# inspect county CWIFT estimates
(cwift_estimates <- county_cwift |> 
  arrange(cnty_cwiftest)  |> 
  mutate(cnty_fips = fct_inorder(factor(cnty_fips)))|> 
  ggplot(aes(cnty_cwiftest, cnty_fips)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbar(
    mapping = aes(
      xmin = cnty_cwiftest - qnorm(0.975) * cnty_cwiftse,
      xmax = cnty_cwiftest + qnorm(0.975) * cnty_cwiftse
    ),
    width = 0.3,
    alpha = 0.2
  ) + 
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
    ) +
  labs(
    x = "County CWIFT",
    y = "Counties"
  ))

# too many individual counties, group by state and use boxplot
(cwift_estimates_by_state <- county_cwift |> 
  mutate(
    st_name = fct_reorder(
        .f = factor(st_name),
        .x = cnty_cwiftest, 
        .fun = median, 
        .na_rm = TRUE
        )
    ) |> 
  ggplot(aes(cnty_cwiftest, st_name)) +
  geom_boxplot(color = "gray60") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "County CWIFT",
    y = NULL
  ))

# univariate graphs for county CWIFT

# dot density plot
(dot_density_cwift <- county_cwift |> 
    ggplot(aes(cnty_cwiftest)) +
    ggdist::geom_dots() +
    theme_classic(base_size = 14) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    xlab("county CWIFT"))

# box plot
(box_plot_cwift <- county_cwift |> 
  ggplot(aes(cnty_cwiftest)) +
  geom_boxplot()) 

# density plot
(density_plot_cwift <-county_cwift |> 
  ggplot(aes(cnty_cwiftest)) +
  geom_density()) 

# CWIFT county heat map
(county_cwift_heatmap <- county_cwift_map_data |> 
  ggplot(aes(fill = cnty_cwiftest)) +
    geom_sf(color = "grey80") +
    scale_fill_distiller(
      name = "County\nCWIFT  ",
      type = "seq",
      direction = 1,
      palette = "Greys", limits = c(0.5, 1.5)
    ) +
    theme_void())

# function to make county map for a single state or group of states
county_cwift_state_map <- function(state_abb = "SD"){
  county_cwift_map_data |> 
    filter(
      stusps %in% c(state_abb)
    ) |> 
    ggplot(aes(fill = cnty_cwiftest)) +
    geom_sf(color = "grey80") +
    scale_fill_distiller(
      name = "County\nCWIFT  ",
      type = "seq",
      direction = 1,
      palette = "Greys", limits = c(0.5, 1.5)
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_sf(crs = 4269)
}

# state maps
county_cwift_state_map("SD")
county_cwift_state_map("MD")
county_cwift_state_map("CA")
county_cwift_state_map("CO") 

# Heat map with states in Census defined West region (remove AK & HI)
# list of states in Census defined west region
census_west_region_states <- tibble(
    state_abb = state.abb, 
    region = as.character(state.region)
  ) |> 
  filter(region == "West") |> 
  pull(state_abb) |> 
  # remove AK & HI
  setdiff(c("AK", "HI"))

# US west without AK & HI
county_cwift_state_map(census_west_region_states)

# county cwift by household income
(cwift_by_income <-income_data |> 
  inner_join(
    county_cwift |> 
      select(cnty_fips, cnty_cwiftest, cnty_cwiftse),
    by = c("geoid" = "cnty_fips")
  ) |>
  ggplot(aes(x = estimate, cnty_cwiftest)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(
    name = "Median Household Income", 
    labels = scales::label_currency()
  ) +
  ylab("County CWIFT") +
  theme_minimal())

# arranging plots & writing/saving plots out for book chapter
# CWIFT heat maps
(county_state_heatmaps <- 
    county_cwift_state_map("CA") + 
    (county_cwift_state_map("SD") / county_cwift_state_map("MD")) +
    plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")") +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      plot.tag = element_text(size = 12)
    ))

# write out/save images
ggsave(
  filename = "plots/cwift_estimates_by_state.png",
  plot = cwift_estimates_by_state,
  height= 7,
  width = 6,
  units = "in"
)

ggsave(
  filename = "plots/county_state_heatmaps.png",
  plot = county_state_heatmaps,
  height= 5,
  width = 6,
  units = "in"
)

ggsave(
  filename = "plots/county_cwift_heatmap.png",
  plot = county_cwift_heatmap,
  height= 5,
  width = 6,
  units = "in"
)

ggsave(
  filename = "plots/county_cwift_by_income.png",
  plot = cwift_by_income,
  height= 5,
  width = 6,
  units = "in"
)

