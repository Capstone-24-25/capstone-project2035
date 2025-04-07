
install.packages(c("tidyverse", "maps", "tigris", "sf", "ggplot2", "tidygeocoder"))
install.packages("patchwork")
install.packages("ggrepel")

library(tidyverse)
library(tidygeocoder)
library(sf)
library(tigris)       # For county shapefiles
library(patchwork)    # For side-by-side plotting
library(ggrepel)

# Read CSV data
df <- read.csv("/Users/nikhilkuniyil/Documents/Winter25/PSTAT197B/capstone-project2035/Data/median_yearly_earning.csv", 
               stringsAsFactors = FALSE)

# ZIP code column is "zipcode"
df$zipcode <- as.character(df$zipcode)


# We assume each zip has 2 rows: one for Green, one for Fossil Fuel
df_wide <- df %>%
  select(zipcode, job_type, estimate) %>%
  # Pivot so that we get columns "Green" and "Fossil Fuel"
  pivot_wider(
    names_from = job_type,
    values_from = estimate,
    # In case multiple rows per job_type per ZIP, we can average them
    values_fn = mean
  ) %>%
  # This results in columns named "Green" and "Fossil Fuel"
  rename(
    green_wage   = Green,
    fossil_wage  = `Fossil Fuel`
  ) %>%
  # Create the difference
  mutate(wage_diff = green_wage - fossil_wage)

head(df_wide)


options(tigris_use_cache = TRUE)

# Grab CA ZCTAs from 2010
ca_zips <- zctas(state = "CA", year = 2010, class = "sf")

# Rename 'ZCTA5CE10' to 'zipcode'
ca_zips <- ca_zips %>%
  rename(zipcode = ZCTA5CE10) %>%
  mutate(zipcode = as.character(zipcode))

# Grab county polygons for CA
ca_counties <- counties(state = "CA", cb = TRUE, class = "sf")

# Filter to just these three counties
counties_of_interest <- c("Ventura", "San Luis Obispo", "Santa Barbara")
three_counties <- ca_counties %>%
  filter(NAME %in% counties_of_interest)

# Keep only ZIP polygons that fall within these 3 counties
zips_in_3counties <- st_join(ca_zips, three_counties, join = st_within)
zips_in_3counties <- zips_in_3counties %>% filter(!is.na(NAME))  # drop others


zips_joined <- left_join(zips_in_3counties, df_wide, by = "zipcode")

# Create a copy of zips_joined with centroid coordinates
zips_joined_points <- zips_joined %>%
  st_centroid() %>%                  # compute polygon centroids
  mutate(
    x = st_coordinates(.)[, 1],     # extract longitude
    y = st_coordinates(.)[, 2]      # extract latitude
  ) %>%
  st_drop_geometry()                 # remove sf geometry so we have a regular data frame


p <- ggplot() +
  # (A) Draw county outlines
  geom_sf(
    data = three_counties,
    fill = NA, 
    color = "black",
    size = 1
  ) +
  # (B) Draw the ZIP polygons in color
  geom_sf(
    data = zips_joined,
    aes(fill = wage_diff),
    color = "white"  # Zip borders in black
  ) +
  # (C) Use numeric x,y in geom_text_repel for labeling
  geom_text_repel(
    data = zips_joined_points,
    aes(x = x, y = y, label = zipcode),
    size = 2,
    segment.color = "black"
  ) +
  # (D) Color scale for wage_diff (example: green vs. red scale)
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green",
    midpoint = 0, 
    name = "Green - Fossil\nWage Diff",
    na.value = "gray80"
  ) +
  labs(
    title = "Difference in Wages (Green - Fossil) by ZIP",
    subtitle = "Ventura, SLO, & Santa Barbara",
    caption = "Negative (red) => Fossil wage higher; Positive (green) => Green wage higher"
  ) +
  theme_void()
