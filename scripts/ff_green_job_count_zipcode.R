library(tidyverse)
library(sf)
library(tigris)      # ZCTAs + county polygons
library(ggrepel)

options(tigris_use_cache = TRUE)

# ── 1.  Load the data ────────────────────────────────────────────────
df <- read_csv(
  "/Users/nikhilkuniyil/Documents/Winter25/PSTAT197B/capstone-project2035/Data/median_yearly_earning.csv",
  col_types = cols(zipcode = col_character())
)

#df <- df %>% 
  #mutate(
    #estimate = readr::parse_number(estimate)   # drops commas, coerc(es) to dbl
  #)

# ── 2.  Count fossil-fuel jobs by ZIP ────────────────────────────────
# ── Sum the employment estimates for all fossil-fuel occupations ──
fossil_zip_counts <- df %>% 
  filter(job_type == "Fossil Fuel") %>% 
  group_by(zipcode) %>% 
  summarise(
    fossil_jobs = sum(estimate, na.rm = TRUE),
    .groups = "drop"
  )
# ── 3.  Get polygons for the seven target counties ───────────────────
ca_zips  <- zctas(state = "CA", year = 2010, class = "sf") %>% 
  rename(zipcode = ZCTA5CE10) %>% 
  mutate(zipcode = as.character(zipcode))

ca_counties <- counties(state = "CA", cb = TRUE, class = "sf")

target_cty <- c("Ventura", "San Luis Obispo", "Santa Barbara")

seven_cty <- ca_counties %>% filter(NAME %in% target_cty)

# keep only ZIPs that fall inside one of the seven counties
zips_in_7 <- st_join(ca_zips, seven_cty, join = st_within) %>% 
  filter(!is.na(NAME))

# attach fossil-job counts
zips_joined <- left_join(zips_in_7, fossil_zip_counts, by = "zipcode") %>% 
  mutate(fossil_jobs = replace_na(fossil_jobs, 0))   # show 0s instead of NA

# label positions
zips_pts <- zips_joined %>% 
  st_centroid() %>% 
  transmute(zipcode,
            x = st_coordinates(geometry)[,1],
            y = st_coordinates(geometry)[,2],
            fossil_jobs) %>% 
  st_drop_geometry()

# ── 4.  Plot ─────────────────────────────────────────────────────────
ggplot() +
  geom_sf(data = seven_cty, fill = NA, colour = "black", linewidth = 0.6) +
  geom_sf(data = zips_joined,
          aes(fill = fossil_jobs),
          colour = "white", linewidth = 0.15) +
  geom_text_repel(data = zips_pts,
                  aes(x = x, y = y, label = zipcode),
                  size = 2, segment.color = "black") +
  scale_fill_gradient(
    low  = "white",
    high = "sienna4",         # rich brown
    labels = scales::label_comma(),
    na.value = "grey80",
    name = "Fossil-fuel\njob count"
  ) +
  labs(title    = "Number of Fossil-Fuel Jobs by ZIP Code",
       subtitle = paste(target_cty, collapse = ", "),
       caption  = "White = zero fossil-fuel jobs   •   Brown = more jobs") +
  theme_void()
ggsave("/Users/nikhilkuniyil/Documents/Winter25/PSTAT197B/capstone-project2035/maps_png/fossil_jobs_map.png", width = 8, height = 6, dpi = 300)
