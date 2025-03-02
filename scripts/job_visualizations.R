library(tidyverse)
library(readxl)
library(httr)

## correct year variable, turn TOT_EMP into numeric variable
data <- read.csv("./Data/MSA_ALL_YEARS.csv") %>% 
  mutate(
    year = year + 2000,
    TOT_EMP = as.numeric(TOT_EMP)
  )
## correct county names 
data$AREA_TITLE[data$AREA_TITLE == "Oxnard-Thousand Oaks-Ventura, CA"] <- "ventura"
data$AREA_TITLE[data$AREA_TITLE == "Ventura, CA PMSA"] <- "ventura"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Paso Robles, CA"] <- "san luis obispo"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Paso Robles-Arroyo Grande, CA"] <- "san luis obispo"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Atascadero-Paso Robles, CA M"] <- "san luis obispo"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Atascadero-Paso Robles, CA MSA"] <- "san luis obispo"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Paso Robles, CA"] <- "san luis obispo"
data$AREA_TITLE[data$AREA_TITLE == "Santa Barbara-Santa Maria-Lompoc, CA MSA"] <- "santa barbara"
data$AREA_TITLE[data$AREA_TITLE == "Santa Maria-Santa Barbara, CA"] <- "santa barbara"
data$AREA_TITLE[data$AREA_TITLE == "Santa Barbara-Santa Maria, CA"] <- "santa barbara"
data$AREA_TITLE[data$AREA_TITLE == "Santa Barbara-Santa Maria-Goleta, CA"] <- "santa barbara"

## standardize occupation titles
data$OCC_TITLE <- tolower(data$OCC_TITLE)

## impute missing TOT_EMP values with the median value for that specific county and job type
temp_county = NA
temp_label = NA
for (i in 1:nrow(data)){
  if (is.na(data[i, ]$TOT_EMP)){
    temp_county = data[i, ]$AREA_TITLE
    temp_label = data[i, ]$label
    data[i, ]$TOT_EMP = median(data$TOT_EMP[data$AREA_TITLE == temp_county & data$label == temp_label], na.rm = T)
  }
}

## create dfs of total ff/green job counts across counties
ff_total_count <- data %>% 
  filter(label == "ff job") %>% 
  group_by(year) %>% 
  summarize(jobs = sum(TOT_EMP))
green_total_count <- data %>% 
  filter(label == "green job") %>% 
  group_by(year) %>% 
  summarize(jobs = sum(TOT_EMP))

## ff/green job counts by county
ff_county_count <- data %>% 
  filter(
    label == "ff job"
  ) %>% 
  group_by(
    year, AREA_TITLE
  ) %>% 
  summarize(
    jobs = sum(TOT_EMP)
  )
green_county_count <- data %>% 
  filter(
    label == "green job"
  ) %>% 
  group_by(
    year, AREA_TITLE
  ) %>% 
  summarize(
    jobs = sum(TOT_EMP)
  )

## creating specific occupation employment dfs
ff_jobs <- data %>% 
  filter(label == "ff job") %>% 
  mutate(
    occupation = if_else(
      OCC_TITLE %in% c("derrick operators, oil and gas", "rotary drill operators, oil and gas", "service unit operators, oil and gas", "service unit operators, oil, gas, and mining"),
      "extraction unit operators",
      if_else(
        OCC_TITLE %in% c("earth drillers, except oil and gas", "earth drillers, except oil and gas; and explosive workers, ordnance handling experts, and blasters"),
        "earth drillers",
        if_else(
          OCC_TITLE %in% c("helpers--extraction workers", "roustabouts, oil and gas"),
          "extraction helpers",
          "surface miners")
      )
    )
  ) %>% 
  group_by(year, occupation) %>%
  summarize(jobs = sum(TOT_EMP))

green_jobs_list <- list(
  c("architectural and engineering managers", "engineering managers", "industrial production managers", "managers, all other", "transportation, storage, and distribution managers"),
  c("civil engineers", "electrical engineers", "mechanical engineers", "geological and petroleum technicians", "engineers, all other"),
  c("construction and related workers, all other", "first-line supervisors of construction trades and extraction workers", "first-line supervisors/managers of construction trades and extraction workers", "sheet metal workers", "solar photovoltaic installers"),
  c("installation, maintenance, and repair workers, all other", "machinists", "maintenance and repair workers, general", "plant and system operators, all other", "power plant operators", "power distributors and dispatchers", "separating, filtering, clarifying, precipitating, and still machine setters, operators, and tenders"),
  c("sales representatives, wholesale and manufacturing, technical and scientific products")
)
green_jobs <- data %>%
  filter(label == "green job") %>%
  mutate(
    occupation = ifelse(
      OCC_TITLE %in% green_jobs_list[[1]],
      "managers",
      ifelse(
        OCC_TITLE %in% green_jobs_list[[2]],
        "engineers/technicians",
        ifelse(
          OCC_TITLE %in% green_jobs_list[[3]],
          "construction/skilled trades",
          ifelse(
            OCC_TITLE %in% green_jobs_list[[4]],
            "operations/maintenance",
            "sales"
          )
        )
      )
    )
  ) %>% 
  group_by(year, occupation) %>% 
  summarize(jobs = sum(TOT_EMP))

# Visualizations

## jobs across all counties
ff_total_count %>% 
  ggplot(
    aes(x = year, y = jobs)
  ) +
  geom_line() +
  ylim(0, 600) +
  labs(title = "Fossil Fuel Jobs on Central Coast 2001-2023", x = "Year", y = "Jobs") +
  theme_bw()
green_total_count %>% 
  ggplot(
    aes(x = year, y = jobs)
  ) +
  geom_line() +
  ylim(0, 25000) +
  labs(title = "Green Jobs on Central Coast 2001-2023", x = "Year", y = "Jobs") +
  theme_bw()

## jobs by county
ff_county_count %>%
  ggplot(
    aes(
      x = year, y = jobs, color = AREA_TITLE
    )
  ) +
  geom_line() +
  ylim(0, 425) +
  labs(title = "Fossil Fuel Jobs by County 2001-2023", x = "Year", y = "Jobs", color = "County") +
  theme_bw()
green_county_count %>%
  ggplot(
    aes(
      x = year, y = jobs, color = AREA_TITLE
    )
  ) +
  geom_line() +
  ylim(0, 12000) +
  labs(title = "Green Jobs by County 2001-2023", x = "Year", y = "Jobs", color = "County") +
  theme_bw()

## specific jobs across years
ff_jobs %>% 
  ggplot(
    aes(
      x = year, y = jobs, color = occupation
    )
  ) +
  geom_line() +
  ylim(0, 425) +
  labs(title = "Employment by Fossil Fuel Occupation 2001-2023", x = "Year", y = "Jobs", color = "Occupation") +
  theme_bw()

green_jobs %>% 
  ggplot(
    aes(
      x = year, y = jobs, color = occupation
    )
  ) +
  geom_line() +
  ylim(0, 8250) +
  labs(title = "Employment by Green Occupation 2001-2023", x = "Year", y = "Jobs", color = "Occupation") +
  theme_bw()
