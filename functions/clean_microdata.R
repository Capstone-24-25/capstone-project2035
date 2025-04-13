library(tidyverse)
library(janitor)

# Load the data
puma_codes <- list(
  "11103" = "Ventura County (Southwest)--Oxnard & Port Hueneme Cities PUMA",
  "11102" = "Ventura County (Southeast)--Thousand Oaks City PUMA",
  "11107" = "Ventura County (North)--Santa Paula, Fillmore & Ojai Cities PUMA",
  "11108" = "Ventura County (Southeast)--Simi Valley City PUMA",
  "11104" = "Ventura County (Southwest)--San Buenaventura (Ventura) City PUMA",
  "11109" = "Ventura County (South Central)--Camarillo City & Moorpark City (Southwest) PUMA",
  "08301" = "Santa Barbara County (Northwest)--Santa Maria City & Orcutt PUMA",
  "08302" = "Santa Barbara County (North)--Lompoc, Guadalupe, Solvang & Buellton Cities PUMA",
  "08303" = "Santa Barbara County--South Coast Region PUMA",
  "07901" = "San Luis Obispo County (West)--Coastal Region PUMA",
  "07902" = "San Luis Obispo County (East)--Inland Region PUMA"
)

#2016-2023 data files
soc_2023 <- list(
  "113051" = "Industrial Production Managers",
  "113071" = "Transportation, Storage, And Distribution Managers",
  "119041" = "Architectural And Engineering Managers",
  "1191XX" = "Other Managers",
  "172051" = "Civil Engineers",
"172070" = 'Electrical And Electronics Engineers',
'172141' = 'Mechanical Engineers',
'1721XX' = 'Petroleum, Mining And Geological Engineers, Including Mining Safety Engineers',
'1721YY' = 'Other Engineers',
'19204X' = 'Geoscientists And Hydrologists, Except Geographers',
'1940XX' = 'Environmental Science And Geoscience Technicians, And Nuclear Technicians',
'1940YY' = 'Other Life, Physical, And Social Science Technicians',
'414010' = 'Sales Representatives, Wholesale And Manufacturing',
'471011' = 'First-Line Supervisors Of Construction Trades And Extraction Workers',
'472211' = 'Sheet Metal Workers',
'472231' = 'Solar Photovoltaic Installers',
'4740XX' = 'Other Construction And Related Workers',
'475020' = 'Surface Mining Machine Operators And Earth Drillers',
'475032' = 'Explosives Workers, Ordnance Handling Experts, And Blasters',
'475040' = 'Underground Mining Machine Operators',
'4750XX' = 'Other Extraction Workers',
'4750YY' = 'Derrick, Rotary Drill, And Service Unit Operators, And Roustabouts, Oil And Gas',
'49904X' = 'Industrial And Refractory Machinery Mechanics',
'4990XX' = 'Other Installation, Maintenance, And Repair Workers',
'514041' = 'Machinists',
'518010' = 'Power Plant Operators, Distributors, And Dispatchers',
'518090' = 'Miscellaneous Plant And System Operators',
'519010' = 'Chemical Processing Machine Setters, Operators, And Tenders',
'519020' = 'Crushing, Grinding, Polishing, Mixing, And Blending Workers'
)
green_jobs <- list("472231", "4990XX", "4740XX", "471011", "414010", "472211", "519010", "518090", "518010", "514041", "1940XX", "172070","172051", "1721XX", "113051", "113071", "119041", "1191XX")

data <- data.frame(pwgtp = numeric(), 
                   puma = character(), 
                   region = character(), 
                   county = character(), 
                   socp = character(), 
                   occupation = character(), 
                   occ_type = character(), 
                   year = numeric(), 
                   agep = numeric(),
                   wagp = numeric())
for (year in 2016:2023) {
  #read in files
  file <- read.csv(paste0("Data/acs_microdata/ACSPUMS5Y", year, ".csv"))
  clean_file <- file %>%
    clean_names() %>%
    select(-any_of(c("state", "x7", "st"))) %>% 
    rename(puma = any_of("puma20")) %>% 
    mutate(
      puma = as.character(puma),
      puma = str_pad(puma, width = 5, side = "left", pad = "0"),
      puma = str_trim(puma)) %>% 
    filter(puma %in% names(puma_codes)) %>%
    mutate(
      region = recode(puma, !!!puma_codes), 
      county = str_extract(region, "([A-Za-z ]+?) County") %>% 
        str_remove_all(" County") %>% 
        str_trim(),
      occ_type = ifelse(socp %in% green_jobs, "Green", "Fossil Fuel"),
      occupation = recode(socp, !!!soc_names) ,
      year = year
    )
  #bind to data
  data <- bind_rows(data, clean_file)
}

data <- data %>% 
  select(-x)

# -2015 data files 
data <- read.csv("Data/acs_microdata/ACSPUMS1Y2013.csv")
unique(data$SOCP)

###
library(tidycensus)
data(pums_variables)
pums_vars_2011 <- pums_variables %>% 
  filter(year == 2011, survey == "acs1")
pums_vars_2011 %>% 
  distinct(var_code, var_label, data_type, level)
