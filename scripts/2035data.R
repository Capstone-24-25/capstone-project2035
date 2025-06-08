library(tidyverse)
library(readxl)

oews_national <- read_xlsx("~/Desktop/PSTAT 197/Projects/capstone-project2035/Data/occupational/oews_national.xlsx")



oews_alldata <- read_xlsx("~/Desktop/PSTAT 197/Projects/capstone-project2035/Data/occupational/oews_all_data_M_2023.xlsx")
# fossil fuel workers
oews_alldata <- oews_alldata %>%
  filter(
    startsWith(OCC_CODE, "47")
  )
# view individual extraction worker titles
oews_alldata %>% 
  count(
    OCC_TITLE
  ) %>% 
  View()
# narrow down fossil fuel workers
oews_alldata <- oews_alldata %>% 
  filter(
    OCC_TITLE %in% c("Construction and Extraction Occupations",
                     "Derrick Operators, Oil and Gas",
                     "Derrick, Rotary Drill, and Service Unit Operators, Oil and Gas",
                     "Earth Drillers, Except Oil and Gas",
                     "Extraction Workers",
                     "Extraction Workers, All Other",
                     "Helpers--Extraction Workers",
                     "Miscellaneous Extraction Workers",
                     "Rotary Drill Operators, Oil and Gas",
                     "Roustabouts, Oil and Gas",
                     "Service Unit Operators, Oil and Gas",
                     "Supervisors of Construction and Extraction Workers",
                     "Surface Mining Machine Operators and Earth Drillers"
                     )
  )



oews_state <- read_xlsx("~/Desktop/PSTAT 197/Projects/capstone-project2035/Data/occupational/oews_state.xlsx")
oews_state <- oews_state %>% 
  filter(
    AREA_TITLE == "California",
    startsWith(OCC_CODE, "47")
  )



qcew_county_qtr1 <- read_xlsx("~/Desktop/PSTAT 197/Projects/capstone-project2035/Data/occupational/2024_all_county_high_level/qcew_county_qtr1.xlsx")
qcew_county_qtr1 <- qcew_county_qtr1 %>% 
  filter(
    `St Name` == "California",
    Area %in% c("Ventura County, California", 
                "Santa Barbara County, California", 
                "San Luis Obispo County, California"),
    Industry %in% c("10 Total, all industries", 
                    "1011 Natural resources and mining", 
                    "1012 Construction")
    )


qcew_county_qtr2 <- read_xlsx("~/Desktop/PSTAT 197/Projects/capstone-project2035/Data/occupational/2024_all_county_high_level/qcew_county_qtr2.xlsx")
qcew_county_qtr2 <- qcew_county_qtr2 %>% 
  filter(
    `St Name` == "California",
    Area %in% c("Ventura County, California", 
                "Santa Barbara County, California", 
                "San Luis Obispo County, California"),
    Industry %in% c("10 Total, all industries", 
                    "1011 Natural resources and mining", 
                    "1012 Construction")
  )

fossil_fuel_jobs <- read.csv("~/Desktop/PSTAT 197/Projects/capstone-project2035/Data/CA_jobs_ff.csv")
green_jobs <- read.csv("~/Desktop/PSTAT 197/Projects/capstone-project2035/Data/CA_jobs_green.csv")





library(tidyverse)
library(readxl)
library(httr)

green_jobs <- c('47-2231', '49-9081', '49-9099', '47-4099', '47-1011', '41-4011', '47-2211', '49-9042', '51-9012', '51-8099', '51-8013', '51-8012', '51-8011', '51-4041', '19-4041', '19-4051', '17-2051', '17-2071', '17-2141', '17-2199', '11-3051', '11-3071', '11-9041', '11-9199')
ff_jobs <- c('47-5000', '47-5010', '47-5011', '47-5012', '47-5013', '47-5022', '47-5041', '47-5043', '47-5044', '47-5071', '47-5081')
counties_code <- c('42200', '37100', '42020')

# from up until 2000, data needs reformatting
#oews_2000_1 <- read_excel("~/Desktop/PSTAT 197/data/oes00ma/MSA_2000_dl_1.xls")
#oews_2000_2 <- read_excel("~/Desktop/PSTAT 197/data/oes00ma/MSA_2000_dl_2.xls")
#oews_2000 <- rbind(oews_2000_1, oews_2000_2)

oews_2001 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oes01ma/MSA_2001_dl_1.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oes01ma/MSA_2001_dl_2.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oes01ma/MSA_2001_dl_3.xls")
)
colnames(oews_2001)[which(names(oews_2001) == "occ_code")] <- "OCC_CODE"
colnames(oews_2001)[which(names(oews_2001) == "area")] <- "AREA"

oews_2002 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oes02ma/MSA_2002_dl_1.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oes02ma/MSA_2002_dl_2.xls")
)
colnames(oews_2002)[which(names(oews_2002) == "occ_code")] <- "OCC_CODE"
colnames(oews_2002)[which(names(oews_2002) == "area")] <- "AREA"

oews_2003 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesn03ma/MSA_november2003_dl_1.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oesn03ma/MSA_november2003_dl_2.xls")
)

oews_2004 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesn04ma/MSA_november2004_dl_1.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oesn04ma/MSA_november2004_dl_2.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oesn04ma/MSA_november2004_dl_3.xls")
)

oews_2005 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm05ma/MSA_may2005_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm05ma/MSA_may2005_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm05ma/MSA_may2005_dl_3.xls")
)

oews_2006 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm06ma/MSA_may2006_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm06ma/MSA_may2006_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm06ma/MSA_may2006_dl_3.xls")
)

oews_2007 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm07ma/MSA_May2007_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm07ma/MSA_May2007_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm07ma/MSA_May2007_dl_3.xls")
)

oews_2008 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm08ma/MSA__M2008_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm08ma/MSA_M2008_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm08ma/MSA_M2008_dl_3.xls")
)

oews_2009 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm09ma/MSA_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm09ma/MSA_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm09ma/MSA_dl_3.xls")
)

oews_2010 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm10ma/MSA_M2010_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm10ma/MSA_M2010_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm10ma/MSA_M2010_dl_3.xls")
)

oews_2011 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm11ma/MSA_M2011_dl_1_AK_IN.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm11ma/MSA_M2011_dl_2_KS_NY.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm11ma/MSA_M2011_dl_3_OH_WY.xls")
)

oews_2012 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm12ma/MSA_M2012_dl_1_AK_IN.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm12ma/MSA_M2012_dl_2_KS_NY.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm12ma/MSA_M2012_dl_3_OH_WY.xls")
)

oews_2013 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm13ma/MSA_M2013_dl_1_AK_IN.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm13ma/MSA_M2013_dl_2_KS_NY.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm13ma/MSA_M2013_dl_3_OH_WY.xls")
)

oews_2014 <- read_excel("~/Desktop/PSTAT 197/data/oesm14ma/MSA_M2014_dl.xlsx")

oews_2015 <- read_excel("~/Desktop/PSTAT 197/data/oesm15ma/MSA_M2015_dl.xlsx")

oews_2016 <- read_excel("~/Desktop/PSTAT 197/data/oesm16ma/MSA_M2016_dl.xlsx")

oews_2017 <- read_excel("~/Desktop/PSTAT 197/data/oesm17ma/MSA_M2017_dl.xlsx")

oews_2018 <- read_excel("~/Desktop/PSTAT 197/data/oesm18ma/MSA_M2018_dl.xlsx")

oews_2019 <- read_excel("~/Desktop/PSTAT 197/data/oesm19ma/MSA_M2019_dl.xlsx")
colnames(oews_2019)[which(names(oews_2019) == "occ_code")] <- "OCC_CODE"
colnames(oews_2019)[which(names(oews_2019) == "area")] <- "AREA"

oews_2020 <- read_excel("~/Desktop/PSTAT 197/data/oesm20ma/MSA_M2020_dl.xlsx")

oews_2021 <- read_excel("~/Desktop/PSTAT 197/data/oesm21ma/MSA_M2021_dl.xlsx")

oews_2022 <- read_excel("~/Desktop/PSTAT 197/data/oesm22ma/MSA_M2022_dl.xlsx")

oews_2023 <- read_excel("~/Desktop/PSTAT 197/data/oesm23ma/MSA_M2023_dl.xlsx")

oews_data <- list(oews_2001, oews_2002, oews_2003, oews_2004, oews_2005, oews_2006, oews_2007,
                  oews_2008, oews_2009, oews_2010, oews_2011, oews_2012, oews_2013, oews_2014, oews_2015,
                  oews_2016, oews_2017, oews_2018, oews_2019, oews_2020, oews_2021, oews_2022, oews_2023)

years <- 2001:2021

for(i in 1:23){
  oews_data[[i]] <- data.frame(oews_data[[i]])
  oews_data[[i]] <- oews_data[[i]][(oews_data[[i]]$OCC_CODE %in% green_jobs | oews_data[[i]]$OCC_CODE %in% ff_jobs) & (oews_data[[i]]$AREA %in% counties_code), ]
}

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
data$AREA_TITLE[data$AREA_TITLE == "Oxnard-Thousand Oaks-Ventura, CA"] <- "Ventura"
data$AREA_TITLE[data$AREA_TITLE == "Ventura, CA PMSA"] <- "Ventura"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Paso Robles, CA"] <- "San Luis Obispo"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Paso Robles-Arroyo Grande, CA"] <- "San Luis Obispo"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Atascadero-Paso Robles, CA M"] <- "San Luis Obispo"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Atascadero-Paso Robles, CA MSA"] <- "San Luis Obispo"
data$AREA_TITLE[data$AREA_TITLE == "San Luis Obispo-Paso Robles, CA"] <- "San Luis Obispo"
data$AREA_TITLE[data$AREA_TITLE == "Santa Barbara-Santa Maria-Lompoc, CA MSA"] <- "Santa Barbara"
data$AREA_TITLE[data$AREA_TITLE == "Santa Maria-Santa Barbara, CA"] <- "Santa Barbara"
data$AREA_TITLE[data$AREA_TITLE == "Santa Barbara-Santa Maria, CA"] <- "Santa Barbara"
data$AREA_TITLE[data$AREA_TITLE == "Santa Barbara-Santa Maria-Goleta, CA"] <- "Santa Barbara"
data$label[data$label == "ff job"] <- "Fossil Fuel"
data$label[data$label == "green job"] <- "Green"

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
            "machinery/operations/maintenance",
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
  labs(title = "Fossil Fuel Jobs Across Years") +
  theme_bw()
green_total_count %>% 
  ggplot(
    aes(x = year, y = jobs)
  ) +
  geom_line() +
  ylim(0, 25000) +
  labs(title = "Green Jobs Across Years") +
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
  labs(title = "Fossil Fuel Jobs by County") +
  theme_bw()
green_county_count %>%
  ggplot(
    aes(
      x = year, y = jobs, color = AREA_TITLE
    )
  ) +
  geom_line() +
  ylim(0, 12000) +
  labs(title = "Green Jobs by County") +
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
  labs(title = "Employment by Specific Fossil Fuel Job from 2001-2023") +
  theme_bw()

green_jobs %>% 
  ggplot(
    aes(
      x = year, y = jobs, color = occupation
    )
  ) +
  geom_line() +
  ylim(0, 8250) +
  labs(title = "Employment by Specific Green Job from 2001-2023") +
  theme_bw()



library(tidyverse)
library(readxl)

# data taken from https://www.ers.usda.gov/data-products/county-level-oil-and-gas-production-in-the-united-states
# from 2000-2011
# natural gas production measured by gross withdrawals
# oil production includes natural gas liquids
production_data <- read_xls("~/Downloads/oilgascounty.xls") %>% 
  filter(
    County_Name %in% c("Santa Barbara County", "Ventura County", "San Luis Obispo County")
  )

# county-level data

oil_data <- production_data %>% 
  select(
    c(County_Name, starts_with("oil2"))
  ) %>% 
  pivot_longer(
    cols = starts_with("oil2"),
    names_to = "Year",
    names_prefix = "oil",
    values_to = "Oil"
  ) %>% 
  mutate(
    Year = as.numeric(Year)
  )

gas_data <- production_data %>% 
  select(
    c(County_Name, starts_with("gas2"))
  ) %>% 
  pivot_longer(
    cols = starts_with("gas"),
    names_to = "Year",
    names_prefix = "gas",
    values_to = "Gas"
  ) %>% 
  mutate(
    Year = as.numeric(Year)
  )

oil_data %>% 
  ggplot(
    aes(
      x = Year, y = Oil, color = County_Name
    )
  ) +
  geom_line() +
  ylim(0, NA) +
  theme_bw() +
  ggtitle("County-Level Oil Production 2000-2011") +
  ylab("Oil (barrels)")
  
gas_data %>% 
  ggplot(
    aes(
      x = Year, y = Gas, color = County_Name
    )
  ) +
  geom_line() +
  ylim(0, NA) +
  theme_bw() +
  ggtitle("County-Level Gas Production 2000-2011") +
  ylab("Gas (1000 cubic ft)")

# county-combined data

oil_data %>% 
  group_by(Year) %>% 
  summarize(
    Oil = sum(Oil)
  ) %>% 
  ggplot(
    aes(
      x = Year, y = Oil
    )
  ) +
  geom_line() +
  ylim(0, NA) +
  theme_bw() +
  labs(title = "Overall Oil Production 2000-2011") +
  ylab("Oil (barrels)")

gas_data %>% 
  group_by(Year) %>% 
  summarize(
    Gas = sum(Gas)
  ) %>% 
  ggplot(
    aes(
      x = Year, y = Gas
    )
  ) +
  geom_line() +
  ylim(0, NA) +
  theme_bw() +
  labs(title = "Overall Gas Production 2000-2011") +
  ylab("Gas (1000 cubic ft)")















# Microdata
library(tidyverse)
library(scales)
microdata <- read.csv("Data/clean_microdata.csv")
microdata <- microdata %>% 
  mutate(occ_group = case_when(
    occupation %in% c(
      "Architectural And Engineering Managers", "Civil Engineers", "Electrical And Electronics Engineers", "Engineering Managers", "Mechanical Engineers", "Other Engineers, Including Nuclear Engineers", "Petroleum, Mining And Geological Engineers, Including Mining Safety Engineers"
    ) ~ "Engineering",
    occupation %in% c(
      "Electricians", "First-Line Supervisors Of Construction Trades And Extraction Workers", "First-Line Supervisors/Managers Of Construction Trades And Extraction Workers", "Other Construction And Related Workers", "Other Construction Workers, Including Solar Photovoltaic Installers", "Sheet Metal Workers", "Solar Photovoltaic Installers"
    ) ~ "Construction Trades",
    occupation %in% c(
      "Derrick, Rotary Drill, And Service Unit Operators, And Roustabouts, Oil And Gas", "Derrick, Rotary Drill, And Service Unit Operators, And Roustabouts, Oil, Gas, and Mining", "Earth Drillers, Except Oil and Gas", "Mining Machine Operators", "Surface Mining Machine Operators And Earth Drillers", "Underground Mining Machine Operators", "Other Extraction Workers", "Explosives Workers, Ordnance Handling Experts, And Blasters"
    ) ~ "Extraction, Oil/Gas",
    occupation %in% c(
      "Chemical Processing Machine Setters, Operators, And Tenders", "Crushing, Grinding, Polishing, Mixing, And Blending Workers", "Industrial And Refractory Machinery Mechanics", "Machinists", "Miscellaneous Plant And System Operators", "Power Plant Operators, Distributors, And Dispatchers", "Pumping Station Operators", "Conveyor, Dredge, And Hoist And Winch Operators"
    ) ~ "Machine Operators/Technicians",
    occupation %in% c(
      "Environmental Scientists and Geoscientists", "Geoscientists And Hydrologists, Except Geographers", "Environmental Science And Geoscience Technicians, And Nuclear Technicians", "Geological and Petroleum Technicians, and Nuclear Technicians", "Other Life, Physical, And Social Science Technicians"
    ) ~ "Environmental Sciences",
    occupation %in% c(
      "Industrial Production Managers", "Transportation, Storage, And Distribution Managers", "Other Managers", "Sales Representatives, Wholesale And Manufacturing", "Other Installation, Maintenance, And Repair Workers"
    ) ~ "Management & Sales",
    TRUE ~ "Uncategorized"
  )) %>% 
  mutate(
    age_group = ifelse(agep < 26, "16-25",
                       ifelse(agep < 36, "26-35",
                              ifelse(agep < 46, "36-45",
                                     ifelse(agep < 56, "46-55",
                                            ifelse(agep < 66, "56-65", "66+")))))
  )
microdata_counts <- microdata %>% 
  group_by(
    county, occupation, occ_type, year, wagp, agep, age_group, occ_group
  ) %>% 
  summarize(
    workers = sum(pwgtp)
  )

# (weighted) employment over time
## ff vs green
microdata_counts %>% 
  group_by(
    occ_type, year
  ) %>% 
  summarize(
    count = sum(workers)
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, colour = occ_type
    )
  ) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Count", title = "Occupation Type Counts (2005-2023)", colour = "Occupation Type") +
  ylim(0, 60000)

## county-wise
microdata_counts %>% 
  group_by(
    year, county
  ) %>% 
  summarize(
    count = sum(workers)
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, colour = county
    )
  ) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Count", title = "Employment by County (2005-2023)", colour = "County") +
  ylim(0, 40000)

## ff by county
microdata_counts %>% 
  group_by(
    occ_type, year, county
  ) %>% 
  summarize(
    count = sum(workers)
  ) %>% 
  filter(
    occ_type == "Fossil Fuel"
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, colour = county
    )
  ) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Count", title = "Fossil Fuel Workers by County (2005-2023)", colour = "County")

## green by county
microdata_counts %>% 
  group_by(
    occ_type, year, county
  ) %>% 
  summarize(
    count = sum(workers)
  ) %>% 
  filter(
    occ_type == "Green"
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, colour = county
    )
  ) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Count", title = "Green Workers by County (2005-2023)", colour = "County") +
  ylim(0, 40000)

## specific occupations
microdata_counts %>% 
  group_by(
    occ_group, year
  ) %>% 
  summarize(
    count = sum(workers)
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, colour = occ_group
    )
  ) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Count", title = "Employment by Specific Occupation (2005-2023)")

## overall age groups 
microdata_counts %>% 
  group_by(
    age_group, year
  ) %>% 
  summarize(
    count = sum(workers)
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, colour = age_group
    )
  ) +
  geom_line() + 
  theme_bw() +
  labs(x = "Year", y = "Count", title = "Employment by Age Group (2005-2023)", colour = "Age Group") +
  ylim(0, 20000)

## fossil fuel age groups
microdata_counts %>% 
  filter(
    occ_type == "Fossil Fuel"
  ) %>% 
  group_by(
    age_group, year
  ) %>% 
  summarize(
    count = sum(workers)
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, colour = age_group
    )
  ) +
  geom_line() + 
  theme_bw() +
  labs(x = "Year", y = "Count", title = "Fossil Fuel Employment by Age Group (2005-2023)", colour = "Age Group")

## green age groups
microdata_counts %>% 
  filter(
    occ_type == "Green"
  ) %>% 
  group_by(
    age_group, year
  ) %>% 
  summarize(
    count = sum(workers)
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, colour = age_group
    )
  ) +
  geom_line() + 
  theme_bw() +
  labs(x = "Year", y = "Count", title = "Green Employment by Age Group (2005-2023)", colour = "Age Group") +
  ylim(0, 17500)

# weighted wage
weighted_wage <- microdata %>% 
  filter(
    wagp != 0
  ) %>% 
  mutate(
    age_group = ifelse(agep < 26, "16-25",
                       ifelse(agep < 36, "26-35",
                              ifelse(agep < 46, "36-45",
                                     ifelse(agep < 56, "46-55",
                                            ifelse(agep < 66, "56-65", "66+")))))
  ) %>% 
  group_by(
    county, occupation, occ_type, year, pwgtp, age_group, occ_group
  ) %>% 
  summarize(
    weighted_avg = sum(wagp * pwgtp) / sum(pwgtp)
  )

## ff vs green
weighted_wage %>% 
  group_by(
    occ_type, year
  ) %>% 
  summarize(
    weighted_avg = mean(weighted_avg)
  ) %>% 
  ggplot(
    aes(
      x = year, y = weighted_avg, colour = occ_type
    )
  ) +
  geom_line() +
  theme_bw() + 
  labs(
    x = "Year", title = "Weighted Average Wage by Occupation Type (2005-2023)", colour = "Occupation Type"
  ) +
  scale_y_continuous(name = "Weighted Average($)", breaks = c(40000, 60000, 80000, 100000), labels = c("40000", "60000", "80000", "100000"))
  
  
## by county
weighted_wage %>% 
  group_by(
    county, year
  ) %>% 
  summarize(
    weighted_avg = mean(weighted_avg)
  ) %>% 
  ggplot(
    aes(
      x = year, y = weighted_avg, colour = county
    )
  ) +
  geom_line() +
  theme_bw() + 
  labs(
    x = "Year", title = "Weighted Average Wage by County (2005-2023)", colour = "County"
  ) +
  scale_y_continuous(name = "Weighted Average($)", breaks = c(60000, 70000, 80000, 90000, 100000), labels = c("60000", "70000", "80000", "90000", "100000"))


## by age group
weighted_wage %>% 
  group_by(
    age_group, year
  ) %>% 
  summarize(
    weighted_avg = mean(weighted_avg)
  ) %>% 
  ggplot(
    aes(
      x = year, y = weighted_avg, colour = age_group
    )
  ) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", title = "Weighted Average Wage by Age Group (2005-2023)", colour = "Age Group") +
  scale_y_continuous(name = "Weighted Average ($)", breaks = c(20000, 60000, 100000, 140000), labels = c("20000", "60000", "100000", "140000"))

## by specific occupation
weighted_wage %>% 
  group_by(
    occ_group, year
  ) %>% 
  summarize(
    weighted_avg = mean(weighted_avg)
  ) %>% 
  ggplot(
    aes(
      x = year, y = weighted_avg, colour = occ_group
    )
  ) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Weighted Average ($)", title = "Weighted Average Wage by Specific Occupation (2005-2023)", colour = "Occupation Group")
  
## highest earning occupations
# not the best bc very few of the highest paid occupations are included in the dataset for > 10 years
top_earners <- c("Architectural and Engineering Managers", "Petroleum, Mining And Geological Engineers, Including Mining Safety Engineers", "Power Plant Operators, Distributors, And Dispatchers", "Geological and Petroleum Technicians, and Nuclear Technicians", "Engineering Managers")
weighted_wage %>% 
  group_by(
    occupation, year
  ) %>% 
  summarize(
    weighted_avg = mean(weighted_avg)
  ) %>% 
  filter(
    occupation %in% top_earners
  ) %>% 
  ggplot(
    aes(
      x = year, y = weighted_avg, colour = occupation
    )
  ) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", title = "Weighted Average Wage of Top 5 Earning Jobs", colour = "Occupation")









# composite graphs
library(egg)
library(ggpubr)

## employment by county and occupation type
p1 <- microdata_counts %>% 
  filter(wagp > 0) %>%
  group_by(
    year, county, occ_type
  ) %>%
  summarize(
    count = sum(workers)
  ) %>% 
  ggplot(
    aes(x = year, y = count, color = occ_type)
  ) +
  geom_line() +
  labs(x = "Year", y = "Count", color = "Job Type") +
  facet_grid(~ county) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000))

## composite BLS
p3 <- data %>% 
  filter(
    year > 2004
  ) %>% 
  group_by(
    AREA_TITLE, year, label
  ) %>% 
  summarize(
    count = sum(TOT_EMP)
  ) %>% 
  ggplot(
    aes(
      x = year, y = count, color = label
    )
  ) +
  geom_line() +
  labs(x = "Year", y = "Count") +
  facet_grid(~ AREA_TITLE) +
  theme_bw(base_size = 12) +
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020), labels = c("2005", "2010", "2015", "2020"))





## mean wage by county and occupation type
weighted_wage %>% 
  group_by(
    year, county, occ_type
  ) %>%
  summarize(
    weighted_avg = mean(weighted_avg)
  ) %>% 
  ggplot(
    aes(x = year, y = weighted_avg, color = occ_type)
  ) +
  geom_line() +
  labs(x = "Year", y = "Mean Wage ($)", color = "Job Type") +
  facet_grid(~ county) +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.5, 0.8))

ggarrange(
  p1 + rremove("xlab") + rremove("ylab"),
  p3 + rremove("ylab"), 
  nrow = 2,
  legend = "bottom",
  common.legend = T)

p3 + theme(legend.position = c(0.5, 0.8)) + labs(color = "Job Type")

