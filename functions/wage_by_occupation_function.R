#read in ACS data for S2411:
# Median Earnings in the Past 12 Months (in 2023 Inflation-Adjusted Dollars) for the Civilian Employed Population 16 Years and Over by Occupation
# link for download: https://data.census.gov/table/ACSST1Y2023.S2411?q=wage+california&g=860XX00US91320,91360,91361,91362,91377,93001,93003,93004,93010,93012,93013,93015,93021,93022,93023,93030,93033,93035,93036,93040,93041,93042,93043,93060,93063,93064,93065,93066,93101,93103,93105,93106,93108,93109,93110,93111,93117,93254,93401,93402,93405,93407,93408,93409,93410,93420,93422,93424,93426,93427,93428,93429,93430,93432,93433,93434,93435,93436,93437,93440,93441,93442,93444,93445,93446,93447,93449,93451,93452,93453,93454,93455,93458,93460,93461,93463,93465
library(tidyverse)
library(janitor)

#create empty dataframe
median_earning <- data.frame(zipcode = character(), year = numeric(), occupation = character(), estimate = numeric(), margin_of_error = numeric())

#create a function to read in the data
old_acs_wage <- function(file, df){
  last_18 <- substr(file_name, nchar(file_name) - 18, nchar(file_name))
  year <- as.numeric(str_extract(last_18, "\\d{4}"))
  data <- read.csv(file, header = TRUE, skip = 1) %>% 
    mutate(year = year)%>% 
    rename(zipcode = Geographic.Area.Name) %>% 
    mutate(zipcode = gsub("ZCTA5 ", "", zipcode)) %>% 
    select(-starts_with("Women"), -starts_with("Median.earnings..dollars..for.female"), 
           -starts_with("Median.earnings..dollars..for.male")) %>% 
    clean_names()
  data <- data[-1,]
  pivoted <- data %>%
    pivot_longer(
      cols = starts_with("median_earnings"), 
      names_to = "occupation",
      values_to = "earnings"
    ) %>% 
    mutate(measure = str_extract(occupation, "estimate|margin_of_error"),
           occupation = str_remove(occupation, "median_earnings_dollars_estimate_|median_earnings_dollars_margin_of_error_"))
  cleaned <- pivoted %>%
    pivot_wider(
      names_from = measure,
      values_from = earnings
    ) %>% 
    select(-x, - geography) %>% 
    filter(!occupation %in% c("management_business_science_and_arts_occupations","service_occupations", "sales_and_office_occupations", "natural_resources_construction_and_maintenance_occupations", "production_transportation_and_material_moving_occupations")) %>%
    mutate(occupation = str_remove(occupation, "management_business_science_and_arts_occupations_|service_occupations_|sales_and_office_occupations_|natural_resources_construction_and_maintenance_occupations_|production_transportation_and_material_moving_occupations_"),
           occupation = str_remove(occupation, "management_business_and_financial_occupations_|computer_engineering_and_science_occupations_|education_legal_community_service_arts_and_media_occupations_|healthcare_practitioner_and_technical_occupations_|protective_service_occupations_")) %>% 
    filter(!occupation %in% c("management_business_and_financial_occupations", "computer_engineering_and_science_occupations", "education_legal_community_service_arts_and_media_occupations", "healthcare_practitioner_and_technical_occupations", "protective_service_occupations"))
  merged <- rbind(df, cleaned)
  return(merged)
}

#newer data a little different
new_acs_wage <- function(file, df){
  last_18 <- substr(file_name, nchar(file_name) - 18, nchar(file_name))
  year <- as.numeric(str_extract(last_18, "\\d{4}"))
  data <- read.csv(file, header = TRUE, skip = 1) %>% 
    mutate(year = year)%>% 
    rename(zipcode = Geographic.Area.Name) %>% 
    mutate(zipcode = gsub("ZCTA5 ", "", zipcode)) %>% 
    select(-contains("women"), -contains("male")) %>% 
    clean_names()
  data <- data[-1,]
  pivoted <- data %>%
    pivot_longer(
      cols = contains("median_earnings"), 
      names_to = "occupation",
      values_to = "earnings"
    ) %>% 
    mutate(measure = str_extract(occupation, "estimate|margin_of_error"),
           occupation = str_remove(occupation, "estimate_median_earnings_dollars_|margin_of_error_median_earnings_dollars_moe_|margin_of_error_median_earnings_dollars_"),
           occupation = str_remove(occupation, "civilian_employed_population_16_years_and_over_with_earnings_"))
  cleaned <- pivoted %>%
    pivot_wider(
      names_from = measure,
      values_from = earnings
    ) %>% 
    select(-x, - geography) %>% 
    filter(!occupation %in% c("management_business_science_and_arts_occupations","service_occupations", "sales_and_office_occupations", "natural_resources_construction_and_maintenance_occupations", "production_transportation_and_material_moving_occupations")) %>%
    mutate(occupation = str_remove(occupation, "management_business_science_and_arts_occupations_|service_occupations_|sales_and_office_occupations_|natural_resources_construction_and_maintenance_occupations_|production_transportation_and_material_moving_occupations_"),
           occupation = str_remove(occupation, "management_business_and_financial_occupations_|computer_engineering_and_science_occupations_|education_legal_community_service_arts_and_media_occupations_|healthcare_practitioner_and_technical_occupations_|protective_service_occupations_|healthcare_practitioners_and_technical_occupations_")) %>% 
    filter(!occupation %in% c("management_business_and_financial_occupations", "computer_engineering_and_science_occupations", "education_legal_community_service_arts_and_media_occupations", "healthcare_practitioner_and_technical_occupations", "protective_service_occupations", "healthcare_practitioners_and_technical_occupations"))
  merged <- rbind(df, cleaned)
  return(merged)
}


for (year in 2015:2016) {
  #read in files
  file_name <- paste0("productDownload_2025-02-24T194809/ACSST5Y", year, ".S2411-Data.csv")
  #call function
  median_earning <- old_acs_wage(file_name, median_earning)
}

for (year in 2017:2023) {
  #read in files
  file_name <- paste0("productDownload_2025-02-24T194809/ACSST5Y", year, ".S2411-Data.csv")
  #call function
  median_earning <- new_acs_wage(file_name, median_earning)
}

#add county variable
vent_zip <- c(91320, 91360, 91361, 91362, 91377, 93001, 93003, 93004, 93010, 93012, 93015, 93021,
            93022, 93023, 93030, 93033, 93035, 93036, 93040, 93041, 93042, 93043, 93060, 93063,
            93064, 93065, 93066)
sb_zip <- c(93013, 93067, 93101, 93103, 93105, 93106, 93108, 93109, 93110, 93111, 93117,
            93254, 93427, 93429, 93434, 93436, 93437, 93440, 93441, 93454, 93455, 93458,
            93460, 93463)
slo_zip <- c(93401,93402,93405,93407,93409,93410,93420,93422,93424,93426,93428,93430,93408,
             93432,93433,93442,93444,93445,93446,93449,93451,93452,93453,93454,93461,93465
)
median_earning <- median_earning %>% 
  mutate(zipcode = as.factor(zipcode),
         county = ifelse(zipcode %in% vent_zip, "Ventura", 
                         ifelse(zipcode %in% sb_zip, "Santa Barbara", 
                                ifelse(zipcode %in% slo_zip, "San Luis Obispo", NA))),
         estimate = as.numeric(estimate),
         margin_of_error = as.numeric(margin_of_error))

#add column for green/ff job
# occupation grouping found at: https://www.bls.gov/oes/current/oes_stru.htm
green <- list("installation_maintenance_and_repair_occupations",
              "sales_and_related_occupations",
              "production_occupations", 
              "life_physical_and_social_science_occupations",
              "architecture_and_engineering_occupations",
              "management_occupations")
data <- median_earning %>% 
  mutate(job_type = ifelse(occupation == "construction_and_extraction_occupations", "Fossil Fuel", 
                           ifelse(occupation %in% green, "Green", "Other")))

write.csv(data, "../Data/median_yearly_earning.csv", row.names = FALSE)


