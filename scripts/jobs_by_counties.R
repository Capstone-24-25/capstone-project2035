green_jobs <- list('47-2231', '49-9081', '49-9099', '47-4099', '47-1011', '41-4011', '47-2211', '49-9042', '51-9012', '51-8099', '51-8013', '51-8012', '51-8011', '51-4041', '19-4041', '19-4051', '17-2051', '17-2071', '17-2141', '17-2199', '11-3051', '11-3071', '11-9041', '11-9199')

library("readxl")
library("dplyr")
library("tidyverse")

# Using the jobs_by_counties function from /functions/jobs_by_counties_function.R

# To use this function, add the MSA xlsx files into a folder labeled "MSA_year_raw"
# in your working directory

# Each MSA file should be labeled "MSA_M20**_dl.xlsx" by default, where ** is the 
# 2 last digits of the year

# The jobs argument is a list of green identified jobs
# Green jobs is a list of the OCC codes identified in the paper

# This function is using the classification of any OCC code starting with "47-5"
# as a fossil fuel job

jobs_23 <- jobs_by_counties(23, jobs = green_jobs)
jobs_22 <- jobs_by_counties(22, jobs = green_jobs)
jobs_21 <- jobs_by_counties(21, jobs = green_jobs)
jobs_20 <- jobs_by_counties(20, jobs = green_jobs)
jobs_19 <- jobs_by_counties(19 ,jobs = green_jobs)
jobs_18 <- jobs_by_counties(18, jobs = green_jobs)
jobs_17 <- jobs_by_counties(17, jobs = green_jobs)
jobs_16 <- jobs_by_counties(16, jobs = green_jobs)
jobs_15 <- jobs_by_counties(15, jobs = green_jobs)
jobs_14 <- jobs_by_counties(14, counties = c('42020', '37100', '42060'), jobs = green_jobs)

# 2013 and before BLS uses xls files instead of xlsx
jobs_13 <- jobs_by_counties(13, counties = c('42020', '37100', '42060'), jobs = green_jobs, xlsx = FALSE)
jobs_12 <- jobs_by_counties(12, counties = c('42020', '37100', '42060'),jobs = green_jobs, xlsx = FALSE)
jobs_11 <- jobs_by_counties(11, counties = c('42020', '37100', '42060'),jobs = green_jobs, xlsx = FALSE)
jobs_10 <- jobs_by_counties(10, counties = c('42020', '37100', '42060'),jobs = green_jobs, xlsx = FALSE)
jobs_09 <- jobs_by_counties('09',counties = c('42020', '37100', '42060'),jobs = green_jobs, xlsx = FALSE)
jobs_08 <- jobs_by_counties('08', counties = c('42020', '37100', '42060'),jobs = green_jobs, xlsx = FALSE)
jobs_07 <- jobs_by_counties('07', counties = c('42020', '37100', '42060'),jobs = green_jobs, xlsx = FALSE)
jobs_06 <- jobs_by_counties('06', counties = c('42020', '37100', '42060'),jobs = green_jobs, xlsx = FALSE)
jobs_05 <- jobs_by_counties('05', counties = c('42020', '37100', '42060'),jobs = green_jobs, xlsx = FALSE)

# 2004 and before use different county codes
jobs_04 <- jobs_by_counties('04', counties = c('7460', '7480', '8735'),jobs = green_jobs, xlsx = FALSE)
jobs_03 <- jobs_by_counties('03', counties = c('7460', '7480', '8735'), jobs = green_jobs, xlsx = FALSE)
jobs_02 <- jobs_by_counties('02', counties = c('7460', '7480', '8735'), jobs = green_jobs, xlsx = FALSE)
jobs_01 <- jobs_by_counties('01', counties = c('7460', '7480', '8735'), jobs = green_jobs, xlsx = FALSE)


# Now we must go through all the data and change column names and county codes/names to format it correctly
# Combinging 2022 and 2023 data
setdiff(union(colnames(jobs_22), colnames(jobs_23)), intersect(colnames(jobs_22), colnames(jobs_23)))
total_data <- rbind(jobs_22, jobs_23)

# Adding 2021 data
setdiff(union(colnames(jobs_21), colnames(total_data)), intersect(colnames(jobs_21), colnames(total_data)))
total_data <- rbind(jobs_21, total_data)

# Adding 2020 data
setdiff(union(colnames(jobs_20), colnames(total_data)), intersect(colnames(jobs_20), colnames(total_data)))
jobs_20$PCT_RPT <- NA
total_data <- rbind(jobs_20, total_data)

# Adding 2019 data
setdiff(union(colnames(jobs_19), colnames(total_data)), intersect(colnames(jobs_19), colnames(total_data)))
jobs_19$PRIM_STATE <- "CA"
jobs_19$PCT_RPT <- NA
total_data <- rbind(jobs_19, total_data)

# Adding 2018 data
setdiff(union(colnames(jobs_18), colnames(total_data)), intersect(colnames(jobs_18), colnames(total_data)))
jobs_18 <- jobs_18 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "OCC_GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_18$NAICS <- NA
jobs_18$NAICS_TITLE <- NA
jobs_18$I_GROUP <- NA
jobs_18$AREA_TYPE <- NA
jobs_18$OWN_CODE <- NA
jobs_18$PCT_TOTAL <- NA
jobs_18$PCT_RPT <- NA
total_data <- rbind(jobs_18, total_data)

# Adding 2017 data
setdiff(union(colnames(jobs_17), colnames(total_data)), intersect(colnames(jobs_17), colnames(total_data)))
jobs_17 <- jobs_17 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "OCC_GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_17$NAICS <- NA
jobs_17$NAICS_TITLE <- NA
jobs_17$I_GROUP <- NA
jobs_17$AREA_TYPE <- NA
jobs_17$OWN_CODE <- NA
jobs_17$PCT_TOTAL <- NA
jobs_17$PCT_RPT <- NA
total_data <- rbind(jobs_17, total_data)

# Adding 2016 data
setdiff(union(colnames(jobs_16), colnames(total_data)), intersect(colnames(jobs_16), colnames(total_data)))
jobs_16 <- jobs_16 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "OCC_GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_16$NAICS <- NA
jobs_16$NAICS_TITLE <- NA
jobs_16$I_GROUP <- NA
jobs_16$AREA_TYPE <- NA
jobs_16$OWN_CODE <- NA
jobs_16$PCT_TOTAL <- NA
jobs_16$PCT_RPT <- NA
total_data <- rbind(jobs_16, total_data)

# Adding 2015 data
setdiff(union(colnames(jobs_15), colnames(total_data)), intersect(colnames(jobs_15), colnames(total_data)))
jobs_15 <- jobs_15 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "OCC_GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_15$NAICS <- NA
jobs_15$NAICS_TITLE <- NA
jobs_15$I_GROUP <- NA
jobs_15$AREA_TYPE <- NA
jobs_15$OWN_CODE <- NA
jobs_15$PCT_TOTAL <- NA
jobs_15$PCT_RPT <- NA
total_data <- rbind(jobs_15, total_data)

# Adding 2014 data
setdiff(union(colnames(jobs_14), colnames(total_data)), intersect(colnames(jobs_14), colnames(total_data)))
jobs_14 <- jobs_14 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "OCC_GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_14$NAICS <- NA
jobs_14$NAICS_TITLE <- NA
jobs_14$I_GROUP <- NA
jobs_14$AREA_TYPE <- NA
jobs_14$OWN_CODE <- NA
jobs_14$PCT_TOTAL <- NA
jobs_14$PCT_RPT <- NA
total_data <- rbind(jobs_14, total_data)

# Adding 2013 data
setdiff(union(colnames(jobs_13), colnames(total_data)), intersect(colnames(jobs_13), colnames(total_data)))
jobs_13 <- jobs_13 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "OCC_GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_13$NAICS <- NA
jobs_13$NAICS_TITLE <- NA
jobs_13$I_GROUP <- NA
jobs_13$AREA_TYPE <- NA
jobs_13$OWN_CODE <- NA
jobs_13$PCT_TOTAL <- NA
jobs_13$PCT_RPT <- NA
total_data <- rbind(jobs_13, total_data)

# Adding 2012 data
setdiff(union(colnames(jobs_12), colnames(total_data)), intersect(colnames(jobs_12), colnames(total_data)))
jobs_12 <- jobs_12 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "OCC_GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_12$NAICS <- NA
jobs_12$NAICS_TITLE <- NA
jobs_12$I_GROUP <- NA
jobs_12$AREA_TYPE <- NA
jobs_12$OWN_CODE <- NA
jobs_12$PCT_TOTAL <- NA
jobs_12$PCT_RPT <- NA
total_data <- rbind(jobs_12, total_data)

# Adding 2011 data
setdiff(union(colnames(jobs_11), colnames(total_data)), intersect(colnames(jobs_11), colnames(total_data)))
jobs_11 <- jobs_11 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_11$NAICS <- NA
jobs_11$NAICS_TITLE <- NA
jobs_11$I_GROUP <- NA
jobs_11$AREA_TYPE <- NA
jobs_11$OWN_CODE <- NA
jobs_11$PCT_TOTAL <- NA
jobs_11$PCT_RPT <- NA
total_data <- rbind(jobs_11, total_data)

# Adding 2010 data
setdiff(union(colnames(jobs_10), colnames(total_data)), intersect(colnames(jobs_10), colnames(total_data)))
jobs_10 <- jobs_10 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP",
         "LOC_QUOTIENT" = "LOC QUOTIENT")
jobs_10$NAICS <- NA
jobs_10$NAICS_TITLE <- NA
jobs_10$I_GROUP <- NA
jobs_10$AREA_TYPE <- NA
jobs_10$OWN_CODE <- NA
jobs_10$PCT_TOTAL <- NA
jobs_10$PCT_RPT <- NA
total_data <- rbind(jobs_10, total_data)

# Adding 2009 data
setdiff(union(colnames(jobs_09), colnames(total_data)), intersect(colnames(jobs_09), colnames(total_data)))
jobs_09 <- jobs_09 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP")
jobs_09$NAICS <- NA
jobs_09$NAICS_TITLE <- NA
jobs_09$I_GROUP <- NA
jobs_09$AREA_TYPE <- NA
jobs_09$OWN_CODE <- NA
jobs_09$PCT_TOTAL <- NA
jobs_09$PCT_RPT <- NA
jobs_09$LOC_QUOTIENT <- NA
total_data <- rbind(jobs_09, total_data)

# Adding 2008 data
setdiff(union(colnames(jobs_08), colnames(total_data)), intersect(colnames(jobs_08), colnames(total_data)))
jobs_08 <- jobs_08 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP")
jobs_08$NAICS <- NA
jobs_08$NAICS_TITLE <- NA
jobs_08$I_GROUP <- NA
jobs_08$AREA_TYPE <- NA
jobs_08$OWN_CODE <- NA
jobs_08$PCT_TOTAL <- NA
jobs_08$PCT_RPT <- NA
jobs_08$LOC_QUOTIENT <- NA
jobs_08$JOBS_1000 <- NA
total_data <- rbind(jobs_08, total_data)

# Adding 2007 data
setdiff(union(colnames(jobs_07), colnames(total_data)), intersect(colnames(jobs_07), colnames(total_data)))
jobs_07 <- jobs_07 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP")
jobs_07$NAICS <- NA
jobs_07$NAICS_TITLE <- NA
jobs_07$I_GROUP <- NA
jobs_07$AREA_TYPE <- NA
jobs_07$OWN_CODE <- NA
jobs_07$PCT_TOTAL <- NA
jobs_07$PCT_RPT <- NA
jobs_07$LOC_QUOTIENT <- NA
jobs_07$JOBS_1000 <- NA
total_data <- rbind(jobs_07, total_data)

# Adding 2006 data
setdiff(union(colnames(jobs_06), colnames(total_data)), intersect(colnames(jobs_06), colnames(total_data)))
jobs_06 <- jobs_06 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP")
jobs_06$NAICS <- NA
jobs_06$NAICS_TITLE <- NA
jobs_06$I_GROUP <- NA
jobs_06$AREA_TYPE <- NA
jobs_06$OWN_CODE <- NA
jobs_06$PCT_TOTAL <- NA
jobs_06$PCT_RPT <- NA
jobs_06$LOC_QUOTIENT <- NA
jobs_06$JOBS_1000 <- NA
total_data <- rbind(jobs_06, total_data)

# Adding 2005 data
setdiff(union(colnames(jobs_05), colnames(total_data)), intersect(colnames(jobs_05), colnames(total_data)))
jobs_05 <- jobs_05 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP")
jobs_05$NAICS <- NA
jobs_05$NAICS_TITLE <- NA
jobs_05$I_GROUP <- NA
jobs_05$AREA_TYPE <- NA
jobs_05$OWN_CODE <- NA
jobs_05$PCT_TOTAL <- NA
jobs_05$PCT_RPT <- NA
jobs_05$LOC_QUOTIENT <- NA
jobs_05$JOBS_1000 <- NA
total_data <- rbind(jobs_05, total_data)

# Adding 2004 data
setdiff(union(colnames(jobs_04), colnames(total_data)), intersect(colnames(jobs_04), colnames(total_data)))
jobs_04 <- jobs_04 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP")
jobs_04$NAICS <- NA
jobs_04$NAICS_TITLE <- NA
jobs_04$I_GROUP <- NA
jobs_04$AREA_TYPE <- NA
jobs_04$OWN_CODE <- NA
jobs_04$PCT_TOTAL <- NA
jobs_04$PCT_RPT <- NA
jobs_04$LOC_QUOTIENT <- NA
jobs_04$JOBS_1000 <- NA
total_data <- rbind(jobs_04, total_data)

# Adding 2003 data
setdiff(union(colnames(jobs_03), colnames(total_data)), intersect(colnames(jobs_03), colnames(total_data)))
jobs_03 <- jobs_03 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP")
jobs_03$NAICS <- NA
jobs_03$NAICS_TITLE <- NA
jobs_03$I_GROUP <- NA
jobs_03$AREA_TYPE <- NA
jobs_03$OWN_CODE <- NA
jobs_03$PCT_TOTAL <- NA
jobs_03$PCT_RPT <- NA
jobs_03$LOC_QUOTIENT <- NA
jobs_03$JOBS_1000 <- NA
jobs_03$HOURLY <- NA
total_data <- rbind(jobs_03, total_data)

# Adding 2002 data
setdiff(union(colnames(jobs_02), colnames(total_data)), intersect(colnames(jobs_02), colnames(total_data)))
jobs_02 <- jobs_02 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP",
         "H_PCT10"="H_WPCT10",
         "H_PCT25"="H_WPCT25",
         "H_PCT75"="H_WPCT75",
         "H_PCT90"="H_WPCT90",
         "A_PCT10"="A_WPCT10",
         "A_PCT25"="A_WPCT25",
         "A_PCT75"="A_WPCT75",
         "A_PCT90"="A_WPCT90")
jobs_02$NAICS <- NA
jobs_02$NAICS_TITLE <- NA
jobs_02$I_GROUP <- NA
jobs_02$AREA_TYPE <- NA
jobs_02$OWN_CODE <- NA
jobs_02$PCT_TOTAL <- NA
jobs_02$PCT_RPT <- NA
jobs_02$LOC_QUOTIENT <- NA
jobs_02$JOBS_1000 <- NA
jobs_02$HOURLY <- NA
total_data <- rbind(jobs_02, total_data)

# Adding 2001 data
setdiff(union(colnames(jobs_01), colnames(total_data)), intersect(colnames(jobs_01), colnames(total_data)))
jobs_01 <- jobs_01 %>%
  rename("AREA_TITLE" = "AREA_NAME", 
         "O_GROUP" = "GROUP",
         "H_PCT10"="H_WPCT10",
         "H_PCT25"="H_WPCT25",
         "H_PCT75"="H_WPCT75",
         "H_PCT90"="H_WPCT90",
         "A_PCT10"="A_WPCT10",
         "A_PCT25"="A_WPCT25",
         "A_PCT75"="A_WPCT75",
         "A_PCT90"="A_WPCT90")
jobs_01$NAICS <- NA
jobs_01$NAICS_TITLE <- NA
jobs_01$I_GROUP <- NA
jobs_01$AREA_TYPE <- NA
jobs_01$OWN_CODE <- NA
jobs_01$PCT_TOTAL <- NA
jobs_01$PCT_RPT <- NA
jobs_01$LOC_QUOTIENT <- NA
jobs_01$JOBS_1000 <- NA
jobs_01$HOURLY <- NA
jobs_01 <- jobs_01 %>%
  select(-c("YEAR"))
total_data <- rbind(jobs_01, total_data)

write.csv(total_data, "./MSA_year_clean/MSA_ALL_YEARS.csv", row.names = FALSE)





