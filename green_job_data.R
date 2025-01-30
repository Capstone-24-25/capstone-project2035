library("readxl")
library("dplyr")

# Data was collected from: https://www.bls.gov/oes/tables.htm
# Using the Metropolitan and non-metropolitan area with XLS
# Then open MSA file

# Rename this to the file loaction
my_data <- read_excel("./data/total_data.xlsx")

green_jobs <- list('47-2231', '49-9081', '49-9099', '47-4099', '47-1011', '41-4011', '47-2211', '49-9042', '51-9012', '51-8099', '51-8013', '51-8012', '51-8011', '51-4041', '19-4041', '19-4051', '17-2051', '17-2071', '17-2141', '17-2199', '11-3051', '11-3071', '11-9041', '11-9199')

counties_code <- list('42200', '37100', '42020')

green_CA_jobs <- my_data[(my_data$OCC_CODE %in% green_jobs) & (my_data$AREA %in% counties_code), ]
