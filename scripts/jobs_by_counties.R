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

jobs_by_counties(23, jobs = green_jobs)
jobs_by_counties(22, jobs = green_jobs)
jobs_by_counties(21, jobs = green_jobs)
jobs_by_counties(20, jobs = green_jobs)
jobs_by_counties(19 ,jobs = green_jobs)
 