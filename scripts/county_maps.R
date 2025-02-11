# install.packages("usmap")

library(maps)
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization
library(tidyverse)

# Loading the data
my_data <- read_csv("./MSA_year_clean/CA_jobs_23.csv")


# Data manipulation
total_employment <- my_data %>% group_by(AREA_TITLE, label) %>%
  summarise(n_jobs = sum(TOT_EMP, na.rm = TRUE)) %>%
  pivot_wider(names_from = label, values_from = n_jobs, values_fill = list(n_jobs = 0))

# Setting value as the test statistic
total_employment$value = total_employment$`ff job` / total_employment$`green job`

total_employment$subregion <- c('ventura', 'san luis obispo', 'santa barbara')

# Subsetting the data into a 3x2 data-frame
data <- total_employment[,c('value', 'subregion')]

# Plotting data
# make sure your working directory is set to the capstone-project2035 folder
plot_county_map(data = data, title = "Proportion of Fossil Fuel Jobs to Green Jobs in 2023", save_title = "prop_ff_green_23")




my_data <- read_csv("./Data/MSA_ALL_YEARS.csv")

my_data <- my_data[my_data$label == "ff job",]

ff_jobs <- my_data[,c("AREA_TITLE", "year", "TOT_EMP")]

ff_jobs$TOT_EMP[ff_jobs$TOT_EMP == "**"] <- 0

ff_jobs$TOT_EMP <- as.numeric(ff_jobs$TOT_EMP)

total_employment <- ff_jobs %>% group_by(AREA_TITLE, year) %>%
  summarise(value = sum(TOT_EMP, na.rm = TRUE))

total_employment$AREA_TITLE[total_employment$AREA_TITLE == "Oxnard-Thousand Oaks-Ventura, CA"] <- "ventura"
total_employment$AREA_TITLE[total_employment$AREA_TITLE == "Ventura, CA PMSA"] <- "ventura"
total_employment$AREA_TITLE[total_employment$AREA_TITLE == "San Luis Obispo-Paso Robles, CA"] <- "san luis obispo"
total_employment$AREA_TITLE[total_employment$AREA_TITLE == "San Luis Obispo-Paso Robles-Arroyo Grande, CA"] <- "san luis obispo"
total_employment$AREA_TITLE[total_employment$AREA_TITLE == "Santa Barbara-Santa Maria-Lompoc, CA MSA"] <- "santa barbara"
total_employment$AREA_TITLE[total_employment$AREA_TITLE == "Santa Maria-Santa Barbara, CA"] <- "santa barbara"

colnames(total_employment)[colnames(total_employment) == "AREA_TITLE"] <- "subregion"

for (i in unique(total_employment$year)) {
  data <- total_employment[total_employment$year == i,]
  
  data <- data[,c('subregion', 'value')]
  
  if (!('ventura' %in% data$subregion)) 
    # Add new row
    data <- rbind(data, data.frame(subregion = 'ventura', value = 0))
  if (!('san luis obispo' %in% data$subregion)) 
    # Add new row
    data <- rbind(data, data.frame(subregion = 'san luis obispo', value = 0))
  if (!('santa barbara' %in% data$subregion)) 
    # Add new row
    data <- rbind(data, data.frame(subregion = 'santa barbara', value = 0))
  
  plot_county_map(data = data, title = sprintf("Number of Fossil Fuel Jobs in 20%s", i), save_title = sprintf("ff_jobs_%s", i))
}
  
