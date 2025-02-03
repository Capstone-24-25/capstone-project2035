# install.packages("usmap")

library(maps)
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization

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

