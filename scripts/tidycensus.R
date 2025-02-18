# install.packages("tidycensus")
library(tidycensus)
library(tidyverse)

# use unique census key, set `install = T` after first usage
census_api_key("b1f8f82b50aeafc2073bef9a04ca07e47a08066a", install = T)

# gives the smallest geography at which 5-year ACS data is available
data("acs5_geography")

# loads all labels from 2023 (most recent) acs5 dataset that include "occupation" on the tract level
# both the label and concept of query can be further specified
View(load_variables(year = 2023, dataset = "acs5") %>% filter(geography == "tract", str_detect(label, "occupation")))

# loads estimate of specified variable (found using above code) from 2023 on county level
# here, the variable (dataset code) is government natural resource workers, construction, and maintenance
occupations23 <- get_acs(geography = "county subdivision",
                         state = "CA",
                         county = "Santa Barbara",
                         variables = "C24060_029",
                         year = 2023)

# loads labels from 2023 acs1 dataset that include "extraction" in its label
View(load_variables(year = 2023, dataset = "acs1") %>% filter(str_detect(label, "Natural resources")))

# detailed data (table IDs that start with B) are not available from API
# code below gets aggregation of gov't workers, self-employed, ... in natural resources
industry23 <- get_acs(geography = "county subdivision",
                               state = "CA",
                               county = "Santa Barbara",
                               variables = "C24060_005",
                               year = 2023)
