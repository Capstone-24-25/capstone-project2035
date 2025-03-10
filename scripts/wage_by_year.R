library(ggplot2)
library(tidyverse)

data <- read.csv("../Data/median_yearly_earning.csv")
  
data %>% 
  mutate(estimate = as.numeric(estimate)) %>% 
  filter(job_type != 'Other') %>% 
  group_by(county,year, job_type) %>% 
  summarise(med_wage = median(estimate, na.rm = T)) %>% 
  ggplot(aes(x = year, y = med_wage,  color = job_type)) +
  geom_line() +
  labs(title = "Median Yearly Wages by County and Job Type",
       x = "Year",
       y = "Median Wage",
       color = "Job Type")+
  facet_wrap(~county)

data %>% 
  mutate(estimate = as.numeric(estimate)) %>% 
  filter(job_type != 'Other') %>% 
  group_by(county,year, job_type) %>% 
  summarise(avg_wage = mean(estimate, na.rm = T)) %>% 
  ggplot(aes(x = year, y = avg_wage,  color = job_type)) +
  geom_line() +
  labs(title = "Average Yearly Wages by County and Job Type",
       x = "Year",
       y = "Average Wage",
       color = "Job Type")+
  facet_wrap(~county)
