library(ggplot2)
library(tidyverse)

data <- read.csv("../Data/median_yearly_earning.csv")
  
data %>% 
  filter(occupation == "construction_and_extraction_occupations") %>% 
  ggplot(aes(x = year, y = estimate, group = zipcode, color = zipcode)) +
  geom_line() +
  labs(title = "Yearly Wages by Zipcode for Construction and Extraction Occupations",
       x = "Year",
       y = "Wage",
       color = "Zipcode") +
  facet_wrap(~county)
data %>% 
  mutate(zipcode = as.factor(zipcode)) %>% 
  filter(occupation == "construction_and_extraction_occupations") %>% 
  ggplot(aes(x = year, y = estimate)) +
  geom_line() +
  labs(title = "Yearly Wages by Zipcode for Construction and Extraction Occupations",
       x = "Year",
       y = "Wage") +
  facet_wrap(~zipcode)

data %>% 
  filter(occupation == "construction_and_extraction_occupations") %>% 
  ggplot(aes(x = factor(zipcode), group = year, fill = year)) +
  geom_bar(stat = "identity", position = 'dodge',aes(y=estimate))+
  labs(title = "Yearly Wages by Zipcode for Construction and Extraction Occupations",
       x = "Zipcode",
       y = "Wage",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~county, scales = "free_x")
