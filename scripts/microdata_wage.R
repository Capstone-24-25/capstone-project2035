library(tidyverse)
library(ggplot2)

data <- read.csv("Data/clean_microdata.csv")

# plot wage distribution of green jobs in 2023 with a red line for average
microdata_wage_2023_2010 <- data %>% 
  filter(year %in% c(2023, 2010)) %>% 
  filter(wagp > 0) %>%
  ggplot(aes(x = wagp)) +
  geom_histogram(binwidth = 4000, fill = 'black') +
  labs(title = "Wage Distribution in 2010 and 2023",
       x = "Wage",
       y = "Count") +
  scale_x_continuous(labels = scales::dollar_format())+
  facet_wrap(~ year + occ_type)+
  theme_minimal()
ggsave("graphs_png/microdata_wage_2023_2010.png", plot = microdata_wage_2023_2010, width = 10, height = 8)
#graph distribution of wage over the 3 different counties
microdata_wage_2023 <- data %>% 
  filter(year == 2023) %>% 
  filter(wagp > 0) %>%
  ggplot(aes(x = wagp)) +
  geom_histogram(binwidth = 4000, fill = 'black') +
  labs(title = "Wage Distribution in 2023 by county",
       x = "Wage",
       y = "Count") +
  scale_x_continuous(labels = scales::dollar_format())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) +
  facet_wrap(~ year + occ_type + county)
ggsave("graphs_png/microdata_wage_2023.png", plot = microdata_wage_2023, width = 10, height = 8)

#graph box plots of wage for each year
data %>% 
  filter(wagp > 0) %>%
  ggplot(aes(x = as.factor(year), y = wagp)) +
  geom_boxplot() +
  labs(title = "Wage Distribution of Green Jobs in 2010 and 2023",
       x = "Year",
       y = "Wage") +
  scale_y_continuous(labels = scales::dollar_format())+
  facet_wrap(~ occ_type)+
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#graph wage over time by occ_type
microdata_avgwage <- data %>% 
  filter(wagp > 0) %>%
  group_by(year, county, occ_type) %>%
  summarise(mean_wage = mean(wagp)) %>%
  ggplot(aes(x = year, y = mean_wage, color = occ_type)) +
  geom_line() +
  labs(title = "Mean Wage Over Time by Microdata",
       x = "Year",
       y = "Mean Wage") +
  scale_y_continuous(labels = scales::dollar_format())+
  facet_grid(~ county)
ggsave("graphs_png/microdata_avgwage.png", plot = microdata_avgwage, width = 10, height = 8)
#county map with average wage in 2023 by region using
library(maps)
data<- data %>% 
  mutate(subregion = county,
         subregion = str_to_lower(str_trim(subregion)))
target_counties <- c("santa barbara", "ventura", "san luis obispo")
map_data <- map_data("county")
ca_counties <- subset(map_data, region == "california") %>% 
  filter(subregion %in% target_counties)
merged <- merge(ca_counties, data, by = c("subregion"), all.x = TRUE)
merged %>% 
  group_by(subregion, occ_type) %>%
  filter(year == 2023) %>% 
  summarise(mean_wage = mean(wagp, na.rm = TRUE),group = group,long=long, lat=lat, occ_type = occ_type) %>% 
  ggplot(aes(x = long, y = lat, fill = mean_wage, group=group)) +
  geom_polygon(color = "black") +
  coord_fixed(1.3)+
  facet_wrap(~occ_type)+
  scale_fill_viridis_c(
    option = "cividis",
    na.value = "gray90",
    labels = scales::label_comma()
  ) +
  theme_void()+
  labs(
    title = "Average Wage in 2023 by County",
    fill = "Average Wage"
  ) 

# make the same but in 2005
merged %>% 
  group_by(subregion, occ_type) %>%
  filter(year == 2005) %>% 
  summarise(mean_wage = mean(wagp, na.rm = TRUE),group = group,long=long, lat=lat, occ_type = occ_type) %>% 
  ggplot(aes(x = long, y = lat, fill = mean_wage, group=group)) +
  geom_polygon(color = "black") +
  coord_fixed(1.3)+
  facet_wrap(~occ_type)+
  scale_fill_viridis_c(
    option = "cividis",
    na.value = "gray90",
    labels = scales::label_comma()
  ) +
  theme_void()+
  labs(
    title = "Average Wage in 2005 by County",
    fill = "Average Wage"
  )

# table of top 10 highest earners 
top10_wage_green <- data %>% 
  filter(wagp > 0, occ_type== 'Green') %>%
  group_by(occ_type) %>%
  arrange(desc(wagp)) %>%
  slice_head(n = 40) %>%
  ungroup() %>%
  mutate(wagp = scales::dollar(wagp)) %>% 
  select(occupation, year, wagp, occ_type,socp, county) %>% 
  kableExtra::kbl()
save_kable(top10_wage_green, file = "graphs_png/top10_wage_green.html")
webshot("graphs_png/top10_wage_green.html", 
        file = "graphs_png/top10_wage_green.png")


#average wages of the top 10 most common occupations
data %>% 
  filter(wagp > 0) %>%
  group_by(occupation) %>%
  summarise(mean_wage = mean(wagp), num_jobs = n()) %>%
  arrange(desc(num_jobs)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(mean_wage = scales::dollar(mean_wage)) %>% 
  kableExtra::kbl()


