install.packages("readxl")

library("readxl")
library("dplyr")
library("tidyverse")

# Data was collected from: https://www.onetcenter.org/dictionary/29.1/excel/skills.html

# Rename this to the file loacation
my_data <- read_excel("./data/Skills.xlsx")

data_df <- data.frame(my_data)

green_jobs <- list('47-2231', '49-9081', '49-9099', '47-4099', '47-1011', '41-4011', '47-2211', '49-9042', '51-9012', '51-8099', '51-8013', '51-8012', '51-8011', '51-4041', '19-4041', '19-4051', '17-2051', '17-2071', '17-2141', '17-2199', '11-3051', '11-3071', '11-9041', '11-9199')
ff_jobs <- list('47-5011', '47-5012', '47-5013', '47-5022', '47-5041', '47-5043', '47-5044', '47-5071', '47-5081')

data_df <- data_df %>% 
  mutate(short_code = substr(O.NET.SOC.Code, 1, 7))

skills_df <- data_df[,c('O.NET.SOC.Code', 'short_code', 'Title', 'Element.Name', 'Scale.ID', 'Data.Value')]


# Cleaning up green data
skills_green <- skills_df[skills_df$short_code %in% green_jobs, ]

job_titles_green <- skills_green$Title
job_titles_green <- job_titles_green[!duplicated(job_titles_green)]
job_titles_green

job_skills_green <- skills_green$Element.Name
job_skills_green <- job_skills_green[!duplicated(job_skills_green)]
job_skills_green

skills_green_lvl <- skills_green[skills_green$Scale.ID == 'LV', ]
skills_green_imp <- skills_green[skills_green$Scale.ID == 'IM', ]

skills_green_lvl_wide <- skills_green_lvl %>%
  pivot_wider(names_from = Element.Name, values_from = Data.Value)

skills_green_imp_wide <- skills_green_lvl %>%
  pivot_wider(names_from = Element.Name, values_from = Data.Value)

# Cleaning up ff data
skills_ff <- skills_df[skills_df$short_code %in% ff_jobs, ]

job_titles_ff <- skills_ff$Title
job_titles_ff <- job_titles_ff[!duplicated(job_titles_ff)]
job_titles_ff

job_skills_ff <- skills_ff$Element.Name
job_skills_ff <- job_skills_ff[!duplicated(job_skills_ff)]
job_skills_ff

skills_ff_lvl <- skills_ff[skills_ff$Scale.ID == 'LV', ]
skills_ff_imp <- skills_ff[skills_ff$Scale.ID == 'IM', ]

skills_ff_lvl_wide <- skills_ff_lvl %>%
  pivot_wider(names_from = Element.Name, values_from = Data.Value)

skills_ff_imp_wide <- skills_ff_imp %>%
  pivot_wider(names_from = Element.Name, values_from = Data.Value)


# Analyzing level vs importance
green_skills_lvl_value <- skills_green[skills_green$Scale.ID == 'LV', "Data.Value"]
green_skills_imp_value <- skills_green[skills_green$Scale.ID == 'IM', "Data.Value"]

graph_data <- cbind(green_skills_lvl_value, green_skills_imp_value)

# Plot shows that they are highly correlated
ggplot(data = graph_data, aes(x = green_skills_lvl_value, y = green_skills_imp_value)) +
  geom_point(alpha = 0.3, color = "darkseagreen4") +
  geom_smooth(method = "lm", color = "darkseagreen3") +
  theme_classic() +
  labs(title = "Ploting the Level of a Skill to the Importance of a Skill", x = "Level", y = "Importance") +
  theme(panel.background = element_rect(fill = "#F5FFF7"),
        plot.background = element_rect(fill = "white"))


# Exporting wide data frames
setwd('./Data')
write.csv(skills_green_lvl_wide,"./skills_green_lvl.csv", row.names = FALSE)
write.csv(skills_ff_lvl_wide,"./skills_ff_lvl.csv", row.names = FALSE)



