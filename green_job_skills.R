install.packages("readxl")

library("readxl")
library("dplyr")

# Data was collected from: https://www.onetcenter.org/dictionary/29.1/excel/skills.html

# Rename this to the file loacation
my_data <- read_excel("./data/Skills.xlsx")

data_df <- data.frame(my_data)

green_jobs <- list('47-2231', '49-9081', '49-9099', '47-4099', '47-1011', '41-4011', '47-2211', '49-9042', '51-9012', '51-8099', '51-8013', '51-8012', '51-8011', '51-4041', '19-4041', '19-4051', '17-2051', '17-2071', '17-2141', '17-2199', '11-3051', '11-3071', '11-9041', '11-9199')

data_df <- data_df %>% 
  mutate(short_code = substr(O.NET.SOC.Code, 1, 7))

skills_df <- data_df[,c('O.NET.SOC.Code', 'short_code', 'Title', 'Element.Name', 'Scale.ID', 'Data.Value')]

green_skills_df <- skills_df[skills_df$short_code %in% green_jobs, ]


job_titles <- green_skills_df$Title
job_titles <- job_titles[!duplicated(job_titles)]
job_titles

job_skills <- green_skills_df$Element.Name
job_skills <- job_skills[!duplicated(job_skills)]
job_skills

green_skills_df_lvl <- green_skills_df[green_skills_df$Scale.ID == 'LV', ]
green_skills_df_imp <- green_skills_df[green_skills_df$Scale.ID == 'IM', ]

green_skills_df_lvl_wide <- green_skills_df_lvl %>%
  pivot_wider(names_from = Element.Name, values_from = Data.Value)

green_skills_df_imp_wide <- green_skills_df_imp %>%
  pivot_wider(names_from = Element.Name, values_from = Data.Value)


green_skills_lvl_value <- green_skills_df[green_skills_df$Scale.ID == 'LV', "Data.Value"]
green_skills_imp_value <- green_skills_df[green_skills_df$Scale.ID == 'IM', "Data.Value"]

graph_data <- cbind(green_skills_lvl_value, green_skills_imp_value)

ggplot(data = graph_data, aes(x = green_skills_lvl_value, y = green_skills_imp_value)) +
  geom_point(alpha = 0.3, color = "darkseagreen4") +
  geom_smooth(method = "lm", color = "darkseagreen3") +
  theme_classic() +
  labs(title = "Ploting the Level of a Skill to the Importance of a Skill", x = "Level", y = "Importance") +
  theme(panel.background = element_rect(fill = "#F5FFF7"),
        plot.background = element_rect(fill = "white"))


