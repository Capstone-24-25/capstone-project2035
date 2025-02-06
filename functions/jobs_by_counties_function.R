# Function that takes in the file name and then filters for the counties and jobs that are needed


jobs_by_counties <- function (file_year, counties = list('42200', '37100', '42020'), jobs) {
  
  MSA_data <- read_excel(sprintf("./MSA_year_raw/MSA_M20%s_dl.xlsx", file_year))
  
  names(MSA_data) <- toupper(names(MSA_data))
  
  CA_jobs_green <- MSA_data[(MSA_data$OCC_CODE %in% jobs) & (MSA_data$AREA %in% counties), ]
  CA_jobs_green$label = 'green job'
  
  CA_jobs_ff <- MSA_data[(substring(MSA_data$OCC_CODE, 1, 4) == '47-5') & (MSA_data$AREA %in% counties), ]
  CA_jobs_ff$label = 'ff job'
  
  CA_jobs_all <- rbind(CA_jobs_green, CA_jobs_ff)
  
  write.csv(CA_jobs_all, sprintf("./MSA_year_clean/CA_jobs_%s.csv", file_year), row.names = FALSE)
}
