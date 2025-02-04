library(tidyverse)
library(readxl)
library(httr)

green_jobs <- c('47-2231', '49-9081', '49-9099', '47-4099', '47-1011', '41-4011', '47-2211', '49-9042', '51-9012', '51-8099', '51-8013', '51-8012', '51-8011', '51-4041', '19-4041', '19-4051', '17-2051', '17-2071', '17-2141', '17-2199', '11-3051', '11-3071', '11-9041', '11-9199')
ff_jobs <- c('47-5000', '47-5010', '47-5011', '47-5012', '47-5013', '47-5022', '47-5041', '47-5043', '47-5044', '47-5071', '47-5081')
counties_code <- c('42200', '37100', '42020')

# from up until 2000, data needs reformatting
#oews_2000_1 <- read_excel("~/Desktop/PSTAT 197/data/oes00ma/MSA_2000_dl_1.xls")
#oews_2000_2 <- read_excel("~/Desktop/PSTAT 197/data/oes00ma/MSA_2000_dl_2.xls")
#oews_2000 <- rbind(oews_2000_1, oews_2000_2)

oews_2001 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oes01ma/MSA_2001_dl_1.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oes01ma/MSA_2001_dl_2.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oes01ma/MSA_2001_dl_3.xls")
)
colnames(oews_2001)[which(names(oews_2001) == "occ_code")] <- "OCC_CODE"
colnames(oews_2001)[which(names(oews_2001) == "area")] <- "AREA"

oews_2002 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oes02ma/MSA_2002_dl_1.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oes02ma/MSA_2002_dl_2.xls")
)
colnames(oews_2002)[which(names(oews_2002) == "occ_code")] <- "OCC_CODE"
colnames(oews_2002)[which(names(oews_2002) == "area")] <- "AREA"

oews_2003 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesn03ma/MSA_november2003_dl_1.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oesn03ma/MSA_november2003_dl_2.xls")
)

oews_2004 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesn04ma/MSA_november2004_dl_1.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oesn04ma/MSA_november2004_dl_2.xls"), 
  read_excel("~/Desktop/PSTAT 197/data/oesn04ma/MSA_november2004_dl_3.xls")
)

oews_2005 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm05ma/MSA_may2005_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm05ma/MSA_may2005_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm05ma/MSA_may2005_dl_3.xls")
)

oews_2006 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm06ma/MSA_may2006_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm06ma/MSA_may2006_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm06ma/MSA_may2006_dl_3.xls")
)

oews_2007 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm07ma/MSA_May2007_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm07ma/MSA_May2007_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm07ma/MSA_May2007_dl_3.xls")
)

oews_2008 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm08ma/MSA__M2008_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm08ma/MSA_M2008_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm08ma/MSA_M2008_dl_3.xls")
)

oews_2009 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm09ma/MSA_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm09ma/MSA_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm09ma/MSA_dl_3.xls")
)

oews_2010 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm10ma/MSA_M2010_dl_1.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm10ma/MSA_M2010_dl_2.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm10ma/MSA_M2010_dl_3.xls")
)

oews_2011 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm11ma/MSA_M2011_dl_1_AK_IN.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm11ma/MSA_M2011_dl_2_KS_NY.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm11ma/MSA_M2011_dl_3_OH_WY.xls")
)

oews_2012 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm12ma/MSA_M2012_dl_1_AK_IN.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm12ma/MSA_M2012_dl_2_KS_NY.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm12ma/MSA_M2012_dl_3_OH_WY.xls")
)

oews_2013 <- rbind(
  read_excel("~/Desktop/PSTAT 197/data/oesm13ma/MSA_M2013_dl_1_AK_IN.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm13ma/MSA_M2013_dl_2_KS_NY.xls"),
  read_excel("~/Desktop/PSTAT 197/data/oesm13ma/MSA_M2013_dl_3_OH_WY.xls")
)

oews_2014 <- read_excel("~/Desktop/PSTAT 197/data/oesm14ma/MSA_M2014_dl.xlsx")

oews_2015 <- read_excel("~/Desktop/PSTAT 197/data/oesm15ma/MSA_M2015_dl.xlsx")

oews_2016 <- read_excel("~/Desktop/PSTAT 197/data/oesm16ma/MSA_M2016_dl.xlsx")

oews_2017 <- read_excel("~/Desktop/PSTAT 197/data/oesm17ma/MSA_M2017_dl.xlsx")

oews_2018 <- read_excel("~/Desktop/PSTAT 197/data/oesm18ma/MSA_M2018_dl.xlsx")

oews_2019 <- read_excel("~/Desktop/PSTAT 197/data/oesm19ma/MSA_M2019_dl.xlsx")
colnames(oews_2019)[which(names(oews_2019) == "occ_code")] <- "OCC_CODE"
colnames(oews_2019)[which(names(oews_2019) == "area")] <- "AREA"

oews_2020 <- read_excel("~/Desktop/PSTAT 197/data/oesm20ma/MSA_M2020_dl.xlsx")

oews_2021 <- read_excel("~/Desktop/PSTAT 197/data/oesm21ma/MSA_M2021_dl.xlsx")

oews_2022 <- read_excel("~/Desktop/PSTAT 197/data/oesm22ma/MSA_M2022_dl.xlsx")

oews_2023 <- read_excel("~/Desktop/PSTAT 197/data/oesm23ma/MSA_M2023_dl.xlsx")

oews_data <- list(oews_2001, oews_2002, oews_2003, oews_2004, oews_2005, oews_2006, oews_2007,
  oews_2008, oews_2009, oews_2010, oews_2011, oews_2012, oews_2013, oews_2014, oews_2015,
  oews_2016, oews_2017, oews_2018, oews_2019, oews_2020, oews_2021, oews_2022, oews_2023)

years <- 2001:2021

for(i in 1:23){
  oews_data[[i]] <- data.frame(oews_data[[i]])
  oews_data[[i]] <- oews_data[[i]][(oews_data[[i]]$OCC_CODE %in% green_jobs | oews_data[[i]]$OCC_CODE %in% ff_jobs) & (oews_data[[i]]$AREA %in% counties_code), ]
}
