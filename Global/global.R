


#------------------------ Global.R ----------------------------------------

# Mempersiapkan libraries

# install.packages(c(
#   "dplyr", "lubridate", "ggplot2", "plotly",
#   "glue", "scales", "ggpubr", "tidyr", "treemapify", "sf", "hrbrthemes", 
#   "viridis", "MatrixModels", "Matrix"
# ))

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(glue)
library(scales)
library(ggpubr)
library(tidyr)
library(treemapify)
library(sf)
library(hrbrthemes)
library(viridis)
library(MatrixModels)
library(Matrix)


# me-non-aktifkan scientific notation
options(scipen = 9999) 

# # Membaca data
 data <- read.csv("data_input/Energydata.csv")
# 
# # Mengubah tipe data
# vids_clean <- vids %>% 
#   mutate_at(.vars = c("title", "category_id", "channel_title", 
#                       "publish_when",  "publish_wday"), 
#             .funs = as.factor) %>% 
#   mutate(trending_date = ymd(trending_date),
#          publish_time = ymd_hms(publish_time))

#-----------------Pivot---------
 data <- data %>% 
   #Filter
   #Merubah bentuk ke long format
   pivot_longer(cols = c(X1990 : X2022),
                names_to = "Year",
                values_to = "values")
#-----------------------Year Edit
 data$Year <- substr(data$Year, 2,5)

#---------------------Select Data
 data <- select(data, Country, Category, Year,values)

#-------------------------Mutate type
 
 data <- data %>% 
   mutate(
     Category = as.character(Category),
     values = as.numeric(values),
     Year = ymd(paste0(Year, "-12-31"))
   )

#-----------------Na Handling
 
 # Menghitung rata-rata values berdasarkan Country, Category, dan Year
 average_values <- data %>%
   ungroup() %>%  # Menghilangkan pengelompokan dari hasil summarize sebelumnya
   group_by(Country, Category, Year) %>%
   summarize(avg_value = mean(values, na.rm = TRUE))
 
 # Mengganti NA dengan rata-rata yang sesuai
 data1 <- data %>%
   left_join(average_values, by = c("Country", "Category", "Year")) %>%
   mutate(values = ifelse(is.na(values), avg_value, values)) %>%
   select(-avg_value)  # Menghapus kolom avg_value yang tidak diperlukan lagi
 
#------------------------Filtering Country 
 
 exclude_countries <- c("World", "OECD", "Asia", "G7", "BRICS", "Europe", "European Union", "CIS", "America", "North America")
 
 # Filter the dataset
 data2 <- data1 %>% 
   filter(!Country %in% exclude_countries)
 
 
 
 #---------------------Penyesuaian dengan data MAP SF
 data2 <- data2 %>%
   mutate(
     Country = case_when(
       Country == "Czechia"        ~ "Czech Republic",
       Country == "Turkiye"        ~ "Turkey",
       Country == "North America"  ~ "United States Minor Outlying Islands",
       Country == "Latin America"  ~ "Brazil",
       Country == "South Korea"    ~ "Korea, Republic of",
       Country == "Pacific"        ~ "Australia",
       Country == "Africa"         ~ "South Africa",
       Country == "Middle-East"    ~ "Iran",
       Country == "Iran"           ~ "Iran (Islamic Republic of)",
       TRUE                        ~ as.character(Country)
     )
   )
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 