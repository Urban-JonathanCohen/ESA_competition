I need to clean a bit this thing... Finishing my phd at the moment...

library(tidyverse)
library(ggplot2)
########################################
library(tmap)
library(raster)
library(ggplot2)
library(tidyr)
library(dplyr)
library(geodata)
library(gridExtra)
library(reshape2)
library(sf)
library(geojsonsf)
library(jsonlite)
library(geojsonio)
library(reticulate)
library(ggridges)
library(zoo)
####################################################


###################################################
setwd('C:/Users/edgardo/OneDrive - Chalmers/Documents/GitHub/FoodRisk/EU')


agri_risk <- raster("cdinx_m_euu_20230121_t.tif")


agri_risk

unique(values(agri_risk))
freq(agri_risk)
agri1 <- as.data.frame(freq(agri_risk))
agri1
class(freq(agri_risk))



############################
# UNZIP
###################################
##Assuming that the data is dowloded...

for (i in 2012:2023) {
  print(i)
  print(list.files(paste0('./',i), pattern = "\\.zip$", full.names = TRUE))
  zip_file <- list.files(paste0('./',i), pattern = "\\.zip$", full.names = TRUE)
  unzip(zip_file, exdir = paste0('./',i))
}


##########################
#create data structure
############################


# Create an empty data frame to store the results
result_df_eu <- data.frame(
  year = numeric(0),
  month = numeric(0),
  month_week = numeric(0),
  week = numeric(0),
  value_0 = numeric(0),
  value_1 = numeric(0),
  value_2 = numeric(0),
  value_4 = numeric(0),
  value_5 = numeric(0),
  value_6 = numeric(0),
  value_7 = numeric(0),
  value_na = numeric(0)
)


#####################################
# Extract values
#######################################

for (i in 2012:2023) {
  
  print("#######################")
  print(i)
  print("#######################")
  
  #print(list.files(paste0('./',i), pattern = "\\.tif$", full.names = TRUE))
  length(list.files(paste0('./',i), pattern = "\\.tif$", full.names = TRUE))
  
  
  for (j in 1:length(list.files(paste0('./',i), pattern = "\\.tif$", full.names = TRUE)) ) {
    #print(j)
    #print(list.files(paste0('./',i), pattern = "\\.tif$", full.names = TRUE)[j])
    agri_risk <- raster((list.files(paste0('./',i), pattern = "\\.tif$", full.names = TRUE)[j]))
    #print(freq(agri_risk))
    row <- as.data.frame(freq(agri_risk))
    #print(sum(freq(agri_risk)[freq(agri_risk)[, "value"] %in% c(1, 2, 3), "count"], na.rm = TRUE))
    
    m <- ((j - 1) %/% 3) + 1
    w <- ((j - 1) %% 3) + 1
    row_data <- data.frame(
      year = i,
      month = m,
      month_week = w,
      week  = j,
      value_0 = row$count[row$value == 0][1],  
      value_1 = row$count[row$value == 1][1],
      value_2 = row$count[row$value == 2][1],
      value_3 = row$count[row$value == 3][1],
      value_4 = row$count[row$value == 4][1],
      value_5 = row$count[row$value == 5][1],
      value_6 = row$count[row$value == 6][1],
      value_7 = row$count[row$value == 7][1],
      value_na = row$count[is.na(row$value)]
    )
    
    result_df_eu <- rbind(result_df_eu, row_data)
    
    
  }
  
  
}

result_df_eu$id <- seq_len(nrow(result_df_eu))


















############################################################################################
#temporal count per region
##############################################3333333

eu_country <- st_read("C:/Users/edgardo/OneDrive - Chalmers/Documents/GitHub/FoodRisk/base/eu_cod.shp")

crs(agri_risk)


eu_country <- st_transform(eu_country, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

eu_country

setwd('C:/Users/edgardo/OneDrive - Chalmers/Documents/GitHub/FoodRisk/EU')
agri_risk <- raster("cdinx_m_euu_20230121_t.tif")
agri_risk




result_df_eu_country <- data.frame(
  year = numeric(0),
  month = numeric(0),
  month_week = numeric(0),
  week = numeric(0),
  value_0  = numeric(0),
  value_1  = numeric(0),
  value_2  = numeric(0),
  value_3  = numeric(0),
  value_4  = numeric(0),
  value_5  = numeric(0),
  value_6  = numeric(0),
  value_7  = numeric(0),
  value_na = numeric(0),
  country  = numeric(0)  # A new column for region numbers
)

eu_country$id


setwd('C:/Users/edgardo/OneDrive - Chalmers/Documents/GitHub/FoodRisk/EU/')

for (i in 2012:2023) {
  
  print("#######################")
  print(i)
  print("#######################")
  
  for (j in 1:length(list.files(paste0('./', i), pattern = "\\.tif$", full.names = TRUE))) {
    agri_risk <- raster(list.files(paste0('./', i), pattern = "\\.tif$", full.names = TRUE)[j])
    
    # Extract values for each region
    eu_values <- extract(agri_risk, eu_country)
    
    # Get the region numbers from the shapefile
    cont_number <- eu_country$id
    
    # Loop through regions and add information to result_df_continents
    for (k in 1:length(eu_values)) {
      values <- eu_values[[k]]
      m <- ((j - 1) %/% 3) + 1
      w <- ((j - 1) %% 3) + 1
      
      row_data <- data.frame(
        year = i,
        month = m,
        month_week = w,
        week = j,
        value_0 = sum(values == 0, na.rm = TRUE),
        value_1 = sum(values == 1, na.rm = TRUE),
        value_2 = sum(values == 2, na.rm = TRUE),
        value_3 = sum(values == 3, na.rm = TRUE),
        value_4 = sum(values == 4, na.rm = TRUE),
        value_5 = sum(values == 5, na.rm = TRUE),
        value_6 = sum(values == 6, na.rm = TRUE),
        value_7 = sum(values == 7, na.rm = TRUE),
        value_na = sum(is.na(values)),
        contient = cont_number[k]  # Add the region number to the data frame
      )
      
      result_df_eu_country <- rbind(result_df_eu_country, row_data)
    }
  }
}

result_df_eu_country


result_df_eu_country_cleanz         <- result_df_eu_country
result_df_eu_country_cleanz$r2_r3   <- result_df_eu_country_cleanz$value_2 + result_df_eu_country_cleanz$value_3
result_df_eu_country_cleanz$total_r <- result_df_eu_country_cleanz$value_1 + result_df_eu_country_cleanz$value_2 + result_df_eu_country_cleanz$value_3  
result_df_eu_country_cleanz$total_b <- result_df_eu_country_cleanz$value_4 + result_df_eu_country_cleanz$value_5 + result_df_eu_country_cleanz$value_6  




result_df_eu_country_cleanz <- result_df_eu_country_cleanz %>%
  group_by(contient) %>%
  mutate(id = row_number()) %>%
  ungroup()







aggregated_eu <- result_df_eu_country_cleanz %>%
  group_by(year, month, contient) %>%
  summarize(
    sum_value_1 = sum(value_1),
    sum_value_2 = sum(value_2),
    sum_value_3 = sum(value_3),
    sum_value_4 = sum(value_4),
    sum_value_5 = sum(value_5),
    sum_value_6 = sum(value_6),
    sum_value_7 = sum(value_7),
    sum_value_0 = sum(value_0),
    sum_value_na = sum(value_na),
    sum_value_r2_r3 = sum(r2_r3),
    sum_value_total_r = sum(total_r),
    sum_value_total_b = sum(total_b)
  ) %>%
  ungroup()


aggregated_eu <- aggregated_eu %>%
  group_by(contient) %>%
  mutate(id = row_number()) %>%
  ungroup()






aggregated_eu_YR <- aggregated_eu %>%
  group_by(year) %>%
  summarize(
    sum_value_0 = sum(sum_value_0),
    sum_value_1 = sum(sum_value_1),
    sum_value_2 = sum(sum_value_2),
    sum_value_3 = sum(sum_value_3),
    sum_value_4 = sum(sum_value_4),
    sum_value_5 = sum(sum_value_5),
    sum_value_6 = sum(sum_value_6),
    sum_value_7 = sum(sum_value_7),
    sum_value_na = sum(sum_value_na),
    sum_value_r2_r3 = sum(sum_value_r2_r3),
    sum_value_total_r = sum(sum_value_total_r),
    sum_value_total_b = sum(sum_value_total_b)
  ) %>%
  ungroup()




aggregated_eu_YR <- aggregated_eu_YR %>%
  mutate(id = row_number()) %>%
  ungroup()













# Assuming your data frame is named df

aggregated_eu_YR$tot <- aggregated_eu_YR$sum_value_0 + 
  aggregated_eu_YR$sum_value_1 + aggregated_eu_YR$sum_value_2 + 
  aggregated_eu_YR$sum_value_3 + aggregated_eu_YR$sum_value_4  + 
  aggregated_eu_YR$sum_value_5 + aggregated_eu_YR$sum_value_6

colnames(aggregated_eu_YR)

# Assuming your data frame is named aggregated_eu_YR and you want to drop specific columns
aggregated_eu_YR_new <- subset(aggregated_eu_YR, select = -c(sum_value_na, sum_value_r2_r3, 
                                                             sum_value_total_r, sum_value_total_b, id))



aggregated_eu_YR_new$sum_value_0 <- aggregated_eu_YR_new$sum_value_0/aggregated_eu_YR$tot
aggregated_eu_YR_new$sum_value_1 <- aggregated_eu_YR_new$sum_value_1/aggregated_eu_YR$tot
aggregated_eu_YR_new$sum_value_2 <- aggregated_eu_YR_new$sum_value_2/aggregated_eu_YR$tot
aggregated_eu_YR_new$sum_value_3 <- aggregated_eu_YR_new$sum_value_3/aggregated_eu_YR$tot
aggregated_eu_YR_new$sum_value_4 <- aggregated_eu_YR_new$sum_value_4/aggregated_eu_YR$tot
aggregated_eu_YR_new$sum_value_5 <- aggregated_eu_YR_new$sum_value_5/aggregated_eu_YR$tot
aggregated_eu_YR_new$sum_value_6 <- aggregated_eu_YR_new$sum_value_6/aggregated_eu_YR$tot

aggregated_eu_YR_new

df_long_yr <-gather(aggregated_eu_YR_new[aggregated_eu_YR_new$year !=2023,], type,count, sum_value_0:sum_value_6)
  

df_long_yr <-gather(aggregated_eu_YR_new, type,count, sum_value_0:sum_value_6)





type_order <- c("sum_value_3",
                "sum_value_1", "sum_value_2",
                "sum_value_5",
                "sum_value_6","sum_value_4",
                "sum_value_0" )

# Convert 'type' to a factor with the specified order
df_long_yr$type <- factor(df_long_yr$type, levels = type_order)


#type_order <- rev(type_order)



my_colors <- rev(c("#afe09f", "#86ade3", "#95c9a7" , "#d6aae3",   "#f5df7d","#f5bd7d",  "#f57d7d" ))
  
  
# Create a stacked bar chart
ggplot(df_long_yr, aes(x = factor(year, levels = rev(unique(year))), y = count, fill = type)) +
  geom_bar(stat = "identity", width =0.2) +
  # labs(title = "Stacked Bar Chart",
  #      x = "Year",
  #      y = "Sum Values",
  #      fill = "Value") +
  scale_fill_manual(values = my_colors,  
                    labels = rev(c("No drought", 
                               "Recovery", 
                               "Temporary vegetation recovery", 
                               "Temporary Soil Moisture recovery", 
                               "Watch", 
                               "Warning", 
                               "Alert"))) +  
  theme_minimal() + 
  coord_flip()  +
  theme(
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),  # Remove the plot border
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    legend.background = element_rect(fill = "transparent")
  )






my_colors <- rev(c("#afe09f", "white", "white" , "white",   "#f5df7d","#f5df7d",  "#f5df7d" ))


# Create a stacked bar chart
ggplot(df_long_yr, aes(x = factor(year, levels = rev(unique(year))), y = count, fill = type)) +
  geom_bar(stat = "identity", width =0.2) +
  # labs(title = "Stacked Bar Chart",
  #      x = "Year",
  #      y = "Sum Values",
  #      fill = "Value") +
  scale_fill_manual(values = my_colors,  
                    labels = rev(c("No drought", 
                                   "Recovery", 
                                   "Temporary vegetation recovery", 
                                   "Temporary Soil Moisture recovery", 
                                   "Watch", 
                                   "Warning", 
                                   "Alert"))) +  
  theme_minimal() + 
  coord_flip()  +
  theme(
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),  # Remove the plot border
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    legend.background = element_rect(fill = "transparent")
  )





