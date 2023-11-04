setwd("D:/Geoinfomatics Study Materials/Second Semster/Geo_Science/RWorking/Ps Project")



# Load the required R packages
library(sf)      # For working with shapefiles
library(leaflet) # For geospatial mapping
library(rgdal)
library(raster)
library(terra)
library(dplyr)
library(ggplot2)
library(rgeos)
library(viridis)
library(rasterVis)
library(randomForest)
library(leaflet.extras)

#Load the in-situ data (shapefile)
insitu_shp <- st_read("LULC_Crop-Types_In-SituData2021_USP/LULC_Crop-Types_In-SituData2021_USP.shp")
# View the first few rows of the attribute data
head(insitu_shp)

# List all the column names in shapefile
column_names <- names(insitu_shp)
print(column_names)

# Remove the non-usable columns from  data frame
insitu_shp <- insitu_shp %>%
  select(-data_Other, -data_Photo, -data_UserN, -data_Phone, -data_Subsc, -data_Email, -data_EndTi, -data_meta_, -F17, -F18)

# See the unique Crop Types
unique_data_CropT <- unique(insitu_shp$data_CropT)
print(unique_data_CropT)

# See the unique Landuse  Types
unique_data_LULC <- unique(insitu_shp$data_LULC)
print(unique_data_LULC)

insitu_shp <- insitu_shp %>%
  mutate(NewColumn = ifelse(data_LULC == "Agriculture", data_CropT, data_LULC))

# View the first few rows of the updated data frame
head(insitu_shp)


insitu_shp <- insitu_shp %>%
  mutate(LU_CT = ifelse(data_LULC %in% c("Residential", "WaterBodies", "PublicUse", "CULARCH", "Forest", "Commercial", "Industrial"), "Mixed_Area", data_LULC))

# View the first few rows of the updated data frame
head(insitu_shp)

insitu_shp <- insitu_shp %>%
  mutate(LU_CT = ifelse(NewColumn %in% c("Residential", "WaterBodies", "PublicUse", "CULARCH", "Forest", "Commercial", "Industrial"), "Mixed_Area", NewColumn))

# View the first few rows of the updated data frame
head(insitu_shp)

# Again Remove the non-usable columns from  data frame
insitu_shp <- insitu_shp %>%
  select(-ClassType, -data_CropT, -data_LULC, -NewColumn)
head(insitu_shp)






# Load the district shapefile
district_shp <- st_read("nepal_data/hermes_NPL_new_wgs_2.shp")

# Define the target districts
target_districts <- c("Dhanusha", "Mahottari", "Sarlahi")

# Filter the shapefile to include only the target districts
filtered_districts <- district_shp[district_shp$DISTRICT %in% target_districts, ]

# Define a custom color palette for the specific class types
color_palette <- colorFactor(
  palette = c("lightgreen", "yellow", "darkgreen", "cyan", "orange","purple"),  # Define colors for each class type
  domain = c("Sugarcane", "PaddyRice", "Orchid","Bamboo", "OtherCrop", "Mixed_Area")  # Define class types
)

# Create a leaflet map with OpenStreetMap (OSM) as the base map
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = filtered_districts, color = "black", weight = 1, opacity = 1, fillOpacity = 0.2) %>%
  addCircleMarkers(data = insitu_shp,
                   lng = ~st_coordinates(insitu_shp$geometry)[, 1],
                   lat = ~st_coordinates(insitu_shp$geometry)[, 2],
                   popup = ~LU_CT,
                   color = ~color_palette(LU_CT),
                   radius = 1)




#importing sentinel image data

red_band <- raster("T45RUK_20211223T045221_B04.jp2")
nir_band <- raster("T45RUK_20211223T045221_B08.jp2")
TCI <- raster("T45RUK_20211223T045221_TCI.jp2")






# Calculation NDVI
ndvi_funtion <- function(red_band, nir_band) {
  ndvi <- (nir_band - red_band) / (nir_band + red_band)
  return(ndvi)
}

ndvi <- ndvi_funtion(red_band , nir_band)

#plotting NDVI
gplot(ndvi) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  coord_quickmap() +
  ggtitle("NDVI") +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +   					   
  theme(plot.title = element_text(hjust = 0.5),             
        text = element_text(size=15),		       	    
        axis.text.x = element_text(angle = 90, hjust = 1)) 


# Check the CRS ofNDVI data
crs_ndvi <- st_crs(ndvi)
# Print the CRS
print(crs_ndvi)


# Check the CRS of shapefile
crs_insitu <- st_crs(insitu_shp)
# Print the CRS
print(crs_insitu)


crs_target <- st_crs("+init=EPSG:32645")

insitu_projected <- st_transform(insitu_shp, crs_target)


# Extract NDVI values for insitu data locations
ndvi_values <- raster::extract(ndvi, insitu_projected)

# Add the extracted NDVI values to the insitu_filtered_projected data
insitu_combined <- cbind(insitu_projected, NDVI = ndvi_values)

head(insitu_combined)

nrow(insitu_combined)

# Remove rows with NA values in the NDVI column
insitu_combined <- insitu_combined[!is.na(insitu_combined$NDVI), ]
nrow(insitu_combined)


# Define the proportion for the training data (70% for training, 30% for testing)
train_prop <- 0.7

# Set a seed for reproducibility
set.seed(123)

# Initialize empty data frames for training and testing sets
train_data <- data.frame()
test_data <- data.frame()

# Define class names
class_names <- c("Bamboo", "Orchid", "PaddyRice", "Sugarcane", "OtherCrop", "Mixed_Area")

# Split the data into training and testing sets with at least one sample from each class
for (class_name in class_names) {
  # Filter rows for the current class
  class_data <- insitu_combined[insitu_combined$LU_CT == class_name, ]
  
  # Calculate the number of samples for training
  num_train_samples <- round(train_prop * nrow(class_data))
  
  # Randomly select samples for training and the rest for testing
  class_train_index <- sample(1:nrow(class_data), num_train_samples)
  class_test_index <- setdiff(1:nrow(class_data), class_train_index)
  
  # Add the selected samples to the training and testing sets
  train_data <- rbind(train_data, class_data[class_train_index, ])
  test_data <- rbind(test_data, class_data[class_test_index, ])
}

# Check the composition of the training and testing sets
table(train_data$LU_CT)
table(test_data$LU_CT)

# Train the random forest model
train_data$LU_CT <- as.factor(train_data$LU_CT)
rf_model <- randomForest(LU_CT ~ NDVI, data = train_data, ntree = 100)

# Make predictions on the test data
rf_predictions <- predict(rf_model, test_data)

predicted_classes <- predict(rf_model, test_data)
actual_classes <- test_data$LU_CT
confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)
print(confusion_matrix)


# Convert the confusion matrix to a data frame
confusion_matrix_df <- as.data.frame(as.table(confusion_matrix))

# Rename the columns for better labels
colnames(confusion_matrix_df) <- c("Predicted", "Actual", "Frequency")

# Create the heatmap using ggplot2
heatmap_plot <- ggplot(confusion_matrix_df, aes(Actual, Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1, size = 4) +
  scale_fill_gradient(low = "orange", high = "red") +  # Define color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted")

# Display the heatmap
print(heatmap_plot)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy: ", round(accuracy, 2), "\n")


# Define  class names
class_names <- c("Bamboo", "Orchid", "PaddyRice", "Sugarcane","OtherCrop","Mixed_Area")

# Initialize vectors to store precision, recall, and F1 values
precision_values <- numeric(length(class_names))
recall_values <- numeric(length(class_names))
f1_values <- numeric(length(class_names))


for (i in 1:length(class_names)) {
  class_name <- class_names[i]
  TP <- confusion_matrix[class_name, class_name]
  FP <- sum(confusion_matrix[, class_name]) - TP
  FN <- sum(confusion_matrix[class_name, ]) - TP
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_value <- 2 * (precision * recall) / (precision + recall)
  
 
  
  # Store values in respective vectors
  precision_values[i] <- precision
  recall_values[i] <- recall
  f1_values[i] <- f1_value
}

# Create a data frame with  results
results_df <- data.frame(
  Class = class_names,
  Precision = round(precision_values, 2),
  Recall = round(recall_values, 2),
  F1_Score = round(f1_values, 2)
)

# Use kable to format the results as a table
kable(results_df, caption = "Precision, Recall, and F1-Score for Each Class")

# Initialize vectors to store producer accuracy (PA) and user accuracy (UA)
PA_values <- numeric(length(class_names))
UA_values <- numeric(length(class_names))


for (i in 1:length(class_names)) {
  class_name <- class_names[i]
  
  # Calculate Producer Accuracy (PA)
  PA <- confusion_matrix[class_name, class_name] / sum(confusion_matrix[class_name, ])
  
  # Calculate User Accuracy (UA)
  UA <- confusion_matrix[class_name, class_name] / sum(confusion_matrix[, class_name])
  

  
  # Store values in respective vectors
  PA_values[i] <- PA
  UA_values[i] <- UA
}

# Create a data frame for  Producer Accuracy and User Accuracy results
PA_UA_df <- data.frame(
  Class = class_names,
  Producer_Accuracy = round(PA_values, 2),
  User_Accuracy = round(UA_values, 2)
)

# Use kable to format the results as a table
kable(PA_UA_df, caption = "Producer and User Accuracy for Each Class")

producer_accuracy_df <- data.frame(
  Class = class_names,
  Producer_Accuracy = round(PA_values, 2)
)
















