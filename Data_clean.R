# Load the data from the CSV file
file_path <- "C:/Users/1234d/Documents/es/assignment2/Canine_Waste_Dispensers.csv"
data <- read.csv(file_path)

# Check for missing values
print(colSums(is.na(data)))

# Handling missing values

# Fill missing numerical values with the column mean
data$CommunityBoard[is.na(data$CommunityBoard)] <- mean(data$CommunityBoard, na.rm = TRUE)
data$CouncilDistrict[is.na(data$CouncilDistrict)] <- mean(data$CouncilDistrict, na.rm = TRUE)

# Fill missing categorical values with the mode (most frequent value)
# Helper function to calculate mode
get_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Fill missing categorical columns with mode
data$Borough[is.na(data$Borough)] <- get_mode(data$Borough)
data$DispenserUnitLocation[is.na(data$DispenserUnitLocation)] <- get_mode(data$DispenserUnitLocation)
data$MountingSurface[is.na(data$MountingSurface)] <- get_mode(data$MountingSurface)
data$RestockedBy[is.na(data$RestockedBy)] <- get_mode(data$RestockedBy)

# Fill other missing values
data$PropertyName[is.na(data$PropertyName)] <- "Unknown"
data$ParkDistrict[is.na(data$ParkDistrict)] <- "Unknown"

# Verify that there are no missing values
print(colSums(is.na(data)))

# Save the cleaned data
save_path <- "C:/Users/1234d/Documents/es/assignment2/cleaned_Canine_Waste_Dispensers.csv"
write.csv(data, save_path, row.names = FALSE)

