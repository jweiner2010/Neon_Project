#### Capstone Project: Canopy Sturucture with LiDAR

outdata <- function(DTM, LAS_file) {
  
  #Import Libraries
  library(rLiDAR)
  library(raster)
  library(plyr)
  library(dplyr)
  
  #Read in LAS file and same named DTM
  LiDAR_data <- rLiDAR::readLAS(LAS_file)
  DTM <- raster(DTM)
  
  #Get x, y, z values from DTM to match with LiDAR data
  DTM_Values <- cbind(xyFromCell(DTM, 1:length(DTM)), getValues(DTM))
  
  #Reframe raster values as a dataframe to make it easier to work with
  #Rename columns to make more sense and to join the two datasets
  DTM_Values <- as.data.frame(DTM_Values)
  names(DTM_Values) <- c("X","Y","Ground_Elev")
  
  #Reframe LiDAR data to make it easier to work with
  LiDAR_data <- as.data.frame(LiDAR_data)
  
  #Floor data to match Xs and Ys
  LiDAR_data[,1] <- floor(LiDAR_data[,1])
  LiDAR_data[,2] <- floor(LiDAR_data[,2])
  
  DTM_Values[,1] <- floor(DTM_Values[,1])
  DTM_Values[,2] <- floor(DTM_Values[,2])
  
  #rand_x <- sample(1:nrow(LiDAR_data), 5000)
    
  #points(x = LiDAR_data[rand_x, 1], y = LiDAR_data[rand_x, 2], add = TRUE)
  
  #Join datasets together
  LiDAR_and_DTM_Data <- join(LiDAR_data, DTM_Values, by = c("X","Y")) 
  
  #Get corrected Z value
  Corrected_Elev <- LiDAR_and_DTM_Data[,3] - LiDAR_and_DTM_Data[,6]
  
  #Spatial Data
  X_Values <- as.data.frame(LiDAR_and_DTM_Data[,1])
  names(X_Values) <- "X_Values"
  Y_Values <- as.data.frame(LiDAR_and_DTM_Data[,2])
  names(Y_Values) <- "Y_Values"
  Spatial_with_CorElev <- cbind(X_Values, Y_Values, as.integer(Corrected_Elev[,1]))
  names(Spatial_with_CorElev) <- cbind("X","Y","Corrected_Elev")
  
  
  
  
  

  
  
  
  
}


##################
# WORKFLOW/TO-DO
#
# Use IF statement to round LiDAR values to nearest .5 value to match DTM data
#
#
# Subtract DTM from LiDAR data
# Note if no DTM, don't use that data point (LiDAR -> NA)
# Have to do some rounding and/or flooring to match LiDAR points to DTM tiles
# Decide on binning/voxel method (pixel size and vertical extent)
# Count returns in each voxel and create profiles for each pixel
# Cluster profiles (using k-means?)
# Distribute across landscape
#
#
#
#




