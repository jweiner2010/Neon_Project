#### Capstone Project: Canopy Sturucture with LiDAR

Canopy_Structure <- function(DTM, LAS_file) {
  
  #Import Libraries
  library(rLiDAR)
  library(raster)
  library(plyr)
  library(dplyr)
  library(VoxR)
  library(ggplot2)
  
  #Turn off scientific notation
  options(scipen=999)
  
  #Read in LAS file and same named DTM
  LiDAR_data <- rLiDAR::readLAS(LAS_file)
  #LiDAR_data <- rLiDAR::readLAS("/Volumes/AOP-NEON1-4/D17/SOAP/2013/SOAP_L1/SOAP_Lidar/Classified_point_cloud/las_files/2013_SOAP_1_298000_4100000.laz.las")
  DTM <- raster(DTM)
  #DTM <- raster("/Volumes/AOP-NEON1-4/D17/SOAP/2013/SOAP_L1/SOAP_Lidar/Classified_point_cloud/las_files/2013_SOAP_1_298000_4100000_DTM.tif")
  
  #Bring in larger rasters
  canopy_raster <- raster("/Volumes/NO NAME/Tresholded_Projected/canopy/hdr.adf")
  shrub_raster <- raster("/Volumes/NO NAME/Tresholded_Projected/shrub/hdr.adf")
  understory_raster <- raster("/Volumes/NO NAME/Tresholded_Projected/understory/hdr.adf")

  #Compare extents
  plot(canopy_raster)
  plot(DTM, add = TRUE)
  
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
  
  #Join datasets together
  LiDAR_and_DTM_Data <- join(LiDAR_data, DTM_Values, by = c("X","Y")) 
  
  #Get corrected Z value
  Corrected_Elev <- LiDAR_and_DTM_Data[,3] - LiDAR_and_DTM_Data[,6]
  Corrected_Elev <- as.data.frame(Corrected_Elev)
  
  #Spatial Data
  X_Values <- as.data.frame(LiDAR_and_DTM_Data[,1])
  names(X_Values) <- "X_Values"
  
  Y_Values <- as.data.frame(LiDAR_and_DTM_Data[,2])
  names(Y_Values) <- "Y_Values"
  
  Spatial_with_CorElev <- cbind(X_Values, Y_Values, (Corrected_Elev[,1]))
  names(Spatial_with_CorElev) <- cbind("X","Y","Corrected_Elev")
  
  #Put data into Voxels
  Voxeled_data <- VoxR::vox(Spatial_with_CorElev)
  names(Voxeled_data) <- c("x","y","z","num_pts")
  
  #Segment across the landscape
  Voxeled_data$xbin <- cut(Voxeled_data$x, 10, labels = FALSE)
  Voxeled_data$ybin <- cut(Voxeled_data$y, 10, labels = FALSE)
  
  #Test plot
  ggplot(Voxeled_data[sample(1:nrow(Voxeled_data), 20000), ], aes(x = num_pts, y = z)) + geom_bin2d() + facet_grid(ybin ~ xbin)
  
  #Extract values from rasters
  Voxeled_data$canopy_TH <- extract(canopy_raster, as.matrix(Voxeled_data[, 1:2]))
  Voxeled_data$shrub_TH <- extract(shrub_raster, as.matrix(Voxeled_data[, 1:2]))
  Voxeled_data$understory_TH <- extract(understory_raster, as.matrix(Voxeled_data[, 1:2]))
  
  #Make classifications 
  Voxeled_data$Classifications <- NA
  
  #Take out NAs
  Voxeled_data_no_NA <- Voxeled_data[!is.na(Voxeled_data$canopy_TH), ]
  
  #Make sums column
  Voxeled_data_no_NA$sum <- rowSums(Voxeled_data_no_NA[, 7:9])
  
  #Identify Vegetation classifications
  Voxeled_data_no_NA$Classifications[Voxeled_data_no_NA$shrub_TH == 1 & Voxeled_data_no_NA$sum == 1] <- "Shrubs"
  
  Voxeled_data_no_NA$Classifications[Voxeled_data_no_NA$canopy_TH == 1 & Voxeled_data_no_NA$sum == 1] <- "Canopy"
  
  Voxeled_data_no_NA$Classifications[Voxeled_data_no_NA$understory_TH == 1 & Voxeled_data_no_NA$sum == 1] <- "Understory"
  
  Voxeled_data_no_NA$Classifications[Voxeled_data_no_NA$understory_TH == 1 & Voxeled_data_no_NA$canopy_TH == 1 & Voxeled_data_no_NA$sum == 2] <- "Understory_and_Canopy"
  
  Voxeled_data_no_NA$Classifications[Voxeled_data_no_NA$shrub_TH == 1 & Voxeled_data_no_NA$canopy_TH == 1 & Voxeled_data_no_NA$sum == 2] <- "Shrubs_and_Canopy"
  
  Voxeled_data_no_NA$Classifications[Voxeled_data_no_NA$shrub_TH == 1 & Voxeled_data_no_NA$understory_TH == 1 & Voxeled_data_no_NA$sum == 2] <- "Shrubs_and_Understory"
  
  Voxeled_data_no_NA$Classifications[Voxeled_data_no_NA$sum == 3] <- "All_3"
  
  Voxeled_data_no_NA$Classifications[Voxeled_data_no_NA$sum == 0] <- "No_ID"
  
  #Plots of structure
  #ggplot(Voxeled_data_no_NA[ , ], aes(x = num_pts, y = z)) + geom_bin2d() + facet_grid(. ~ Classifications)
  
  Voxeled_data_no_NA[ , ] %>%
    filter(Classifications %in% c("All_3")) %>%
    all3_plot <- ggplot(aes(x = num_pts, y = z)) +   geom_bin2d() + facet_grid(. ~ Classifications)
  
  Voxeled_data_no_NA[ , ] %>%
    filter(Classifications %in% c("Shrubs", "Understory", "Canopy")) %>%
    uniques_plot <- ggplot(aes(x = num_pts, y = z)) +   geom_bin2d() + facet_grid(. ~ Classifications)
  
  Voxeled_data_no_NA[ , ] %>%
    filter(Classifications %in% c("Shrubs", "Shrubs_and_Canopy", "Shrubs_and_Understory")) %>%
    shrubs_plot <- ggplot(aes(x = num_pts, y = z)) +   geom_bin2d() + facet_grid(. ~ Classifications)
  
  Voxeled_data_no_NA[ , ] %>%
    filter(Classifications %in% c("Understory", "Shrubs_and_Understory", "Understory_and_Canopy")) %>%
    understory_plot <- ggplot(aes(x = num_pts, y = z)) +   geom_bin2d() + facet_grid(. ~ Classifications)
  
  Voxeled_data_no_NA[ , ] %>%
    filter(Classifications %in% c("Canopy", "Shrubs_and_Canopy", "Understory_and_Canopy")) %>%
    canopy_plot <- ggplot(aes(x = num_pts, y = z)) +   geom_bin2d() + facet_grid(. ~ Classifications)
  
  Voxeled_data_no_NA[ , ] %>%
    filter(Classifications %in% c("No_ID", "All_3")) %>%
    noID_plot <- ggplot(aes(x = num_pts, y = z)) + geom_bin2d() + facet_grid(. ~ Classifications)
  
}



