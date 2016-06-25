####### Function for retrieving files

File_Retrieval <- function(LAS_folder, DTM_folder) {

  las_files <- list.files(LAS_folder)
  DTM_files <- list.files(DTM_folder)

  for (i in 1:length(las_files)) {
    LiDAR_file <- las_files[i]
    site_info <- Kmisc::strip_extension(LiDAR_file)
    site_info <- Kmisc::strip_extension(site_info)
    DTM <- list.files("/Volumes/AOP-NEON1-4/D17/SOAP/2013/SOAP_L1/SOAP_Lidar/Classified_point_cloud/las_files/file_for_subset/DTM_files/", pattern = site_info)
    full_dtm_path <- paste0("/Volumes/AOP-NEON1-4/D17/SOAP/2013/SOAP_L1/SOAP_Lidar/Classified_point_cloud/las_files/file_for_subset/DTM_files/", DTM)
    full_lidar_path <- paste0("/Volumes/AOP-NEON1-4/D17/SOAP/2013/SOAP_L1/SOAP_Lidar/Classified_point_cloud/las_files/file_for_subset/las_files/", LiDAR_file)
    Canopy_Structure(full_dtm_path, full_lidar_path)
  }
}
