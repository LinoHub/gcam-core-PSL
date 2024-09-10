# Potential Species Loss on GCAM Land outputs 

# Load packages ----

library(rgcam)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sf)
library(s2)
# library(mapview)
library(rredlist)
library(tibble)
library(ggplot2)
library(mapdata)
library(mapproj)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringi)
library(purrr)
library(raster)
library(ncdf4)
library(shapefiles)
library(xlsx)
library(openxlsx)
library(terra)
library(reticulate)

# remove.packages("reticulate")
# install.packages("reticulate")

# Create and load project ----
db_path <- "output/"
db_name <- "database_basexdb"
query_name <- "data/DetailedLU_query.xml"

# Update scenario name 
prj_name <- "PSL_Project.dat"
scen_name <- c("Reference", "Reference_UCT")

# Upload basin mapping
basin.id <- read_excel("data/basin_to_country_mapping.xlsx",col_names = TRUE)

# Extract scenario data from database
conn <- localDBConn(db_path, db_name)

# Extract information specified by XML query for each scenario
proj <- addScenario(conn, prj_name, scen_name, query_name)

# Format GCAM land use outputs to fit as Demeter inputs ----
det.LU <- getQuery(proj, "detailed land allocation") %>% separate(landleaf, into = c("landclass", "GLU_name", "irrtype", "hiORlo"), sep = "_") %>% 
  mutate(landclass = case_when (!is.na(irrtype) ~ paste0(landclass,irrtype, hiORlo),TRUE ~ landclass)) %>%
  merge(basin.id,by="GLU_name") %>% dplyr::select(region, landclass, GCAM_basin_ID, year, value, scenario)  %>% 
  rename("metric_id"="GCAM_basin_ID") %>% spread(year, value) %>% dplyr::select(-"1975")

# Get unique scenario names
scenario_names <- unique(det.LU$scenario)

# Loop to create one file per scenario in "input/projected" demeter folder and configuration files ----
for (scenario_name in scenario_names) {
  
  # # DEBUG
  # scenario_name = "Reference"
  
  # Filter the dataframe for the current scenario
  filtered_df <- det.LU[det.LU$scenario == scenario_name, ]
  
  # Define the file paths for saving the filtered dataframe and the configuration file
  file_path <- paste0("demeter-2.0/demeter/GCAM_Demeter_protection_scenario/inputs/projected/Scenario_", scenario_name, ".csv")
  config_path <- paste0("demeter-2.0/demeter/GCAM_Demeter_protection_scenario/config_files/Scenario_", scenario_name, ".ini")
  
  # Config file parameters
  projected_file <- paste0("Scenario_", scenario_name, ".csv")
  
  # Create the content for the config file
  config_file <- paste0(
    "[STRUCTURE]\n",
    "run_dir =                       demeter-2.0/demeter/GCAM_Demeter_protection_scenario\n",
    "in_dir =                        inputs\n",
    "out_dir =                       outputs\n\n",
    "[INPUTS]\n",
    "allocation_dir =                allocation\n",
    "observed_dir =                  observed\n",
    "constraints_dir =               constraints\n",
    "projected_dir =                 projected\n\n",
    "[[ALLOCATION]]\n",
    "spatial_allocation_file =       gcam_regbasin_moirai_v3_type5_5arcmin_observed_alloc.csv\n",
    "gcam_allocation_file =          gcam_regbasin_moirai_v3_type5_5arcmin_projected_alloc.csv\n",
    "kernel_allocation_file =        gcam_regbasin_moirai_v3_type5_5arcmin_kernel_weighting.csv\n",
    "transition_order_file =         gcam_regbasin_moirai_v3_type5_5arcmin_transition_alloc.csv\n",
    "treatment_order_file =          gcam_regbasin_moirai_v3_type5_5arcmin_order_alloc.csv\n",
    "constraints_file =              gcam_regbasin_moirai_v3_type5_5arcmin_constraint_alloc.csv\n\n",
    "[[OBSERVED]]\n",
    "observed_lu_file =              baselayer_GCAM6_WGS84_5arcmin_2022_HighBioDiversity.zip\n\n",
    "[[PROJECTED]]\n",
    "projected_lu_file =             ", projected_file, "\n\n",
    "[[MAPPING]]\n",
    "region_mapping_file =           gcam_regions_32.csv\n",
    "basin_mapping_file =            gcam_basin_lookup.csv\n\n",
    "[PARAMS]\n",
    "# scenario name\n",
    "scenario =                      ", scenario_name, "\n\n",
    "# run description\n",
    "run_desc =                      run_description_null\n\n",
    "# spatial base layer id field name\n",
    "observed_id_field =             fid\n\n",
    "# first year to process\n",
    "start_year =                    2015\n\n",
    "# last year to process\n",
    "end_year =                      2050\n\n",
    "# enter 1 to use non-kernel density constraints, 0 to ignore non-kernel density constraints\n",
    "use_constraints =               1\n\n",
    "# the spatial resolution of the observed spatial data layer in decimal degrees\n",
    "spatial_resolution =            0.0833333\n\n",
    "# error tolerance in km2 for PFT area change not completed\n",
    "errortol =                      0.001\n\n",
    "# time step in years\n",
    "timestep =                      5\n\n",
    "# factor to multiply the projected land allocation by\n",
    "proj_factor =                   1000\n\n",
    "# from 0 to 1; ideal fraction of LUC that will occur during intensification, the remainder will be expansion\n",
    "intensification_ratio =         0.8\n\n",
    "# activates the stochastic selection of grid cells for expansion of any PFT\n",
    "stochastic_expansion =          1\n\n",
    "# threshold above which grid cells are selected to receive a given land type expansion; between 0 and 1, where 0 is all\n",
    "#     land cells can receive expansion and set to 1 only the grid cell with the maximum likelihood will expand.  For\n",
    "#     a 0.75 setting, only grid cells with a likelihood >= 0.75 x max_likelihood are selected.\n",
    "selection_threshold =           0.75\n\n",
    "# radius in grid cells to use when computing the kernel density; larger is smoother but will increase run-time\n",
    "kernel_distance =               30\n\n",
    "# create kernel density maps; 1 is True\n",
    "map_kernels =                   1\n\n",
    "# create land change maps per time step per land class\n",
    "map_luc_pft =                   0\n\n",
    "# create land change maps for each intensification and expansion step\n",
    "map_luc_steps =                 0\n\n",
    "# creates maps of land transitions for each time step\n",
    "map_transitions =               0\n\n",
    "# years to save data for, default is all; otherwise a semicolon delimited string e.g, 2005;2050\n",
    "target_years_output =           all\n\n",
    "# save tabular spatial landcover as CSV; define tabular_units below (default sqkm)\n",
    "save_tabular =                  0\n\n",
    "# untis to output tabular data in (sqkm or fraction)\n",
    "tabular_units =                 fraction\n\n",
    "# exports CSV files of land transitions for each time step in km2\n",
    "save_transitions =              0\n\n",
    "# create a NetCDF file of land cover percent for each year by grid cell containing each land class\n",
    "save_netcdf_yr =                1\n"
  )
  
  # Save the filtered dataframe to CSV as "projected" Demeter input 
  write.csv(filtered_df, file = file_path, row.names = FALSE)
  write(config_file, file = config_path)
  
  # Import Python module and Run Demeter (approx. 50 min per scenario) ----
  demeter <- reticulate::import("demeter")
  sys <- reticulate::import("sys")
  config_path = "demeter-2.0\\demeter\\GCAM_demeter_protection_scenario\\config_files\\"
  config_name = paste0("Scenario_", scenario_name, ".ini")
  config_file = paste0(config_path, config_name)
  demeter$run_model(config_file=config_file, write_outputs=TRUE)
  
}

# Extract surfaces by land use type from the netCDF files -----
year <- c('2020','2050')
output_year <- data.frame(year)
areas_land_types <- read.csv("data/Coordinates.csv")

# Create path for specific scenario
dir_demeter <- "demeter-2.0/demeter/GCAM_demeter_protection_scenario/demeter-2.0/demeter/GCAM_demeter_protection_scenario/outputs"

# List and rename files ----
files <- list.files(dir_demeter)

# Loop through each file
for (file in files) {
  # Full path of the original file
  old_file_path <- file.path(dir_demeter, file)
  # Use sub to extract the scenario name, everything before the first underscore
  new_name <- sub("_2024.+", "", file)
  # Full path for the new file name
  new_file_path <- file.path(dir_demeter, new_name)
  # Rename the file
  file.rename(old_file_path, new_file_path)
}

# Create outputs folders ----
dir.create("results/")
dir.create("results/QGIS input files/")
dir.create("results/Processed Demeter outputs/")
dir.create("results/Output data by ecoregion/")

# Load & Process Ecoregions shp ---- 
ecoregions_shp <- st_read("data/Ecoregions_shp/wwf_terr_ecos.shp")
# Process ecoregion_shp
class(ecoregions_shp)
crs(ecoregions_shp)
# Check the geometries
ecoregions_valid <- st_is_valid(ecoregions_shp)
# Identify invalid geometries
invalid_ecoregions <- ecoregions_shp[!ecoregions_valid, ]
# Fix the geometries 
ecoregions_shp = st_make_valid(ecoregions_shp)
# Simplify ecoregions
ecoregions_id <- ecoregions_shp %>% 
  dplyr::select(all_of(c("OBJECTID", "eco_code")))
length(unique(ecoregions_id$OBJECTID))
# Read file with the Ecoregion names from Chaudhary and Brookes (2018)
ecoregions_ID <- read.xlsx("data/Ecoregion ID.xlsx",1)


# Create the NetCDF Files ----
for (scenario_name in scenario_names) {

  
    # Need to make sure the receiving frame has 2020 and 2050 ################
    
    for (j in 1:2) {
  
    # Initialize the receiving dataframe 
    merge_df = read.csv("data/lonlat_coord")
    merge_df = merge_df[,2:3]
      
    # Create path components 
    netcdffolder <- "spatial_landcover_netcdf"
    nc_file <- paste0("_demeter_",scenario_name,"_",output_year[j,1],".nc")
    NetCDFfiles_path <- file.path(dir_demeter,scenario_name,netcdffolder,nc_file)
    
    # Read the nc file from Demeter outputs
    ncin <- nc_open(NetCDFfiles_path)
    
    # Set the list of variables 
    variables = names(ncin$var)
    
    # Get longitude and latitude
    lon <- ncvar_get(ncin,"longitude")
    nlon <- dim(lon)
    head(lon)
    lat <- ncvar_get(ncin,"latitude")
    nlat <- dim(lat)
    head(lat)
    lonlat <- as.matrix(expand.grid(lon, lat))
    dim(lonlat)
    
      # Function to create dataframes from the variables of the nc file
      for (i in 1:length(variables)) {
        
        # i = 5
        landuse_df <- na.omit(data.frame(cbind(lonlat, as.vector(ncvar_get(ncin, ncin$var[[i]])))))
        names(landuse_df) <- c("lon", "lat", paste(ncin$var[[i]]$longname)) # instead of landuse, the longname of the variable i
        # assign(paste(ncin$var[[i]]$longname, "df", sep="_"), landuse_df)
        merge_df = merge(merge_df, landuse_df)
        print(ncin$var[[i]]$longname)
        
      }
    
    # Now I want to create a new column with the sum of the columns that finish by "irrigated" 
    merge_df$crop_irr = rowSums(merge_df[, grepl("irrigated", names(merge_df))])
    merge_df$crop_rfd = rowSums(merge_df[, grepl("rainfed|otherarableland", names(merge_df))])

    merge_df = merge_df %>% 
      dplyr::select(!contains("irrigated")) %>% 
      dplyr::select(!contains("rainfed")) %>% 
      dplyr::select(!contains("otherarableland")) %>%
      dplyr::select(!c(basin_id, region_id, water))
    
    # assign(paste(ncin$var[[i]]$longname, "df", sep="_"), merge_df)
    assign(paste("merge", j, "df", sep="_"), merge_df)
    
    }
  
  # Merge 2020 and 2050 
  merge_df = merge(merge_1_df, merge_2_df, by = c("lon", "lat"))
  
  # Geography parameters ----
  areas_land_types <- merge_df %>% rename ("longitude"="lon") %>% rename ("latitude"="lat")
  areas_land_types_shp <- areas_land_types
  coordinates(areas_land_types_shp)=~longitude+latitude
  proj4string(areas_land_types_shp)<- CRS("+proj=longlat +datum=WGS84")
  
  # # Not necessary actually -- Save processed Demeter outputs csv and shp ----
  # shapefile(areas_land_types_shp, paste0("QGIS input files/",scenario_name,".shp"))
  # write.csv(areas_land_types,paste0("Processed Demeter outputs/",scenario_name,".csv"),row.names=TRUE)

  # Ecoregions aggregation processing into R -----
  
  # # IF NECESSARY -- Load the Demeter outputs, otherwise it is here as "areas_land_types_shapefile" 
  # setwd("C:/GCAM/GCAM_7.0_Theo_2/QGIS input files")
  # areas_land_types_shp_2 <- st_read("QGIS input files/Reference.shp")
  
    
  areas_land_types_shp = st_as_sf(areas_land_types_shp)
    
  # areas_land_types_shp <- st_read("QGIS input files/gcam6.shp")
    
  
  # Unique geometries
  unique(st_geometry_type(areas_land_types_shp$geometry))
  
  # Check the classes
  class(areas_land_types_shp)
  crs(areas_land_types_shp)
  areas_land_types_shp = st_transform(areas_land_types_shp, crs=4326)
  
  # Transform an object of SP class into SF (if necessary)
  # areas_land_types_sf = as(areas_land_types_shapefile, "sf")
  # class(areas_land_types_sf)
  
  # Check the geometries
  areas_land_types_valid <- st_is_valid(areas_land_types_shp)
  
  # Identify invalid geometries
  invalid_areas_land_types <- areas_land_types_shp[!areas_land_types_valid, ]
  
  # Perform test on subset with 100k lines
  # joined_shp_id = st_join(areas_land_types_shp[1:100000,], ecoregions_id, join = st_intersects)
  
  # Join attributes by location 
  joined_shp_id = st_join(areas_land_types_shp, ecoregions_id, join = st_intersects) # way too long, need to simply both with index 
  
  # NEED TO TRY WITH TWO YEARS FROM HERE ######################################
  
  # Aggregate per land use, dropping geometry
  joined_shp_agg <- joined_shp_id %>%
    st_drop_geometry() %>%
    group_by(OBJECTID) %>% 
    summarise(shrubland.x = sum(shrubland.x),
              protected_shrubland.x = sum(protected_shrubland.x),
              grassland.x = sum(grassland.x),
              protected_grassland.x = sum(protected_grassland.x),
              tundra.x = sum(tundra.x),
              forest.x = sum(forest.x),
              protected_forest.x = sum(protected_forest.x),
              pasture.x = sum(pasture.x),
              protected_pasture.x = sum(protected_pasture.x),
              crop_irr.x = sum(crop_irr.x),
              crop_rfd.x = sum(crop_rfd.x),
              rockicedesert.x = sum(rockicedesert.x),
              urbanland.x = sum(urbanland.x),
              shrubland.y = sum(shrubland.y),
              protected_shrubland.y = sum(protected_shrubland.y),
              grassland.y = sum(grassland.y),
              protected_grassland.y = sum(protected_grassland.y),
              tundra.y = sum(tundra.y),
              forest.y = sum(forest.y),
              protected_forest.y = sum(protected_forest.y),
              pasture.y = sum(pasture.y),
              protected_pasture.y = sum(protected_pasture.y),
              crop_irr.y = sum(crop_irr.y),
              crop_rfd.y = sum(crop_rfd.y),
              rockicedesert.y = sum(rockicedesert.y),
              urbanland.y = sum(urbanland.y)) %>% 
    ungroup() %>% 
    full_join(ecoregions_shp[,c(1,18)]) %>%
    rename(ECOREGION_CODE = eco_code) %>%
    mutate_at(vars(-ECOREGION_CODE), ~replace(., is.na(.), 0))
  
  # joined_shp_agg
  
  # # Check the final sum -- Problem: There are many points without polygons
  # joined_shp_full <- joined_shp_agg %>%
  #   summarise(shr.x = sum(shr.x),
  #             ptshrub.x = sum(ptshrub.x),
  #             grass.x = sum(grass.x),
  #             ptgrass.x = sum(ptgrass.x),
  #             tundra.x = sum(tundra.x),
  #             forest.x = sum(forest.x),
  #             protected_forest.x = sum(protected_forest.x),
  #             pasture.x = sum(pasture.x),
  #             protected_pasture.x = sum(protected_pasture.x),
  #             irrcrop.x = sum(irrcrop.x),
  #             rfcrop.x = sum(rfcrop.x),
  #             rid.x = sum(rid.x),
  #             urban.x = sum(urban.x),
  #             shr.y = sum(shr.y),
  #             ptshrub.y = sum(ptshrub.y),
  #             grass.y = sum(grass.y),
  #             ptgrass.y = sum(ptgrass.y),
  #             tundra.y = sum(tundra.y),
  #             forest.y = sum(forest.y),
  #             protected_forest.y = sum(protected_forest.y),
  #             pasture.y = sum(pasture.y),
  #             protected_pasture.y = sum(protected_pasture.y),
  #             irrcrop.y = sum(irrcrop.y),
  #             rfcrop.y = sum(rfcrop.y),
  #             rid.y = sum(rid.y),
  #             urban.y = sum(urban.y))
  
  # Export final files as csv ----
  
  # Incorporate the ecoregions names from Chaudhary and Brookes (2018) to the Results matrix obtained from QGIS with data on land use type area changes over time
  # Conversion function from square km to square meters
  sqm <- function(x, na.rm = FALSE) (x*1000000)
  
  joined_shp_extended <- joined_shp_agg %>% 
    merge(ecoregions_ID,by="ECOREGION_CODE") %>%  
    relocate(ECO_NAME, .after=ECOREGION_CODE) %>%  
    group_by(ECOREGION_CODE,ECO_NAME) %>%
    summarise(across(where(is.numeric), sum)) %>% 
    dplyr::select(-OBJECTID) %>% 
    mutate_if(is.numeric, sqm, na.rm = FALSE) %>% 
    ungroup() %>% 
    as.data.frame() %>% 
    mutate(shrubland.diff = shrubland.x - shrubland.y,
              protected_shrubland.diff = protected_shrubland.x - protected_shrubland.y,
              grassland.diff = grassland.x - grassland.y,
              protected_grassland.diff = protected_grassland.x - protected_grassland.y,
              tundra.diff = tundra.x - tundra.y,
              forest.diff = forest.x - forest.y,
              protected_forest.diff = protected_forest.x - protected_forest.y,
              pasture.diff = pasture.x - pasture.y,
              protected_pasture.diff = protected_pasture.x - protected_pasture.y,
              crop_irr.diff = crop_irr.x - crop_irr.y,
              crop_rfd.diff = crop_rfd.x - crop_rfd.y,
              rockicedesert.diff = rockicedesert.x - rockicedesert.y,
              urbanland.diff = urbanland.x - urbanland.y)
  
  # scen_name_shp = "scenario_name"
  
  # joined_shp_extended[is.na(joined_shp_extended)] <- 0
  # scen_outputfile_extended <- as.data.frame(scen_outputfile_extended) %>% dplyr::select(1:28)
  
  write.xlsx(joined_shp_extended,paste0("results/Output data by ecoregion/",scenario_name,"_LUC_2020_2050.xlsx") ,
             overwrite = TRUE, rowNames=TRUE, colNames=TRUE)


} 





