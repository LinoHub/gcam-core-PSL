# Net CDF optimization 

# # Open and describe the NetCDF w/ raster
# netcdf = raster("demeter-2.0/demeter/GCAM_demeter_protection_scenario/demeter-2.0/demeter/GCAM_demeter_protection_scenario/outputs/Reference/spatial_landcover_netcdf/_demeter_Reference_2020.nc")
# proj4string(netcdf)=CRS("+init=EPSG:4326")
# print(netcdf)
# class(netcdf)

# Open and describe the NetCDF w/ ncdf4
ncin <- nc_open("demeter-2.0/demeter/GCAM_demeter_protection_scenario/demeter-2.0/demeter/GCAM_demeter_protection_scenario/outputs/Reference/spatial_landcover_netcdf/_demeter_Reference_2020.nc", readunlim=FALSE)
# print(ncin)
# class(ncin)
print(paste("The file has",ncin$nvars,"variables"))

# Get longitude and latitude
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)

# Get a variable and its attributes 
basin_var = ncin$var[[1]]
region_var = ncin$var[[2]]
water_var = ncin$var[[3]]
shrub_var = ncin$var[[4]]
grassland_var = ncin$var[[5]]

basin_array <- ncvar_get(ncin, basin_var)
shrub_array = ncvar_get(ncin, shrub_var)
grassland_array <- ncvar_get(ncin, grassland_var)

# dim(shrub_array)
# ?ncvar_get

# Reshape raster to rectangular 
library(lattice)
library(RColorBrewer)

# Quick map with lattice 
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(grassland_array ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))))

# Create a dataframe 
# First, matrix of lons and lats with expand.grid and as.matrix
lonlat <- as.matrix(expand.grid(lon, lat))
dim(lonlat)

# Second, create vector of var values 
shrub_vec <- as.vector(shrub_array)
grassland_vec <- as.vector(grassland_array)
length(shrub_vec)

# Thrid, create df and add names 
shrub_df01 <- data.frame(cbind(lonlat, shrub_vec))
grassland_df01 <- data.frame(cbind(lonlat, grassland_vec))

names(shrub_df01) <- c("lon", "lat", paste("shrub"))
names(grassland_df01) <- c("lon", "lat", paste("grassland"))

# Fourth, remove NA values 
shrub_df01_na = na.omit(shrub_df01)
grassland_df01_na <- na.omit(grassland_df01)

# Fifth, merge the DF by lat and lon
merge_df01 <- merge(shrub_df01_na, grassland_df01_na, by = c("lon", "lat"))

# Sixth, Write out the dataframe
dir.create("csv_files_netcdf")
csvpath <- "csv_files_netcdf/"
csvname <- "merge_df01.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(merge_df01), csvfile, row.names = FALSE, sep=",")




# Now try to automatize everything 
# Start with a function based on the variables of the ncin 

# Set the list of variables 
variables = names(ncin$var)

# Initialize the receiving dataframe
merge_df = read.csv("data/lonlat_coord")
merge_df = merge_df[,2:3]

# Function to create dataframes from the raster
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
  dplyr::select(!contains("irrigated|rainfed|otherarableland")) %>% 
  dplyr::select(!c(basin_id, region_id, water))
