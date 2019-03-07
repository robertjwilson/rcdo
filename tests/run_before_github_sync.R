library(rcdo)
library(ggplot2)
# Regridding NOAA temperature data to a depth of 5 and 30 metres in the waters around the UK
ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
# read all data into a data frame
uk_coords <- expand.grid(Longitude = seq(-20, 10, 1), Latitude = seq(48, 62, 1))

ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
nc_cellareas(ff)

# clip to a specific depth range using vert_range

nc_clip(ff, vert_range = c(1, 5))

# clip to a specific longitude and latitude box
# Clipping to the region around the UK

uk_sst <- nc_clip(ff, lon_range = c(-12, 10), lat_range = c(48, 62))

nc_dates(ff)

nc_depths(ff)

nc_format(ff)

nc_read(ff)
# By default nc_read reads in all data fields. But we probably just want to subset it
# If we only want to read in specific fields, we can use vars
nc_read(ff, vars = "t_an")

nc_variables(ff)
nc_remap(ff, vars = "t_an", coords = uk_coords)
nc_remap(ff, vars = "t_an", coords = uk_coords, vert_depths = c(5, 50))

nc_vertmean(ff, vars = "t_an", vert_scale = c(5, 30, 5))
nc_vertmean(ff, vars = "t_an", coords = uk_coords, vert_scale = c(5, 30, 5))
nc_vertsum(ff, vars = "t_an", coords = uk_coords, vert_scale = c(5, 30, 5))
