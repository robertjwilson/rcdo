
# to be added: year range options
# figure out how to handle very large files. Use gebco bath

# think about whether multiple grids should throw a warning or error


#' @title remap a ncdf file
#' @description This function allows you to remap a netcdf horizontally and vertically to a specific latlon box
#' @param ff This is the file to move. This must be the full system path to the file.
#' @param vars Select the variables you want to regrid. If this is not given, all variables will be regridded.
#' @param lon_range longitude range. c(min_longituse, max_longitude).
#' @param lat_range latitude range. c(min_latitude, max_latitude).
#' @param date_range This is the range of dates you want. c(date_min, date_max). "day/month/year" character string format.
#' @param months Months you want. c(month_1, month_2,...)
#' @param years Months you want. c(year_1, year_2,...)
#' @param coord_rds longitudinal and latitudinal range for the regridding. c(lon_res, lat_res). If one number given the lon_res and lat_res will be the same.
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param remapping The type of remapping. bil = bilinear. nn = nearest neighbour. dis = distance weighted.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @return data frame or netcdf file.
#' @export

# need an option for cacheing results...

#'@examples

#' # Remapping NOAA world ocean atlas data to the region around the UK
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' # remapping to 1 degree resolution across all depth layers
#' nc_remap(ff, vars = "t_an", lon_range = c(-20, 10), lat_range = c(48,62), coord_res = 1)

#' # remapping to 1 degree resolution for 5, 50 and 100 metres
#' nc_remap(ff, vars = "t_an", lon_range = c(-20, 10), lat_range = c(48,62), vert_depths= c(5, 50, 100), coord_res = 1)

#'



nc_remap <- function(ff, vars = NULL, lon_range, lat_range, coord_res,date_range = NULL, months = NULL, years = NULL, out_file = NULL, vert_depths = NULL, remapping = "bil", cdo_output = FALSE) {

	if(!file_valid(ff))
		stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))

	if(!is.numeric(coord_res))
		stop("error: check coord_res format")

	if(length(coord_res) != 2){
		warning("only 1 variable given for coord_res. This will be used for both lon_res and lat_res")
		coord_res <- c(coord_res[1], coord_res[1])
	}


	# check that the vars given are actually in the file
	if(!is.null(vars)){
		var_list <- stringr::str_flatten(nc_variables(ff), collapse  = " ")
	for(vv in vars){
		if(vv %in% nc_variables(ff) == FALSE)
			stop(stringr::str_glue("variable {vv} does not appear to be in the file. Available variables are {var_list}"))

	}



	}


	if(!cdo_compatible(ff))
		stop("error: file is not cdo compatible")

		if(as.integer(system(stringr::str_c("cdo ngrids ", ff), intern = TRUE)) > 1)
			warning("error: there is more than one horizontal grid in the netcdf file. This function cannot currently handle multiple grids")


	if(remapping %nin% c("bil", "dis", "nn"))
		stop(stringr::str_glue("remapping method {remapping} is invalid"))

  # take note of the working directory, so that it can be reset to this on exit

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- tempdir()

  # copy the file to the temporary

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"), overwrite = TRUE)
  setwd(temp_dir)

  if(getwd() == init_dir)
  	stop("error: there was a problem changing the directory")
  # we need to set up a grid so that cdo can do a remapping. Easy enough

  lon_res <- coord_res[1]
  lat_res <- coord_res[2]

  lon_range <- bin_value(lon_range, lon_res)
  lat_range <- bin_value(lat_range, lat_res)

  x_size <- length(seq(min(lon_range), max(lon_range), lon_res))
  y_size <- length(seq(min(lat_range), max(lat_range), lat_res))
  c(
    "gridtype = lonlat", stringr::str_glue("xsize = {x_size}"),
    stringr::str_glue("ysize = {y_size}"),
    stringr::str_glue("xfirst = {min(lon_range)}"),
    stringr::str_glue("yfirst = {min(lat_range)}"),
    stringr::str_glue("xinc= {lon_res}"),
    stringr::str_glue("yinc= {lat_res}")
  ) %>%
    readr::write_lines("mygrid")

  # remove anything from the temporary folder to make sure there are no clashes etc.

  if (file.exists(stringr::str_c(temp_dir, "/raw_clipped.nc"))) {
    file.remove(stringr::str_c(temp_dir, "/raw_clipped.nc"))
  }


  # ad the variables we need to add attributes for


  # Now, we need to select the variables we are interested in....
  if (!is.null(vars)) {
    system(stringr::str_c("cdo selname,", stringr::str_flatten(vars, ","), " raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw.nc")
  }


  # now, subset it to a specific latlonbox
  # to be added later
#
#   lon_min <- max(-180, lon_range[1] - 5)
#   lat_min <- max(-90, lat_range[1] - 5)
#   lon_max <- max(180, lon_range[2] + 5)
#   lat_max <- max(90, lat_range[2] + 5)
#
#     system(stringr::str_c("cdo sellonlatbox,", lon_min, ",", lat_min, ",", lon_max, ",", lat_max, " raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
#     file.rename("dummy.nc", "raw.nc")


   if (!is.null(vert_depths)) {
     vert_depths <- stringr::str_flatten(vert_depths, ",")

     system(stringr::str_c("cdo intlevel,", vert_depths, " ", "raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
     file.rename("dummy.nc", "raw.nc")
   }

  file.rename("raw.nc", "raw_clipped.nc")

  if(!is.null(date_range)){
  	min_date <- lubridate::dmy(date_range[1])
  	max_date <- lubridate::dmy(date_range[2])

  	if(is.na(min_date) | is.na(max_date))
  		stop("error check date range supplied")


  	system(stringr::str_c("cdo seldate,", min_date, ",", max_date, " raw_clipped.nc dummy.nc"), ignore.stderr = TRUE)
  	file.rename("dummy.nc", "raw_clipped.nc")
  }


  if(!is.null(months)){
  	file_months <- system(stringr::str_c("cdo showmon ", "raw_clipped.nc"), intern = TRUE, ignore.stderr = TRUE) %>%
  		stringr::str_split(" ") %>%
  		.[[1]] %>%
  		as.integer()
  	num_months <- 0
  	for(mm in months){
  		if(mm %in%unique(file_months[complete.cases(file_months)]))
  			num_months <- num_months + 1
  	}

  	if(num_months == 0)
  		stop("error: check months supplied")

  	system(stringr::str_c("cdo selmonth,", stringr::str_flatten(months, ","), " raw_clipped.nc dummy.nc"), ignore.stderr = TRUE)
  	file.rename("dummy.nc", "raw_clipped.nc")
  }

  if(!is.null(years)){
  	file_years <- system(stringr::str_c("cdo showyear ", "raw_clipped.nc"), intern = TRUE, ignore.stderr = TRUE) %>%
  		stringr::str_split(" ") %>%
  		.[[1]] %>%
  		as.integer()
  	num_years <- 0
  	for(yy in years){
  		if(yy %in%unique(file_years[complete.cases(file_years)]))
  			num_years <- num_years + 1
  	}

  	if(num_years == 0)
  		stop("error: check years supplied")
  	system(stringr::str_c("cdo selyear,", stringr::str_flatten(years, ","), " raw_clipped.nc dummy.nc"), ignore.stderr = TRUE)
  	file.rename("dummy.nc", "raw_clipped.nc")
  }

  system(stringr::str_c("cdo gen", remapping, ",mygrid raw_clipped.nc remapweights.nc"), ignore.stderr = (cdo_output == FALSE))
  system(stringr::str_c("cdo remap", remapping, ",mygrid raw_clipped.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  file.rename("dummy.nc", "raw_clipped.nc")

  # at this stage, we need to output a data frame if asked

  if (is.null(out_file)) {
    file_name <- "raw_clipped.nc"
    depths <- system(stringr::str_c("cdo showlevel ", file_name), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
      stringr::str_split(" ") %>%
      .[[1]] %>%
      as.numeric()

    depths <- depths[complete.cases(depths)]

    times <- system(stringr::str_c("cdo showtimestamp ", file_name), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
      stringr::str_split(" ") %>%
      .[[1]]
    times <- times[nchar(times) > 0]

    # now, pull in the longitudes and latitudes...
    nc_raw <- ncdf4::nc_open(file_name)
    nc_lon <- ncdf4::ncvar_get(nc_raw, "lon")
    nc_lat <- ncdf4::ncvar_get(nc_raw, "lat")

    # this is coded on the assumption that when there is only one depth and time, those dimensions will be collapsed to nothing


    nc_grid <- eval(parse(text = stringr::str_c(
      "expand.grid(Longitude = nc_lon, Latitude = nc_lat",
      ifelse(length(depths) > 1, ",Depth = depths", ""),
      ifelse(length(times) > 1, ",Time = times", ""),
      ")"
    )))


    if(is.null(vars)){
    	vars <- system(stringr::str_c("cdo showname ", file_name), intern = TRUE, ignore.stderr = TRUE)
    	vars <- stringr::str_split(vars, " ") %>%
    		.[[1]]
    	vars <- vars[nchar(vars) > 0]

    }

    for (vv in vars) {
      nc_var <- ncdf4::ncvar_get(nc_raw, vv)
      nc_grid$var <- nc_var %>% as.numeric()
      names(nc_grid)[ncol(nc_grid)] <- vv
    }

    nc_grid <- nc_grid %>%
      # tidyr::drop_na() %>%
      dplyr::as_tibble()

    ncdf4::nc_close(nc_raw)
    return(nc_grid)
  }


  # save the file, if that's what you chose to do
  # change the working directory back to the original

  setwd(init_dir)

  file.copy(stringr::str_c(temp_dir, "/raw_clipped.nc"), out_file, overwrite = TRUE)
}

