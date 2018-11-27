

# to be added
# code to work out if the date ranges are invalid


#' @title nc_clip
#'
#' @description This function allows you to clip a netcdf file to a specific latlon box
#' @param ff This is the file to clip. This must be the full system path to the file.
#' @param vars Select the variables you want. If this is not given, all variables will be regridded.
#' @param lon_range longitude range. c(min_longituse, max_longitude).
#' @param lat_range latitude range. c(min_latitude, max_latitude).
#' @param vert_range This is the range of depths for vertical clipping, if wanted. c(min_depth, max_depth)
#' @param date_range This is the range of dates you want. c(date_min, date_max). "day/month/year" character string format.
#' @param months Months you want. c(month_1, month_2,...)
#' @param years Months you want. c(year_1, year_2,...)
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @export

# need an option for cacheing results...

nc_clip <-  function(ff, vars = NULL, lon_range = c(-180, 180), lat_range = c(-90, 90), vert_range = NULL, date_range = NULL, months = NULL, years = NULL, out_file = NULL,  cdo_output = FALSE) {


	# check if vert_range is valid

	if(!is.null(vert_range)){
		if(!is.numeric(vert_range))
			stop("error: vert_range is not numeric")

		if(vert_range[2] < vert_range[1])
			stop("error: vert_range is not valid")

	}


	# check that vars is a chracter stringr

	if(!is.null(vars))
		if(!is.character(vars))
			stop("error: vars is not a character string")

	 if(!cdo_compatible(ff))
	 	stop("error: file is not cdo compatible")
	 	
	 	if(as.integer(system(stringr::str_c("cdo ngrids ", ff), intern = TRUE)) > 1)
		stop("error: there is more than one horizontal grid in the netcdf file. This function cannot currently handle multiple grids")


  # take note of the working directory, so that it can be reset to this on exit

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # to be safe, if the working directory is the CAO one, switch it to the home directory at this point
#
#   setwd("~")
#
#   if (!file.exists(ff)) {
#     stop(stringr::str_glue("File {ff} either does not exist or does not have the full path"))
#   }

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- tempdir()
  dir.exists(temp_dir)

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"), overwrite = TRUE)

  setwd(temp_dir)
  if(getwd() == init_dir)
  	stop("error: there was a problem changing the directory")

  # remove anything from the temporary folder to make sure there are no clashes etc.

  # if (file.exists(stringr::str_c(temp_dir, "/raw.nc"))) {
  #   file.remove(stringr::str_c(temp_dir, "/raw.nc"))
  # }
  if (file.exists(stringr::str_c(temp_dir, "/raw_clipped.nc"))) {
    file.remove(stringr::str_c(temp_dir, "/raw_clipped.nc"))
  }

  # copy the file to the temporary


  # Now, we need to select the variables we are interested in....
  if (!is.null(vars)) {
    system(stringr::str_c("cdo selname,", stringr::str_flatten(vars, ","), " raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw.nc")
  }

  # clip to the box

  # if(is.null(vars))
  	system(stringr::str_c("cdo sellonlatbox,",stringr::str_flatten(c(lon_range, lat_range), collapse = ","), " raw.nc dummy.nc"), ignore.stderr = TRUE)
  	# system(stringr::str_c("cdo -selname,", stringr::str_flatten(vars, ","), " -sellonlatbox,",stringr::str_flatten(c(lon_range, lat_range), collapse = ","), " raw.nc dummy.nc"), ignore.stderr = TRUE)

  file.rename("dummy.nc", "raw_clipped.nc")


    depths <- system(stringr::str_c("cdo showlevel ", "raw_clipped.nc"), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
      stringr::str_split(" ") %>%
      .[[1]] %>%
      as.numeric()

    depths <- depths[complete.cases(depths)]
    depths <- depths[depths <= vert_range[2] & depths >= vert_range[1]]

  if(!is.null(date_range)){
  	min_date <- lubridate::dmy(date_range[1])
  	max_date <- lubridate::dmy(date_range[2])

  	if(is.na(min_date) | is.na(max_date))
  		stop("error check date range supplied")


  	system(stringr::str_c("cdo seldate,", min_date, ",", max_date, " raw_clipped.nc dummy.nc"), ignore.stderr = TRUE)
    file.rename("dummy.nc", "raw_clipped.nc")
  }


  if(!is.null(vert_range)){
  	system(stringr::str_c("cdo sellevel,", stringr::str_flatten(depths, ","), " raw_clipped.nc dummy.nc"), ignore.stderr = TRUE)
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


  if (is.null(out_file)) {
    print("converting to a data frame")
    nc_grid <- nc_read("raw_clipped.nc")
    return(nc_grid)
  }


  file.copy(stringr::str_c(temp_dir, "/raw_clipped.nc"), out_file, overwrite = TRUE)
}






