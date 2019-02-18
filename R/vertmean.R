
# to be added: generalize this to cover vertical min, max etc.

# the function should perhaps be modified so that it takes ..., and then sends ... to remap


#' @title nc_vertmean
#' @description This function allows you to remap a netcdf horizontally and vertically to a specific latlon box
#' @param ff This is the file to move. This must be the full system path to the file.
#' @param vars Select the variables you want to regrid. If this is not given, all variables will be regridded.
#' @param lon_range longitude range. c(min_longituse, max_longitude).
#' @param lat_range latitude range. c(min_latitude, max_latitude).
#' @param date_range This is the range of dates you want. c(date_min, date_max). "day/month/year" character string format.
#' @param vert_scale This is the vertical scale you want. c(min_depth, max_depth, cell size).
#' @param months Months you want. c(month_1, month_2,...)
#' @param years Months you want. c(year_1, year_2,...)
#' @param coord_rds longitudinal and latitudinal range for the regridding. c(lon_res, lat_res).
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param remapping The type of remapping. bil = bilinear. nn = nearest neighbour. dis = distance weighted.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @return data frame or netcdf file.
#' @export

# need an option for cacheing results...

nc_vertmean <- function(ff, vars = NULL, lon_range = NULL, lat_range = NULL, coord_res = NULL, date_range = NULL, months = NULL, years = NULL, out_file = NULL, vert_scale = NULL, remapping = "bil", cdo_output = FALSE) {



	# check that the vars given are actually in the file
	if(!is.null(vars)){
		var_list <- stringr::str_flatten(nc_variables(ff), collapse  = " ")
		for(vv in vars){
			if(vv %in% nc_variables(ff) == FALSE)
				stop(stringr::str_glue("variable {vv} does not appear to be in the file. Available variables are {var_list}"))
		}
	}


  if (!file_valid(ff)) {
    stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
  }
  # first, make sure the vertical grid provided is acceptable
  if (!is.numeric(vert_scale)) {
    stop("error vertical scale is not numeric")
  }

  if (vert_scale[1] > vert_scale[2]) {
    stop("error: check ordering of vertical scale")
  }

  if (vert_scale[1] < 0 | vert_scale[2] < 0) {
    stop("error: check that vertical scale is positive")
  }

  if (vert_scale[3] < 0) {
    stop("error: check ordering of vertical scale")
  }

  vert_depths <- seq(vert_scale[1], vert_scale[2], vert_scale[3])

  if (!cdo_compatible(ff)) {
    stop("error: file is not cdo compatible")
  }

  if (remapping %nin% c("bil", "dis", "nn")) {
    stop(stringr::str_glue("remapping method {remapping} is invalid"))
  }

  if (as.integer(system(stringr::str_c("cdo ngrids ", ff), intern = TRUE)) > 1) {
    stop("error: there is more than one horizontal grid in the netcdf file. This function cannot currently handle multiple grids")
  }

  # make sure all of the relevant inputs have been supplied for horizontal gridding
  grid_check <- (as.integer(!is.null(lon_range)) +  as.integer(!is.null(lat_range)) + as.integer(!is.null(coord_res)))
	# print(as.integer(!is.null(lon_range) + !is.null(lat_range) + !is.null(coord_res)))
  if (grid_check >= 1 & grid_check < 3)
      stop("error: check lon_range, lat_range and coord_res are all supplied")

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # to be safe, if the working directory is the CAO one, switch it to the home directory at this point

  setwd("~")

  if (!file.exists(ff)) {
    stop(stringr::str_glue("File {ff} either does not exist or does not have the full path"))
  }

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- tempdir()

  setwd(temp_dir)

  if (getwd() != temp_dir) {
    stop("error: unable to reset the working directory to a temporary folder, for whatever reason")
  }

  # we need to set up a grid so that cdo can do a remapping. Easy enough

  # remove anything from the temporary folder to make sure there are no clashes etc.

  if (file.exists(stringr::str_c(temp_dir, "/raw.nc"))) {
    file.remove(stringr::str_c(temp_dir, "/raw.nc"))
  }
  if (file.exists(stringr::str_c(temp_dir, "/raw_clipped.nc"))) {
    file.remove(stringr::str_c(temp_dir, "/raw_clipped.nc"))
  }

  # copy the file to the temporary

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"))

  # ad the variables we need to add attributes for

  # Now, we need to select the variables we are interested in....
  if (!is.null(vars)) {
    system(stringr::str_c("cdo selname,", stringr::str_flatten(vars, ","), " raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw.nc")
  }


  if (!is.null(lon_range) | !is.null(lat_range) | !is.null(coord_res)) {
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
  }

  if (!is.null(vert_depths)) {
    vert_depths <- stringr::str_flatten(vert_depths, ",")

    system(stringr::str_c("cdo intlevel,", vert_depths, " ", "raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw.nc")
  }

  file.rename("raw.nc", "raw_clipped.nc")

  if (!is.null(date_range)) {
    min_date <- lubridate::dmy(date_range[1])
    max_date <- lubridate::dmy(date_range[2])

    if (is.na(min_date) | is.na(max_date)) {
      stop("error check date range supplied")
    }

    system(stringr::str_c("cdo seldate,", min_date, ",", max_date, " raw_clipped.nc dummy.nc"), ignore.stderr = TRUE)
    file.rename("dummy.nc", "raw_clipped.nc")
  }


  if (!is.null(months)) {
    file_months <- system(stringr::str_c("cdo showmon ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
      stringr::str_split(" ") %>%
      .[[1]] %>%
      as.integer()
    num_months <- 0
    for (mm in months) {
      if (mm %in% unique(file_months[complete.cases(file_months)])) {
        num_months <- num_months + 1
      }
    }

    if (num_months == 0) {
      stop("error: check months supplied")
    }

    system(stringr::str_c("cdo selmonth,", stringr::str_flatten(months, ","), " raw_clipped.nc dummy.nc"), ignore.stderr = TRUE)
    file.rename("dummy.nc", "raw_clipped.nc")
  }

  if (!is.null(years)) {
    file_years <- system(stringr::str_c("cdo showyear ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
      stringr::str_split(" ") %>%
      .[[1]] %>%
      as.integer()
    num_years <- 0
    for (yy in years) {
      if (yy %in% unique(file_years[complete.cases(file_years)])) {
        num_years <- num_years + 1
      }
    }

    if (num_years == 0) {
      stop("error: check years supplied")
    }
    system(stringr::str_c("cdo selyear,", stringr::str_flatten(years, ","), " raw_clipped.nc dummy.nc"), ignore.stderr = TRUE)
    file.rename("dummy.nc", "raw_clipped.nc")
  }

  if (!is.null(lon_range)) {
    system(stringr::str_c("cdo gen", remapping, ",mygrid raw_clipped.nc remapweights.nc"), ignore.stderr = (cdo_output == FALSE))
    system(stringr::str_c("cdo remap", remapping, ",mygrid raw_clipped.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw_clipped.nc")
  }

  # calculate the vertical mean

  system(stringr::str_c("cdo vertmean raw_clipped.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  file.rename("dummy.nc", "raw_clipped.nc")

  # at this stage, we need to output a data frame if asked

  if (is.null(out_file)) {
    print("converting to a data frame")
    nc_grid <- nc_read("raw_clipped.nc")
    return(nc_grid)
  }
  # save the file, if that's what you chose to do
  # change the working directory back to the original

  setwd(init_dir)
  file.copy(stringr::str_c(temp_dir, "/raw_clipped.nc"), out_file, overwrite = TRUE)
}



