

# to do
# month and year selection options
# a way to estimate the potential size of the data frame to ask the user of they still want to read it in
# the above works right now for the lonlat case. Get it working for the curvilinear case

# need an option for cacheing results potentially...

# a smarter way to handle nas in the data frame, as some values might be na but others not
# add drop_na option

# need to modify the code so that it can run in parallel or multiple sessions, without the temporary folders clashing.

# also, add a check for the number of horizontal grids. Shouldn't read the netcdf if there is more than 1 GB(ish)


#' @title nc_read
#'
#' @description This is a quick and easy way to read a netcdf file to a data frame. It will read all or a specified list of variables into a data frame. You are able to specify a date range. This requires that grid details are clear in the netcdf file, so in some rare cases there will be an error message.
#' @param ff This is the file to read.
#' @param vars A list of variables you want to read in. Character vector. Everything is read in if this is empty.
#' @param date_range This is the range of dates you want. c(date_min, date_max). "day/month/year" character string format.
#' @param cdo_output Do you want to show the cdo output? Set to TRUE in case you want to troubleshoot errors.
#' @param dim_check The number of data points in the final data frame that will ask to continue. Set to NULL if you don't want to check.
#' @export

#'@examples

#' # Reading in data from the NOAA World Ocean Atlas sample file.
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' # if we simply want to read the data into a tibble, we just need to use nc_read

#' nc_read(ff)

#' # By default nc_read reads in all data fields. But we probably just want to subset it
#' # If we only want to read in specific fields, we can use vars

#' nc_read(ff, vars = "t_an")


nc_read <- function(ff, vars = NULL, date_range = NULL, cdo_output = FALSE, dim_check = 15e7) {

	if(!file_valid(ff))
		stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))

  if (!cdo_compatible(ff)) {
    stop("error: file is not cdo compatible")
  }
	if(as.integer(system(stringr::str_c("cdo ngrids ", ff), intern = TRUE)) > 1)
		stop("error: there is more than one horizontal grid in the netcdf file. This function cannot currently handle multiple grids")


	# check that the vars given are actually in the file
	if(!is.null(vars)){
		var_list <- stringr::str_flatten(nc_variables(ff), collapse  = " ")
		for(vv in vars){
			if(vv %in% nc_variables(ff) == FALSE)
				stop(stringr::str_glue("variable {vv} does not appear to be in the file. Available variables are {var_list}"))
		}
	}


  init_dir <- getwd()
  on.exit(setwd(init_dir))
  temp_dir <- tempdir()
  dir.exists(temp_dir)

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"), overwrite = TRUE)

  # remove anything from the temporary folder to make sure there are no clashes etc.

  setwd(temp_dir)

  if (file.exists("dummy.nc")) {
    file.remove("dummy.nc")
  }

  # copy the file to the temporary folder

  ff <- "raw.nc"

  grid_details <- system(stringr::str_c("cdo griddes ", ff), intern = TRUE, ignore.stderr = TRUE)

  grid_details <- grid_details %>%
    dplyr::as_tibble()

  grid_details <- grid_details %>%
    dplyr::mutate(Total = 0) %>%
    dplyr::mutate(Total = ifelse(startsWith(value, "yunits"), 1, Total)) %>%
    dplyr::mutate(Total = cumsum(Total)) %>%
    dplyr::filter(Total < 1)

  grid_type <- grid_details %>%
    dplyr::filter(startsWith(value, "gridtype")) %>%
    tidyr::separate(value, into = c("ignore", "grid")) %>%
    dplyr::select(grid) %>%
    dplyr::pull(grid)

  grid_details <- grid_details %>%
    dplyr::filter(startsWith(value, "xname") | startsWith(value, "yname")) %>%
    tidyr::separate(value, into = c("ignore", "variable"), sep = "=") %>%
    dplyr::mutate(ignore = stringr::str_replace_all(ignore, " ", "")) %>%
    dplyr::mutate(variable = stringr::str_replace_all(variable, " ", "")) %>%
    tidyr::spread(ignore, variable)

  lon_name <- stringr::str_replace_all(grid_details$xname, " ", "")
  lat_name <- stringr::str_replace_all(grid_details$yname, " ", "")

  depths <- system(stringr::str_c("cdo showlevel ", ff), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
    stringr::str_split(" ") %>%
    .[[1]] %>%
    as.numeric()

  if (!is.null(date_range)) {
    min_date <- lubridate::dmy(date_range[1])
    max_date <- lubridate::dmy(date_range[2])

    if (is.na(min_date) | is.na(max_date)) {
      stop("error check date range supplied")
    }

    system(stringr::str_c("cdo seldate,", min_date, ",", max_date, " ", ff, " dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: please check date range supplied")
    }
    file.rename("dummy.nc", ff)
  }
  depths <- depths[complete.cases(depths)]

  times <- system(stringr::str_c("cdo showtimestamp ", ff), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
    stringr::str_split(" ") %>%
    .[[1]]
  times <- times[nchar(times) > 0]

  # now, pull in the longitudes and latitudes...
  nc_raw <- ncdf4::nc_open(ff)
  nc_lon <- ncdf4::ncvar_get(nc_raw, lon_name)
  nc_lat <- ncdf4::ncvar_get(nc_raw, lat_name)

  # this is coded on the assumption that when there is only one depth and time, those dimensions will be collapsed to nothing
  # this should be a valid assumption

  if ("curvilinear" %nin% grid_type) {

    # we need to estimate the data frame size for the curvilinear grid case

    if (dim_check < length(nc_lon) * length(nc_lat) * length(depths) * length(times) * (2 + (length(depths) > 1) + (length(times) > 1))) {
      choice <- readline(prompt = "This file is potentially large. Do you want to continue? (y/n): ")
      print(choice)
      if (choice != "y") {
        (return("file is too big to read in"))
      }
    }

    nc_grid <- eval(parse(text = stringr::str_c(
      "expand.grid(Longitude = nc_lon, Latitude = nc_lat",
      ifelse(length(depths) > 1, ",Depth = depths", ""),
      ifelse(length(times) > 1, ",Time = times", ""),
      ")"
    )))
  } else {

  	# right now, this is relatively simplistic. Only uses row numbers. Could be smarter....
    if (dim_check < length(nc_lon)){
      choice <- readline(prompt = "This file is potentially large. Do you want to continue? (y/n): ")
      print(choice)
      if (choice != "y") {
        (return("file is too big to read in"))
      }
    }

    if (length(depths) < 2 & length(times) < 2) {
      nc_grid <- dplyr::data_frame(Longitude = as.numeric(nc_lon), Latitude = as.numeric(nc_lat))
    } else {
      nc_grid <- dplyr::data_frame(Longitude = as.numeric(nc_lon), Latitude = as.numeric(nc_lat))

      dt_grid <- eval(parse(text = stringr::str_c(
        "expand.grid(",
        ifelse(length(depths) > 1, "Depth = depths", ""),
        ifelse(length(depths) > 1 & length(times) > 1, ",", ""),
        ifelse(length(times) > 1, "Time = times", ""),
        ")"
      )))

      nc_grid <- dt_grid %>%
        dplyr::mutate(data = purrr::map(
          1,
          function(x) return(nc_grid)
        )) %>%
        unnest()
    }
  }

  if (is.null(vars)) {
    vars <- system(stringr::str_c("cdo showname ", ff), intern = TRUE, ignore.stderr = TRUE)
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
