

# to do
# month and year selection options
# a way to estimate the potential size of the data frame to ask the user of they still want to read it in
# the above works right now for the lonlat case. Get it working for the curvilinear case

# need an option for cacheing results potentially...

# a smarter way to handle nas in the data frame, as some values might be na but others not
# add drop_na option

# also, add a check for the number of horizontal grids. Shouldn't read the netcdf if there is more than 1 GB(ish)


#' @title Read a list of netcdf file to a nested data frame. Experimental
#'
#' @description This reads a list of netcdf files to a data frame. This function is strict and currently only works when the files are of identifical format, and it will fail for netcdf files with multiple times. Experimental feature, likely to change.
#' @param ff_list Files to read. A character vector.
#' @param vars A list of variables you want to read in. Character vector. Everything is read in if this is empty.
#' @param cdo_output Do you want to show the cdo output? Set to TRUE in case you want to troubleshoot errors.
#' @param dim_check The number of data points in the final data frame that will ask to continue. Set to NULL if you don't want to check.
#' @export

nc_read_list <- function(ff_list, vars = NULL, cdo_output = FALSE, dim_check = 15e7) {

  init_dir <- getwd()
  on.exit(setwd(init_dir))

	if(!is.character(ff_list))
		stop("error: ff_list is not a character vector")

	for(ff in ff_list){
  if (!file_valid(ff)) {
    stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
  }
	}

	# let's loop through the files

	result <- list()
	tracker <- 1

	grid_checked <- FALSE

	for(ff_file in ff_list){
		ff <- ff_file

	if(grid_checked == FALSE){
  if (as.integer(system(stringr::str_glue("cdo ngrids {ff}"), intern = TRUE, ignore.stderr = TRUE)) > 1) {
    warning("warning: there is more than one horizontal grid in the netcdf file. Please check output for errors!")
  }

  # check that the vars given are actually in the file
  if (!is.null(vars)) {
    var_list <- stringr::str_flatten(nc_variables(ff), collapse = " ")
    for (vv in vars) {
      if (vv %in% nc_variables(ff) == FALSE) {
        stop(stringr::str_glue("variable {vv} does not appear to be in the file. Available variables are {var_list}"))
      }
    }
  }



  # It is possible the files are not cdo compatible.
  # Right now the package attempts to make them cdo compatible. Is there a smarter way round this for reading?
  #
  delete_copy <- FALSE

  if (!cdo_compatible(ff)) {
    delete_copy <- TRUE
    temp_dir <- random_temp()

    file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"), overwrite = TRUE)

    # remove anything from the temporary folder to make sure there are no clashes etc.

    setwd(temp_dir)

    if (getwd() == init_dir) {
      stop("error: there was a problem changing the directory")
    }

    if (getwd() != temp_dir) {
      stop("error: there was a problem changing the directory")
    }

    temp_dir <- stringr::str_c(temp_dir, "/")

    if (file.exists("dummy.nc")) {
      file.remove("dummy.nc")
    }

    # We need to change ff to the name of the copied file so that the following code works

    ff <- "raw.nc"

    # If the file is not cdo compatible, we'll need to attempt to add coordinate variables...

    add_missing_grid(ff, vars = vars)

    # If this does not make the file cdo compatible, then we need to throw an error

    if (!cdo_compatible("raw.nc")) {
      stop("error: file is not cdo compatible, even after trying to fix the coordinates")
    }
  }

  grid_details <- system(stringr::str_glue("cdo griddes {ff}"), intern = TRUE, ignore.stderr = TRUE)

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

  depths <- system(stringr::str_glue("cdo showlevel {ff}"), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
    stringr::str_split(" ") %>%
    .[[1]] %>%
    as.numeric()

  depths <- depths[complete.cases(depths)]

  times <- NULL
	}

  # now, pull in the longitudes and latitudes...
  nc_raw <- ncdf4::nc_open(ff)
  nc_lon <- ncdf4::ncvar_get(nc_raw, lon_name)
  nc_lat <- ncdf4::ncvar_get(nc_raw, lat_name)

  # this is coded on the assumption that when there is only one depth and time, those dimensions will be collapsed to nothing
  # this should be a valid assumption, and works on all netcdf files tested

  if(grid_checked == FALSE){
  if ("curvilinear" %nin% grid_type & "unstructured" %nin% grid_type) {

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
    if (dim_check < length(nc_lon)) {
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
        tidyr::unnest()
    }
  }
  grid_checked <- TRUE
  }

  # if no vars are supplied, use the variables in the netcdf file

  if (is.null(vars)) {
    vars <- nc_variables(ff)
  }

  nc_grid <- nc_grid %>%
  	select(names(.)[names(.) %in% c("Longitude", "Latitude", "Depth")])

  for (vv in vars) {
    nc_var <- ncdf4::ncvar_get(nc_raw, vv)
    nc_grid$var <- nc_var %>% as.numeric()
    names(nc_grid)[ncol(nc_grid)] <- vv
  }

  nc_grid <- nc_grid %>%
    # tidyr::drop_na() %>%
    dplyr::as_tibble()

  ncdf4::nc_close(nc_raw)


  result[[tracker]] <- nc_grid
  tracker <- tracker + 1

	}
  if (delete_copy) {
    file.remove(stringr::str_glue(temp_dir, ff))
  }

	result <- tibble::enframe(result) %>%
		dplyr::rename(File = name, data = value) %>%
		mutate(File = ff_list)

  return(result)
}

