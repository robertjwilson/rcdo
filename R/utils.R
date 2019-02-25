

`%nin%` <- function(x, y) x %in% y == FALSE

"%>%" <- magrittr::`%>%`


# library(tidyverse)

bin_value.numeric <- function(x, bin_res) {
  floor((x + bin_res / 2) / bin_res + 0.5) * bin_res - bin_res / 2
}


# function to bin coordinates

bin_value <- function(x, bin_res) {
  UseMethod("bin_value")
}


cdo_compatible <- function(ff) {
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

  if (nrow(grid_details) == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}



file_valid <- function(ff) {
  if (!is.character(ff)) {
    return(FALSE)
  }

  if (length(ff) > 1) {
    return(FALSE)
  }

  if (!file.exists(ff)) {
    return(FALSE)
  }

  if (!endsWith(ff, ".nc")) {
    return(FALSE)
  }

  return(TRUE)
}

# function to generate grid.

generate_grid <- function(coords) {

  # make everything numeric in the grid
  coords <- as.data.frame(coords)
  num_coords <- nrow(coords)

  if(ncol(coords) != 2)
  	stop("error: coords has the incorrect number of columns")

  coords[, 1] <- as.numeric(coords[, 1])
  coords[, 2] <- as.numeric(coords[, 2])
  names(coords) <- c("x", "y")
  coords <- as.matrix(coords)
  grid_type <- NA

  # work out the grid_type

  lon_unique <- unique(coords[, 1])
  lat_unique <- unique(coords[, 2])

  # deal with the case where there is only one coordinate

  lon_step <- (max(lon_unique) - min(lon_unique)) / (length(lon_unique) - 1)
  lat_step <- (max(lat_unique) - min(lat_unique)) / (length(lat_unique) - 1)

  if(length(lon_unique) == 1)
  	lon_step = 0
  if(length(lat_unique) == 1)
  	lat_step = 0

  x <- seq(min(lon_unique), max(lon_unique), lon_step)
  y <- seq(min(lat_unique), max(lat_unique), lat_step)

  if (all.equal(x, lon_unique) != TRUE | all.equal(y, lat_unique) != TRUE) {
    grid_type <- "unstructured"
  }

  if (is.na(grid_type)) {
    test_grid <- expand.grid(x = x, y = y) %>%
      as.matrix()
    if (dplyr::all_equal(coords, test_grid) == TRUE) {
      grid_type <- "lonlat"
    } else {
      grid_type <- "unstructured"
    }
  }

  # we now have the grid type. Time to create the grid

  grid_type


  if (grid_type == "unstructured") {
    coords <- as.data.frame(coords)
    names(coords) <- c("Longitude", "Latitude")
    x_size <- nrow(coords)
    x_values <- stringr::str_flatten(coords$Longitude, collapse = " ")
    y_values <- stringr::str_flatten(coords$Latitude, collapse = " ")
    c(
      "gridtype = unstructured", stringr::str_glue("gridsize = {x_size}"),
      stringr::str_glue("xvals = {x_values}"),
      stringr::str_glue("yvals = {y_values}")
    ) %>%
      readr::write_lines("mygrid")
  }

  if (grid_type == "lonlat") {
    lon_res <- lon_step
    lat_res <- lat_step

    lon_range <- range(lon_unique)
    lat_range <- range(lat_unique)

    x_size <- length(x)
    y_size <- length(y)
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
  grid_type
}



# function to create new temporary folder...


random_temp <- function(){

	check <- TRUE
	while(check){
		new_temp <- stringr::str_c(dirname(tempdir()), "/",  stringi::stri_rand_strings(1,7, '[A-Z]'))
		if(!dir.exists(new_temp)){
			dir.create(new_temp)
		break
		}
	}
	new_temp
}

# function to add missing grid details if it is generic

add_missing_grid <- function(ff, vars = NULL) {

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
	if (nrow(grid_details) != 0)
		return(NULL)

	# find the name of longitude and latitude. This can probably be improved upon....

	long_use <- system(stringr::str_glue("cdo codetab {ff}"),intern = TRUE, ignore.stderr = TRUE) %>%
		tibble::enframe() %>%
		dplyr::filter(stringr::str_detect(value, "[L-l]ongitude")) %>%
		dplyr::mutate(value = trimws(value)) %>%
		dplyr::mutate(value = stringr::str_replace_all(value, "  ", " ")) %>%
		tidyr::separate(value, into = c("ignore1", "coord"), sep = " ", extra = "drop") %>%
		dplyr::pull(coord)

	lat_use <- system(stringr::str_glue("cdo codetab {ff}"),intern = TRUE, ignore.stderr = TRUE) %>%
		tibble::enframe() %>%
		dplyr::filter(stringr::str_detect(value, "[L-l]atitude")) %>%
		dplyr::mutate(value = trimws(value)) %>%
		dplyr::mutate(value = stringr::str_replace_all(value, "  ", " ")) %>%
		tidyr::separate(value, into = c("ignore1", "coord"), sep = " ", extra = "drop") %>%
		dplyr::pull(coord)

	# add the grid...

	vars_2grid <- nc_variables(ff)
	vars_2grid <- vars_2grid[vars_2grid != long_use & vars_2grid != lat_use]

	if(!is.null(vars))
		vars_2grid <- vars_2grid[vars_2grid %in% vars]


	stringr::str_glue('{vars_2grid}@coordinates="{lat_use} {long_use}"') %>%
		readr::write_lines("myattributes.txt")

	# at this point, we need to only select what we want.
	# This may still not be general to all generic grid netcdf files.

	sel_names <- stringr::str_flatten(c(vars, long_use, lat_use), collapse = ",")
	system(stringr::str_c("cdo selname,", stringr::str_flatten(sel_names, ","), " raw.nc dummy.nc"))
	file.rename("dummy.nc", "raw.nc")


	system("cdo setattribute,FILE=myattributes.txt raw.nc dummy.nc")
	file.rename("dummy.nc", "raw.nc")
	vars_2grid <- stringr::str_flatten(vars_2grid, collapse =  " ")

	warning(stringr::str_glue("The raw file does not have a grid type cdo can work with. Grid information has been added for variables {vars_2grid}. Please check output!"))

}
