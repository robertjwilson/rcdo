
#' @title List the variables in a netcdf file.
#' @description This function will tell you the variables in a netcdf file.
#' @param ff This is the file to read variables from.
#' @param details Do you want a data frame that provides the long name and units of the variable?
#' @return list of variables
#' @export
#' @examples
#' # Get the variable names in the NOAA WOA sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_variables(ff)
#' # A data frame showing the variables, their long names and units
#' nc_variables(ff, details = TRUE)
nc_variables <- function(ff, detailed = FALSE) {
  if (!file_valid(ff)) {
    stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
  }

  ff_names <<- system(stringr::str_c("cdo showname ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
    stringr::str_split(" ") %>%
    .[[1]]
  ff_names <- ff_names[ff_names != ""]
  ff_names <- as.character(ff_names)
  result <- ff_names
  # now, we need to pull in the details

  if(detailed){
  nc <- ncdf4::nc_open(ff)
  df_var <- list()
  for(i in 1:length(ff_names)){
  	i_name <- ncdf4::ncatt_get(nc, ff_names[i])$long_name
  	i_units <- ncdf4::ncatt_get(nc, ff_names[i])$units
  	df_var[[i]] <- tibble::tibble(variable = ff_names[i], long_name = i_name, units = i_units)
  }
  ncdf4::nc_close(nc)
  result <- df_var %>%
  	dplyr::bind_rows()
  }
  result
}
