
#' @title grid cell areas of a ncdf file
#' @description Calculates the areas of each grid cell of a netcdf file.
#' @param ff This is the file to analyze.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @return data frame with the cell areas in square metres
#' @export

# need an option for cacheing results...

#'@examples

#' # Calculating cell areas for NOAA world ocean atlas data sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_cellareas(ff)


nc_cellareas <- function(ff,  cdo_output = FALSE) {

	if(!file_valid(ff))
		stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))

	if(!cdo_compatible(ff))
		stop("error: file is not cdo compatible")

		if(as.integer(system(stringr::str_c("cdo ngrids ", ff), intern = TRUE, ignore.stderr = (cdo_output == FALSE)    )) > 1)
			warning("error: there is more than one horizontal grid in the netcdf file. This function cannot currently handle multiple grids")

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- random_temp()

  # copy the file to the temporary

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"), overwrite = TRUE)
  setwd(temp_dir)

  if(getwd() == init_dir)
  	stop("error: there was a problem changing the directory")

  if(getwd() != temp_dir)
  	stop("error: there was a problem changing the directory")

  temp_dir <- stringr::str_c(temp_dir, "/")

  # use gridarea to calculate the grid cell area
 if(file.exists("grid_area.nc"))
  file.remove("grid_area.nc")

 system("cdo gridarea raw.nc grid_area.nc", ignore.stderr = (cdo_output == FALSE))
 if(!file.exists("grid_area.nc"))
 	stop("error: grid areas could not be calculated. Please check cdo output by setting cdo_output = TRUE!")

  # read the grid cell areas to a data frame
   nc_read("grid_area.nc", cdo_output = cdo_output)

}

