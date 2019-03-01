
# to be added: year range options
# figure out how to handle very large files. Use gebco bath
# add a check whether there are any vertical depths in the file



#' @title remap a ncdf file
#' @description This function allows you to remap a netcdf horizontally and vertically to any set of latlon coordinates.
#' @param ff This is the file to regrid.
#' @param vars Select the variables you want to regrid. If this is not given, all variables will be regridded.
#' @param coords A 2 column matrix or data frame of the form (longitude, latitude) with coordinates for regridding. This can be regular or irregular. The function will calculate which it is.
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param remapping The type of remapping. bil = bilinear. nn = nearest neighbour. dis = distance weighted.
#' @param na_value This is a value in the raw netcdf file that needs to be treated as an na.#'
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param ... optional arguments to be sent to nc_clip if you need to clip prior to remapping.
#' @return data frame or netcdf file.
#' @export

# need an option for cacheing results...

#' @examples
#'
#' # Remapping NOAA world ocean atlas data to the region around the UK
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' # remapping to 1 degree resolution across all depth layers
#' uk_coords <- expand.grid(Longitude = seq(-20, 10, 1), Latitude = seq(48, 62, 1))
#' nc_remap(ff, vars = "t_an", coords = uk_coords)
#'
#' # remapping to 1 degree resolution for 5, 50 and 100 metres in the region around the uk
#' nc_remap(ff, vars = "t_an", coords = uk_coords, vert_depths = c(5, 50, 100))
nc_remap <- function(ff, vars = NULL, coords = NULL, vert_depths = NULL, out_file = NULL, cdo_output = FALSE, remapping = "bil", na_value = NULL, ...) {
  if (!file_valid(ff)) {
    stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
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
  if (remapping %nin% c("bil", "dis", "nn")) {
    stop(stringr::str_glue("remapping method {remapping} is invalid"))
  }

  # take note of the working directory, so that it can be reset to this on exit

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- random_temp()

  # copy the file to the temporary

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"), overwrite = TRUE)
  setwd(temp_dir)

  if (getwd() == init_dir) {
    stop("error: there was a problem changing the directory")
  }

  # check if the raw file is compatible with cdo. If not, just regrid it

  add_missing_grid("raw.nc", vars)


  # set the missing value, if it has not been set already
  if(!is.null(na_value)){
  	system(stringr::str_glue("cdo -setmissval,{na_value} raw.nc dummy.nc"))
  	file.rename("dummy.nc", "raw.nc")
  }


  # check the the number of grids..

  if (as.integer(system(stringr::str_c("cdo ngrids ", "raw.nc"), intern = TRUE, ignore.stderr = (cdo_output == FALSE))) > 1) {
    warning("warning: there is more than one horizontal grid in the netcdf file.  Please check the outputs")
  }


  # Generate mygrid for remapping
  if(!is.null(coords))
  	generate_grid(coords)


  # to be added
  # check the validity of the variables selected

  # if(!is.null(vars))
  # 	var_validity("raw.nc", vars)
  #
  # if(is.null(vars))
  # 	var_validity("raw.nc", nc_variables("raw.nc"))


  # Now, we need to select the variables we are interested in....
  if (!is.null(vars)) {
    system(stringr::str_c("cdo selname,", stringr::str_flatten(vars, ","), " raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw.nc")
  }

  if (!is.null(vert_depths)) {
    vert_depths <- stringr::str_flatten(vert_depths, ",")

    system(stringr::str_c("cdo intlevel,", vert_depths, " ", "raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw.nc")
  }

  file.rename("raw.nc", "raw_clipped.nc")

  # clip the file
  if (length(list(...)) >= 1) {
    nc_clip("raw_clipped.nc", ..., out_file = "dummy.nc")
    file.rename("dummy.nc", "raw_clipped.nc")
  }

  if(!is.null(coords)){
  	system(stringr::str_c("cdo gen", remapping, ",mygrid raw_clipped.nc remapweights.nc"), ignore.stderr = (cdo_output == FALSE))
  	system(stringr::str_c("cdo remap", remapping, ",mygrid raw_clipped.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  	file.rename("dummy.nc", "raw_clipped.nc")
  }

  # at this stage, we need to output a data frame if asked

  if (is.null(out_file)) {
    nc_grid <- nc_read("raw_clipped.nc")
    # remove the files that have been generated
    file.remove(stringr::str_c(temp_dir, "/raw_clipped.nc"))
    # file.remove(stringr::str_c(temp_dir, "/raw.nc"))
    file.remove(stringr::str_c(temp_dir, "/remapweights.nc"))
    file.remove(stringr::str_c(temp_dir, "/mygrid"))

    return(nc_grid)
  }

  # save the file, if that's what you chose to do
  # change the working directory back to the original

  setwd(init_dir)

  file.copy(stringr::str_c(temp_dir, "/raw_clipped.nc"), out_file, overwrite = TRUE)
}
