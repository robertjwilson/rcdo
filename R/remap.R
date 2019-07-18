
# remap version that doesn't copy files


# figure out how to handle very large files. Use gebco bath
# add a check whether there are any vertical depths in the file

#' @title Horizontally and vertically remap a ncdf file
#' @description This function allows you to remap a netcdf horizontally and vertically to any set of latlon coordinates.
#' @param ff This is the file to regrid.
#' @param vars Select the variables you want to regrid. If this is not given, all variables will be regridded.
#' @param coords A 2 column matrix or data frame of the form (longitude, latitude) with coordinates for regridding. This can be regular or irregular. The function will calculate which it is.
#' @param remapping The type of remapping. bil = bilinear. nn = nearest neighbour. dis = distance weighted.
#' @param na_value This is a value in the raw netcdf file that needs to be treated as an na.#'
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param zip_file Do you want any output file to be zipped to save space. Default is FALSE.
#' @param overwrite Do you want to overwrite out_file if it exists? Defaults to FALSE
#' @param ... optional arguments to be sent to nc_clip if you need to clip prior to remapping.#'
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
nc_remap <- function(ff, vars = NULL, coords = NULL, vert_depths = NULL, out_file = NULL, zip_file = FALSE, cdo_output = FALSE, remapping = "bil", na_value = NULL, overwrite = FALSE, ...) {

  # log the original file and get the full system path
  ff_orig <- normalizePath(ff)

  # holding nc. This is used as the file name for any netcdf manipulations
  holding_nc <- ff_orig

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

  # Create a temporary directory for manipulating the files
  temp_dir <- random_temp()

  setwd(temp_dir)

  # the following is ultra-safe for preventing a panic attack
  if (getwd() == init_dir) {
    stop("error: there was a problem changing the directory")
  }
  if (getwd() != temp_dir) {
    stop("error: there was a problem changing the directory")
  }

  temp_dir <- stringr::str_c(temp_dir, "/")

  # check if the raw file is compatible with cdo. If not, just regrid it

  if (!cdo_compatible(ff_orig)) {
    file.copy(ff_orig, "temp.nc")
    holding_nc <- "temp.nc"
    add_missing_grid(holding_nc, vars)
  }

  # set the missing value, if it has not been set already
  if (!is.null(na_value)) {

    system(stringr::str_glue("cdo -setmissval,{na_value} {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }

    file.rename("dummy.nc", holding_nc)
  }

  # check the the number of grids..

  if (as.integer(system(stringr::str_c("cdo ngrids ", holding_nc), intern = TRUE, ignore.stderr = (cdo_output == FALSE))) > 1) {
    warning("warning: there is more than one horizontal grid in the netcdf file.  Please check the outputs")
  }

  # Generate mygrid for remapping
  if (!is.null(coords)) {
    generate_grid(coords)
  }

  # Now, we need to select the variables we are interested in....
  if (!is.null(vars)) {
    var_select <- stringr::str_flatten(vars, ",")
    system(stringr::str_glue("cdo selname,{var_select} {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))

    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }

    # throw error if selecting vars fails
    if (!file.exists("dummy.nc")) {
      stop("error: problem subselecting vars from {ff}. Please consider setting cdo_output = TRUE and re-running")
    }
    file.rename("dummy.nc", holding_nc)
  }

  # it is possible there are no vertical depths in the file. In this case we throw a warning message
  vertical_remap <- TRUE
  num_depths <- nrow(nc_depths(holding_nc))
  if (!is.null(vert_depths)) {
    if (num_depths < 2) {
      warning("There are none or one vertical depths in the file. Vertical interpolation not carried out.")
      vertical_remap <- FALSE
    }
  }

  if (!is.null(vert_depths) & vertical_remap) {
    available_depths <- nc_depths(holding_nc)
    if (min(vert_depths) < min(available_depths$Depth)) {
      stop("error: minimum depth supplied is too low")
    }

    if (max(vert_depths) > max(available_depths$Depth)) {
      stop("error: maximum depth supplied is too low")
    }

    vert_depths <- stringr::str_flatten(vert_depths, ",")
    system(stringr::str_glue("cdo intlevel,{vert_depths}, {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))

    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }

    # throw error if vertical interpolation failed
    if (!file.exists("dummy.nc")) {
      stop("error: problem vertically interpolating file. Please consider setting cdo_output = TRUE and re-running")
    }

    file.rename("dummy.nc", holding_nc)
  }

  # clip the file
  if (length(list(...)) >= 1) {
    nc_clip(holding_nc, ..., out_file = "dummy.nc")


    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }

    file.rename("dummy.nc", holding_nc)
  }

  if (!is.null(coords)) {
  	# generate the weights for the horizontal remapping
    system(stringr::str_c("cdo gen{remapping},mygrid {holding_nc} remapweights.nc"), ignore.stderr = (cdo_output == FALSE))

  	# do the horizontal remapping
    system(stringr::str_glue("cdo remap{remapping},mygrid {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))

    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }

    # throw error if vertical interpolation failed
    if (!file.exists("dummy.nc")) {
      stop("error: problem horizontally remapping data. Please consider setting cdo_output = TRUE and re-running")
    }
    file.rename("dummy.nc", holding_nc)
  }

  # at this stage, we need to output a data frame if asked

  if (is.null(out_file)) {
    nc_grid <- nc_read(holding_nc)
    # remove the temporary files that have been generated
    # this checks how many files are in the folder, and makes sure it is less than 6
    # If it's greater than 5 something has gone wrong
    if (length(dir(temp_dir)) < 6 & temp_dir != init_dir) {
      unlink(temp_dir, recursive = TRUE)
    }

    return(nc_grid)
  }

  # save the file, if that's what you chose to do
  # change the working directory back to the original

  # zip the file if requested
  if (zip_file) {
    nc_zip(holding_nc, overwrite = TRUE)
  }
  setwd(init_dir)

  file.copy(stringr::str_c(temp_dir, holding_nc), out_file, overwrite = overwrite)

  # remove the temporary files created
  setwd(temp_dir)
  if (length(dir(temp_dir)) < 6 & temp_dir != init_dir) {
    unlink(temp_dir, recursive = TRUE)
  }
}

