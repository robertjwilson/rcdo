
# to be added: generalize this to cover vertical min, max etc.

# the function should perhaps be modified so that it takes ..., and then sends ... to remap


#' @title Calculate vertical means from a netcdf file.
#' @description This function allows you to remap a netcdf horizontally and vertically to a specific latlon box. It outputs a data frame with the depth averaged value and the maximum depth used for the calculation.
#' @param ff This is the file to analyze.
#' @param vars Select the variables you want to regrid. If this is not given, all variables will be regridded.
#' @param vert_scale This is the vertical scale you want. c(min_depth, max_depth, vertical_resolution).
#' @param coords A 2 column matrix or data frame of the form (longitude, latitude) with coordinates for regridding. This can be regular or irregular. The function will calculate which it is.
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param na_value This is a value in the raw netcdf file that needs to be treated as an na.
#' @param ... Additional terms to be sent to nc_remap2
#' @return data frame or netcdf file.
#' @export

# need an option for cacheing results...

#'@examples

#' # Calculating vertical mean NOAA world ocean atlas data to the region around the UK
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' # calculate vertical mean of temperature between 5 and 30 m, using a resolution of 5 for vertical interpolation
#' nc_vertmean(ff, vars = "t_an", vert_scale = c(5, 30, 5))


#'
nc_vertmean2 <- function(ff, vars = NULL, vert_scale = NULL, coords = NULL, na_value = NULL, out_file = NULL, cdo_output = FALSE,  ...) {

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

  # if (!cdo_compatible(ff)) {
  #   stop("error: file is not cdo compatible")
  # }

  if (as.integer(system(stringr::str_c("cdo ngrids ", ff), intern = TRUE)) > 1) {
    stop("error: there is more than one horizontal grid in the netcdf file. This function cannot currently handle multiple grids")
  }

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # to be safe, if the working directory is the CAO one, switch it to the home directory at this point

  setwd("~")

  if (!file.exists(ff)) {
    stop(stringr::str_glue("File {ff} either does not exist or does not have the full path"))
  }

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- random_temp()

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

  # do the remapping

  remap_run <- FALSE

  if (length(list(...)) >= 1 | !is.null(coords)) {
  	nc_remap2("raw.nc", vars = vars, cdo_output = TRUE, coords = coords, ..., out_file = "dummy.nc", na_value = na_value)
    file.rename("dummy.nc", "raw.nc")
    remap_run <- TRUE
  }


  add_missing_grid("raw.nc", vars)

  # Now, we need to select the variables we are interested in
  # This only needs to happen when nc_remap2 has not been run
  if(remap_run == FALSE){
  if (!is.null(vars)) {
    system(stringr::str_c("cdo selname,", stringr::str_flatten(vars, ","), " raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw.nc")
  }

  # set the missing value, if it has not been set already
  if(!is.null(na_value)){
  system(stringr::str_glue("cdo -setmissval,{na_value} raw.nc dummy.nc"))
  file.rename("dummy.nc", "raw.nc")
  }
  }

  # add the missing grid information if it isn't already there


  if (!is.null(vert_depths)) {
    vert_depths <- stringr::str_flatten(vert_depths, ",")

    system(stringr::str_c("cdo intlevel,", vert_depths, " ", "raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw.nc")
  }

  file.rename("raw.nc", "raw_clipped.nc")

  # calculate the vertical mean
  max_depths <- nc_read("raw_clipped.nc") %>%
  	tidyr::drop_na()

 	#it's possible only one vertical depth has been selected. Add depth in in this case
	depths_selected <- seq(vert_scale[1], vert_scale[2], vert_scale[3])

	if(length(depths_selected) == 1)
  	max_depths <- max_depths %>%
			dplyr::mutate(Depth = depths_selected[1])

  max_depths <- max_depths %>%
  	dplyr::group_by(Longitude, Latitude) %>%
  	dplyr::summarize(Maximum_Depth = max(Depth)) %>%
  	dplyr::ungroup() %>%
  	dplyr::select(Longitude, Latitude, Maximum_Depth)

  system(stringr::str_c("cdo vertmean raw_clipped.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  file.rename("dummy.nc", "raw_clipped.nc")

  # at this stage, we need to output a data frame if asked

  if (is.null(out_file)) {
    print("converting to a data frame")
    nc_grid <- nc_read("raw_clipped.nc")
    nc_grid <- nc_grid %>%
    	dplyr::inner_join(max_depths)
    return(nc_grid)
  }
  # save the file, if that's what you chose to do
  # change the working directory back to the original

  setwd(init_dir)
  file.copy(stringr::str_c(temp_dir, "/raw_clipped.nc"), out_file, overwrite = TRUE)
}

