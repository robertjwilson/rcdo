

nc_vertstat <- function(metric = NULL, ff, vars = NULL, vert_scale = NULL, coords = NULL, expr = NULL, na_value = NULL, out_file = NULL, cdo_output = FALSE, overwrite = FALSE,  ...) {

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

  if (as.integer(system(stringr::str_c("cdo ngrids ", ff), intern = TRUE)) > 1) {
    stop("error: there is more than one horizontal grid in the netcdf file. This function cannot currently handle multiple grids")
  }

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- random_temp()

  # copy the file to the temporary

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"))
  setwd(temp_dir)

  if (getwd() == init_dir) {
  	stop("error: there was a problem changing the directory")
  }

  if (getwd() != temp_dir) {
  	stop("error: there was a problem changing the directory")
  }

  temp_dir <- stringr::str_c(temp_dir, "/")

  # we need to set up a grid so that cdo can do a remapping. Easy enough

  # remove anything from the temporary folder to make sure there are no clashes etc.

  if (file.exists(stringr::str_c(temp_dir, "/raw_clipped.nc"))) {
    file.remove(stringr::str_c(temp_dir, "/raw_clipped.nc"))
  }

  # do the remapping

  remap_run <- FALSE

  if (length(list(...)) >= 1 | !is.null(coords)) {
  	nc_remap("raw.nc", vars = vars, cdo_output = TRUE, coords = coords, ..., out_file = "dummy.nc", na_value = na_value)
    file.rename("dummy.nc", "raw.nc")
    remap_run <- TRUE
  }

  # add the missing grid information if it isn't already there

  if(remap_run == FALSE){
  add_missing_grid("raw.nc", vars)

  # Now, we need to select the variables we are interested in
  # This only needs to happen when nc_remap has not been run
  if (!is.null(vars)) {
    system(stringr::str_c("cdo selname,", stringr::str_flatten(vars, ","), " raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  	# throw error if subselecting vars failed
  	if(!file.exists("dummy.nc"))
  		stop("error: problem subselecting vars from file. Considering setting cdo_output=TRUE and rerunning")
    file.rename("dummy.nc", "raw.nc")
  }

  # set the missing value, if it has not been set already
  if(!is.null(na_value)){
  system(stringr::str_glue("cdo -setmissval,{na_value} raw.nc dummy.nc"))
  	# throw error if missing value could not be set
  	if(!file.exists("dummy.nc"))
  		stop("error: problem setting missing value. Considering setting cdo_output=TRUE and rerunning.")

  file.rename("dummy.nc", "raw.nc")
  }
  }

  if (!is.null(vert_depths)) {
    vert_depths <- stringr::str_flatten(vert_depths, ",")

    system(stringr::str_c("cdo intlevel,", vert_depths, " ", "raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  	# throw error if vertical interpolation failed.
  	if(!file.exists("dummy.nc"))
  		stop("error: problem carrying out vertical interpolation. Considering setting cdo_output=TRUE and rerunning.")
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
  	dplyr::summarize(Maximum_Depth = max(Depth), Minimum_Depth = min(Depth)) %>%
  	dplyr::ungroup() %>%
  	dplyr::select(Longitude, Latitude, Minimum_Depth, Maximum_Depth)

  if (!is.null(expr)) {
  	expr <- stringr::str_replace_all(expr, " ", "")
  	print(expr)
  	system(stringr::str_glue("cdo aexpr,'{expr}' raw_clipped.nc dummy.nc"))
  	# throw error if expr application failed
  	if(!file.exists("dummy.nc"))
  		stop("error: problem applying expr. Considering setting cdo_output=TRUE and rerunning.")
  	file.rename("dummy.nc", "raw_clipped.nc")
  }

  system(stringr::str_glue("cdo vert{metric} raw_clipped.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  # throw error if calculationg of vertical mean failed
  	if(!file.exists("dummy.nc"))
  		stop("error: problem calculating vertical mean. Considering setting cdo_output=TRUE and rerunning.")
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
  file.copy(stringr::str_c(temp_dir, "/raw_clipped.nc"), out_file, overwrite = overwrite)

  if(file.exists(stringr::str_c(temp_dir, "/raw_clipped.nc")))
  	file.remove(stringr::str_c(temp_dir, "/raw_clipped.nc"))

}

