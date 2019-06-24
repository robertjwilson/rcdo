

#' @title Rename variables in a netcdf file
#' @description This function allows you to rename variables in a netcdf file. This function requires that the none  #' of the variables in the function contains arithmetic variables.
#' @param ff This is the file to analyze.
#' @param expr A cdo expression.
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param zip_file Do you want any output file to be zipped to save space. Default is FALSE.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param na_value This is a value in the raw netcdf file that needs to be treated as an na.
#' @param overwrite Do you want to overwrite out_file if it exists? Defaults to FALSE for safety
#' @param ... optional arguments to be sent to nc_remap if you need to remap the netcdf prior to calculating the stat
#' @return data frame or netcdf file.
#' @export

nc_expr <- function(ff, expr = NULL, out_file = NULL, zip_file = FALSE, cdo_output = FALSE, overwrite = FALSE, ...) {

 contains_arithmetic <- function(x) {
    result <- FALSE
    result <- result + stringr::str_detect(x, "\\+")
    result <- result + stringr::str_detect(x, "\\/")
    result <- result + stringr::str_detect(x, "\\*")
    result <- result + stringr::str_detect(x, "\\-")
    result <- result + stringr::str_detect(x, "\\^")
    return(as.logical(result))
  }

  temp_file <- stringr::str_c(tempfile(), ".nc")
  dummy_file <- stringr::str_c(tempfile(), ".nc")

  expr_list <- stringr::str_split(expr, ";")[[1]]

  # now, we need to loop through the new variable names.
  # These cannot contain arithemtic things, like -

  for (i in 1:length(expr_list)) {
    i_target <- stringr::str_split(expr_list[i], "=")[[1]][1]
    if (contains_arithmetic(i_target)) {
      stop("new variable names cannot contain arithmetic operators")
    }
  }

  # some checks on the input

  if(!is.null(out_file))
  if (file.exists(out_file) & overwrite == FALSE) {
    return(stringr::str_glue("{ff} already exists, so will not be overwritten"))
  }

  temp_file <- stringr::str_c(tempfile(), ".nc")

  # check variables exist in file

  if (is.null(expr)) {
    return("No expr defined")
  }

  # First we need to check

  if (sum(contains_arithmetic(nc_variables(ff))) > 0) {

  	for(vv in nc_variables(ff)){
  		if(contains_arithemtic(vv)){
  			if(str_detect(expr, vv))
  				stop("expr relies on variables that contain arithmetic operators")
  		}
  	}
  }

  if (sum(contains_arithmetic(nc_variables(ff))) == 0) {

  	# case when there are no ... arguments
  if (length(list(...)) == 0) {
    system(stringr::str_glue("cdo expr,'{expr}' {ff} {temp_file}"), ignore.stderr = (cdo_output == FALSE))
  	if(!file.exists(temp_file))
  		stop("problem apply expr. Consider setting cdo_output = TRUE and rerunning")
  }

  	# case when there are ... arguments
  # remap the file if needed
  if (length(list(...)) >= 1) {
  	nc_remap(ff, ..., out_file = temp_file, overwrite = TRUE)

    system(stringr::str_glue("cdo expr,'{expr}' {temp_file} {dummy_file}"), ignore.stderr = (cdo_output == FALSE))
  	if(!file.exists(dummy_file))
  		stop("problem apply expr. Consider setting cdo_output = TRUE and rerunning")
  	file.rename(dummy_file, temp_file)
  }

  }
  # return the results as a data frame if requested

  if (is.null(out_file)) {
    nc_grid <- nc_read(temp_file)
    return(nc_grid)
  }

  # save the results as netcdf if requested

  if (zip_file) {
    nc_zip(temp_file, overwrite = TRUE)
  }

  file.copy(temp_file, out_file, overwrite = overwrite)

  # remove the temporary file created
  file.remove(temp_file)
}





