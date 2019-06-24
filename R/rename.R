

#' @title Rename variables in a netcdf file
#' @description This function allows you to rename variables in a netcdf file
#' @param ff This is the file to analyze.
#' @param old Character vector listing the old variable names.
#' @param new Character vector listing the new variable names. Must be the same length as old.
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param zip_file Do you want any output file to be zipped to save space. Default is FALSE.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param na_value This is a value in the raw netcdf file that needs to be treated as an na.
#' @param overwrite Do you want to overwrite out_file if it exists? Defaults to FALSE
#' @return data frame or netcdf file.
#' @export

nc_rename <- function(ff, old = NULL, new = NULL, out_file = NULL, zip_file = FALSE, cdo_output = FALSE, overwrite = FALSE) {

	# some checks on the input

  if (file.exists(ff) & overwrite == FALSE & !is.null(out_file)) {
    return(stringr::str_glue("{ff} already exists, so will not be overwritten"))
  }

  temp_file <- stringr::str_c(tempfile(), ".nc")
  if (is.null(old) | is.null(new)) {
    stop("no variables selected")
  }

  # check variables exist in file

  for(vv in old)
  	if(vv %nin% nc_variables(ff))
  		stop(stringr::str_glue("{ff} is  not in the netcdf file"))

  cdo_vars <- c()
  for (i in 1:length(old)) {
    cdo_vars <- c(cdo_vars, stringr::str_glue("{old[i]},{new[i]}"))
  }

  cdo_vars <- stringr::str_flatten(cdo_vars, ",")

  system(stringr::str_glue("cdo chname,{cdo_vars} {ff} {temp_file}"), ignore.stderr = (cdo_output == FALSE))

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
