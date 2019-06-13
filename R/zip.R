#' @title Zip a netcdf file
#'
#' @description Zip a netcdf file
#' @param ff This is the file to zip.
#' @param out_file The name of the file output. Defaults to ff
#' @param overwrite Do you want to overwrite out_file if it exists? Defaults to FALSE for safety
#' @return data frame or netcdf file.
#' @export
nc_zip <- function(ff, out_file = ff, overwrite = FALSE) {
  if (file.exists(out_file) & overwrite == FALSE) {
    return("warning: out_file already exists. Set overwrite to TRUE if you want to overwrite it")
  }

  if (!file_valid(ff)) {
    stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
  }

  # create a temp file for the cdo zipping process
  dummy_zip <- str_c(tempfile(), ".nc")

  # zip the file
  system(stringr::str_glue("cdo -f nc4 -z zip_9 copy {ff} {dummy_zip}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  file.rename(dummy_zip, out_file)
}
