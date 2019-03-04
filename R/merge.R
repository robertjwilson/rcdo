
# function to merge and remap a list of netcdf files

#' @title merge netcdf files
#' @description This function allows you to remap a netcdf horizontally and vertically to a specific latlon box
#' @param ff_list This is a list of files to merge
#' @param merge This is the merge type. Use "merge" to combine files, "mergetime" to merge based on time.
#' @param expr This is an expression to apply to the merged files.
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param overwrite Do you want to overwrite out_file if it exists? Defaults to FALSE
#' @return data frame or netcdf file.
#' @export

nc_merge <- function(ff_list, merge = "merge", expr = NULL,  out_file = NULL, cdo_output = TRUE, overwrite = FALSE) {

  # loop through the files
  for (ff in ff_list)
    if (!file_valid(ff)) {
      stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
    }

  # take note of the working directory, so that it can be reset to this on exit

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- random_temp()

  # copy the file to the temporary

  new_ens <- c()

  tracker <- 1
  for (ff in ff_list) {
    ens_file <- stringr::str_glue("raw{tracker}.nc")
    new_ens <- c(new_ens, ens_file)
    tracker <- tracker + 1
    file.copy(ff, stringr::str_c(temp_dir, ens_file), overwrite = TRUE)
  }

  setwd(temp_dir)

  if (getwd() == init_dir) {
    stop("error: there was a problem changing the directory")
  }

  if (getwd() != temp_dir) {
    stop("error: there was a problem changing the directory")
  }

  temp_dir <- stringr::str_c(temp_dir, "/")


  # OK. We now need to merge the files...

  ens_string <- stringr::str_flatten(new_ens, collapse = " ")

  system(stringr::str_glue("cdo {merge} {ens_string} merged.nc"))

  if(!file.exists("merged.nc"))
  	stop("error: problem merging files. Please set cdo_output = TRUE and rerun")

  # we no longer need the ensemble files post-merging.
  # Delete them

  file.remove(new_ens)

  # now, apply the expression if it has been supplied.....
  # First we need to make sure the expression has no spaces

  if (!is.null(expr)) {
    expr <- stringr::str_replace_all(expr, " ", "")
    print(expr)
    system(stringr::str_glue("cdo aexpr,'{expr}' merged.nc dummy.nc"))
    file.rename("dummy.nc", "merged.nc")
  }

  # read in the merged file to a data frame if there is no out_file
  if (is.null(out_file)) {
    result <- nc_read("merged.nc")
    return(result)
  }

  # if out_file is given, save the merged nc file to this

  setwd(init_dir)
  file.copy(stringr::str_c(temp_dir, "/merged.nc"), out_file, overwrite = overwrite)
  if(file.exists(stringr::str_c(temp_dir, "/merged.nc")))
  	file.remove(stringr::str_c(temp_dir, "/merged.nc"))
}
