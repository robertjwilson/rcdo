
# years should probably be a default option...

#' @title Calculates a climatology from a netcdf file
#' @description This function allows you to calculate climatological monthly statistics. Data must be monthly
#' @param ff Target netcdf file
#' @param period Type of climatology. Must be one of yearly, monthly or seasonal.
#' @param vars Select the variables you want. If this is not given, stats will be calculated for all variables.
#' @param stat Statistic you want calculated. Options are min, max, range, sum, mean, var, std. Default is mean. Only min, max and mean allowed for yearly currently.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param zip_file Do you want any output file to be zipped to save space? Default is FALSE.
#' @param overwrite Do you want to overwrite out_file if it exists? Defaults to FALSE
#' @param ... optional arguments to be sent to nc_remap if you need to remap the netcdf prior to calculating the stat
#' @return data frame or netcdf file.
#' @export

nc_climatology <- function(ff, period = "seasonal", stat = "mean", vars = NULL, out_file = NULL, zip_file = FALSE, cdo_output = FALSE, overwrite = FALSE, ...) {

  # check that the period is valid
  # ...

  if (period %nin% c("monthly", "yearly", "seasonal")) {
    stop("Period supplied is not valid. It has to be one of monthly, yearly or seasonal")
  }

  if(stat %nin% c("min", "max", "range", "sum", "mean", "var", "std"))
    stop("stat supplied is not valid")

  if(period == "yearly" & stat %nin% c("min", "max", "mean"))
    stop("stat supplied is not valid")

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

  if (getwd() != temp_dir) {
    stop("error: there was a problem changing the directory")
  }

  temp_dir <- stringr::str_c(temp_dir, "/")

  # check if the raw file is compatible with cdo. If not, just regrid it

  if (!cdo_compatible("raw.nc")) {
    add_missing_grid("raw.nc", vars)
  }

  # Now, we need to select the variables we are interested in....
  if (!is.null(vars)) {
    system(stringr::str_c("cdo selname,", stringr::str_flatten(vars, ","), " raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    # throw error if selecting vars fails
    if (!file.exists("dummy.nc")) {
      stop("error: problem subselecting vars from {ff}. Please consider setting cdo_output = TRUE and re-running")
    }
    file.rename("dummy.nc", "raw.nc")
  }

  # remap the file if needed
  if (length(list(...)) >= 1) {
    nc_remap("raw.nc", ..., out_file = "dummy.nc", overwrite = TRUE)
    file.rename("dummy.nc", "raw.nc")
  }

  # calculate the monthly stat

  print(stringr::str_c("Stat calculated using the years: ", stringr::str_flatten(nc_years("raw.nc")$Year, " ")))

  if (period == "seasonal") {
    system(stringr::str_glue("cdo yseas{stat} raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  }

  if (period == "monthly") {
    system(stringr::str_glue("cdo ymon{stat} raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  }

  if (period == "yearly") {
    system(stringr::str_glue("cdo ymon{stat} raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))

    if (!file.exists("dummy.nc")) {
      stop("error: problem calculating the climatology using cdo. Please consider setting cdo_output = TRUE and re-running")
    }

    file.rename("dummy.nc", "raw.nc")
    system(stringr::str_glue("cdo year{stat} raw.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  }

  if (!file.exists("dummy.nc")) {
    stop("error: problem calculating the climatology using cdo. Please consider setting cdo_output = TRUE and re-running")
  }

  file.rename("dummy.nc", "raw.nc")

  # at this stage, we need to output a data frame if asked

  if (is.null(out_file)) {
    nc_grid <- nc_read("raw.nc")
    # pull out the month if it's a monthly climatology

    if (period == "monthly") {
      nc_grid <- nc_grid %>%
        dplyr::mutate(Month = as.integer(stringr::str_sub(Time, 6, 7))) %>%
        dplyr::select(-Time) %>%
        dplyr::select(Longitude, Latitude, Month, everything())
    }

    if (period == "seasonal") {
      nc_grid <- nc_grid %>%
        dplyr::mutate(Month = as.integer(stringr::str_sub(Time, 6, 7))) %>%
        dplyr::mutate(
          Season = dplyr::case_when(
            Month == 2 ~ "Winter",
            Month == 5 ~ "Spring",
            Month == 8 ~ "Summer",
            Month == 11 ~ "Autumn"
          )
        ) %>%
        dplyr::select(-Time, -Month) %>%
        dplyr::select(Longitude, Latitude, Season, everything())
    }

    if (period == "yearly") {
      nc_grid <- nc_grid %>%
        dplyr::select(-Time)
    }

    # remove the files that have been generated
    # this checks how many files are in the folder, and makes sure it is less than 6
    # If it's greater than 5 something has gone wrong
    if (length(dir(temp_dir)) < 6 & temp_dir != init_dir) {
      unlink(temp_dir, recursive = TRUE)
    }

    # at this point we need to replace time with month
    return(nc_grid)
  }

  # save the file, if that's what you chose to do
  # change the working directory back to the original

  # zip the file if requested
  if (zip_file) {
    nc_zip("raw.nc", overwrite = TRUE)
  }
  setwd(init_dir)

  file.copy(stringr::str_c(temp_dir, "/raw.nc"), out_file, overwrite = overwrite)

  # remove the temporary files created
  setwd(temp_dir)
  if (length(dir(temp_dir)) < 6 & temp_dir != init_dir) {
    unlink(temp_dir, recursive = TRUE)
  }
}




