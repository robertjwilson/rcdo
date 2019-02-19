
# to be added: year range options
# figure out how to handle very large files. Use gebco bath

# think about whether multiple grids should throw a warning or error


#' @title remap a ncdf file (development version)
#' @description This function allows you to remap a netcdf horizontally and vertically to a specific latlon box
#' @param ff This is the file to regrid.
#' @param vars Select the variables you want to regrid. If this is not given, all variables will be regridded.
#' @param coords A 2 column matrix or data frame of the form (longitude, latitude) with coordinates for regridding. This can be regular or irregular. The function will calculate which it is.
#' @param date_range This is the range of dates you want. c(date_min, date_max). "day/month/year" character string format. Ignored if not supplied
#' @param months Months you want. c(month_1, month_2,...). Ignored if not supplied.
#' @param years Years you want. c(year_1, year_2,...). Ignored if not supplied.
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param remapping The type of remapping. bil = bilinear. nn = nearest neighbour. dis = distance weighted.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @return data frame or netcdf file.
#' @export

# need an option for cacheing results...

#' @examples
#'
#' # Remapping NOAA world ocean atlas data to the region around the UK
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' # remapping to 1 degree resolution across all depth layers
#' uk_coords <- expand.grid(Longitude = seq(-20, 10, 1), Latitude = seq(48, 62, 1))
#' nc_remap2(ff, vars = "t_an", coords = uk_coords)
#'
#' # remapping to 1 degree resolution for 5, 50 and 100 metres in the region around the uk
#' nc_remap2(ff, vars = "t_an", coords = uk_coords, vert_depths = c(5, 50, 100))
nc_remap2 <- function(ff, vars = NULL, coords = NULL, date_range = NULL, months = NULL, years = NULL, out_file = NULL, vert_depths = NULL, remapping = "bil", cdo_output = FALSE) {
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

  if (!cdo_compatible(ff)) {
    stop("error: file is not cdo compatible")
  }

  if (as.integer(system(stringr::str_c("cdo ngrids ", ff), intern = TRUE), ignore.stderr = (cdo_output == FALSE)) > 1) {
    warning("error: there is more than one horizontal grid in the netcdf file. This function cannot currently handle multiple grids")
  }

  if (remapping %nin% c("bil", "dis", "nn")) {
    stop(stringr::str_glue("remapping method {remapping} is invalid"))
  }

  # take note of the working directory, so that it can be reset to this on exit

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- tempdir()

  # copy the file to the temporary

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"), overwrite = TRUE)
  setwd(temp_dir)

  if (getwd() == init_dir) {
    stop("error: there was a problem changing the directory")
  }

  # Generate mygrid for remapping

  generate_grid(coords)

  # ad the variables we need to add attributes for

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

  if (!is.null(date_range)) {
    min_date <- lubridate::dmy(date_range[1])
    max_date <- lubridate::dmy(date_range[2])

    if (is.na(min_date) | is.na(max_date)) {
      stop("error check date range supplied")
    }

    system(stringr::str_c("cdo seldate,", min_date, ",", max_date, " raw_clipped.nc dummy.nc"),  ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw_clipped.nc")
  }

  if (!is.null(months)) {
    file_months <- system(stringr::str_c("cdo showmon ", "raw_clipped.nc"), intern = TRUE,  ignore.stderr = (cdo_output == FALSE)) %>%
      stringr::str_split(" ") %>%
      .[[1]] %>%
      as.integer()
    num_months <- 0
    for (mm in months) {
      if (mm %in% unique(file_months[complete.cases(file_months)])) {
        num_months <- num_months + 1
      }
    }

    if (num_months == 0) {
      stop("error: check months supplied")
    }

    system(stringr::str_c("cdo selmonth,", stringr::str_flatten(months, ","), " raw_clipped.nc dummy.nc"),  ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw_clipped.nc")
  }

  if (!is.null(years)) {
    file_years <- system(stringr::str_c("cdo showyear ", "raw_clipped.nc"), intern = TRUE,  ignore.stderr = (cdo_output == FALSE)) %>%
      stringr::str_split(" ") %>%
      .[[1]] %>%
      as.integer()
    num_years <- 0
    for (yy in years) {
      if (yy %in% unique(file_years[complete.cases(file_years)])) {
        num_years <- num_years + 1
      }
    }

    if (num_years == 0) {
      stop("error: check years supplied")
    }
    system(stringr::str_c("cdo selyear,", stringr::str_flatten(years, ","), " raw_clipped.nc dummy.nc"),  ignore.stderr = (cdo_output == FALSE))
    file.rename("dummy.nc", "raw_clipped.nc")
  }

  system(stringr::str_c("cdo gen", remapping, ",mygrid raw_clipped.nc remapweights.nc"), ignore.stderr = (cdo_output == FALSE))
  system(stringr::str_c("cdo remap", remapping, ",mygrid raw_clipped.nc dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  file.rename("dummy.nc", "raw_clipped.nc")

  # at this stage, we need to output a data frame if asked

  if (is.null(out_file)) {

    nc_grid <- nc_read("raw_clipped.nc")
    return(nc_grid)
  }

  # save the file, if that's what you chose to do
  # change the working directory back to the original

  setwd(init_dir)

  file.copy(stringr::str_c(temp_dir, "/raw_clipped.nc"), out_file, overwrite = TRUE)
}
