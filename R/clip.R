
# possibly remove the date range option. Is it useful?

# to be added
# code to work out if the date ranges are invalid


#' @title Clip a netcdf file
#'
#' @description Clip a netcdf file to a specific latlon box
#' @param ff This is the file to clip.
#' @param vars Select the variables you want. If this is not given, all variables will be regridded.
#' @param lon_range longitude range. c(min_longituse, max_longitude).
#' @param lat_range latitude range. c(min_latitude, max_latitude).
#' @param vert_range This is the range of depths for vertical clipping, if wanted. c(min_depth, max_depth)
#' @param date_range This is the range of dates you want. c(date_min, date_max). "day/month/year" character string format.
#' @param months Months you want. c(month_1, month_2,...)
#' @param years Months you want. c(year_1, year_2,...)
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param zip_file Do you want any output file to be zipped to save space. Default is FALSE.
#' @param overwrite Do you want to overwrite out_file if it exists? Defaults to FALSE
#' @return data frame or netcdf file.
#' @export

# need an option for cacheing results...

#' @examples
#'
#' # Clipping data from the NOAA World Ocean Atlas sample file.
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#'
#' # clip to a specific depth range using vert_range
#'
#' nc_clip(ff, vert_range = c(1, 5))
#'
#' # clip to a specific longitude and latitude box
#' # Clipping to the region around the UK
#'
#' uk_sst <- nc_clip(ff, lon_range = c(-12, 10), lat_range = c(48, 62))
nc_clip <- function(ff, vars = NULL, lon_range = c(-180, 180), lat_range = c(-90, 90), vert_range = NULL, date_range = NULL, months = NULL, years = NULL, out_file = NULL, cdo_output = FALSE, zip_file = FALSE, overwrite = FALSE) {

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

  # check the lon and lat range validity

  if (!is.numeric(lon_range) | !is.numeric(lat_range)) {
    stop("lon or lat ranges are not numeric")
  }
  if (length(lon_range) != 2 | length(lat_range) != 2) {
    stop("lon or lat range is not a 2 value numeric")
  }

  if (lon_range[1] > lon_range[2]) {
    stop("lon_range is not valid")
  }
  if (lat_range[1] > lat_range[2]) {
    stop("lat_range is not valid")
  }

  # check if vert_range is valid

  if (!is.null(vert_range)) {
    if (!is.numeric(vert_range)) {
      stop("error: vert_range is not numeric")
    }

    if (vert_range[2] < vert_range[1]) {
      stop("error: vert_range is not valid")
    }
  }

  # check that months are valid

  if(!is.null(years)){
    if(!is.numeric(years))
      stop("error: years is not numeric")
  }

  # check that months are valid

  if(!is.null(months)){
    if(!is.numeric(months))
      stop("error: months is not numeric")

    valid_months <- 1:12
    if((sum(months %in% valid_months) == length(months)) == FALSE)
      stop("error: months supplied are not actual months")
  }

  # check that vars is a chracter stringr

  if (!is.null(vars)) {
    if (!is.character(vars)) {
      stop("error: vars is not a character string")
    }
  }

  if (as.integer(system(stringr::str_c("cdo ngrids ", ff), ignore.stderr = (cdo_output == FALSE), intern = TRUE)) > 1) {
    warning("warning: there is more than one horizontal grid in the netcdf file. Please check the output!")
  }

  # take note of the working directory, so that it can be reset to this on exit

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- random_temp()

  setwd(temp_dir)
  if (getwd() == init_dir) {
    stop("error: there was a problem changing the directory")
  }

  if (getwd() != temp_dir) {
    stop("error: there was a problem changing the directory")
  }

  # add "/" to the end of the temporary directory, for safety
  temp_dir <- stringr::str_c(temp_dir, "/")


  # check if the raw file is compatible with cdo. If not, just regrid it

  if (!cdo_compatible(ff_orig)) {
    file.copy(ff_orig, "temp.nc")
    holding_nc <- "temp.nc"
    add_missing_grid(holding_nc, vars)
  }

  # copy the file to the temporary

  # Now, we need to select the variables we are interested in....
  if (!is.null(vars)) {
    system(stringr::str_glue("cdo selname,{stringr::str_flatten(vars, ", ")} {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect vars chosen. Set cdo_output = TRUE and inspect output.")
    }
    file.rename("dummy.nc", holding_nc)
  }

  # clip to the box
  lat_box <- stringr::str_flatten(c(lon_range, lat_range), collapse = ",")
  # if(is.null(vars))
  system(stringr::str_glue("cdo sellonlatbox,{lat_box}, {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))
  # throw an error if this did not work
  if (!file.exists("dummy.nc")) {
    stop("error: cdo cannot subselect the lonlat box. Set cdo_output = TRUE and inspect output.")
  }
  if (holding_nc == ff_orig) {
    holding_nc <- "temp.nc"
  }

  file.rename("dummy.nc", holding_nc)


  depths <- system(stringr::str_c("cdo showlevel ", holding_nc), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
    stringr::str_split(" ") %>%
    .[[1]] %>%
    as.numeric()

  depths <- depths[complete.cases(depths)]
  depths <- depths[depths <= vert_range[2] & depths >= vert_range[1]]

  if (!is.null(date_range)) {
    min_date <- lubridate::dmy(date_range[1])
    max_date <- lubridate::dmy(date_range[2])

    if (is.na(min_date) | is.na(max_date)) {
      stop("error check date range supplied")
    }


    system(stringr::str_c("cdo seldate,{min_date},{max_date} {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect the dates. Set cdo_output = TRUE and inspect output.")
    }
    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }
    file.rename("dummy.nc", holding_nc)
  }


  if (!is.null(vert_range)) {
    if (length(depths) == 0) {
      stop("error: no depths within the depth range selected")
    }

    levels_selected <- stringr::str_flatten(depths, ",")
    system(stringr::str_glue("cdo sellevel,{levels_selected} {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect the vertical levels. Set cdo_output = TRUE and inspect output.")
    }

    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }

    file.rename("dummy.nc", holding_nc)
  }

  if (!is.null(months)) {
    file_months <- system(stringr::str_c("cdo showmon ", holding_nc), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
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

    month_choice <- stringr::str_flatten(months, ",")
    system(stringr::str_glue("cdo selmonth,{month_choice} {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect the months. Set cdo_output = TRUE and inspect output.")
    }

    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }

    file.rename("dummy.nc", holding_nc)
  }

  if (!is.null(years)) {
    file_years <- system(stringr::str_c("cdo showyear ", holding_nc), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
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
    year_choice <- stringr::str_flatten(years, ",")
    system(stringr::str_glue("cdo selyear,{year_choice} {holding_nc} dummy.nc"), ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect the years. Set cdo_output = TRUE and inspect output.")
    }

    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }

    file.rename("dummy.nc", holding_nc)
  }

  if (is.null(out_file)) {
    nc_grid <- nc_read(holding_nc)

    # if there is only one depth layer, depth will not be in the data frame. It needs to be added back in
    if (length(depths) == 1) {
      nc_grid <- nc_grid %>%
        dplyr::mutate(Depth = depths[1])
    }

    # remove the temporary files created
    setwd(temp_dir)
    if (length(dir(temp_dir)) < 6 & temp_dir != init_dir) {
      unlink(temp_dir, recursive = TRUE)
    }

    return(nc_grid)
  }
  # save the file, if that's what you chose to do

  # zip the file if requested
  if (zip_file) {
    nc_zip(holding_nc, overwrite = TRUE)
  }

  # change the working directory back to the original
  setwd(init_dir)

  file.copy(stringr::str_c(temp_dir, holding_nc), out_file, overwrite = overwrite)

  # remove the temporary files created
  setwd(temp_dir)
  if (length(dir(temp_dir)) < 6 & temp_dir != init_dir) {
    unlink(temp_dir, recursive = TRUE)
  }
}
