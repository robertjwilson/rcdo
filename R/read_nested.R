

# to do
# I need an option for having a nested data frame
# add months and years options



#' @title read ncdf file to a data frame
#'
#' @description This function allows you to clip a netcdf file to a specific latlon box
#' @param ff This is the file to move. This must be the full system path to the file.
#' @param vars A list of variables you want to read in. Character vector. Everything is read in if this is empty.
#' @param date_range This is the range of dates you want. c(date_min, date_max). "day/month/year" character string format.
#' @param cdo_output Do you want to show the cdo output? Set to TRUE in case you want to troubleshoot errors.
#' @export

# need an option for cacheing results...

nc_read_nested <- function(ff, vars = NULL, date_range = NULL, cdo_output = FALSE) {
  if (!cdo_compatible(ff)) {
    stop("error: file is not cdo compatible")
  }


  init_dir <- getwd()
  on.exit(setwd(init_dir))
  temp_dir <- tempdir()
  dir.exists(temp_dir)

  setwd(temp_dir)

  # remove anything from the temporary folder to make sure there are no clashes etc.

  if (file.exists(stringr::str_c(temp_dir, "/raw.nc"))) {
    file.remove(stringr::str_c(temp_dir, "/raw.nc"))
  }
  if (file.exists(stringr::str_c(temp_dir, "/raw_clipped.nc"))) {
    file.remove(stringr::str_c(temp_dir, "/raw_clipped.nc"))
  }

  # copy the file to the temporary

  file.copy(ff, stringr::str_c(temp_dir, "/raw.nc"))

  ff <- "raw.nc"

  grid_details <- system(stringr::str_c("cdo griddes ", ff), intern = TRUE, ignore.stderr = TRUE)

  grid_details <- grid_details %>%
    dplyr::as_tibble()

  grid_details <- grid_details %>%
    dplyr::mutate(Total = 0) %>%
    dplyr::mutate(Total = ifelse(startsWith(value, "yunits"), 1, Total)) %>%
    dplyr::mutate(Total = cumsum(Total)) %>%
    dplyr::filter(Total < 1)

  grid_type <- grid_details %>%
    dplyr::filter(startsWith(value, "gridtype")) %>%
    tidyr::separate(value, into = c("ignore", "grid")) %>%
    dplyr::select(grid) %>%
    dplyr::pull(grid)

  grid_details <- grid_details %>%
    dplyr::filter(startsWith(value, "xname") | startsWith(value, "yname")) %>%
    tidyr::separate(value, into = c("ignore", "variable"), sep = "=") %>%
    dplyr::mutate(ignore = stringr::str_replace_all(ignore, " ", "")) %>%
    dplyr::mutate(variable = stringr::str_replace_all(variable, " ", "")) %>%
    tidyr::spread(ignore, variable)


  lon_name <- stringr::str_replace_all(grid_details$xname, " ", "")
  lat_name <- stringr::str_replace_all(grid_details$yname, " ", "")

  depths <- system(stringr::str_c("cdo showlevel ", ff), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
    stringr::str_split(" ") %>%
    .[[1]] %>%
    as.numeric()

  if (!is.null(date_range)) {
    min_date <- lubridate::dmy(date_range[1])
    max_date <- lubridate::dmy(date_range[2])

    if (is.na(min_date) | is.na(max_date)) {
      stop("error check date range supplied")
    }

    system(stringr::str_c("cdo seldate,", min_date, ",", max_date, " ", ff, " dummy.nc"))
    file.rename("dummy.nc", ff)
  }
  depths <- depths[complete.cases(depths)]

  times <- system(stringr::str_c("cdo showtimestamp ", ff), intern = TRUE, ignore.stderr = (cdo_output == FALSE)) %>%
    stringr::str_split(" ") %>%
    .[[1]]
  times <- times[nchar(times) > 0]

  # now, pull in the longitudes and latitudes...
  nc_raw <- ncdf4::nc_open(ff)
  nc_lon <- ncdf4::ncvar_get(nc_raw, lon_name)
  nc_lat <- ncdf4::ncvar_get(nc_raw, lat_name)

  # this is coded on the assumption that when there is only one depth and time, those dimensions will be collapsed to nothing
  # this should be a valid assumption


  if ("curvilinear" %nin% grid_type) {
    nc_grid <- eval(parse(text = stringr::str_c(
      "expand.grid(Longitude = nc_lon, Latitude = nc_lat",
      ifelse(length(depths) > 1, ",Depth = depths", ""),
      ifelse(length(times) > 1, ",Time = times", ""),
      ")"
    )))
  } else {
    if (length(depths) < 2 & length(times) < 2) {
      nc_grid <- dplyr::data_frame(Longitude = as.numeric(nc_lon), Latitude = as.numeric(nc_lat))
    } else {
      nc_grid <- dplyr::data_frame(Longitude = as.numeric(nc_lon), Latitude = as.numeric(nc_lat))

      dt_grid <- eval(parse(text = stringr::str_c(
        "expand.grid(",
        ifelse(length(depths) > 1, "Depth = depths", ""),
        ifelse(length(depths) > 1 & length(times) > 1, ",", ""),
        ifelse(length(times) > 1, "Time = times", ""),
        ")"
      )))

      nc_grid <- dt_grid %>%
        dplyr::mutate(data = purrr::map(
          1,
          function(x) return(nc_grid)
        )) %>%
        unnest()
    }
  }

  if (is.null(vars)) {
    vars <- system(stringr::str_c("cdo showname ", ff), intern = TRUE, ignore.stderr = TRUE)
    vars <- stringr::str_split(vars, " ") %>%
      .[[1]]
    vars <- vars[nchar(vars) > 0]
  }

  for (vv in vars) {
    nc_var <- ncdf4::ncvar_get(nc_raw, vv)
    nc_grid$var <- nc_var %>% as.numeric()
    names(nc_grid)[ncol(nc_grid)] <- vv
  }

  nc_grid <- nc_grid %>%
    tidyr::drop_na() %>%
    dplyr::as_tibble()

  ncdf4::nc_close(nc_raw)
  return(nc_grid)
}
