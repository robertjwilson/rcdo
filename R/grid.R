
#' @title Extract the horizontal grid from a netcdf file
#'
#' @description This function longitude/latitude grid cells in a netcdf file
#' @param ff This is the file to analyze.
#' @param cdo_output Do you want to show the cdo output? Set to TRUE in case you want to troubleshoot errors.
#' @export

#' @examples
#'
#' # Get the horizontal grid of the sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_grid(ff)
nc_grid <- function(ff, cdo_output = FALSE) {
  if (!file_valid(ff)) {
    stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
  }
  if (!cdo_compatible(ff)) {
    stop("error: file is not cdo compatible")
  }

  ff <- ff

  grid_details <- system(stringr::str_c("cdo griddes ", ff), intern = TRUE, ignore.stderr = TRUE)

  grid_details <- grid_details %>%
    tibble::enframe(name = NULL)

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

  # now, pull in the longitudes and latitudes...
  nc_raw <- ncdf4::nc_open(ff)
  nc_lon <- ncdf4::ncvar_get(nc_raw, lon_name)
  nc_lat <- ncdf4::ncvar_get(nc_raw, lat_name)

  # this is coded on the assumption that when there is only one depth and time, those dimensions will be collapsed to nothing
  # this should be a valid assumption

  if ("lonlat" %in% grid_type) {
    nc_grid <- eval(parse(text = stringr::str_c(
      "expand.grid(Longitude = nc_lon, Latitude = nc_lat",
      ")"
    )))
  } else {
    if (length(depths) < 2 & length(times) < 2) {
      nc_grid <- dplyr::data_frame(Longitude = as.numeric(nc_lon), Latitude = as.numeric(nc_lat))
    } else {
      nc_grid <- dplyr::data_frame(Longitude = as.numeric(nc_lon), Latitude = as.numeric(nc_lat))

    }
  }

  nc_grid
}
