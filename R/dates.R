


#' @title Find dates in a netcdf file
#'
#' @description Find dates in a netcdf file
#' @param ff This is the file to read dates from.
#' @export

# need an option for cacheing results...

#'@examples

#' # Get the dates in the sample file
#' ff <- system.file("extdata", "woa18_decav_t01_01.nc", package = "rcdo")
#' nc_dates(ff)
#'

nc_dates <- function(ff) {


	if(!file_valid(ff))
		stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))

	ff_dates <<- system(stringr::str_c("cdo showtimestamp ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
		stringr::str_split(" ") %>%
		.[[1]]
	ff_dates <- ff_dates[ff_dates != ""]
	ff_dates <- as.Date(ff_dates)
	dplyr::data_frame(Date = ff_dates) %>%
		dplyr::distinct() %>%
		dplyr::mutate(Year = lubridate::year(Date)) %>%
		dplyr::mutate(Month = lubridate::month(Date)) %>%
		dplyr::mutate(Day = lubridate::day(Date))

}



