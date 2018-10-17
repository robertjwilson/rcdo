


#' @title get dates available in netcdf file
#'
#' @description This function will tell you the dates in a netcdf file
#' @param ff This is the file to check. Must be the full system path to the file.
#' @export

# need an option for cacheing results...

nc_dates <- function(ff) {

	ff_dates <<- system(stringr::str_c("cdo showtimestamp ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
		stringr::str_split(" ") %>%
		.[[1]]
	ff_dates <- ff_dates[ff_dates != ""]
	ff_dates <- as.Date(ff_dates)
	dplyr::data_frame(Date = ff_dates) %>%
		dplyr::mutate(Year = lubridate::year(Date)) %>%
		dplyr::mutate(Month = lubridate::month(Date)) %>%
		dplyr::mutate(Day = lubridate::day(Date)) %>%
		dplyr::distinct()

}



