

#' @title get the depths available in a netcdf file
#'
#' @description This function will tell you the depths available in a netcdf file
#' @param ff This is the file to check.
#' @export

# need an option for cacheing results...

nc_depths <- function(ff) {

	ff_depths <<- system(stringr::str_c("cdo showlevel ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
		stringr::str_split(" ") %>%
		.[[1]]
	ff_depths <- ff_depths[ff_depths != ""]
	ff_depths <- as.numeric(ff_depths)
	if(length(ff_depths) < 2)
		return("No depth information")


	dplyr::data_frame(Depth = ff_depths) %>%
		dplyr::distinct()

}



