


#' @title get variables available in netcdf file
#'
#' @description This function will tell you the variables in a netcdf file
#' @param ff This is the file to check. This must be the full system path to the file.
#' @export

# need an option for cacheing results...

nc_variables <- function(ff) {

	ff_names <<- system(stringr::str_c("cdo showname ", ff), intern = TRUE, ignore.stderr = TRUE) %>%
		stringr::str_split(" ") %>%
		.[[1]]
	ff_names <- ff_names[ff_names != ""]
	ff_names <- as.character(ff_names)
	ff_names

}




