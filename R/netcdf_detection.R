

#' @title search netcdf files in folder
#'
#' @description This function will list the netcdf files in a directory that contain a specified variable
#' @param directory This is the directory to check.
#' @param vars a character string of the variable to search for
#' @return list of files
#' @export

nc_detect <- function(directory = getwd(), var = NULL, recursive = TRUE) {
	on.exit(setwd(getwd()))
	setwd(directory)

	if(is.null(var))
		stop("error: no variable named")
	if(length(var) > 1)
		stop("error: more than one variable given")

	# step 1. Get a list of netcdf files in the folder
	all_files <- dir(directory, recursive = recursive) %>%
		dplyr::as_tibble() %>%
		dplyr::rename(File = value) %>%
		dplyr::filter(endsWith(File, ".nc"))

	#
	contains_var <- function(ff){
		var %in% nc_variables(ff)
	}

	all_files <- all_files %>%
		dplyr::mutate(contains = purrr::map_lgl(File, contains_var)) %>%
		dplyr::filter(contains) %>%
		dplyr::select(File) %>%
		dplyr::pull(File)

	all_files
}


