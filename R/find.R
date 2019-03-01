

#' @title search netcdf files in folder
#'
#' @description This function will list the netcdf files in a directory that contain a specified variable
#' @param directory This is the directory to check.
#' @param vars a character string of the variable to search for
#' @return list of files
#' @export

nc_find <- function(directory = getwd(), var = NULL, recursive = TRUE) {
	setwd(directory)
	on.exit(setwd(getwd()))

	if(is.null(var))
		stop("error: no variable named")
	if(length(var) > 1)
		stop("error: more than one variable given")

	print("getting here")

	# step 1. Get a list of netcdf files in the folder
	all_files <- dir(directory, recursive = recursive) %>%
		tibble::enframe(name = NULL) %>%
		dplyr::rename(File = value) %>%
		dplyr::filter(endsWith(File, ".nc"))
	print("getting here slowly?")

	#
	contains_var <- function(ff){
		print(stringr::str_glue("searching {ff}"))
		var %in% nc_variables(ff)
	}

	all_files <- all_files %>%
		dplyr::mutate(contains = purrr::map_lgl(File, contains_var)) %>%
		dplyr::filter(contains) %>%
		dplyr::select(File) %>%
		dplyr::pull(File)

	all_files
}


