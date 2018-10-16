

`%nin%` <- function(x, y) x %in% y == FALSE

"%>%" <- magrittr::`%>%`


# library(tidyverse)

bin_value.numeric <- function(x, bin_res) {
	floor((x + bin_res / 2) / bin_res + 0.5) * bin_res - bin_res / 2
}


# function to bin coordinates

bin_value <- function(x, bin_res) {
	UseMethod("bin_value")
}


cdo_compatible <- function(ff){

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

if (nrow(grid_details) == 0)
	return(FALSE) else
		return(TRUE)


}

