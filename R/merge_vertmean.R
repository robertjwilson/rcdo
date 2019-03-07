
# function to merge, remap and calculate a vertical mean from a list of netcdf files

#' @title merge and calculate the vertical means for a list of ncdf files
#' @description This function allows you to remap a netcdf horizontally and vertically to a specific latlon box
#' @param ff_list This is a character vector containing the files to merge
#' @param merge This is the merge type. Use "merge" to combine files, "mergetime" to merge based on time.
#' @param vars The variables to select from the files for merging..
#' @param coords A lonlat grid to regrid to. Longitude/Latitude columns. No horizontal mapping occurs if not supplied.
#' @param vert_scale This is the vertical scale you want. c(min_depth, max_depth, vertical_resolution).
#' @param expr This is a cdo expression to apply to the merged files.
#' @param remap_point This is the remap_point. Set to "pre" if you want to remap the files before merging, or "post" if you want to remap post-merging. The default is pre as this insures against horizontal grids being slightly different.
#' @param na_value This is a value in the raw netcdf file that needs to be treated as an na.#'
#' @param out_file The name of the file output. If this is not stated, a data frame will be the output.
#' @param cdo_output set to TRUE if you want to see the cdo output
#' @param overwrite Do you want to overwrite out_file if it exists? Defaults to FALSE
#' @return data frame or netcdf file.
#' @export

nc_merge_vertmean <- function(ff_list, vars = NULL, coords = NULL, vert_scale, merge = "merge", expr = NULL, remap_point = "pre", na_value = NULL, out_file = NULL, cdo_output = TRUE, overwrite = FALSE) {
  if (remap_point %nin% c("pre", "post")) {
    stop(stringr::str_glue("error: remap_point = {remap_point} is not valid"))
  }
  # first, make sure the vertical grid provided is acceptable
  if (!is.numeric(vert_scale)) {
    stop("error vertical scale is not numeric")
  }

  if (vert_scale[1] > vert_scale[2]) {
    stop("error: check ordering of vertical scale")
  }

  if (vert_scale[1] < 0 | vert_scale[2] < 0) {
    stop("error: check that vertical scale is positive")
  }

  if (vert_scale[3] < 0) {
    stop("error: check ordering of vertical scale")
  }
  # loop through the files
  for (ff in ff_list)
    if (!file_valid(ff)) {
      stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
    }

  # take note of the working directory, so that it can be reset to this on exit

  init_dir <- getwd()
  on.exit(setwd(init_dir))

  # Create a temporary directory and move the file we are manipulating to it...
  temp_dir <- random_temp()

  # copy the file to the temporary

  new_ens <- c()

  tracker <- 1
  for (ff in ff_list) {
    ens_file <- stringr::str_glue("raw{tracker}.nc")
    new_ens <- c(new_ens, ens_file)
    tracker <- tracker + 1
    file.copy(ff, stringr::str_c(temp_dir, ens_file), overwrite = TRUE)
  }

  setwd(temp_dir)

  if (getwd() == init_dir) {
    stop("error: there was a problem changing the directory")
  }

  if (getwd() != temp_dir) {
    stop("error: there was a problem changing the directory")
  }

  temp_dir <- stringr::str_c(temp_dir, "/")

  # Now, we possibly need to remap the data pre-merging. Do this.

  remapped <- FALSE

  if (!is.null(coords)) {
    vert_seq <- seq(vert_scale[1], vert_scale[2], vert_scale[3])

    for (ff in new_ens) {
      if (!is.null(vars)) {
        var_select <- nc_variables(ff)
        var_select <- var_select[var_select %in% vars]
        # throw an error if none of the variables in the file are in vars
        if (length(var_select) == 0) {
          stop(stringr::str_glue("error: check that the variables in the ensemble files are in vars"))
        }

        nc_remap(ff, vars = var_select, coords = coords, vert_depths = vert_seq, out_file = "dummy.nc")
      }
      if (is.null(var_select)) {
        nc_remap(ff, coords = coords, vert_depths = vert_seq, out_file = "dummy.nc")
      }

      file.rename("dummy.nc", ff)
      if (!is.null(na_value)) {
        system(stringr::str_glue("cdo -setmissval,{na_value} {ff} dummy.nc"))
        file.rename("dummy.nc", ff)
      }
    }

    remapped <- TRUE
  }

  # OK. We now need to merge the files...

  ens_string <- stringr::str_flatten(new_ens, collapse = " ")

  system(stringr::str_glue("cdo {merge} {ens_string} merged.nc"))

  # we no longer need the ensemble files post-merging.
  # Delete them

  file.remove(new_ens)

  # now, apply the expression if it has been supplied.....
  # First we need to make sure the expression has no spaces

  if (!is.null(expr)) {
    expr <- stringr::str_replace_all(expr, " ", "")
    print(expr)
    system(stringr::str_glue("cdo aexpr,'{expr}' merged.nc dummy.nc"))
    # throw error if remapping fails
    if (!file.exists("dummy.nc")) {
      stop("error: problem apply expr. Please check expr and consider setting cdo_output = TRUE and rerun")
    }
    file.rename("dummy.nc", "merged.nc")
  }

  # Now, remap the merged files if needed
  if (remap_point == "post") {
    vert_seq <- seq(vert_scale[1], vert_scale[2], vert_scale[3])
    nc_remap("merged.nc", out_file = "dummy.nc", coords = coords, vert_depths = )

    # throw error if remapping fails
    if (!file.exists("dummy.nc")) {
      stop("error: problem remapping merged files. Please consider setting cdo_output = TRUE and rerun")
    }

    file.rename("dummy.nc", "merged.nc")
    remapped <- TRUE
  }

  # do a vertical interpolation if the files have not been remapped already

  if (remapped == FALSE) {
    vert_seq <- seq(vert_scale[1], vert_scale[2], vert_scale[3])
    vert_seq <- stringr::str_flatten(vert_seq, collapse = ",")
    system(stringr::str_glue("cdo intlevel,{vert_seq} merged.nc dummy.nc"))
    # throw error if vertical interpolation failed
    if (!file.exists("dummy.nc")) {
      stop("error: problem doing the vertical interpolation. Please consider setting cdo_output = TRUE and rerun")
    }
    file.rename("dummy.nc", "merged.nc")
  }

  # then calculate the minimum and maximum depths for each cell
  max_depths <- nc_read("merged.nc") %>%
    tidyr::drop_na()

  # it's possible only one vertical depth has been selected. Add depth in in this case
  depths_selected <- seq(vert_scale[1], vert_scale[2], vert_scale[3])

  if (length(depths_selected) == 1) {
    max_depths <- max_depths %>%
      dplyr::mutate(Depth = depths_selected[1])
  }

  max_depths <- max_depths %>%
    dplyr::group_by(Longitude, Latitude) %>%
    dplyr::summarize(Maximum_Depth = max(Depth), Minimum_Depth = min(Depth)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Longitude, Latitude, Minimum_Depth, Maximum_Depth)
  # finally, do the vertical mean
  system("cdo vertmean merged.nc dummy.nc")
  # throw error if vertical interpolation failed
  if (!file.exists("dummy.nc")) {
    stop("error: problem calculating the vertical mean. Please consider setting cdo_output = TRUE and rerun")
  }
  file.rename("dummy.nc", "merged.nc")

  # read in the merged file to a data frame if there is no out_file
  if (is.null(out_file)) {
    result <- nc_read("merged.nc")
    result <- result %>%
      dplyr::inner_join(max_depths)

  # remove the temporary files created
  setwd(temp_dir)
  if(length(dir(temp_dir)) < 6 & temp_dir != init_dir)
  	unlink(temp_dir, recursive = TRUE)

    return(result)
  }

  # if out_file is given, save the merged nc file to this

  setwd(init_dir)
  file.copy(stringr::str_c(temp_dir, "/merged.nc"), out_file, overwrite = overwrite)

  # remove the temporary files created
  setwd(temp_dir)
  if(length(dir(temp_dir)) < 6 & temp_dir != init_dir)
  	unlink(temp_dir, recursive = TRUE)


}
