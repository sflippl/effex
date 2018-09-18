#' @describeIn fx_write exports to a json format via [jsonlite::toJSON()].
#'
#' @section JSON formats:
#' Currently, only a json format is implemented. This format represents a good
#' compromise between flexibility and readability. However, it is advised to
#' make certain that the retrieval via [fx_read()] yields the same data. Due to
#' the sparse specification that comes with readability for non-programmers,
#' some tweaks in the retrieval function might have to be added. If `data` is
#' grouped, a sub-folder will be created for every grouping variable. In order
#' to ensure that the folders are approriately named, we remove or replace the
#' following characters: `.`, `:`, `*`, `?`, `\"`, `<`, `>`, `/`, `\\` and `|`.
#'
#' @param data either a metaframe or an object with attribute metaframe
#' @param path The path specifying the folder of the different objects
#' @param reader specify your own reader for the resulting `filepath` columns.
#' @param write_params A list of additional parameters for
#' [jsonlite::write_json()]
#' @param ... A name-value pair of the name of the file and the respective
#' columns that are to be saved in that file
#'
#' @export

fx_write_json <- function(data, path, ...,
                          reader = NULL,
                          write_params = list(
                            pretty = TRUE,
                            null = "null"
                          )) {
  if(is.null(reader)) reader <- function(file, ...) {
    jsonlite::read_json(file, simplifyVector = TRUE) %>%
      dplyr::as_tibble()
  }
  file_specs <- rlang::dots_list(...)
  if(length(file_specs) == 0) return(data)
  file_data <- json_file_data(data, path)
  assertthat::assert_that(
    !dir.exists(path),
    msg = "You should specify a folder that does not exist yet so that nothing is broken"
  )
  purrr::walk(file_data[[".folder_path"]],
              ~ if(!dir.exists(.)) dir.create(., recursive = TRUE))
  # Grab the data that remains unaffected (except for the temporary variables)
  data_unaff <- dplyr::select(file_data,
                              -!!unlist(file_specs), -.folder_path, name)
  # Create the affected data, consisting of the .file_path and a tibble list
  # column for every exported file.
  data_aff <- purrr::map(names(file_specs),
                         ~ tidyr::nest(file_data, !!file_specs[[.]], name,
                                       .key = !!.)) %>%
    purrr::reduce(~ dplyr::inner_join(.x, .y, by = ".file_path"))
  # For every kind of file nam save the tibble from every row i
  for(nam in names(file_specs)) {
    # The separation is different if the file path is empty so far:
    sep <-
      dplyr::if_else(stringr::str_detect(data_aff[[".file_path"]], "\\/$"),
                     "", "-")
    tmp_paths <- paste0(data_aff[[".file_path"]], sep, nam, ".json")
    for(i in seq_len(nrow(data_aff))) {
      do.call("write_json",
              c(list(x = data_aff[[nam]][[i]], path = tmp_paths[i]),
                write_params),
              envir = asNamespace("jsonlite"))
    }
    data_aff[[nam]] <- new_filepath(
      tmp_paths, fields = file_specs[[nam]],
      reader = reader
    )
  }
  ret_data <- dplyr::left_join(data_unaff, data_aff, by = ".file_path") %>%
    dplyr::ungroup() %>%
    dplyr::select(-.file_path)
  ret_data
}

#' Change filenames so that they are suitable for file naming
#'
#' Removes or replaces characters that cannot name a file or folder.

save_filenames <- function(filenames) {
  filenames <- stringr::str_replace_all(filenames, "\\/", " or ") %>%
    stringr::str_replace_all("\"", "'") %>%
    stringr::str_replace_all("[\\.:\\*\\?<>|\\\\]", " ") %>%
    stringr::str_trim()
  filenames
}

#' Generate the columns .folder_path and .file_path
#'
#' This function generates the paths for a data frame. It returns a data frame
#' that is not grouped by any variables except `.file_path`. It also cleans the
#' potential file names (see [save_filenames()])

json_file_data <- function(data, path) {
  grouping_vars <- dplyr::group_vars(data)
  for(nam in grouping_vars) {
    data[[paste0(".", nam)]] <- save_filenames(data[[nam]])
  }
  # The .grouping_vars are clean versions of the grouping variables.
  .grouping_vars <- paste0(".", grouping_vars)
  # If the data is not grouped, this is much easier.
  if(dplyr::is_grouped_df(data)) {
    file_data <- dplyr::ungroup(data) %>%
      tidyr::unite(".folder_path", !!.grouping_vars, sep = "/",
                   remove = FALSE) %>%
      tidyr::unite(".file_path", !!.grouping_vars, sep = " - ",
                   remove = TRUE) %>%
      dplyr::mutate(
        .folder_path = paste(path, .folder_path, sep = "/"),
        .file_path = paste0(.folder_path, "/", .file_path)
      ) %>%
      dplyr::group_by(.file_path)
  }
  else {
    file_data <- dplyr::mutate(data,
      .folder_path = path,
      .file_path = paste0(path, "/")
    ) %>%
      dplyr::group_by(.file_path)
  }
  file_data
}
