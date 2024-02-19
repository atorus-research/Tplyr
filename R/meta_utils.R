#' Extract the result metadata of a Tplyr table
#'
#' Given a row_id value and a result column, this function will return the
#' tplyr_meta object associated with that 'cell'.
#'
#' If a Tplyr table is built with the `metadata=TRUE` option specified, then
#' metadata is assembled behind the scenes to provide traceability on each
#' result cell derived. The functions `get_meta_result()` and
#' `get_meta_subset()` allow you to access that metadata by using an ID provided
#' in the row_id column and the column name of the result you'd like to access.
#' The purpose is of the row_id variable instead of a simple row index is to
#' provide a sort resistant reference of the originating column, so the output
#' Tplyr table can be sorted in any order but the metadata are still easily
#' accessible.
#'
#' The `tplyr_meta` object provided a list with two elements - names and
#' filters. The metadata contain every column from the target data.frame of the
#' Tplyr table that factored into the specified result cell, and the filters
#' contains all the necessary filters to subset to data summarized to create the
#' specified result cell. `get_meta_subset()` additionally provides a parameter to
#' specify any additional columns you would like to include in the returned
#' subset data frame.
#'
#' @param x A built Tplyr table or a dataframe
#' @param row_id The row_id value of the desired cell, provided as a character
#'   string
#' @param column The result column of interest, provided as a character string
#' @param ... additional arguments
#'
#' @return A tplyr_meta object
#' @md
#'
#' @export
#'
#' @examples
#' t <- tplyr_table(mtcars, cyl) %>%
#'   add_layer(
#'     group_desc(hp)
#'   )
#'
#' dat <- t %>% build(metadata = TRUE)
#'
#' get_meta_result(t, 'd1_1', 'var1_4')
#'
#' m <- t$metadata
#' dat <- t$target
#'
#' get_meta_result(t, 'd1_1', 'var1_4')
get_meta_result <- function(x, row_id, column, ...) {
  UseMethod("get_meta_result")
}

#' @export
get_meta_result.tplyr_table <- function(x, row_id, column, ...) {
  m <- x$metadata

  get_meta_result.data.frame(m, row_id, column, ...)
}

#' @export
get_meta_result.data.frame <- function(x, row_id, column, ...) {
  if (!inherits(row_id, 'character') || !(row_id %in% x$row_id)) {
    stop('Invalid row_id selected. row_id must be provided as a string present in built Tplyr table.',
         call.=FALSE)
  }

  if (!inherits(column, 'character') || !(column %in% names(x))) {
    stop(paste0('column must provided as a character string and a valid result ',
                'column present in the built Tplyr dataframe'), call.=FALSE)
  }

  if (length(list(...)) > 0) {
    warning("Extra arguments were provided to get_meta_result() that will not be used.", immediate.=TRUE)
  }

  # Pull out the cell of interest
  res <- x[[which(x$row_id == row_id), column]][[1]]

  if (!inherits(res, 'tplyr_meta')) {
    stop('Specified column must be a result column', call.=FALSE)
  }

  res
}

#' Extract the subset of data based on result metadata
#'
#' Given a row_id value and a result column, this function will return the
#' subset of data referenced by the tplyr_meta object associated with that
#' 'cell', which provides traceability to tie a result to its source.
#'
#' If a Tplyr table is built with the `metadata=TRUE` option specified, then
#' metadata is assembled behind the scenes to provide traceability on each
#' result cell derived. The functions `get_meta_result()` and
#' `get_meta_subset()` allow you to access that metadata by using an ID provided
#' in the row_id column and the column name of the result you'd like to access.
#' The purpose is of the row_id variable instead of a simple row index is to
#' provide a sort resistant reference of the originating column, so the output
#' Tplyr table can be sorted in any order but the metadata are still easily
#' accessible.
#'
#' The `tplyr_meta` object provided a list with two elements - names and
#' filters. The metadata contain every column from the target data.frame of the
#' Tplyr table that factored into the specified result cell, and the filters
#' contains all the necessary filters to subset to data summarized to create the
#' specified result cell. `get_meta_subset()` additionally provides a parameter
#' to specify any additional columns you would like to include in the returned
#' subset data frame.
#'
#' @param x A built Tplyr table or a dataframe
#' @param row_id The row_id value of the desired cell, provided as a character
#'   string
#' @param column The result column of interest, provided as a character string
#' @param add_cols  Additional columns to include in subset data.frame output
#' @param target A data frame to be subset (if not pulled from a Tplyr table)
#' @param pop_data A data frame to be subset through an anti-join (if not pulled
#'   from a Tplyr table)
#' @param ... additional arguments
#'
#' @return A data.frame
#' @rdname get_meta_subset
#' @md
#'
#' @export
#'
#' @examples
#' t <- tplyr_table(mtcars, cyl) %>%
#'   add_layer(
#'     group_desc(hp)
#'   )
#'
#'
#' dat <- t %>% build(metadata = TRUE)
#'
#' get_meta_subset(t, 'd1_1', 'var1_4', add_cols = dplyr::vars(carb))
#'
#' m <- t$metadata
#' dat <- t$target
#'
#' get_meta_subset(t, 'd1_1', 'var1_4', add_cols = dplyr::vars(carb), target = target)
get_meta_subset <- function(x, row_id, column, add_cols = vars(USUBJID), ...) {
  UseMethod("get_meta_subset")
}

#' @export
#' @rdname get_meta_subset
get_meta_subset.data.frame <- function(x, row_id, column,
                                       add_cols = vars(USUBJID),
                                       target = NULL, pop_data = NULL, ...) {
  # Get the metadata object ready
  m <- get_meta_result(x, row_id, column)

  if (!inherits(add_cols, 'quosures')) {
    stop("add_cols must be provided using `dplyr::vars()`", call.=FALSE)
  }

  # Subset and return the data
  if (is.null(target)) {
    stop("If querying metadata without a tplyr_table, a target must be provided", call.=FALSE)
  }

  if (length(list(...)) > 0) {
    warning("Extra arguments were provided to get_meta_subset() that will not be used.", immediate.=TRUE)
  }

  out <- target %>%
    filter(!!!m$filters) %>%
    select(!!!add_cols, !!!m$names)

  if (!is.null(m$anti_join)) {
    aj <- m$anti_join
    pd <- pop_data %>%
      filter(!!!aj$join_meta$filters) %>%
      select(!!!aj$on, !!!add_cols, !!!aj$join_meta$names)

    mrg_var <- map_chr(aj$on, as_name)
    names(mrg_var) <- mrg_var

    if (!(mrg_var %in% names(pd)) | !(mrg_var %in% names(out))) {
      stop(paste0(
        "The `on` variable specified is missing from either the target data or the population data subsets.\n  ",
        "Try adding the `on` variables to the `add_cols` parameter")
      )
    }
    out <- anti_join(pd, out, by=mrg_var)
  }

  out
}

#' @export
#' @rdname get_meta_subset
get_meta_subset.tplyr_table <- function(x, row_id, column, add_cols = vars(USUBJID), ...) {

  # Get the metadata object ready
  m <- get_meta_result(x, row_id, column)

  get_meta_subset(x$metadata, row_id, column, add_cols = add_cols,
                  target = x$target, pop_data = x$pop_data)
}

