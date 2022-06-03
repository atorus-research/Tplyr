#' Extract the result metadata of a Tplyr table
#'
#' Given a row_id value and a result column, this function will return the
#' tplyr_meta object associated with that 'cell'.
#'
#' @param t A built Tplyr table
#' @param row_id The row_id value of the desired cell
#' @param column The result column of interest
#'
#' @return a tplyr_meta object
#' @export
#'
#' @examples
#'
#' # TODO
get_meta_result <- function(t, row_id, column) {
  m <- t$metadata

  if (!(row_id %in% m$row_id)) {
    stop('Invalid row_id selected. row_id must be present in built Tplyr table.', call.=FALSE)
  }

  # Pull out the cell of interest
  res <- m[[which(m$row_id == row_id), column]][[1]]

  if (!inherits(res, 'tplyr_meta')) {
    stop('Specified column must be a result column', call.=FALSE)
  }

  res
}

#' Extra a subset of the target data of a Tplyr table
#'
#' Given a row_id value and a result column, this function will return the
#' subset of data from the target dataset which was used to produce that "cell"
#'
#' @param t A built Tplyr table
#' @param row_id The row_id value of the desired cell
#' @param column The result column of interest
#' @param add_cols Additional columns
#'
#' @return
#' @export
#'
#' @examples
#'
#' # TODO
get_meta_subset <- function(t, row_id, column, add_cols = vars(USUBJID)) {

  # Get the metadata object ready
  m <- get_meta_result(t, row_id, column)

  # Subset and return the data
  t$target %>%
    filter(!!!m$filters) %>%
    select(!!!add_cols, !!!m$names)
}
