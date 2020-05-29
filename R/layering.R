### Layering Interfaces

#' Attach a layer to a \code{tplyr_table} object
#'
#' @param parent
#' @param layer
#'
#' @return
#' @export
#'
#' @examples
add_layer <- function(parent, layer) {

  # Capture the layer code as a quosure
  layer <- enquo(layer)

  # Insert the `parent` argument into the topmost call of the layer code
  # (i.e. if any pipes %>% then pull out the left most call and modify it)
  l <- modify_nested_call(layer, parent=parent)
  dmessage(paste('Modified call:', as_label(l)))

  # Evaluate the layer and grab `tplyr_layer` or `tplyr_subgroup_layer` object
  executed_layer <- eval(quo_get_expr(l))

  # Insert the layer into the parent object
  parent$layers <- append(parent$layers, executed_layer)
  parent
}

#' Create a \code{count} layer for discrete count based summaries
#'
#' @param parent
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
group_count <- function(parent, ...) {
  tplyr_layer(parent, type='count', ...)
}

#' Create a \code{desc} layer for descriptive statistics summaries
#'
#' @param parent
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
group_desc <- function(parent, ...) {
  tplyr_layer(parent, type='desc', ...)
}

#' Create a \code{count} layer for shift count summaries
#'
#' @param parent
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
group_shift <- function(parent, ...) {
  tplyr_layer(parent, type='shift', ...)
}
