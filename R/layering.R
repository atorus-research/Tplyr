### Layering Interfaces

#' Attach a layer to a \code{tplyr_table} object
#'
#' @description
#' \code{add_layer} inserts a \code{tplyr_layer} into the \code{layers} container of a \code{tplyr_table} object. This allows
#' for a tidy style of programming (using \code{magrittr} piping - \code{\%>\%}) with a secondary advantage - the construction
#' of the layer object may consist of a series of piped functions itself.
#'
#' \code{tplyr} encourages a user to view the construction of a table as a series of "layers". The construction of each of these
#' layers are isolated and independent of one another - but each of these layers are children of the table itself. \code{add_layer}
#' isolates the construction of an individual layer and allows the user to construct that layer and insert it back into the parent.
#' The syntax for this is intuitive and allows for tidy piping. Simply pipe the current table object in, and write the code to
#' construct your layer within the \code{layer} parameter.
#'
#'
#' @param parent A \code{tplyr_table} or \code{tplyr_layer}/\code{tplyr_subgroup_layer} object
#' @param layer A layer construction function and associated modifier functions
#' @param name A name to provide the layer in the table layers container
#'
#' @family Layer attachment
#' @rdname layer_attachment
#'
#' @return A \code{tplyr_table} or \code{tplyr_layer}/\code{tplyr_subgroup_layer} with a new layer inserted into the \code{layer}
#'   binding
#'
#' @seealso [tplyr_table(), tplyr_layer(), group_count(), group_desc(), group_shift()]
#'
#' @export
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' ## Single layer
#' t <- tplyr_table(mtcars, cyl) %>%
#'   add_layer(
#'     group_desc(target_var=mpg)
#'   )
#'
#' ## Single layer with name
#' t <- tplyr_table(mtcars, cyl) %>%
#'   add_layer(name='mpg',
#'     group_desc(target_var=mpg)
#'   )
#'
#' # Using add_layers
#' t <- tplyr_table(mtcars, cyl)
#' l1 <- group_desc(t, target_var=mpg)
#' l2 <- group_count(t, target_var=cyl)
#'
#' t <- add_layers(t, l1, 'cyl' = l2)
#'
add_layer <- function(parent, layer, name=NULL) {

  assert_that(!missing(parent), msg = "`parent` parameter must be provided")
  assert_that(!missing(layer), msg = "`layer` parameter must be provided")

  # Capture the layer code as a quosure
  layer <- enquo(layer)
  dmessage(layer)

  # Insert the `parent` argument into the topmost call of the layer code
  # (i.e. if any pipes %>% then pull out the left most call and modify it)
  l <- modify_nested_call(layer, parent=parent)

  # Evaluate the layer and grab `tplyr_layer` or `tplyr_subgroup_layer` object
  executed_layer <- list(eval(quo_get_expr(l)))

  # Attach the name
  names(executed_layer) <- name

  # Insert the layer into the parent object
  parent$layers <- append(parent$layers, executed_layer)
  parent
}

#' @param parent A \code{tplyr_table} or \code{tplyr_layer}/\code{tplyr_subgroup_layer} object
#' @param ... Layers to be added
#'
#' @return
#' @export
#'
#' @family Layer attachment
#' @rdname layer_attachment
#'
#' @export
add_layers <- function(parent, ...) {
  # Parent exists
  assert_that(!missing(parent), msg = "`parent` parameter must be provided")
  # all objects are Tplyr layers
  map(list(...), assert_is_layer)

  # Insert the layer into the parent object
  parent$layers <- append(parent$layers, list(...))
  parent
}

#' Create a \code{count}, \code{desc}, or \code{shift} layer for discrete count based summaries, descriptive statistics summaries,
#'  or shift count summaries
#'
#' @description
#' This family of functions specifies the type of summary that is to be performed within a layer. \code{count} layers are used
#' to create summary counts of some discrete variable. \code{desc} layer create summary statistics, and \code{shift} layers
#' summaries the counts of different changes in states. See the "details" section below for more information.
#'
#' @param parent \code{tplyr_table} or \code{tplyr_layer}. Required. The parent environment of the layer. This must be either the
#'   \code{tplyr_table} object that the layer is contained within, or another \code{tplyr_layer} object of which
#'   the layer is a subgroup.
#' @param target_var Symbol. Required, The variable name(s) on which the summary is to be performed. Must be a variable within
#'   the target dataset. Enter unquoted - i.e. target_var = AEBODSYS. You may also provide multiple variables with
#'   \code{\link[dplyr]{vars}}.
#' @param by A string, a variable name, or a list of variable names supplied using \code{\link[dplyr]{vars}}
#' @param where Call. Filter logic used to subset the target data when performing a summary.
#' @param ... Additional arguments that will be passed directly into the \code{tplyr_layer} environment. See the
#'   \href{<link tbd>}{vignette} on adding extensions.
#'
#' @details
#' \describe{
#'   \item{Count Layers}{Count layers allow you to easily create summaries based on counting values with a variable.
#'   Additionally, this layer allows you to create n (%) summaries where you're also summarizing the proportion of
#'   instances a value occurs compared to some denominator. Count layers are also capable of producing counts of nested relationships.
#'   For example, if you want to produce counts of an overall outside group, and then the subgroup counts within that group, you can
#'   simply specify the target variable as vars(OutsideVariable, InsideVariable). This allows you to do tables like Adverse Events
#'   where you want to see the Preferred Terms within Body Systems, all in one layer.
#'   \strong{NOTE:} Currently, % values are calculated on the fly
#'   using header N values calculated from the target dataset. This is something that we will be adding enhanced flexibility
#'   for in future releases (remember - this is an alpha release :))}
#'   \item{Descriptive Statistics Layers}{Descriptive statistics layers perform summaries on continuous variables. There are
#'   a number of summaries built into Tplyr already that you can perform, including n, mean, median, standard deviation,
#'   variance, min, max, interquartile range, Q1, Q3, and missing value counts. From these available summaries, the default presentation
#'   of a descriptive statistic layer will output 'n', 'Mean (SD)', 'Median', 'Q1, Q3', 'Min, Max', and 'Missing'.
#'   You can change these summaries using \code{\link{set_format_strings}}, and you can also add your own summaries using
#'   \code{\link{set_custom_summaries}}. This allows you to easily implement any additional summary statistics you want presented.}
#'   \item{Shift Layers}{A shift layer displays an endpoints 'shift' throughout
#'   the duration of the study. It is an abstraction over the count layer,
#'   however we have proveded a method that is more effeicent, and more
#'   intuitive. Targets are passed as named quosures using `dplyr::vars`.
#'   Geneerally the baseline flag is passed with the name 'row' and the shift is
#'   passed with the name 'column'. Both counts(n) and percentages(pct) are
#'   supported and can be specifed with the `set_format_strings` function. To
#'   allow for flexibility when defining percetnages, you can define the
#'   denominator using the `set_denom_by` function. This function takes variable
#'   names and uses those to determine the denominator for the counts.}
#' }
#'
#' @return An \code{tplyr_layer} environment that is a child of the specified parent. The environment contains the object
#'   as listed below.
#'
#' @section \code{tplyr_layer} Object Structure:
#' \describe{
#' \item{\code{type}}{This is an attribute. A string indicating the layer type, which controls the summary that will be performed.}
#' \item{\code{target_var}}{A quosure of a name, which is the variable on which a summary will be performed.}
#' \item{\code{by}}{A list of quosures representing either text labels or variable names used in grouping. Variable names must exist
#' within the target dataset Text strings submitted do not need to exist in the target dataset.}
#' \item{\code{where}}{A quosure of a call that containers the filter logic used to subset the target dataset. This is
#' in addition to the subset applied in \code{\link{tplyr_table}}}
#' \item{\code{sort}}{A string containing the sort method. Defaults to 'asc' for ascending. NOTE: Not yet implemented in practice}
#' \item{\code{layers}}{A list with class \code{tplyr_layer_container}. Initialized as empty, but serves as the container for
#' any sublayers of the current layer.}
#' }
#'
#' @return A \code{tplyr_layer} object
#'
#' @family Layer construction functions
#'
#' @rdname layer_constructors
#'
#' @seealso [add_layer(), add_layers() tplyr_table(), tplyr_layer()]
#'
#' @export
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' t <- tplyr_table(iris, Sepal.Width) %>%
#'   add_layer(
#'     group_desc(target_var=Species)
#'   )
group_count <- function(parent, target_var, by=vars(), where=TRUE, ...) {
  tplyr_layer(parent, type='count', by=enquos(by), target_var=enquos(target_var), where=enquo(where), ...)
}

#' @rdname layer_constructors
#' @export
group_desc <- function(parent, target_var, by=vars(), where=TRUE, ...) {
  tplyr_layer(parent, type='desc', by=enquos(by), target_var=enquos(target_var), where=enquo(where), ...)
}

#' @rdname layer_constructors
#' @export
group_shift <- function(parent, target_var, by=vars(), where=TRUE, ...) {
  tplyr_layer(parent, type='shift', by=enquos(by), target_var=enquos(target_var), where=enquo(where), ...)
}
