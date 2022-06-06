#' Tplyr Metadata Object
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
#' filters. The names contain every column from the target data.frame of the
#' Tplyr table that factored into the specified result cell, and the filters
#' contains all the necessary filters to subset the target data to create the
#' specified result cell. `get_meta_subset()` additionally provides a parameter to
#' specify any additional columns you would like to include in the returned
#' subset data frame.
#'
#' @param names List of symbols
#' @param filters  List of expressions
#'
#' @return tplyr_meta object
#' @export
#'
#' @examples
#'
#' tplyr_meta(
#'    names = quos(x, y, z),
#'    filters = quos(x == 1, y==2, z==3)
#'  )
#'
tplyr_meta <- function(names, filters) {
  meta <- new_tplyr_meta()
  meta <- add_variables(meta, names)
  meta <- add_filters(meta, filters)
  meta
}

#' Create a tplyr_meta object
#'
#' @return tplyr_meta object
new_tplyr_meta <- function() {
  structure(
    list(
      names = list(),
      filters = exprs()
    ),
    class = 'tplyr_meta'
  )
}

#' Add variables to a tplyr_meta object
#'
#'
#' @param meta tplyr_meta object
#' @param names Variables to be added
#'
#' @return tplyr_meta object
#' @noRd
add_variables <- function(meta, names) {
  meta$names <- append(meta$names, names)
  meta
}

#' Add variables to a tplyr_meta object
#'
#'
#' @param meta tplyr_meta object
#' @param ... Variables to be added
#'
#' @return tplyr_meta object
#' @noRd
add_filters <- function(meta, filters) {
  meta$filters <- append(meta$filters, filters)
  meta
}

#' Return proper quoting for a given value
#'
#' This function returns whatever value should be necessary to
#' create the string for a value that will be parsed. For example,
#' in `x == 'hi'`, the value 'hi' must be quoted like a string. But
#' if the input variable is numeric, such as `x == 1`, the 1 should
#' not be provided in quotes.
#'
#' @param val Value which needs parsing
#'
#' @return A character string
#' @noRd
#'
#' @examples
#'
#' get_parse_string_value('hello')
#' get_parse_string_value(1)
get_parse_string_value <- function(val) {
  if (class(val) %in% c('character', 'factor') && !is.na(val)) {
    paste0('"', val, '"')
  } else{
    val
  }
}

#' Convert supplied values into a string that will parse as a vector
#'
#' By passing in some vector, the text necessary to create that vector is returned.
#'
#' @param values
#'
#' @return
#' @noRd
#'
#'
#' @examples
#'
#' x <- make_vect_str(c(1,2,3))
#' y <- parse(text = x)
#' eval(y)
#'
#' x <- make_vect_str(c('a', 'b', 'c'))
#' y <- parse(text = x)
#' eval(y)
make_vect_str <- function(values) {
  inner <- paste0(map_chr(values, get_parse_string_value), collapse = ", ")

  paste(c('c(', inner, ')'), collapse = "")
}

#' Create a parsed string necessary to create filter logic
#'
#' Given a symbol and values, this function will return an expression required
#' to subset the given variable to that set of values
#'
#' @param variables Variables to filter
#' @param values Values to be filtered
#' @param negate  Negate the filter
#'
#' @noRd
make_parsed_strings <- function(variables, values, negate=FALSE) {

  out <- vector('list', length(variables))

  for (i in seq_along(variables)) {

    vals <- values[[i]]
    vname <- as_label(variables[[i]])

    na_present <- any(is.na(vals))
    na_s <- paste0("is.na(",vname,")")
    vals <- vals[which(!is.na(vals))]

    pre <- ""
    post <- ""
    preneg <- ""

    if (negate) {
      na_s <- paste0("!", na_s)
      eq <- "!="
      comb <- "&"
    } else {
      eq <- "=="
      comb <- "|"
    }

    # Store the NA string as output upfront
    s <- na_s

    if (length(vals) >= 1) {

      if (length(vals) > 1) {
        if (negate) {
          pre <-  "!("
          post <- ")"
        }
        opr <- "%in%"
      } else{
        opr <- eq
      }

      # Build the filter string and negate plurals if necessary
      s <- paste0(pre, vname, " ", opr, " ", make_vect_str(vals), post)

      # Tack on NA's if necessary
      if (na_present) {
        s <- paste0(s, comb, na_s)
      }

    }

    out[[i]] <- str2lang(s)
  }
  out
}

#' Return the vector of treatment groups based on treatment column
#'
#' Given that sets of treatment groups can be combined, this function
#' allows you to get the original treatment groups back out of the specified
#' combination name
#'
#' @param value Specified treatment group
#' @param layer Tplyr layer
#'
#' @return A character vector of treatment groups
#' @noRd
translate_treat_grps <- function(value, treat_grps) {
  out <- as.character(value)
  if (out %in% names(treat_grps)) {
    out <- treat_grps[[out]]
  }
  out
}

#' Translate a filter expression to the symbols in the filter
#'
#' This function will return a list of symbols that are present
#' in a give filter expression
#'
#' @param f Filter expression
#'
#' @return List of symbols
#' @noRd
get_vars_from_filter <- function(f) {
  syms(all.vars(quo_get_expr(f)))
}

#' Extract value of outer layer text value
#'
#' @param layer A Tplyr layer object
#'
#' @return Single element character vector
#' @noRd
get_character_outer <- function(layer) {
  qlist <- layer$target_var_saved

  if (!is.null(qlist) && !quo_is_symbol(qlist[[1]])) {
    return(quo_get_expr(qlist[[1]]))
  } else{
    return(NA_character_)
  }
}

#' Check if a layer is unnested with character target
#'
#' @param layer A Tplyr layer object
#'
#' @return Boolean
#' @noRd
is_unnested_character <- function(layer) {
  unnested <- is.null(layer$target_var_saved)

  if (unnested) {
    return(!quo_is_symbol(layer$target_var[[1]]))
  } else{
    return(FALSE)
  }
}

#' Use available metadata to build the tplyr_meta object
#'
#' This is the main driver function, and layer specific variants
#' adapt on top of this function
#'
#' @param table_where Table level where filter
#' @param layer_where Layer level where filter
#' @param treat_grps Treatment groups from the tplyr_table parent environment
#' @param ... All grouping variables
#'
#' @return tplyr_meta object
#' @noRd
build_meta <- function(table_where, layer_where, treat_grps, variables, values) {

  # Make an assumption that the treatment variable was the first variable provided
  values[[1]] <- translate_treat_grps(values[[1]], treat_grps)

  filters <- make_parsed_strings(variables, values)

  meta <- tplyr_meta(
    names = variables,
    filters = filters
  )

  meta <- meta %>%
    add_filters(layer_where) %>%
    add_variables(get_vars_from_filter(layer_where)) %>%
    add_filters(table_where) %>%
    add_variables(get_vars_from_filter(table_where))

  meta
}

#' Build metadata for desc_layers
#'
#' @param target Target variable currently being summarized
#' @param table_where Table level where filter
#' @param layer_where Layer level where filter
#' @param treat_grps Treatment groups from the tplyr_table parent environment
#' @param ... All grouping variables
#'
#' @return tplyr_meta object
#' @noRd
build_desc_meta <- function(target, table_where, layer_where, treat_grps, ...) {

  variables <- call_args(match.call())

  # Don't want any of the named parameters here
  variables <- variables[which(names(variables)=='')]
  values <- list(...)

  # Get rid of text provided by variables
  inds <- which(map_lgl(unname(variables), ~ quo_class(.) == "name"))
  variables <- variables[inds]
  values <- values[inds]

  # Output vector
  meta <- vector('list', length(values[[1]]))

  # Vectorize across the input data
  for (i in seq_along(values[[1]])) {
    # Pull out the current row's values
    cur_values <- map(values, ~ .x[i])
    # Build the tplyr_meta object
    meta[[i]] <- build_meta(table_where, layer_where, treat_grps, variables, cur_values) %>%
      add_variables(target)
  }

  meta
}

#' Build metadata for count_layers
#'
#' @param target Target variable currently being summarized
#' @param table_where Table level where filter
#' @param layer_where Layer level where filter
#' @param treat_grps Treatment groups from the tplyr_table parent environment
#' @param ... All grouping variables
#'
#' @return tplyr_meta object
#' @noRd
build_count_meta <- function(layer, table_where, layer_where, treat_grps, summary_var, ...) {

  variables <- call_args(match.call())

  # Don't want any of the named parameters here
  variables <- variables[which(names(variables)=='')]
  values <- list(...)

  # Get rid of text provided by variables
  inds <- which(map_lgl(unname(variables), ~ quo_class(.) == "name"))
  #TODO: handle target variables provided as a string
  variables <- variables[inds]
  values <- values[inds]

  # The total row label may not pass through, so set it
  total_row_label <- ifelse(is.null(layer$total_row_label), 'Total', layer$total_row_label)
  count_missings <- ifelse(is.null(layer$count_missings), FALSE, layer$count_missings)
  mlist <- layer$missing_count_list

  # If the outer layer was provided as a text variable, get value
  character_outer <- get_character_outer(layer)
  unnested_character <- is_unnested_character(layer)

  meta <- vector('list', length(values[[1]]))

  # Vectorize across the input data
  for (i in seq_along(values[[1]])) {

    if (!unnested_character) {
      add_vars <- layer$target_var
    } else {
      add_vars <- quos()
    }

    row_filter <- list()

    # Pull out the current row's values
    cur_values <- map(values, ~ .x[i])

    # The outer layer will currently be NA for the outer layer summaries, so adjust the filter appropriately
    if (any(is.na(cur_values))) {

      # Total row or outer layer
      na_var <- variables[which(is.na(cur_values))]

      # work around outer letter being NA
      filter_variables <- variables[which(!is.na(cur_values))]
      filter_values <- cur_values[which(!is.na(cur_values))]

      if (summary_var[i] == total_row_label && !count_missings) {
        # Filter out the missing counts if the total row should exclude missings
        row_filter <- make_parsed_strings(layer$target_var, list(mlist), negate=TRUE)
      }
      else if (summary_var[i] %in% names(mlist)) {
        # Get the values for the missing row
        miss_val <- mlist[which(names(mlist) == summary_var[i])]
        row_filter <- make_parsed_strings(layer$target_var, list(miss_val))
      }
      else if (summary_var[i] != total_row_label) {
        # Subset to outer layer value
        row_filter <- make_parsed_strings(na_var, summary_var[i])
      }

      add_vars <- append(add_vars, na_var)

    }
    else {
      # Inside the nested layer
      filter_variables <- variables
      filter_values <- cur_values

      # Toss out the indentation
      if (!is.null(layer$indentation) && str_starts(summary_var[i], layer$indentation)) {
        summary_var[i] <- str_sub(summary_var[i], layer$indentation_length+1)
      }

      #TODO: Is this redundant and will this always be the same as the inner layer?
      if (summary_var[i] %in% names(mlist)) {
        # Get the values for the missing row
        miss_val <- mlist[which(names(mlist) == summary_var[i])]
        row_filter <- make_parsed_strings(layer$target_var, list(miss_val))
      }
      else if (summary_var[i] == total_row_label && !count_missings) {
        # Filter out the missing counts if the total row should exclude missings
        row_filter <- make_parsed_strings(layer$target_var, list(mlist), negate=TRUE)
      }
      else if (!is.na(character_outer) && summary_var[i] == character_outer) {
        # If the outer layer is a character string then don't provide a filter
        row_filter <- list()
      }
      else if (summary_var[i] != total_row_label && !unnested_character) {
        # If we're not in a total row, build the filter
        row_filter <- make_parsed_strings(layer$target_var, summary_var[i])
      }
    }

    # Make the meta object
    meta[[i]] <- build_meta(table_where, layer_where, treat_grps, filter_variables, filter_values) %>%
      add_filters(row_filter) %>%
      add_variables(add_vars)

  }

  meta
}

#' Build metadata for risk difference comparisons
#'
#' @param meta A tplyr_metadata object
#' @param treat_var the treatment variable
#' @param comp The current rdiff comparison
#'
#' @return tplyr_meta object
#' @noRd
build_rdiff_meta <- function(meta, treat_var, comp){

  for (i in seq_along(meta)) {
    # Make a new filter that contains the current comparison being made
    filt <- make_parsed_strings(list(treat_var), list(comp))[[1]]
    # Add the filter in the spot where the treatment groups are held,
    # which is always the first element (in a count layer)
    meta[[i]]$filters[[1]] <- filt
  }

  meta
}

#' Build metadata for shift_layers
#'
#' @param target Target variable currently being summarized
#' @param table_where Table level where filter
#' @param layer_where Layer level where filter
#' @param treat_grps Treatment groups from the tplyr_table parent environment
#' @param ... All grouping variables
#'
#' @return tplyr_meta object
#' @noRd
build_shift_meta <- function(layer, table_where, layer_where, treat_grps, summary_var, ...) {

  variables <- call_args(match.call())

  # Don't want any of the named parameters here
  variables <- variables[which(names(variables)=='')]
  values <- list(...)

  # Get rid of text provided by variables
  inds <- which(map_lgl(unname(variables), ~ quo_class(.) == "name"))
  variables <- variables[inds]
  values <- values[inds]

  meta <- vector('list', length(values[[1]]))

  # Vectorize across the input data
  for (i in seq_along(values[[1]])) {

    # Pull out the current row's values
    cur_values <- map(values, ~ .x[i])

    # Make the meta object
    meta[[i]] <- build_meta(table_where, layer_where, treat_grps, variables, cur_values) %>%
      add_variables(layer$target_var$row) %>%
      add_filters(make_parsed_strings(layer$target_var['row'], list(summary_var[i])))
  }

  meta
}
