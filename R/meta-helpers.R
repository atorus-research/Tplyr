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
#' @return Character string
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
