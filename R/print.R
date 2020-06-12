# Dispatches for print and str. These don't need UseMethods as the base
# function will dispatch to these.

#' Print tplyr_table
#'
#' @param x A tplyr_table to display
#' @param ... A paranthesis passed through from print dispatch
#'
#'  @export
#'  @noRd
print.tplyr_table <- function(x, ...) {
  evalq({
    cat("*** tplyr_table ***\n")
    # Print str of target data.frame
    cat("Target (data.frame):\n")
    cat("\tName: ", attr(target, "target_name"))
    cat(c("\n\tRows: ", nrow(target)))
    cat(c("\n\tColumns: ", ncol(target), "\n"))
    # Print str of pop_data data.frame
    if(!identical(pop_data, target)){
      cat("pop_data (data.frame)\n")
      cat("\tName: ", attr(pop_data, "pop_data_name"), "\n")
      cat(c("\tRows: ", nrow(pop_data), "\n"))
      cat(c("\tColumns: ", ncol(pop_data), "\n"))
    }
    # Print out treat_var
    cat("treat_var variable (quosure)\n\t")
    cat(as.character(quo_get_expr(treat_var)))
    cat("\n")
    # Print out pop_treat_var
    if(!identical(pop_treat_var, treat_var)){
      cat("pop_treat_var variable (quosure)\n\t")
      cat(as.character(quo_get_expr(pop_treat_var)))
      cat("\n")
    }
    # Print headers
    cat("header: ")
    cat(header)
    # Print header_n
    cat("\nheader_n: ")
    for(i in seq(header_n)) {
      cat(names(header_n)[i])
      cat(": ")
      cat(header_n[i])
      if (i < length(header_n)) cat(", ")
    }
    # Print out treat_groups
    cat("\ntreat_grps groupings (list)\n")
    for(i in seq(treat_grps)) {
      cat("\t")
      cat(names(treat_grps)[i])
      cat("\n")
    }
    # Print out layers
    cat("Number of layer(s): ")
    cat(length(layers))
    cat("\n")
    # Print out Layer output
    cat("layer_output: ")
    if(!exists("layer_output")) cat("<Table Not Built Yet>")
    else cat(length(layer_output))
  }, envir = x)
  invisible(x)
}

#' Print a tplyr_layer package
#'
#' @param x A tplyr_layer object to print
#' @param ... Arguments passed through from print dispatch.
#'
#' @export
#' @noRd
print.tplyr_layer <- function(x, ...) {
  cat("*** tplyr_layer ***\n")
  cat("Self: ", class(x)[2], "<", env_label(x), ">")
  cat("\nParent: ", class(env_parent(x))[1], "<", env_label(env_parent(x)), ">")
  evalq({
    # Print out target_var
    cat("\ntarget_var: ")
    for(i in seq(target_var)) {
      cat("\n", quo_get_expr(target_var[[i]]))
    }
    cat("\n")
    # Print out by
    cat("by: ")
    cat(as.character(purrr::map(by, quo_get_expr)))
    cat("\n")
    # sort_vars
    cat("sort_vars: ")
    cat(as.character(purrr::map(sort_vars, quo_get_expr)))
    cat("\n")
    # Print sort
    cat("sort: ")
    cat(sort)
    cat("\n")
    # Print where
    cat("where: ")
    cat(as.character(quo_get_expr(where)))
    cat("\n")
    # Print Layers
    cat("Layer(s): ")
    cat(length(layers))
    cat("\n")
  }, envir = x)
  invisible(x)
}

#' Print a f_str object
#'
#' @param x A f_str object to print
#' @param ... Arguments passed through print dispatch
#'
#' @export
#' @noRd
print.f_str <- function(x, ...) {
  str(x, ...)
}

#' A tplyr_table object to str
#'
#' @param object A tplyr_table object
#' @param ... Arguments passed from str
#'
#' @export
#' @noRd
str.tplyr_table <- function(object, ...) {
  evalq({
    cat("*** target data.frame ***\n")
    cat("Target Name: ", attr(target, "target_name"), "\n")
    str(head(target))
    cat("*** treat_var***\n")
    cat(quo_get_expr(treat_var))
    cat("\n*** pop_data data.frame ***\n")
    str(pop_data)
    cat("*** pop_treat_var ***\n")
    cat(quo_get_expr(pop_treat_var))
    cat("\n*** treat_grps ***")
    for(i in seq(treat_grps)) {
      cat("\n", names(treat_grps)[i])
      cat(":\n\t", treat_grps[[i]])
    }
    cat("\n*** header binding ***\n")
    for (i in seq(header)) {
      cat("\t", header[i], "\n")
    }
    cat("*** header_n ***\n")
    for (i in seq(header_n)) {
      cat("\t", names(header_n)[i], ": ")
      cat(header_n[i], "\n")
    }
    cat("*** Layer(s) ***\n")
    cat(as.character(purrr::map_dfc(layers, class)[2,]))
  }, envir = object)
  invisible(object)
}

#' Display a tplyr_layer object
#'
#' @param object A tplyr_layer object
#' @param ... Arguemnts passed from str
#'
#' @export
#' @noRd
str.tplyr_layer <- function(object, ...) {
  cat("*** tplyr_layer ***")
  cat("\nSelf: ")
  cat("\n\tAddress: ", env_label(object))
  cat("\n\tType: ", class(object)[2])
  cat("\n\tDepth from table: ", depth_from_table(object, 0))
  cat("\nParent: ")
  cat("\n\tAddress: ", env_label(env_parent(object)))
  cat("\n\tType: ", class(env_parent(object))[1])
  # Only Print target name if parent is table
  if (class(env_parent(object))[1] == "tplyr_table"){
    cat("\n\tTarget Name: ", attr(env_parent(object)$target, "target_name"))
  }
  evalq({
    cat("\n*** target_var ***")
    for(i in seq(target_var)) {
      cat("\n", as.character(quo_get_expr(target_var[[i]])))
    }
    cat("\n*** sort_vars ***\n")
    cat(as.character(purrr::map(sort_vars, quo_get_expr)))
    cat("\n*** by ***\n")
    cat(as.character(purrr::map(by, quo_get_expr)))
    cat("\n*** where ***\n")
    cat(as.character(quo_get_expr(where)))
    cat("\n*** Layer(s) ***\n")
    cat(as.character(purrr::map_dfc(layers, class)[2,]))
  }, envir = object)

  invisible(object)
}

#' Print a f_str object
#'
#' @param object A f_str object to display
#' @param ... Arguments passed from f_str
#'
#' @export
#' @noRd
str.f_str <- function(object, ...) {
  cat("*** Format String ***\n")
  cat(object$format_string)
  cat("\n*** vars, extracted formats, and settings ***\n")
  for(i in seq(along = object$vars)) {
    cat(object$vars[[i]])
    cat(" formated as: ")
    cat(object$formats[i])
    cat("\n\tinteger length: ")
    cat(object$settings[[i]][1])
    cat("\n\tdecimal length: ")
    cat(object$settings[[i]][2])
    cat("\n")
  }
  cat("Total Format Size: ")
  cat(object$size)
}
