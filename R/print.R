# Dispatches for print and str. These don't need UseMethods as the base
# function will dispatch to these.


#'
#'  @export
print.tplyr_table <- function(x, ...) {
  evalq({
    print_out <- "~~~tplyr_table~~~\n"
    # Print str of target data.frame
    print_out <- append(print_out, "~target (data.frame)~\n")
    print_out <- append(print_out, c("Rows: ", nrow(target), "\n"))
    print_out <- append(print_out, c("Columns: ", ncol(target), "\n"))
    # Print str of pop_data data.frame
    print_out <- append(print_out, "~pop_data (data.frame)~\n")
    print_out <- append(print_out, c("Rows: ", nrow(pop_data), "\n"))
    print_out <- append(print_out, c("Columns: ", ncol(pop_data), "\n"))
    # Print out header
    print_out <- append(print_out, "~header column (chr)~\n")
    print_out <- append(print_out, paste(header, collapse = "\n"))
    print_out <- append(print_out, "\n")
    # Print out treat_var
    print_out <- append(print_out, "~treat_var variable (quosure)~\n")
    print_out <- append(print_out, quo_get_expr(treat_var))
    print_out <- append(print_out, "\n")
    # Print out header_n
    print_out <- append(print_out, "~header_n data (int)~\n")
    print_out <- append(print_out, paste(header_n, collapse = "\n"))
    print_out <- append(print_out, "\n")
    # Print out pop_treat_var
    print_out <- append(print_out, "~pop_treat_var variable (quosure)~\n")
    print_out <- append(print_out, quo_get_expr(pop_treat_var))
    print_out <- append(print_out, "\n")
    # Print out treat_groups
    print_out <- append(print_out, "~treat_grps groupings (list)~\n")
    for(i in seq(treat_grps)) {
      print_out <- append(print_out, names(treat_grps)[i])
      print_out <- append(print_out, paste("\n\t", treat_grps[i], collapse = "\t\n"))
      print_out <- append(print_out, "\n")
    }
    # Print out layers
    print_out <- append(print_out, "~layers: ")
    print_out <- append(print_out, length(layers))
    print_out <- append(print_out, "\n")
    # Print out Layer output
    print_out <- append(print_out, "~layer_output: ")
    if(!exists("layer_output")) print_out <- append(print_out, "<Table Not Built Yet>")
    else print_out <- append(print_out, length(layer_output))

    cat(paste(as.character(print_out), sep = "", collapse = ""))
    rm(print_out)

  }, envir = x)
  invisible(x)
}

#' @export
print.tplyr_layer <- function(x, ...) {
  print(class(x))
  evalq({
    print_out <- "~~tplyr_layer~~\n"
    # Print out target_var
    print_out <- append(print_out, "target_var: ")
    print_out <- append(print_out, quo_get_expr(target_var))
    print_out <- append(print_out, "\n")
    # Print out by
    print_out <- append(print_out, "by: ")
    print_out <- append(print_out, purrr::map(by, quo_get_expr))
    print_out <- append(print_out, "\n")
    # sort_vars
    print_out <- append(print_out, "sort_vars: ")
    print_out <- append(print_out, purrr::map(sort_vars, quo_get_expr))
    print_out <- append(print_out, "\n")
    # Print sort
    print_out <- append(print_out, "sort: ")
    print_out <- append(print_out, sort)
    print_out <- append(print_out, "\n")
    # Print where
    print_out <- append(print_out, "where: ")
    print_out <- append(print_out, quo_get_expr(where))
    print_out <- append(print_out, "\n")
    # Print formatter
    print_out <- append(print_out, "formatter: ")
    print_out <- append(print_out, substitute(formatter))
    print_out <- append(print_out, "\n")
    # Print Layers
    print_out <- append(print_out, "Layer Container(s): ")
    print_out <- append(print_out, length(layers))
    print_out <- append(print_out, "\n")
    cat(paste(as.character(print_out), sep = "", collapse = ""))
    rm(print_out)
  }, envir = x)
  invisible(x)
}

#' @export
print.f_str <- function(x, ...) {
  str(x, ...)
}

#' @export
str.tplyr_table <- function(object, ...) {
  evalq({
    cat("*** target data.frame ***\n")
    str(head(target))
    cat("*** treat_var***\n")
    cat(quo_get_expr(treat_var))
    cat("\n*** pop_data data.frame ***\n")
    str(pop_data)
    cat("*** pop_treat_var ***\n")
    cat(quo_get_expr(pop_treat_var))
    cat("\n*** header binding ***\n")
    cat(header)
    cat("\n*** header_n ***\n")
    cat(header_n)
    cat("\n*** Layer(s) ***\n")
    cat(as.character(purrr::map_dfc(layers, class)[2,]))
  }, envir = object)
  invisible(object)
}

#' @export
str.tplyr_layer <- function(object, ...) {
  evalq({
    cat("*** target_var ***\n")
    cat(as.character(quo_get_expr(target_var)))
    cat("\n*** sort_vars ***\n")
    cat(as.character(purrr::map(sort_vars, quo_get_expr)))
    cat("\n*** by ***\n")
    cat(as.character(purrr::map(by, quo_get_expr)))
    cat("\n*** where ***\n")
    cat(as.character(quo_get_expr(where)))
    cat("\n*** formatter ***\n")
    cat(as.character(deparse(formatter)))
    cat("\n*** Layer(s) ***\n")
    cat(as.character(purrr::map_dfc(layers, class)[2,]))
  }, envir = object)

  invisible(object)
}

#' @export
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
