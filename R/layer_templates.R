#' export
new_layer_template <- function(name, template) {
  template <- enexpr(template)

  if (name %in% objects("package:Tplyr")) {
    stop("Template name cannot be a name already present in Tplyr's namespace")
  }
  # Make sure that the tempalte is valid
  modify_nested_call(template, examine_only = TRUE)

  # Turn the template into a function, with class to mark as a template
  func <-structure(
    eval(str2lang(paste0(c("function(...) {", template, "}"), collapse="\n"))),
    class = c("tplyr_layer_template", "function")
  )

  add_func <- list(func)
  names(add_func) <- name

  # Insert the function into the Tplyr namespace
  options(
    tplyr.layer_templates = append(getOption("tplyr.layer_templates"), add_func)
  )
}

#' @export
use_template <- function(name) {
  template <- getOption("tplyr.layer_templates")[[name]]

  if (!inherits(template, "tplyr_layer_template")) {
    stop("Invalid template - templates must be created using `new_layer_template()`")
  }

  template
}

