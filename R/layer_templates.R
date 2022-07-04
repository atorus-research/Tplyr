#' export
new_layer_template <- function(name, template) {
  template <- enexpr(template)

  if (name %in% objects("package:Tplyr")) {
    stop(
      "Template name cannot be a name already present in Tplyr's namespace",
      call.=FALSE
      )
  }

  if (name %in% names(getOption("tplyr.layer_templates"))) {
    warning(
      sprintf("A template by the name %s already exists. Template will be overwritten.", name),
      call. = FALSE
    )
    tmps <- getOption('tplyr.layer_templates')
    options(tplyr.layer_templates = tmps[names(tmps) != name])
  }

  # Make sure that the template is valid
  modify_nested_call(template, examine_only = TRUE)

  # Turn the template into a function, with class to mark as a template
  func <-structure(
    paste0(c("{",template,"}"), collapse="\n"),
    class = c("tplyr_layer_template")
  )

  add_func <- list(func)
  names(add_func) <- name

  # Insert the function into the Tplyr namespace
  options(
    tplyr.layer_templates = append(getOption("tplyr.layer_templates"), add_func)
  )
}

#' @export
use_template <- function(name, ..., add_params = NULL) {

  # From the add_params call, pull out the call arguments This allows us to
  # capture any names or call as name or calls without quoting non-quoted
  # arguments within the list
  add_params_args <- call_args(enquo(add_params))

  template <- getOption("tplyr.layer_templates")[[name]]

  if (!inherits(template, "tplyr_layer_template")) {
    stop("Invalid template - templates must be created using `new_layer_template()`")
  }

  # Based on the types provided in template_param, only run those replaces
  template <- make_template(template, add_params_args)

  template_func <- eval(str2lang(paste0(c("function(..., add_params) {", template, "}"), collapse="\n")))

  template_func(..., add_params = add_params_args)
}

make_template <- function(template, add_params_args) {
  # Pick out the classes of each argument
  arg_types <- map_chr(add_params_args, class)
  # Find the arguments that actually need to be unquoted
  quo_arg_names <- add_params_args[which(arg_types %in% c("name", "call"))]
  # Get those arguments names
  search_names <- names(quo_arg_names)
  # This regex finds the text template_param("<param> based on the names
  # supplied above, and creates a group around template_param so we can target
  # that replacement
  rx <- regex(sprintf("(template_param)\\([[:punct:]](%s)[[:punct:]]\\)",
                      paste(search_names, collapse="|")))
  # Finally, replace only template_param calls to the name or call parameters as
  # unquoted arguments. This is using references to group matches from the regex
  # above inside the replacement
  str_replace(template, rx, "!!template_param(\'\\2\')")
}

#' @export
template_param <- function(param_name) {
  add_params <- env_parent(caller_env())[['add_params']]
  param <- add_params[[param_name]]
  param
}
