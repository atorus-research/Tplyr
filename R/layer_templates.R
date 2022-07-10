#' export
new_layer_template <- function(name, template) {
  template <- enexpr(template)

  # Make sure that the template is valid
  modify_nested_call(template, examine_only = TRUE)

  # Have to convert the call to a character and collapse it to order text correctly
  raw_template <- paste0(c(template), collapse="\n")

  # Enforce ellipsis on layer constructors
  if (!str_detect(raw_template, "group_(count|desc|shift)\\(\\.{3}\\)")) {
    msg <- paste0(
      "Invalid template - templates must start with an ellipsis (i.e. ...) passed to either ",
      "group_count, group_desc, or group_shift. For example, group_count(...)"
    )
    stop(msg, call.=FALSE)
  }

  if (name %in% names(getOption("tplyr.layer_templates"))) {
    warning(
      sprintf("A template by the name %s already exists. Template will be overwritten.", name),
      call. = FALSE
    )
    tmps <- getOption('tplyr.layer_templates')
    options(tplyr.layer_templates = tmps[names(tmps) != name])
  }

  # Find any add_params provided into the template
  params <- str_match_all(raw_template, "\\{\\s+([\\w\\.]+)\\s+\\}")[[1]][,2]

  # Turn the template into a function, with class to mark as a template
  func <-structure(
    paste0(c("{",raw_template,"}"), collapse="\n"),
    params = params,
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
  add_params <- enquo(add_params)

  if (is_call(quo_get_expr(add_params))) {
    if (call_name(add_params) != "list") {
      stop("Arguments must be passed to `add_params` in a list.", call.=FALSE)
    }
    add_params_args <- call_args(add_params)

    if (!is_named(add_params_args)) {
      stop("Arguments pass in `add_params` must be named", call.=FALSE)
    }
  }  else {
    add_params_args <- list()
  }

  template <- getOption("tplyr.layer_templates")[[name]]

  if (!inherits(template, "tplyr_layer_template")) {
    stop("Invalid template - templates must be created using `new_layer_template()`")
  }

  # Param checks
  template_params <- attr(template, 'params')

  # Args provided not in template
  invalid_args <- setdiff(names(add_params_args), template_params)
  # Template params not in args
  missing_args <- setdiff(template_params, names(add_params_args))

  if (!is_empty(invalid_args)) {
    bad_args <- paste0(invalid_args, collapse=", ")
    stop(
      sprintf("In use_template() the following parameters provided to add_params are invalid: %s", bad_args),
      call.=FALSE
      )
  }

  if (!is_empty(missing_args)) {
    bad_args <- paste0(missing_args, collapse=", ")
    stop(
      sprintf("In use_template() the following parameters provided to add_params are missing: %s", bad_args),
      call.=FALSE
    )
  }

  # Based on the types provided in template_param, only run those replaces
  template <- make_template(template, add_params_args)

  template_func <- eval(str2lang(paste0(c("function(..., add_params) {", template, "}"), collapse="\n")))

  template_func(..., add_params = add_params_args)
}

#' Process the template function text ready for evaluation
#'
#' In order for the template function to evaluate properly, names or calls must
#' be unquoted when they're passed into the layer modifying functions.
#' Otherwise, the template parameter function itself will be passed in and
#' quoted, failing type checks of the layer modifiers. This internal function
#' processes the template to add the template_params functions and unquote as
#' necessary.
#'
#' @param template Template function as a character string
#' @param add_params_args Argument list captured in use_template
#'
#' @return Template function text with template_param arguments inserted
#'
#' @noRd
make_template <- function(template, add_params_args) {
  # Pick out the classes of each argument
  arg_types <- map_chr(add_params_args, class)

  # Find the arguments that actually need to be unquoted and the ones that
  # don't separately
  quo_arg_names <- add_params_args[which(arg_types %in% c("name", "call"))]
  oth_arg_names <- add_params_args[which(!(arg_types %in% c("name", "call")))]

  # Get those arguments names
  quo_search_names <- names(quo_arg_names)
  oth_search_names <- names(oth_arg_names)

  # This regex finds the text template_param("<param> based on the names
  # supplied above, and creates a group around template_param so we can target
  # that replacement
  rx_str <- "\\{\\s+(%s)\\s+\\}"
  quo_rx <- regex(sprintf(rx_str,
                      paste(quo_search_names, collapse="|")))
  oth_rx <- regex(sprintf(rx_str,
                          paste(oth_search_names, collapse="|")))

  # Finally, replace only replace the curly bracket arguments with a call to
  # template_param, unquoting for calls and names and not unquoting for
  # other arguments.
  out_template <- str_replace(template, quo_rx, "!!template_param(\'\\1\')")
  str_replace(out_template, oth_rx, "template_param(\'\\1\')")
}

#' Extract a parameter in a template context
#'
#' Beyond the group_<type> functions, templates need a method to hand parameters
#' from use_template down to other tplyr_layer modifier functions. Users supply
#' arguments to the template and no to the modifier functions themselves, so
#' those arguments need to be passed down. Furthermore, we don't want them to
#' have to think about quasiquotation. So the purpose of `template_param()` is
#' to abstract the handing of parameters away from the user and allow them to
#' just use the add_params parameter instead.
#'
#' @param param_name Parameter name specified in add_params of `use_template()`
#'
#' @return Extracted parameter
#'
#' @noRd
template_param <- function(param_name) {
  # The caller_env() is where template_param() was called
  # The parent of that environment is template_func(), which was created
  # in use_template(). So we look back there for the add_params argument
  # passed down from use_template
  add_params <- env_parent(caller_env())[['add_params']]
  # Pull the desired parameter out and return it
  param <- add_params[[param_name]]
  param
}
