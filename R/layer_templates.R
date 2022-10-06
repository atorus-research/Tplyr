#' Create, view, extract, remove, and use Tplyr layer templates
#'
#' There are several scenarios where a layer template may be useful. Some
#' tables, like demographics tables, may have many layers that will all
#' essentially look the same. Categorical variables will have the same count
#' layer settings, and continuous variables will have the same desc layer
#' settings. A template allows a user to build those settings once per layer,
#' then reference the template when the Tplyr table is actually built.
#'
#' This suite of functions allows a user to create and use layer templates.
#' Layer templates allow a user to pre-build and reuse an entire layer
#' configuration, from the layer constructor down to all modifying functions.
#' Furthermore, users can specify parameters they may want to be
#' interchangeable. Additionally, layer templates are extensible, so a template
#' can be use and then further extended with additional layer modifying
#' functions.
#'
#' Layers are created using `new_layer_template()`. To use a layer, use the
#' function `use_template()` in place of `group_count|desc|shift()`. If you want
#' to view a specific template, use `get_layer_template()`. If you want to view
#' all templates, use `get_layer_templates()`. And to remove a layer template use
#' `remove_layer_template()`. Layer templates themselves are stored in the
#' option `tplyr.layer_templates`, but a user should not access this directly
#' and instead use the Tplyr supplied functions.
#'
#' When providing the template layer syntax, the layer must start with a layer
#' constructor. These are one of the function `group_count()`, `group_desc()`,
#' or `group_shift()`. Instead of passing arguments into these function,
#' templates are specified using an ellipsis in the constructor, i.e.
#' `group_count(...)`. This is required, as after the template is built a user
#' supplies these arguments via `use_template()`
#'
#' `use_template()` takes the `group_count|desc|shift()` arguments by default.
#' If a user specified additional arguments in the template, these are provided
#' in a list throught the argument `add_params`. Provide these arguments exactly
#' as you would in a normal layer. When creating the template, these parameters
#' can be specified by using curly brackets. See the examples for details.
#'
#' @param name Template name
#' @param template Template layer syntax, starting with a layer constructor
#'   `group_count|desc|shift`. This function should be called with an ellipsis
#'   argument (i.e. group_count(...)).
#'
#' @md
#' @export
#'
#' @family Layer Templates
#' @rdname layer_templates
#'
#' @examples
#'
#' op <- options()
#'
#' new_layer_template(
#'   "example_template",
#'   group_count(...) %>%
#'     set_format_strings(f_str('xx (xx%)', n, pct))
#' )
#'
#' get_layer_templates()
#'
#' get_layer_template("example_template")
#'
#' tplyr_table(mtcars, vs) %>%
#'   add_layer(
#'     use_template("example_template", gear)
#'   ) %>%
#'   build()
#'
#' remove_layer_template("example_template")
#'
#' new_layer_template(
#'   "example_template",
#'   group_count(...) %>%
#'     set_format_strings(f_str('xx (xx%)', n, pct)) %>%
#'     set_order_count_method({sort_meth}) %>%
#'     set_ordering_cols({sort_cols})
#' )
#'
#' get_layer_template("example_template")
#'
#' tplyr_table(mtcars, vs) %>%
#'   add_layer(
#'     use_template("example_template", gear, add_params =
#'                    list(
#'                      sort_meth = "bycount",
#'                      sort_cols = `1`
#'                    ))
#'   ) %>%
#'   build()
#'
#' remove_layer_template("example_template")
#'
#' options(op)
new_layer_template <- function(name, template) {
  template <- enexpr(template)

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

  # Make sure that the template is valid
  modify_nested_call(template, examine_only = TRUE)

  if (name %in% names(getOption("tplyr.layer_templates"))) {
    warning(
      sprintf("A template by the name %s already exists. Template will be overwritten.", name),
      call. = FALSE
    )
    remove_layer_template(name)
  }

  # Find any add_params provided into the template.
  # The indexing here is pulling the first element out of the list, because I
  # only pass a single element character vector. Next, I use [,2] because I know
  # I need column two because I'm specifically requesting the group within the
  # parens of the regex
  params <- str_match_all(raw_template, "\\{\\s+([\\w\\.]+)\\s+\\}")[[1]][,2]

  # Turn the template into a function, with class to mark as a template
  func <-structure(
    paste0(c("{",raw_template,"}"), collapse="\n"),
    params = params,
    template_name = name,
    class = c("tplyr_layer_template")
  )

  add_func <- list(func)
  names(add_func) <- name

  # Insert the function into the Tplyr namespace
  options(
    tplyr.layer_templates = append(getOption("tplyr.layer_templates"), add_func)
  )
}

#' @family Layer Templates
#' @rdname layer_templates
#' @export
remove_layer_template <- function(name) {
  tmps <- getOption('tplyr.layer_templates')

  if (name %in% names(tmps)) {
    options(tplyr.layer_templates = tmps[names(tmps) != name])
  } else{
    warning(sprintf("No template named %s", name))
  }
}

#' @family Layer Templates
#' @rdname layer_templates
#' @export
get_layer_template <- function(name) {
  tmps <- getOption('tplyr.layer_templates')
  if (!(name %in% names(tmps))) {
    stop(sprintf("Template %s does not exist", name), call.=FALSE)
  }
  tmps[[name]]
}

#' @family Layer Templates
#' @rdname layer_templates
#' @export
get_layer_templates <- function() {
  getOption('tplyr.layer_templates')
}

#' @param ... Arguments passed directly into a layer constructor, matching the
#'   target, by, and where parameters.
#' @param add_params Additional parameters passed into layer modifier functions.
#'   These arguments are specified in a template within curly brackets such as
#'   {param}. Supply as a named list, where the element name is the parameter.
#'
#' @family Layer Templates
#' @rdname layer_templates
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
  }  else if (!is.null(quo_get_expr(add_params))) {
    stop("Arguments must be passed to `add_params` in a list.", call.=FALSE)
  } else {
    add_params_args <- list()
  }

  template <- get_layer_template(name)

  if (!inherits(template, "tplyr_layer_template")) {
    stop("Invalid template - templates must be created using `new_layer_template()`", call.=FALSE)
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

#' @export
print.tplyr_layer_template <- function(x, ...) {
  cat(sprintf("Template name: %s\n", attr(x, 'template_name')))
  params <- attr(x, 'params')
  if (is_empty(params)) {
    params <- "None"
  }
  cat(sprintf("Template parameters: %s\n", paste(c(params), collapse=", ")))
  cat("Template code:\n")
  cat(x, "\n")
}
