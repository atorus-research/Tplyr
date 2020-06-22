### Build/Render Functions

#' Trigger the execution of the \code{tplyr_table}
#'
#' @description
#' The functions used to assemble a \code{tplyr_table} object and each of the layers do not trigger the processing of any data. Rather, a lazy
#' execution style is used to allow you to contruct your table, preview what the output should look like, and then explicitly state when the
#' data processing should happen. \code{build} triggers this event.
#'
#' @details
#' When the \code{build} command is executed, all of the data processing commences. Any preprocessing necessary within the table environment
#' takes place first. Next, each of the layers begings executing. This process is recursive, as when a layer is starts executing, \code{tplyr}
#' looks to see if there are any sublayers within that layer. Once the layers complete executing, within a layer, the provided instruction
#' will join them together, or within the table the outputs will be stacked together.
#'
#' Once this process is complete, any post-processing necessary within the table environment takes place, and the final output can be
#' delivered. Metadata and traceability information are kept within each of the layer environments, which allows an investigation into the
#' source of the resulting datapoints.
#'
#' @param x A \code{tplyr_table} object
#'
#'
#' @return An executed \code{tplyr_table}
#' @export
#'
#' @examples
#' # TBD
#'
#' @seealso tplyr_table, tplyr_layer, add_layer, layer_constructors
build <- function(x) {
  UseMethod("build")
}

#' tplyr_table S3 method
#' @noRd
#' @export
build.tplyr_table <- function(x) {

  output <- evalq({
    # Dummies for treatment groups added to target dataset
    built_target <- target
    built_pop_data <- pop_data
    for (i in seq(along = treat_grps)) {
      # The following is a little nasty but the idea is to:
      # Add a T/F column named '.tplyr-treat_grp_name'
      target[, paste0(".tplyr-", names(treat_grps)[i])] <- unlist(target[, as_label(treat_var)]) %in% treat_grps[[i]]
      # Make a new data.frame with only the grouped rows
      grped_df <- target[unlist(target[, paste0(".tplyr-", names(treat_grps)[i])]),]
      # Change the treatment group column to the name of the group.
      grped_df[, as_label(treat_var)] <- names(treat_grps)[i]
      # Rbind, suppressing warnings due to factor issues
      built_target <- suppressWarnings(bind_rows(built_target, grped_df))
    }
    # Dummies for treatment groups added to population dataset
    for (i in seq(along = treat_grps)) {
      # The following is a little nasty but the idea is to:
      # Add a T/F column named '.tplyr-treat_grp_name'
      pop_data[, paste0(".tplyr-", names(treat_grps)[i])] <- unlist(pop_data[, as_label(treat_var)]) %in% treat_grps[[i]]
      # Make a new data.frame with only the grouped rows
      grped_df <- pop_data[unlist(pop_data[, paste0(".tplyr-", names(treat_grps)[i])]),]
      # Change the treatment group column to the name of the group.
      grped_df[, as_label(treat_var)] <- names(treat_grps)[i]
      # Rbind
      built_pop_data <- suppressWarnings(bind_rows(built_pop_data, grped_df))
    }
    rm(i)


  }, envir=x)

  output <- map(x$layers, build)

  # Feed the output up
  bind_rows(output)
}

#' count_layer S3 method
#' @noRd
build.count_layer <- function(x) {

  # Prepare the layer environment as appropriate
  layer_output <- NULL
  layers <- NULL

  verify_layer_compatibility(x)

  output <- evalq({

    # Add the count_fmt if it wasn't set
    if(!exists("count_fmt")) count_fmt <- f_str("{layer_width(x)} (xxx.x%)", n, pct)

    # Build the layers
    lapply(layers, build)

  }, envir=x)
  process_count_layer(x)
  # Feed the output up
  output
}

#' desc_layer S3 method
#' @noRd
#' @export
build.desc_layer <- function(x) {

  # Prepare the layer environment as appropriate
  layer_output <- NULL
  layers <- NULL

  output <- evalq({
    # Build the layers
    lapply(layers, build)

  }, envir=x)
  # Build the sub-layers
  sublayer_output <- map(x$layers, build)

  # Feed the output up
  layer_output <- process_desc_layer(x)

  # TODO: Some combination process
  output <- layer_output
  output
}

#' shift_layer S3 method
#' @noRd
build.shift_layer <- function(x) {

  # Prepare the layer environment as appropriate
  layer_output <- NULL
  layers <- NULL

  output <- evalq({
    # Build the layers
    lapply(layers, build)

  }, envir=x)

  # Feed the output up
  output
}

verify_layer_compatibility <- function(layer) {
  NextMethod("verify_layer_compatibility")
}

verify_layer_compatibility.count_layer <- function(layer){

  evalq({

  }, envir = layer)
  return(invisible(layer))
}



