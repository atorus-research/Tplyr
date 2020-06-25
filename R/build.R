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

  treatment_group_build(x)

  output <- map(x$layers, build) %>%
    bind_rows()

  # Rearange columns. Currently just alphabetical
  output[, sort(names(output))]
}

#' count_layer S3 method
#' @noRd
build.count_layer <- function(x) {

  target_var_length <- length(env_get(x, "target_var"))

    if(target_var_length == 2) {



      # Begin with the layer itself and process the first target vars values one by one
      layer_output <- map_dfr(unlist(get_target_levels(x, env_get(x, "target_var")[[1]])),
              # target_var_1_i represents the current outer target variable
              function(target_var_1_i) {
                # This contains the subset of the first target variable. TODO: The by variables are currently
                # set to the 'by' variable. Where and cols should pull through from x
                inner_layer <- build(group_count(env_parent(x), target_var = !!get_target_var(x)[[2]],
                                  by = !!target_var_1_i, cols = vars(!!!env_get(x, "cols")),
                                  where = !!get_where(x) & !!get_target_var(x)[[1]] == !!target_var_1_i) %>%
                        set_include_total_row(FALSE))
                # This should be a single row with the total of target_var 1
                outer_layer <- build(group_count(env_parent(x), target_var = !!get_target_var(x)[[1]],
                                  by = vars(!!!get_by(x)), cols = vars(!!!env_get(x, "cols")),
                                  where = !!get_where(x) & !!get_target_var(x)[[1]] == !!target_var_1_i) %>%
                        set_include_total_row(FALSE))
                # Bind these two to gether and add a row mask
                bind_rows(outer_layer, inner_layer) %>%
                  apply_row_masks()
              })

      # Build the sub-layers
      sublayer_output <- map(x$layers, build)

      # Feed the output up
      #layer_output <- process_count_layer(x)

      # TODO: Some combination process
      output <- layer_output
      output

      # If there are not two, and not one, fail. TODO: move this into compatibility
    } else if (target_var_length != 1) {
      abort("target_var can only contain one or two target_variables.
            Other amounts are not currently implemented.")

      # If there is just one no need for logic
    } else {

      # Build the sub-layers
      sublayer_output <- map(x$layers, build)

      # Feed the output up
      layer_output <- process_count_layer(x)

      # TODO: Some combination process
      output <- layer_output
      output
    }
}

#' desc_layer S3 method
#' @noRd
#' @export
build.desc_layer <- function(x) {

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

  # Build the sub-layers
  sublayer_output <- map(x$layers, build)

  # Feed the output up
  layer_output <- process_shift_layer(x)

  # TODO: Some combination process
  output <- layer_output
  output
}





