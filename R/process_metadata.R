#' Process metadata for a layer of type \code{desc}
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_metadata.desc_layer <- function(x, ...) {

  evalq({
    # meta_sums store the metadata table built alongside trans_sums
    meta_sums <- vector("list", length(target_var))
    form_meta <- vector("list", length(target_var))

    for (i in seq_along(target_var)) {
      cur_var <- target_var[[i]]

      # Prepare metadata table
      meta_sum <- num_sums_raw[[i]] %>%
        group_by(!!treat_var, !!!by, !!!cols) %>%
        group_keys() %>%
        # TODO: Vectorize build_meta functions to avoid the rowwise functions because they're slow
        rowwise() %>%
        mutate(
          meta = build_desc_meta(cur_var, table_where, where, treat_grps, !!treat_var, !!!by, !!!cols)
        )

      # Join meta table with the transposed summaries ready for formatting
      meta_sums[[i]] <- trans_sums[[i]] %>%
        select(!!treat_var, !!!by, !!!cols, row_label) %>%
        left_join(meta_sum, by=c(as_label(treat_var), match_exact(by), match_exact(cols)))

      if (stats_as_columns) {
        # Transpose the metadata identical to the summary
        form_meta[[i]] <- meta_sums[[i]] %>%
          pivot_wider(id_cols=c(!!treat_var, match_exact(by)),
                      names_from = match_exact(vars(row_label, !!!cols)),
                      names_prefix = paste0('var', i, "_"),
                      values_from = meta
          )
      } else {
        form_meta[[i]] <- meta_sums[[i]] %>%
          pivot_wider(id_cols=c('row_label', match_exact(by)),
                      names_from = match_exact(vars(!!treat_var, !!!cols)),
                      names_prefix = paste0('var', i, "_"),
                      values_from = meta
          )
      }

    }

    if (stats_as_columns) {
      formatted_meta <- reduce(form_meta, full_join, by=c(as_label(treat_var), match_exact(by)))
      formatted_meta <- replace_by_string_names(formatted_meta, by, treat_var)
    } else {
      formatted_meta <- reduce(form_meta, full_join, by=c('row_label', match_exact(by)))
      formatted_meta <- replace_by_string_names(formatted_meta, by)
    }

  }, envir=x)

  env_get(x, "formatted_meta")

}

#' Process metadata for a layer of type \code{count}
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_metadata.count_layer <- function(x, ...) {

  evalq({
    layer <- current_env()

    # Build up the metadata for the count layer
    meta_sum <- numeric_data %>%
      group_by(!!treat_var, !!!by, !!!cols, summary_var) %>%
      group_keys() %>%
      rowwise() %>%
      mutate(
        meta = build_count_meta(
          layer,
          table_where,
          where,
          treat_grps,
          summary_var,
          !!treat_var,
          !!!cols,
          !!!by
          )
      )

    # Pivot the meta table
    formatted_meta <- meta_sum %>%
      pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                  names_from = c(!!treat_var, match_exact(cols)), values_from = meta,
                  names_prefix = "var1_") %>%
      replace_by_string_names(quos(!!!by, summary_var))

    if (is_built_nest) {
      row_labels_meta <- vars_select(names(formatted_meta), starts_with("row_label"))
      formatted_meta[is.na(formatted_meta[[1]]), 1] <- formatted_meta[is.na(formatted_meta[[1]]),
                                                                      tail(row_labels, 1)]
    }

    if (!is_empty(stats)) {
      formatted_stats_metadata <- map(stats, process_metadata) %>%
        reduce(full_join, by = c('summary_var', match_exact(c(by, head(target_var, -1))))) %>%
        # Replace the by variables and target variable names with `row_label<n>`
        replace_by_string_names(quos(!!!by, summary_var))

      formatted_meta <- full_join(formatted_meta, formatted_stats_metadata,
                                  by = vars_select(names(formatted_meta), starts_with("row_label")))
    }

  }, envir=x)

  env_get(x, "formatted_meta")

}

#' Process metadata for a layer of type \code{count}
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_metadata.tplyr_riskdiff <- function(x, ...) {

  evalq({
    stats_meta <- vector('list', length(comparisons))

    for (i in seq_along(comparisons)) {

      # Weird looking, but this will give me
      stats_meta[[i]] <- meta_sum %>%
        select(-!!treat_var) %>%
        mutate(
          meta = list(build_rdiff_meta(meta, treat_var, comparisons[[i]]))
        )

      # Rename the meta variable
      names(stats_meta[[i]])[ncol(stats_meta[[i]])] <- paste(c("rdiff", comparisons[[i]]), collapse = "_")

    }

    # Join the rdiff columns together
    formatted_stats_meta <- reduce(stats_meta,
                                   full_join,
                                   by=c(match_exact(c(by, cols, head(target_var, -1))),  'summary_var')) %>%
      distinct()

  }, envir=x)

  env_get(x, "formatted_stats_meta")

}
