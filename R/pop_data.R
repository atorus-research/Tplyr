### Populations Functions

default_header_n <- function(table) {
  tplyr_header_n(table) <- evalq({
    df <- pop_data %>%
      group_by(!!pop_treat_var) %>%
      summarise(N = n())

    vec <- unlist(df[, 2])
    names(vec) <- unlist(df[, 1])
    vec
  }, envir = table)
  table
}
