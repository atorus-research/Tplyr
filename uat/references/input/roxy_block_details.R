#' @title Retrieve the value in a function specifiation block of a custom section
#' @param tag name of the section, case insensitive.
#' @param block character vector that holds the documentation block
#' @return section value
get_section_contents <- function(tag, block){
  tag_no <- which(grepl(pattern = tolower(tag), tolower(block)))
  gsub(pattern = "[[:space:]]^|[[:space:]]$", replacement = "",
       x = block[tag_no + 1])
}

#' @title Generate at data.frame from the function specification roxygen documentation
#' block.
#' @description May be used for specification or test case .Rmd files located
#' in \code{function_specification} folder.
#' @param one_file full filepath of the .Rmd file to scrape
#' @return data.frame with variables: \code{title}, \code{last_update_by}, and
#' \code{last_updated_date}.
#' @examples
#' \dontrun{
#'   library(here)
#'   library(magrittr)
#'   list.files(here("vignettes", "function_specifications", "test_cases"),
#'            full.names = TRUE)[1:4] %>%
#'   lapply(., scrape_function_specification_block) %>%
#'   do.call(rbind, .)
#'
#'   list.files(here("vignettes", "function_specifications", "specifications"),
#'             full.names = TRUE)[1:4] %>%
#'   lapply(., scrape_function_specifications) %>%
#'   do.call(rbind, .)
#' }
scrape_function_specification_block <- function(one_file){
  lines <- readLines(one_file)
  rox_block <- gsub(pattern = "#' ", replacement = "",
                    x = lines[grep(pattern = "#' ", x = lines)])
  data.frame(title = basename(one_file),
             last_update_by = get_section_contents("Last updated by", rox_block),
             last_updated_date = lubridate::parse_date_time(get_section_contents("last update date", rox_block),
                                                            orders = c("ymd", "mdy")),
             stringsAsFactors = FALSE)
}


#' @title Generate at data.frame from the function roxygen documentation
#' block.
#' @description May be used for specification or test case .Rmd files located
#' in \code{function_specification} folder.
#' @param one_file full filepath of the .R file to scrape
#' @return data.frame with variables: \code{title}, \code{author}, and
#' \code{last_updated_date}.
scrape_function_author_block <- function(one_file){
  refs <- parse(one_file, keep.source =  TRUE,
                srcfile = srcfilecopy(one_file, lines, isFile = TRUE)) %>%
    utils::getSrcref()
  srcfile <- attr(refs[[1]], "srcfile")
  do.call(rbind, lapply(seq_along(refs), function(i) {
    first_byte <- refs[[i]][4]
    first_line <- ifelse(i == 1, 1, refs[[i - 1]][1])
    last_line <- refs[[i]][1] - 1
    last_byte <- refs[[i]][3]
    lloc <- c(first_line, first_byte, last_line, last_byte)
    out <- as.character(srcref(srcfile, lloc))
    out_cleaned <- out[grepl(out, pattern = "#' ")]
    if (length(out_cleaned) == 0 | length(grep(out_cleaned, pattern = "#' @section")) == 0) {
      return(data.frame())
    } else {
      roxy_block <- gsub(pattern = "#' ", replacement = "",
                         out_cleaned[grep(pattern = "#' ", out_cleaned)])
      return(data.frame(title = as.character(parse(text = as.character(refs[[i]]))[[1]])[2],
                        author = get_section_contents("updated by", roxy_block),
                        last_updated_date = lubridate::parse_date_time(get_section_contents(" date", tolower(roxy_block)),
                                                                       orders = c("ymd", "mdy")),
                        stringsAsFactors = FALSE))
    }
  }))
}
