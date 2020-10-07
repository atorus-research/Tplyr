#' @title Evaluate the test code
#' @param one_file full path to test code file.
#' @return a table with variables: \code{Test}, \code{Expected}, \code{Results},
#' \code{Pass/Fail}. Suitable for including in validation report
eval_test_code <- function(one_file) {
  # each test code file (in spec_testing) is run in a new, clean environment
  test_env <- new.env()
  print(getwd())

  # Get the testthat reults (via reporter)
  # Used for obtaining the Pass/Fail variable
  # output <- testthat::test_file(one_file , reporter = Reporter)
  # results <- lapply(output, `[[`, "results")

  # Get the code as expressions.
  # Used for obtaining the expected and observed results later.
  lines <- readLines(one_file)
  parsed <- parse(text = lines)

  # MUNGING the parsed code
  # sort into "context" "setup" and "test sub cases"
  #   "context" is ignored
  #   run code from "setup" as-is in test_env 1st
  #   run code from "code" arg in test_that call ("test sub cases") to capture
  #     output in the test_env [run in loop below to track output of expect_SCHARP*]
  context <- grep(parsed, pattern = "context")

  reporter_messages <- capture_output({
    reporter <- testthat::test_file(one_file, env = test_env)
  })


  # For each instance of test_that in test code file, get the test case name,
  #    expected results, observed results and Pass/Fail designation
  messages<- capture.output({out <-
    do.call(rbind, lapply(
      seq_along(reporter),
      FUN = function(i) {

        test_case <- reporter[[i]]

        # managing the environments:
        # create a new child env for each instance of test_that
        # child used to eval values for test code summary without changing setup
        # variables


        test_description <- test_case$results[[1]]$test

        # get the observed (actual), expected and results value from the
        # expect_SCHARP* output
        all_results <-
          do.call(rbind, lapply(test_case$results, function(x) {
            pass_fail <- ifelse(
              inherits(x, "expectation_success"),
              "Pass",
              "Fail"
            )
            data.frame(
              Test = test_description,
              Results = capture_output(testthat:::print.expectation(x)),
              `Pass/Fail` = pass_fail,
              stringsAsFactors = FALSE
            )
          }))

        if (is.null(all_results)) {
          all_results <- data.frame()
        } else {
          all_results$Test <- paste0(all_results$Test, ".", 1:nrow(all_results))
        }

        return(all_results)
      }
    ))})

  # if(file.exists("~/pharmaRTF/vignettes/Validation/vur_auto.Rds")) {
  #   vur <- readRDS("~/pharmaRTF/vignettes/Validation/vur_auto.Rds")
  #   out$Log <- NA
  #   print(out)
  #   print(vur)
  #   for(i in seq(nrow(out))){
  #     test_i <- out[i, "Test"]
  #     if(any(vur$ID %in% test_i)) {
  #       out[which(vur$ID %in% test_i), "Log"] <- vur[vur$ID %in% test_i, "Log"]
  #     }
  #   }
  # }

  # formatting
  out


}

print_eval_test_results <- function(out) {

  rownames(out) <- NULL
  kable(
    out,
    escape = TRUE,
    col.names = c("Check", "Results", "Pass/Fail"),
    format = "latex",
    longtable = TRUE) %>%
    kable_styling(latex_options = c("repeat_header"))

}

#' @title Generate at data.frame from the test code roxygen documentation blocks.
#' @description May be used for .R or .r files located
#' in \code{tests/test_specs} folder.
#' @param one_file full filepath of the .R or .r file to scrape
#' @return data.frame with variables: \code{title}, \code{last_update_by}, and
#' \code{last_updated_date}.
#' @examples
#' \dontrun{
#'   list.files(here("tests", "spec_tests"), pattern = ".R", ignore.case = TRUE,
#'            full.names = TRUE)[1:4] %>%
#'     lapply(., scrape_test_code_block) %>%
#'     do.call(rbind, .)
#' }
scrape_test_code_block <- function(one_file){
  lines <- readLines(one_file)
  refs <- parse(text = lines, keep.source =  TRUE,
                srcfile = srcfilecopy(one_file, lines, isFile = TRUE)) %>%
    utils::getSrcref()
  srcfile <- attr(refs[[1]], "srcfile")

  do.call(rbind, lapply(seq_along(refs)[-1], function(i) {

    first_byte <- refs[[i - 1]][4] + 1
    first_line <- refs[[i - 1]][3]
    last_line <- refs[[i]][3]
    last_byte <- refs[[i]][4]
    lloc <- c(first_line, first_byte, last_line, last_byte)
    out <- as.character(srcref(srcfile, lloc))
    out_cleaned <- out[grepl(out, pattern = "#' |test_that")]
    if (length(out_cleaned) == 0) {
      return(data.frame())
    } else {
      roxy_block <- gsub(pattern = "#' ", replacement = "",
                         out_cleaned[grep(pattern = "#' ", out_cleaned)])
      if(length(roxy_block) == 0) return(data.frame())
      return(data.frame(title = str_replace(basename(one_file), "_", "\\\\_"),
                        last_update_by = get_section_contents("Updated By", roxy_block),
                        last_updated_date = lubridate::parse_date_time(get_section_contents("Last Update Date:", roxy_block), orders = c("ymd", "mdy")),
                        stringsAsFactors = FALSE))
    }
  }))
}

#' @title eval_processing_tc_code
#' @description Evaluate the procesing test code from .RMD file when the top
#' section has been identified as a code chunk (should start at first line)
#' @param locations postion in \code{lines} which correspond to start and stop
#' of code chunks
#' @param lines RMD source with previously rendered sections omitted.
#' @param envir Environment to evaluate R code chunks. Allows for later chunks
#' in single RMD to depend on earlier ones.
eval_processing_tc_code <- function(locations, lines, envir){
  new_start <- locations[1] + 1
  new_end <- locations[2] - 1
  cat(eval(parse(text = lines[new_start:new_end]), envir = envir), sep = "\n")
  # exclude the current code chunk lines from next recursion
  new_lines <- lines[(locations[2] + 1):length(lines)]
  # recursive call
  process_single_rmd(new_lines, envir = envir)
}

# process a section of the RMD that has been identified as code
#' @title eval_processing_tc_markdown
#' @description Evaluate the procesing test code from .RMD file when the top
#' section has been identified as a markdown section.
#' @param locations postion in \code{lines} which correspond to start and stop
#' of next code chunks
#' @param lines RMD source with previously rendered sections omitted.
#' @param envir Environment to evaluate R code chunks. Allows for later chunks
#' in single RMD to depend on earlier ones.
eval_processing_tc_markdown <- function(locations, lines, envir){
  new_start <- 1
  new_end <- locations[1] - 1
  cat(lines[new_start:new_end], sep = "\n")
  # exclude the markdown before next code chunk from next recursion
  new_lines <- lines[locations[1]:length(lines)]
  # recursive call
  process_single_rmd(new_lines, envir = envir)
}

# recursive function
#' @title process_single_rmd
#' @description Recursive function to handle processing test code RMD files.
#' @param lines RMD source with previously rendered sections omitted.
#' @param envir environment for evaluating R code. Allows for later chunks in
#' single RMD to depend on earlier ones.
process_single_rmd <- function(lines, envir = new.env()){
  locations <- grep("```", lines)
  if (length(locations) < 2) { # base case, nothing
    invisible()
  } else {
    # is the next section code or markdown?
    code_or_markdown <- ifelse(locations[1] == 1, "code", "markdown")
    # setup for recursive call will be different if code vs. markdown
    switch(code_or_markdown,
           code = eval_processing_tc_code(locations, lines, envir = envir),
           markdown = eval_processing_tc_markdown(locations, lines, envir = envir))

  }
}

#' @name scrape_processing_rmd
#' @title Scrape a processing test code RMD file
#' @param file character vector specifying file location
#' @param envir Environment to evaluate R code chunks. Allows for later chunks
#' in single RMD to depend on earlier ones.
scrape_spec_rmd <- function(file, envir = new.env()){
  lines <- readLines(file)
  # remove the roxy section
  clean_lines <- lines[!grepl("#' ", lines)]
  # if it detects that there are roxygen code chunks to execute,
  # process them and cat the markdown
  if (length(grep("^```\\{r.*\\}", clean_lines)) > 0) {
    process_single_rmd(clean_lines, envir = envir)
  } else {
    clean_lines <- str_replace_all(
      str_replace_all(clean_lines, "<", "\\\\<"),
      ">", "\\\\>"
    )
    cat(clean_lines,sep = "\n")
  }
}

#' @name make_test_case_rmd
#' @title Convert a given CSV dataset into a test case RMD file
#' @param file character vector specifying CSV file location
make_test_case_rmd <- function(file) {
  require(stringr)

  # Read in the test data from the CSV
  test_case_df <- read.csv(file, stringsAsFactors=FALSE)

  # Prepare the output rows
  dat <- test_case_df %>%
    # Requires rowwise operations
    rowwise() %>%
    # Append any necessary text to the front of lines based on type
    mutate(out = case_when(
      # Title
      LineType == "Title" ~ paste("#' @title", Text),

      # Last Updated By
      LineType == "UpdatedBy" ~ paste("#' @section Last Updated By:\n#'", Text),

      # Last Updated Date
      LineType == "UpdatedDate" ~ paste("#' @section Last Update Date:\n#'", Text),

      # Setup
      LineType == "Setup" ~ paste0(paste(rep(' ', 0*2), collapse=''), "+ Setup: ", Text, "\n"),

      # Test Cases
      LineType == "TestCase" ~ paste0(paste(rep(' ', 1*2), collapse=''), "+ ", TestID, ": ", Text),

      # Test Cases
      LineType == "Check" ~ paste0(paste(rep(' ', 2*2), collapse=''), "+ ", TestID, ".", CheckID, ": ", Text)
    ))

  # Create the file text vector - need to write 'Test Cases' inbetween the headers lines and the rest of the text
  outfile <- c(
    dat[!(dat$LineType %in% c("Check", "TestCase", "Setup")), ][['out']],
    c('', 'This section contains details of each test executed. Checks verifying each test are included as sub-bullets of their associated test.', ''),
    dat[dat$LineType %in% c("Check", "TestCase", "Setup"), ][['out']]
  )

  # Write the lines to each output file
  writeLines(outfile, '~/Tplyr/uat/references/output/test_cases.Rmd')
}

#' @name make_specification_rmd
#' @title Convert a given CSV dataset into a specificcation RMD file
#' @param file character vector specifying CSV file location
make_specification_rmd <- function(file) {
  require(stringr)

  # Read in the test data from the CSV
  specs_df <- read.csv(file, stringsAsFactors=FALSE)

  # Prepare the output rows
  dat <- specs_df %>%
    # Requires rowwise operations
    rowwise() %>%
    # Append any necessary text to the front of lines based on type
    mutate(out = case_when(
      # Title
      LineType == "Title" ~ paste("#' @title", Text),

      # Last Updated By
      LineType == "UpdatedBy" ~ paste("#' @section Last Updated By:\n#'", Text),

      # Last Updated Date
      LineType == "UpdatedDate" ~ paste("#' @section Last Update Date:\n#'", Text),

      # Test Cases
      LineType == "Specs" ~ paste0(paste(rep(' ', 0*2), collapse=''), "+ ", RequirementID, ": ", Text)
    ))

  # Create the file text vector - need to write 'Test Cases' inbetween the headers lines and the rest of the text
  outfile <- c(
    dat[dat$LineType != 'Specs', ][['out']],
    dat[dat$LineType == 'Specs', ][['out']]
  )

  # Write the lines to each output file
  writeLines(outfile, '~/Tplyr/uat/references/output/specification.Rmd')
}
