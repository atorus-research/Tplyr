#' ADSL Data
#'
#' A subset of the PHUSE Test Data Factory ADSL data set.
#'
#' @format A data.frame with 254 rows and 49 columns.
#'
#' @seealso [get_data_labels()]
#'
#' @source https://github.com/phuse-org/TestDataFactory
#'
"tplyr_adsl"


#' ADAE Data
#'
#' A subset of the PHUSE Test Data Factory ADAE data set.
#'
#' @format A data.frame with 276 rows and 55 columns.
#'
#' @seealso [get_data_labels()]
#'
#' @source https://github.com/phuse-org/TestDataFactory
#'
"tplyr_adae"

#' ADAS Data
#'
#' A subset of the PHUSE Test Data Factory ADAS data set.
#'
#' @format A data.frame with 1,040 rows and 40 columns.
#'
#' @seealso [get_data_labels()]
#'
#' @source https://github.com/phuse-org/TestDataFactory
#'
"tplyr_adas"

#' ADLB Data
#'
#' A subset of the PHUSE Test Data Factory ADLB data set.
#'
#' @format A data.frame with 311 rows and 46 columns.
#'
#' @seealso [get_data_labels()]
#'
#' @source https://github.com/phuse-org/TestDataFactory
#'
"tplyr_adlb"

#' ADPE Data
#'
#' A mock-up dataset that is fit for testing data limiting
#'
#' @format A data.frame with 21 rows and 8 columns.
#'
#'
"tplyr_adpe"

#' Get Data Labels
#'
#' Get labels for data sets included in Tplyr.
#'
#' @param data A Tplyr data set.
#'
#' @return A data.frame with columns `name` and `label` containing the names and labels of each column.
#'
#' @export
get_data_labels <- function(data) {
  map_dfr(
    names(data),
    function(name) {
      list(name = name, label = attr(data[[name]], "label"))
    }
  )
}
