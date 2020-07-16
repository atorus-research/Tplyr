process_statistics <- function(x, stat) {
  UseMethod('process_statistic')
}

process_statistics.tplyr_riskdiff <- function(x, stat) {

}
