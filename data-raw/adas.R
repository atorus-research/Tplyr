# note: adlb.Rdata was copied over from vignettes/adsl.Rdata
# this is a copy of the PHUSE Test Data Factory data, trimmed down for size

load("data-raw/adas.Rdata")
tplyr_adas <- adas
usethis::use_data(tplyr_adas, overwrite = TRUE)
