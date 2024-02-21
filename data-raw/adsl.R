# note: adsl.Rdata was copied over from vignettes/adsl.Rdata
# this is a copy of the PHUSE Test Data Factory data, trimmed down for size

load("data-raw/adsl.Rdata")
tplyr_adsl <- adsl
usethis::use_data(tplyr_adsl, overwrite = TRUE)
