# note: adlb.Rdata was copied over from vignettes/adsl.Rdata
# this is a copy of the PHUSE Test Data Factory data, trimmed down for size

load("data-raw/adlb.Rdata")
tplyr_adlb <- adlb
usethis::use_data(tplyr_adlb, overwrite = TRUE)
