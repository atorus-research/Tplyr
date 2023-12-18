# note: adae.Rdata was copied over from vignettes/adsl.Rdata
# this is a copy of the PHUSE Test Data Factory data, trimmed down for size

load("data-raw/adae.Rdata")
tplyr_adae <- adae
usethis::use_data(tplyr_adae, overwrite = TRUE)
