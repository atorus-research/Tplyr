# This adpe dataset is just a mock-up that's fit for purpose to test and demonstrate data limiting
tplyr_adpe <- tibble::tribble(~USUBJID,  ~AVISIT,     ~PECAT, ~PARAM,    ~TRT01A,   ~AVALC, ~AVAL, ~BASEC,
                        "101-001", "Screening", "A",    "Head",    "TRT A",  "Normal", 1, "Abnormal",
                        "101-001", "Screening", "A",    "Lungs",   "TRT A",  "Normal", 2, "Semi-Normal",
                        "101-001", "Day -1",    "A",    "Lungs",   "TRT A",  "Normal", 3, "Normal",
                        "101-001", "Day 5",     "A",    "Lungs",   "TRT A",  "Normal", 4, "Normal",
                        "101-002", "Screening", "A",    "Head",    "TRT B",  "Semi-Normal", 5, "Normal",
                        "101-002", "Screening", "A",    "Lungs",   "TRT B",  "Normal", 6, "Normal",
                        "101-002", "Day -1",    "A",    "Lungs",   "TRT B",  "Normal", 7, "Normal",
                        "101-002", "Day 5",     "A",    "Lungs",   "TRT B",  "Normal", 8, "Semi-Normal",
                        "101-003", "Screening", "A",    "Head",    "TRT A",  "Normal", 9, "Normal",
                        "101-003", "Screening", "A",    "Lungs",   "TRT A",  "Abnormal", 10, "Abnormal",
                        "101-003", "Day -1",    "A",    "Lungs",   "TRT A",  "Normal", 11, "Abnormal",
                        "101-003", "Day 5",     "A",    "Lungs",   "TRT A",  "Abnormal", 12, "Abnormal",

                        "101-001", "Screening", "B",     "Lungs",   "TRT A",  "Normal", 13, "Normal",
                        "101-001", "Day -1",    "B",     "Lungs",   "TRT A",  "Normal", 14, "Normal",
                        "101-001", "Day 5",     "B",     "Lungs",   "TRT A",  "Normal", 15, "Semi-Normal",
                        "101-002", "Screening", "B",     "Lungs",   "TRT B",  "Normal", 16, "Normal",
                        "101-002", "Day -1",    "B",     "Lungs",   "TRT B",  "Normal", 17, "Abnormal",
                        "101-002", "Day 5",     "B",     "Lungs",   "TRT B",  "Normal", 18, "Abnormal",
                        "101-003", "Screening", "B",     "Lungs",   "TRT A",  "Abnormal", 19, "Normal",
                        "101-003", "Day -1",    "B",    "Lungs",    "TRT A",   "Normal", 20, "Normal",
                        "101-003", "Day 5",     "B",     "Lungs",   "TRT A",  "Abnormal", 21, "Normal"
)

tplyr_adpe$AVALC <- factor(tplyr_adpe$AVALC, levels = c("Normal", "Semi-Normal", "Abnormal"))
tplyr_adpe$BASEC <- factor(tplyr_adpe$BASEC, levels = c("Normal", "Semi-Normal", "Abnormal"))
tplyr_adpe$AVISIT <- factor(tplyr_adpe$AVISIT, levels = c("Screening", "Day -1", "Day 5"))

usethis::use_data(tplyr_adpe, overwrite = TRUE)
