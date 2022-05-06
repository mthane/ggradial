## code to prepare `DATASET` dataset goes here
tinnitus = readRDS("data-raw/tinnitus.rds")
save(tinnitus, file = "tinnitus.RData")
usethis::use_data(tinnitus, overwrite = TRUE)
