## code to prepare `fit_imp` dataset goes here
fit_imp <- readr::read_rds("data-raw/imputed_models_no_data.RDS")

## correct the name of systolib blood pressure: rr -> sbp
fit_imp <- dplyr::rename(fit_imp, ind_sbp = ind_rr)
fit_imp <- dplyr::mutate(fit_imp, set = sub("rr", "sbp", set))

usethis::use_data(fit_imp, overwrite = TRUE)
