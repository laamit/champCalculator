## code to prepare `fit_imp` dataset goes here
fit_imp <- readr::read_rds("data-raw/imputed_models_no_data.RDS")

usethis::use_data(fit_imp, overwrite = TRUE)
