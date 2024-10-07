#' Names of the variables
#'
#' Names of the variables used in the CHAMP algorithm. Matches the variable 
#' definition excel to the functions.
#'
#' @format A tibble with 11 rows and 2 variables:
#' \describe{
#'   \item{data}{Name in the data}
#'   \item{func}{Name in the function}
#' }
"var_names"

#' Models fitted using the imputed data
#'
#' Contains models for each of the missing variable combinations
#'
#' @format A tibble with 11 rows and 2 variables:
#' \describe{
#'   \item{ind_pulse}{Indicator for missing pulse}
#'   \item{ind_sbp}{Indicator for missing blood pressure}
#'   \item{ind_spo2}{Indicator for missing SpO2}
#'   \item{ind_gcs}{Indicator for missing GCS}
#'   \item{ind_rhythm}{Indicator for missing rhythm}
#'   \item{set}{Name of the set}
#'   \item{mod}{Fitted model used for getting model fits}
#' }
"fit_imp"

