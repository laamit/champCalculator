#' wrangle_variable:  Wrangle a single input variable for the champ calculator 
#'
#' @description A utils function
#'
#'
#' @param var_name Name of the variable to be wrangled
#' @param df_in Input data frame
#' @param df_definitions Variable definitions gotten from the 
#'    variable_definitions.xlsx
#' @return Vector containing the wrangled variable to be used in the calculate_champ-function
#'
#' @importFrom dplyr filter %>% pull
#' @importFrom tibble tribble
#' 
#' @export
#'
#' @examples
#' 
#' # files in the root directory
#' if (FALSE) {
#' df_defintions <- readxl::read_excel("variable_definitions_example.xlsx", sheet = 1)
#' df_in <- readxl::read_excel("example_data.xlsx")
#' 
#' # wrangle data
#' c_age <- wrangle_variable("Age (years)", df_in, df_defintions)
#' c_pulse <- wrangle_variable("Heart rate (bpm)", df_in, df_defintions)
#' c_bp <- wrangle_variable("Systolic blood pressure (mmHg)", df_in, df_defintions)
#' c_spo2 <- wrangle_variable("Oxygen saturation (%)", df_in, df_defintions)
#' c_time <- wrangle_variable("Time from alarm to HEMS arrival (minutes)", df_in, df_defintions)
#' c_gcs <- wrangle_variable("Glasgow Coma Scale", df_in, df_defintions)
#' c_sex <- wrangle_variable("Patient sex", df_in, df_defintions)
#' c_crhythm <- wrangle_variable("Cardiac rhythm", df_in, df_defintions)
#' c_med_facility <- wrangle_variable("Medical facility or nursing home", df_in, df_defintions)
#' c_vehicle <- wrangle_variable("HEMS vehicle", df_in, df_defintions)
#' c_code <- wrangle_variable("Patient group", df_in, df_defintions)
#' 
#' # calculate risk
#' calculate_champ(rr = c_bp,
#'                pulse = c_pulse,
#'                spo2 = c_spo2,
#'                gcs = c_gcs,
#'                time_from_alarm = c_time,
#'                cardiac_rhythm = c_crhythm,
#'                age = c_age,
#'                medical_facility = c_med_facility,
#'                vehicle_ground_unit = c_vehicle,
#'                sex_man = c_sex,
#'                code = c_code)
#' 
#' }
wrangle_variable <- function(var_name, df_in, df_definitions) {
  # setup ------------------------------------------------
  df_var_info <- df_definitions %>% dplyr::filter(.data$variable == var_name)
  na_vals <- df_var_info %>% 
    dplyr::filter(.data$name_of_category == "<missing>") %>% 
    dplyr::pull(.data$value)
  
  x <- df_in[[df_var_info$name_in_data[1]]]
  
  if (!is.na(na_vals)) {
    ## potential multiple values need to be converted to a vector
    na_vals <- na_vals %>% strsplit(",") %>% unlist() 
    x <- ifelse(x %in% na_vals, NA, x)
  }
  
  # forcefully convert numeric variables to numeric--------------------------
  if (var_name %in% c(
    "Age (years)",
    "Heart rate (bpm)",
    "Systolic blood pressure (mmHg)",
    "Oxygen saturation (%)",
    "Time from alarm to HEMS arrival (minutes)",
    "Glasgow Coma Scale"
  )) x <- as.numeric(x)
  

  # wrangle categorical data ------------------------------------------------
  
  if (var_name %in% c(
    "Patient sex",
    "Cardiac rhythm",
    "Medical facility or nursing home",
    "HEMS vehicle",
    "Patient group"
  )) {
    ## force variable to be character
    x <- as.character(x)
    var_levels <- df_var_info %>% dplyr::filter(.data$name_of_category != "<missing>")
    
    for (lvl_i in 1:nrow(var_levels)) {
      ## multiple values per level need to be pre wrangled i.e. separated to a vector
      lvl_i_values <- var_levels$value[lvl_i] %>% strsplit(",") %>% unlist() %>% trimws()
      x[x %in% lvl_i_values] <- var_levels$name_of_category[lvl_i]
    }
    ## change the "<any other value>" values to NA, i.e. values that were not defined above
    x[!(x %in% var_levels$name_of_category)] <- NA
    
    ## change binary variables to 0/1
    if (var_name == "Patient sex") {
      x <- dplyr::case_when(x == "Male" ~ 1, x == "Female" ~ 0)
    }
    if (var_name == "Cardiac rhythm") {
      x <- dplyr::case_when(x == "VF, VT, Asystole, PEA" ~ 1, 
                            x != "VF, VT, Asystole, PEA" ~ 0)
      
    }
    if (var_name == "Medical facility or nursing home") {
      x  <- dplyr::case_when(x == "Yes" ~ 1,
                             x != "Yes" ~ 0)
    }
    if (var_name == "HEMS vehicle") {
      x  <- dplyr::case_when(x == "Ground unit" ~ 1,
                             x != "Ground unit" ~ 0)
      
    }
  }
  
  x
}
