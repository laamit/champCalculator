## code to prepare `var_names` dataset goes here
var_names <- tibble::tribble(
  ~data,                                        ~func,
  "Age (years)",                               "age",
  "Heart rate (bpm)",                          "pulse",
  "Systolic blood pressure (mmHg)",            "sbp",
  "Oxygen saturation (%)",                     "spo2",
  "Time from alarm to HEMS arrival (minutes)",            "time_from_alarm",
  "Glasgow Coma Scale",                        "gcs",
  "Patient sex",                               "sex_male",
  "Cardiac rhythm",                            "cardiac_rhythm",
  "Medical facility or nursing home",          "medical_facility",
  "HEMS vehicle",                              "vehicle_ground_unit",
  "Patient group",                             "code",
)

usethis::use_data(var_names, overwrite = TRUE)
