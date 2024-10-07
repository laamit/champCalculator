
# setup -------------------------------------------------------------------

df_mods <- champCalculator::fit_imp
library(dplyr)
library(testthat)
library(champCalculator)

# tests for all model types - single patient ----------------------------------------------------


for (i in 1:nrow(df_mods)) {
  
  test_that(paste("single champ calculation works for model", i, "/ 32"), {
    
    
    # setup data --------------------------------------------------------------
    
    
    gcs_i <- 15 
    pulse_i <- 100 
    cardiac_rhythm_i <- 1 
    rr_i <- 100
    spo2_i <- 100 
    time_from_alarm_i <- 100 
    age_i <- 20
    medical_facility_i <- 0 
    vehicle_ground_unit_i <- 1 
    sex_man_i <- 1 
    code_i <- "trauma"
    limit_values <- TRUE
    
    df_mod_i <- df_mods %>% dplyr::slice(i)
    
    if (df_mod_i$ind_gcs)           gcs_i <- NA
    if (df_mod_i$ind_pulse)         pulse_i <- NA
    if (df_mod_i$ind_rhythm)        cardiac_rhythm_i <- NA
    if (df_mod_i$ind_rr)            rr_i <- NA
    if (df_mod_i$ind_spo2)          spo2_i <- NA
    
    
    # model predict -----------------------------------------------------------
    risk_function <- calculate_champ(
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_from_alarm = time_from_alarm_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_man = sex_man_i, code = code_i, limit_values = limit_values)
    
    
    
    # manual predict ----------------------------------------------------------
    
    code_cardiac_arrest  = dplyr::case_when(code_i == "cardiac arrest"     ~  1, !is.na(code_i) ~ 0)          
    code_trauma       = dplyr::case_when(code_i == "trauma"             ~  1, !is.na(code_i) ~ 0)   
    code_respitory    = dplyr::case_when(code_i == "respitory failure"  ~  1, !is.na(code_i) ~ 0)        
    code_chest_pain   = dplyr::case_when(code_i == "chest pain"         ~  1, !is.na(code_i) ~ 0)         
    code_stroke       = dplyr::case_when(code_i == "stroke"             ~  1, !is.na(code_i) ~ 0)   
    code_neuro        = dplyr::case_when(code_i == "neurological"       ~  1, !is.na(code_i) ~ 0)     
    code_psyc_intox   = dplyr::case_when(code_i == "psychiatric or intoxication" ~  1, !is.na(code_i) ~ 0)     
    code_other        = dplyr::case_when(code_i == "other"                     ~  1, !is.na(code_i) ~ 0) 
    
    
    df_new <- tibble::tibble(
      rr = rr_i,
      pulse = pulse_i,
      spo2 = spo2_i,
      gcs = gcs_i,
      time_from_alarm = time_from_alarm_i,
      age = age_i,
      cardiac_rhythm = cardiac_rhythm_i,
      med_facility = medical_facility_i,
      vehicle_ground_unit = vehicle_ground_unit_i,
      sex_man = sex_man_i,
      code_cardiac_arrest = code_cardiac_arrest,
      code_trauma = code_trauma,
      code_respitory = code_respitory,
      code_chest_pain = code_chest_pain,
      code_stroke = code_stroke,
      code_neuro = code_neuro,
      code_psyc_intox = code_psyc_intox,
      code_other = code_other,
    ) 
    
    ## winsorize numeric data  ---------------------------------------
    if (limit_values) {
      ## Limits taken from the original data as 0.5 and 99.5 percentile values
      ## for variables can have long tails based on the original data
      df_limits <- tibble::tribble(
        ~x,               ~low,      ~up,
        "pulse",            25,       200, 
        "rr",               51,       235, 
        "spo2",             50,       100,
        "time_from_alarm",  4,        80,
      )
      
      for (i in 1:nrow(df_limits)) {
        df_limits_i <- df_limits %>% dplyr::slice(i)
        df_new[[ df_limits_i$x ]] <- ifelse(df_new[[ df_limits_i$x ]] < df_limits_i$low, 
                                            df_limits_i$low, df_new[[ df_limits_i$x ]])
        df_new[[ df_limits_i$x ]] <- ifelse(df_new[[ df_limits_i$x ]] > df_limits_i$up, 
                                            df_limits_i$up, df_new[[ df_limits_i$x ]])
        
        
      }
      
    } 
    
    
    log_odds <- predict(df_mod_i$mod[[1]], newdata = df_new) 
    risk_manual <- exp(log_odds) / (1 + exp(log_odds))
    
    expect_equal(risk_function, risk_manual[[1]])
  })
}


# tests for all model types - multiple patients ----------------------------------------------------




for (i in 1:nrow(df_mods)) {
  
  test_that(paste("single champ calculation works for model", i, "/ 32"), {
    
    
    # setup data --------------------------------------------------------------
    
    
    gcs_i <- 10:15 
    pulse_i <- 95:100 
    cardiac_rhythm_i <- c(0, 0, 0, 1, 1, 1) 
    rr_i <- 95:100
    spo2_i <- 95:100 
    time_from_alarm_i <- 95:100 
    age_i <- 50:55
    medical_facility_i <- c(0, 0, 0, 1, 1, 1)  
    vehicle_ground_unit_i <- c(1, 1, 1, 0, 0, 0)  
    sex_man_i <- c(1, 0, 1, 0, 1, 0)  
    code_i <- c("trauma", "other", "stroke", "chest pain", "trauma", "cardiac arrest")
    limit_values <- TRUE
    
    df_mod_i <- df_mods %>% dplyr::slice(i)
    
    if (df_mod_i$ind_gcs)           gcs_i <- NA
    if (df_mod_i$ind_pulse)         pulse_i <- NA
    if (df_mod_i$ind_rhythm)        cardiac_rhythm_i <- NA
    if (df_mod_i$ind_rr)            rr_i <- NA
    if (df_mod_i$ind_spo2)          spo2_i <- NA
    
    
    # model predict -----------------------------------------------------------
    risk_function <- calculate_champ(
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_from_alarm = time_from_alarm_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_man = sex_man_i, code = code_i, limit_values = limit_values)
    
    
    
    # manual predict ----------------------------------------------------------
    
    code_cardiac_arrest  = dplyr::case_when(code_i == "cardiac arrest"     ~  1, !is.na(code_i) ~ 0)          
    code_trauma       = dplyr::case_when(code_i == "trauma"             ~  1, !is.na(code_i) ~ 0)   
    code_respitory    = dplyr::case_when(code_i == "respitory failure"  ~  1, !is.na(code_i) ~ 0)        
    code_chest_pain   = dplyr::case_when(code_i == "chest pain"         ~  1, !is.na(code_i) ~ 0)         
    code_stroke       = dplyr::case_when(code_i == "stroke"             ~  1, !is.na(code_i) ~ 0)   
    code_neuro        = dplyr::case_when(code_i == "neurological"       ~  1, !is.na(code_i) ~ 0)     
    code_psyc_intox   = dplyr::case_when(code_i == "psychiatric or intoxication" ~  1, !is.na(code_i) ~ 0)     
    code_other        = dplyr::case_when(code_i == "other"                     ~  1, !is.na(code_i) ~ 0) 
    
    
    df_new <- tibble::tibble(
      rr = rr_i,
      pulse = pulse_i,
      spo2 = spo2_i,
      gcs = gcs_i,
      time_from_alarm = time_from_alarm_i,
      age = age_i,
      cardiac_rhythm = cardiac_rhythm_i,
      med_facility = medical_facility_i,
      vehicle_ground_unit = vehicle_ground_unit_i,
      sex_man = sex_man_i,
      code_cardiac_arrest = code_cardiac_arrest,
      code_trauma = code_trauma,
      code_respitory = code_respitory,
      code_chest_pain = code_chest_pain,
      code_stroke = code_stroke,
      code_neuro = code_neuro,
      code_psyc_intox = code_psyc_intox,
      code_other = code_other,
    ) 
    
    ## winsorize numeric data  ---------------------------------------
    if (limit_values) {
      ## Limits taken from the original data as 0.5 and 99.5 percentile values
      ## for variables can have long tails based on the original data
      df_limits <- tibble::tribble(
        ~x,               ~low,      ~up,
        "pulse",            25,       200, 
        "rr",               51,       235, 
        "spo2",             50,       100,
        "time_from_alarm",  4,        80,
      )
      
      for (i in 1:nrow(df_limits)) {
        df_limits_i <- df_limits %>% dplyr::slice(i)
        df_new[[ df_limits_i$x ]] <- ifelse(df_new[[ df_limits_i$x ]] < df_limits_i$low, 
                                            df_limits_i$low, df_new[[ df_limits_i$x ]])
        df_new[[ df_limits_i$x ]] <- ifelse(df_new[[ df_limits_i$x ]] > df_limits_i$up, 
                                            df_limits_i$up, df_new[[ df_limits_i$x ]])
        
        
      }
      
    } 
    
    
    log_odds <- predict(df_mod_i$mod[[1]], newdata = df_new) 
    names(log_odds) <- NULL # drop names so format matches
    risk_manual <- exp(log_odds) / (1 + exp(log_odds))
    
    expect_equal(risk_function, risk_manual)
  })
}



# tests for all model types - single patient ----------------------------------------------------


for (i in 1:nrow(df_mods)) {
  
  test_that(paste("single champ calculation works (no winz) for model", i, "/ 32"), {
    
    
    # setup data --------------------------------------------------------------
    
    
    gcs_i <- 15 
    pulse_i <- 100 
    cardiac_rhythm_i <- 1 
    rr_i <- 100
    spo2_i <- 100 
    time_from_alarm_i <- 100 
    age_i <- 20
    medical_facility_i <- 0 
    vehicle_ground_unit_i <- 1 
    sex_man_i <- 1 
    code_i <- "trauma"
    limit_values <- FALSE
    
    df_mod_i <- df_mods %>% dplyr::slice(i)
    
    if (df_mod_i$ind_gcs)           gcs_i <- NA
    if (df_mod_i$ind_pulse)         pulse_i <- NA
    if (df_mod_i$ind_rhythm)        cardiac_rhythm_i <- NA
    if (df_mod_i$ind_rr)            rr_i <- NA
    if (df_mod_i$ind_spo2)          spo2_i <- NA
    
    
    # model predict -----------------------------------------------------------
    risk_function <- calculate_champ(
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_from_alarm = time_from_alarm_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_man = sex_man_i, code = code_i, limit_values = limit_values)
    
    
    
    # manual predict ----------------------------------------------------------
    
    code_cardiac_arrest  = dplyr::case_when(code_i == "cardiac arrest"     ~  1, !is.na(code_i) ~ 0)          
    code_trauma       = dplyr::case_when(code_i == "trauma"             ~  1, !is.na(code_i) ~ 0)   
    code_respitory    = dplyr::case_when(code_i == "respitory failure"  ~  1, !is.na(code_i) ~ 0)        
    code_chest_pain   = dplyr::case_when(code_i == "chest pain"         ~  1, !is.na(code_i) ~ 0)         
    code_stroke       = dplyr::case_when(code_i == "stroke"             ~  1, !is.na(code_i) ~ 0)   
    code_neuro        = dplyr::case_when(code_i == "neurological"       ~  1, !is.na(code_i) ~ 0)     
    code_psyc_intox   = dplyr::case_when(code_i == "psychiatric or intoxication" ~  1, !is.na(code_i) ~ 0)     
    code_other        = dplyr::case_when(code_i == "other"                     ~  1, !is.na(code_i) ~ 0) 
    
    
    df_new <- tibble::tibble(
      rr = rr_i,
      pulse = pulse_i,
      spo2 = spo2_i,
      gcs = gcs_i,
      time_from_alarm = time_from_alarm_i,
      age = age_i,
      cardiac_rhythm = cardiac_rhythm_i,
      med_facility = medical_facility_i,
      vehicle_ground_unit = vehicle_ground_unit_i,
      sex_man = sex_man_i,
      code_cardiac_arrest = code_cardiac_arrest,
      code_trauma = code_trauma,
      code_respitory = code_respitory,
      code_chest_pain = code_chest_pain,
      code_stroke = code_stroke,
      code_neuro = code_neuro,
      code_psyc_intox = code_psyc_intox,
      code_other = code_other,
    ) 
    
    ## winsorize numeric data  ---------------------------------------
    if (limit_values) {
      ## Limits taken from the original data as 0.5 and 99.5 percentile values
      ## for variables can have long tails based on the original data
      df_limits <- tibble::tribble(
        ~x,               ~low,      ~up,
        "pulse",            25,       200, 
        "rr",               51,       235, 
        "spo2",             50,       100,
        "time_from_alarm",  4,        80,
      )
      
      for (i in 1:nrow(df_limits)) {
        df_limits_i <- df_limits %>% dplyr::slice(i)
        df_new[[ df_limits_i$x ]] <- ifelse(df_new[[ df_limits_i$x ]] < df_limits_i$low, 
                                            df_limits_i$low, df_new[[ df_limits_i$x ]])
        df_new[[ df_limits_i$x ]] <- ifelse(df_new[[ df_limits_i$x ]] > df_limits_i$up, 
                                            df_limits_i$up, df_new[[ df_limits_i$x ]])
        
        
      }
      
    } 
    
    
    log_odds <- predict(df_mod_i$mod[[1]], newdata = df_new) 
    risk_manual <- exp(log_odds) / (1 + exp(log_odds))
    
    expect_equal(risk_function, risk_manual[[1]])
  })
}
# tests for all model types - multiple patients, no winz ----------------------------------------------------




for (i in 1:nrow(df_mods)) {
  
  test_that(paste("single champ calculation works (no winz) for model", i, "/ 32"), {
    
    
    # setup data --------------------------------------------------------------
    
    
    gcs_i <- 10:15 
    pulse_i <- 95:100 
    cardiac_rhythm_i <- c(0, 0, 0, 1, 1, 1) 
    rr_i <- 95:100
    spo2_i <- 95:100 
    time_from_alarm_i <- 95:100 
    age_i <- 50:55
    medical_facility_i <- c(0, 0, 0, 1, 1, 1)  
    vehicle_ground_unit_i <- c(1, 1, 1, 0, 0, 0)  
    sex_man_i <- c(1, 0, 1, 0, 1, 0)  
    code_i <- c("trauma", "other", "stroke", "chest pain", "trauma", "cardiac arrest")
    limit_values <- FALSE
    
    df_mod_i <- df_mods %>% dplyr::slice(i)
    
    if (df_mod_i$ind_gcs)           gcs_i <- NA
    if (df_mod_i$ind_pulse)         pulse_i <- NA
    if (df_mod_i$ind_rhythm)        cardiac_rhythm_i <- NA
    if (df_mod_i$ind_rr)            rr_i <- NA
    if (df_mod_i$ind_spo2)          spo2_i <- NA
    
    
    # model predict -----------------------------------------------------------
    risk_function <- calculate_champ(
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_from_alarm = time_from_alarm_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_man = sex_man_i, code = code_i, limit_values = limit_values)
    
    
    
    # manual predict ----------------------------------------------------------
    
    code_cardiac_arrest  = dplyr::case_when(code_i == "cardiac arrest"     ~  1, !is.na(code_i) ~ 0)          
    code_trauma       = dplyr::case_when(code_i == "trauma"             ~  1, !is.na(code_i) ~ 0)   
    code_respitory    = dplyr::case_when(code_i == "respitory failure"  ~  1, !is.na(code_i) ~ 0)        
    code_chest_pain   = dplyr::case_when(code_i == "chest pain"         ~  1, !is.na(code_i) ~ 0)         
    code_stroke       = dplyr::case_when(code_i == "stroke"             ~  1, !is.na(code_i) ~ 0)   
    code_neuro        = dplyr::case_when(code_i == "neurological"       ~  1, !is.na(code_i) ~ 0)     
    code_psyc_intox   = dplyr::case_when(code_i == "psychiatric or intoxication" ~  1, !is.na(code_i) ~ 0)     
    code_other        = dplyr::case_when(code_i == "other"                     ~  1, !is.na(code_i) ~ 0) 
    
    
    df_new <- tibble::tibble(
      rr = rr_i,
      pulse = pulse_i,
      spo2 = spo2_i,
      gcs = gcs_i,
      time_from_alarm = time_from_alarm_i,
      age = age_i,
      cardiac_rhythm = cardiac_rhythm_i,
      med_facility = medical_facility_i,
      vehicle_ground_unit = vehicle_ground_unit_i,
      sex_man = sex_man_i,
      code_cardiac_arrest = code_cardiac_arrest,
      code_trauma = code_trauma,
      code_respitory = code_respitory,
      code_chest_pain = code_chest_pain,
      code_stroke = code_stroke,
      code_neuro = code_neuro,
      code_psyc_intox = code_psyc_intox,
      code_other = code_other,
    ) 
    
    ## winsorize numeric data  ---------------------------------------
    if (limit_values) {
      ## Limits taken from the original data as 0.5 and 99.5 percentile values
      ## for variables can have long tails based on the original data
      df_limits <- tibble::tribble(
        ~x,               ~low,      ~up,
        "pulse",            25,       200, 
        "rr",               51,       235, 
        "spo2",             50,       100,
        "time_from_alarm",  4,        80,
      )
      
      for (i in 1:nrow(df_limits)) {
        df_limits_i <- df_limits %>% dplyr::slice(i)
        df_new[[ df_limits_i$x ]] <- ifelse(df_new[[ df_limits_i$x ]] < df_limits_i$low, 
                                            df_limits_i$low, df_new[[ df_limits_i$x ]])
        df_new[[ df_limits_i$x ]] <- ifelse(df_new[[ df_limits_i$x ]] > df_limits_i$up, 
                                            df_limits_i$up, df_new[[ df_limits_i$x ]])
        
        
      }
      
    } 
    
    
    log_odds <- predict(df_mod_i$mod[[1]], newdata = df_new) 
    names(log_odds) <- NULL # drop names so format matches
    risk_manual <- exp(log_odds) / (1 + exp(log_odds))
    
    expect_equal(risk_function, risk_manual)
  })
}


