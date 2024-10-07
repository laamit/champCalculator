#' single_patient UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_single_patient_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel(h1("CHAMP risk calculator", align = "center")),
    hr(),
    fluidRow(
      column(
        5,
        
        selectInput(ns("dispatch_code"), 
                    h3("Patient group"),
                    choices = list(
                      "Cardiac arrest"              = "Cardiac arrest"              , 
                      "Trauma"                      = "Trauma"                      ,
                      "Respiratory failure"         = "Respiratory failure"         ,
                      "Chest pain"                  = "Chest pain"                  ,
                      "Stroke"                      = "Stroke"                      ,
                      "Neurological"                = "Neurological"                ,
                      "Gynaecology and obsterics"   = "Gynaecology and obsterics"   ,
                      "Infection"                   = "Infection"                   ,
                      "Psychiatric or intoxication" = "Psychiatric or intoxication" ,
                      "Other"                       = "Other"                       ),
                    selected = "Cardiac arrest"),
        
        radioButtons(ns("vehicle"), 
                     h3("HEMS vehicle"),
                     choices = list("Ground unit" = 1, "Helicopter"  = 0), 
                     selected = 1),
        
        radioButtons(ns("med_facility"), 
                     h3("Medical facility or nursing home"),
                     choices = list("No"  = 0, "Yes" = 1), 
                     selected = 0),
        
        radioButtons(ns("sex_man"), 
                     h3("Patient sex"),
                     choices = list("Female"  = 0, "Male" = 1), 
                     selected = 0),
        
        numericInput(ns("age"), h3("Age (years)"), 
                     value = NA, min = 16, max = 100),
        
        numericInput(ns("time_from_alarm"), h3("Time from alarm to HEMS arrival (minutes)"), 
                     value = NA, min = 0, max = 160),
        
      ),
      column(4, 
             radioButtons(ns("cardiac_rhythm"), 
                          h3("Cardiac rhythm"),
                          choices = list("Other category"        = 0, 
                                         "VF, VT, Asystole, PEA" = 1,
                                         "<Missing>"             = -1), 
                          selected = -1),
             numericInput(ns("pulse"), h3("Heart rate (bpm)"), 
                          value = NA, min = 20, max = 220),
             
             numericInput(ns("sbp"), h3("Systolic blood pressure (mmHg)"), 
                          value = NA, min = 40, max = 250),
             
             numericInput(ns("spo2"), h3("Oxygen saturation (%)"), 
                          value = NA, min = 40, max = 100),   
             
             numericInput(ns("gcs"), h3("Glasgow Coma Scale"),  
                          min = 3, max = 15, value = NA),
             
      ),
      column(3,
             h3("30d mortality risk:"),
             h2(textOutput(ns("patient_risk"))),
             tags$br(),
             tags$br(),
             h3("Options:"),
             checkboxInput(ns("winsorize_values"), "Winsorize values to match original data", TRUE),
      )
    )
  )
}

#' single_patient Server Functions
#'
#' @noRd 
mod_single_patient_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    patient_risk <- reactive({
      
      cardiac_rhythm <- input$cardiac_rhythm %>% as.numeric()
      if (cardiac_rhythm == -1) cardiac_rhythm <- as.numeric(NA)
      
      # if (
      #   is.na(time_from_alarm) |
      #   is.na(age) |
      #   is.na(sex_man) |
      #   is.na(med_facility) |
      #   is.na(vehicle) |
      #   is.na(dispatch_code)
      # ) return("A required variable is missing")
      
      if (is.na(input$time_from_alarm)) return("-")
      if (is.na(input$age)) return("-")
      if (is.na(input$sex_man)) return("-")
      if (is.na(input$med_facility)) return("-")
      if (is.na(input$vehicle)) return("-")
      if (is.na(input$dispatch_code)) return("-")
      
      risk <- calculate_champ(
        pulse               = input$pulse           %>% as.numeric(),
        sbp                 = input$sbp             %>% as.numeric(),
        spo2                = input$spo2            %>% as.numeric(),
        time_from_alarm     = input$time_from_alarm %>% as.numeric(),
        gcs                 = input$gcs             %>% as.numeric(),
        cardiac_rhythm      = cardiac_rhythm,
        age                 = input$age           %>% as.numeric(),
        sex_man             = input$sex_man       %>% as.numeric(),
        medical_facility    = input$med_facility  %>% as.numeric(),
        vehicle_ground_unit = input$vehicle       %>% as.numeric(),
        code                = input$dispatch_code,
        limit_values        = input$winsorize_values)
      
      risk <- paste0(round(risk, digits = 3) * 100, "%")
      
      risk
      
    })
    
    output$patient_risk <- renderText({ patient_risk() })
  })
}



## To be copied in the UI
# mod_single_patient_ui("single_patient_ui_1")

## To be copied in the server
# mod_single_patient_server("single_patient_ui_1")
