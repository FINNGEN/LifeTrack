#
# server.R  - LifeTrack
#
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinybusy)
library(shinyjs)
library(bigrquery)
library(colourpicker)
library(ggiraph)
library(DT)
library(FinnGenUtilsR)

log_entry <- function(...){
  cat(toString(now()), "Sandbox-LifeTrack-log:", ..., "\n")
}
log_entry_multiline <- function(f){
  f.out <- capture.output(f)
  f.out <- stringr::str_replace(f.out, "^", paste(toString(now()),"Sandbox-TVT-log: "))
  lapply(f.out, cat, "\n")
}

log_entry("initializing")

# what is the environment?
SANDBOX_PATH <- (path.expand('~') == "/home/ivm")
BUCKET_SANDBOX_IVM <- Sys.getenv("BUCKET_SANDBOX_IVM") |> 
  stringr::str_remove("-red$") |> 
  stringr::str_remove("_ivm$")
DOCKER_PATH <- (path.expand('~') == "/root")

log_entry("PATH", path.expand('~'))
log_entry("BUCKET_SANDBOX_IVM", BUCKET_SANDBOX_IVM)

# set the HOST accordingly, four possible environments
HOST <- case_when(
  # laptop or sandbox RStudio, Mac Docker
  BUCKET_SANDBOX_IVM == "" &&  DOCKER_PATH && !SANDBOX_PATH ~ "MAC_DOCKER",
  BUCKET_SANDBOX_IVM == "" && !DOCKER_PATH && !SANDBOX_PATH ~ "MAC",
  BUCKET_SANDBOX_IVM == "" && !DOCKER_PATH &&  SANDBOX_PATH ~ "SANDBOX",
  # IVM DOCKER
  str_detect(BUCKET_SANDBOX_IVM, "production") ~ "SANDBOX_DOCKER",
  TRUE ~ "UNKNOWN HOST"
)

log_entry("HOST", HOST)

switch(HOST,
       'MAC' = {
         projectid <- "atlas-development-270609"
         bq_auth(path = "bq_auth/atlas-development-270609-0c4bdf74dad0.json")
         VERSION <- system2("gitversion", args = c("/showVariable", "SemVer"), stdout = TRUE)
         # data tables
         longitudinal_data_table <-
           "atlas-development-270609.sandbox_tools_r11.finngen_r11_service_sector_detailed_longitudinal_v1"
         minimum_data_table <-
           "atlas-development-270609.sandbox_tools_r11.finngenid_info_r11_v1"
         fg_codes_info_table <-
           "atlas-development-270609.medical_codes.fg_codes_info_v5"
         code_prevalence_table <- 
           "atlas-development-270609.sandbox_tools_r11.code_prevalence_stratified_v1"
         # "atlas-development-270609.sandbox_tools_r11.code_prevalence_stratified_r11_v1"
         birth_table <- 
           "atlas-development-270609.sandbox_tools_r11.birth_mother_r11_v1"
       },
       'MAC_DOCKER' = {
         projectid <- "atlas-development-270609"
         options(gargle_oauth_cache=FALSE) #to avoid the question that freezes the app
         bigrquery::bq_auth(path = "bq_auth/atlas-development-270609-0c4bdf74dad0.json")
         # deactivate https
         httr::set_config(httr::config(ssl_verifypeer=FALSE))
         VERSION <- Sys.getenv("VERSION")
         # data tables
         longitudinal_data_table <-
           "atlas-development-270609.sandbox_tools_r11.finngen_r11_service_sector_detailed_longitudinal_v1"
         minimum_data_table <-
           "atlas-development-270609.sandbox_tools_r11.finngenid_info_r11_v1"
         fg_codes_info_table <-
           "atlas-development-270609.medical_codes.fg_codes_info_v5"
         code_prevalence_table <- 
           "atlas-development-270609.sandbox_tools_r11.code_prevalence_stratified_v1"
         birth_table <- 
           "atlas-development-270609.sandbox_tools_r11.birth_mother_r11_v1"
       },
       'SANDBOX' = {
         # RStudio sanitizes BUCKET_SANDBOX_IVM away, must be hard-coded
         projectid <- "fg-production-sandbox-4" 
         VERSION <- Sys.getenv("VERSION")  
         # data tables
         longitudinal_data_table <-
           "finngen-production-library.sandbox_tools_r11.finngen_r11_service_sector_detailed_longitudinal_v1"
         minimum_data_table <-
           "finngen-production-library.sandbox_tools_r11.finngenid_info_r11_v1"
         fg_codes_info_table <-
           "finngen-production-library.medical_codes.fg_codes_info_v5"
         code_prevalence_table <- 
           "finngen-production-library.sandbox_tools_r11.code_prevalence_stratified_r11_v1"
         birth_table <- 
           "finngen-production-library.sandbox_tools_r11.birth_mother_r11_v1"
         options(gargle_oauth_cache = FALSE)
         bq_auth(scopes = "https://www.googleapis.com/auth/bigquery")
       },
       'SANDBOX_DOCKER' = {
         projectid <- BUCKET_SANDBOX_IVM
         VERSION <- Sys.getenv("VERSION")
         options(gargle_oauth_cache=FALSE) #to avoid the question that freezes the app
         bigrquery::bq_auth(scopes = "https://www.googleapis.com/auth/bigquery")
         httr::set_config(httr::config(ssl_verifypeer=FALSE))
         # data tables
         longitudinal_data_table <-
           "finngen-production-library.sandbox_tools_r11.finngen_r11_service_sector_detailed_longitudinal_v1"
         minimum_data_table <-
           "finngen-production-library.sandbox_tools_r11.finngenid_info_r11_v1"
         fg_codes_info_table <-
           "finngen-production-library.medical_codes.fg_codes_info_v5"
         code_prevalence_table <- 
           "finngen-production-library.sandbox_tools_r11.code_prevalence_stratified_r11_v2"
          # "fg-production-sandbox-6.sandbox.code_prevalence_stratified_v1"
         birth_table <- 
           "finngen-production-library.sandbox_tools_r11.birth_mother_r11_v1"
       },
       {
         stop("unknown HOST")
       }
)

log_entry("VERSION", VERSION)

# register time spans and color
df_register_spans <- tribble(
  ~SOURCE, ~START_DATE, ~COLOR, 
  "PURCH", ymd("1995-01-01"), "#6fcb6c",
  "REIMB", ymd("1964-01-01"), "#ab172b",
  "PRIM_OUT", ymd("2011-01-01"), "#c200c6", # "#fdc3ac",
  "INPAT", ymd("1969-01-01"), "#ff4e31",
  "OUTPAT", ymd("1998-01-01"),  "#ad86b7",
  "CANC", ymd("1953-01-01"), "#c1c8c8",
  "DEATH", ymd("1969-01-01"), "black",
  "OPER_IN", ymd("1969-01-01"), "#0f71e0",
  "OPER_OUT", ymd("1969-01-01"),  "#36c4e3",
  "BIRTH", ymd("1953-01-01"), "#62F701"
)

register_colors <- deframe(select(df_register_spans, SOURCE, COLOR))

# modal dialog helper
build_message <- function(...){
  if(length(list(...))) 
    HTML(str_c(..., sep = "<br>"))
} 
# modal dialog
showModalProgress <- function(..., title = NULL, footer = modalButton("OK")){
  if(length(list(...))) 
    showModal(
      modalDialog(div(build_message(...)), title = title, footer = footer, fade = FALSE)
    )
}

#
# build_plot_values ####
#
# - assigns the data into categories and classes
#

build_plot_values <- function(df_all, values){
  df_all <- df_all |> 
    mutate(chapter = case_when(
      SOURCE == "DEATH" ~ "DEATH",
      SOURCE == "BIRTH" ~ "BIRTH",
      SOURCE == "REIMB" ~ "REIMB",
      SOURCE == "CANC" ~ "CANC",
      str_detect(vocabulary_id, "ATC") ~ str_sub(CODE1, end = 1), # the ATC top level
      str_detect(vocabulary_id, "ICD10fi") ~ str_sub(str_replace(FG_CODE1, fixed("."), ""), end = 3),
      str_detect(vocabulary_id, "ICD(8|9)fi") ~ "ICD8or9",
      str_detect(vocabulary_id, "FHL") ~ "FHL",
      str_starts(vocabulary_id, "SPAT") ~ "SPAT",
      str_starts(vocabulary_id, "ICPC") ~ "ICPC",
      str_starts(vocabulary_id, "NCSP") ~ "NCSP",
      str_starts(vocabulary_id, "HPN") ~ "HPN",
      str_starts(vocabulary_id, "HPO") ~ "HPO",
      TRUE ~ "Unclassified"
    )) |> 
    mutate(ORDER = case_when(
      SOURCE == "BIRTH" ~ 4,
      SOURCE == "DEATH" ~ 5,
      SOURCE == "REIMB" ~ 6,
      SOURCE == "CANC" ~ 7,
      str_detect(vocabulary_id, "ATC") ~ 1,
      str_detect(vocabulary_id, "ICD10fi") ~ 2,
      str_detect(vocabulary_id, "ICD(8|9)fi") ~ 3,
      str_detect(vocabulary_id, "FHL") ~ 8,
      str_starts(vocabulary_id, "ICPC") ~ 9,
      str_starts(vocabulary_id, "HPN") ~ 10,
      str_starts(vocabulary_id, "NCSP") ~ 11,
      str_starts(vocabulary_id, "HPO") ~ 12,
      str_starts(vocabulary_id, "SPAT") ~ 13,
      TRUE ~ 0 # "Unclassified"
    ))
  
  df_all <- df_all |>
    mutate(CLASSIFICATION = case_when(
      # ATC
      str_length(chapter) == 1 & chapter == "A" ~ "A. Alimentary tract and metabolism",
      str_length(chapter) == 1 & chapter == "B" ~ "B. Blood and blood forming organs",
      str_length(chapter) == 1 & chapter == "C" ~ "C. Cardiovascular system",
      str_length(chapter) == 1 & chapter == "D" ~ "D. Dermatologicals",
      str_length(chapter) == 1 & chapter == "G" ~ "G. Genito urinary system and sex hormones",
      str_length(chapter) == 1 & chapter == "H" ~ "H. Systemic hormonal preparations, excluding sex hormones and insulins",
      str_length(chapter) == 1 & chapter == "J" ~ "J. Anti-infective for systemic use",
      str_length(chapter) == 1 & chapter == "L" ~ "L. Antineoplastic and immunomodulating agents",
      str_length(chapter) == 1 & chapter == "M" ~ "M. Musculo-skeletal system",
      str_length(chapter) == 1 & chapter == "N" ~ "N. Nervous systems",
      str_length(chapter) == 1 & chapter == "P" ~ "P. Antiparasitic products, insecticides and repellents",
      str_length(chapter) == 1 & chapter == "R" ~ "R. Respiratory system",
      str_length(chapter) == 1 & chapter == "S" ~ "S. Sensory organs",
      str_length(chapter) == 1 & chapter == "V" ~ "V. Various",
      # ICD
      str_length(chapter) == 3 & chapter >= "A00" & chapter <= "B99" ~ "A00-B99 Infectious and parasitic", # Certain infectious and parasitic diseases
      str_length(chapter) == 3 & chapter >= "C00" & chapter <= "D48" ~ "C00-D48 Neoplasms", # Neoplasms
      str_length(chapter) == 3 & chapter >= "D50" & chapter <= "D89" ~ "D50-D89 Blood, blood-forming organs", # Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism
      str_length(chapter) == 3 & chapter >= "E00" & chapter <= "E90" ~ "E00-E90 Endocrine, nutritional, metabolic", # : Endocrine, nutritional and metabolic diseases
      str_length(chapter) == 3 & chapter >= "F00" & chapter <= "F99" ~ "F00-F99 Mental & behavioural disorders", 
      str_length(chapter) == 3 & chapter >= "G00" & chapter <= "G99" ~ "G00-G99 Nervous system", # : Diseases of the nervous system
      str_length(chapter) == 3 & chapter >= "H00" & chapter <= "H59" ~ "H00-H59 Eye & adnexa", # : Diseases of the eye and adnexa
      str_length(chapter) == 3 & chapter >= "H60" & chapter <= "H95" ~ "H60-H95 Ear & mastoid process", # : Diseases of the ear and mastoid process
      str_length(chapter) == 3 & chapter >= "I00" & chapter <= "I99" ~ "I00-I99 Circulatory system", # : Diseases of the circulatory system
      str_length(chapter) == 3 & chapter >= "J00" & chapter <= "J99" ~ "J00-J99 Respiratory system", # : Diseases of the respiratory system
      str_length(chapter) == 3 & chapter >= "K00" & chapter <= "K93" ~ "K00-K93 Digestive system", # : Diseases of the digestive system
      str_length(chapter) == 3 & chapter >= "L00" & chapter <= "L99" ~ "L00-L99 Skin & subcutaneous tissue", # : Diseases of the skin and subcutaneous tissue
      str_length(chapter) == 3 & chapter >= "M00" & chapter <= "M99" ~ "M00-M99 Musculoskeletal system", # : Diseases of the musculoskeletal system and connective tissue
      str_length(chapter) == 3 & chapter >= "N00" & chapter <= "N99" ~ "N00-N99 Genitourinary system ", # : Diseases of the genitourinary system
      str_length(chapter) == 3 & chapter >= "O00" & chapter <= "O99" ~ "O00-O99 Pregnancy, childbirth & puerperium", 
      str_length(chapter) == 3 & chapter >= "P00" & chapter <= "P96" ~ "P00-P96 Conditions originating in the perinatal period", # : Certain conditions originating in the perinatal period
      str_length(chapter) == 3 & chapter >= "Q00" & chapter <= "Q99" ~ "Q00-Q99 Congenital malformations & deformations", # : Congenital malformations, deformations and chromosomal abnormalities
      str_length(chapter) == 3 & chapter >= "R00" & chapter <= "R99" ~ "R00-R99 Symptoms, signs & abnormal", # : Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified
      str_length(chapter) == 3 & chapter >= "S00" & chapter <= "T98" ~ "S00-T98 Injury, poisoning & external causes", # : Injury, poisoning and certain other consequences of external causes
      str_length(chapter) == 3 & chapter >= "V01" & chapter <= "Y98" ~ "V01-Y98 External causes of morbidity & mortality", #  External causes of morbidity and mortality
      str_length(chapter) == 3 & chapter >= "Z00" & chapter <= "Z99" ~ "Z00-Z99 Health status & contact with health services", # : Factors influencing health status and contact with health services
      str_length(chapter) == 3 & chapter >= "U00" & chapter <= "U85" ~ "U00-U85 Codes for special purposes",
      str_length(chapter) == 3 & chapter == "HPN" ~ "Heart Procedure, New (HPN)",
      str_length(chapter) == 3 & chapter == "HPO" ~ "Heart Procedure, Old (HPO)",
      str_length(chapter) == 3 & chapter == "FHL" ~ "Finnish Hospital League (FHL)",
      str_length(chapter) == 4 & chapter == "SPAT" ~ "Procedure (SPAT)",
      str_length(chapter) == 4 & chapter == "ICPC" ~ "Reason for visit (ICPC)",
      str_length(chapter) == 4 & chapter == "NCSP" ~ "Nordic Classification of Surgical Procedures (NCSP)",
      chapter == "ICD8or9" ~ "ICD8 or ICD9",
      SOURCE == "BIRTH" ~ "Birth Registry",
      SOURCE == "CANC" ~ "Cancer Registry",
      SOURCE == "REIMB" ~ "KELA Reimbursement",
      SOURCE == "DEATH" ~ "Death Registry",
      TRUE ~ "Unclassified"
    ))
  
  log_entry("register entries:", nrow(df_all))
  
  df_points <- df_all |> 
    mutate(SOURCE = factor(SOURCE)) |> 
    arrange(ORDER, desc(chapter), CLASSIFICATION) |> 
    arrange(desc(CLASSIFICATION == "Unclassified")) |> 
    mutate(y_order = row_number()) |> 
    mutate(CLASSIFICATION = fct_reorder(CLASSIFICATION, y_order)) |> 
    mutate(X_label = fct_reorder(str_wrap(CLASSIFICATION, 30), y_order)) |> 
    mutate(data_id = case_when(
      is.na(INDEX) | INDEX == '' ~ paste0(row_number(), '_BIRTH'),
      TRUE ~ INDEX
    ))
  
  df_points <- df_points |>
    mutate_at(c('name_en','name_en_top'), ~replace_na(.,"")) |> 
    mutate_at(c('provider_name_en','visit_type_name_en'), ~replace_na(.,"")) 
  
  # df_labels: grey category section labels within plot
  
  df_labels <- df_points |> 
    mutate(SECTION = case_when(
      CLASSIFICATION == "Unclassified" ~ "UNCLASSIFIEDS",
      SOURCE == "PURCH" ~ "MEDICATIONS (ATC)",
      SOURCE %in% c("REIMB", "CANC", "DEATH", "BIRTH") ~ "CERTIFIED DIAGNOSES",
      str_starts(vocabulary_id, "SPAT") ~ "PROCEDURES",
      str_starts(vocabulary_id, "ICPC") ~ "PROCEDURES",
      str_starts(vocabulary_id, "NCSP") ~ "PROCEDURES",
      str_starts(vocabulary_id, "HPN") ~ "PROCEDURES",
      str_starts(vocabulary_id, "HPO") ~ "PROCEDURES",
      str_detect(vocabulary_id, "ICD10fi") ~ "DIAGNOSES (ICD10)",
      str_detect(vocabulary_id, "ICD(8|9)fi") ~ "DIAGNOSES (ICD8, ICD9)",
      TRUE ~ ""
    )) |> 
    group_by(SECTION) |> 
    summarise(ymin = min(y_order, na.rm = TRUE),
              ymax = max(y_order, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(ymin = df_points$CLASSIFICATION[ymin]) |> 
    mutate(ymax = df_points$CLASSIFICATION[ymax]) |> 
    arrange(desc(ymin)) 
  
  df_labels$xmin <- min(df_points$APPROX_EVENT_DAY, na.rm = TRUE)
  df_labels$xmax <- max(df_points$APPROX_EVENT_DAY, na.rm = TRUE)
  
  # for marking the register start point
  
  df_register_start <- df_points |> 
    select(CLASSIFICATION, SOURCE) |> 
    distinct()
  
  df_register_start <- left_join(df_register_start,
                                 df_register_spans,
                                 by = "SOURCE")
  
  df_register_source_min <- df_points |> 
    group_by(SOURCE) |> 
    summarise(min_date = min(APPROX_EVENT_DAY)) |> 
    ungroup()
  
  df_register_start <- left_join(df_register_start,
                                 df_register_source_min,
                                 by = "SOURCE")

  df_register_start <- df_register_start |> 
    filter(interval(START_DATE, min_date) < years(2))
  
  values$df_height <- length(unique(df_all$CLASSIFICATION))
  values$df_labels <- df_labels
  values$df_register_start <- df_register_start
  values$df_points <- df_points
}

# event handling ####

server <- function(input, output, session){
  
  visited_persons <- NULL
  cohort <- NULL
  cohort_saved <- NULL
  person <- ""

  # reactive data
  values <- reactiveValues(
    df_all = NULL,
    df_points = NULL, 
    df_height = NULL,
    df_labels = NULL,
    df_register_start = NULL,
    df_minimum = NULL,
    df_selected = NULL,
    date_range = NULL
  )
  
  reset_values <- function(){
    log_entry("reset values")
    values$df_all <- NULL
    values$df_height <- NULL
    values$df_labels <- NULL
    values$df_register_start <- NULL
    values$df_minimum <- NULL
    values$df_points <- NULL
    values$df_selected <- NULL
    values$date_range <- NULL
  }
  
  get_all_data <- function(finngenid){
    log_entry("reading person:", finngenid)
    # sql <- paste0(
    #   "SELECT * ",
    #   "FROM ", longitudinal_data_table, " ",
    #   "WHERE FINNGENID = '", finngenid, "'"
    # )
    sql <- paste0(
      "SELECT * ",
      "FROM ", longitudinal_data_table, " ",
      "WHERE FINNGENID = '", finngenid, "' ",
      "UNION DISTINCT ",
      "SELECT " ,
      "MOTHER_FINNGENID AS FINNGENID, ",
      "'BIRTH', ",
      "MOTHER_AGE AS EVENT_AGE, ",
      "APPROX_BIRTH_DATE AS APPROX_EVENT_DAY, ",
      "SDIAG1 AS CODE1, ",
      "NULL, ",
      "NULL, ",
      "NULL, ",
      "NULL, ",
      "NULL, ",
      "NULL, ",
      "NULL, ",
      "NULL, ",
      "NULL, ",
      "NULL, ",
      "NULL ",
      "FROM ", birth_table, " ", 
      "WHERE MOTHER_FINNGENID = '", finngenid, "'"    
    )
    tb <- bq_project_query(projectid, sql, quiet = TRUE)
  }
  
  get_minimum_data <- function(finngenid){
    sql <- paste0(
      "SELECT * ",
      "FROM ", minimum_data_table, " ",
      "WHERE FINNGENID = '", finngenid, "'"
    )
    tb <- bq_project_query(projectid, sql, quiet = TRUE)
  }
  
  output$version <- renderText({
    return(paste0("v", VERSION))
  })
  
  #
  # upload cohort into person selector ####
  #
  
  output$upload_cohort_ui <- renderUI({
    fileInput('upload_cohort', label = 'Cohort',
              accept = c(
                'application/tsv',
                '.tsv'
              )
    )
  })
  
  observeEvent(input$upload_cohort, {
    log_entry("reading cohort:", input$upload_cohort$name)
    visited_persons <<- NULL
    tryCatch(
      {
        cohort <<- read_tsv(input$upload_cohort$datapath, show_col_types = FALSE)
      },
      warning = function(w) {
        showModalProgress("The file does not contain FINNGENID column")
      },
      error = function(e) {
        showModalProgress(paste("Error:", e))
      }
    )
    if("FINNGENID" %in% names(cohort)){
      cohort <<- cohort_saved <<- pull(cohort, FINNGENID)
      # unselect person, fill with FINNGENIDs from the cohort
      updateSelectizeInput(session, "person", 
                           choices = cohort,
                           selected = character(0),
                           options = list(multiple = FALSE, placeholder = 'Select ID', closeAfterSelect = TRUE),
                           server = TRUE
      )
    } else {
      showModalProgress("The file does not contain FINNGENID column")
    }
  }, ignoreInit = TRUE)
  
  #
  # "Hide visited" checkbox
  #
  
  observeEvent(input$hide_visited, {
    if(input$hide_visited == TRUE){
      # from FALSE -> TRUE
      cohort <<- setdiff(cohort_saved, visited_persons)
    } else {
      # from TRUE -> FALSE
      cohort <<- cohort_saved
    }
    updateSelectizeInput(session, "person", choices = cohort, selected = character(0), server = TRUE)
  })
  
  #
  # fetch all data for a person ####
  #
  
  observeEvent(input$person, {
    # exit if person is not selected
    if(input$person == "") 
      return()
    
    # check if we are trying to reload the same person
    if(input$person == person){
      updateSelectizeInput(session, "person", selected = character(0))
      showModalProgress(paste(person, "already loaded!"))
      return()
    }
    shinyjs::runjs("window.scrollTo(0, 0)")
    
    person <<- input$person
    
    visited_persons <<- c(visited_persons, person)
    if(input$hide_visited){
      cohort <<- setdiff(cohort, visited_persons)
      updateSelectizeInput(session, "person", choices = cohort, selected = character(0), server = TRUE)
    } else {
      updateSelectizeInput(session, "person", selected = character(0))
    }

    # clear prior data, will erase graphics
    reset_values()
    
    showModalProgress(paste0("Reading data for ", person, "..."), footer = NULL)
    
    #
    # code translations ####
    #
    tb_saved <- tb <- get_all_data(person)
    
    df <- fg_bq_append_code_info_to_longitudinal_data(
      projectid, tb,
      fg_codes_info_table,
      ICD10fi_precision = 3,
      ICD9fi_precision = 3,
      ICD8fi_precision = 3,
      ATC_precision = 3,
      new_colums_sufix = "_code3"
    )
    
    df_all_code3 <- bq_table_download(df, quiet = TRUE) 
    tb <- tb_saved
    
    df <- fg_bq_append_code_info_to_longitudinal_data(
      projectid, tb,
      fg_codes_info_table,
      ICD10fi_precision = 5,
      ICD9fi_precision = 5,
      ICD8fi_precision = 5,
      ATC_precision = 7,
      new_colums_sufix = "_code5"
    )
    
    df_all_code5 <- bq_table_download(df, quiet = TRUE)

    df_all <- left_join(
      df_all_code5, 
      select(df_all_code3, CODE1, CATEGORY, INDEX, name_en_code3),
      by = c("CODE1", "CATEGORY", "INDEX")
    ) 

    #
    # provider translations ####
    #
    
    tb <- tb_saved
    
    df <- fg_bq_append_provider_info_to_service_sector_data(
      projectid, 
      tb,
      fg_codes_info_table,
      new_colums_sufix = ""
    )
    
    df_all_provider <- bq_table_download(df, quiet = TRUE) 

    df_all <- left_join(
      df_all, 
      select(df_all_provider, CODE1, CATEGORY, INDEX, provider_name_en),
      by = c("CODE1", "CATEGORY", "INDEX")
    ) 
    
    #
    # visit_type translations ####
    #
    
    tb <- tb_saved
    
    df <- fg_bq_append_visit_type_info_to_service_sector_data(
      projectid, 
      tb,
      fg_codes_info_table,
      new_colums_sufix = ""
    )
    
    df_all_provider <- bq_table_download(df, quiet = TRUE) 
    
    df_all <- left_join(
      df_all, 
      select(df_all_provider, CODE1, CATEGORY, INDEX, visit_type_name_en),
      by = c("CODE1", "CATEGORY", "INDEX")
    ) 
    
    # check if the FINNGENID is in the data
    if(nrow(df_all) == 0){
      showModalProgress("No such FINNGENID in the database!")
      updateSelectizeInput(session, "person", selected = character(0))
      reset_values()
      return(NULL)
    }   
    
    if(nrow(df_all) != nrow(df_all_provider)){
      message("join failed, the primary key is false")
      browser()
    }
    
    df_timespan <- df_all %>% 
      summarise(DATE_MIN = min(APPROX_EVENT_DAY, na.rm = TRUE), DATE_MAX = max(APPROX_EVENT_DAY, na.rm = TRUE))
    updateSliderInput(session, "date_range", 
                      min = df_timespan$DATE_MIN, 
                      max = df_timespan$DATE_MAX,
                      value = c(df_timespan$DATE_MIN, df_timespan$DATE_MAX)
    )
    values$date_range <- c(df_timespan$DATE_MIN, df_timespan$DATE_MAX)
 
    # clean up the translations
    df_all <- df_all |> 
      mutate(name_en = str_to_sentence(name_en_code5)) |> 
      mutate(name_en_top = str_to_sentence(name_en_code3)) |> 
      mutate(name_en = str_replace(name_en, "\\[.*\\]", ""))
    
    df_minimum <- bq_table_download(get_minimum_data(input$person),quiet = TRUE) 

    values$df_minimum <- df_minimum
    
    df_all <- left_join(df_all, select(df_minimum, FINNGENID, SEX, APPROX_BIRTH_DATE), by = "FINNGENID") 
    df_all <- left_join(df_all, select(df_register_spans, SOURCE, COLOR), by = "SOURCE")
    
    #
    # get code prevalences ####
    #
    # - download only the omop codes the person has
    #
    omop_codes <- toString(df_all$omop_concept_id_code5 |> na.omit())
    sql <- paste0(
      "SELECT * ",
      "FROM ", code_prevalence_table, " ",
      "WHERE sex = '", values$df_minimum$SEX, "' ",
      "AND year_of_birth = ", year(values$df_minimum$APPROX_BIRTH_DATE), " ",
      "AND source_concept_id IN (", omop_codes, ")" # source_concept_id/omop_concept_id
    )
    tb <- bq_project_query(projectid, sql, quiet = TRUE)
    df_prevalence <- bq_table_download(tb, quiet = TRUE) |> 
      rename(omop_concept_id_code5 = source_concept_id) |> # source_concept_id/omop_concept_id
      rename(SEX = sex) |> 
      mutate(p = ifelse(is.na(n_persons_in_observation), NA, n_persons_with_code / n_persons_in_observation)) |> 
      mutate(std = ifelse(is.na(n_persons_in_observation), NA, sqrt(p * (1 - p) / n_persons_in_observation)))
    
    df_all <- df_all |> 
      mutate(age_decile = floor(round(EVENT_AGE)/10) * 10) |> 
      mutate(omop_concept_id_code5 = as.integer(omop_concept_id_code5)) |> 
      mutate(year_of_birth = year(APPROX_BIRTH_DATE))
    
    # browser()

    if(nrow(df_prevalence) == 0){
      # no prevalence data, should happen only with atlas_dev?
      df_all$n_persons_with_code <- NA
      df_all$n_persons_in_observation <- NA
    } else {   
      # augment df_all with code prevalences
      df_all <- left_join(
        df_all, 
        df_prevalence, 
        by = c("omop_concept_id_code5", "age_decile", "SEX", "year_of_birth")
      )
    }
    
    # mutate prevalence to 0..1, missing data coded 1
    df_all <- df_all |> 
      mutate(prevalence = case_when(
        !is.na(n_persons_with_code) & !is.na(n_persons_in_observation) ~ (n_persons_with_code / n_persons_in_observation),
        TRUE ~ NA
      ))
    
    # build the points
    build_plot_values(df_all, values)
    values$df_all <- df_all
    
    # apply filters to fetched data
    
    class_regexp <- str_trim(input$class_regexp)
    if(class_regexp != ""){
      log_entry("got regexp on class")
      df_all <- left_join(
        df_all, 
        select(values$df_points, CODE1, CATEGORY, INDEX, CLASSIFICATION),
        by = c("CODE1", "CATEGORY", "INDEX")
      )
      
      df_all <- df_all |>
        filter(str_detect(CLASSIFICATION, regex(class_regexp, ignore_case = TRUE)))
      
      df_all$CLASSIFICATION <- NULL
      build_plot_values(df_all, values)
    }
    
    entry_regexp <- str_trim(input$entry_regexp)
    if(entry_regexp == "") {
      values$df_selected <- NULL
    } else {
      log_entry("got regexp on entry")
      df_selected <- values$df_points |> 
        filter(
          str_detect(CODE1, regex(entry_regexp, ignore_case = TRUE)) |
            str_detect(name_en, regex(entry_regexp, ignore_case = TRUE)) |
            str_detect(visit_type_name_en, regex(entry_regexp, ignore_case = TRUE))
        )
      
      values$df_selected <- df_selected
    }

    removeModal()
  }, ignoreInit = TRUE)
  
  #
  # filter_data ####
  #
  
  observeEvent(input$filter_data, {
    log_entry("filter changed")

    if(is.null(values$df_all)) return()
    
    shinyjs::runjs("window.scrollTo(0, 0)")
    
    # filter according to date range
    df_all <- values$df_all |> 
      filter(APPROX_EVENT_DAY >= input$date_range[1] & APPROX_EVENT_DAY <= input$date_range[2])
    if(nrow(df_all) != nrow(values$df_all))
      build_plot_values(df_all, values)
    
    class_regexp <- str_trim(input$class_regexp)
    if(class_regexp != ""){
      log_entry("got regexp on class")
      df_all <- left_join(
        df_all, 
        select(values$df_points, CODE1, CATEGORY, INDEX, CLASSIFICATION),
        by = c("CODE1", "CATEGORY", "INDEX")
      )
      
      df_all <- df_all |>
        filter(str_detect(CLASSIFICATION, regex(class_regexp, ignore_case = TRUE)))

      df_all$CLASSIFICATION <- NULL
      build_plot_values(df_all, values)
    }
    
    entry_regexp <- str_trim(input$entry_regexp)
    if(entry_regexp == "") {
      values$df_selected <- NULL
    } else {
      log_entry("got regexp on entry")
      df_selected <- values$df_points |> 
        filter(
          str_detect(CODE1, regex(entry_regexp, ignore_case = TRUE)) |
            str_detect(name_en, regex(entry_regexp, ignore_case = TRUE)) |
            str_detect(visit_type_name_en, regex(entry_regexp, ignore_case = TRUE))
        )
      
      values$df_selected <- df_selected
      
      # warn if none was found
      if(nrow(df_selected) == 0){
        showModalProgress(
          paste0("No entries with \"", entry_regexp, "\" found!"))
      }
    }
  }, ignoreInit = TRUE)
  
  #
  # reset_filter ####
  #
  
  observeEvent(input$reset_filter, {
    
    log_entry("filter reset")
    
    updateTextInput(session, "entry_regexp", value = "")
    updateTextInput(session, "class_regexp", value = "")
    updateSliderInput(session, "date_range", value = values$date_range)

    if(is.null(values$df_all)) return()
    
    build_plot_values(values$df_all, values)
    
    values$df_selected <- NULL
    shinyjs::runjs("window.scrollTo(0, 0)")
    
  }, ignoreInit = TRUE)
  
  #
  # prevalence ####
  #
  
  observeEvent(input$prevalence, {
    shinyjs::runjs("window.scrollTo(0, 0)")
  }, ignoreInit = TRUE)
  
  #
  # selection to table ####
  #
  
  observeEvent(input$mainplot_selected, {
    
    # remove the previous selection
    session$sendCustomMessage(type = 'mainplot_set', message = character(0))
    # get selected points from girafe
    selected_rows <- input$mainplot_selected
    
    # if any of the highlighted values are in selected, extend to all of them
    if(!is.null(values$df_selected) && any(selected_rows %in% values$df_selected$INDEX)){ 
      selected_rows <- values$df_selected$INDEX
    }
    
    df_lasso <- values$df_points |> 
      filter(data_id %in% selected_rows) 
      
    # if only one date is selected -> extend selection
    # by +/- input$selection_extension
    if(length(unique(df_lasso$APPROX_EVENT_DAY)) == 1){
      event_date <- first(df_lasso$APPROX_EVENT_DAY)
      extension <- isolate(input$selection_extension)
      selection_start <- event_date - months(extension)
      selection_end <- event_date + months(extension)
      selected_points <- values$df_points |> 
        filter(APPROX_EVENT_DAY >= selection_start & APPROX_EVENT_DAY <= selection_end )
    } else {
      selected_points <- values$df_points |> 
        filter(data_id %in% selected_rows)
    }
    
    df_lasso <- df_lasso |> 
      mutate(prevalence = round(100 * prevalence,1)) |> 
      mutate(prevalence_ratio = paste0(n_persons_with_code, "/", n_persons_in_observation)) |> 
      mutate(std = round(std,3)) |> 
      select(
        visit_type_name_en, 
        APPROX_EVENT_DAY, 
        CODE1, 
        prevalence, 
        prevalence_ratio,
        std,
        name_en
      )
    
    showModal(
      modalDialog(
        DT::renderDataTable({ 
          DT::datatable(
            df_lasso,
            colnames = c(
              'Event date' = 'APPROX_EVENT_DAY',
              'Code' = 'CODE1',
              'Translation' = 'name_en',
              'Prev%' = 'prevalence',
              'Prev' = 'prevalence_ratio',
              'STD' = 'std',
              'Service sector' = 'visit_type_name_en'
            )
          )
        }),
        size = "l",
        easyClose = FALSE,
        title = paste0("Entries (", nrow(df_lasso), ")"),
        footer = modalButton("Close"),
        options = list(
          autowidth = TRUE
        )
      )
    )
  })
  
  #
  # render the plot ####
  #
  
  output$mainplot <- renderGirafe({
    
    req(values$df_points, values$df_labels)
    log_entry("generating SVG")
    
    min_date <- min(values$df_points$APPROX_EVENT_DAY, na.rm = TRUE)
    max_date <- max(values$df_points$APPROX_EVENT_DAY, na.rm = TRUE)
    interval_months <- interval(min_date, max_date)/months(1)
    dotsize <- 0.485 + 0.022 * interval_months
    
    log_entry("interval_months", interval_months)
    log_entry("dotsize", dotsize + input$dotsize)
    
    # make selected points bright, if no selection then all are bright
    # selection can originate from regex or prevalence
    df_points <- values$df_points |> 
      rowwise() |> 
      mutate(alpha = ifelse(is.null(values$df_selected) | INDEX %in% values$df_selected$INDEX, "bright", "dim")) |>
      mutate(stroke = case_when(
        input$prevalence & !is.na(prevalence) & prevalence <= 0.01 ~ 0.3,
        input$prevalence & !is.na(prevalence) & prevalence > 0.01 & prevalence <= 0.05 ~ 1.0,
        input$prevalence & !is.na(prevalence) & prevalence > 0.05 & prevalence < 0.10 ~ 2.5,
        !input$prevalence ~ 0.5,
        TRUE ~ 4.0
      ))
      # mutate(stroke = ifelse(input$prevalence & !is.na(prevalence), 0.5 + 3 * sqrt(prevalence), 1.0))
      
    # View(select(df_points, 1:3, prevalence, stroke))
    # browser()
    
    gg_plot <- ggplot() +
      geom_dotplot_interactive(
        data = df_points, 
        binwidth = input$binwidth,
        method = input$method,
        dotsize = dotsize + input$dotsize,
        position = input$position,
        binaxis = 'y',
        binpositions = 'all',
        stackdir = input$stackdir,
        stackratio = input$stackratio,
        stackgroups = TRUE,
        color = ifelse(input$prevalence, "white", "black"),
        # color = "white",
        aes(
          x = CLASSIFICATION, y = APPROX_EVENT_DAY,
          alpha = alpha,
          stroke = stroke,
          fill = SOURCE,
          # color = SOURCE,
          tooltip = paste0(APPROX_EVENT_DAY, "\n",
                           SOURCE, "\n",
                           "CODE : ", CODE1, "\n",
                           "VOCABULARY : ", vocabulary_id, "\n",
                           "PREVALENCE : ", ifelse(is.na(prevalence), NA, round(prevalence * 100, 2)), "%\n",
                           "PREV.RATIO : ", n_persons_with_code, "/", n_persons_in_observation, "\n",
                           "STD : ", round(std, 3), "\n",
                           "CAT  : ", CATEGORY, "\n",
                           "AGE : ", EVENT_AGE, "\n\n",
                           str_wrap(name_en, 30), "\n\n",
                           ifelse(name_en != name_en_top & str_length(name_en_top),
                                  paste("-", str_wrap(name_en_top, 30), "\n\n"), ""),
                           "- ", X_label, "\n\n",
                           ifelse(!is.na(provider_name_en) & str_length(provider_name_en), paste(provider_name_en, "\n"), ""),
                           ifelse(!is.na(visit_type_name_en) & str_length(visit_type_name_en),
                                  paste(str_wrap(visit_type_name_en, 30)), "")
          ),
          data_id = data_id # paste0(CLASSIFICATION, "---", APPROX_EVENT_DAY)
        ),
        hover_nearest = TRUE) + # requires data_id
      geom_spoke(
        data = values$df_register_start,
        aes(x = CLASSIFICATION, y = START_DATE, angle = 0, radius = 0.4, color = SOURCE), alpha = 0.5
      ) +
      geom_spoke(
        data = values$df_register_start,
        aes(x = CLASSIFICATION, y = START_DATE, angle = 180, radius = 0.5, color = SOURCE), alpha = 0.5
      ) +
      geom_text(data = values$df_labels, 
                aes(x = ymax, y = xmin, label = SECTION),
                hjust= 0.0, vjust = -1, color = "gray", alpha = 0.5, size = 3,
                show.legend = FALSE
      ) +
      scale_fill_manual(name = "SOURCE", values = register_colors) +
      scale_color_manual(name = "SOURCE", values = register_colors) +
      scale_alpha_manual(values = c("bright" = 1.0, "dim" = 0.2, "zero" = 0.0)) +
      scale_y_date(breaks = "5 years", date_minor_breaks = "1 years", date_labels = "%Y") +
      scale_x_discrete(labels = str_trunc(levels(values$df_points$X_label), 60), expand = expansion(add = 1.5)) +
      coord_flip() +
      theme_light() +
      theme(
        text = element_text(family = "sans", face = "plain"), # Helvetica
        plot.title = element_text(size = 6),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(7, "mm"),
        legend.justification = "top",
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6),
        axis.title.x = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.y = element_text(size = 5),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(linewidth = 0.2)
      ) +
      labs(
        title = paste(
          person, 
          values$df_minimum$SEX, 
          paste0(year(values$df_minimum$APPROX_BIRTH_DATE)),
          paste0(values$df_minimum$HEIGHT, "cm"), 
          paste0("smoke:", values$df_minimum$SMOKE2, "/", 
                 values$df_minimum$SMOKE3, "/", 
                 values$df_minimum$SMOKE5
          ), 
          paste0("offspring: ", values$df_minimum$NUMBER_OF_OFFSPRING),
          paste0(values$df_minimum$regionofbirthname),
          paste0("\n", nrow(values$df_all), " entries; period: ", values$date_range[1], " — ", values$date_range[2]),
          sep = "; "
        ),
        x = "", # "CLASS"
        y = ""  # "DATE"
      ) +
      guides(color = "none", alpha = "none")
    
    gg_girafe <- girafe(ggobj = gg_plot, 
           height_svg = (12 * input$x_factor) * values$df_height / 44 + 1, 
           width_svg = 6,
           options = list(
             opts_selection(
               type = "multiple",
               css = "fill:black;stroke:gray;r:5pt;"
             ),
             opts_hover(
               css = "fill:none;opacity:1.0;stroke:red;r:2pt;"
             ),
             opts_toolbar(
               saveaspng = FALSE,
               hidden = c('lasso_deselect', 'zoom_onoff')
             )
           )
    )
    log_entry("return SVG object")
    return(gg_girafe)
  })
  
  # handle window close ####
  onStop(function() {
    log_entry("window close")
    # remove global vars
    # rm(df_prevalence, df_register_spans, envir = .GlobalEnv)
    stopApp()
  }, session)
  
}


