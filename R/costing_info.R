# costing information
#' @importFrom dplyr pull
#' @export
costing_info <- function(data, metadata){
  data <- data$meta_information
  # data <- d$meta_information
  consultingnum <- data$consulting_num
  studyname <- data$study
  initcosting <- data$initial_costing == 1
  projnum <- data$initial_projnum

  title_txt <- paste("Costing for consulting",
                     ifelse(is.na(consultingnum),
                            "[not a consulting]",
                            consultingnum),
                     ":", studyname)

  init_or_amendment_txt <- ifelse(initcosting,
                                  "This is the intial costing for the project",
                                  paste("This is an amendment to project", projnum))

  # institute/company
  print(data$sponsor_insel)
  insel <- data$sponsor_insel == 1
  if(insel){
    inst <- specific_option(metadata, data, "institute")
    sign <- clinic_heads %>%
      filter(Clinic == inst) %>%
      pull(Head.of.clinic)
  } else {
    inst <- data$company
    sign <- data$institute_auth
  }

  rate <- specific_option(metadata, data, "rate")
  internal <- data$sponsor_insel == 1

  # DLF
  # LOGIC FOR DETERMINING WHETHER DLF FUNDING IS RELEVANT
  dlf <- FALSE

  fn <- function(m, d, v){
    ifelse(!is.na(d[, v]),
           specific_option(m, d, v),
           NA)
  }

  return(
    list(
      # project
      title_txt =             title_txt,
      projnum =               projnum,
      consultingnum =         consultingnum,
      studyname =             studyname,
      acronym =               data$study_abbr,
      initcosting =           initcosting,
      init_or_amendment_txt = init_or_amendment_txt,
      ratelab =               rateopts$label[data$rate],
      dlf =                   dlf,
      # signatories
      sponsor =               inst,
      contact =               data$sponsor,
      sign =                  sign,
      # design
      design =                fn(metadata, data, "study_design"),
      design_src =            fn(metadata, data, "study_design_src"),
      # durations
      duration =              data$study_duration,
      duration_src =          fn(metadata, data, "study_duration_src"),
      duration_enrol =        data$study_enrol,
      duration_enrol_src =    fn(metadata, data, "study_enrol_src"),
      # participants/sites
      participants =          data$n_participants,
      participants_src =      fn(metadata, data, "n_participants_src"),
      sites =                 data$n_sites,
      sites_src =             fn(metadata, data, "n_sites_src"),
      location =              fn(metadata, data, "site_location"),
      location_src =          fn(metadata, data, "site_location_src"),
      # visits
      n_visits =              data$n_visits,
      n_visits_src =          fn(metadata, data, "n_visits_src"),
      # variables/database
      n_vars =                data$n_vars,
      n_vars_src =            fn(metadata, data, "n_vars_src"),
      n_database =            data$n_db,
      n_database_src =        fn(metadata, data, "n_db_src"),

      intervention =          fn(metadata, data, "int_type"),
      internal =              data$sponsor_insel == 1,
      discount_db =           ifelse(initcosting, data$discount, data$discount2),
      costing_txt =           data$costing_txt
    )
  )

}
