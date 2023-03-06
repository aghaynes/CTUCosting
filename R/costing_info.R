# costing information
#' @importFrom dplyr pull
#' @export
costing_info <- function(data, metadata){
  data <- data$meta_information
  # data <- d$meta_information
  initcosting <- data$initial_costing == 1
  projnum <- data$initial_projnum
  consultingnum <- data$consulting_num
  studyname <- data$study

  title_txt <- paste("Costing for consulting",
                     ifelse(is.na(consultingnum),
                            "[not a consulting]",
                            consultingnum),
                     ":", studyname)

  init_or_amendment_txt <- ifelse(initcosting,
                                  "This is the intial costing for the project",
                                  paste("This is an amendment to project", projnum))

  ratelab <- rateopts$label[data$rate]

  duration <- data$study_duration

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

  design <- specific_option(metadata, data, "study_design")
  int <- specific_option(metadata, data, "int_type")

  n_part <- data$n_participants
  n_sites <- data$n_sites
  location <- specific_option(metadata, data, "site_location")
  rate <- specific_option(metadata, data, "rate")
  internal <- data$sponsor_insel == 1


  return(
    list(
      title_txt = title_txt,
      initcosting = initcosting,
      projnum = projnum,
      consultingnum = consultingnum,
      studyname = studyname,
      acronym = data$study_abbr,
      sponsor = inst,
      contact = data$sponsor,
      sign = sign,
      init_or_amendment_txt = init_or_amendment_txt,
      ratelab = ratelab,
      duration = duration,
      design = design,
      intervention = int,
      participants = n_part,
      sites = n_sites,
      location = location,
      internal = internal,
      discount_db = ifelse(initcosting, data$discount, data$discount2)
    )
  )

}
