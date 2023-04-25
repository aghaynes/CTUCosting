# lookup tables

rateopts <- tibble::tribble(
~code, ~label,
1, "Internal",
2, "External non-profit",
3, "External for-profit",
4, "SNF",
)


servicenames <- tibble::tribble(
  ~service, ~Service,                    ~form,
  "docdev", "Document development",      "document_development",
  "dml",    "Data management (light)",   "dm_redcap_light",
  "dmf",    "Data management (full)",    "dm_full_services",
  "sta",    "Statistics",                "statistics",
  "csm",    "Clinical study management", "clinical_study_management",
  "qm",     "Quality management",        "quality_management",
  "ci",     "Clinical investigation",    NA,
  "admin",  "Administration",            NA,
  "sen",    "Seniors",                   NA,
  "rs",     "Regulatory services",       "regulatory_support",
  "sw",     "Study website",             "study_website",
  "cloud",  "ShareFile cloud",           "sharefile_cloud",
  "mon",    "Monitoring",                "monitoring_onsite_remote",
  "cdm",    "Central data monitoring",   "central_data_monitoring",
  "cdmra",  "Central data monitoring",   NA,
  "ra",     "Research assistant",        NA
)


ratenames <- tibble::tribble(
  ~service, ~rate_name,
  "docdev", "rate_mon",
  "dml",    "rate_dm",
  "dmf",    "rate_dm",
  "dm",     "rate_dm",
  "sta",    "rate_sta",
  "csm",    "rate_csm",
  "qm",     "rate_qm",
  "ci",     "rate_ci",
  "admin",  "rate_admin",
  "sen",    "rate_senior",
  "rs",     "rate_mon",
  "sw",     "rate_dm",
  "cloud",  "rate_dm",
  "mon",    "rate_mon",
  "cdm",    "rate_mon",
  "ra",     "rate_ra",
  "cdmra",  "rate_ra"

)
