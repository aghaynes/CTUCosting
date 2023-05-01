# lookup tables

rateopts <- tibble::tribble(
~code, ~label,
1, "Internal",
2, "External non-profit",
3, "External for-profit",
4, "SNF",
)


servicenames <- tibble::tribble(
  ~service, ~Service,                    ~div ,   ~form,
  "docdev", "Document development",      "MON",   "document_development",
  "dml",    "Data management (light)",   "DM",    "dm_redcap_light",
  "dmf",    "Data management (full)",    "DM",    "dm_full_services",
  "sta",    "Statistics",                "STA",   "statistics",
  "csm",    "Clinical study management", "CSM",   "clinical_study_management",
  "qm",     "Quality management",        "QM",    "quality_management",
  "ci",     "Clinical investigation",    "CI",    NA,
  "admin",  "Administration",            "ADMIN", NA,
  "sen",    "Seniors",                   "SEN",   NA,
  "rs",     "Regulatory services",       "MON",   "regulatory_support",
  "sw",     "Study website",             "DM",    "study_website",
  "cloud",  "ShareFile cloud",           "DM",    "sharefile_cloud",
  "mon",    "Monitoring",                "MON",   "monitoring_onsite_remote",
  "cdm",    "Central data monitoring",   "MON",   "central_data_monitoring",
  "cdmra",  "Central data monitoring",   "MON",   "central_data_monitoring",
  "ra",     "Research assistant",        NA,      NA
)

divnames <- tibble::tribble(
  ~div, ~division,
  "ADMIN", "Administration",
  "CI", "Clinical Investigation",
  "CSM", "Clinical Study Management",
  "DM", "Data Management",
  "MON", "Monitoring",
  "QM", "Quality Management",
  "SEN", "Seniours",
  "STA", "Statistics",
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
