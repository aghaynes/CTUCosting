# lookup tables

rateopts <- tibble::tribble(
~code, ~label,
1, "Internal",
2, "External non-profit",
3, "External for-profit",
4, "SNF",
)


servicenames <- tibble::tribble(
  ~service, ~Service,
  "docdev", "Document development",
  "dml",    "Data management (light)",
  "dmf",    "Data management (full)",
  "sta",    "Statistics",
  "csm",    "Clinical study management",
  "qm",     "Quality management",
  "ci",     "Clinical investigation",
  "admin",  "Administration",
  "sen",    "Seniors",
  "rs",     "Regulatory services",
  "sw",     "Study website",
  "cloud",  "ShareFile cloud",
  "mon",    "Monitoring",
  "cdm",    "Central data monitoring",
  "cdmra",    "Central data monitoring",
  "ra",     "Research assistant"
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
