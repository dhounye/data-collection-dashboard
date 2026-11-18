# Live data: reading the forms directly from SurveyCTO
#
# The dashboard reads SurveyCTO when the environment variable SCTO_SERVER is defined.
# Otherwise it uses the simulated data of the folder "data".
#
# The connection details are never written in the code. They are stored in
# environment variables, for example in a file named ".Renviron" placed in the
# folder of the app (this file must not be uploaded to GitHub):
#
#   SCTO_SERVER=myserver                 (for https://myserver.surveycto.com)
#   SCTO_USER=name@organisation.org
#   SCTO_PASSWORD=...
#   SCTO_FORM_INTERVIEWS=household_survey
#   SCTO_FORM_DAILY_REPORTS=daily_report         (optional)
#   SCTO_FORM_BACK_CHECKS=back_check             (optional)
#   SCTO_PRIVATE_KEY=path/to/private_key.pem     (only if the forms are encrypted)
#
# The SurveyCTO user needs the permission to use the API ("Allow server API access").
# Requires the package rsurveycto: install.packages("rsurveycto")

use_surveycto <- function() nzchar(Sys.getenv("SCTO_SERVER"))

# ---------------------------------------------------------------------------
# Names of the fields
# ---------------------------------------------------------------------------
# On the left: the name used by the dashboard. On the right: the name of the field
# in the SurveyCTO form. Only the names on the right must be changed to fit a form.

fields_interviews <- c(
  interview_id     = "KEY",
  household_id     = "hhid",
  start            = "starttime",
  end              = "endtime",
  enumerator       = "enumerator",
  supervisor       = "supervisor",
  department       = "department",
  commune          = "commune",
  village          = "village",
  latitude         = "gps-Latitude",
  longitude        = "gps-Longitude",
  consent          = "consent",
  spot_check       = "spot_check",
  respondent_sex   = "respondent_sex",
  respondent_age   = "respondent_age",
  household_size   = "household_size",
  children_under_5 = "children_under_5",
  owns_phone       = "owns_phone",
  food_expenditure = "food_expenditure",
  water_source     = "water_source"
)

fields_daily_reports <- c(
  date               = "report_date",
  enumerator         = "enumerator",
  supervisor         = "supervisor",
  commune            = "commune",
  village            = "village",
  planned_interviews = "planned_interviews",
  transport          = "transport",
  road_condition     = "road_condition",
  weather            = "weather",
  call_mtn           = "call_mtn",
  call_moov          = "call_moov",
  call_celtiis       = "call_celtiis",
  internet_mtn       = "internet_mtn",
  internet_moov      = "internet_moov",
  internet_celtiis   = "internet_celtiis",
  incident           = "incident",
  incident_type      = "incident_type",
  incident_reported  = "incident_reported"
)

fields_back_checks <- c(
  interview_id        = "interview_key",
  back_check_date     = "back_check_date",
  back_checker        = "back_checker",
  household_size_bc   = "household_size",
  respondent_age_bc   = "respondent_age",
  owns_phone_bc       = "owns_phone",
  children_under_5_bc = "children_under_5"
)

numeric_fields <- c("latitude", "longitude", "respondent_age", "household_size", "children_under_5",
                    "food_expenditure", "planned_interviews", "household_size_bc",
                    "respondent_age_bc", "children_under_5_bc")

# ---------------------------------------------------------------------------
# Reading
# ---------------------------------------------------------------------------

# Read all the submissions of one form, approved or waiting for review
read_surveycto_form <- function(form_id) {
  if (!requireNamespace("rsurveycto", quietly = TRUE)) {
    stop("The package rsurveycto is needed to read SurveyCTO: install.packages(\"rsurveycto\")")
  }
  auth <- rsurveycto::scto_auth(servername = Sys.getenv("SCTO_SERVER"),
                                username   = Sys.getenv("SCTO_USER"),
                                password   = Sys.getenv("SCTO_PASSWORD"))
  private_key <- Sys.getenv("SCTO_PRIVATE_KEY")
  data <- rsurveycto::scto_read(auth, ids = form_id,
                                review_status = c("approved", "pending"),
                                private_key = if (nzchar(private_key)) private_key else NULL)
  as.data.frame(data)
}

# Keep the fields used by the dashboard and give them the names of the dashboard
rename_fields <- function(data, fields) {
  missing_fields <- setdiff(fields, names(data))
  if (length(missing_fields) > 0) {
    stop("Fields not found in the SurveyCTO form: ", paste(missing_fields, collapse = ", "),
         ". Correct the names in surveycto.R.")
  }
  data <- data[, fields, drop = FALSE]
  names(data) <- names(fields)
  for (field in intersect(names(data), numeric_fields)) data[[field]] <- as.numeric(data[[field]])
  data
}

# Interviews: the date, the start time and the duration come from the start and end times
surveycto_interviews <- function(raw) {
  interviews <- rename_fields(raw, fields_interviews)
  start <- as.POSIXct(interviews$start, tz = "UTC")
  end   <- as.POSIXct(interviews$end, tz = "UTC")
  interviews$date         <- as.character(as.Date(start))
  interviews$start_time   <- format(start, "%H:%M")
  interviews$duration_min <- round(as.numeric(difftime(end, start, units = "mins")))
  interviews$start <- NULL
  interviews$end   <- NULL
  interviews
}

# Read the three forms. The daily reports and the back checks are optional.
read_surveycto_data <- function() {
  optional_form <- function(variable, fields) {
    form_id <- Sys.getenv(variable)
    if (!nzchar(form_id)) return(NULL)
    rename_fields(read_surveycto_form(form_id), fields)
  }
  list(
    interviews    = surveycto_interviews(read_surveycto_form(Sys.getenv("SCTO_FORM_INTERVIEWS"))),
    daily_reports = optional_form("SCTO_FORM_DAILY_REPORTS", fields_daily_reports),
    back_checks   = optional_form("SCTO_FORM_BACK_CHECKS", fields_back_checks)
  )
}
