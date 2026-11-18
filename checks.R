# Data quality checks and alerts used by the dashboard
#
# The thresholds are defined in config.R.

# Questions of the questionnaire and their labels
question_labels <- c(
  respondent_sex   = "Sex of the respondent",
  respondent_age   = "Age of the respondent",
  household_size   = "Household size",
  children_under_5 = "Children under 5",
  owns_phone       = "Owns a mobile phone",
  food_expenditure = "Monthly food expenditure",
  water_source     = "Main source of water"
)

operators <- c(mtn = "MTN", moov = "Moov", celtiis = "Celtiis")

percent <- function(x) paste0(round(100 * x, 1), "%")

# ---------------------------------------------------------------------------
# Periods
# ---------------------------------------------------------------------------

# First and last dates of a period that ends on the reference date
period_dates <- function(period, reference_date, first_date) {
  reference_date <- as.Date(reference_date)
  start <- switch(
    period,
    "Day"             = reference_date,
    "Week to date"    = reference_date - (as.integer(format(reference_date, "%u")) - 1),   # since Monday
    "Month to date"   = as.Date(format(reference_date, "%Y-%m-01")),
    "Since the start" = as.Date(first_date)
  )
  c(start, reference_date)
}

# ---------------------------------------------------------------------------
# Preparation of the interviews
# ---------------------------------------------------------------------------

# Distance in kilometres between two GPS points (haversine formula)
distance_km <- function(lat1, lon1, lat2, lon2) {
  to_radians <- pi / 180
  d_lat <- (lat2 - lat1) * to_radians
  d_lon <- (lon2 - lon1) * to_radians
  a <- sin(d_lat / 2)^2 + cos(lat1 * to_radians) * cos(lat2 * to_radians) * sin(d_lon / 2)^2
  6371 * 2 * asin(sqrt(a))
}

# Outliers of a positive variable such as an expenditure: values whose logarithm is
# more than 3 median absolute deviations away from the median
is_outlier <- function(x) {
  log_x <- log(x)
  centre <- median(log_x, na.rm = TRUE)
  spread <- mad(log_x, na.rm = TRUE)
  !is.na(x) & abs(log_x - centre) > 3 * spread
}

# Add to the interviews the variables needed by the checks
prepare_interviews <- function(interviews, villages) {
  centre <- villages[match(interviews$village, villages$village), ]
  interviews$date        <- as.Date(interviews$date)
  interviews$completed   <- interviews$consent == "Yes"
  interviews$start_hour  <- as.numeric(substr(interviews$start_time, 1, 2))
  interviews$distance_km <- round(distance_km(interviews$latitude, interviews$longitude,
                                              centre$latitude, centre$longitude), 1)
  interviews
}

# ---------------------------------------------------------------------------
# Problems found in the interviews
# ---------------------------------------------------------------------------

# List of all the problems found: one row per interview and per problem
flag_interviews <- function(interviews) {
  completed <- interviews[interviews$completed, ]
  one_check <- function(rows, check, detail) {
    data.frame(interview_id = rows$interview_id, check = rep(check, nrow(rows)),
               detail = detail, stringsAsFactors = FALSE)
  }

  # 1. The same household submitted more than once
  rows <- interviews[interviews$household_id %in%
                       interviews$household_id[duplicated(interviews$household_id)], ]
  duplicates <- one_check(rows, "Duplicate household", paste("Household", rows$household_id, "submitted more than once"))

  # 2. Duration of the completed interviews
  rows <- completed[completed$duration_min < min_duration_min, ]
  short <- one_check(rows, "Short interview", paste(rows$duration_min, "minutes"))
  rows <- completed[completed$duration_min > max_duration_min, ]
  long <- one_check(rows, "Long interview", paste(rows$duration_min, "minutes"))

  # 3. Interview started outside the working hours
  rows <- interviews[interviews$start_hour < earliest_hour | interviews$start_hour >= latest_hour, ]
  hours <- one_check(rows, "Outside working hours", paste("Started at", rows$start_time))

  # 4. Impossible or unlikely values
  rows <- completed[!is.na(completed$respondent_age) &
                      (completed$respondent_age < min_age | completed$respondent_age > max_age), ]
  age <- one_check(rows, "Age out of range", paste("Age:", rows$respondent_age))
  rows <- completed[is_outlier(completed$food_expenditure), ]
  expenditure <- one_check(rows, "Expenditure outlier",
                           paste("Food expenditure:", format(rows$food_expenditure, big.mark = ",", trim = TRUE), "FCFA"))

  # 5. Answers that contradict each other
  rows <- completed[!is.na(completed$children_under_5) & completed$children_under_5 > completed$household_size, ]
  children <- one_check(rows, "Inconsistent answers",
                        paste(rows$children_under_5, "children under 5 in a household of", rows$household_size))

  # 6. GPS point far from the village
  rows <- interviews[!is.na(interviews$distance_km) & interviews$distance_km > max_distance_km, ]
  gps <- one_check(rows, "GPS far from village", paste(rows$distance_km, "km from the centre of the village"))

  rbind(duplicates, short, long, hours, age, expenditure, children, gps)
}

# ---------------------------------------------------------------------------
# Back checks
# ---------------------------------------------------------------------------

# Compare the back-check answers with the first answers.
# Returns one row per back-checked interview and per question, with a column "different".
compare_back_checks <- function(interviews, back_checks) {
  both <- merge(back_checks, interviews, by = "interview_id")
  compare <- function(question, different) {
    data.frame(interview_id = both$interview_id, enumerator = both$enumerator,
               supervisor = both$supervisor, question = rep(question_labels[[question]], nrow(both)),
               first_answer = as.character(both[[question]]),
               back_check_answer = as.character(both[[paste0(question, "_bc")]]),
               different = different, stringsAsFactors = FALSE)
  }
  rbind(
    compare("household_size",   both$household_size != both$household_size_bc),
    compare("respondent_age",   abs(both$respondent_age - both$respondent_age_bc) > age_tolerance),
    compare("owns_phone",       both$owns_phone != both$owns_phone_bc),
    compare("children_under_5", both$children_under_5 != both$children_under_5_bc)
  )
}

# ---------------------------------------------------------------------------
# Missing answers
# ---------------------------------------------------------------------------

# Share of completed interviews with no answer or "Don't know", by enumerator and question
no_answer_rates <- function(completed) {
  rates <- lapply(names(question_labels), function(question) {
    values <- completed[[question]]
    no_answer <- is.na(values) | values == "Don't know"
    result <- aggregate(no_answer, by = list(enumerator = completed$enumerator), FUN = mean)
    data.frame(enumerator = result$enumerator, question = question_labels[[question]], rate = result$x)
  })
  do.call(rbind, rates)
}

# ---------------------------------------------------------------------------
# Alerts
# ---------------------------------------------------------------------------

# Automatic alerts for the selected period, at four levels:
# all the teams together, each team, each person, each commune.
# The arguments are the tables already restricted to the selected period and filters.
build_alerts <- function(interviews, daily_reports, comparisons) {
  alerts <- list()
  add <- function(level, unit, alert) {
    alerts[[length(alerts) + 1]] <<- data.frame(level = level, unit = unit, alert = alert,
                                                stringsAsFactors = FALSE)
  }
  completed <- interviews[interviews$completed, ]
  agent <- tolower(agent_label)

  # 1. Completed interviews compared with the plan
  realisation <- function(done, planned) if (length(planned) > 0 && sum(planned) > 0) done / sum(planned) else NA
  low <- function(rate) !is.na(rate) && rate < alert_realisation
  message_low <- function(rate) paste0("Less than ", percent(alert_realisation),
                                       " of the planned interviews were completed (", percent(rate), ").")
  rate <- realisation(nrow(completed), daily_reports$planned_interviews)
  if (low(rate)) add("Overall", "All teams", message_low(rate))
  for (team in unique(daily_reports$supervisor)) {
    rate <- realisation(sum(completed$supervisor == team),
                        daily_reports$planned_interviews[daily_reports$supervisor == team])
    if (low(rate)) add(team_label, team, message_low(rate))
  }
  for (person in unique(daily_reports$enumerator)) {
    rate <- realisation(sum(completed$enumerator == person),
                        daily_reports$planned_interviews[daily_reports$enumerator == person])
    if (low(rate)) add(agent_label, person, message_low(rate))
  }

  # 2. Spot checks: coverage of each team, and concentration on the same person
  for (team in unique(completed$supervisor)) {
    team_interviews <- completed[completed$supervisor == team, ]
    spot_checks <- team_interviews[team_interviews$spot_check == "Yes", ]
    coverage <- nrow(spot_checks) / nrow(team_interviews)
    if (coverage < alert_spot_check) {
      add(team_label, team, paste0("Less than ", percent(alert_spot_check),
                                   " of the completed interviews had a spot check (", percent(coverage), ")."))
    }
    if (nrow(spot_checks) >= 5 && length(unique(team_interviews$enumerator)) > 1) {
      share <- max(table(spot_checks$enumerator)) / nrow(spot_checks)
      if (share > alert_concentration) {
        most_checked <- names(which.max(table(spot_checks$enumerator)))
        add(team_label, team, paste0("The spot checks are often done with the same ", agent, ": ",
                                     percent(share), " of them are with ", most_checked, "."))
      }
    }
  }

  # 3. Productivity very different from the others (completed interviews per day worked)
  if (nrow(daily_reports) > 0 && length(unique(daily_reports$enumerator)) >= 4) {
    days <- tapply(daily_reports$date, daily_reports$enumerator, function(x) length(unique(x)))
    done <- table(factor(completed$enumerator, levels = names(days)))
    per_day <- as.numeric(done) / as.numeric(days)
    if (sd(per_day) > 0) {
      z <- (per_day - mean(per_day)) / sd(per_day)
      for (i in which(abs(z) > alert_z_score)) {
        add(agent_label, names(days)[i],
            paste0("Completed significantly ", ifelse(z[i] > 0, "more", "fewer"),
                   " interviews per day than the others (", round(per_day[i], 1), " against ",
                   round(mean(per_day), 1), " on average)."))
      }
    }
  }

  # 4. Short interviews
  if (nrow(completed) > 0) {
    short <- tapply(completed$duration_min < min_duration_min, completed$enumerator, mean)
    for (person in names(short)[short > alert_short_interviews]) {
      add(agent_label, person, paste0(percent(short[[person]]), " of the interviews last less than ",
                                      min_duration_min, " minutes."))
    }
  }

  # 5. Back checks
  if (!is.null(comparisons) && nrow(comparisons) > 0) {
    difference <- tapply(comparisons$different, comparisons$enumerator, mean)
    compared   <- table(comparisons$enumerator)
    for (person in names(difference)) {
      if (compared[[person]] >= 8 && difference[[person]] > alert_back_check) {
        add(agent_label, person, paste0(percent(difference[[person]]),
                                        " of the back-check answers differ from the first answers."))
      }
    }
  }

  # 6. Missing answers
  if (nrow(completed) > 0) {
    rates <- no_answer_rates(completed)
    rates <- rates[rates$rate > alert_no_answer, ]
    for (i in seq_len(nrow(rates))) {
      add(agent_label, rates$enumerator[i], paste0(percent(rates$rate[i]), " of the answers to \"",
                                                   rates$question[i], "\" are missing or \"Don't know\"."))
    }
  }

  # 7. GPS points far from the village
  far <- table(interviews$enumerator[!is.na(interviews$distance_km) & interviews$distance_km > max_distance_km])
  for (person in names(far)[far >= alert_far_gps]) {
    add(agent_label, person, paste0(far[[person]], " interviews recorded more than ", max_distance_km,
                                    " km from the village."))
  }

  # 8. Incidents
  incidents <- daily_reports[daily_reports$incident == "Yes", ]
  for (team in unique(incidents$supervisor)) {
    rows <- incidents[incidents$supervisor == team, ]
    not_reported <- sum(rows$incident_reported == "No")
    add(team_label, team, paste0(nrow(rows), " incident(s)",
                                 ifelse(not_reported > 0, paste0(", of which ", not_reported, " not reported."), ".")))
  }

  # 9. Mobile network
  for (commune in unique(daily_reports$commune)) {
    rows <- daily_reports[daily_reports$commune == commune, ]
    for (operator in names(operators)) {
      good <- mean(rows[[paste0("call_", operator)]] == "Good")
      if (good < alert_network) {
        add("Commune", commune, paste0("Calls on ", operators[[operator]], " are good less than half of the time (",
                                       percent(good), ")."))
      }
    }
  }

  if (length(alerts) == 0) {
    return(data.frame(level = character(), unit = character(), alert = character()))
  }
  do.call(rbind, alerts)
}
