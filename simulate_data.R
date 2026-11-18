# Simulated data for the data collection dashboard
#
# This script creates a fictitious household survey: 30 villages in 6 communes of Benin,
# 12 enumerators in 4 teams, 18 days of fieldwork. No real respondent, enumerator or
# project is involved. Problems are added on purpose (short interviews, duplicates,
# missing answers, outliers, GPS points far from the village, back-check differences,
# low productivity, uneven spot checks, incidents), so that the dashboard has something
# to detect.
#
# Run this script once to create the files of the folder "data".

set.seed(2026)

# ---------------------------------------------------------------------------
# 1. Survey design
# ---------------------------------------------------------------------------

communes <- data.frame(
  department = c("Atlantique", "Atlantique", "Zou", "Zou", "Borgou", "Borgou"),
  commune    = c("Abomey-Calavi", "Ouidah", "Bohicon", "Abomey", "Parakou", "N'Dali"),
  latitude   = c(6.4485, 6.3631, 7.1783, 7.1829, 9.3372, 9.8608),
  longitude  = c(2.3557, 2.0851, 2.0667, 1.9912, 2.6303, 2.7181)
)

villages_per_commune <- 5
target_per_village   <- 40   # households to visit in each village
planned_per_day      <- 6    # interviews planned for each enumerator and each day

# Each village is placed a few kilometres around the centre of its commune
villages <- communes[rep(1:nrow(communes), each = villages_per_commune), ]
villages$village    <- sprintf("V%02d", 1:nrow(villages))
villages$latitude   <- villages$latitude  + runif(nrow(villages), -0.08, 0.08)
villages$longitude  <- villages$longitude + runif(nrow(villages), -0.08, 0.08)
villages$target     <- target_per_village
# Four teams: the first two teams have 8 villages each, the last two have 7
villages$supervisor <- rep(c("SUP1", "SUP2", "SUP3", "SUP4"), times = c(8, 8, 7, 7))
# Usual condition of the road that leads to the village
road_levels <- c("Very bad", "Rather bad", "Rather good", "Very good")
villages$road <- sample(road_levels, nrow(villages), replace = TRUE, prob = c(0.08, 0.27, 0.45, 0.20))
villages <- villages[, c("village", "commune", "department", "latitude", "longitude",
                         "target", "supervisor", "road")]
rownames(villages) <- NULL

enumerators <- data.frame(
  enumerator = sprintf("ENUM%02d", 1:12),
  supervisor = rep(c("SUP1", "SUP2", "SUP3", "SUP4"), each = 3)
)

# 18 days of fieldwork, Monday to Saturday, during three weeks
all_days   <- seq(as.Date("2026-09-07"), as.Date("2026-09-26"), by = "day")
field_days <- as.character(all_days[weekdays(all_days) != "Sunday"])

# Weather of each team and each day
weather_levels <- c("Normal", "Strong heat", "Strong wind", "Fog", "Rain")
weather <- expand.grid(date = field_days, supervisor = unique(enumerators$supervisor),
                       stringsAsFactors = FALSE)
weather$weather <- sample(weather_levels, nrow(weather), replace = TRUE,
                          prob = c(0.66, 0.12, 0.08, 0.04, 0.10))

# ---------------------------------------------------------------------------
# 2. Visits: who interviews where, when and for how long
# ---------------------------------------------------------------------------

visits  <- list()
reports <- list()   # one daily report per enumerator and per day worked
for (team in unique(enumerators$supervisor)) {
  team_villages    <- villages$village[villages$supervisor == team]
  team_enumerators <- enumerators$enumerator[enumerators$supervisor == team]
  village_index    <- 1
  visited          <- 0   # households visited in the current village

  for (day in field_days) {
    if (village_index > length(team_villages)) break   # the team has finished its villages
    day_weather <- weather$weather[weather$date == day & weather$supervisor == team]
    day_village <- team_villages[village_index]
    day_road    <- villages$road[villages$village == day_village]

    for (enumerator in team_enumerators) {
      n_interviews <- sample(4:7, 1, prob = c(0.2, 0.35, 0.3, 0.15))
      # Fewer interviews when it rains or when the road is very bad
      if (day_weather == "Rain")   n_interviews <- n_interviews - 2
      if (day_road == "Very bad")  n_interviews <- n_interviews - 1
      # ENUM09 works slowly: about half of the planned interviews
      if (enumerator == "ENUM09")  n_interviews <- sample(2:4, 1)

      reports[[length(reports) + 1]] <- data.frame(
        date = day, enumerator = enumerator, supervisor = team, village = day_village,
        planned_interviews = planned_per_day, weather = day_weather, road_condition = day_road
      )

      clock <- 8 * 60 + rnorm(1, 0, 15)   # the first interview starts around 8:00
      for (i in seq_len(n_interviews)) {
        if (village_index > length(team_villages)) break
        duration <- max(25, rnorm(1, 45, 8))
        visits[[length(visits) + 1]] <- data.frame(
          date = day, enumerator = enumerator, supervisor = team,
          village = team_villages[village_index],
          start_minute = clock, duration_min = duration
        )
        clock   <- clock + duration + runif(1, 10, 25)   # walk to the next household
        visited <- visited + 1
        if (visited >= target_per_village) {             # village completed: go to the next one
          village_index <- village_index + 1
          visited <- 0
        }
      }
    }
  }
}
interviews    <- do.call(rbind, visits)
daily_reports <- do.call(rbind, reports)
n <- nrow(interviews)

interviews <- merge(interviews, villages[, c("village", "commune", "department")], by = "village")
interviews <- interviews[order(interviews$date, interviews$enumerator, interviews$start_minute), ]
rownames(interviews) <- NULL

# Identifiers: one key per submitted interview, one household identifier per household
interviews$interview_id <- sprintf("INT-%04d", 1:n)
interviews$household_id <- paste0(interviews$village, "-H",
                                  sprintf("%02d", ave(1:n, interviews$village, FUN = seq_along)))

# ---------------------------------------------------------------------------
# 3. Answers to the questionnaire
# ---------------------------------------------------------------------------

interviews$consent <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.96, 0.04))

interviews$respondent_sex   <- sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.55, 0.45))
interviews$respondent_age   <- pmin(85, pmax(18, round(rnorm(n, 41, 13))))
interviews$household_size   <- pmin(15, 1 + rpois(n, 4.6))
interviews$children_under_5 <- rbinom(n, pmin(interviews$household_size - 1, 4), 0.25)
interviews$owns_phone       <- sample(c("Yes", "No", "Don't know"), n, replace = TRUE,
                                      prob = c(0.78, 0.20, 0.02))
# Monthly food expenditure of the household, in FCFA
interviews$food_expenditure <- round(rlnorm(n, meanlog = log(42000), sdlog = 0.45), -2)
interviews$water_source     <- sample(c("Borehole", "Public tap", "Well", "Surface water", "Other"),
                                      n, replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.10, 0.05))

# GPS point of the interview: a few hundred metres around the centre of the village
village_position <- villages[match(interviews$village, villages$village), ]
interviews$latitude  <- village_position$latitude  + rnorm(n, 0, 0.004)
interviews$longitude <- village_position$longitude + rnorm(n, 0, 0.004)

# Ordinary missing answers
interviews$food_expenditure[runif(n) < 0.02] <- NA
interviews$water_source[runif(n) < 0.01]     <- NA

# Spot checks: the supervisor attends the interview.
# SUP1 and SUP3 attend about 20% of the interviews of each enumerator.
# SUP2 attends mostly the interviews of ENUM04. SUP4 attends very few interviews.
p_spot_check <- ifelse(interviews$supervisor %in% c("SUP1", "SUP3"), 0.20,
                ifelse(interviews$supervisor == "SUP4", 0.05,
                ifelse(interviews$enumerator == "ENUM04", 0.45, 0.06)))
interviews$spot_check <- ifelse(runif(n) < p_spot_check, "Yes", "No")

# ---------------------------------------------------------------------------
# 4. Problems added on purpose
# ---------------------------------------------------------------------------

# 4.1 ENUM07 rushes a quarter of the interviews (less than 15 minutes)
rushed <- which(interviews$enumerator == "ENUM07" & runif(n) < 0.25)
interviews$duration_min[rushed] <- runif(length(rushed), 6, 14)

# 4.2 A few very long interviews, and a few interviews started late in the evening
too_long <- sample(n, 6)
interviews$duration_min[too_long] <- runif(6, 130, 180)
late <- sample(n, 5)
interviews$start_minute[late] <- runif(5, 21 * 60, 23 * 60)

# 4.3 ENUM11 often records no answer for food expenditure and for phone ownership
enum11 <- interviews$enumerator == "ENUM11"
interviews$food_expenditure[enum11 & runif(n) < 0.15] <- NA
interviews$owns_phone[enum11 & runif(n) < 0.18]       <- "Don't know"

# 4.4 Outliers: one zero too many in the expenditure, impossible ages
extra_zero <- sample(which(!is.na(interviews$food_expenditure)), 8)
interviews$food_expenditure[extra_zero] <- interviews$food_expenditure[extra_zero] * 10
interviews$respondent_age[sample(n, 4)] <- c(7, 9, 112, 120)

# 4.5 Answers that contradict each other: more children under 5 than household members
contradiction <- sample(n, 8)
interviews$children_under_5[contradiction] <- interviews$household_size[contradiction] + sample(1:2, 8, replace = TRUE)

# 4.6 ENUM03 records 12 interviews at the same place, far from the village
#     (as if the questionnaires were filled in without going to the households)
same_place <- sample(which(interviews$enumerator == "ENUM03"), 12)
interviews$latitude[same_place]  <- village_position$latitude[same_place]  + 0.06
interviews$longitude[same_place] <- village_position$longitude[same_place] + 0.03
# and 5 other interviews recorded far from their village
far <- sample(setdiff(1:n, same_place), 5)
interviews$latitude[far] <- interviews$latitude[far] + runif(5, 0.04, 0.07)

# 4.7 Duplicates: 6 households are submitted twice
duplicated_rows <- sample(n, 6)
for (row in duplicated_rows) {
  same_village <- setdiff(which(interviews$village == interviews$village[row]), row)
  interviews$household_id[row] <- interviews$household_id[sample(same_village, 1)]
}

# ---------------------------------------------------------------------------
# 5. Final variables of the interviews
# ---------------------------------------------------------------------------

# Without consent, the interview stops after a few minutes and has no answer
no_consent <- interviews$consent == "No"
interviews$duration_min[no_consent] <- runif(sum(no_consent), 2, 5)
questions <- c("respondent_sex", "respondent_age", "household_size", "children_under_5",
               "owns_phone", "food_expenditure", "water_source")
interviews[no_consent, questions] <- NA

interviews$duration_min <- round(interviews$duration_min)
interviews$start_time   <- sprintf("%02d:%02d", floor(interviews$start_minute / 60) %% 24,
                                   floor(interviews$start_minute %% 60))
interviews$latitude  <- round(interviews$latitude, 5)
interviews$longitude <- round(interviews$longitude, 5)

interviews <- interviews[, c("interview_id", "household_id", "date", "start_time", "duration_min",
                             "enumerator", "supervisor", "department", "commune", "village",
                             "latitude", "longitude", "consent", "spot_check", questions)]

# ---------------------------------------------------------------------------
# 6. Daily reports of the enumerators: plan, field conditions and incidents
# ---------------------------------------------------------------------------

r <- nrow(daily_reports)
daily_reports$commune <- villages$commune[match(daily_reports$village, villages$village)]

# The road is worse than usual when it rains
rain <- daily_reports$weather == "Rain"
level <- match(daily_reports$road_condition, road_levels)
level[rain] <- pmax(1, level[rain] - 1)
daily_reports$road_condition <- road_levels[level]

daily_reports$transport <- sample(c("Motorbike", "Car", "On foot"), r, replace = TRUE,
                                  prob = c(0.78, 0.14, 0.08))

# Quality of the mobile network in the village, for calls and for the Internet.
# The three operators do not have the same coverage in all the communes.
p_good <- function(operator, commune) {
  p <- c(MTN = 0.92, Moov = 0.85, Celtiis = 0.88)[operator]
  p <- ifelse(commune == "N'Dali" & operator == "Moov", 0.35, p)       # weak Moov network in N'Dali
  p <- ifelse(commune == "Ouidah" & operator == "Celtiis", 0.45, p)    # weak Celtiis network in Ouidah
  p
}
for (operator in c("MTN", "Moov", "Celtiis")) {
  p <- p_good(operator, daily_reports$commune)
  daily_reports[[paste0("call_", tolower(operator))]]     <- ifelse(runif(r) < p, "Good", "Bad")
  daily_reports[[paste0("internet_", tolower(operator))]] <- ifelse(runif(r) < p - 0.08, "Good", "Bad")
}

# Incidents: 9 incidents during the fieldwork, 3 of them not reported to the coordination
daily_reports$incident          <- "No"
daily_reports$incident_type     <- NA
daily_reports$incident_reported <- NA
with_incident <- sample(r, 9)
daily_reports$incident[with_incident]      <- "Yes"
daily_reports$incident_type[with_incident] <- sample(
  c("Motorbike breakdown", "Tablet failure", "Refusal by the village authority",
    "Minor road accident", "Security concern"), 9, replace = TRUE)
daily_reports$incident_reported[with_incident] <- sample(rep(c("Yes", "No"), times = c(6, 3)))

daily_reports <- daily_reports[, c("date", "enumerator", "supervisor", "commune", "village",
                                   "planned_interviews", "transport", "road_condition", "weather",
                                   "call_mtn", "call_moov", "call_celtiis",
                                   "internet_mtn", "internet_moov", "internet_celtiis",
                                   "incident", "incident_type", "incident_reported")]

# ---------------------------------------------------------------------------
# 7. Back checks: 15% of the completed interviews are visited again
# ---------------------------------------------------------------------------

completed   <- interviews[interviews$consent == "Yes", ]
back_checks <- completed[sample(nrow(completed), round(0.15 * nrow(completed))), ]
m <- nrow(back_checks)

# Probability that the back-check answer differs from the first answer:
# 4% in general, 30% for the interviews of ENUM07
p_difference <- ifelse(back_checks$enumerator == "ENUM07", 0.30, 0.04)

household_size_bc <- back_checks$household_size
change <- runif(m) < p_difference
household_size_bc[change] <- pmax(1, household_size_bc[change] + sample(c(-3, -2, 2, 3), sum(change), replace = TRUE))

# For the age, a difference of one year is normal; a real difference is 5 years or more
respondent_age_bc <- back_checks$respondent_age + sample(-1:1, m, replace = TRUE)
change <- runif(m) < p_difference
respondent_age_bc[change] <- respondent_age_bc[change] + sample(c(-9, -6, 6, 9), sum(change), replace = TRUE)

owns_phone_bc <- back_checks$owns_phone
change <- runif(m) < p_difference
owns_phone_bc[change] <- ifelse(owns_phone_bc[change] == "Yes", "No", "Yes")

children_under_5_bc <- back_checks$children_under_5
change <- runif(m) < p_difference
children_under_5_bc[change] <- children_under_5_bc[change] + 1

back_checks <- data.frame(
  interview_id        = back_checks$interview_id,
  back_check_date     = as.character(as.Date(back_checks$date) + sample(1:3, m, replace = TRUE)),
  back_checker        = sample(c("BC1", "BC2"), m, replace = TRUE),
  household_size_bc   = household_size_bc,
  respondent_age_bc   = respondent_age_bc,
  owns_phone_bc       = owns_phone_bc,
  children_under_5_bc = children_under_5_bc
)
back_checks <- back_checks[order(back_checks$interview_id), ]

# ---------------------------------------------------------------------------
# 8. Save the files
# ---------------------------------------------------------------------------

villages$latitude  <- round(villages$latitude, 5)
villages$longitude <- round(villages$longitude, 5)
villages$road <- NULL

dir.create("data", showWarnings = FALSE)
write.csv(villages,      "data/villages.csv",      row.names = FALSE)
write.csv(interviews,    "data/interviews.csv",    row.names = FALSE)
write.csv(daily_reports, "data/daily_reports.csv", row.names = FALSE)
write.csv(back_checks,   "data/back_checks.csv",   row.names = FALSE)

cat("Villages:", nrow(villages), "\n")
cat("Interviews:", nrow(interviews), "\n")
cat("Daily reports:", nrow(daily_reports), "\n")
cat("Back checks:", nrow(back_checks), "\n")
