# Settings of the dashboard
#
# Everything that changes from one survey to another is in this file:
# the words used for the field staff, the colours, the thresholds of the checks
# and the connection to SurveyCTO.

# ---------------------------------------------------------------------------
# Words used in the dashboard
# ---------------------------------------------------------------------------

# In a household survey, the field staff are enumerators and supervisors.
# In a monitoring project, they can be called monitors and blocs: change the words here.
agent_label <- "Enumerator"
team_label  <- "Supervisor"

# ---------------------------------------------------------------------------
# Colours
# ---------------------------------------------------------------------------

colour_main  <- "#1E4CA1"   # main blue: bars, lines, main figures
colour_dark  <- "#27354F"   # dark blue: navigation bar, problems, limits
colour_light <- "#4DA6FF"   # light blue: second series
colour_muted <- "#5C7D87"   # grey blue: third series, secondary text
colour_grey  <- "#DADADA"   # light grey: targets, background of bars
colour_back  <- "#F9FAFB"   # background of the page

# ---------------------------------------------------------------------------
# Thresholds of the data quality checks
# ---------------------------------------------------------------------------

min_duration_min <- 20    # an interview shorter than this is flagged
max_duration_min <- 120   # an interview longer than this is flagged
earliest_hour    <- 6     # an interview started before this hour is flagged
latest_hour      <- 20    # an interview started at this hour or later is flagged
max_distance_km  <- 3     # maximum distance between the GPS point and the centre of the village
min_age          <- 18    # the respondent must be an adult
max_age          <- 100
age_tolerance    <- 2     # back check: a difference of 2 years or less in the age is accepted

# ---------------------------------------------------------------------------
# Thresholds of the alerts
# ---------------------------------------------------------------------------

alert_realisation      <- 0.60   # alert when less than 60% of the planned interviews are completed
alert_spot_check       <- 0.10   # alert when less than 10% of the completed interviews have a spot check
alert_concentration    <- 0.60   # alert when more than 60% of the spot checks of a team are on one person
alert_short_interviews <- 0.10   # alert when more than 10% of the interviews of one person are short
alert_back_check       <- 0.15   # alert when more than 15% of the back-check answers differ
alert_no_answer        <- 0.10   # alert when more than 10% of the answers to a question are missing
alert_far_gps          <- 5      # alert from 5 interviews recorded far from the village
alert_network          <- 0.50   # alert when the network is good less than half of the time
alert_z_score          <- 2      # alert when the productivity is more than 2 standard deviations from the mean

# ---------------------------------------------------------------------------
# Live data from SurveyCTO
# ---------------------------------------------------------------------------

# The dashboard reads SurveyCTO when the server name is defined (see surveycto.R and the README).
# Otherwise it uses the simulated data of the folder "data".
refresh_minutes <- 10   # the data are read again every 10 minutes
