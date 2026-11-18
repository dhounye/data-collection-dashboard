# Data collection dashboard
#
# A Shiny app to monitor a household survey during fieldwork: progress against the plan,
# alerts, productivity, duration of the interviews, data quality checks, spot checks and
# back checks, field conditions, GPS points and the list of interviews to verify.
#
# The data are simulated (see simulate_data.R). The app can also read the forms directly
# from SurveyCTO (see surveycto.R).

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)

source("config.R")
source("checks.R")
source("surveycto.R")

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

# Read the data: from SurveyCTO when the connection is defined, from the simulated files otherwise
load_data <- function() {
  villages <- read.csv("data/villages.csv")
  if (use_surveycto()) {
    forms  <- read_surveycto_data()
    source <- "SurveyCTO"
  } else {
    forms <- list(interviews    = read.csv("data/interviews.csv"),
                  daily_reports = read.csv("data/daily_reports.csv"),
                  back_checks   = read.csv("data/back_checks.csv"))
    source <- "Simulated data"
  }
  # The daily reports and the back checks are optional: empty tables when they are not available
  if (is.null(forms$daily_reports)) {
    forms$daily_reports <- read.csv("data/daily_reports.csv")[0, ]
  }
  if (is.null(forms$back_checks)) {
    forms$back_checks <- read.csv("data/back_checks.csv")[0, ]
  }
  forms$daily_reports$date <- as.Date(forms$daily_reports$date)
  list(villages      = villages,
       interviews    = prepare_interviews(forms$interviews, villages),
       daily_reports = forms$daily_reports,
       back_checks   = forms$back_checks,
       source        = source,
       loaded_at     = Sys.time())
}

# The data are kept in memory and shared by all the users of the app. They are read again
# when they are older than the refresh time, or when a user clicks on "Refresh".
cache <- new.env()
get_data <- function() {
  too_old <- is.null(cache$data) ||
    difftime(Sys.time(), cache$data$loaded_at, units = "mins") >= refresh_minutes
  if (too_old || isTRUE(cache$force)) {
    cache$force <- FALSE
    new_data <- tryCatch(load_data(), error = function(e) {
      cache$error <- conditionMessage(e)
      NULL
    })
    if (!is.null(new_data)) {
      cache$data  <- new_data
      cache$error <- NULL
    }
  }
  cache$data
}

initial    <- get_data()
first_date <- if (is.null(initial)) Sys.Date() else min(initial$interviews$date)
last_date  <- if (is.null(initial)) Sys.Date() else max(initial$interviews$date)
choices <- function(column) if (is.null(initial)) NULL else sort(unique(initial$interviews[[column]]))

# ---------------------------------------------------------------------------
# Appearance
# ---------------------------------------------------------------------------

theme_set(
  theme_minimal(base_size = 12) +
    theme(text = element_text(colour = colour_dark),
          axis.text = element_text(colour = colour_muted),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#ECEEF1"))
)

# Turn a ggplot into an interactive plot: the information appears when the cursor is on the plot
interactive <- function(plot, tooltip = "text") {
  widget <- ggplotly(plot, tooltip = tooltip) %>%
    config(displayModeBar = FALSE) %>%
    layout(hoverlabel = list(bgcolor = colour_dark, bordercolor = colour_dark,
                             font = list(color = "#FFFFFF", size = 13)),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(color = colour_dark))
  suppressWarnings(plotly_build(widget))
}

box_theme <- function(background, text = "#FFFFFF") value_box_theme(bg = background, fg = text)

css <- paste0("
  body { background-color: ", colour_back, "; }
  :root { --bslib-navbar-default-bg: ", colour_dark, "; --bslib-navbar-inverse-bg: ", colour_dark, "; }
  nav.navbar.navbar-default, nav.navbar.navbar-inverse, nav.navbar { background-color: ", colour_dark, " !important; }
  .navbar .navbar-brand { color: #FFFFFF !important; font-weight: 600; }
  .navbar .nav-link { color: ", colour_grey, " !important; }
  .navbar .nav-link.active, .navbar .nav-link:hover { color: #FFFFFF !important; border-bottom-color: ", colour_light, " !important; }
  .bslib-sidebar-layout > .sidebar { background-color: #FFFFFF; }
  .card { border: 1px solid ", colour_grey, "; box-shadow: none; }
  .card-header { background-color: #FFFFFF; color: ", colour_dark, "; font-weight: 600; }
  .bslib-value-box .value-box-title { font-size: 0.95rem; }
  .source-note { color: ", colour_muted, "; font-size: 0.85rem; }
")

agent  <- tolower(agent_label)
agents <- paste0(agent, "s")
team   <- tolower(team_label)

# ---------------------------------------------------------------------------
# User interface
# ---------------------------------------------------------------------------

ui <- page_navbar(
  title = "Data collection dashboard",
  theme = bs_theme(version = 5, bg = "#FFFFFF", fg = colour_dark, primary = colour_main,
                   secondary = colour_muted, info = colour_light),
  header = tags$head(tags$style(HTML(css))),
  fillable = FALSE,

  # Filters, common to all the pages
  sidebar = sidebar(
    title = "Period and filters", width = 300,
    radioButtons("period", "Period",
                 choices = c("Day", "Week to date", "Month to date", "Since the start", "Custom dates"),
                 selected = "Since the start"),
    conditionalPanel(
      "input.period != 'Custom dates'",
      dateInput("reference_date", "Reference date", value = last_date, min = first_date, max = last_date)
    ),
    conditionalPanel(
      "input.period == 'Custom dates'",
      dateRangeInput("custom_dates", "Dates of interview", start = first_date, end = last_date,
                     min = first_date, max = last_date)
    ),
    selectizeInput("commune", "Commune", choices = choices("commune"),
                   multiple = TRUE, options = list(placeholder = "All")),
    selectizeInput("supervisor", team_label, choices = choices("supervisor"),
                   multiple = TRUE, options = list(placeholder = "All")),
    selectizeInput("enumerator", agent_label, choices = choices("enumerator"),
                   multiple = TRUE, options = list(placeholder = "All")),
    actionButton("refresh", "Refresh the data", icon = icon("rotate"), class = "btn-primary"),
    div(class = "source-note", textOutput("data_source"))
  ),

  nav_panel(
    "Overview",
    layout_columns(
      value_box("Completed interviews", textOutput("box_completed"), textOutput("box_realisation"),
                showcase = icon("clipboard-check"), showcase_layout = "top right", theme = box_theme(colour_main)),
      value_box("Progress to the target", textOutput("box_progress"), textOutput("box_target"),
                showcase = icon("bullseye"), showcase_layout = "top right", theme = box_theme(colour_light, colour_dark)),
      value_box("Consent rate", textOutput("box_consent"), "of the households visited",
                showcase = icon("handshake"), showcase_layout = "top right", theme = box_theme(colour_muted)),
      value_box("Alerts", textOutput("box_alerts"), "for the selected period",
                showcase = icon("bell"), showcase_layout = "top right", theme = box_theme(colour_dark))
    ),
    layout_columns(
      value_box("Spot checks", textOutput("box_spot_check"), "of the completed interviews",
                showcase = icon("eye"), showcase_layout = "top right", theme = box_theme(colour_muted)),
      value_box("Back-check differences", textOutput("box_difference"), "of the answers compared",
                showcase = icon("code-compare"), showcase_layout = "top right", theme = box_theme(colour_main)),
      value_box("Interviews to verify", textOutput("box_flagged"), textOutput("box_flagged_share"),
                showcase = icon("flag"), showcase_layout = "top right", theme = box_theme(colour_dark)),
      value_box("Incidents", textOutput("box_incidents"), textOutput("box_incidents_reported"),
                showcase = icon("triangle-exclamation"), showcase_layout = "top right", theme = box_theme(colour_light, colour_dark))
    ),
    layout_columns(
      card(full_screen = TRUE, card_header("Completed interviews (bars) and planned interviews (dashed line) by day"),
           plotlyOutput("plot_daily", height = 320)),
      card(full_screen = TRUE, card_header("Completed interviews since the start, compared with the target"),
           plotlyOutput("plot_progress", height = 320))
    ),
    card(full_screen = TRUE, card_header("Completed interviews and target by commune, since the start"),
         plotlyOutput("plot_commune", height = 300))
  ),

  nav_panel(
    "Alerts",
    layout_columns(
      card(full_screen = TRUE, card_header("Number of alerts by level"),
           plotlyOutput("plot_alerts_level", height = 280)),
      card(full_screen = TRUE, card_header(paste0("Number of alerts by ", team, " and by ", agent)),
           plotlyOutput("plot_alerts_unit", height = 280))
    ),
    card(card_header("Alerts for the selected period"),
         selectizeInput("alert_level", "Level", choices = c("Overall", team_label, agent_label, "Commune"),
                        multiple = TRUE, options = list(placeholder = "All")),
         DTOutput("table_alerts"))
  ),

  nav_panel(
    "Productivity",
    card(full_screen = TRUE, card_header(paste0("Completed interviews by ", agent, " and by day")),
         plotlyOutput("plot_productivity", height = 380)),
    card(full_screen = TRUE,
         card_header(paste0("Completed interviews as a share of the planned interviews, by ", agent,
                            ". Dashed line: alert below ", percent(alert_realisation))),
         plotlyOutput("plot_realisation", height = 340)),
    card(card_header(paste0("Summary by ", agent,
                            ". The last columns compare each person with the team over the last 7 days")),
         DTOutput("table_productivity"))
  ),

  nav_panel(
    "Duration",
    layout_columns(
      card(full_screen = TRUE,
           card_header("Duration of the completed interviews. Dashed lines: limits for short and long interviews"),
           plotlyOutput("plot_duration", height = 340)),
      card(full_screen = TRUE, card_header(paste0("Duration by ", agent)),
           plotlyOutput("plot_duration_enumerator", height = 340))
    ),
    card(card_header(paste0("Short interviews (less than ", min_duration_min, " minutes) by ", agent)),
         DTOutput("table_duration"))
  ),

  nav_panel(
    "Data quality",
    card(full_screen = TRUE,
         card_header(paste0("Share of completed interviews with no answer or \"Don't know\", by question and ", agent)),
         plotlyOutput("plot_missing", height = 380)),
    layout_columns(
      card(full_screen = TRUE, card_header("Monthly food expenditure: distribution and outliers"),
           plotlyOutput("plot_expenditure", height = 320)),
      card(card_header("Households submitted more than once"), DTOutput("table_duplicates"))
    )
  ),

  nav_panel(
    "Supervision",
    layout_columns(
      value_box("Interviews with a spot check", textOutput("box_spot_checked"),
                textOutput("box_spot_checked_share"), showcase = icon("eye"), showcase_layout = "top right", theme = box_theme(colour_muted)),
      value_box("Interviews back-checked", textOutput("box_back_checked"),
                textOutput("box_back_checked_share"), showcase = icon("rotate-left"), showcase_layout = "top right", theme = box_theme(colour_main)),
      value_box("Back-check answers that differ", textOutput("box_difference_2"),
                "of the answers compared", showcase = icon("code-compare"), showcase_layout = "top right", theme = box_theme(colour_dark))
    ),
    layout_columns(
      card(full_screen = TRUE, card_header(paste0("Share of completed interviews with a spot check, by ", agent,
                                                ". Dashed line: ", percent(alert_spot_check))),
           plotlyOutput("plot_spot_check", height = 340)),
      card(full_screen = TRUE,
           card_header(paste0("Who receives the spot checks of each ", team, "? Dashed line: ",
                              percent(alert_concentration))),
           plotlyOutput("plot_spot_share", height = 340))
    ),
    layout_columns(
      card(full_screen = TRUE, card_header(paste0("Share of back-check answers that differ, by ", agent,
                                                ". Dashed line: alert above ", percent(alert_back_check))),
           plotlyOutput("plot_back_check", height = 340)),
      card(card_header("Back-check differences by question"), DTOutput("table_back_check"))
    ),
    card(card_header("Back-check answers that differ"), DTOutput("table_differences"))
  ),

  nav_panel(
    "Field conditions",
    layout_columns(
      card(full_screen = TRUE, card_header("Weather"), plotlyOutput("plot_weather", height = 260)),
      card(full_screen = TRUE, card_header("Condition of the road"), plotlyOutput("plot_road", height = 260)),
      card(full_screen = TRUE, card_header("Means of transport"), plotlyOutput("plot_transport", height = 260))
    ),
    layout_columns(
      card(full_screen = TRUE, card_header("Mobile network: share of daily reports with a good network"),
           plotlyOutput("plot_network", height = 320)),
      card(full_screen = TRUE, card_header("Good network for calls, by commune and operator"),
           plotlyOutput("plot_network_commune", height = 320))
    ),
    layout_columns(
      card(full_screen = TRUE,
           card_header(paste0("Completed interviews per ", agent, " and per day, by weather")),
           plotlyOutput("plot_weather_productivity", height = 300)),
      card(card_header("Incidents"), DTOutput("table_incidents"))
    )
  ),

  nav_panel(
    "Map",
    card(full_screen = TRUE,
         card_header(paste0("GPS points of the interviews. Dark points: more than ", max_distance_km,
                            " km from the centre of the village. Put the cursor on a point to see the interview")),
         leafletOutput("map", height = 600))
  ),

  nav_panel(
    "Interviews to verify",
    layout_columns(
      card(full_screen = TRUE, card_header("Problems by type"), plotlyOutput("plot_flags_check", height = 300)),
      card(full_screen = TRUE, card_header(paste0("Problems by ", agent)),
           plotlyOutput("plot_flags_enumerator", height = 300))
    ),
    card(card_header("List of the problems to send to the field team"),
         layout_columns(
           col_widths = c(4, 4),
           selectizeInput("check_type", "Type of problem",
                          choices = c("Age out of range", "Duplicate household", "Expenditure outlier",
                                      "GPS far from village", "Inconsistent answers", "Long interview",
                                      "Outside working hours", "Short interview"),
                          multiple = TRUE, options = list(placeholder = "All")),
           div(style = "padding-top: 32px;",
               downloadButton("download_flags", "Download the list (CSV)", class = "btn-primary"))
         ),
         DTOutput("table_flags"))
  ),

  nav_panel(
    "About",
    card(
      card_header("About this dashboard"),
      p("This dashboard shows how a household survey can be monitored while it is in the field:
         progress against the plan and the target, alerts, productivity of the field staff,
         duration of the interviews, high-frequency checks on the answers, spot checks,
         back checks, field conditions and GPS checks."),
      p(strong("The data shown by default are simulated."), "They describe a fictitious survey of 30 villages
         in 6 communes of Benin, with 12 enumerators in 4 teams. No real respondent, enumerator or
         project is involved. Problems were added on purpose so that the checks have something to detect."),
      p("The dashboard can also read the forms directly from SurveyCTO, so that it follows the
         fieldwork in real time. The file surveycto.R explains how."),
      p(strong("Periods."), "Day: the reference date. Week to date: from Monday to the reference date.
         Month to date: from the first day of the month to the reference date."),
      p(strong("Rules of the checks.")),
      tags$ul(
        tags$li(paste0("Short interview: less than ", min_duration_min, " minutes. Long interview: more than ",
                       max_duration_min, " minutes.")),
        tags$li(paste0("Outside working hours: started before ", earliest_hour, ":00 or from ", latest_hour, ":00.")),
        tags$li(paste0("Age out of range: under ", min_age, " or over ", max_age, ".")),
        tags$li("Expenditure outlier: the logarithm of the value is more than 3 median absolute deviations from the median."),
        tags$li("Inconsistent answers: more children under 5 than household members."),
        tags$li(paste0("GPS far from village: more than ", max_distance_km, " km from the centre of the village.")),
        tags$li(paste0("Back check: an answer differs when it is not the same as the first answer. For the age, a difference of ",
                       age_tolerance, " years or less is accepted."))
      ),
      p(strong("Rules of the alerts.")),
      tags$ul(
        tags$li(paste0("Less than ", percent(alert_realisation), " of the planned interviews completed.")),
        tags$li(paste0("Less than ", percent(alert_spot_check), " of the completed interviews with a spot check, or more than ",
                       percent(alert_concentration), " of the spot checks of a team with the same person.")),
        tags$li(paste0("Completed interviews per day more than ", alert_z_score, " standard deviations away from the average.")),
        tags$li(paste0("More than ", percent(alert_short_interviews), " of short interviews, more than ",
                       percent(alert_back_check), " of back-check answers that differ, more than ",
                       percent(alert_no_answer), " of missing answers to a question.")),
        tags$li(paste0(alert_far_gps, " interviews or more recorded far from the village.")),
        tags$li("Any incident, and a mobile network that is good less than half of the time.")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # ----- Data, read again regularly or when the user clicks on "Refresh" ------

  refresh_click <- reactiveVal(0)
  observeEvent(input$refresh, {
    cache$force <- TRUE
    refresh_click(refresh_click() + 1)
  })

  survey <- reactive({
    refresh_click()
    invalidateLater(refresh_minutes * 60 * 1000)
    data <- get_data()
    validate(need(!is.null(data), paste("The data could not be read.", cache$error)))
    data
  })

  output$data_source <- renderText({
    paste0("Source: ", survey()$source, ". Last update: ", format(survey()$loaded_at, "%d %b %Y, %H:%M"),
           if (!is.null(cache$error)) paste0(". Last attempt failed: ", cache$error) else "")
  })

  # When new data arrive, the filters follow them: new communes, teams and persons appear in
  # the lists, and the reference date moves to the latest day unless the user chose another day.
  latest_day <- reactiveVal(last_date)
  observeEvent(survey(), {
    interviews <- survey()$interviews
    updateSelectizeInput(session, "commune", choices = sort(unique(interviews$commune)), selected = input$commune)
    updateSelectizeInput(session, "supervisor", choices = sort(unique(interviews$supervisor)), selected = input$supervisor)
    updateSelectizeInput(session, "enumerator", choices = sort(unique(interviews$enumerator)), selected = input$enumerator)
    new_latest <- max(interviews$date)
    follow <- is.null(input$reference_date) || input$reference_date >= latest_day()
    updateDateInput(session, "reference_date", min = min(interviews$date), max = new_latest,
                    value = if (follow) new_latest else input$reference_date)
    updateDateRangeInput(session, "custom_dates", min = min(interviews$date), max = new_latest)
    latest_day(new_latest)
  })

  all_flags       <- reactive(flag_interviews(survey()$interviews))
  all_comparisons <- reactive(compare_back_checks(survey()$interviews, survey()$back_checks))

  # ----- Period and filters ---------------------------------------------------

  # First and last dates of the selected period
  window <- reactive({
    if (input$period == "Custom dates") {
      req(input$custom_dates)
      as.Date(input$custom_dates)
    } else {
      req(input$reference_date)
      period_dates(input$period, input$reference_date, min(survey()$interviews$date))
    }
  })

  # Keep the rows of a table that match the filters on commune, team and person
  apply_filters <- function(data) {
    if (length(input$commune) > 0 && "commune" %in% names(data))       data <- data[data$commune %in% input$commune, ]
    if (length(input$supervisor) > 0 && "supervisor" %in% names(data)) data <- data[data$supervisor %in% input$supervisor, ]
    if (length(input$enumerator) > 0 && "enumerator" %in% names(data)) data <- data[data$enumerator %in% input$enumerator, ]
    data
  }
  in_window <- function(data) data[data$date >= window()[1] & data$date <= window()[2], ]

  # Interviews of the selected period
  selected <- reactive({
    data <- apply_filters(in_window(survey()$interviews))
    validate(need(nrow(data) > 0, "No interview for this period and these filters."))
    data
  })
  completed <- reactive(selected()[selected()$completed, ])

  # Interviews from the start of the fieldwork to the end of the selected period
  since_start <- reactive({
    data <- survey()$interviews
    apply_filters(data[data$date <= window()[2], ])
  })

  # Daily reports of the selected period
  reports <- reactive(apply_filters(in_window(survey()$daily_reports)))

  # Target of the selected villages. The target is defined by village, not by person,
  # so there is no target to show when a person is selected.
  target <- reactive({
    if (length(input$enumerator) > 0) return(NA)
    sum(apply_filters(survey()$villages)$target)
  })

  # Problems found in the selected interviews
  flags <- reactive({
    all_flags() %>%
      inner_join(selected() %>% select(interview_id, date, enumerator, supervisor, commune, village),
                 by = "interview_id") %>%
      select(interview_id, date, enumerator, supervisor, commune, village, check, detail) %>%
      arrange(date, enumerator, interview_id)
  })

  comparisons <- reactive(all_comparisons()[all_comparisons()$interview_id %in% selected()$interview_id, ])

  alerts <- reactive(build_alerts(selected(), reports(), comparisons()))

  # ----- Overview -------------------------------------------------------------

  output$box_completed <- renderText(format(nrow(completed()), big.mark = ","))
  output$box_realisation <- renderText({
    planned <- sum(reports()$planned_interviews)
    if (planned == 0) return("No plan available for this period")
    paste0(percent(nrow(completed()) / planned), " of the ", format(planned, big.mark = ","), " planned")
  })
  output$box_progress <- renderText({
    if (is.na(target())) return("-")
    percent(sum(since_start()$completed) / target())
  })
  output$box_target <- renderText({
    if (is.na(target())) return(paste0("The target is defined by village, not by ", agent))
    paste0(format(sum(since_start()$completed), big.mark = ","), " of ", format(target(), big.mark = ","),
           " households since the start")
  })
  output$box_consent <- renderText(percent(mean(selected()$completed)))
  output$box_alerts  <- renderText(nrow(alerts()))
  output$box_spot_check <- renderText(percent(mean(completed()$spot_check == "Yes")))
  output$box_difference <- renderText({
    if (nrow(comparisons()) == 0) return("-")
    percent(mean(comparisons()$different))
  })
  output$box_flagged <- renderText(n_distinct(flags()$interview_id))
  output$box_flagged_share <- renderText(paste0(percent(n_distinct(flags()$interview_id) / nrow(selected())),
                                                " of the households visited"))
  output$box_incidents <- renderText(sum(reports()$incident == "Yes"))
  output$box_incidents_reported <- renderText({
    not_reported <- sum(reports()$incident == "Yes" & reports()$incident_reported == "No")
    paste0(not_reported, " not reported")
  })

  output$plot_daily <- renderPlotly({
    by_day <- completed() %>% count(date, name = "completed") %>%
      full_join(reports() %>% group_by(date) %>% summarise(planned = sum(planned_interviews)), by = "date") %>%
      mutate(completed = coalesce(completed, 0L), planned = coalesce(planned, 0L)) %>%
      arrange(date) %>%
      mutate(label = paste0(format(date, "%A %d %B"), "<br>Completed: ", completed, "<br>Planned: ", planned,
                            ifelse(planned > 0, paste0("<br>Realisation: ", percent(completed / planned)), "")))
    plot <- ggplot(by_day, aes(date, text = label)) +
      geom_col(aes(y = completed), fill = colour_main) +
      geom_line(aes(y = planned, group = 1), colour = colour_dark, linetype = "dashed") +
      geom_point(aes(y = planned), colour = colour_dark, size = 1.5) +
      labs(x = NULL, y = "Interviews")
    interactive(plot)
  })

  output$plot_progress <- renderPlotly({
    progress <- since_start() %>% filter(completed) %>% count(date) %>% arrange(date) %>%
      mutate(total = cumsum(n),
             label = paste0(format(date, "%A %d %B"), "<br>Completed since the start: ", format(total, big.mark = ","),
                            if (!is.na(target())) paste0("<br>Share of the target: ", percent(total / target())) else ""))
    validate(need(nrow(progress) > 0, "No completed interview."))
    plot <- ggplot(progress, aes(date, total, text = label, group = 1)) +
      geom_area(fill = colour_light, alpha = 0.25) +
      geom_line(colour = colour_main, linewidth = 1) +
      geom_point(colour = colour_main, size = 1.8) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "Completed interviews (cumulative)")
    if (!is.na(target())) {
      plot <- plot +
        geom_hline(yintercept = target(), linetype = "dashed", colour = colour_dark) +
        annotate("text", x = min(progress$date), y = target() * 1.04, label = "Target", hjust = 0,
                 colour = colour_dark, size = 3.8) +
        expand_limits(y = target() * 1.08)
    }
    interactive(plot)
  })

  output$plot_commune <- renderPlotly({
    done <- since_start() %>% filter(completed) %>% count(commune, name = "completed")
    by_commune <- apply_filters(survey()$villages) %>% group_by(commune) %>% summarise(target = sum(target)) %>%
      left_join(done, by = "commune") %>%
      mutate(completed = coalesce(completed, 0L),
             label = paste0(commune, "<br>Completed: ", completed, "<br>Target: ", target,
                            "<br>Share of the target: ", percent(completed / target)))
    if (is.na(target())) {
      by_commune <- by_commune %>% filter(completed > 0) %>%
        mutate(label = paste0(commune, "<br>Completed: ", completed))
      plot <- ggplot(by_commune, aes(reorder(commune, completed), completed, text = label)) +
        geom_col(fill = colour_main, width = 0.6) +
        coord_flip() + labs(x = NULL, y = "Completed interviews")
    } else {
      plot <- ggplot(by_commune, aes(reorder(commune, completed), text = label)) +
        geom_col(aes(y = target), fill = colour_grey, width = 0.8) +
        geom_col(aes(y = completed), fill = colour_main, width = 0.5) +
        coord_flip() + labs(x = NULL, y = "Completed interviews (blue) and target (grey)")
    }
    interactive(plot)
  })

  # ----- Alerts ---------------------------------------------------------------

  output$plot_alerts_level <- renderPlotly({
    validate(need(nrow(alerts()) > 0, "No alert for this period."))
    by_level <- alerts() %>% count(level) %>% mutate(label = paste0(level, "<br>Alerts: ", n))
    interactive(ggplot(by_level, aes(reorder(level, n), n, text = label)) +
                  geom_col(fill = colour_main, width = 0.6) + coord_flip() +
                  scale_y_continuous(breaks = function(limits) unique(floor(pretty(limits)))) +
                  labs(x = NULL, y = "Number of alerts"))
  })

  output$plot_alerts_unit <- renderPlotly({
    by_unit <- alerts() %>% filter(level %in% c(team_label, agent_label)) %>% count(level, unit)
    validate(need(nrow(by_unit) > 0, "No alert for this period."))
    by_unit <- by_unit %>% mutate(label = paste0(unit, "<br>Alerts: ", n))
    interactive(ggplot(by_unit, aes(reorder(unit, n), n, fill = level, text = label)) +
                  geom_col(width = 0.6) + coord_flip() +
                  scale_fill_manual(values = setNames(c(colour_dark, colour_light), c(team_label, agent_label)), name = NULL) +
                  scale_y_continuous(breaks = function(limits) unique(floor(pretty(limits)))) +
                  labs(x = NULL, y = "Number of alerts"))
  })

  output$table_alerts <- renderDT({
    shown <- alerts()
    if (length(input$alert_level) > 0) shown <- shown[shown$level %in% input$alert_level, ]
    shown %>% rename(Level = level, Concerns = unit, Alert = alert) %>%
      datatable(rownames = FALSE, options = list(pageLength = 15))
  })

  # ----- Productivity ---------------------------------------------------------

  output$plot_productivity <- renderPlotly({
    by_day <- completed() %>% count(enumerator, date) %>%
      left_join(reports() %>% select(enumerator, date, planned_interviews, weather), by = c("enumerator", "date")) %>%
      mutate(label = paste0(enumerator, ", ", format(date, "%A %d %B"), "<br>Completed: ", n,
                            ifelse(is.na(planned_interviews), "", paste0("<br>Planned: ", planned_interviews)),
                            ifelse(is.na(weather), "", paste0("<br>Weather: ", weather))))
    days <- sort(unique(by_day$date))
    by_day$day <- factor(format(by_day$date, "%d %b"), levels = format(days, "%d %b"))
    plot <- ggplot(by_day, aes(day, enumerator, fill = n, text = label)) +
      geom_tile(colour = "white") +
      scale_fill_gradient(low = "#CFE4FA", high = colour_main, name = "Interviews") +
      scale_y_discrete(limits = rev) +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    interactive(plot)
  })

  output$plot_realisation <- renderPlotly({
    validate(need(nrow(reports()) > 0, "No daily report for this selection."))
    by_person <- reports() %>% group_by(enumerator, supervisor) %>%
      summarise(planned = sum(planned_interviews), .groups = "drop") %>%
      left_join(completed() %>% count(enumerator, name = "completed"), by = "enumerator") %>%
      mutate(completed = coalesce(completed, 0L), rate = completed / planned,
             below = rate < alert_realisation,
             label = paste0(enumerator, " (", supervisor, ")<br>Completed: ", completed, "<br>Planned: ", planned,
                            "<br>Realisation: ", percent(rate)))
    plot <- ggplot(by_person, aes(enumerator, rate, fill = below, text = label)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = alert_realisation, linetype = "dashed", colour = colour_dark) +
      scale_fill_manual(values = c(`FALSE` = colour_main, `TRUE` = colour_dark), guide = "none") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "Completed / planned") +
      theme(legend.position = "none")
    interactive(plot)
  })

  output$table_productivity <- renderDT({
    # Completed interviews over the 7 days that end with the selected period, compared with the team
    last_7_days <- since_start() %>%
      filter(completed, date >= window()[2] - 6) %>% count(enumerator, name = "last_7_days")
    summary <- selected() %>%
      group_by(enumerator, supervisor) %>%
      summarise(days = n_distinct(date), visited = n(), completed = sum(completed), .groups = "drop") %>%
      left_join(reports() %>% group_by(enumerator) %>% summarise(planned = sum(planned_interviews)), by = "enumerator") %>%
      left_join(last_7_days, by = "enumerator") %>%
      mutate(last_7_days = coalesce(last_7_days, 0L)) %>%
      group_by(supervisor) %>%
      mutate(z_score = if (n() > 1 && sd(last_7_days) > 0)
        round((last_7_days - mean(last_7_days)) / sqrt(mean((last_7_days - mean(last_7_days))^2)), 2) else 0) %>%
      ungroup()
    summary %>%
      transmute(Person = enumerator, Team = supervisor, `Days worked` = days, Planned = planned,
                `Households visited` = visited, `Completed interviews` = completed,
                Realisation = ifelse(is.na(planned) | planned == 0, "-", percent(completed / planned)),
                `Completed per day` = round(completed / days, 1),
                `Completed, last 7 days` = last_7_days, `Z-score in the team` = z_score) %>%
      rename(!!agent_label := Person, !!team_label := Team) %>%
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 50, scrollX = TRUE))
  })

  # ----- Duration -------------------------------------------------------------

  output$plot_duration <- renderPlotly({
    bins <- completed() %>%
      mutate(bin = floor(duration_min / 5) * 5) %>% count(bin) %>%
      mutate(label = paste0(bin, " to ", bin + 4, " minutes<br>Interviews: ", n))
    plot <- ggplot(bins, aes(bin + 2.5, n, text = label)) +
      geom_col(fill = colour_main, width = 4.6) +
      geom_vline(xintercept = c(min_duration_min, max_duration_min), linetype = "dashed", colour = colour_dark) +
      labs(x = "Duration (minutes)", y = "Interviews")
    interactive(plot)
  })

  output$plot_duration_enumerator <- renderPlotly({
    plot <- ggplot(completed(), aes(enumerator, duration_min)) +
      geom_boxplot(fill = "#CFE4FA", colour = colour_main, outlier.colour = colour_dark) +
      geom_hline(yintercept = min_duration_min, linetype = "dashed", colour = colour_dark) +
      scale_x_discrete(limits = rev) + coord_flip() +
      labs(x = NULL, y = "Duration (minutes)")
    interactive(plot, tooltip = c("y", "x"))
  })

  output$table_duration <- renderDT({
    completed() %>%
      group_by(Person = enumerator, Team = supervisor) %>%
      summarise(`Completed interviews` = n(),
                `Median duration (minutes)` = median(duration_min),
                `Short interviews` = sum(duration_min < min_duration_min),
                `Share of short interviews` = percent(mean(duration_min < min_duration_min)),
                .groups = "drop") %>%
      arrange(desc(`Short interviews`)) %>%
      rename(!!agent_label := Person, !!team_label := Team) %>%
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 50))
  })

  # ----- Data quality ---------------------------------------------------------

  output$plot_missing <- renderPlotly({
    validate(need(nrow(completed()) > 0, "No completed interview."))
    rates <- no_answer_rates(completed()) %>%
      mutate(question = factor(question, levels = rev(unname(question_labels))),
             label = paste0(enumerator, "<br>", question, "<br>No answer or \"Don't know\": ", percent(rate)))
    plot <- ggplot(rates, aes(enumerator, question, fill = rate, text = label)) +
      geom_tile(colour = "white") +
      scale_fill_gradient(low = "#F1F6FC", high = colour_main, labels = scales::percent, name = NULL) +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    interactive(plot)
  })

  output$plot_expenditure <- renderPlotly({
    outliers <- all_flags()$interview_id[all_flags()$check == "Expenditure outlier"]
    values <- completed() %>% filter(!is.na(food_expenditure)) %>%
      mutate(outlier = ifelse(interview_id %in% outliers, "Outlier", "Usual value"),
             bin = round(log10(food_expenditure) * 10) / 10) %>%
      count(bin, outlier) %>%
      mutate(label = paste0("Around ", format(round(10^bin, -2), big.mark = ","), " FCFA<br>", outlier,
                            "<br>Interviews: ", n))
    validate(need(nrow(values) > 0, "No expenditure recorded."))
    plot <- ggplot(values, aes(10^bin, n, fill = outlier, text = label)) +
      geom_col() +
      scale_x_log10(labels = scales::comma) +
      scale_fill_manual(values = c(`Usual value` = colour_main, Outlier = colour_dark), name = NULL) +
      labs(x = "Monthly food expenditure (FCFA, logarithmic scale)", y = "Interviews")
    interactive(plot)
  })

  output$table_duplicates <- renderDT({
    selected() %>%
      filter(interview_id %in% all_flags()$interview_id[all_flags()$check == "Duplicate household"]) %>%
      arrange(household_id, date) %>%
      select(Household = household_id, Interview = interview_id, Date = date,
             Person = enumerator, Village = village) %>%
      rename(!!agent_label := Person) %>%
      datatable(rownames = FALSE, options = list(dom = "tp", pageLength = 6))
  })

  # ----- Supervision: spot checks and back checks --------------------------------

  output$box_spot_checked <- renderText(sum(completed()$spot_check == "Yes"))
  output$box_spot_checked_share <- renderText(paste0(percent(mean(completed()$spot_check == "Yes")),
                                                     " of the completed interviews"))
  output$box_back_checked <- renderText(n_distinct(comparisons()$interview_id))
  output$box_back_checked_share <- renderText(paste0(percent(n_distinct(comparisons()$interview_id) / nrow(completed())),
                                                     " of the completed interviews"))
  output$box_difference_2 <- renderText({
    if (nrow(comparisons()) == 0) return("-")
    percent(mean(comparisons()$different))
  })

  output$plot_spot_check <- renderPlotly({
    by_person <- completed() %>% group_by(enumerator, supervisor) %>%
      summarise(interviews = n(), spot_checks = sum(spot_check == "Yes"), .groups = "drop") %>%
      mutate(rate = spot_checks / interviews,
             label = paste0(enumerator, " (", supervisor, ")<br>Spot checks: ", spot_checks, " of ", interviews,
                            " interviews<br>Share: ", percent(rate)))
    plot <- ggplot(by_person, aes(enumerator, rate, fill = supervisor, text = label)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = alert_spot_check, linetype = "dashed", colour = colour_dark) +
      scale_fill_manual(values = c(colour_main, colour_light, colour_muted, colour_dark, colour_grey), name = team_label) +
      scale_x_discrete(limits = rev) + scale_y_continuous(labels = scales::percent) + coord_flip() +
      labs(x = NULL, y = "Share of interviews with a spot check")
    interactive(plot)
  })

  output$plot_spot_share <- renderPlotly({
    shares <- completed() %>% filter(spot_check == "Yes") %>% count(supervisor, enumerator)
    validate(need(nrow(shares) > 0, "No spot check for this selection."))
    shares <- shares %>% group_by(supervisor) %>%
      mutate(share = n / sum(n), rank = rank(-share, ties.method = "first"),
             label = paste0(supervisor, "<br>", enumerator, ": ", n, " spot checks<br>",
                            percent(share), " of the spot checks of the ", team)) %>% ungroup()
    plot <- ggplot(shares, aes(supervisor, share, fill = factor(rank), text = label)) +
      geom_col(width = 0.6, colour = "white") +
      geom_hline(yintercept = alert_concentration, linetype = "dashed", colour = colour_dark) +
      scale_fill_manual(values = c(colour_main, colour_light, colour_muted, colour_grey, "#ECEEF1", "#F1F6FC"),
                        guide = "none") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "Share of the spot checks") +
      theme(legend.position = "none")
    interactive(plot)
  })

  output$plot_back_check <- renderPlotly({
    validate(need(nrow(comparisons()) > 0, "No back check for this selection."))
    by_person <- comparisons() %>% group_by(enumerator) %>%
      summarise(rate = mean(different), compared = n(), differences = sum(different)) %>%
      mutate(above = rate > alert_back_check,
             label = paste0(enumerator, "<br>Answers that differ: ", differences, " of ", compared,
                            "<br>Share: ", percent(rate)))
    plot <- ggplot(by_person, aes(enumerator, rate, fill = above, text = label)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = alert_back_check, linetype = "dashed", colour = colour_dark) +
      scale_fill_manual(values = c(`FALSE` = colour_main, `TRUE` = colour_dark), guide = "none") +
      scale_x_discrete(limits = rev) + scale_y_continuous(labels = scales::percent) + coord_flip() +
      labs(x = NULL, y = "Share of answers that differ") +
      theme(legend.position = "none")
    interactive(plot)
  })

  output$table_back_check <- renderDT({
    comparisons() %>%
      group_by(Question = question) %>%
      summarise(`Answers compared` = n(), `Answers that differ` = sum(different),
                `Share` = percent(mean(different))) %>%
      datatable(rownames = FALSE, options = list(dom = "t"))
  })

  output$table_differences <- renderDT({
    comparisons() %>% filter(different) %>%
      select(Interview = interview_id, Person = enumerator, Team = supervisor,
             Question = question, `First answer` = first_answer, `Back-check answer` = back_check_answer) %>%
      rename(!!agent_label := Person, !!team_label := Team) %>%
      datatable(rownames = FALSE, options = list(pageLength = 8))
  })

  # ----- Field conditions -----------------------------------------------------

  # Bar chart of the number of daily reports for each value of a variable
  conditions_plot <- function(variable, levels) {
    validate(need(nrow(reports()) > 0, "No daily report for this selection."))
    counts <- reports() %>% count(value = .data[[variable]]) %>%
      mutate(value = factor(value, levels = levels), share = n / sum(n),
             label = paste0(value, "<br>Daily reports: ", n, "<br>Share: ", percent(share)))
    interactive(ggplot(counts, aes(value, n, text = label)) +
                  geom_col(fill = colour_main, width = 0.6) +
                  scale_x_discrete(limits = rev(levels)) + coord_flip() +
                  labs(x = NULL, y = "Daily reports"))
  }
  output$plot_weather   <- renderPlotly(conditions_plot("weather", c("Normal", "Strong heat", "Strong wind", "Fog", "Rain")))
  output$plot_road      <- renderPlotly(conditions_plot("road_condition", c("Very good", "Rather good", "Rather bad", "Very bad")))
  output$plot_transport <- renderPlotly(conditions_plot("transport", c("Motorbike", "Car", "On foot")))

  # Share of daily reports with a good network, for each operator and each use
  network_rates <- function(data) {
    rates <- lapply(names(operators), function(operator) {
      data.frame(operator = operators[[operator]], use = c("Calls", "Internet"),
                 good = c(mean(data[[paste0("call_", operator)]] == "Good"),
                          mean(data[[paste0("internet_", operator)]] == "Good")))
    })
    do.call(rbind, rates)
  }

  output$plot_network <- renderPlotly({
    validate(need(nrow(reports()) > 0, "No daily report for this selection."))
    rates <- network_rates(reports()) %>%
      mutate(label = paste0(operator, ", ", tolower(use), "<br>Good network: ", percent(good)))
    plot <- ggplot(rates, aes(operator, good, fill = use, text = label)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_hline(yintercept = alert_network, linetype = "dashed", colour = colour_dark) +
      scale_fill_manual(values = c(Calls = colour_main, Internet = colour_light), name = NULL) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(x = NULL, y = "Good network")
    interactive(plot)
  })

  output$plot_network_commune <- renderPlotly({
    validate(need(nrow(reports()) > 0, "No daily report for this selection."))
    rates <- lapply(split(reports(), reports()$commune), function(rows) {
      data.frame(commune = rows$commune[1], network_rates(rows))
    })
    rates <- do.call(rbind, rates) %>% filter(use == "Calls") %>%
      mutate(label = paste0(commune, ", ", operator, "<br>Good network for calls: ", percent(good)))
    plot <- ggplot(rates, aes(operator, commune, fill = good, text = label)) +
      geom_tile(colour = "white") +
      scale_fill_gradient(low = colour_dark, high = "#CFE4FA", labels = scales::percent, limits = c(0, 1), name = NULL) +
      scale_y_discrete(limits = rev) +
      labs(x = NULL, y = NULL)
    interactive(plot)
  })

  output$plot_weather_productivity <- renderPlotly({
    validate(need(nrow(reports()) > 0, "No daily report for this selection."))
    by_weather <- reports() %>%
      left_join(completed() %>% count(enumerator, date, name = "completed"), by = c("enumerator", "date")) %>%
      mutate(completed = coalesce(completed, 0L)) %>%
      group_by(weather) %>% summarise(average = mean(completed), days = n()) %>%
      mutate(label = paste0(weather, "<br>Completed interviews per person and per day: ", round(average, 1),
                            "<br>Daily reports: ", days))
    plot <- ggplot(by_weather, aes(reorder(weather, average), average, text = label)) +
      geom_col(fill = colour_main, width = 0.6) + coord_flip() +
      labs(x = NULL, y = "Completed interviews per person and per day")
    interactive(plot)
  })

  output$table_incidents <- renderDT({
    reports() %>% filter(incident == "Yes") %>% arrange(date) %>%
      select(Date = date, Person = enumerator, Team = supervisor, Village = village,
             Incident = incident_type, Reported = incident_reported) %>%
      rename(!!agent_label := Person, !!team_label := Team) %>%
      datatable(rownames = FALSE, options = list(dom = "tp", pageLength = 6))
  })

  # ----- Map ------------------------------------------------------------------

  output$map <- renderLeaflet({
    points <- selected() %>% filter(!is.na(latitude), !is.na(longitude)) %>%
      mutate(far = distance_km > max_distance_km,
             label = paste0(interview_id, " | ", enumerator, " | village ", village, " | ", date, " at ", start_time,
                            " | ", distance_km, " km from the centre of the village"))
    leaflet(points) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 5, stroke = FALSE, fillOpacity = 0.75,
                       color = ~ifelse(far, colour_dark, colour_light), label = ~label,
                       popup = ~paste0("<b>", interview_id, "</b><br>", enumerator, ", village ", village,
                                       "<br>", date, " at ", start_time,
                                       "<br>", distance_km, " km from the centre of the village")) %>%
      addLegend(position = "bottomright", colors = c(colour_light, colour_dark), opacity = 0.9,
                labels = c(paste0("Within ", max_distance_km, " km of the village"),
                           paste0("More than ", max_distance_km, " km from the village")))
  })

  # ----- Interviews to verify -------------------------------------------------

  output$plot_flags_check <- renderPlotly({
    validate(need(nrow(flags()) > 0, "No problem found for this selection."))
    by_check <- flags() %>% count(check) %>% mutate(label = paste0(check, "<br>Problems: ", n))
    interactive(ggplot(by_check, aes(reorder(check, n), n, text = label)) +
                  geom_col(fill = colour_main, width = 0.6) + coord_flip() +
                  labs(x = NULL, y = "Number of problems"))
  })

  output$plot_flags_enumerator <- renderPlotly({
    validate(need(nrow(flags()) > 0, "No problem found for this selection."))
    by_person <- flags() %>% count(enumerator, check) %>%
      group_by(enumerator) %>% mutate(total = sum(n)) %>% ungroup() %>%
      mutate(label = paste0(enumerator, "<br>", check, ": ", n, "<br>All problems: ", total))
    interactive(ggplot(by_person, aes(enumerator, n, fill = check, text = label)) +
                  geom_col(width = 0.6) +
                  scale_fill_manual(values = c(colour_main, colour_light, colour_muted, colour_dark, colour_grey,
                                               "#8FB8E8", "#3E5A73", "#B7C4CC"), name = NULL) +
                  scale_x_discrete(limits = rev) + coord_flip() +
                  labs(x = NULL, y = "Number of problems"))
  })

  # Problems shown in the list: all of them, or the types chosen on the page
  listed_flags <- reactive({
    if (length(input$check_type) > 0) flags()[flags()$check %in% input$check_type, ] else flags()
  })

  output$table_flags <- renderDT({
    listed_flags() %>%
      rename(Interview = interview_id, Date = date, Person = enumerator, Team = supervisor,
             Commune = commune, Village = village, Problem = check, Detail = detail) %>%
      rename(!!agent_label := Person, !!team_label := Team) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10))
  })

  output$download_flags <- downloadHandler(
    filename = function() paste0("interviews_to_verify_", Sys.Date(), ".csv"),
    content = function(file) write.csv(listed_flags(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
