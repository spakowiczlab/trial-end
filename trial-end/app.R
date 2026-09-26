# trial-end: project the completion date of a clinical trial
#
# Constant-rate projection from enrollment so far:
#   weeks_open = days since the trial opened / 7
#   rate = current_accrual / weeks_open
#   weeks_to_full_accrual = ceiling((target - current) / rate)
#   full_accrual = today + weeks_to_full_accrual
#   study_completion = full_accrual + weeks_after_accrual
# weeks_after_accrual is follow-up after the accrual target is met.

library(shiny)
library(lubridate)
library(ggplot2)

project_trial_dates <- function(today, open_date, accrual_target,
                                current_accrual, weeks_after_accrual) {
  weeks_open <- as.numeric(today - open_date) / 7
  patients_to_go <- accrual_target - current_accrual

  if (!is.finite(weeks_open) || weeks_open <= 0) {
    stop("Trial open date must be before today's date.", call. = FALSE)
  }
  if (!is.finite(current_accrual) || current_accrual < 1) {
    stop("Current accrual must be at least 1.", call. = FALSE)
  }
  if (!is.finite(accrual_target) || accrual_target < 1) {
    stop("Accrual target must be at least 1.", call. = FALSE)
  }
  if (!is.finite(patients_to_go) || patients_to_go < 0) {
    stop("Current accrual is already above the accrual target.", call. = FALSE)
  }
  if (!is.finite(weeks_after_accrual) || weeks_after_accrual < 0) {
    stop("Weeks after full accrual must be zero or positive.", call. = FALSE)
  }

  rate <- current_accrual / weeks_open
  weeks_to_full <- ceiling(patients_to_go / rate)
  if (!is.finite(weeks_to_full)) {
    stop("These inputs do not produce a finite projection.", call. = FALSE)
  }

  list(
    weeks_open = weeks_open,
    patients_to_go = patients_to_go,
    rate = rate,
    weeks_to_full = weeks_to_full,
    full = today + lubridate::weeks(weeks_to_full),
    completion = today + lubridate::weeks(weeks_to_full + weeks_after_accrual)
  )
}

pretty_date <- function(d) {
  gsub(" +", " ", format(d, "%B %e, %Y"))
}

followup_label <- function(weeks) {
  if (weeks == 0) {
    "Same day as full accrual"
  } else if (weeks == 1) {
    "1 week after full accrual"
  } else {
    paste0(format(weeks, scientific = FALSE, trim = TRUE), " weeks after full accrual")
  }
}

weeks_remaining_label <- function(weeks) {
  if (weeks == 0) {
    "Accrual target met"
  } else if (weeks == 1) {
    "1 week from today"
  } else {
    paste0(format(weeks, scientific = FALSE, trim = TRUE), " weeks from today")
  }
}

build_accrual_plot <- function(today, open_date, current, target, full, completion) {
  enrolled <- data.frame(
    date = c(open_date, today),
    accrual = c(0, current),
    series = "Enrolled so far"
  )
  pieces <- list(enrolled)
  if (full > today) {
    pieces[[length(pieces) + 1]] <- data.frame(
      date = c(today, full),
      accrual = c(current, target),
      series = "Projected accrual"
    )
  }
  if (completion > full) {
    pieces[[length(pieces) + 1]] <- data.frame(
      date = c(full, completion),
      accrual = c(target, target),
      series = "Follow-up"
    )
  }
  segments <- do.call(rbind, pieces)
  segments$series <- factor(
    segments$series,
    levels = c("Enrolled so far", "Projected accrual", "Follow-up")
  )

  markers <- data.frame(
    date = c(today, full, completion),
    accrual = c(current, target, target),
    fill = c("#14556E", "#1C7C96", "#E07A5F"),
    stringsAsFactors = FALSE
  )
  markers <- markers[!duplicated(markers$date), ]

  colors <- c(
    "Enrolled so far" = "#14556E",
    "Projected accrual" = "#1C7C96",
    "Follow-up" = "#E07A5F"
  )
  linetypes <- c(
    "Enrolled so far" = "solid",
    "Projected accrual" = "22",
    "Follow-up" = "solid"
  )

  ggplot(segments, aes(x = date, y = accrual, color = series, linetype = series)) +
    geom_hline(
      yintercept = target,
      linetype = "22",
      color = "#F2997A",
      linewidth = 0.55
    ) +
    geom_vline(
      xintercept = today,
      color = "#14556E",
      linewidth = 0.35,
      alpha = 0.35
    ) +
    geom_line(linewidth = 1.15, lineend = "round") +
    geom_point(
      data = markers,
      aes(x = date, y = accrual, fill = fill),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.8,
      stroke = 0.7,
      color = "#F7F4EE"
    ) +
    annotate(
      "text",
      x = today,
      y = target * 1.14,
      label = "Today",
      hjust = -0.15,
      fontface = "bold",
      color = "#14556E",
      size = 3.6
    ) +
    scale_fill_identity() +
    scale_color_manual(values = colors, drop = TRUE) +
    scale_linetype_manual(values = linetypes, drop = TRUE) +
    scale_x_date(expand = expansion(mult = c(0.04, 0.08))) +
    scale_y_continuous(
      limits = c(0, target * 1.22),
      expand = expansion(mult = c(0.02, 0.02)),
      breaks = pretty
    ) +
    labs(x = NULL, y = "Participants") +
    theme_minimal(base_size = 13, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E6EEF1", linewidth = 0.4),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(color = "#0C3344"),
      axis.title.y = element_text(color = "#14556E", margin = margin(r = 8)),
      axis.text = element_text(color = "#0C3344"),
      plot.margin = margin(10, 16, 4, 8)
    )
}

app_css <- "
  body {
    background: #F4EFE6;
    color: #0C3344;
    font-family: \"Avenir Next\", \"Segoe UI\", Helvetica, Arial, sans-serif;
  }
  .container-fluid { padding-top: 0; }
  .te-header {
    background: linear-gradient(165deg, #1C7C96 0%, #14556E 58%, #0C3344 100%);
    color: #F7F4EE;
    margin: 0 -15px 22px;
    padding: 22px 28px;
  }
  .te-brand { display: flex; align-items: center; gap: 16px; }
  .te-mark { height: 76px; width: auto; display: block; }
  .te-header h1 {
    margin: 0;
    font-size: 34px;
    font-weight: 700;
    letter-spacing: -0.4px;
    color: #F7F4EE;
  }
  .te-header p { margin: 4px 0 0; color: #F7F4EE; opacity: 0.92; font-size: 16px; }
  .well {
    background: #fff;
    border: none;
    border-radius: 14px;
    box-shadow: 0 10px 28px rgba(12, 51, 68, 0.08);
    padding: 18px 18px 8px;
  }
  .control-label { color: #0C3344; font-weight: 600; font-size: 13px; }
  .form-control {
    border-radius: 8px;
    border-color: #D5E3E8;
    box-shadow: none;
    color: #0C3344;
  }
  .form-control:focus {
    border-color: #1C7C96;
    box-shadow: 0 0 0 3px rgba(28, 124, 150, 0.15);
  }
  .help-block { color: #3d6474; font-size: 13px; line-height: 1.45; }
  .te-cards { display: flex; flex-wrap: wrap; gap: 14px; }
  .te-card {
    flex: 1 1 220px;
    background: #fff;
    border-radius: 14px;
    padding: 14px 16px 12px;
    box-shadow: 0 10px 28px rgba(12, 51, 68, 0.08);
    border-top: 4px solid #1C7C96;
  }
  .te-card-done { border-top-color: #F2997A; }
  .te-kicker {
    font-size: 11px;
    letter-spacing: 0.08em;
    text-transform: uppercase;
    color: #14556E;
    font-weight: 700;
  }
  .te-date {
    font-size: 26px;
    line-height: 1.2;
    font-weight: 700;
    color: #0C3344;
    margin-top: 4px;
  }
  .te-meta { color: #3d6474; margin-top: 4px; font-size: 14px; }
  .te-rate { color: #14556E; margin: 14px 2px 0; font-size: 15px; }
  .te-progress-label {
    display: flex;
    justify-content: space-between;
    color: #3d6474;
    font-size: 13px;
    margin-top: 12px;
  }
  .te-progress {
    height: 8px;
    background: #E6EEF1;
    border-radius: 99px;
    overflow: hidden;
    margin-top: 6px;
  }
  .te-progress-fill {
    height: 100%;
    background: linear-gradient(90deg, #14556E, #77D1E5);
    border-radius: 99px;
  }
  .te-plot-card {
    background: #fff;
    border-radius: 14px;
    margin-top: 16px;
    padding: 8px 8px 4px;
    box-shadow: 0 10px 28px rgba(12, 51, 68, 0.08);
  }
  .shiny-output-error-validation {
    color: #0C3344;
    background: #fff;
    border-left: 4px solid #F2997A;
    border-radius: 12px;
    padding: 14px 16px;
    box-shadow: 0 10px 28px rgba(12, 51, 68, 0.08);
  }
  .te-note { color: #3d6474; font-size: 13px; margin: 2px 10px 10px; line-height: 1.45; }
"

ui <- fluidPage(
  tags$head(
    tags$title("trial-end"),
    tags$style(HTML(app_css))
  ),

  div(
    class = "te-header",
    div(
      class = "te-brand",
      tags$img(
        src = "trial-end-hex.svg",
        class = "te-mark",
        alt = "trial-end hex sticker"
      ),
      div(
        h1("trial-end"),
        p("When will accrual finish, and when will the study complete?")
      )
    )
  ),

  sidebarLayout(
    sidebarPanel(
      dateInput("todays.date",
                "Today's date",
                value = Sys.Date()),
      dateInput("trial.open.date",
                "Date the trial opened",
                value = Sys.Date() - 84),
      numericInput("accrual.target",
                  "Accrual target",
                  min = 1, value = 63, step = 1),
      numericInput("current.accrual",
                  "Current accrual",
                  min = 1, value = 5, step = 1),
      numericInput("study.duration",
                  "Weeks from full accrual to study completion",
                  min = 0, value = 16, step = 1),
      helpText(
        "The accrual rate is current accrual divided by the weeks since",
        "the trial opened, and that rate is held constant.",
        "The last input is follow-up after the accrual target is met."
      )
    ),

    mainPanel(
      uiOutput("forecast")
    )
  )
)

server <- function(input, output) {

  forecast <- reactive({
    req(input$todays.date, input$trial.open.date,
        input$accrual.target, input$current.accrual, input$study.duration)

    weeks_open <- as.numeric(input$todays.date - input$trial.open.date) / 7

    validate(
      need(is.finite(weeks_open) && weeks_open > 0,
           "Trial open date must be before today's date."),
      need(is.finite(input$current.accrual) && input$current.accrual >= 1,
           "Current accrual must be at least 1 so an accrual rate can be estimated."),
      need(is.finite(input$accrual.target) && input$accrual.target >= 1,
           "Accrual target must be at least 1."),
      need(input$current.accrual <= input$accrual.target,
           "Current accrual is already above the accrual target, so a forward projection does not apply."),
      need(is.finite(input$study.duration) && input$study.duration >= 0,
           "Weeks from full accrual to study completion must be zero or positive.")
    )

    f <- project_trial_dates(
      input$todays.date,
      input$trial.open.date,
      input$accrual.target,
      input$current.accrual,
      input$study.duration
    )
    f$today <- input$todays.date
    f$open <- input$trial.open.date
    f$target <- input$accrual.target
    f$current <- input$current.accrual
    f$followup <- input$study.duration
    f
  })

  output$forecast <- renderUI({
    f <- forecast()
    rate_label <- format(round(f$rate, 2), nsmall = 2)
    pct <- round(100 * f$current / f$target, 1)
    tagList(
      div(
        class = "te-cards",
        div(
          class = "te-card",
          div(class = "te-kicker", "Full accrual"),
          div(class = "te-date", pretty_date(f$full)),
          div(class = "te-meta", weeks_remaining_label(f$weeks_to_full))
        ),
        div(
          class = "te-card te-card-done",
          div(class = "te-kicker", "Study completion"),
          div(class = "te-date", pretty_date(f$completion)),
          div(class = "te-meta", followup_label(f$followup))
        )
      ),
      div(
        class = "te-progress-label",
        span(paste0(f$current, " of ", f$target, " enrolled")),
        span(paste0(pct, "%"))
      ),
      div(
        class = "te-progress",
        div(class = "te-progress-fill", style = sprintf("width: %s%%;", pct))
      ),
      p(
        class = "te-rate",
        paste0(
          "Observed accrual is ", rate_label,
          " patients per week since ", pretty_date(f$open), "."
        )
      ),
      div(
        class = "te-plot-card",
        plotOutput("accrual_plot", height = "380px"),
        p(
          class = "te-note",
          "The solid line connects zero participants on the open date to enrollment today.",
          "The dashed line holds that weekly rate constant until the accrual target.",
          "The orange segment is follow-up after the target is met,",
          "and the coral dashes mark the target."
        )
      )
    )
  })

  output$accrual_plot <- renderPlot({
    f <- forecast()
    build_accrual_plot(
      f$today, f$open, f$current, f$target, f$full, f$completion
    )
  }, res = 120)
}

shinyApp(ui = ui, server = server)
