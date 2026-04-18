# ============================================================
# Patient Scheduling Simulation — Shiny App
# Columbia-Presbyterian Hospital / Columbia Business School
# ============================================================
#
# Required packages (run once to install):
#   install.packages(c("shiny", "ggplot2"))
#
# To run in RStudio:
#   Open this file → click "Run App" in the top-right of the editor
#   OR call: shiny::runApp("app.R")
#
# Note on ggplot2 version:
#   Uses `linewidth` aesthetic (ggplot2 >= 3.4.0).
#   If you have an older version, replace `linewidth =` with `size =`
#   in the two geom_line() calls inside the time-series plot.
# ============================================================

library(shiny)
library(ggplot2)

# ── Brand colors (matching simulation.html) ──────────────────────────────────
COL_SERVED    <- "#16a34a"
COL_BALKED    <- "#ef4444"
COL_CANCELLED <- "#f59e0b"
COL_NOSHOW    <- "#8b5cf6"
COL_ARRIVALS  <- "#2563eb"

OUTCOME_COLORS <- c(
  served    = COL_SERVED,
  cancelled = COL_CANCELLED,
  noshow    = COL_NOSHOW,
  balked    = COL_BALKED
)

# ============================================================
# SIMULATION ENGINE
# ============================================================

# Poisson inter-arrival time: t = -ln(U) / rate
# Since E[-ln(U)] = 1, we get E[t] = 1/rate  ✓
# This is the same formula from the professor's notes, just R syntax.
exp_rand <- function(rate) -log(runif(1) + 1e-300) / rate

# 7-day rolling average (for smoothing the time-series chart)
roll_avg <- function(x, w = 7L) {
  n <- length(x)
  vapply(seq_len(n), function(i) mean(x[max(1L, i - w + 1L):i]), numeric(1))
}

run_simulation <- function(rate1, C, balk_cut, balk_near, balk_far,
                            cancel_p, ns_cut, ns_near, ns_far, sim_days,
                            window_days, p_retry_0, p_retry_decay) {

  # total_days must cover sim_days + the booking window so future slots exist
  total_days <- as.integer(sim_days) + as.integer(window_days) + 10L
  slots_used <- integer(total_days)   # slots booked on each future day

  # Pre-allocate outcome storage (avoids slow vector growth in hot loop)
  max_out     <- max(1000L, as.integer((rate1 + 2) * sim_days * 3L))
  out_days     <- integer(max_out)
  out_outcome  <- character(max_out)
  out_offered  <- integer(max_out)   # days out when slot was quoted to patient
  n_out        <- 0L

  add_outcome <- function(d, oc, offered) {
    n_out              <<- n_out + 1L
    out_days[n_out]    <<- d
    out_outcome[n_out] <<- oc
    out_offered[n_out] <<- offered   # NA_integer_ for forced-balk (no slot existed)
  }

  # Booked patient lists (small at any moment, so c() growth is fine)
  b_arr     <- integer(0)   # arrival day of each booked patient
  b_slot    <- integer(0)   # appointment slot day of each booked patient
  b_offered <- integer(0)   # offered wait (days out) recorded at booking time

  # Callback / retry pool: patients who balked and may call back later.
  # Stores the day each patient balked so we can compute days-since-balk.
  retry_balk_day <- integer(0)

  # Per-day counters
  d_arr   <- integer(sim_days)
  d_serv  <- integer(sim_days)
  d_balk  <- integer(sim_days)
  d_canc  <- integer(sim_days)
  d_nosh  <- integer(sim_days)
  d_horiz <- integer(sim_days)
  d_retry <- integer(sim_days)   # callbacks that actually called back today

  for (day in seq_len(sim_days)) {

    # ── 1. Cancellations ────────────────────────────────────────────────────
    # Daily cancel probability = cancel_p / days_to_appointment.
    # This concentrates cancellations near the appointment date:
    # a patient 1 day out faces cancel_p; one 14 days out faces cancel_p/14.
    if (length(b_slot) > 0L) {
      keep <- rep(TRUE, length(b_slot))
      for (i in seq_along(b_slot)) {
        if (b_slot[i] > day) {
          dta <- b_slot[i] - day                    # days to appointment (>= 1)
          ecp <- min(0.99, cancel_p / dta)
          if (runif(1L) < ecp) {
            slots_used[b_slot[i]] <- slots_used[b_slot[i]] - 1L
            add_outcome(day - b_arr[i], "cancelled", b_offered[i])
            d_canc[day] <- d_canc[day] + 1L
            keep[i]     <- FALSE
          }
        }
      }
      b_arr     <- b_arr[keep]
      b_slot    <- b_slot[keep]
      b_offered <- b_offered[keep]
    }

    # ── 2. Today's appointments: served or no-show ───────────────────────────
    if (length(b_slot) > 0L) {
      today <- b_slot == day
      if (any(today)) {
        for (i in which(today)) {
          days_out <- day - b_arr[i]
          p_ns <- if (days_out <= ns_cut) ns_near else ns_far
          if (runif(1L) < p_ns) {
            add_outcome(days_out, "noshow", b_offered[i])
            d_nosh[day] <- d_nosh[day] + 1L
          } else {
            add_outcome(days_out, "served", b_offered[i])
            d_serv[day] <- d_serv[day] + 1L
          }
        }
        b_arr     <- b_arr[!today]
        b_slot    <- b_slot[!today]
        b_offered <- b_offered[!today]
      }
    }

    # ── 3. Record booking horizon ────────────────────────────────────────────
    # Capped at window_days: that is the furthest out a patient can book.
    h <- 0L
    while (h < window_days &&
           (day + h) <= total_days &&
           slots_used[day + h] >= C) {
      h <- h + 1L
    }
    d_horiz[day] <- h   # h == window_days means the whole window is full

    # ── 4. Callbacks from retry pool ─────────────────────────────────────────
    # Each patient in the pool called and balked on retry_balk_day[i].
    # On day k since balking, they call back with probability:
    #   p(k) = p_retry_0 * p_retry_decay^(k-1)
    # This is decreasing in k (more likely to call back sooner).
    # Once a patient calls back they leave the pool, whether they book or balk.
    # If p(k) drops below 0.001 we assume they've given up and drop them.
    if (length(retry_balk_day) > 0L) {
      still_waiting <- rep(TRUE, length(retry_balk_day))
      for (i in seq_along(retry_balk_day)) {
        k       <- day - retry_balk_day[i]          # days since balking (>= 1)
        p_today <- p_retry_0 * p_retry_decay^(k - 1L)

        if (p_today < 0.001) {
          still_waiting[i] <- FALSE                 # gave up — drop from pool
        } else if (runif(1L) < p_today) {
          still_waiting[i] <- FALSE                 # calling back today
          d_retry[day] <- d_retry[day] + 1L

          # Attempt to book — same slot-search logic as a fresh arrival
          sd2 <- day
          while (sd2 <= day + window_days &&
                 sd2 <= total_days &&
                 slots_used[sd2] >= C) sd2 <- sd2 + 1L

          if (sd2 <= min(day + window_days, total_days)) {
            days_out2 <- sd2 - day
            p_balk2   <- if (days_out2 <= balk_cut) balk_near else balk_far
            if (runif(1L) >= p_balk2) {
              # Accepted — book the slot (outcome recorded later as served/cancel/noshow)
              slots_used[sd2] <- slots_used[sd2] + 1L
              b_arr     <- c(b_arr,     day)
              b_slot    <- c(b_slot,    sd2)
              b_offered <- c(b_offered, days_out2)
            }
            # If they balk again: they leave the pool (still_waiting already FALSE)
          }
        }
      }
      retry_balk_day <- retry_balk_day[still_waiting]
    }

    # ── 5. Poisson arrivals via exponential inter-arrival times ──────────────
    # Generate t_1 = -ln(U_1)/rate, then t_1 + t_2, etc., until sum >= 1 day.
    if (rate1 > 0) {
      t <- exp_rand(rate1)
      while (t < 1) {
        d_arr[day] <- d_arr[day] + 1L

        # Find first available slot within the rolling window [day, day+window_days]
        sd <- day
        while (sd <= day + window_days &&
               sd <= total_days &&
               slots_used[sd] >= C) sd <- sd + 1L

        if (sd > min(day + window_days, total_days)) {
          # No slot available within window — no offer made, patient balks
          add_outcome(0L, "balked", NA_integer_)
          d_balk[day] <- d_balk[day] + 1L
          # Still add to retry pool — they may call back when a slot opens up
          retry_balk_day <- c(retry_balk_day, day)
        } else {
          days_out <- sd - day   # the offered wait — recorded regardless of decision
          p_balk <- if (days_out <= balk_cut) balk_near else balk_far

          if (runif(1L) < p_balk) {
            # Patient was quoted days_out but chose to balk
            add_outcome(0L, "balked", days_out)
            d_balk[day] <- d_balk[day] + 1L
            retry_balk_day <- c(retry_balk_day, day)   # enters callback pool
          } else {
            slots_used[sd] <- slots_used[sd] + 1L
            b_arr     <- c(b_arr,     day)
            b_slot    <- c(b_slot,    sd)
            b_offered <- c(b_offered, days_out)   # carry offered wait forward
          }
        }

        t <- t + exp_rand(rate1)
      }
    }
  }

  # Return tidy data frames
  list(
    outcomes = data.frame(
      days    = out_days[seq_len(n_out)],
      outcome = factor(out_outcome[seq_len(n_out)],
                       levels = c("served", "cancelled", "noshow", "balked")),
      offered = out_offered[seq_len(n_out)],   # days out when slot was quoted (NA = no slot existed)
      stringsAsFactors = FALSE
    ),
    daily = data.frame(
      day       = seq_len(sim_days),
      arrivals  = d_arr,
      served    = d_serv,
      balked    = d_balk,
      cancelled = d_canc,
      noshow    = d_nosh,
      horizon   = d_horiz,
      retries   = d_retry   # patients from callback pool who called back that day
    )
  )
}

# ============================================================
# SHARED ggplot2 THEME
# ============================================================

sim_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "#e2e8f0", linewidth = 0.4),
      axis.text         = element_text(color = "#64748b", size = 9),
      axis.title        = element_text(color = "#64748b", size = 9),
      legend.text       = element_text(size = 9),
      legend.key.size   = unit(10, "pt"),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      plot.background   = element_blank(),
      panel.background  = element_blank()
    )
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(

  tags$head(tags$style(HTML("
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      background: #f1f5f9;
    }
    /* Sidebar card */
    .well {
      background: #ffffff !important;
      border: 1px solid #e2e8f0 !important;
      border-radius: 10px !important;
      box-shadow: none !important;
    }
    /* Section labels inside sidebar */
    .section-lbl {
      font-size: 10px; font-weight: 700; text-transform: uppercase;
      letter-spacing: 0.08em; color: #94a3b8;
      margin: 16px 0 4px; padding-bottom: 4px;
      border-bottom: 1px solid #f1f5f9;
    }
    /* Slider accent color */
    .irs--shiny .irs-bar {
      background: #2563eb;
      border-top: 1px solid #2563eb;
      border-bottom: 1px solid #2563eb;
    }
    .irs--shiny .irs-handle { background: #2563eb; border: 2px solid #1d4ed8; }
    .irs--shiny .irs-from,
    .irs--shiny .irs-to,
    .irs--shiny .irs-single { background: #2563eb; }
    /* Stat boxes */
    .stat-box {
      background: #fff; border: 1px solid #e2e8f0; border-radius: 8px;
      padding: 12px 8px; text-align: center; margin-bottom: 10px;
    }
    .stat-val { font-size: 24px; font-weight: 700; }
    .stat-lbl {
      font-size: 9px; font-weight: 700; text-transform: uppercase;
      letter-spacing: 0.06em; margin-top: 2px;
    }
    .stat-pct { font-size: 11px; color: #94a3b8; margin-top: 2px; }
    /* Stat row */
    .stat-row-box {
      background: #fff; border: 1px solid #e2e8f0; border-radius: 8px;
      padding: 10px 16px; margin-bottom: 10px;
      font-size: 12px; color: #64748b;
    }
    /* Chart cards */
    .chart-box {
      background: #fff; border: 1px solid #e2e8f0; border-radius: 10px;
      padding: 14px 16px; margin-bottom: 12px;
    }
    .chart-title {
      font-size: 11px; font-weight: 600; text-transform: uppercase;
      letter-spacing: 0.06em; color: #64748b; margin-bottom: 6px;
    }
    /* Re-run button */
    #rerun {
      width: 100%; background: #2563eb; color: white; border: none;
      border-radius: 8px; padding: 10px; font-weight: 600;
      margin-top: 10px; font-size: 13px;
    }
    #rerun:hover { background: #1d4ed8; }
    /* Hint text */
    .hint-txt { font-size: 10px; color: #94a3b8; text-align: center;
                margin-top: 5px; }
  "))),

  # ── Header band ──────────────────────────────────────────────────────────
  tags$div(
    style = paste(
      "background: linear-gradient(135deg, #1e40af, #2563eb);",
      "color: white; padding: 16px 24px; margin-bottom: 16px;",
      "border-radius: 0 0 10px 10px;"
    ),
    tags$h1("Patient Scheduling Simulation",
            style = "font-size: 20px; font-weight: 700; margin: 0 0 4px;"),
    tags$p(
      "Columbia-Presbyterian Hospital \u2014 Outpatient Appointment Scheduling \u00b7 R / Shiny",
      style = "font-size: 11px; opacity: 0.75; margin: 0;"
    )
  ),

  sidebarLayout(

    # ── Left: Controls ──────────────────────────────────────────────────────
    sidebarPanel(
      width = 3,

      tags$div(class = "section-lbl", "Arrival Process (Poisson)"),
      sliderInput("rate1", "Arrival rate (patients / day)",
                  min = 0, max = 30, value = 5, step = 0.5),

      tags$div(class = "section-lbl", "Schedule Capacity"),
      sliderInput("C", "Slots per day (C)",
                  min = 1, max = 40, value = 8, step = 1),

      tags$div(class = "section-lbl", "Balking \u2014 Patient Rejects Offered Slot"),
      sliderInput("balk_cut",  "Cutoff (days out)",
                  min = 0, max = 60, value = 7, step = 1),
      sliderInput("balk_near", "Balk prob if wait \u2264 cutoff",
                  min = 0, max = 1, value = 0.10, step = 0.01),
      sliderInput("balk_far",  "Balk prob if wait > cutoff",
                  min = 0, max = 1, value = 0.50, step = 0.01),

      tags$div(class = "section-lbl", "Cancellation"),
      sliderInput("cancel_p", "Peak cancel prob (day before appt)",
                  min = 0, max = 0.5, value = 0.05, step = 0.01),
      tags$p("Daily prob = peak \u00f7 days remaining",
             style = "font-size: 10px; color: #94a3b8; margin-top: -6px;"),

      tags$div(class = "section-lbl", "No-Show (on appointment day)"),
      sliderInput("ns_cut",  "Cutoff (days booked in advance)",
                  min = 0, max = 60, value = 14, step = 1),
      sliderInput("ns_near", "No-show prob if booked \u2264 cutoff",
                  min = 0, max = 1, value = 0.10, step = 0.01),
      sliderInput("ns_far",  "No-show prob if booked > cutoff",
                  min = 0, max = 1, value = 0.20, step = 0.01),

      tags$div(class = "section-lbl", "Booking Window"),
      sliderInput("window_days", "Max days out a patient can book",
                  min = 1, max = 90, value = 30, step = 1),
      tags$p("Appointments beyond this horizon are unavailable to new patients.",
             style = "font-size: 10px; color: #94a3b8; margin-top: -6px;"),

      tags$div(class = "section-lbl", "Callback / Retry"),
      sliderInput("p_retry_0", "Prob of calling back next day (k = 1)",
                  min = 0, max = 1, value = 0.30, step = 0.01),
      sliderInput("p_retry_decay", "Daily decay factor",
                  min = 0, max = 1, value = 0.70, step = 0.01),
      tags$p("Retry prob on day k: p\u2080 \u00d7 decay^(k\u22121). Patients with p < 0.001 give up.",
             style = "font-size: 10px; color: #94a3b8; margin-top: -6px;"),

      tags$div(class = "section-lbl", "Simulation Length"),
      sliderInput("sim_days", "Days to simulate",
                  min = 30, max = 1000, value = 365, step = 10),

      actionButton("rerun", "\u25b6  Re-run (new random seed)"),
      tags$p("Sliders auto-update \u00b7 button re-randomizes",
             class = "hint-txt")
    ),

    # ── Right: Charts ───────────────────────────────────────────────────────
    mainPanel(
      width = 9,

      # Summary stat boxes
      fluidRow(
        column(3, uiOutput("stat_served")),
        column(3, uiOutput("stat_balked")),
        column(3, uiOutput("stat_cancelled")),
        column(3, uiOutput("stat_noshow"))
      ),
      tags$div(class = "stat-row-box", uiOutput("stat_row")),

      # Chart 1: Outcome histogram
      tags$div(class = "chart-box",
        tags$div(class = "chart-title",
          "Days from Arrival to Service or Quit \u2014 Distribution"),
        plotOutput("hist_plot", height = "240px")
      ),

      # Chart 2: Daily time series
      tags$div(class = "chart-box",
        tags$div(class = "chart-title",
          "Daily Counts Over Simulation Time (7-day rolling average)"),
        plotOutput("time_plot", height = "210px")
      ),

      # Chart 3: Booking horizon
      tags$div(class = "chart-box",
        tags$div(class = "chart-title",
          "Schedule Backlog \u2014 Days Until Next Free Slot"),
        plotOutput("horizon_plot", height = "175px")
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # ── Reactive simulation result ─────────────────────────────────────────────
  # Runs automatically whenever any slider changes, OR when Re-run is clicked.
  sim_result <- reactive({
    # Declare all reactive dependencies explicitly
    rate1        <- input$rate1
    C            <- input$C
    balk_cut     <- input$balk_cut
    balk_near    <- input$balk_near
    balk_far     <- input$balk_far
    cancel_p     <- input$cancel_p
    ns_cut       <- input$ns_cut
    ns_near      <- input$ns_near
    ns_far       <- input$ns_far
    sim_days     <- input$sim_days
    window_days  <- input$window_days
    p_retry_0    <- input$p_retry_0
    p_retry_decay <- input$p_retry_decay
    input$rerun   # <- makes the button invalidate the cache (new random seed)

    run_simulation(rate1, C, balk_cut, balk_near, balk_far,
                   cancel_p, ns_cut, ns_near, ns_far, sim_days,
                   window_days, p_retry_0, p_retry_decay)
  })

  # ── Helper: build a stat box ───────────────────────────────────────────────
  stat_box <- function(result, oc, color, label) {
    n     <- nrow(result$outcomes)
    count <- sum(result$outcomes$outcome == oc)
    pct   <- if (n > 0) sprintf("%.1f%%", 100 * count / n) else "\u2014"
    fmt   <- if (count >= 1000) sprintf("%.1fk", count / 1000) else as.character(count)
    tags$div(class = "stat-box",
      tags$div(class = "stat-val", style = paste0("color:", color), fmt),
      tags$div(class = "stat-lbl", style = paste0("color:", color), label),
      tags$div(class = "stat-pct", pct)
    )
  }

  output$stat_served    <- renderUI(stat_box(sim_result(), "served",    COL_SERVED,    "Served"))
  output$stat_balked    <- renderUI(stat_box(sim_result(), "balked",    COL_BALKED,    "Balked"))
  output$stat_cancelled <- renderUI(stat_box(sim_result(), "cancelled", COL_CANCELLED, "Cancelled"))
  output$stat_noshow    <- renderUI(stat_box(sim_result(), "noshow",    COL_NOSHOW,    "No-Show"))

  # ── Summary row ────────────────────────────────────────────────────────────
  output$stat_row <- renderUI({
    res    <- sim_result()
    oc     <- res$outcomes
    n      <- nrow(oc)
    served <- oc[oc$outcome == "served", ]

    # Avg wait (served): slot_day - arrival_day for patients who actually attended
    avg_served <- if (nrow(served) > 0)
      sprintf("%.1f", mean(served$days)) else "\u2014"

    # Avg offered wait: the days-out figure quoted to EVERY patient who received
    # a slot offer (balked or not, showed or not). NA = no slot existed at all.
    # This is what the scheduling policy directly controls.
    offered_vals <- oc$offered[!is.na(oc$offered)]
    avg_offered  <- if (length(offered_vals) > 0)
      sprintf("%.1f", mean(offered_vals)) else "\u2014"

    # Avg offered wait broken out by decision (accepted vs balked)
    avg_offered_accepted <- {
      v <- oc$offered[oc$outcome != "balked" & !is.na(oc$offered)]
      if (length(v) > 0) sprintf("%.1f", mean(v)) else "\u2014"
    }
    avg_offered_balked <- {
      v <- oc$offered[oc$outcome == "balked" & !is.na(oc$offered)]
      if (length(v) > 0) sprintf("%.1f", mean(v)) else "\u2014"
    }

    util <- if (input$C > 0 && input$sim_days > 0)
      sprintf("%.1f%%",
        100 * sum(oc$outcome == "served") / (input$C * input$sim_days))
    else "\u2014"

    tags$div(style = "display:flex; gap:20px; flex-wrap:wrap; align-items:baseline;",
      tags$span(HTML(paste0(
        "Total arrivals: <strong>", format(n, big.mark = ","), "</strong>"))),
      tags$span(HTML(paste0(
        "Avg wait (served): <strong>", avg_served, " days</strong>"))),
      tags$span(
        style = "border-left: 2px solid #e2e8f0; padding-left: 20px;",
        HTML(paste0(
          "Avg offered wait \u2014 all: <strong>", avg_offered,
          " days</strong> &nbsp;|&nbsp; ",
          "accepted: <strong>", avg_offered_accepted,
          " days</strong> &nbsp;|&nbsp; ",
          "balked: <strong>", avg_offered_balked, " days</strong>"))
      ),
      tags$span(HTML(paste0(
        "Slot utilization: <strong>", util, "</strong>")))
    )
  })

  # ── Chart 1: Outcome histogram ─────────────────────────────────────────────
  output$hist_plot <- renderPlot({
    oc <- sim_result()$outcomes
    if (nrow(oc) == 0) return(NULL)

    oc$days <- pmin(oc$days, 90L)   # cap display at 90 days

    ggplot(oc, aes(x = days, fill = outcome)) +
      geom_histogram(binwidth = 1, position = "stack", color = NA) +
      scale_fill_manual(
        values = OUTCOME_COLORS,
        labels = c(served    = "Served",
                   cancelled = "Cancelled",
                   noshow    = "No-Show",
                   balked    = "Balked (day 0)")
      ) +
      scale_x_continuous(breaks = seq(0, 90, 10),
                         limits = c(-0.5, 90.5),
                         expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "Days from Arrival", y = "Patient Count") +
      sim_theme()
  }, bg = "transparent")

  # ── Chart 2: Daily time series ─────────────────────────────────────────────
  output$time_plot <- renderPlot({
    daily <- sim_result()$daily
    if (nrow(daily) == 0) return(NULL)

    # Build long-format data frame (no tidyr needed)
    series_names  <- c("arrivals", "served",  "balked",   "cancelled", "noshow",  "retries")
    series_labels <- c("Arrivals", "Served",  "Balked",   "Cancelled", "No-Show", "Callbacks")
    series_colors <- c(COL_ARRIVALS, COL_SERVED, COL_BALKED, COL_CANCELLED, COL_NOSHOW, "#0891b2")
    names(series_colors) <- series_labels

    df_long <- do.call(rbind, lapply(seq_along(series_names), function(i) {
      data.frame(
        day    = daily$day,
        series = series_labels[i],
        value  = roll_avg(daily[[series_names[i]]]),
        stringsAsFactors = FALSE
      )
    }))
    df_long$series <- factor(df_long$series, levels = series_labels)

    ggplot(df_long, aes(x = day, y = value, color = series)) +
      # Arrivals drawn on top and slightly thicker for emphasis
      geom_line(data = subset(df_long, series != "Arrivals"),
                linewidth = 0.8) +
      geom_line(data = subset(df_long, series == "Arrivals"),
                linewidth = 1.4) +
      scale_color_manual(values = series_colors) +
      scale_x_continuous(expand = c(0.01, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(x = "Simulation Day", y = "Count (7-day avg)") +
      sim_theme()
  }, bg = "transparent")

  # ── Chart 3: Booking horizon ───────────────────────────────────────────────
  output$horizon_plot <- renderPlot({
    daily <- sim_result()$daily
    if (nrow(daily) == 0) return(NULL)

    df <- data.frame(
      day   = daily$day,
      horiz = roll_avg(daily$horizon)
    )

    ggplot(df, aes(x = day, y = horiz)) +
      geom_area(fill = COL_ARRIVALS, alpha = 0.12) +
      geom_line(color = COL_ARRIVALS, linewidth = 1.2) +
      scale_x_continuous(expand = c(0.01, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(x = "Simulation Day", y = "Days backlog") +
      sim_theme()
  }, bg = "transparent")
}

# ============================================================
shinyApp(ui, server)
