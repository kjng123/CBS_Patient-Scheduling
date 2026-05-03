library(shiny)
library(ggplot2)
library(scales)

# =============================
# COLORS
# =============================
COL_SERVED        <- "#16a34a"
COL_BALKED_NOSLOT <- "#ef4444"
COL_BALKED_WAIT   <- "#f97316"
COL_CANCELLED     <- "#f59e0b"
COL_NOSHOW        <- "#8b5cf6"
COL_ARRIVALS      <- "#2563eb"

OUTCOME_COLORS <- c(
  served        = COL_SERVED,
  cancelled     = COL_CANCELLED,
  noshow        = COL_NOSHOW,
  balked_noslot = COL_BALKED_NOSLOT,
  balked_wait   = COL_BALKED_WAIT
)

OUTCOME_LABELS <- c(
  served        = "Served",
  cancelled     = "Cancelled before appt",
  noshow        = "No-show",
  balked_noslot = "Balked — no slot in window",
  balked_wait   = "Balked — wait too long"
)

# =============================
# HELPERS
# =============================
exp_rand <- function(rate) -log(runif(1)) / rate

roll_avg <- function(x, w = 7L) {
  n <- length(x)
  vapply(seq_len(n), function(i) mean(x[max(1, i - w + 1):i]), numeric(1))
}

# =============================
# SIMULATION ENGINE
# =============================
run_simulation <- function(rate1, C,
                           alpha,
                           gamma,
                           delta0, delta_w,
                           sim_days, window_days,
                           p_retry_full,
                           p_retry_balk,
                           p_retry_dropoff) {
  
  total_days <- sim_days + window_days + 10
  slots_used <- integer(total_days)
  
  buf_size     <- max(2000L, as.integer(rate1 * sim_days * 4))
  out_arr_day  <- integer(buf_size)
  out_appt_day <- integer(buf_size)
  out_outcome  <- character(buf_size)
  out_wait     <- integer(buf_size)
  n_out <- 0L
  
  add_outcome <- function(arr_day, appt_day, oc, wait) {
    if (n_out >= buf_size) {
      buf_size     <<- buf_size * 2L
      length(out_arr_day)  <<- buf_size
      length(out_appt_day) <<- buf_size
      length(out_outcome)  <<- buf_size
      length(out_wait)     <<- buf_size
    }
    n_out <<- n_out + 1L
    out_arr_day[n_out]  <<- arr_day
    out_appt_day[n_out] <<- if (is.na(appt_day)) NA_integer_ else appt_day
    out_outcome[n_out]  <<- oc
    out_wait[n_out]     <<- wait
  }
  
  BUF <- max(500L, as.integer(C * window_days * 2))
  bk_arr  <- integer(BUF)
  bk_slot <- integer(BUF)
  bk_can  <- logical(BUF)
  bk_n    <- 0L
  
  book_patient <- function(arr_day, slot_day) {
    if (bk_n >= BUF) {
      BUF <<- BUF * 2L
      length(bk_arr)  <<- BUF
      length(bk_slot) <<- BUF
      length(bk_can)  <<- BUF
    }
    wait     <- slot_day - arr_day
    p_cancel <- 1 - exp(-gamma * wait)
    bk_n <<- bk_n + 1L
    bk_arr[bk_n]  <<- arr_day
    bk_slot[bk_n] <<- slot_day
    bk_can[bk_n]  <<- (runif(1) < p_cancel)
    slots_used[slot_day] <<- slots_used[slot_day] + 1L
  }
  
  rt_arr_full  <- integer(0)
  rt_arr_balk  <- integer(0)
  rt_day_full  <- integer(0)
  rt_day_balk  <- integer(0)
  
  d_arr          <- integer(sim_days)
  d_serv         <- integer(sim_days)
  d_balk_noslot  <- integer(sim_days)
  d_balk_wait    <- integer(sim_days)
  d_canc         <- integer(sim_days)
  d_nosh         <- integer(sim_days)
  d_horiz        <- integer(sim_days)
  d_retry        <- integer(sim_days)
  d_util         <- numeric(sim_days)
  
  for (day in seq_len(sim_days)) {
    
    # 1. CANCELLATIONS
    if (bk_n > 0L) {
      keep <- rep(TRUE, bk_n)
      for (i in seq_len(bk_n)) {
        if (bk_can[i] && bk_slot[i] > day) {
          slots_used[bk_slot[i]] <- slots_used[bk_slot[i]] - 1L
          wait <- bk_slot[i] - bk_arr[i]
          add_outcome(bk_arr[i], bk_slot[i], "cancelled", wait)
          d_canc[day] <- d_canc[day] + 1L
          keep[i] <- FALSE
        }
      }
      if (!all(keep)) {
        bk_arr  <- bk_arr[keep]
        bk_slot <- bk_slot[keep]
        bk_can  <- bk_can[keep]
        bk_n    <- sum(keep)
      }
    }
    
    # 2. SERVE / NO-SHOW
    if (bk_n > 0L) {
      today <- which(bk_slot[1:bk_n] == day)
      if (length(today) > 0L) {
        for (i in today) {
          wait <- day - bk_arr[i]
          p_ns <- 1 - exp(-(delta0 + delta_w * wait))
          if (runif(1) < p_ns) {
            add_outcome(bk_arr[i], day, "noshow", wait)
            d_nosh[day] <- d_nosh[day] + 1L
          } else {
            add_outcome(bk_arr[i], day, "served", wait)
            d_serv[day] <- d_serv[day] + 1L
          }
        }
        keep <- rep(TRUE, bk_n)
        keep[today] <- FALSE
        bk_arr  <- bk_arr[keep]
        bk_slot <- bk_slot[keep]
        bk_can  <- bk_can[keep]
        bk_n    <- sum(keep)
      }
    }
    
    d_util[day] <- d_serv[day] / C
    
    # 3. BOOKING HORIZON
    h <- 1L
    while (h <= window_days &&
           (day + h) <= total_days &&
           slots_used[day + h] >= C) h <- h + 1L
    d_horiz[day] <- if (h > window_days) NA_integer_ else h
    
    # 4. RETRY ATTEMPTS
    process_retries <- function(arr_days, balk_days, p0) {
      n <- length(arr_days)
      if (n == 0L) return(list(arr = arr_days, bday = balk_days))
      still <- rep(TRUE, n)
      for (i in seq_len(n)) {
        k       <- day - balk_days[i]
        p_today <- p0 * (1 - p_retry_dropoff)^(k - 1)
        if (p_today < 0.001) { still[i] <- FALSE; next }
        if (runif(1) < p_today) {
          still[i] <- FALSE
          d_retry[day] <<- d_retry[day] + 1L
          sd <- day + 1L
          while (sd <= day + window_days && slots_used[sd] >= C) sd <- sd + 1L
          if (sd <= day + window_days) {
            if (runif(1) < exp(-alpha * (sd - day))) {
              book_patient(arr_days[i], sd)
            }
          }
        }
      }
      list(arr = arr_days[still], bday = balk_days[still])
    }
    
    res1 <- process_retries(rt_arr_full, rt_day_full, p_retry_full)
    rt_arr_full <- res1$arr; rt_day_full <- res1$bday
    
    res2 <- process_retries(rt_arr_balk, rt_day_balk, p_retry_balk)
    rt_arr_balk <- res2$arr; rt_day_balk <- res2$bday
    
    # 5. NEW ARRIVALS
    t <- exp_rand(rate1)
    while (t < 1) {
      d_arr[day] <- d_arr[day] + 1L
      
      sd <- day + 1L
      while (sd <= day + window_days && slots_used[sd] >= C) sd <- sd + 1L
      
      if (sd > day + window_days) {
        add_outcome(day, NA_integer_, "balked_noslot", 0L)
        d_balk_noslot[day] <- d_balk_noslot[day] + 1L
        rt_arr_full <- c(rt_arr_full, day)
        rt_day_full <- c(rt_day_full, day)
      } else {
        wait <- sd - day
        if (runif(1) < exp(-alpha * wait)) {
          book_patient(day, sd)
        } else {
          add_outcome(day, NA_integer_, "balked_wait", wait)
          d_balk_wait[day] <- d_balk_wait[day] + 1L
          rt_arr_balk <- c(rt_arr_balk, day)
          rt_day_balk <- c(rt_day_balk, day)
        }
      }
      
      t <- t + exp_rand(rate1)
    }
    
  }
  
  outcomes_df <- data.frame(
    arr_day  = out_arr_day[1:n_out],
    appt_day = out_appt_day[1:n_out],
    outcome  = factor(out_outcome[1:n_out],
                      levels = c("served", "cancelled", "noshow",
                                 "balked_noslot", "balked_wait")),
    wait     = out_wait[1:n_out]
  )
  
  daily_df <- data.frame(
    day           = seq_len(sim_days),
    arrivals      = d_arr,
    served        = d_serv,
    balked_noslot = d_balk_noslot,
    balked_wait   = d_balk_wait,
    cancelled     = d_canc,
    noshow        = d_nosh,
    horizon       = d_horiz,
    retries       = d_retry,
    util          = d_util
  )
  
  list(outcomes = outcomes_df, daily = daily_df)
}

# =============================
# UI
# =============================
ui <- fluidPage(
  titlePanel("Patient Appointment Scheduling Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Demand"),
      sliderInput("rate1", "Patient arrival rate (patients/day)", 0, 30, 8, step = 0.5),
      sliderInput("C",     "Daily appointment capacity (slots/day)", 1, 40, 10),
      
      hr(),
      h4("Patient behaviour"),
      sliderInput("alpha",
                  HTML("Wait sensitivity &alpha;<br/>
                        <small>Prob. of accepting slot = e<sup>-&alpha; &times; wait</sup>.
                        Higher = more wait-averse.</small>"),
                  0.001, 0.050, 0.025, step = 0.001),
      uiOutput("alpha_readout"),
      
      hr(),
      h4("Cancellation"),
      sliderInput("gamma",
                  HTML("Cancellation hazard &gamma;<br/>
                        <small>P(cancel) = 1 - e<sup>-&gamma; &times; wait</sup>.
                        Higher = more cancellations on long waits.</small>"),
                  0.001, 0.050, 0.025, step = 0.001),
      # Live readout: implied cancel rate at 3 representative waits
      uiOutput("gamma_readout"),
      
      hr(),
      h4("No-show"),
      sliderInput("delta0",  "Base no-show rate (regardless of wait)",  0.00, 0.5,  0.05, step = 0.01),
      sliderInput("delta_w", "Additional no-show risk per day of wait", 0.00, 0.1,  0.01, step = 0.005),
      
      hr(),
      h4("Scheduling"),
      sliderInput("window_days", "Booking horizon (days ahead patients can book)", 1, 90, 30),
      sliderInput("sim_days",    "Simulation length (days)", 30, 1000, 365),
      
      hr(),
      h4("Retry behaviour"),
      sliderInput("p_retry_full",
                  HTML("Initial retry probability — no slot found<br/>
                        <small>Prob. a blocked patient tries again the next day.</small>"),
                  0, 1, 0.8, step = 0.05),
      sliderInput("p_retry_balk",
                  HTML("Initial retry probability — wait too long<br/>
                        <small>Lower than above: patient already knows waits are long.</small>"),
                  0, 1, 0.3, step = 0.05),
      sliderInput("p_retry_dropoff",
                  HTML("Retry drop-off rate per day<br/>
                        <small>0 = no decay (retries indefinitely).<br/>
                        1 = gives up after one attempt.<br/>
                        Formula: p &times; (1 - dropoff)<sup>k</sup></small>"),
                  0, 1, 0.3, step = 0.05),
      uiOutput("retry_readout"),
      
      hr(),
      actionButton("rerun", "Re-run simulation", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      width = 9,
      
      h3("Summary metrics"),
      fluidRow(
        column(3, wellPanel(uiOutput("m_total"))),
        column(3, wellPanel(uiOutput("m_util"))),
        column(3, wellPanel(uiOutput("m_avg_wait"))),
        column(3, wellPanel(uiOutput("m_balk_rate")))
      ),
      
      h3("Outcome breakdown"),
      fluidRow(
        column(6,
               h4("Proportion of all contacts by outcome"),
               tableOutput("outcome_table")
        ),
        column(6,
               h4("Wait times by outcome"),
               tableOutput("wait_table")
        )
      ),
      
      hr(),
      
      h3("Wait-time distribution by outcome"),
      p("Each bar shows patient contacts by days between initial contact and outcome.
        'Balked — no slot' always has wait = 0 (no slot was ever offered).
        'Balked — wait too long' shows the wait that was offered but rejected."),
      plotOutput("hist", height = "320px"),
      
      hr(),
      h3("Daily throughput (7-day rolling average)"),
      p("Arrivals (blue), patients served (green), no-slot balks (red),
        and wait-averse balks (orange) per day."),
      plotOutput("time", height = "300px"),
      
      hr(),
      h3("Slot availability horizon (7-day rolling average)"),
      p("Days until the first open slot for a new patient arriving today.
        When demand is below capacity this naturally sits near 1 — most future
        slots are empty and available immediately. The horizon only climbs toward
        the booking window limit when sustained demand fills slots well in advance."),
      plotOutput("horizon", height = "280px"),
      
      hr(),
      h3("Daily utilization (7-day rolling average)"),
      p("Fraction of daily capacity used by patients who showed up (served / capacity).
        Values below 100% reflect no-shows and cancellations leaving slots unfilled."),
      plotOutput("util_plot", height = "250px")
    )
  )
)

# =============================
# SERVER
# =============================
server <- function(input, output) {
  
  # ---- Live gamma readout ----
  output$gamma_readout <- renderUI({
    g <- input$gamma
    waits <- c(1, 5, 10)
    pcts  <- round((1 - exp(-g * waits)) * 100, 1)
    tagList(
      tags$div(
        style = paste0(
          "margin-top:6px; padding:8px 10px; border-radius:6px;",
          "background:#fff8e1; border:1px solid #f59e0b;",
          "font-size:12px; color:#555; line-height:1.8;"
        ),
        tags$b("Implied cancellation rate:"),
        tags$br(),
        sprintf("1-day wait:  %s%%", pcts[1]),
        tags$br(),
        sprintf("5-day wait:  %s%%", pcts[2]),
        tags$br(),
        sprintf("10-day wait: %s%%", pcts[3])
      )
    )
  })
  
  output$alpha_readout <- renderUI({
    g <- input$alpha
    waits <- c(1, 5, 10)
    pcts  <- round((1 - exp(-g * waits)) * 100, 1)
    tagList(
      tags$div(
        style = paste0(
          "margin-top:6px; padding:8px 10px; border-radius:6px;",
          "background:#fff8e1; border:1px solid #f59e0b;",
          "font-size:12px; color:#555; line-height:1.8;"
        ),
        tags$b("Implied rejection rate:"),
        tags$br(),
        sprintf("1-day wait:  %s%%", pcts[1]),
        tags$br(),
        sprintf("5-day wait:  %s%%", pcts[2]),
        tags$br(),
        sprintf("10-day wait: %s%%", pcts[3])
      )
    )
  })
  
  output$retry_readout <- renderUI({
    d      <- input$p_retry_dropoff
    p_full <- input$p_retry_full
    p_balk <- input$p_retry_balk
    
    total_tries <- function(p0) {
      if (p0 < 0.001) return(0)
      if (d < 0.001) return(Inf)
      p <- p0; tries <- 0
      while (p >= 0.001) { tries <- tries + p; p <- p * (1 - d) }
      round(tries, 1)
    }
    
    fmt_line <- function(p0) {
      t <- total_tries(p0)
      if (is.infinite(t)) return("retries indefinitely")
      if (t < 0.1)        return("almost never retries")
      paste0("~", t, " expected retry attempts total")
    }
    
    tagList(
      tags$div(
        style = paste0(
          "margin-top:6px; padding:8px 10px; border-radius:6px;",
          "background:#f0f9ff; border:1px solid #2563eb;",
          "font-size:12px; color:#555; line-height:1.8;"
        ),
        tags$b("Expected retries per balked patient:"), tags$br(),
        tags$b("No-slot:   "), fmt_line(p_full), tags$br(),
        tags$b("Wait-averse: "), fmt_line(p_balk)
      )
    )
  })
  
  sim <- reactive({
    input$rerun
    isolate({
      run_simulation(
        rate1           = input$rate1,
        C               = input$C,
        alpha           = input$alpha,
        gamma           = input$gamma,
        delta0          = input$delta0,
        delta_w         = input$delta_w,
        sim_days        = input$sim_days,
        window_days     = input$window_days,
        p_retry_full    = input$p_retry_full,
        p_retry_balk    = input$p_retry_balk,
        p_retry_dropoff = input$p_retry_dropoff
      )
    })
  })
  
  # ---- KPI boxes ----
  make_kpi <- function(label, value, sub = NULL) {
    tagList(
      tags$div(style = "font-size:13px; color:#666;", label),
      tags$div(style = "font-size:28px; font-weight:bold; line-height:1.2;", value),
      if (!is.null(sub)) tags$div(style = "font-size:12px; color:#999;", sub)
    )
  }
  
  output$m_total <- renderUI({
    make_kpi("Total patient contacts",
             formatC(nrow(sim()$outcomes), format = "d", big.mark = ","))
  })
  
  output$m_util <- renderUI({
    make_kpi("Mean daily utilization",
             paste0(round(mean(sim()$daily$util, na.rm = TRUE) * 100, 1), "%"),
             "served / capacity (excludes no-shows)")
  })
  
  output$m_avg_wait <- renderUI({
    oc     <- sim()$outcomes
    served <- oc[oc$outcome == "served", ]
    make_kpi("Mean wait (served patients)",
             paste0(round(mean(served$wait, na.rm = TRUE), 1), " days"),
             "from first contact to appointment")
  })
  
  output$m_balk_rate <- renderUI({
    oc     <- sim()$outcomes
    n_balk <- sum(oc$outcome %in% c("balked_noslot", "balked_wait"))
    make_kpi("Overall balk rate",
             paste0(round(n_balk / nrow(oc) * 100, 1), "%"),
             "contacts that never booked")
  })
  
  # ---- Outcome proportion table ----
  output$outcome_table <- renderTable({
    oc   <- sim()$outcomes
    lvls <- c("served", "cancelled", "noshow", "balked_noslot", "balked_wait")
    labs <- c("Served", "Cancelled", "No-show",
              "Balked — no slot", "Balked — wait too long")
    counts <- sapply(lvls, function(l) sum(oc$outcome == l))
    data.frame(
      Outcome         = labs,
      Count           = formatC(counts, format = "d", big.mark = ","),
      `% of contacts` = paste0(round(counts / nrow(oc) * 100, 1), "%"),
      check.names     = FALSE
    )
  }, striped = TRUE, hover = TRUE)
  
  # ---- Wait by outcome table ----
  output$wait_table <- renderTable({
    oc    <- sim()$outcomes
    lvls  <- c("served", "cancelled", "noshow", "balked_noslot", "balked_wait")
    labs  <- c("Served", "Cancelled", "No-show",
               "Balked — no slot", "Balked — wait too long")
    notes <- c("arrival → appointment day",
               "arrival → scheduled appt day",
               "arrival → appointment day",
               "always 0 (no slot offered)",
               "offered wait that was rejected")
    rows <- lapply(seq_along(lvls), function(i) {
      sub <- oc[oc$outcome == lvls[i], ]
      data.frame(
        Outcome            = labs[i],
        `Mean (days)`      = ifelse(nrow(sub) == 0, NA,
                                    round(mean(sub$wait, na.rm = TRUE), 1)),
        `Median (days)`    = ifelse(nrow(sub) == 0, NA,
                                    round(median(sub$wait, na.rm = TRUE), 1)),
        `What is measured` = notes[i],
        check.names        = FALSE
      )
    })
    do.call(rbind, rows)
  }, striped = TRUE, hover = TRUE, na = "—")
  
  # ---- Wait-time histogram ----
  output$hist <- renderPlot({
    oc <- sim()$outcomes
    ggplot(oc, aes(x = wait, fill = outcome)) +
      geom_histogram(binwidth = 1, colour = "white", linewidth = 0.2) +
      scale_fill_manual(values = OUTCOME_COLORS, labels = OUTCOME_LABELS, drop = FALSE) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Days from initial contact to outcome",
           y = "Number of patient contacts",
           fill = "Outcome") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right", panel.grid.minor = element_blank())
  })
  
  # ---- Daily throughput ----
  output$time <- renderPlot({
    d  <- sim()$daily
    df <- data.frame(
      day    = rep(d$day, 4),
      value  = c(roll_avg(d$arrivals),
                 roll_avg(d$served),
                 roll_avg(d$balked_noslot),
                 roll_avg(d$balked_wait)),
      series = rep(c("Arrivals", "Served",
                     "Balked — no slot", "Balked — wait too long"),
                   each = nrow(d))
    )
    df$series <- factor(df$series,
                        levels = c("Arrivals", "Served",
                                   "Balked — no slot", "Balked — wait too long"))
    ggplot(df, aes(x = day, y = value, colour = series)) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = c(
        "Arrivals"               = COL_ARRIVALS,
        "Served"                 = COL_SERVED,
        "Balked — no slot"       = COL_BALKED_NOSLOT,
        "Balked — wait too long" = COL_BALKED_WAIT
      )) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Simulation day", y = "Patients per day (7-day avg)", colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
  })
  
  # ---- Booking horizon ----
  output$horizon <- renderPlot({
    d   <- sim()$daily
    raw <- ifelse(is.na(d$horizon), input$window_days, d$horizon)
    avg <- roll_avg(raw)
    df  <- data.frame(day = d$day, horizon = avg)
    ggplot(df, aes(x = day, y = horizon)) +
      geom_line(colour = COL_ARRIVALS, linewidth = 0.9) +
      geom_hline(yintercept = input$window_days, linetype = "dashed",
                 colour = COL_BALKED_NOSLOT, alpha = 0.7) +
      annotate("text", x = max(d$day) * 0.02, y = input$window_days,
               label = "Booking window limit", vjust = -0.5,
               colour = COL_BALKED_NOSLOT, size = 3.5, hjust = 0) +
      scale_y_continuous(limits = c(0, input$window_days + 2),
                         breaks = scales::pretty_breaks(6)) +
      labs(x = "Simulation day",
           y = "Days until first available slot (7-day avg)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
  
  # ---- Utilization ----
  output$util_plot <- renderPlot({
    d  <- sim()$daily
    df <- data.frame(day = d$day, util = roll_avg(d$util))
    ggplot(df, aes(x = day, y = util)) +
      geom_line(colour = COL_SERVED, linewidth = 0.9) +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "#666", alpha = 0.7) +
      annotate("text", x = max(d$day) * 0.02, y = 1,
               label = "Full capacity", vjust = -0.5,
               colour = "#666", size = 3.5, hjust = 0) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, max(1.05, max(df$util, na.rm = TRUE) + 0.05)),
        breaks = scales::pretty_breaks(6)
      ) +
      labs(x = "Simulation day",
           y = "Utilization (served / capacity, 7-day avg)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
}

shinyApp(ui, server)
