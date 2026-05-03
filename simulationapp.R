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

COL_POP1 <- "#2563eb"
COL_POP2 <- "#db2777"

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
run_simulation <- function(
    rate1, rate2, C,
    alpha1, alpha2,
    gamma1, gamma2,
    delta0_1, delta_w_1,
    delta0_2, delta_w_2,
    sim_days, window_days,
    p_retry_full1, p_retry_balk1, p_retry_dropoff1,
    p_retry_full2, p_retry_balk2, p_retry_dropoff2) {
  
  total_days <- sim_days + window_days + 10
  slots_used <- integer(total_days)
  
  buf_size     <- max(2000L, as.integer((rate1 + rate2) * sim_days * 4))
  out_arr_day  <- integer(buf_size)
  out_appt_day <- integer(buf_size)
  out_outcome  <- character(buf_size)
  out_wait     <- integer(buf_size)
  out_pop      <- integer(buf_size)   # 1 or 2
  n_out <- 0L
  
  add_outcome <- function(arr_day, appt_day, oc, wait, pop) {
    if (n_out >= buf_size) {
      buf_size     <<- buf_size * 2L
      length(out_arr_day)  <<- buf_size
      length(out_appt_day) <<- buf_size
      length(out_outcome)  <<- buf_size
      length(out_wait)     <<- buf_size
      length(out_pop)      <<- buf_size
    }
    n_out <<- n_out + 1L
    out_arr_day[n_out]  <<- arr_day
    out_appt_day[n_out] <<- if (is.na(appt_day)) NA_integer_ else appt_day
    out_outcome[n_out]  <<- oc
    out_wait[n_out]     <<- wait
    out_pop[n_out]      <<- pop
  }
  
  BUF <- max(500L, as.integer(C * window_days * 2))
  bk_arr  <- integer(BUF)
  bk_slot <- integer(BUF)
  bk_can  <- logical(BUF)
  bk_pop  <- integer(BUF)   # population tag
  bk_n    <- 0L
  
  book_patient <- function(arr_day, slot_day, pop) {
    if (bk_n >= BUF) {
      BUF <<- BUF * 2L
      length(bk_arr)  <<- BUF
      length(bk_slot) <<- BUF
      length(bk_can)  <<- BUF
      length(bk_pop)  <<- BUF
    }
    wait   <- slot_day - arr_day
    gamma  <- if (pop == 1L) gamma1 else gamma2
    p_cancel <- 1 - exp(-gamma * wait)
    bk_n <<- bk_n + 1L
    bk_arr[bk_n]  <<- arr_day
    bk_slot[bk_n] <<- slot_day
    bk_can[bk_n]  <<- (runif(1) < p_cancel)
    bk_pop[bk_n]  <<- pop
    slots_used[slot_day] <<- slots_used[slot_day] + 1L
  }
  
  # Retry queues for each pop × balk-type
  rt_arr_full1  <- integer(0); rt_day_full1  <- integer(0)
  rt_arr_balk1  <- integer(0); rt_day_balk1  <- integer(0)
  rt_arr_full2  <- integer(0); rt_day_full2  <- integer(0)
  rt_arr_balk2  <- integer(0); rt_day_balk2  <- integer(0)
  
  # Daily accumulators — split by pop where meaningful
  d_arr1         <- integer(sim_days)
  d_arr2         <- integer(sim_days)
  d_serv1        <- integer(sim_days)
  d_serv2        <- integer(sim_days)
  d_balk_noslot1 <- integer(sim_days)
  d_balk_noslot2 <- integer(sim_days)
  d_balk_wait1   <- integer(sim_days)
  d_balk_wait2   <- integer(sim_days)
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
          add_outcome(bk_arr[i], bk_slot[i], "cancelled", wait, bk_pop[i])
          d_canc[day] <- d_canc[day] + 1L
          keep[i] <- FALSE
        }
      }
      if (!all(keep)) {
        bk_arr  <- bk_arr[keep]
        bk_slot <- bk_slot[keep]
        bk_can  <- bk_can[keep]
        bk_pop  <- bk_pop[keep]
        bk_n    <- sum(keep)
      }
    }
    
    # 2. SERVE / NO-SHOW
    if (bk_n > 0L) {
      today <- which(bk_slot[1:bk_n] == day)
      if (length(today) > 0L) {
        for (i in today) {
          wait  <- day - bk_arr[i]
          pop_i <- bk_pop[i]
          d0    <- if (pop_i == 1L) delta0_1 else delta0_2
          dw    <- if (pop_i == 1L) delta_w_1 else delta_w_2
          p_ns  <- 1 - exp(-(d0 + dw * wait))
          if (runif(1) < p_ns) {
            add_outcome(bk_arr[i], day, "noshow", wait, pop_i)
            d_nosh[day] <- d_nosh[day] + 1L
          } else {
            add_outcome(bk_arr[i], day, "served", wait, pop_i)
            if (pop_i == 1L) d_serv1[day] <- d_serv1[day] + 1L
            else             d_serv2[day] <- d_serv2[day] + 1L
          }
        }
        keep <- rep(TRUE, bk_n)
        keep[today] <- FALSE
        bk_arr  <- bk_arr[keep]
        bk_slot <- bk_slot[keep]
        bk_can  <- bk_can[keep]
        bk_pop  <- bk_pop[keep]
        bk_n    <- sum(keep)
      }
    }
    
    d_util[day] <- (d_serv1[day] + d_serv2[day]) / C
    
    # 3. BOOKING HORIZON
    h <- 1L
    while (h <= window_days && (day + h) <= total_days && slots_used[day + h] >= C) h <- h + 1L
    d_horiz[day] <- if (h > window_days) NA_integer_ else h
    
    # 4. RETRY HELPER
    process_retries <- function(arr_days, balk_days, p0, p_dropoff, alpha, pop) {
      n <- length(arr_days)
      if (n == 0L) return(list(arr = arr_days, bday = balk_days))
      still <- rep(TRUE, n)
      for (i in seq_len(n)) {
        k       <- day - balk_days[i]
        p_today <- p0 * (1 - p_dropoff)^(k - 1)
        if (p_today < 0.001) { still[i] <- FALSE; next }
        if (runif(1) < p_today) {
          still[i] <- FALSE
          d_retry[day] <<- d_retry[day] + 1L
          sd <- day + 1L
          while (sd <= day + window_days && slots_used[sd] >= C) sd <- sd + 1L
          if (sd <= day + window_days) {
            if (runif(1) < exp(-alpha * (sd - day))) {
              book_patient(arr_days[i], sd, pop)
            }
          }
        }
      }
      list(arr = arr_days[still], bday = balk_days[still])
    }
    
    r1f <- process_retries(rt_arr_full1, rt_day_full1, p_retry_full1, p_retry_dropoff1, alpha1, 1L)
    rt_arr_full1 <- r1f$arr; rt_day_full1 <- r1f$bday
    r1b <- process_retries(rt_arr_balk1, rt_day_balk1, p_retry_balk1, p_retry_dropoff1, alpha1, 1L)
    rt_arr_balk1 <- r1b$arr; rt_day_balk1 <- r1b$bday
    
    r2f <- process_retries(rt_arr_full2, rt_day_full2, p_retry_full2, p_retry_dropoff2, alpha2, 2L)
    rt_arr_full2 <- r2f$arr; rt_day_full2 <- r2f$bday
    r2b <- process_retries(rt_arr_balk2, rt_day_balk2, p_retry_balk2, p_retry_dropoff2, alpha2, 2L)
    rt_arr_balk2 <- r2b$arr; rt_day_balk2 <- r2b$bday
    
    # 5. NEW ARRIVALS — helper
    handle_arrival <- function(pop) {
      alpha  <- if (pop == 1L) alpha1 else alpha2
      if (pop == 1L) d_arr1[day] <<- d_arr1[day] + 1L
      else           d_arr2[day] <<- d_arr2[day] + 1L
      
      sd <- day + 1L
      while (sd <= day + window_days && slots_used[sd] >= C) sd <- sd + 1L
      
      if (sd > day + window_days) {
        add_outcome(day, NA_integer_, "balked_noslot", 0L, pop)
        if (pop == 1L) {
          d_balk_noslot1[day] <<- d_balk_noslot1[day] + 1L
          rt_arr_full1 <<- c(rt_arr_full1, day)
          rt_day_full1 <<- c(rt_day_full1, day)
        } else {
          d_balk_noslot2[day] <<- d_balk_noslot2[day] + 1L
          rt_arr_full2 <<- c(rt_arr_full2, day)
          rt_day_full2 <<- c(rt_day_full2, day)
        }
      } else {
        wait <- sd - day
        if (runif(1) < exp(-alpha * wait)) {
          book_patient(day, sd, pop)
        } else {
          add_outcome(day, NA_integer_, "balked_wait", wait, pop)
          if (pop == 1L) {
            d_balk_wait1[day] <<- d_balk_wait1[day] + 1L
            rt_arr_balk1 <<- c(rt_arr_balk1, day)
            rt_day_balk1 <<- c(rt_day_balk1, day)
          } else {
            d_balk_wait2[day] <<- d_balk_wait2[day] + 1L
            rt_arr_balk2 <<- c(rt_arr_balk2, day)
            rt_day_balk2 <<- c(rt_day_balk2, day)
          }
        }
      }
    }
    
    # Pop 1 arrivals
    if (rate1 > 0) {
      t <- exp_rand(rate1)
      while (t < 1) { handle_arrival(1L); t <- t + exp_rand(rate1) }
    }
    # Pop 2 arrivals
    if (rate2 > 0) {
      t <- exp_rand(rate2)
      while (t < 1) { handle_arrival(2L); t <- t + exp_rand(rate2) }
    }
  }
  
  outcomes_df <- data.frame(
    arr_day  = out_arr_day[1:n_out],
    appt_day = out_appt_day[1:n_out],
    outcome  = factor(out_outcome[1:n_out],
                      levels = c("served", "cancelled", "noshow",
                                 "balked_noslot", "balked_wait")),
    wait     = out_wait[1:n_out],
    pop      = factor(out_pop[1:n_out], levels = c(1, 2),
                      labels = c("Population 1", "Population 2"))
  )
  
  daily_df <- data.frame(
    day            = seq_len(sim_days),
    arrivals1      = d_arr1,
    arrivals2      = d_arr2,
    served1        = d_serv1,
    served2        = d_serv2,
    balked_noslot1 = d_balk_noslot1,
    balked_noslot2 = d_balk_noslot2,
    balked_wait1   = d_balk_wait1,
    balked_wait2   = d_balk_wait2,
    cancelled      = d_canc,
    noshow         = d_nosh,
    horizon        = d_horiz,
    retries        = d_retry,
    util           = d_util
  )
  
  list(outcomes = outcomes_df, daily = daily_df)
}

# =============================
# UI HELPERS
# =============================
pop_panel <- function(pop_num, col, defaults) {
  label <- paste0("Population ", pop_num)
  pfx   <- paste0("p", pop_num, "_")
  
  tagList(
    tags$div(
      style = paste0(
        "border-left: 4px solid ", col, ";",
        "padding-left: 10px; margin-bottom: 6px;"
      ),
      tags$h4(label, style = paste0("color:", col, "; margin-top:0;")),
      
      sliderInput(paste0(pfx, "rate"),
                  "Arrival rate (patients/day)",
                  0, 30, defaults$rate, step = 0.5),
      
      sliderInput(paste0(pfx, "alpha"),
                  HTML("Wait sensitivity &alpha;<br/><small>Accept prob = e<sup>-&alpha;&times;wait</sup></small>"),
                  0.001, 0.100, defaults$alpha, step = 0.001),
      uiOutput(paste0(pfx, "alpha_ro")),
      
      hr(),
      sliderInput(paste0(pfx, "gamma"),
                  HTML("Cancellation hazard &gamma;<br/><small>P(cancel) = 1 - e<sup>-&gamma;&times;wait</sup></small>"),
                  0.001, 0.100, defaults$gamma, step = 0.001),
      uiOutput(paste0(pfx, "gamma_ro")),
      
      hr(),
      sliderInput(paste0(pfx, "delta0"),  "Base no-show rate",           0.00, 0.5, defaults$delta0,  step = 0.01),
      sliderInput(paste0(pfx, "delta_w"), "Extra no-show risk per wait-day", 0.00, 0.1, defaults$delta_w, step = 0.005),
      
      hr(),
      sliderInput(paste0(pfx, "p_retry_full"),
                  HTML("Retry prob — no slot found"),
                  0, 1, defaults$p_retry_full, step = 0.05),
      sliderInput(paste0(pfx, "p_retry_balk"),
                  HTML("Retry prob — wait too long"),
                  0, 1, defaults$p_retry_balk, step = 0.05),
      sliderInput(paste0(pfx, "p_retry_dropoff"),
                  HTML("Retry drop-off per day"),
                  0, 1, defaults$p_retry_dropoff, step = 0.05),
      uiOutput(paste0(pfx, "retry_ro"))
    )
  )
}

implied_readout <- function(rate, waits, label, bg, border) {
  pcts <- round((1 - exp(-rate * waits)) * 100, 1)
  tags$div(
    style = paste0(
      "margin-top:4px; padding:7px 10px; border-radius:5px;",
      "background:", bg, "; border:1px solid ", border, ";",
      "font-size:11px; color:#555; line-height:1.8;"
    ),
    tags$b(label), tags$br(),
    sprintf("1-day: %s%%", pcts[1]), tags$br(),
    sprintf("5-day: %s%%", pcts[2]), tags$br(),
    sprintf("10-day: %s%%", pcts[3])
  )
}

retry_readout <- function(p_full, p_balk, dropoff) {
  total_tries <- function(p0) {
    if (p0 < 0.001) return(0)
    if (dropoff < 0.001) return(Inf)
    p <- p0; tries <- 0
    while (p >= 0.001) { tries <- tries + p; p <- p * (1 - dropoff) }
    round(tries, 1)
  }
  fmt <- function(p0) {
    t <- total_tries(p0)
    if (is.infinite(t)) "retries indefinitely"
    else if (t < 0.1)   "almost never retries"
    else paste0("~", t, " expected retries")
  }
  tags$div(
    style = paste0(
      "margin-top:4px; padding:7px 10px; border-radius:5px;",
      "background:#f0f9ff; border:1px solid #2563eb;",
      "font-size:11px; color:#555; line-height:1.8;"
    ),
    tags$b("Expected retries/patient:"), tags$br(),
    tags$b("No-slot: "), fmt(p_full), tags$br(),
    tags$b("Wait-averse: "), fmt(p_balk)
  )
}

# =============================
# UI
# =============================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .well { padding: 10px; }
    .sidebar-section { margin-bottom: 4px; }
    hr { margin: 8px 0; }
    h4 { margin-bottom: 6px; }
  "))),
  
  titlePanel("Patient Appointment Scheduling — Two Populations"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      tabsetPanel(
        tabPanel("Population 1",
                 br(),
                 pop_panel(1, COL_POP1, list(
                   rate = 8, alpha = 0.025, gamma = 0.025,
                   delta0 = 0.05, delta_w = 0.01,
                   p_retry_full = 0.8, p_retry_balk = 0.3, p_retry_dropoff = 0.3
                 ))
        ),
        tabPanel("Population 2",
                 br(),
                 pop_panel(2, COL_POP2, list(
                   rate = 4, alpha = 0.010, gamma = 0.040,
                   delta0 = 0.10, delta_w = 0.02,
                   p_retry_full = 0.5, p_retry_balk = 0.2, p_retry_dropoff = 0.5
                 ))
        ),
        tabPanel("System",
                 br(),
                 h4("Shared capacity"),
                 sliderInput("C",           "Daily slots (shared)", 1, 40, 10),
                 sliderInput("window_days", "Booking horizon (days)", 1, 90, 30),
                 sliderInput("sim_days",    "Simulation length (days)", 30, 1000, 365)
        )
      ),
      
      br(),
      actionButton("rerun", "▶  Run simulation", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      width = 8,
      
      h3("Summary metrics"),
      fluidRow(
        column(3, wellPanel(uiOutput("m_total"))),
        column(3, wellPanel(uiOutput("m_util"))),
        column(3, wellPanel(uiOutput("m_avg_wait"))),
        column(3, wellPanel(uiOutput("m_balk_rate")))
      ),
      
      h3("Outcome breakdown by population"),
      tabsetPanel(
        tabPanel("Combined",
                 br(),
                 fluidRow(
                   column(6, h5("Outcomes"), tableOutput("outcome_table")),
                   column(6, h5("Wait times"), tableOutput("wait_table"))
                 )
        ),
        tabPanel("Population 1",
                 br(),
                 fluidRow(
                   column(6, h5("Outcomes"), tableOutput("outcome_table1")),
                   column(6, h5("Wait times"), tableOutput("wait_table1"))
                 )
        ),
        tabPanel("Population 2",
                 br(),
                 fluidRow(
                   column(6, h5("Outcomes"), tableOutput("outcome_table2")),
                   column(6, h5("Wait times"), tableOutput("wait_table2"))
                 )
        )
      ),
      
      hr(),
      h3("Wait-time distribution by outcome"),
      fluidRow(
        column(6,
               h5("Population 1", style = paste0("color:", COL_POP1)),
               plotOutput("hist1", height = "260px")
        ),
        column(6,
               h5("Population 2", style = paste0("color:", COL_POP2)),
               plotOutput("hist2", height = "260px")
        )
      ),
      
      hr(),
      h3("Daily throughput (7-day rolling average)"),
      plotOutput("time", height = "300px"),
      
      hr(),
      h3("Arrivals by population (7-day rolling average)"),
      plotOutput("arrivals_plot", height = "240px"),
      
      hr(),
      h3("Slot availability horizon"),
      plotOutput("horizon", height = "240px"),
      
      hr(),
      h3("Daily utilization"),
      plotOutput("util_plot", height = "220px")
    )
  )
)

# =============================
# SERVER
# =============================
server <- function(input, output) {
  
  # ---- Live readouts for each pop ----
  for (pop in 1:2) {
    local({
      p <- pop
      pfx <- paste0("p", p, "_")
      col <- if (p == 1) COL_POP1 else COL_POP2
      
      output[[paste0(pfx, "alpha_ro")]] <- renderUI({
        implied_readout(input[[paste0(pfx, "alpha")]], c(1,5,10),
                        "Implied rejection rate:", "#fff0f9", col)
      })
      output[[paste0(pfx, "gamma_ro")]] <- renderUI({
        implied_readout(input[[paste0(pfx, "gamma")]], c(1,5,10),
                        "Implied cancellation rate:", "#fff8e1", "#f59e0b")
      })
      output[[paste0(pfx, "retry_ro")]] <- renderUI({
        retry_readout(input[[paste0(pfx, "p_retry_full")]],
                      input[[paste0(pfx, "p_retry_balk")]],
                      input[[paste0(pfx, "p_retry_dropoff")]])
      })
    })
  }
  
  # ---- Simulation reactive ----
  sim <- reactive({
    input$rerun
    isolate({
      run_simulation(
        rate1 = input$p1_rate,   rate2 = input$p2_rate,
        C     = input$C,
        alpha1 = input$p1_alpha, alpha2 = input$p2_alpha,
        gamma1 = input$p1_gamma, gamma2 = input$p2_gamma,
        delta0_1  = input$p1_delta0,  delta_w_1 = input$p1_delta_w,
        delta0_2  = input$p2_delta0,  delta_w_2 = input$p2_delta_w,
        sim_days    = input$sim_days,
        window_days = input$window_days,
        p_retry_full1    = input$p1_p_retry_full,
        p_retry_balk1    = input$p1_p_retry_balk,
        p_retry_dropoff1 = input$p1_p_retry_dropoff,
        p_retry_full2    = input$p2_p_retry_full,
        p_retry_balk2    = input$p2_p_retry_balk,
        p_retry_dropoff2 = input$p2_p_retry_dropoff
      )
    })
  })
  
  # ---- KPI boxes ----
  make_kpi <- function(label, value, sub = NULL) {
    tagList(
      tags$div(style = "font-size:12px; color:#666;", label),
      tags$div(style = "font-size:24px; font-weight:bold; line-height:1.2;", value),
      if (!is.null(sub)) tags$div(style = "font-size:11px; color:#999;", sub)
    )
  }
  
  output$m_total <- renderUI({
    make_kpi("Total patient contacts",
             formatC(nrow(sim()$outcomes), format = "d", big.mark = ","))
  })
  output$m_util <- renderUI({
    make_kpi("Mean daily utilization",
             paste0(round(mean(sim()$daily$util, na.rm = TRUE) * 100, 1), "%"),
             "served / capacity")
  })
  output$m_avg_wait <- renderUI({
    oc <- sim()$outcomes
    served <- oc[oc$outcome == "served", ]
    make_kpi("Mean wait (served)",
             paste0(round(mean(served$wait, na.rm = TRUE), 1), " days"))
  })
  output$m_balk_rate <- renderUI({
    oc <- sim()$outcomes
    n_balk <- sum(oc$outcome %in% c("balked_noslot", "balked_wait"))
    make_kpi("Overall balk rate",
             paste0(round(n_balk / nrow(oc) * 100, 1), "%"))
  })
  
  # ---- Outcome table helper ----
  make_outcome_table <- function(oc) {
    lvls <- c("served", "cancelled", "noshow", "balked_noslot", "balked_wait")
    labs <- c("Served", "Cancelled", "No-show", "Balked — no slot", "Balked — wait")
    counts <- sapply(lvls, function(l) sum(oc$outcome == l))
    data.frame(
      Outcome = labs,
      Count   = formatC(counts, format = "d", big.mark = ","),
      `% of contacts` = paste0(round(counts / max(nrow(oc), 1) * 100, 1), "%"),
      check.names = FALSE
    )
  }
  
  make_wait_table <- function(oc) {
    lvls  <- c("served", "cancelled", "noshow", "balked_noslot", "balked_wait")
    labs  <- c("Served", "Cancelled", "No-show", "Balked — no slot", "Balked — wait")
    rows <- lapply(seq_along(lvls), function(i) {
      sub <- oc[oc$outcome == lvls[i], ]
      data.frame(
        Outcome         = labs[i],
        `Mean (days)`   = ifelse(nrow(sub) == 0, NA, round(mean(sub$wait, na.rm=TRUE), 1)),
        `Median (days)` = ifelse(nrow(sub) == 0, NA, round(median(sub$wait, na.rm=TRUE), 1)),
        check.names     = FALSE
      )
    })
    do.call(rbind, rows)
  }
  
  output$outcome_table  <- renderTable({ make_outcome_table(sim()$outcomes) },
                                       striped=TRUE, hover=TRUE)
  output$outcome_table1 <- renderTable({
    make_outcome_table(sim()$outcomes[sim()$outcomes$pop == "Population 1", ])
  }, striped=TRUE, hover=TRUE)
  output$outcome_table2 <- renderTable({
    make_outcome_table(sim()$outcomes[sim()$outcomes$pop == "Population 2", ])
  }, striped=TRUE, hover=TRUE)
  
  output$wait_table  <- renderTable({ make_wait_table(sim()$outcomes) },
                                    striped=TRUE, hover=TRUE, na="—")
  output$wait_table1 <- renderTable({
    make_wait_table(sim()$outcomes[sim()$outcomes$pop == "Population 1", ])
  }, striped=TRUE, hover=TRUE, na="—")
  output$wait_table2 <- renderTable({
    make_wait_table(sim()$outcomes[sim()$outcomes$pop == "Population 2", ])
  }, striped=TRUE, hover=TRUE, na="—")
  
  # ---- Histogram helper ----
  make_hist <- function(oc) {
    ggplot(oc, aes(x = wait, fill = outcome)) +
      geom_histogram(binwidth = 1, colour = "white", linewidth = 0.2) +
      scale_fill_manual(values = OUTCOME_COLORS, labels = OUTCOME_LABELS, drop = FALSE) +
      scale_x_continuous(breaks = scales::pretty_breaks(6)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Days from contact to outcome", y = "Contacts", fill = "Outcome") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", legend.text = element_text(size=9),
            panel.grid.minor = element_blank())
  }
  
  output$hist1 <- renderPlot({
    make_hist(sim()$outcomes[sim()$outcomes$pop == "Population 1", ])
  })
  output$hist2 <- renderPlot({
    make_hist(sim()$outcomes[sim()$outcomes$pop == "Population 2", ])
  })
  
  # ---- Daily throughput ----
  output$time <- renderPlot({
    d  <- sim()$daily
    df <- data.frame(
      day   = rep(d$day, 4),
      value = c(roll_avg(d$served1 + d$served2),
                roll_avg(d$balked_noslot1 + d$balked_noslot2),
                roll_avg(d$balked_wait1 + d$balked_wait2),
                roll_avg(d$arrivals1 + d$arrivals2)),
      series = rep(c("Served", "Balked — no slot", "Balked — wait", "Arrivals"),
                   each = nrow(d))
    )
    df$series <- factor(df$series, levels = c("Arrivals","Served","Balked — no slot","Balked — wait"))
    ggplot(df, aes(x = day, y = value, colour = series)) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = c(
        "Arrivals"       = COL_ARRIVALS,
        "Served"         = COL_SERVED,
        "Balked — no slot" = COL_BALKED_NOSLOT,
        "Balked — wait"  = COL_BALKED_WAIT
      )) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Day", y = "Patients/day (7-day avg)", colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
  })
  
  # ---- Arrivals by population ----
  output$arrivals_plot <- renderPlot({
    d <- sim()$daily
    df <- data.frame(
      day   = rep(d$day, 2),
      value = c(roll_avg(d$arrivals1), roll_avg(d$arrivals2)),
      pop   = rep(c("Population 1", "Population 2"), each = nrow(d))
    )
    ggplot(df, aes(x = day, y = value, colour = pop)) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = c("Population 1" = COL_POP1, "Population 2" = COL_POP2)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Day", y = "Arrivals/day (7-day avg)", colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
  })
  
  # ---- Booking horizon ----
  output$horizon <- renderPlot({
    d   <- sim()$daily
    raw <- ifelse(is.na(d$horizon), input$window_days, d$horizon)
    df  <- data.frame(day = d$day, horizon = roll_avg(raw))
    ggplot(df, aes(x = day, y = horizon)) +
      geom_line(colour = COL_ARRIVALS, linewidth = 0.9) +
      geom_hline(yintercept = input$window_days, linetype = "dashed",
                 colour = COL_BALKED_NOSLOT, alpha = 0.7) +
      annotate("text", x = max(d$day)*0.02, y = input$window_days,
               label = "Booking window limit", vjust=-0.5,
               colour = COL_BALKED_NOSLOT, size = 3.5, hjust = 0) +
      scale_y_continuous(limits = c(0, input$window_days + 2),
                         breaks = scales::pretty_breaks(6)) +
      labs(x = "Day", y = "Days to first open slot (7-day avg)") +
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
      annotate("text", x = max(d$day)*0.02, y = 1, label = "Full capacity",
               vjust = -0.5, colour = "#666", size = 3.5, hjust = 0) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, max(1.05, max(df$util, na.rm=TRUE) + 0.05)),
        breaks = scales::pretty_breaks(6)
      ) +
      labs(x = "Day", y = "Utilization (7-day avg)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
}

shinyApp(ui, server)

