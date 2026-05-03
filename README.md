# Patient Appointment Scheduling Simulation

A discrete-event simulation built in R Shiny for studying optimal appointment scheduling at an outpatient clinic. Developed in collaboration with Columbia Business School and Columbia-Presbyterian Hospital.

---

## Overview

The simulation models a clinic with a fixed number of daily appointment slots shared across two distinct patient populations, each arriving via an independent Poisson process. Patients contact the clinic, are offered the next available slot, and may accept or balk depending on how far out that slot falls. Booked patients may subsequently cancel or no-show. The primary outputs are the long-run distribution of days-to-resolution and the proportion of patients reaching each possible outcome.

---

## Running the App

You need R with the following packages installed:

```r
install.packages(c("shiny", "ggplot2", "scales"))
```

Then launch with:

```r
shiny::runApp("app_two_population.R")
```

---

## Parameters

The sidebar is organized into three tabs.

### Population 1 / Population 2

Each population has its own independent set of behavioral parameters:

| Parameter | What it controls |
|---|---|
| **Arrival rate** (λ) | Expected patients per day from this stream (Poisson) |
| **Wait sensitivity** (α) | Probability of accepting an offered slot = e^(−α × wait). Higher α = more wait-averse. |
| **Cancellation hazard** (γ) | P(cancel before appointment) = 1 − e^(−γ × wait). Higher γ = more cancellations on long bookings. |
| **Base no-show rate** (δ₀) | Baseline probability of not showing up, regardless of wait |
| **Extra no-show risk** (δ_w) | Additional no-show probability added per day of wait |
| **Retry prob — no slot** | Probability a fully-blocked patient tries again the next day |
| **Retry prob — wait too long** | Probability a wait-averse balker tries again the next day |
| **Retry drop-off** | Geometric decay in retry probability each passing day (0 = retries forever, 1 = gives up immediately) |

### System

| Parameter | What it controls |
|---|---|
| **Daily slots** (C) | Total appointment capacity per day, shared across both populations |
| **Booking horizon** | How many days ahead patients can book. Patients offered no slot within this window balk immediately. |
| **Simulation length** | Number of days to simulate |

---

## Patient Flow

Each patient contact follows this path:

```
Arrive → Offered next available slot within booking horizon
            │
            ├─ No slot available → Balk (no slot) → may retry later
            │
            └─ Slot offered
                  │
                  ├─ Wait too long → Balk (wait) → may retry later
                  │
                  └─ Book appointment
                        │
                        ├─ Cancel before appointment day → slot reopens
                        │
                        └─ Appointment day
                              │
                              ├─ No-show → slot wasted
                              │
                              └─ Served ✓
```

Cancellations free the slot immediately, making it available to new arrivals or retrying patients.

---

## Outputs

### Summary metrics
Four headline KPIs: total contacts, mean daily utilization, mean wait for served patients, and overall balk rate.

### Outcome breakdown
Tabbed tables showing counts and percentages for each outcome (served, cancelled, no-show, balked — no slot, balked — wait too long), available combined and split by population.

### Wait-time histograms
Side-by-side histograms for each population showing the distribution of days from first contact to final outcome, color-coded by outcome type.

### Daily throughput
Time series of arrivals, patients served, and both balk types — smoothed with a 7-day rolling average to show steady-state behavior.

### Arrivals by population
Separate time series for each population's daily arrival counts, useful for confirming the Poisson rates are as expected.

### Slot availability horizon
Days until the first open slot for a hypothetical new patient each day. Rises toward the booking window limit under heavy demand; stays near 1 when capacity is ample.

### Daily utilization
Fraction of daily capacity actually used by patients who showed up (served ÷ capacity). Values below 100% reflect waste from no-shows and late cancellations.

---

## Key Dynamics to Explore

- **Capacity vs. demand balance**: Set total arrival rate (λ₁ + λ₂) near or above C to observe queue saturation and balk cascades.
- **Cancellation recycling**: High γ frees slots but only helps if new patients are available to fill them. Short booking horizons limit this effect.
- **No-show waste**: δ₀ and δ_w interact with wait length — a congested system produces longer waits, more no-shows, more wasted slots, and longer waits still.
- **Population heterogeneity**: Set one population as highly wait-averse (high α) and the other as tolerant (low α) to see how a shared queue differentially affects each group.
- **Retry persistence**: High retry drop-off means balked patients are permanently lost. Low drop-off means they keep trying, effectively creating a hidden secondary demand stream.
