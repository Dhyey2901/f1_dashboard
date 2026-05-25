# Assignment 3 – Storytelling with Open Data
# Dashboard Title: F1 Starting Position Analysis
# Author: Dhyey U Vyas
# Student ID: S4097968

# Summary:
# This Shiny dashboard explores the impact of starting position on Formula 1 race results
# using data from the Formula 1 World Championship (1950–2023). It includes:
# 1. Pole position win rate analysis across eras
# 2. Grid vs finish position scatter insights
# 3. Most dramatic comeback drives in F1 history

# Data source: Rao, R. (2024). Kaggle F1 Dataset (1950–2023)

# Load libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(janitor)
library(ggplot2)

# Data preparation function
prepare_f1_data <- function() {
  tryCatch({
    qualifying <- read_csv("data/qualifying.csv", show_col_types = FALSE) %>% clean_names()
    results <- read_csv("data/results.csv", show_col_types = FALSE) %>% clean_names()
    races <- read_csv("data/races.csv", show_col_types = FALSE) %>% clean_names()
    drivers <- read_csv("data/drivers.csv", show_col_types = FALSE) %>% clean_names()
    constructors <- read_csv("data/constructors.csv", show_col_types = FALSE) %>% clean_names()
  }, error = function(e) {
    stop("Error reading CSV files. Ensure all required files are inside the data/ folder: ", e$message)
  })

  combined_data <- qualifying %>%
    inner_join(results, by = c("race_id", "driver_id")) %>%
    inner_join(races, by = "race_id") %>%
    inner_join(drivers, by = "driver_id")

  if ("constructor_id.x" %in% names(combined_data)) {
    combined_data <- combined_data %>%
      inner_join(constructors, by = c("constructor_id.x" = "constructor_id"))
  } else if ("constructor_id.y" %in% names(combined_data)) {
    combined_data <- combined_data %>%
      inner_join(constructors, by = c("constructor_id.y" = "constructor_id"))
  } else {
    combined_data <- combined_data %>%
      inner_join(constructors, by = "constructor_id")
  }

  final_data <- combined_data %>%
    filter(!is.na(position.x), !is.na(position.y)) %>%
    mutate(
      grid_pos = as.numeric(position.x),
      finish_pos = as.numeric(ifelse(position.y == "\\N", NA, position.y)),
      positions_gained = grid_pos - finish_pos,
      driver_full_name = paste(forename, surname),
      team = name.y,
      grand_prix = name.x,
      won_from_pole = ifelse(grid_pos == 1 & finish_pos == 1, TRUE, FALSE),
      started_pole = ifelse(grid_pos == 1, TRUE, FALSE),
      got_podium = ifelse(finish_pos <= 3 & !is.na(finish_pos), TRUE, FALSE),
      f1_era = case_when(
        year <= 1970 ~ "Early Years (1950-1970)",
        year <= 1990 ~ "Turbo Era (1971-1990)",
        year <= 2010 ~ "Modern Era (1991-2010)",
        TRUE ~ "Hybrid Era (2011+)"
      )
    ) %>%
    filter(
      grid_pos > 0,
      !is.na(finish_pos),
      finish_pos > 0,
      !is.na(positions_gained),
      !is.na(driver_full_name),
      !is.na(team)
    )

  return(final_data)
}

# Load data
tryCatch({
  f1_data <- prepare_f1_data()
  cat("Data loaded successfully. Rows:", nrow(f1_data), "\n")
}, error = function(e) {
  stop("Failed to prepare F1 data: ", e$message)
})

# UI
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = "F1 Grid Analysis",
    titleWidth = 250
  ),

  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tabs",
      menuItem("Pole Position Analysis",   tabName = "pole_tab",     icon = icon("flag-checkered")),
      menuItem("Grid vs Finish Position",  tabName = "scatter_tab",  icon = icon("chart-line")),
      menuItem("Greatest Comebacks",       tabName = "comeback_tab",     icon = icon("trophy")),
      menuItem("Constructor Analysis",     tabName = "constructor_tab",  icon = icon("industry")),
      menuItem("Circuit Breakdown",        tabName = "circuit_tab",      icon = icon("road")),
      menuItem("Driver Leaderboard",       tabName = "driver_tab",       icon = icon("user")),
      menuItem("Statistical Analysis",     tabName = "stats_tab",        icon = icon("calculator")),
      menuItem("Predictive Analysis",      tabName = "predict_tab",      icon = icon("brain"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"),
      tags$style(HTML("
        /* ─── Base ─── */
        * { font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif !important; }
        body, .wrapper           { background: #111827 !important; }
        .content-wrapper         { background: #111827 !important; }

        /* ─── Header & sidebar ─── */
        .main-sidebar, .left-side { background: #0d1117 !important; }
        .skin-black .main-header .navbar,
        .skin-black .main-header .logo {
          background: #0d1117 !important;
          border-bottom: 1px solid #1f2937 !important;
        }
        .skin-black .main-header .logo {
          border-right: 1px solid #1f2937 !important;
          color: #f9fafb !important;
          font-weight: 600 !important;
        }
        .skin-black .sidebar a {
          color: #9ca3af !important;
          font-size: 13px !important;
          font-weight: 400 !important;
          padding: 10px 20px !important;
        }
        .skin-black .main-sidebar .sidebar .sidebar-menu > li.active > a {
          background: rgba(220,38,38,0.1) !important;
          border-left: 2px solid #dc2626 !important;
          color: #f9fafb !important;
          font-weight: 500 !important;
        }
        .skin-black .main-sidebar .sidebar .sidebar-menu > li:hover > a {
          background: rgba(255,255,255,0.04) !important;
          color: #e5e7eb !important;
        }

        /* ─── Cards ─── */
        .box {
          background: #1f2937 !important;
          border: 1px solid #374151 !important;
          border-radius: 6px !important;
          box-shadow: none !important;
          margin-bottom: 20px;
        }
        .box-header {
          background: transparent !important;
          border-bottom: 1px solid #374151 !important;
          padding: 13px 18px !important;
        }
        /* Uniform header accent — no multicolor status bars */
        .box.box-primary > .box-header,
        .box.box-success  > .box-header,
        .box.box-info     > .box-header,
        .box.box-warning  > .box-header,
        .box.box-danger   > .box-header {
          background: transparent !important;
          border-left: 3px solid #dc2626 !important;
        }
        .box-title {
          color: #e5e7eb !important;
          font-size: 11px !important;
          font-weight: 600 !important;
          text-transform: uppercase !important;
          letter-spacing: 0.7px !important;
        }
        .box-body { color: #9ca3af !important; padding: 18px !important; }

        /* ─── Value boxes ─── */
        .small-box {
          border-radius: 6px !important;
          border: 1px solid #374151 !important;
          box-shadow: none !important;
        }
        .small-box h3 { font-size: 26px !important; font-weight: 700 !important; color: #f9fafb !important; }
        .small-box p  { font-size: 11px !important; font-weight: 500 !important;
                        text-transform: uppercase !important; letter-spacing: 0.5px !important; }
        .small-box .icon { opacity: 0.1 !important; }

        /* ─── Inputs ─── */
        .selectize-input, .form-control {
          background: #111827 !important;
          color: #e5e7eb !important;
          border: 1px solid #374151 !important;
          border-radius: 5px !important;
          font-size: 13px !important;
          box-shadow: none !important;
          padding: 7px 11px !important;
        }
        .selectize-input.focus, .form-control:focus {
          border-color: #dc2626 !important;
          box-shadow: 0 0 0 2px rgba(220,38,38,0.12) !important;
          outline: none !important;
        }
        .selectize-dropdown {
          background: #1f2937 !important;
          color: #e5e7eb !important;
          border: 1px solid #374151 !important;
          border-radius: 5px !important;
          font-size: 13px !important;
        }
        .selectize-dropdown .option:hover,
        .selectize-dropdown .option.active { background: #374151 !important; }
        label {
          color: #6b7280 !important;
          font-size: 11px !important;
          font-weight: 500 !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
          margin-bottom: 5px !important;
          display: block !important;
        }

        /* ─── DataTables ─── */
        table.dataTable thead th {
          background: #111827 !important;
          color: #6b7280 !important;
          font-size: 10px !important;
          font-weight: 600 !important;
          text-transform: uppercase !important;
          letter-spacing: 0.8px !important;
          border-bottom: 1px solid #374151 !important;
          padding: 10px 14px !important;
        }
        table.dataTable tbody tr { background: #1f2937 !important; color: #d1d5db !important; font-size: 13px !important; }
        table.dataTable tbody tr:hover { background: #263548 !important; }
        table.dataTable tbody td { border-top: 1px solid #2d3748 !important; padding: 10px 14px !important; }
        .dataTables_wrapper, .dataTables_wrapper .dataTables_info { color: #6b7280 !important; font-size: 12px !important; }

        /* ─── Static table (quick_stats) ─── */
        .table { color: #d1d5db !important; font-size: 13px !important; }
        .table > thead > tr > th {
          border-bottom: 1px solid #374151 !important;
          color: #6b7280 !important; font-size: 10px !important;
          text-transform: uppercase !important; letter-spacing: 0.5px !important;
          font-weight: 600 !important; padding: 8px 10px !important;
        }
        .table > tbody > tr > td { border-top: 1px solid #2d3748 !important; padding: 8px 10px !important; }
        .table-bordered { border: 1px solid #374151 !important; }

        /* ─── Verbatim output ─── */
        pre.shiny-text-output {
          background: #111827 !important;
          color: #d1d5db !important;
          border: 1px solid #374151 !important;
          border-radius: 5px !important;
          font-size: 12px !important;
          line-height: 1.75 !important;
          padding: 14px !important;
        }

        /* ─── Typography ─── */
        p    { color: #9ca3af !important; font-size: 13px !important; line-height: 1.7 !important; margin-bottom: 5px !important; }
        h4   { color: #e5e7eb !important; font-weight: 600 !important; font-size: 11px !important;
               text-transform: uppercase !important; letter-spacing: 0.6px !important;
               margin: 0 0 12px 0 !important; padding-bottom: 8px !important;
               border-bottom: 1px solid #374151 !important; }
        em   { color: #6b7280 !important; }
        strong { color: #e5e7eb !important; }
        a    { color: #dc2626 !important; text-decoration: none !important; }
        a:hover { text-decoration: underline !important; }

        /* ─── Stat rows ─── */
        .stat-row {
          display: flex; justify-content: space-between; align-items: center;
          padding: 8px 0; border-bottom: 1px solid #2d3748;
          font-size: 12px; color: #9ca3af;
        }
        .stat-row:last-child { border-bottom: none; }
        .stat-val { color: #f9fafb; font-weight: 600; font-size: 13px; }

        /* ─── Insight items ─── */
        .ins-item {
          padding: 7px 0; border-bottom: 1px solid #2d3748;
          font-size: 12px; color: #9ca3af; line-height: 1.5;
        }
        .ins-item:last-child { border-bottom: none; }

        /* ─── Footer ─── */
        .footer-strip {
          padding: 12px 20px; border-top: 1px solid #1f2937;
          color: #4b5563; font-size: 11px;
          display: flex; justify-content: space-between; flex-wrap: wrap; gap: 4px;
        }
        .footer-strip a { color: #6b7280 !important; }
        .footer-strip a:hover { color: #dc2626 !important; }

        /* ─── Scrollbar ─── */
        ::-webkit-scrollbar { width: 5px; height: 5px; }
        ::-webkit-scrollbar-track { background: #111827; }
        ::-webkit-scrollbar-thumb { background: #374151; border-radius: 3px; }
      "))
    ),

    tabItems(

      # ── Tab 1: Pole Analysis ─────────────────────────────────────────────
      tabItem(
        tabName = "pole_tab",

        fluidRow(
          valueBoxOutput("vbox_total_poles",   width = 4),
          valueBoxOutput("vbox_pole_wins",     width = 4),
          valueBoxOutput("vbox_pole_win_rate", width = 4)
        ),

        fluidRow(
          box(
            title = "Research Question", status = "primary", solidHeader = TRUE, width = 12,
            p("Does qualifying pace predict race outcome? This analysis examines the pole-to-win conversion rate across all Formula 1 seasons from 1950 to 2023, segmented by competitive era."),
            p("A declining conversion rate over time would suggest increased mid-race competitiveness, strategy variance, and reduced aerodynamic advantage for leading cars.")
          )
        ),

        fluidRow(
          box(
            title = "Pole-to-Win Conversion Rate by Season",
            status = "primary", solidHeader = TRUE, width = 8,
            selectInput("era_select", "Era Filter:",
              choices = c("All Eras", sort(unique(f1_data$f1_era))),
              selected = "All Eras", width = "50%"),
            plotlyOutput("pole_chart", height = "310px")
          ),
          box(
            title = "Dataset Summary", status = "info", solidHeader = TRUE, width = 4,
            tags$div(class = "stat-row",
              tags$span("Seasons covered"),
              tags$span(class = "stat-val", paste0(min(f1_data$year), "–", max(f1_data$year)))
            ),
            tags$div(class = "stat-row",
              tags$span("Race entries"),
              tags$span(class = "stat-val", format(nrow(f1_data), big.mark = ","))
            ),
            tags$div(class = "stat-row",
              tags$span("Drivers"),
              tags$span(class = "stat-val", length(unique(f1_data$driver_full_name)))
            ),
            tags$div(class = "stat-row",
              tags$span("Constructors"),
              tags$span(class = "stat-val", length(unique(f1_data$team)))
            ),
            br(),
            h4("Pole Conversion"),
            tableOutput("quick_stats")
          )
        ),

        fluidRow(
          box(
            title = "Pole-to-Win Rate by Competitive Era",
            status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("era_table")
          )
        )
      ),

      # ── Tab 2: Scatter ───────────────────────────────────────────────────
      tabItem(
        tabName = "scatter_tab",
        fluidRow(
          box(
            title = "Grid vs. Race Finish Position",
            status = "primary", solidHeader = TRUE, width = 9,
            p("Each point is one race entry. Points below the diagonal line gained positions from grid to finish; points above lost ground. A strong diagonal cluster indicates that qualifying position is a reliable predictor of race outcome."),
            selectInput("year_filter", "Time Period:",
              choices = list(
                "All Seasons (1950–2023)" = "all",
                "2020–2023"               = "recent",
                "2010–2019"               = "2010s",
                "2000–2009"               = "2000s",
                "1990–1999"               = "1990s",
                "Pre-1990"                = "early"
              ), width = "45%"),
            plotlyOutput("position_scatter", height = "420px")
          ),
          box(
            title = "Chart Guide", status = "warning", solidHeader = TRUE, width = 3,
            h4("Legend"),
            tags$div(class = "ins-item",
              tags$span(style = "display:inline-block;width:9px;height:9px;border-radius:50%;background:#fbbf24;margin-right:8px;vertical-align:middle;"),
              "Podium finish (P1–P3)"),
            tags$div(class = "ins-item",
              tags$span(style = "display:inline-block;width:9px;height:9px;border-radius:50%;background:#4b5563;margin-right:8px;vertical-align:middle;"),
              "Outside podium"),
            tags$div(class = "ins-item", "— — Dashed line = no net change"),
            br(),
            h4("Key Findings"),
            tags$div(class = "ins-item", "Front-row starters show highest podium probability"),
            tags$div(class = "ins-item", "Variance increases significantly from P8 onwards"),
            tags$div(class = "ins-item", "Outliers reflect safety cars, strategy, and weather")
          )
        )
      ),

      # ── Tab 3: Comebacks ─────────────────────────────────────────────────
      tabItem(
        tabName = "comeback_tab",
        fluidRow(
          box(
            title = "Greatest Position-Gain Drives",
            status = "success", solidHeader = TRUE, width = 8,
            p("Ranked by net positions gained from qualifying grid slot to race classification. These represent the most statistically improbable recoveries in the 1950–2023 dataset."),
            selectInput("comeback_type", "Filter:",
              choices = list(
                "All-Time (1950–2023)"    = "all_time",
                "Last Decade (2014–2023)" = "recent_years",
                "By Constructor"          = "by_team"
              ), width = "55%"),
            conditionalPanel(
              condition = "input.comeback_type == 'by_team'",
              selectInput("team_select", "Constructor:", choices = NULL, width = "55%")
            ),
            plotOutput("comeback_plot", height = "400px")
          ),
          box(
            title = "Top Recovery", status = "info", solidHeader = TRUE, width = 4,
            h4("Featured Drive"),
            verbatimTextOutput("featured_comeback"),
            br(),
            h4("Enabling Factors"),
            tags$div(class = "ins-item", "Wet or changing track conditions"),
            tags$div(class = "ins-item", "Undercut pit-stop strategy"),
            tags$div(class = "ins-item", "Safety car deployment timing"),
            tags$div(class = "ins-item", "Competitor retirements / penalties"),
            tags$div(class = "ins-item", "Superior tyre management")
          )
        )
      ),

      # ── Tab 4: Constructor Analysis ────────────────────────────────────────
      tabItem(
        tabName = "constructor_tab",
        fluidRow(
          valueBoxOutput("vbox_top_constructor",   width = 4),
          valueBoxOutput("vbox_best_team_rate",    width = 4),
          valueBoxOutput("vbox_constructor_count", width = 4)
        ),
        fluidRow(
          box(
            title = "Top 15 Constructors by Pole Positions",
            status = "primary", solidHeader = TRUE, width = 7,
            plotlyOutput("constructor_poles_chart", height = "360px")
          ),
          box(
            title = "Best Pole Conversion Rate (min. 10 poles)",
            status = "info", solidHeader = TRUE, width = 5,
            plotlyOutput("constructor_rate_chart", height = "360px")
          )
        ),
        fluidRow(
          box(
            title = "All Constructor Pole Statistics",
            status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("constructor_table")
          )
        )
      ),

      # ── Tab 5: Circuit Breakdown ────────────────────────────────────────────
      tabItem(
        tabName = "circuit_tab",
        fluidRow(
          valueBoxOutput("vbox_best_circuit",  width = 4),
          valueBoxOutput("vbox_worst_circuit", width = 4),
          valueBoxOutput("vbox_circuit_count", width = 4)
        ),
        fluidRow(
          box(
            title = "Research Question", status = "primary", solidHeader = TRUE, width = 12,
            p("Do circuit characteristics affect how much pole position matters? Street circuits and low-overtaking venues are hypothesised to show significantly higher pole-to-win conversion rates than high-speed or high-degradation circuits where alternative strategies and overtaking are more feasible.")
          )
        ),
        fluidRow(
          box(
            title = "Pole-to-Win Rate by Circuit",
            status = "primary", solidHeader = TRUE, width = 9,
            sliderInput("circuit_min_races", "Minimum races held at circuit:",
              min = 5, max = 30, value = 10, step = 5, width = "55%"),
            plotlyOutput("circuit_chart", height = "450px")
          ),
          box(
            title = "Interpretation", status = "warning", solidHeader = TRUE, width = 3,
            h4("What This Shows"),
            tags$div(class = "ins-item", "Street circuits (Monaco, Baku) restrict overtaking"),
            tags$div(class = "ins-item", "High-downforce circuits favour qualifying pace"),
            tags$div(class = "ins-item", "Low-downforce power circuits allow strategic variation"),
            tags$div(class = "ins-item", "Tyre degradation circuits increase mid-race strategy impact"),
            br(),
            h4("Reading the Chart"),
            p("Circuits sorted by pole win rate. A higher value means grid position is more deterministic of race outcome — offering fewer opportunities for mid-race position changes.")
          )
        )
      ),

      # ── Tab 6: Driver Leaderboard ───────────────────────────────────────────
      tabItem(
        tabName = "driver_tab",
        fluidRow(
          valueBoxOutput("vbox_most_poles_driver", width = 4),
          valueBoxOutput("vbox_most_pw_driver",    width = 4),
          valueBoxOutput("vbox_best_driver_rate",  width = 4)
        ),
        fluidRow(
          box(
            title = "Poles vs. Wins from Pole by Driver (min. 5 poles)",
            status = "primary", solidHeader = TRUE, width = 8,
            p("Each point is one driver. The dashed diagonal represents a 1:1 conversion rate — every pole resulting in a win. Points above the line exceed average conversion efficiency."),
            plotlyOutput("driver_scatter", height = "370px")
          ),
          box(
            title = "How to Read", status = "warning", solidHeader = TRUE, width = 4,
            h4("Legend"),
            tags$div(class = "ins-item", "X-axis = total pole positions"),
            tags$div(class = "ins-item", "Y-axis = wins starting from pole"),
            tags$div(class = "ins-item", "— Diagonal = 100% conversion reference"),
            tags$div(class = "ins-item", "Above line = above-average converters"),
            br(),
            h4("Key Insight"),
            p("Separates great qualifiers from great racers. Drivers below the diagonal qualify well but struggle to convert. Drivers above the line are exceptionally efficient at winning from pole.")
          )
        ),
        fluidRow(
          box(
            title = "Driver Pole Statistics (min. 3 poles)",
            status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("driver_table")
          )
        )
      ),

      # ── Tab 7: Statistical Analysis ─────────────────────────────────────────
      tabItem(
        tabName = "stats_tab",
        fluidRow(
          valueBoxOutput("vbox_correlation",    width = 4),
          valueBoxOutput("vbox_r_squared",      width = 4),
          valueBoxOutput("vbox_mean_abs_error", width = 4)
        ),
        fluidRow(
          box(
            title = "Research Context", status = "primary", solidHeader = TRUE, width = 12,
            p("Pearson's r between grid position and race finish position quantifies the linear relationship between qualifying and race performance. A coefficient of r = 1 would indicate the qualifying order is perfectly preserved. The analysis is stratified by competitive era to test whether the predictive power of qualifying has changed across different regulatory regimes.")
          )
        ),
        fluidRow(
          box(
            title = "Grid vs. Finish — OLS Regression by Competitive Era",
            status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("stats_regression_chart", height = "370px")
          ),
          box(
            title = "Correlation Coefficients by Era",
            status = "info", solidHeader = TRUE, width = 4,
            DT::dataTableOutput("era_correlation_table"),
            br(),
            p(tags$strong("Note:"), " Higher r indicates grid position more strongly predicts race outcome. All values are expected to be statistically significant given the sample sizes involved.")
          )
        )
      ),

      # ── Tab 8: Predictive Analysis ──────────────────────────────────────────
      tabItem(
        tabName = "predict_tab",

        fluidRow(
          valueBoxOutput("vbox_prob_p1",    width = 4),
          valueBoxOutput("vbox_prob_p10",   width = 4),
          valueBoxOutput("vbox_odds_ratio", width = 4)
        ),

        fluidRow(
          box(
            title = "Model Specification", status = "primary", solidHeader = TRUE, width = 12,
            p("A binary logistic regression model is fitted to predict the probability of a podium finish (P1–P3) as a function of starting grid position across all seasons (1950–2023)."),
            p("Model form: ", tags$strong("logit( P(Podium) ) = β₀ + β₁ × Grid Position")),
            p("A statistically significant negative coefficient β₁ confirms that starting further back on the grid systematically reduces the probability of a podium finish, controlling for no other covariates in this baseline specification.")
          )
        ),

        fluidRow(
          box(
            title = "Observed Podium Rate vs Model Prediction (Grid P1–P20)",
            status = "primary", solidHeader = TRUE, width = 8,
            p("Grey bars show the empirically observed podium rate per grid slot. The red line is the logistic regression fit with a 95% confidence interval band."),
            plotlyOutput("predict_curve", height = "360px")
          ),
          box(
            title = "Model Diagnostics", status = "info", solidHeader = TRUE, width = 4,
            h4("Logistic Regression Output"),
            tableOutput("model_stats_table"),
            br(),
            p(tags$strong("Interpretation:"), " The odds ratio quantifies how much the odds of a podium finish change per additional grid position. A ratio below 1 confirms a negative monotonic effect of starting position on race outcome.")
          )
        ),

        fluidRow(
          box(
            title = "Predicted Podium Probability by Competitive Era",
            status = "success", solidHeader = TRUE, width = 12,
            p("Separate logistic regression models fitted per era. Steeper curves indicate stronger grid-position dependence; flatter curves suggest more variable race outcomes regardless of qualifying result."),
            plotlyOutput("predict_era_chart", height = "340px")
          )
        )
      )
    ),

    # ── Footer ───────────────────────────────────────────────────────────────
    tags$div(
      class = "footer-strip",
      tags$span(
        tags$strong(style = "color:#6b7280;", "Source: "),
        "Rao, R. (2024). Formula 1 World Championship Dataset (1950–2023). Kaggle. ",
        tags$a("View dataset",
          href = "https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020",
          target = "_blank")
      ),
      tags$span("Data Visualisation — Assignment 3 | RMIT University")
    )
  )
)

# Server
server <- function(input, output, session) {

  observe({
    req(f1_data)
    team_options <- sort(unique(f1_data$team))
    updateSelectInput(session, "team_select", choices = team_options)
  })

  output$vbox_total_poles <- renderValueBox({
    total <- sum(f1_data$started_pole, na.rm = TRUE)
    valueBox(value = format(total, big.mark = ","), subtitle = "Total Pole Positions (1950–2023)",
             icon = icon("flag"), color = "red")
  })

  output$vbox_pole_wins <- renderValueBox({
    wins <- sum(f1_data$won_from_pole, na.rm = TRUE)
    valueBox(value = format(wins, big.mark = ","), subtitle = "Races Won From Pole",
             icon = icon("trophy"), color = "yellow")
  })

  output$vbox_pole_win_rate <- renderValueBox({
    rate <- round((sum(f1_data$won_from_pole, na.rm = TRUE) /
                   sum(f1_data$started_pole, na.rm = TRUE)) * 100, 1)
    valueBox(value = paste0(rate, "%"), subtitle = "All-Time Pole-to-Win Rate",
             icon = icon("percent"), color = "green")
  })

  output$pole_chart <- renderPlotly({
    req(f1_data)

    data_to_use <- if (input$era_select == "All Eras") {
      f1_data
    } else {
      f1_data %>% filter(f1_era == input$era_select)
    }

    if (nrow(data_to_use) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for selected era"))
    }

    yearly_stats <- data_to_use %>%
      group_by(year) %>%
      summarise(
        poles = sum(started_pole, na.rm = TRUE),
        pole_wins = sum(won_from_pole, na.rm = TRUE),
        win_percentage = ifelse(poles > 0, (pole_wins / poles) * 100, 0),
        .groups = "drop"
      ) %>%
      filter(poles > 0)

    if (nrow(yearly_stats) == 0) {
      return(plotly_empty() %>% layout(title = "No pole position data available"))
    }

    p <- ggplot(yearly_stats, aes(x = year, y = win_percentage)) +
      geom_col(fill = "#e10600", alpha = 0.85) +
      geom_smooth(se = FALSE, color = "#9ca3af", method = "loess", linewidth = 1) +
      labs(x = "Year", y = "Win Rate (%)", title = NULL) +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background    = element_rect(fill = "#1f2937", color = NA),
        panel.background   = element_rect(fill = "#1f2937", color = NA),
        panel.grid.major   = element_line(color = "#2d3748"),
        panel.grid.minor   = element_blank(),
        axis.text          = element_text(color = "#cfd8e3"),
        axis.title         = element_text(color = "#cfd8e3", size = 11)
      )

    ggplotly(p) %>% config(displayModeBar = FALSE) %>%
      layout(paper_bgcolor = "#1f2937", plot_bgcolor = "#1f2937",
             font = list(color = "#cfd8e3"))
  })

  output$quick_stats <- renderTable({
    req(f1_data)

    overall_stats <- f1_data %>%
      summarise(
        `Total Poles` = sum(started_pole, na.rm = TRUE),
        `Pole Wins` = sum(won_from_pole, na.rm = TRUE),
        `Success Rate` = paste0(
          round((sum(won_from_pole, na.rm = TRUE) / sum(started_pole, na.rm = TRUE)) * 100, 1),
          "%"
        )
      )

    data.frame(
      Metric = c("All-Time Pole Win Rate", "Total Pole Positions", "Races Won from Pole"),
      Value = c(overall_stats$`Success Rate`, overall_stats$`Total Poles`, overall_stats$`Pole Wins`)
    )
  }, bordered = TRUE)

  output$era_table <- DT::renderDataTable({
    req(f1_data)

    era_stats <- f1_data %>%
      group_by(f1_era) %>%
      summarise(
        `Pole Positions` = sum(started_pole, na.rm = TRUE),
        `Wins from Pole` = sum(won_from_pole, na.rm = TRUE),
        `Win Rate (%)` = round((`Wins from Pole` / `Pole Positions`) * 100, 1),
        .groups = "drop"
      ) %>%
      filter(`Pole Positions` > 0) %>%
      arrange(desc(`Win Rate (%)`))

    DT::datatable(era_stats, options = list(pageLength = 5, dom = "t"), rownames = FALSE)
  })

  output$position_scatter <- renderPlotly({
    req(f1_data)

    plot_data <- switch(
      input$year_filter,
      "all" = f1_data,
      "recent" = f1_data %>% filter(year >= 2020),
      "2010s" = f1_data %>% filter(year >= 2010 & year < 2020),
      "2000s" = f1_data %>% filter(year >= 2000 & year < 2010),
      "1990s" = f1_data %>% filter(year >= 1990 & year < 2000),
      "early" = f1_data %>% filter(year < 1990),
      f1_data
    )

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for selected period"))
    }

    if (nrow(plot_data) > 3000) {
      plot_data <- sample_n(plot_data, 3000)
    }

    p <- ggplot(
      plot_data,
      aes(
        x = grid_pos, y = finish_pos, color = got_podium,
        text = paste0(
          "<b>", driver_full_name, "</b>",
          "<br>Team: ", team,
          "<br>Year: ", year,
          "<br>Grid P", grid_pos, " → Finish P", finish_pos,
          "<br>Δ Positions: ", positions_gained
        )
      )
    ) +
      geom_point(alpha = 0.45, size = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#aaa", alpha = 0.4) +
      scale_color_manual(values = c("FALSE" = "#4b5563", "TRUE" = "#fbbf24"),
                         name = "", labels = c("Other", "Podium")) +
      labs(x = "Starting Position", y = "Finishing Position", title = NULL) +
      xlim(1, 24) + ylim(1, 24) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#1f2937", color = NA),
        panel.background = element_rect(fill = "#1f2937", color = NA),
        panel.grid.major = element_line(color = "rgba(255,255,255,0.06)"),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(color = "#cfd8e3"),
        axis.title       = element_text(color = "#cfd8e3", size = 11),
        legend.background = element_rect(fill = "#1f2937", color = NA),
        legend.text       = element_text(color = "#cfd8e3")
      )

    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE) %>%
      layout(paper_bgcolor = "#1f2937", plot_bgcolor = "#1f2937",
             font = list(color = "#cfd8e3"),
             legend = list(bgcolor = "#1f2937", font = list(color = "#9ca3af")))
  })

  output$comeback_plot <- renderPlot({
    req(f1_data)

    comeback_data <- switch(
      input$comeback_type,
      "all_time" = f1_data,
      "recent_years" = f1_data %>% filter(year >= 2014),
      "by_team" = {
        if (!is.null(input$team_select) && input$team_select != "") {
          f1_data %>% filter(team == input$team_select)
        } else {
          f1_data %>% slice(0)
        }
      },
      f1_data
    )

    best_comebacks <- comeback_data %>%
      filter(positions_gained > 5, !is.na(positions_gained)) %>%
      group_by(driver_full_name, team) %>%
      slice_max(positions_gained, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(desc(positions_gained)) %>%
      head(12) %>%
      mutate(
        driver_team = paste0(driver_full_name, " (", team, ")"),
        gain_label = paste0("+", positions_gained)
      )

    if (nrow(best_comebacks) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No significant comebacks found",
                 size = 6, color = "#cfd8e3") +
        theme_void() +
        theme(plot.background = element_rect(fill = "#1f2937", color = NA))
    } else {
      ggplot(best_comebacks, aes(x = reorder(driver_team, positions_gained), y = positions_gained)) +
        geom_col(fill = "#e10600", alpha = 0.9, width = 0.65) +
        geom_text(aes(label = gain_label), hjust = -0.15, size = 3.5, color = "#9ca3af", fontface = "bold") +
        coord_flip() +
        labs(x = NULL, y = "Positions Gained", title = NULL) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_minimal(base_size = 12) +
        theme(
          plot.background  = element_rect(fill = "#1f2937", color = NA),
          panel.background = element_rect(fill = "#1f2937", color = NA),
          panel.grid.major.x = element_line(color = "#2d3748"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.y  = element_text(size = 9.5, color = "#cfd8e3"),
          axis.text.x  = element_text(size = 10,  color = "#cfd8e3"),
          axis.title.x = element_text(size = 11,  color = "#9ab", face = "bold")
        )
    }
  })

  output$featured_comeback <- renderText({
    req(f1_data)

    comeback_data <- switch(
      input$comeback_type,
      "all_time" = f1_data,
      "recent_years" = f1_data %>% filter(year >= 2014),
      "by_team" = {
        if (!is.null(input$team_select) && input$team_select != "") {
          f1_data %>% filter(team == input$team_select)
        } else {
          f1_data %>% slice(0)
        }
      },
      f1_data
    )

    best_drive <- comeback_data %>%
      filter(positions_gained > 0, !is.na(positions_gained)) %>%
      arrange(desc(positions_gained)) %>%
      slice(1)

    if (nrow(best_drive) > 0) {
      paste0(
        best_drive$driver_full_name, " at the ", best_drive$year, " ",
        best_drive$grand_prix, " gained ", best_drive$positions_gained,
        " positions!\n\nStarted P", best_drive$grid_pos,
        " and finished P", best_drive$finish_pos, "."
      )
    } else {
      "No major comebacks found in this selection."
    }
  })

  # ── Shared theme helper ──────────────────────────────────────────────────
  dark_theme <- function(base = 12) {
    theme_minimal(base_size = base) +
      theme(
        plot.background    = element_rect(fill = "#1f2937", color = NA),
        panel.background   = element_rect(fill = "#1f2937", color = NA),
        panel.grid.major   = element_line(color = "#2d3748"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text          = element_text(color = "#d1d5db", size = 9),
        axis.title         = element_text(color = "#9ca3af", size = 10)
      )
  }
  dark_layout <- function(p) {
    p %>% config(displayModeBar = FALSE) %>%
      layout(paper_bgcolor = "#1f2937", plot_bgcolor = "#1f2937",
             font = list(color = "#9ca3af"))
  }

  # ── Constructor Analysis ─────────────────────────────────────────────────
  constructor_stats <- reactive({
    f1_data %>%
      group_by(team) %>%
      summarise(
        Poles                = sum(started_pole, na.rm = TRUE),
        `Wins from Pole`     = sum(won_from_pole, na.rm = TRUE),
        `Conversion Rate (%)` = ifelse(Poles > 0, round(`Wins from Pole` / Poles * 100, 1), NA),
        `Race Entries`       = n(),
        .groups = "drop"
      ) %>%
      filter(Poles > 0) %>%
      arrange(desc(Poles))
  })

  output$vbox_top_constructor <- renderValueBox({
    top <- constructor_stats() %>% slice(1)
    valueBox(top$team, paste0("Most Poles — ", top$Poles, " pole positions"),
             icon = icon("industry"), color = "red")
  })
  output$vbox_best_team_rate <- renderValueBox({
    best <- constructor_stats() %>% filter(Poles >= 10) %>%
      arrange(desc(`Conversion Rate (%)`)) %>% slice(1)
    valueBox(paste0(best$`Conversion Rate (%)`, "%"),
             paste0("Best Conversion — ", best$team),
             icon = icon("chart-bar"), color = "yellow")
  })
  output$vbox_constructor_count <- renderValueBox({
    valueBox(nrow(constructor_stats()), "Constructors with At Least One Pole",
             icon = icon("list"), color = "green")
  })

  output$constructor_poles_chart <- renderPlotly({
    df <- constructor_stats() %>% head(15)
    p <- ggplot(df, aes(x = reorder(team, Poles), y = Poles,
           text = paste0("<b>", team, "</b><br>Poles: ", Poles,
                         "<br>Wins from pole: ", `Wins from Pole`))) +
      geom_col(fill = "#dc2626", alpha = 0.85, width = 0.7) +
      coord_flip() +
      labs(x = NULL, y = "Pole Positions") +
      dark_theme()
    dark_layout(ggplotly(p, tooltip = "text"))
  })

  output$constructor_rate_chart <- renderPlotly({
    df <- constructor_stats() %>% filter(Poles >= 10) %>%
      arrange(desc(`Conversion Rate (%)`)) %>% head(15)
    p <- ggplot(df, aes(x = reorder(team, `Conversion Rate (%)`),
                        y = `Conversion Rate (%)`,
                        fill = `Conversion Rate (%)`,
           text = paste0("<b>", team, "</b><br>Rate: ", `Conversion Rate (%)`,
                         "%<br>Poles: ", Poles))) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = "#374151", high = "#dc2626", guide = "none") +
      coord_flip() +
      labs(x = NULL, y = "Conversion Rate (%)") +
      dark_theme()
    dark_layout(ggplotly(p, tooltip = "text"))
  })

  output$constructor_table <- DT::renderDataTable({
    df <- constructor_stats() %>% rename(Constructor = team)
    DT::datatable(df, options = list(pageLength = 10, dom = "frtip"), rownames = FALSE) %>%
      DT::formatStyle("Conversion Rate (%)",
        background = DT::styleColorBar(c(0, 100), "#dc2626"),
        backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center")
  })

  # ── Circuit Breakdown ────────────────────────────────────────────────────
  circuit_base <- reactive({
    f1_data %>%
      group_by(grand_prix) %>%
      summarise(
        Races              = sum(started_pole, na.rm = TRUE),
        `Wins from Pole`   = sum(won_from_pole, na.rm = TRUE),
        `Pole Win Rate (%)` = round(`Wins from Pole` / Races * 100, 1),
        .groups = "drop"
      ) %>%
      filter(Races >= 10)
  })

  output$vbox_best_circuit <- renderValueBox({
    top <- circuit_base() %>% arrange(desc(`Pole Win Rate (%)`)) %>% slice(1)
    valueBox(paste0(top$`Pole Win Rate (%)`, "%"),
             paste0("Highest Rate — ", top$grand_prix),
             icon = icon("arrow-up"), color = "red")
  })
  output$vbox_worst_circuit <- renderValueBox({
    bot <- circuit_base() %>% arrange(`Pole Win Rate (%)`) %>% slice(1)
    valueBox(paste0(bot$`Pole Win Rate (%)`, "%"),
             paste0("Lowest Rate — ", bot$grand_prix),
             icon = icon("arrow-down"), color = "yellow")
  })
  output$vbox_circuit_count <- renderValueBox({
    valueBox(n_distinct(f1_data$grand_prix), "Distinct Circuits in Dataset",
             icon = icon("road"), color = "green")
  })

  output$circuit_chart <- renderPlotly({
    df <- f1_data %>%
      group_by(grand_prix) %>%
      summarise(
        Races              = sum(started_pole, na.rm = TRUE),
        `Wins from Pole`   = sum(won_from_pole, na.rm = TRUE),
        `Pole Win Rate (%)` = round(`Wins from Pole` / Races * 100, 1),
        .groups = "drop"
      ) %>%
      filter(Races >= input$circuit_min_races) %>%
      arrange(desc(`Pole Win Rate (%)`))
    if (nrow(df) == 0) return(plotly_empty())
    p <- ggplot(df, aes(
          x = reorder(grand_prix, `Pole Win Rate (%)`),
          y = `Pole Win Rate (%)`, fill = `Pole Win Rate (%)`,
          text = paste0("<b>", grand_prix, "</b><br>Rate: ", `Pole Win Rate (%)`,
                        "%<br>Races held: ", Races))) +
      geom_col(width = 0.75) +
      scale_fill_gradient(low = "#374151", high = "#dc2626", guide = "none") +
      coord_flip() +
      labs(x = NULL, y = "Pole-to-Win Rate (%)") +
      dark_theme(11) +
      theme(panel.grid.major.x = element_line(color = "#2d3748"),
            panel.grid.major.y = element_blank())
    dark_layout(ggplotly(p, tooltip = "text"))
  })

  # ── Driver Leaderboard ───────────────────────────────────────────────────
  driver_stats <- reactive({
    f1_data %>%
      group_by(driver_full_name) %>%
      summarise(
        Poles                = sum(started_pole, na.rm = TRUE),
        `Wins from Pole`     = sum(won_from_pole, na.rm = TRUE),
        Podiums              = sum(got_podium, na.rm = TRUE),
        `Race Entries`       = n(),
        `Conversion Rate (%)` = ifelse(Poles > 0, round(`Wins from Pole` / Poles * 100, 1), NA),
        .groups = "drop"
      ) %>%
      filter(Poles >= 3) %>%
      arrange(desc(Poles))
  })

  output$vbox_most_poles_driver <- renderValueBox({
    top <- driver_stats() %>% arrange(desc(Poles)) %>% slice(1)
    valueBox(top$driver_full_name, paste0("Most Poles — ", top$Poles),
             icon = icon("flag"), color = "red")
  })
  output$vbox_most_pw_driver <- renderValueBox({
    top <- driver_stats() %>% arrange(desc(`Wins from Pole`)) %>% slice(1)
    valueBox(top$driver_full_name, paste0("Most Wins from Pole — ", top$`Wins from Pole`),
             icon = icon("trophy"), color = "yellow")
  })
  output$vbox_best_driver_rate <- renderValueBox({
    top <- driver_stats() %>% filter(Poles >= 10) %>%
      arrange(desc(`Conversion Rate (%)`)) %>% slice(1)
    valueBox(paste0(top$`Conversion Rate (%)`, "%"),
             paste0("Best Conversion (≥10 poles) — ", top$driver_full_name),
             icon = icon("percent"), color = "green")
  })

  output$driver_scatter <- renderPlotly({
    df <- driver_stats() %>% filter(Poles >= 5)
    p <- ggplot(df, aes(x = Poles, y = `Wins from Pole`,
           text = paste0("<b>", driver_full_name, "</b>",
                         "<br>Poles: ", Poles,
                         "<br>Wins from pole: ", `Wins from Pole`,
                         "<br>Conversion: ", `Conversion Rate (%)`, "%"))) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  color = "#4b5563", alpha = 0.6) +
      geom_point(color = "#dc2626", alpha = 0.75, size = 2.5) +
      labs(x = "Total Pole Positions", y = "Wins from Pole") +
      dark_theme() +
      theme(panel.grid.major.y = element_line(color = "#2d3748"),
            panel.grid.major.x = element_line(color = "#2d3748"))
    dark_layout(ggplotly(p, tooltip = "text"))
  })

  output$driver_table <- DT::renderDataTable({
    df <- driver_stats() %>% rename(Driver = driver_full_name)
    DT::datatable(df, options = list(pageLength = 10, dom = "frtip"), rownames = FALSE) %>%
      DT::formatStyle("Conversion Rate (%)",
        background = DT::styleColorBar(c(0, 100), "#dc2626"),
        backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center")
  })

  # ── Statistical Analysis ─────────────────────────────────────────────────
  output$vbox_correlation <- renderValueBox({
    r <- round(cor(f1_data$grid_pos, f1_data$finish_pos, use = "complete.obs"), 3)
    valueBox(r, "Pearson r — Grid vs Finish Position",
             icon = icon("chart-line"), color = "red")
  })
  output$vbox_r_squared <- renderValueBox({
    r2 <- round(cor(f1_data$grid_pos, f1_data$finish_pos, use = "complete.obs")^2, 3)
    valueBox(r2, "R² — Variance in Finish Explained by Grid",
             icon = icon("chart-bar"), color = "yellow")
  })
  output$vbox_mean_abs_error <- renderValueBox({
    mae <- round(mean(abs(f1_data$positions_gained), na.rm = TRUE), 2)
    valueBox(mae, "Mean Absolute Position Change per Race",
             icon = icon("ruler"), color = "green")
  })

  output$stats_regression_chart <- renderPlotly({
    set.seed(42)
    df <- f1_data %>%
      filter(!is.na(grid_pos), !is.na(finish_pos)) %>%
      group_by(f1_era) %>%
      sample_n(min(500, n())) %>%
      ungroup()
    p <- ggplot(df, aes(x = grid_pos, y = finish_pos, color = f1_era,
           text = paste0("Grid: P", grid_pos, " → Finish: P", finish_pos,
                         "<br>Era: ", f1_era))) +
      geom_point(alpha = 0.2, size = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.3) +
      scale_color_manual(name = "Era", values = c(
        "Early Years (1950-1970)" = "#6b7280",
        "Turbo Era (1971-1990)"   = "#9ca3af",
        "Modern Era (1991-2010)"  = "#dc2626",
        "Hybrid Era (2011+)"      = "#fbbf24"
      )) +
      labs(x = "Grid Position", y = "Finish Position") +
      dark_theme() +
      theme(
        panel.grid.major.y = element_line(color = "#2d3748"),
        panel.grid.major.x = element_line(color = "#2d3748"),
        legend.background  = element_rect(fill = "#1f2937", color = NA),
        legend.text        = element_text(color = "#9ca3af", size = 9)
      )
    dark_layout(ggplotly(p, tooltip = "text")) %>%
      layout(legend = list(bgcolor = "#1f2937", font = list(color = "#9ca3af")))
  })

  output$era_correlation_table <- DT::renderDataTable({
    df <- f1_data %>%
      group_by(Era = f1_era) %>%
      summarise(
        n   = n(),
        r   = round(cor(grid_pos, finish_pos, use = "complete.obs"), 3),
        `R²` = round(r^2, 3),
        .groups = "drop"
      ) %>%
      arrange(desc(r))
    DT::datatable(df, options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })

  # ── Predictive Analysis — fit models once per session ────────────────────
  model_podium <- glm(got_podium ~ grid_pos, data = f1_data, family = binomial(link = "logit"))

  grid_pred_df <- local({
    gp   <- data.frame(grid_pos = 1:24)
    pred <- predict(model_podium, newdata = gp, type = "link", se.fit = TRUE)
    data.frame(
      grid_pos = 1:24,
      prob     = plogis(pred$fit),
      lower    = plogis(pred$fit - 1.96 * pred$se.fit),
      upper    = plogis(pred$fit + 1.96 * pred$se.fit)
    )
  })

  era_pred_df <- do.call(rbind, lapply(sort(unique(f1_data$f1_era)), function(e) {
    d  <- f1_data[f1_data$f1_era == e, ]
    m  <- glm(got_podium ~ grid_pos, data = d, family = binomial(link = "logit"))
    gp <- data.frame(grid_pos = 1:20)
    data.frame(grid_pos = 1:20,
               prob = predict(m, newdata = gp, type = "response"),
               era  = e)
  }))

  output$vbox_prob_p1 <- renderValueBox({
    valueBox(paste0(round(grid_pred_df$prob[1] * 100, 1), "%"),
             "Predicted P(Podium) Starting from Pole",
             icon = icon("circle-check"), color = "red")
  })
  output$vbox_prob_p10 <- renderValueBox({
    valueBox(paste0(round(grid_pred_df$prob[10] * 100, 1), "%"),
             "Predicted P(Podium) Starting from P10",
             icon = icon("circle-minus"), color = "yellow")
  })
  output$vbox_odds_ratio <- renderValueBox({
    or  <- round(exp(coef(model_podium)["grid_pos"]), 3)
    pct <- round((1 - or) * 100, 1)
    valueBox(paste0("-", pct, "%"),
             paste0("Reduction in Podium Odds per Grid Position Back (OR = ", or, ")"),
             icon = icon("arrow-trend-down"), color = "green")
  })

  output$predict_curve <- renderPlotly({
    obs <- f1_data %>%
      filter(grid_pos <= 20) %>%
      group_by(grid_pos) %>%
      summarise(obs_rate = mean(got_podium, na.rm = TRUE), n = n(), .groups = "drop")

    df <- merge(obs, grid_pred_df[grid_pred_df$grid_pos <= 20, ], by = "grid_pos") %>%
      mutate(
        tip_bar  = paste0("Grid P", grid_pos, "<br>Observed: ",
                          round(obs_rate * 100, 1), "%<br>n = ", n),
        tip_line = paste0("Grid P", grid_pos, "<br>Predicted: ",
                          round(prob * 100, 1), "%<br>95% CI: [",
                          round(lower * 100, 1), "%, ", round(upper * 100, 1), "%]")
      )

    p <- ggplot(df) +
      geom_ribbon(aes(x = grid_pos, ymin = lower, ymax = upper),
                  fill = "#dc2626", alpha = 0.15) +
      geom_col(aes(x = grid_pos, y = obs_rate, text = tip_bar),
               fill = "#374151", alpha = 0.85, width = 0.65) +
      geom_line(aes(x = grid_pos, y = prob, text = tip_line),
                color = "#dc2626", linewidth = 1.4) +
      geom_point(aes(x = grid_pos, y = prob, text = tip_line),
                 color = "#dc2626", size = 2.2) +
      scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"),
                         limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      scale_x_continuous(breaks = seq(2, 20, 2)) +
      labs(x = "Starting Grid Position", y = "P(Podium Finish)") +
      dark_theme() +
      theme(panel.grid.major.x = element_line(color = "#2d3748"),
            panel.grid.major.y = element_line(color = "#2d3748"))

    dark_layout(ggplotly(p, tooltip = "text"))
  })

  output$model_stats_table <- renderTable({
    ms  <- summary(model_podium)$coefficients
    ci  <- confint.default(model_podium)
    null_ll  <- as.numeric(logLik(glm(got_podium ~ 1, data = f1_data, family = binomial)))
    model_ll <- as.numeric(logLik(model_podium))

    data.frame(
      Metric = c("Intercept (β₀)", "Grid Position (β₁)", "Odds Ratio (exp β₁)",
                 "OR 95% CI", "p-value", "McFadden R²", "Observations"),
      Value  = c(
        round(ms["(Intercept)", "Estimate"], 3),
        round(ms["grid_pos", "Estimate"], 4),
        round(exp(ms["grid_pos", "Estimate"]), 4),
        paste0("[", round(exp(ci["grid_pos", 1]), 4), ", ",
               round(exp(ci["grid_pos", 2]), 4), "]"),
        format(ms["grid_pos", "Pr(>|z|)"], scientific = TRUE, digits = 2),
        round(1 - model_ll / null_ll, 4),
        format(nrow(f1_data), big.mark = ",")
      )
    )
  }, bordered = TRUE, striped = FALSE)

  output$predict_era_chart <- renderPlotly({
    era_colors <- c(
      "Early Years (1950-1970)" = "#6b7280",
      "Turbo Era (1971-1990)"   = "#9ca3af",
      "Modern Era (1991-2010)"  = "#dc2626",
      "Hybrid Era (2011+)"      = "#fbbf24"
    )
    p <- ggplot(era_pred_df,
                aes(x = grid_pos, y = prob, color = era,
                    text = paste0("Era: ", era, "<br>Grid P", grid_pos,
                                  "<br>P(Podium): ", round(prob * 100, 1), "%"))) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 1.8, alpha = 0.85) +
      scale_color_manual(values = era_colors, name = "Era") +
      scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      scale_x_continuous(breaks = seq(2, 20, 2)) +
      labs(x = "Starting Grid Position", y = "P(Podium Finish)") +
      dark_theme() +
      theme(panel.grid.major.x = element_line(color = "#2d3748"),
            panel.grid.major.y = element_line(color = "#2d3748"),
            legend.background  = element_rect(fill = "#1f2937", color = NA),
            legend.text        = element_text(color = "#9ca3af", size = 9))

    dark_layout(ggplotly(p, tooltip = "text")) %>%
      layout(legend = list(bgcolor = "#1f2937", font = list(color = "#9ca3af")))
  })
}

# Run the application
shinyApp(ui = ui, server = server)