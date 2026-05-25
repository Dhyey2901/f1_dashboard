# F1 Starting Grid Analysis Dashboard

An interactive analytical dashboard built with **R Shiny** examining the relationship between qualifying grid position and race outcome across 70+ years of Formula 1 (1950–2023).

Submitted as part of **Data Visualisation — Assignment 3**, RMIT University.

---

## Overview

This dashboard investigates whether starting grid position is a reliable predictor of race outcome in Formula 1, and how that predictive relationship has evolved across different competitive eras. Using a comprehensive dataset of 73 seasons, the analysis spans pole position conversion rates, position-change patterns, circuit-level variance, constructor and driver performance, and formal statistical correlation modelling.

---

## Research Questions

1. Does starting from pole position reliably predict a race win, and has this changed over time?
2. How does grid position correlate with final race classification across different eras?
3. Which circuits, constructors, and drivers show the greatest and least dependence on qualifying position?
4. What does a formal regression analysis reveal about the predictive power of grid position?

---

## Dashboard Modules

| Tab | Description |
| --- | ----------- |
| **Pole Position Analysis** | Season-by-season pole-to-win conversion rate, filterable by era, with summary statistics and an era breakdown table |
| **Grid vs Finish Position** | Scatter plot of every race entry (grid → finish), filterable by decade, with podium highlighting |
| **Greatest Comebacks** | Top position-gain drives ranked by net positions recovered, filterable by era and constructor |
| **Constructor Analysis** | Pole count and conversion rate rankings across all constructors; full statistics table |
| **Circuit Breakdown** | Pole win rate per circuit, ranked and colour-coded; adjustable minimum race threshold |
| **Driver Leaderboard** | Driver-level pole count, win conversion, and a poles vs wins scatter revealing qualification efficiency |
| **Statistical Analysis** | Pearson correlation, R², mean absolute position change, and OLS regression lines stratified by competitive era |

---

## Tech Stack

| Package | Purpose |
| ------- | ------- |
| `shiny` | Web application framework |
| `shinydashboard` | Dashboard layout and components |
| `ggplot2` | Static chart rendering |
| `plotly` | Interactive chart rendering |
| `dplyr` | Data manipulation |
| `readr` | CSV ingestion |
| `janitor` | Column name normalisation |
| `DT` | Interactive data tables |

---

## Dataset

**Source:** Rao, R. (2024). *Formula 1 World Championship Dataset (1950–2023)*. Kaggle.  
[https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020](https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020)

**Files used:**

```text
data/
├── qualifying.csv       # Qualifying session results (grid positions)
├── results.csv          # Race finish positions and status
├── races.csv            # Race metadata (year, circuit, name)
├── drivers.csv          # Driver names and identifiers
└── constructors.csv     # Constructor names and identifiers
```

---

## Getting Started

### Prerequisites

- R ≥ 4.1
- RStudio (recommended)

### Installation

1. Clone the repository:

```bash
git clone https://github.com/Dhyey2901/f1_dashboard.git
cd f1_dashboard
```

1. Install required packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "plotly",
  "dplyr", "readr", "janitor", "ggplot2", "DT"
))
```

1. Ensure the `data/` directory contains the five CSV files listed above.

### Running the App

```r
shiny::runApp("app.R")
```

Or open `app.R` in RStudio and click **Run App**.

---

## Key Findings

- Pole position leads to victory in approximately **40% of races** across all seasons, but this varies significantly by era.
- The Turbo Era (1971–1990) shows the highest pole-to-win conversion rates; the Hybrid Era (2011+) the lowest, indicating increased mid-race competitiveness.
- Pearson correlation between grid position and finishing position is moderate-to-strong (r ≈ 0.5–0.6), confirming qualifying as a meaningful but imperfect predictor.
- Street circuits (e.g., Monaco) consistently show pole win rates above 60%, while high-degradation circuits show much lower rates.
- A small number of drivers and constructors convert poles at well above the dataset average, suggesting systematic performance advantages beyond raw qualifying pace.

---

## Author

**Dhyey U Vyas**  
Student ID: S4097968  
RMIT University — Bachelor of Information Technology  

[LinkedIn](https://www.linkedin.com/in/dhyeyuvyas/) · [GitHub](https://github.com/Dhyey2901)
