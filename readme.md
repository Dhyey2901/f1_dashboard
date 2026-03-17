# 🏎️ F1 Starting Position Analysis Dashboard

An interactive data analytics dashboard built using **R Shiny** to analyse how starting grid position impacts race outcomes in Formula 1.

This project explores historical F1 data (1950–2023) to uncover insights into race performance, pole position advantage, and remarkable comeback drives.

---

## 📊 Project Overview

This dashboard answers key questions such as:

- Does starting from pole position guarantee a win?
- How has the pole position advantage changed across eras?
- Can drivers recover from poor starting positions?
- What are the greatest comeback performances in F1 history?

---

## 🚀 Features

- 📈 **Pole Position Analysis**  
  Visualises win rates from pole position across different eras of Formula 1.

- 📉 **Grid vs Finish Analysis**  
  Interactive scatter plots comparing starting position vs finishing position.

- 🏆 **Comeback Drive Analysis**  
  Highlights the most impressive position gains in race history.

- 🎛️ **Interactive Dashboard**  
  Dynamic filters for era selection, time periods, and teams.

---

## 🛠️ Tech Stack

- **R**
- **Shiny / shinydashboard**
- **ggplot2**
- **Plotly**
- **dplyr**
- **DT**
- **janitor**

---



## 📊 Dataset

- Source: Kaggle – Formula 1 World Championship Dataset  
- Covers races from **1950 to 2023**

---

## ▶️ How to Run the Project

1. Clone the repository:
   ```bash
   git clone https://github.com/Dhyey2901/f1-starting-position-analysis.git
2. Open app.R in RStudio
3. Install required packages:

install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "readr", "janitor", "ggplot2", "DT"))

4. Run the app:
shiny::runApp()

💡 Key Insights

Pole position does not always guarantee victory

The advantage of starting first has decreased in modern F1

Significant position gains are possible due to race strategy and conditions

Performance trends vary significantly across racing eras

👤 Author

Dhyey U Vyas
📧 dhyeyvyas2003@gmail.com

[🔗 LinkedIn](https://www.linkedin.com/in/dhyeyuvyas/)

[💻 GitHub](https://github.com/Dhyey2901)

🎯 Resume Value

This project demonstrates:

End-to-end data analytics workflow

Data cleaning and feature engineering

Interactive dashboard development

Storytelling with data

Real-world dataset handling

📌 Future Improvements

Deploy dashboard using ShinyApps.io

Add predictive modelling for race outcomes

Enhance UI/UX design

Include driver/team performance comparisons