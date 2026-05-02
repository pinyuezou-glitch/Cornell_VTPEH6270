# VTPEH 6270 - NY State School Immunization Analysis

This repository contains code and materials for VTPEH 6270 final report. The project examines whether measles vaccination coverage differs between public and private schools in New York State (excluding New York City) during the 2018–2019 school year.

---

## Author

Fina Zou, MPH ’27, Cornell University, Infectious Disease Epidemiology

## Contact

**pz277@cornell.edu**

---

## Research Question

Does measles vaccination coverage among students in kindergarten through grade 12 differ between public and private schools in New York State (excluding New York City) during the 2018–2019 school year?

---

## Project Description

This project uses school-level aggregate immunization data from the New York State Department of Health to compare measles vaccination coverage across school types. The analysis includes descriptive statistics, hypothesis testing (Welch two-sample t-test and Wilcoxon rank-sum test as sensitivity analysis), and an interactive Shiny app for data exploration.

Key findings: Public schools had significantly higher mean measles coverage (98.85%) than private schools (88.93%), with a statistically significant difference (t = −18.04, df = 1954.2, p < 2.2e−16). Private schools showed substantially greater variability and more schools falling below the 95% herd immunity threshold.

---

## Shiny App 

An interactive Shiny app has been developed to visualize and explore the analysis results.

🔗 **Live App:** https://pinyuezou.shinyapps.io/Final_report/

### App Features

The app includes four interactive tabs:

- **Distribution** – Histogram and box plot of vaccination coverage by school type for the 2018–2019 school year. Users can select any vaccine variable, adjust bin width, and run a Welch t-test or Wilcoxon rank-sum test with full hypothesis output.
- **Heatmap** – Vaccine × school year heatmap showing mean coverage, median coverage, or % of schools below 95%, separately for public and private schools (2012–2019).
- **Trends Over Time** – Line chart of mean vaccination coverage ± 1 SD across all seven school years (2012–2019), with the 95% herd immunity threshold marked.
- **About** – Research question, data source, statistical methods, and AI disclosure.

### Running the App Locally

1. Clone this repository
2. Make sure the following R packages are installed: `shiny`, `ggplot2`, `dplyr`
3. Place `app.R` and `school_immun_2012_2019.csv` in the same folder
4. Open RStudio and run:

```r
shiny::runApp("Shiny App/Final_report")
```

---

## Repository Structure

```
Cornell_VTPEH6270/
├── Shiny App/
│   └── Final_report/
│       ├── app.R                        # CP07 Shiny application
│       └── school_immun_2012_2019.csv   # Data file used by the app
│
├── data/
|   └── school_immun_2012_2019.csv      # Data file used by the final report                             
│
├── scripts/
│   └── Final_Report.Rmd                 # Final report (R Markdown)
│
└── output/                              # Final  report outputs
|   └── Final_Report.pdf     
```

---

## Data Source

Data were obtained from the New York State Department of Health School Immunization Survey, available through the NY State Open Data portal.

🔗 https://health.data.ny.gov/Health/School-Immunization-Survey-From-School-Year-2012-2/5pme-xbs5/about_data

The dataset contains school-level aggregate immunization information for grades K–12 in New York State, excluding New York City public schools, covering school years 2012–2013 through 2018–2019. Analysis was restricted to schools classified as Public School or Private School with non-missing measles vaccination coverage.

---

## Statistical Methods

- **Primary analysis:** Welch two-sample t-test comparing mean measles coverage between public and private schools. Valid for large samples even without normality (Central Limit Theorem).
- **Sensitivity analysis:** Wilcoxon rank-sum test (non-parametric; does not assume normality).
- Shapiro–Wilk tests confirmed departures from normality in both groups (p < 2.2e−16), supporting the use of both approaches.

---

## AI Tool Disclosure

Claude (Anthropic) was used to assist with recalling R and Shiny functions, suggesting code structure, and debugging. All analysis logic, interpretations, and final decisions were reviewed and approved by the author.
