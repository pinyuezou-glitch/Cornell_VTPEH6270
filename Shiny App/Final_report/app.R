# =============================================================================
# NY State School Immunization Explorer
# Author: Fina Zou | Cornell University, VTPEH 6270
# Data: NYS Department of Health School Immunization Survey (2012–2019)
# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)

if (nzchar(Sys.getenv("RSTUDIO"))) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# ── Data prep ─────────────────────────────────────────────────────────────────
pct_to_num <- function(x) { as.numeric(gsub("%", "", x)) }

raw <- read.csv("school_immun_2012_2019.csv", stringsAsFactors = FALSE,
                check.names = FALSE)
colnames(raw) <- c(
  "school_id","district","period","type","school_name",
  "pct_medical_exemp","pct_religious_exemp",
  "pct_polio","pct_measles","pct_mumps","pct_rubella",
  "pct_diphtheria","pct_hepb","pct_varicella",
  "pct_complete","street","city","county","state","zip",
  "immun_group","coordinates"
)
pct_cols <- c("pct_measles","pct_polio","pct_mumps","pct_rubella",
              "pct_diphtheria","pct_hepb","pct_varicella","pct_complete",
              "pct_medical_exemp","pct_religious_exemp")
for (col in pct_cols) raw[[col]] <- pct_to_num(raw[[col]])

df <- raw %>%
  filter(type %in% c("Public School","Private School")) %>%
  mutate(type = factor(type, levels = c("Public School","Private School")))

all_periods <- sort(unique(df$period))

dist_vaccines <- c(
  "Measles"             = "pct_measles",
  "Polio"               = "pct_polio",
  "Mumps"               = "pct_mumps",
  "Rubella"             = "pct_rubella",
  "Diphtheria"          = "pct_diphtheria",
  "Hepatitis B"         = "pct_hepb",
  "Varicella"           = "pct_varicella",
  "Fully Immunized"     = "pct_complete",
  "Religious Exemption" = "pct_religious_exemp",
  "Medical Exemption"   = "pct_medical_exemp"
)
heat_vaccines <- dist_vaccines[1:8]   # coverage vaccines only

PUB_COL  <- "#4a7fb5"
PRIV_COL <- "#c0622a"

# ── CSS ───────────────────────────────────────────────────────────────────────
css <- '
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  background: #f0ece3;
  font-family: "Helvetica Neue", Arial, sans-serif;
  color: #1a1a1a;
}

/* ── TOP HEADER BAR ─────────────────────────── */
.app-header {
  background: #1a2035;
  padding: 18px 28px 14px 28px;
  border-bottom: 3px solid #c9a84c;
}
.app-header h1 {
  font-size: 24px; font-weight: 700;
  color: #ffffff; letter-spacing: .01em;
  margin-bottom: 4px;
}
.app-header .subtitle {
  font-size: 13px; color: #9aa5c4;
  letter-spacing: .02em;
}

/* ── GOAL BANNER ────────────────────────────── */
.goal-banner {
  background: #ffffff;
  border-left: 5px solid #c9a84c;
  margin: 16px 24px;
  padding: 14px 20px;
  font-size: 14px; line-height: 1.7;
  color: #2c2c2c; border-radius: 3px;
}

/* ── TABS ───────────────────────────────────── */
.outer { padding: 0 24px 40px 24px; }

.nav-tabs {
  border-bottom: 2px solid #c9a84c !important;
  background: transparent;
  margin-bottom: 0 !important;
}
.nav-tabs > li > a {
  font-size: 13.5px; font-weight: 500;
  color: #888; padding: 10px 24px;
  border: 1px solid transparent !important;
  border-radius: 3px 3px 0 0 !important;
  background: transparent !important;
  margin-bottom: -2px;
  transition: color .15s;
}
.nav-tabs > li > a:hover { color: #333; }
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  color: #1a2035 !important; font-weight: 700;
  background: #ffffff !important;
  border-color: #c9a84c #c9a84c #ffffff !important;
}
.tab-content { background: transparent; }

/* ── STAT CARDS ─────────────────────────────── */
.cards-row {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 10px;
  margin: 14px 0 16px 0;
}
.stat-card {
  background: #fff;
  border: 1px solid #ddd;
  border-radius: 6px;
  padding: 16px 18px;
}
.card-label {
  font-size: 10px; font-weight: 700;
  text-transform: uppercase; letter-spacing: .1em;
  color: #b0b0b0; margin-bottom: 8px;
}
.card-value {
  font-size: 32px; font-weight: 700;
  color: #1a2035; line-height: 1.1;
}
.card-value.pub  { color: #4a7fb5; }
.card-value.priv { color: #c0622a; }
.card-value.warn { color: #b03a2e; }
.card-sub {
  font-size: 11.5px; color: #c0c0c0;
  margin-top: 5px;
}

/* ── TAB BODY LAYOUT (sidebar + main) ───────── */
.tab-body {
  display: flex; gap: 16px;
  align-items: flex-start;
  margin-top: 14px;
}
.tab-sidebar {
  width: 230px; flex-shrink: 0;
  background: #fff;
  border: 1px solid #ddd;
  border-radius: 6px;
  padding: 18px 16px;
}
.tab-sidebar .sidebar-title {
  font-size: 14px; font-weight: 700;
  color: #1a2035;
  padding-bottom: 10px;
  border-bottom: 1px solid #eee;
  margin-bottom: 14px;
}
.ctrl-label {
  font-size: 10.5px; font-weight: 700;
  text-transform: uppercase; letter-spacing: .09em;
  color: #888; margin: 14px 0 5px 0;
}
.ctrl-label:first-of-type { margin-top: 0; }
.radio label, .checkbox label {
  font-size: 13px !important; color: #333 !important;
}
.form-group, .shiny-input-container { margin-bottom: 0 !important; }

/* ── RUN BUTTON ─────────────────────────────── */
.btn-run {
  display: block; width: 100%;
  background: #1a2035 !important;
  color: #fff !important;
  font-size: 13px !important; font-weight: 700 !important;
  letter-spacing: .04em;
  border: none !important;
  border-radius: 5px !important;
  padding: 11px 0 !important;
  margin-top: 18px; cursor: pointer;
}
.btn-run:hover { background: #2c3555 !important; }

/* github text link below button */
.gh-link {
  margin-top: 10px;
  font-size: 11.5px; color: #aaa;
  text-align: center;
}
.gh-link a { color: #7a8aaa; text-decoration: underline; }

/* sidebar note */
.sidebar-note {
  margin-top: 14px;
  font-size: 11.5px; color: #aaa; line-height: 1.5;
}

/* ── CONTENT CARD ───────────────────────────── */
.tab-main { flex: 1; min-width: 0; }
.content-card {
  background: #fff;
  border: 1px solid #ddd;
  border-radius: 6px;
  padding: 22px 24px;
  margin-bottom: 14px;
}
.content-card h3 {
  font-size: 17px; font-weight: 700;
  color: #1a2035; margin-bottom: 3px;
}
.plot-sub { font-size: 12px; color: #bbb; margin-bottom: 14px; }

/* ── HYPOTHESIS BOX ─────────────────────────── */
.hyp-box {
  background: #fafaf8;
  border: 1px solid #e8e3d8;
  border-radius: 5px;
  padding: 18px 22px;
  font-size: 13.5px; line-height: 1.9;
}
.test-name {
  font-size: 11px; font-weight: 700;
  text-transform: uppercase; letter-spacing: .09em;
  color: #555; margin-bottom: 12px;
}
.concl-sig { color: #1a6e3c; font-weight: 700; font-style: italic; }
.concl-ns  { color: #a84300; font-weight: 700; font-style: italic; }

/* ── ABOUT ──────────────────────────────────── */
.about-card {
  background: #fff; border: 1px solid #ddd; border-radius: 6px;
  padding: 28px 32px; max-width: 760px; margin-top: 14px;
  font-size: 14px; line-height: 1.8;
}
.about-card h4 {
  font-size: 10.5px; font-weight: 700;
  text-transform: uppercase; letter-spacing: .09em;
  color: #b0b0b0; margin: 22px 0 2px 0;
}
.about-card h4:first-child { margin-top: 0; }
.about-card hr { border: none; border-top: 1px solid #eee; margin: 2px 0 8px 0; }
.about-card a { color: #1a2035; }
'

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  
  # Header bar
  div(class = "app-header",
      tags$h1("Measles Vaccination Coverage Explorer"),
      div(class = "subtitle",
          "NY State School Immunization Records \u00b7 Fina Zou")
  ),
  
  # Goal banner
  div(class = "goal-banner",
      HTML('<b>App Goal:</b> This app explores whether measles vaccination coverage
    differs between <b>public</b> and <b>private</b> schools in New York State
    (excluding NYC). The <b>Distribution</b> tab focuses on the <b>2018\u20132019</b>
    school year. The <b>Heatmap</b> and <b>Trends</b> tabs cover
    <b>2012\u20132019</b>. Use the controls to select a chart type or metric,
    then press <b>"Run Analysis"</b> to update the visuals.')
  ),
  
  div(class = "outer",
      tabsetPanel(id = "main_tabs",
                  
                  # ══════════════════════════════════════════════════════════
                  # TAB 1  Distribution
                  # ══════════════════════════════════════════════════════════
                  tabPanel("Distribution",
                           
                           # stat cards
                           div(class = "cards-row",
                               div(class = "stat-card",
                                   div(class = "card-label", "Total Schools"),
                                   div(class = "card-value", textOutput("c_total", inline = TRUE)),
                                   div(class = "card-sub",  "2018\u20132019")
                               ),
                               div(class = "stat-card",
                                   div(class = "card-label", "Mean \u2014 Public"),
                                   div(class = "card-value pub", textOutput("c_pub", inline = TRUE)),
                                   div(class = "card-sub", uiOutput("c_pub_n"))
                               ),
                               div(class = "stat-card",
                                   div(class = "card-label", "Mean \u2014 Private"),
                                   div(class = "card-value priv", textOutput("c_priv", inline = TRUE)),
                                   div(class = "card-sub", uiOutput("c_priv_n"))
                               ),
                               div(class = "stat-card",
                                   div(class = "card-label", "% Private Below 95%"),
                                   div(class = "card-value warn", textOutput("c_below", inline = TRUE)),
                                   div(class = "card-sub", "Herd immunity threshold")
                               )
                           ),
                           
                           div(class = "tab-body",
                               
                               # sidebar
                               div(class = "tab-sidebar",
                                   div(class = "sidebar-title", "Controls"),
                                   
                                   div(class = "ctrl-label", "School Type"),
                                   radioButtons("d_stype", NULL,
                                                choices = c("Both"="both","Public only"="public","Private only"="private"),
                                                selected = "both"),
                                   
                                   div(class = "ctrl-label", "Vaccine / Variable"),
                                   selectInput("d_vax", NULL,
                                               choices = names(dist_vaccines), selected = "Measles"),
                                   
                                   div(class = "ctrl-label", "Chart Type"),
                                   radioButtons("d_ptype", NULL,
                                                choices = c("Histogram"="hist","Box plot"="box"),
                                                selected = "hist"),
                                   
                                   conditionalPanel("input.d_ptype == 'hist'",
                                                    div(class = "ctrl-label", "Bin Width (%)"),
                                                    sliderInput("d_bw", NULL, min=0.1, max=5, value=1, step=0.1),
                                                    checkboxInput("d_logy", "Log y-axis", value=FALSE)
                                   ),
                                   
                                   div(class = "ctrl-label", "Statistical Test"),
                                   radioButtons("d_test", NULL,
                                                choices = c("Welch t-test"="ttest","Wilcoxon rank-sum"="wilcox"),
                                                selected = "ttest"),
                                   
                                   actionButton("d_run", "\u25b6 Run Analysis", class="btn-run"),
                                   
                                   div(class = "gh-link",
                                       "GitHub: ",
                                       tags$a(href="https://github.com/pinyuezou-glitch/Cornell_VTPEH6270.git",
                                              target="_blank", "pinyuezou-glitch/Cornell_VTPEH6270")
                                   )
                               ),
                               
                               # main
                               div(class = "tab-main",
                                   div(class = "content-card",
                                       h3(textOutput("d_title")),
                                       div(class = "plot-sub", textOutput("d_sub")),
                                       plotOutput("d_plot", height="370px")
                                   ),
                                   div(class = "content-card",
                                       uiOutput("d_hyp")
                                   )
                               )
                           )
                  ),
                  
                  # ══════════════════════════════════════════════════════════
                  # TAB 2  Heatmap
                  # ══════════════════════════════════════════════════════════
                  tabPanel("Heatmap",
                           div(class = "tab-body",
                               
                               div(class = "tab-sidebar",
                                   div(class = "sidebar-title", "Controls"),
                                   
                                   div(class = "ctrl-label", "School Type"),
                                   radioButtons("h_stype", NULL,
                                                choices = c("Public schools"="public","Private schools"="private"),
                                                selected = "public"),
                                   
                                   div(class = "ctrl-label", "Color Metric"),
                                   radioButtons("h_metric", NULL,
                                                choices = c(
                                                  "Mean coverage (%)"   = "mean",
                                                  "Median coverage (%)" = "median",
                                                  "% schools below 95%" = "below95"
                                                ), selected = "mean"),
                                   
                                   actionButton("h_run", "\u25b6 Run Analysis", class="btn-run"),
                                   
                                   div(class = "sidebar-note",
                                       "Each cell = the selected metric for one vaccine in one school year (2012\u20132019).
               Darker = higher value.")
                               ),
                               
                               div(class = "tab-main",
                                   div(class = "content-card",
                                       h3(textOutput("h_title")),
                                       div(class = "plot-sub", textOutput("h_sub")),
                                       plotOutput("h_plot", height="420px")
                                   )
                               )
                           )
                  ),
                  
                  # ══════════════════════════════════════════════════════════
                  # TAB 3  Trends
                  # ══════════════════════════════════════════════════════════
                  tabPanel("Trends Over Time",
                           div(class = "tab-body",
                               
                               div(class = "tab-sidebar",
                                   div(class = "sidebar-title", "Controls"),
                                   
                                   div(class = "ctrl-label", "Vaccine / Variable"),
                                   selectInput("t_vax", NULL,
                                               choices = names(dist_vaccines), selected = "Measles")
                               ),
                               
                               div(class = "tab-main",
                                   div(class = "content-card",
                                       h3(textOutput("t_title")),
                                       div(class = "plot-sub",
                                           "Mean coverage \u00b1\u20091 SD per year.
                 Dashed line = 95% herd immunity threshold."),
                                       plotOutput("t_trend", height="360px")
                                   ),
                                   
                               )
                           )
                  ),
                  
                  # ══════════════════════════════════════════════════════════
                  # TAB 4  About
                  # ══════════════════════════════════════════════════════════
                  tabPanel("About",
                           div(class = "about-card",
                               h4("Research Question"), tags$hr(),
                               p("Does measles vaccination coverage among students in kindergarten
             through grade 12 differ between public and private schools in
             New York State (excluding New York City) during the 2018\u20132019
             school year?"),
                               
                               h4("Scientific Context"), tags$hr(),
                               p("Measles is highly contagious; outbreaks can occur when coverage falls
             below approximately 95%. Prior research shows immunization exemption
             rates are significantly higher in private schools, suggesting greater
             susceptibility in those settings."),
                               
                               h4("Data"), tags$hr(),
                               p("New York State Department of Health \u2014 School Immunization Survey.
             School-level aggregate data for grades K\u201312 in NYS (excluding NYC
             public schools), school years 2012\u20132013 through 2018\u20132019.
             Analysis restricted to Public and Private schools with non-missing
             measles coverage."),
                               
                               h4("Statistical Methods"), tags$hr(),
                               tags$ul(
                                 tags$li(strong("Primary: Welch two-sample t-test."),
                                         " Valid for large samples even without normality (CLT)."),
                                 tags$li(strong("Sensitivity: Wilcoxon rank-sum test."),
                                         " Non-parametric; does not assume normality."),
                                 tags$li("Shapiro\u2013Wilk tests confirmed departures from normality
              in both groups (p\u202f<\u202f2.2e\u221216).")
                               ),
                               
                               h4("Author"), tags$hr(),
                               p("Fina Zou \u2014 Cornell University, VTPEH 6270"),
                               
                               h4("GitHub"), tags$hr(),
                               tags$a(href="https://github.com/pinyuezou-glitch/Cornell_VTPEH6270.git",
                                      target="_blank",
                                      "https://github.com/pinyuezou-glitch/Cornell_VTPEH6270"),
                               
                               h4("AI Use Disclosure"), tags$hr(),
                               p("This app was developed with assistance from Claude (Anthropic).
             Claude was used to help recall R/Shiny functions and suggest code
             structure. All analysis logic, interpretations, and final decisions
             were reviewed and approved by the author.")
                           )
                  )
      )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Distribution ────────────────────────────────────────────────────────────
  d_snap <- eventReactive(input$d_run, {
    vax <- dist_vaccines[[input$d_vax]]
    d <- df %>%
      filter(period == "2018-2019", !is.na(.data[[vax]])) %>%
      mutate(coverage = .data[[vax]])
    if (input$d_stype == "public")  d <- filter(d, type == "Public School")
    if (input$d_stype == "private") d <- filter(d, type == "Private School")
    d
  }, ignoreNULL = FALSE)
  
  # stat cards always on measles 2018-2019 full dataset
  d_full <- eventReactive(input$d_run, {
    df %>%
      filter(period == "2018-2019", !is.na(pct_measles)) %>%
      mutate(coverage = pct_measles)
  }, ignoreNULL = FALSE)
  
  output$c_total <- renderText({
    formatC(nrow(d_full()), format="d", big.mark=",")
  })
  output$c_pub <- renderText({
    s <- d_full() %>% filter(type=="Public School")
    if (nrow(s)==0) "—" else paste0(round(mean(s$coverage,na.rm=TRUE),2),"%")
  })
  output$c_pub_n <- renderUI({
    n <- nrow(filter(d_full(), type=="Public School"))
    HTML(paste0("n\u202f=\u202f", formatC(n,format="d",big.mark=",")))
  })
  output$c_priv <- renderText({
    s <- d_full() %>% filter(type=="Private School")
    if (nrow(s)==0) "—" else paste0(round(mean(s$coverage,na.rm=TRUE),2),"%")
  })
  output$c_priv_n <- renderUI({
    n <- nrow(filter(d_full(), type=="Private School"))
    HTML(paste0("n\u202f=\u202f", formatC(n,format="d",big.mark=",")))
  })
  output$c_below <- renderText({
    s <- d_full() %>% filter(type=="Private School")
    if (nrow(s)==0) return("—")
    paste0(round(100*mean(s$coverage < 95, na.rm=TRUE),1),"%")
  })
  
  # plot title / subtitle
  output$d_title <- renderText({
    lbl <- if (input$d_ptype=="hist") "Histogram" else "Box plot"
    paste0(lbl," of ",tolower(input$d_vax),
           " coverage by school type (2018\u20132019)")
  })
  output$d_sub <- renderText({
    if (input$d_ptype=="hist")
      paste0("Overlapping histograms; bin width\u202f=\u202f",input$d_bw,"%.")
    else
      "Box\u202f=\u202fmedian\u202f+\u202fIQR; whiskers\u202f=\u202f1.5\u00d7IQR; dots\u202f=\u202foutliers."
  })
  
  # main plot
  output$d_plot <- renderPlot({
    d   <- d_snap(); req(nrow(d)>0)
    vlab <- paste0(input$d_vax," vaccination coverage (%)")
    cap  <- paste0("Data: NY State school immunization records, 2018\u20132019",
                   "  \u00b7  Variable: ",input$d_vax)
    col_vals <- c("Public School"=PUB_COL,"Private School"=PRIV_COL)
    base_thm <- theme_minimal(base_size=13) +
      theme(legend.position="top", panel.grid.minor=element_blank(),
            plot.caption=element_text(color="#aaa",size=10,hjust=1))
    
    if (input$d_ptype=="hist") {
      p <- ggplot(d, aes(x=coverage, fill=type)) +
        scale_fill_manual(values=col_vals, name=NULL) +
        geom_histogram(binwidth=input$d_bw, alpha=0.72,
                       color="white", position="identity") +
        geom_vline(xintercept=95, linetype="dashed",
                   color="firebrick", linewidth=0.9) +
        annotate("text", x=94.3, y=Inf,
                 label="95% threshold", vjust=1.7, hjust=1,
                 color="firebrick", size=3.3) +
        labs(x=vlab, y="Count", caption=cap) + base_thm
      if (input$d_logy) p <- p + scale_y_log10()
      p
    } else {
      ldf <- data.frame(x="Public School", y=96.5,
                        label="95% herd immunity threshold",
                        stringsAsFactors=FALSE)
      ggplot(d, aes(x=type, y=coverage, fill=type)) +
        scale_fill_manual(values=col_vals, name=NULL) +
        geom_boxplot(alpha=0.80, outlier.alpha=0.25, outlier.size=0.8) +
        geom_hline(yintercept=95, linetype="dashed",
                   color="firebrick", linewidth=0.9) +
        geom_text(data=ldf, aes(x=x,y=y,label=label),
                  color="firebrick", size=3.3, hjust=0, inherit.aes=FALSE) +
        labs(x="School type", y=vlab, caption=cap) + base_thm
    }
  })
  
  # hypothesis box
  output$d_hyp <- renderUI({
    d    <- d_snap()
    pub  <- d$coverage[d$type=="Public School"]
    priv <- d$coverage[d$type=="Private School"]
    vname <- tolower(input$d_vax)
    
    if (length(pub)<2 || length(priv)<2)
      return(div(class="hyp-box",
                 p("Select \u201cBoth\u201d school types to run a comparison test.")))
    
    if (input$d_test=="ttest") {
      res   <- t.test(pub, priv, var.equal=FALSE)
      pval  <- res$p.value
      sig   <- pval < 0.05
      p_str <- if (pval<0.001) "< 0.001" else paste0("= ",round(pval,4))
      ci    <- sprintf("[%.2f%%, %.2f%%]", res$conf.int[1], res$conf.int[2])
      cclass <- if (sig) "concl-sig" else "concl-ns"
      ctext  <- if (sig) paste0("statistically significant (p ",p_str,")")
      else      paste0("not statistically significant (p ",p_str,")")
      dir <- if (sig) {
        if (mean(priv,na.rm=TRUE) < mean(pub,na.rm=TRUE))
          paste0(" Public schools: ",round(mean(pub,na.rm=TRUE),2),
                 "% vs. private schools: ",round(mean(priv,na.rm=TRUE),2),"%.")
        else
          paste0(" Private schools: ",round(mean(priv,na.rm=TRUE),2),
                 "% vs. public schools: ",round(mean(pub,na.rm=TRUE),2),"%.")
      } else ""
      
      div(class="hyp-box",
          div(class="test-name","Welch Two-Sample T-Test"),
          tags$p(HTML(paste0(
            "<b>H\u2080:</b> The mean ",vname,
            " coverage is equal between public and private schools."))),
          tags$p(HTML(paste0(
            "<b>H\u2081:</b> The mean ",vname,
            " coverage differs between school types."))),
          tags$p(HTML(paste0(
            "<b>t</b>\u202f=\u202f",round(res$statistic,2),
            " \u2502 <b>df</b>\u202f=\u202f",round(res$parameter,1),
            " \u2502 <b>p</b>\u202f",p_str,
            " \u2502 <b>95% CI for \u0394:</b>\u202f",ci))),
          tags$p(HTML(paste0(
            "<b>Conclusion:</b> The difference in mean ",vname,
            " coverage is <span class='",cclass,"'>",ctext,"</span>.",dir)))
      )
      
    } else {
      res   <- wilcox.test(pub, priv, exact=FALSE)
      pval  <- res$p.value
      sig   <- pval < 0.05
      p_str <- if (pval<0.001) "< 0.001" else paste0("= ",round(pval,4))
      cclass <- if (sig) "concl-sig" else "concl-ns"
      ctext  <- if (sig) paste0("statistically significant (p ",p_str,")")
      else      paste0("not statistically significant (p ",p_str,")")
      
      div(class="hyp-box",
          div(class="test-name","Wilcoxon Rank-Sum Test (Sensitivity Analysis)"),
          tags$p(HTML(paste0(
            "<b>H\u2080:</b> The distribution of ",vname,
            " coverage is the same in public and private schools."))),
          tags$p(HTML(paste0(
            "<b>H\u2081:</b> The distributions differ between school types."))),
          tags$p(HTML(paste0(
            "<b>W</b>\u202f=\u202f",format(res$statistic,big.mark=",",scientific=FALSE),
            " \u2502 <b>p</b>\u202f",p_str))),
          tags$p(HTML(paste0(
            "<b>Conclusion:</b> The difference in ",vname,
            " coverage is <span class='",cclass,"'>",ctext,
            "</span>. Consistent with the primary Welch t-test.")))
      )
    }
  })
  
  # ── Heatmap ──────────────────────────────────────────────────────────────────
  h_snap <- eventReactive(input$h_run, {
    type_sel <- if (input$h_stype=="public") "Public School" else "Private School"
    metric   <- input$h_metric
    bind_rows(lapply(names(heat_vaccines), function(vname) {
      col <- heat_vaccines[[vname]]
      df %>%
        filter(type==type_sel, !is.na(.data[[col]])) %>%
        group_by(period) %>%
        summarise(
          val = switch(metric,
                       mean    = mean(.data[[col]],  na.rm=TRUE),
                       median  = median(.data[[col]],na.rm=TRUE),
                       below95 = 100*mean(.data[[col]]<95,na.rm=TRUE)),
          .groups="drop") %>%
        mutate(vaccine=vname)
    }))
  }, ignoreNULL=FALSE)
  
  output$h_title <- renderText({
    t <- if (input$h_stype=="public") "Public" else "Private"
    m <- switch(input$h_metric,
                mean="Mean Coverage (%)", median="Median Coverage (%)",
                below95="% Schools Below 95%")
    paste0("Heatmap \u2014 ",t," Schools \u2014 ",m)
  })
  output$h_sub <- renderText({
    "Each cell shows the selected metric per vaccine per school year. Darker color = higher value."
  })
  
  output$h_plot <- renderPlot({
    hd <- h_snap(); req(nrow(hd)>0)
    fill_scale <- if (input$h_metric=="below95")
      scale_fill_gradient(low="#fff5f0", high="#a93226",
                          name="% Below 95%", na.value="grey90")
    else
      scale_fill_gradient(low="#d6e8f7", high="#1a4f8a",
                          name=if(input$h_metric=="mean") "Mean (%)" else "Median (%)",
                          na.value="grey90")
    
    ggplot(hd, aes(x=period, y=vaccine, fill=val)) +
      geom_tile(color="white", linewidth=0.7) +
      geom_text(aes(label=round(val,1)), size=3.6, color="#2a2a2a") +
      fill_scale +
      labs(x="School Year", y=NULL) +
      theme_minimal(base_size=13) +
      theme(axis.text.x=element_text(angle=30,hjust=1),
            axis.text.y=element_text(size=12),
            panel.grid=element_blank(),
            legend.position="right",
            plot.caption=element_text(color="#aaa",size=10,hjust=1))
  })
  
  # ── Trends ───────────────────────────────────────────────────────────────────
  output$t_title <- renderText({
    paste0(input$t_vax," vaccination coverage \u2014 trend 2012\u20132019")
  })
  
  output$t_trend <- renderPlot({
    vax <- dist_vaccines[[input$t_vax]]
    col_vals <- c("Public School"=PUB_COL,"Private School"=PRIV_COL)
    trend <- df %>%
      filter(!is.na(.data[[vax]])) %>%
      group_by(period,type) %>%
      summarise(mn=mean(.data[[vax]],na.rm=TRUE),
                sd=sd(.data[[vax]],  na.rm=TRUE), .groups="drop")
    
    ggplot(trend, aes(x=period,y=mn,color=type,group=type,fill=type)) +
      geom_ribbon(aes(ymin=mn-sd,ymax=mn+sd),alpha=0.12,color=NA) +
      geom_line(linewidth=1.3) + geom_point(size=3.5) +
      geom_hline(yintercept=95,linetype="dashed",color="firebrick",linewidth=0.9) +
      annotate("text",x=1,y=95.9,label="95% threshold",
               color="firebrick",size=3.2,hjust=0) +
      scale_color_manual(values=col_vals,name=NULL) +
      scale_fill_manual(values=col_vals, name=NULL) +
      labs(x="School Year",y=paste0(input$t_vax," coverage (%)"),
           caption="Data: NYS Dept. of Health. Bands\u202f=\u202f\u00b11\u202fSD.") +
      theme_minimal(base_size=13) +
      theme(axis.text.x=element_text(angle=30,hjust=1),
            legend.position="top", panel.grid.minor=element_blank(),
            plot.caption=element_text(color="#aaa",size=10,hjust=1))
  })
  
}

shinyApp(ui, server)