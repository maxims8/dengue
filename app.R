# app.R
# Mock R Shiny dashboard for Dengue Cost Registry (HTA-focused)
# NOTE: This is a self-contained mock. Replace the mock data sections with real data sources.

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)



library(plotly)
library(DT)
library(scales)

# -----------------------------
# MOCK DATA GENERATION
# -----------------------------
set.seed(123)

countries <- c("Thailand","Philippines","Brunei","Vietnam","Indonesia","Malaysia","Cambodia","Laos","Myanmar")
provinces <- c("Bangkok","Chiang Mai","Samut Prakan","Khon Kaen","Nakhon Ratchasima","Udon Thani")
years <- 2015:2024
settings <- c("ED","Outpatient","Inpatient","ICU")
severity <- c("DF","DHF")
age_groups <- c("0-4","5-14","15-24","25-44","45-64","65+")
payers <- c("OOP","Government","Private")
facility_levels <- c("Primary","Secondary","Tertiary")
currency_modes <- c("Local Base Year","PPP-USD")

# Episode-level aggregated metrics by strata
costs_df <- expand.grid(
  Country = countries,
  Province = provinces,
  Year = years,
  Setting = settings,
  Severity = severity,
  AgeGroup = age_groups,
  PayerScheme = c("UCS","SSS","CSMBS","Private"),
  FacilityLevel = facility_levels,
  stringsAsFactors = FALSE
) %>%
  mutate(
    N = sample(50:1000, n(), replace = TRUE),
    MeanCost = case_when(
      Setting == "ED" ~ runif(n(), 50, 200),
      Setting == "Outpatient" ~ runif(n(), 30, 150),
      Setting == "Inpatient" ~ runif(n(), 500, 3000),
      Setting == "ICU" ~ runif(n(), 1500, 8000)
    ) * ifelse(Severity == "DHF", runif(n(), 1.5, 2.5), runif(n(), 0.8, 1.2)),
    SD = MeanCost * runif(n(), 0.2, 0.8),
    MedianCost = MeanCost * runif(n(), 0.7, 1.0),
    CI_L = pmax(MeanCost - 1.96 * (SD / sqrt(N)), 0),
    CI_U = MeanCost + 1.96 * (SD / sqrt(N)),
    # Component shares sum to 1
    PharmaShare = runif(n(), 0.1, 0.4),
    DiagnosticsShare = runif(n(), 0.05, 0.2),
    ProceduresShare = runif(n(), 0.05, 0.25),
    BedDaysShare = runif(n(), 0.1, 0.5),
    RehabShare = runif(n(), 0.0, 0.1),
    OtherShare = pmax(1 - (PharmaShare + DiagnosticsShare + ProceduresShare + BedDaysShare + RehabShare), 0.01),
    # Volumes for burden
    Cases = round(runif(n(), 10, 500))
  )

# Service basket: unit costs and utilization
service_df <- expand.grid(
  Country = countries,
  Year = years,
  Service = c("PCR","ELISA","CBC","IV Fluids","Antipyretics","Imaging","Ward Day","ICU Day","Rehab Session"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    UnitCost_PPPUSD = case_when(
      Service == "PCR" ~ runif(n(), 10, 35),
      Service == "ELISA" ~ runif(n(), 5, 20),
      Service == "CBC" ~ runif(n(), 2, 10),
      Service == "IV Fluids" ~ runif(n(), 5, 25),
      Service == "Antipyretics" ~ runif(n(), 1, 5),
      Service == "Imaging" ~ runif(n(), 20, 150),
      Service == "Ward Day" ~ runif(n(), 30, 150),
      Service == "ICU Day" ~ runif(n(), 150, 1000),
      Service == "Rehab Session" ~ runif(n(), 10, 60)
    ),
    UtilizationPerEpisode = case_when(
      Service %in% c("PCR","ELISA") ~ runif(n(), 0.3, 1.2),
      Service == "CBC" ~ runif(n(), 0.5, 2.5),
      Service == "IV Fluids" ~ runif(n(), 0.5, 1.8),
      Service == "Antipyretics" ~ runif(n(), 1, 5),
      Service == "Imaging" ~ runif(n(), 0.1, 0.6),
      Service == "Ward Day" ~ runif(n(), 0.1, 6),
      Service == "ICU Day" ~ runif(n(), 0, 2),
      Service == "Rehab Session" ~ runif(n(), 0, 3)
    ),
    CI_L = pmax(UnitCost_PPPUSD - runif(n(), 0.5, 5), 0),
    CI_U = UnitCost_PPPUSD + runif(n(), 0.5, 5)
  )

# Payer mix: shares by strata
payer_mix_df <- expand.grid(
  Country = countries,
  Province = provinces,
  Year = years,
  Setting = settings,
  Severity = severity,
  stringsAsFactors = FALSE
) %>%
  mutate(
    OOP_Share = runif(n(), 0.05, 0.5),
    Gov_Share = runif(n(), 0.3, 0.8),
    Private_Share = pmax(1 - (OOP_Share + Gov_Share), 0.05)
  )

# Provincial adjusted indices
province_df <- expand.grid(
  Country = countries,
  Province = provinces,
  Year = years,
  stringsAsFactors = FALSE
) %>%
  mutate(
    AdjustedIndex = runif(n(), 0.7, 1.4),
    CI_L = pmax(AdjustedIndex - runif(n(), 0.05, 0.15), 0),
    CI_U = AdjustedIndex + runif(n(), 0.05, 0.15)
  )

# Pathways (non-causal)
pathways_df <- expand.grid(
  Country = countries,
  Year = years,
  Pathway = c("ED→Ward","ED→Ward→ICU","Outpatient→Ward","ED→Ward→Rehab","ED→ICU"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    Share = runif(n(), 0.05, 0.5),
    MeanCost = runif(n(), 300, 5000),
    CI_L = pmax(MeanCost - runif(n(), 50, 300), 0),
    CI_U = MeanCost + runif(n(), 50, 300),
    LOS = runif(n(), 0.5, 12),
    ReadmissionRate = runif(n(), 0.02, 0.25),
    ICUUse = ifelse(grepl("ICU", Pathway), runif(n(), 0.2, 0.9), runif(n(), 0.0, 0.2)),
    Mortality = runif(n(), 0.001, 0.05)
  )

# Economic burden (aggregate totals)
burden_df <- costs_df %>%
  group_by(Country, Year, Setting, Severity) %>%
  summarise(
    MeanCost = weighted.mean(MeanCost, w = Cases),
    Cases = sum(Cases),
    TotalSpending = MeanCost * Cases,
    .groups = "drop"
  ) %>%
  mutate(
    UI_L = pmax(TotalSpending * runif(n(), 0.85, 0.95), 0),
    UI_U = TotalSpending * runif(n(), 1.05, 1.20)
  )

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Dengue Cost Registry — HTA Dashboard (Mock)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country", choices = countries, selected = "Thailand"),
      selectInput("province", "Province/Region", choices = provinces, selected = provinces[1]),
      sliderInput("year", "Year", min = min(years), max = max(years), value = max(years), step = 1, sep = ""),
      selectInput("setting", "Care Setting", choices = c("All", settings), selected = "All"),
      selectInput("severity", "Clinical Classification", choices = c("All", severity), selected = "All"),
      selectInput("agegroup", "Age Group", choices = c("All", age_groups), selected = "All"),
      selectInput("payer_scheme", "Payer/Insurance Scheme", choices = c("All","UCS","SSS","CSMBS","Private"), selected = "All"),
      selectInput("facility_level", "Facility Level", choices = c("All", facility_levels), selected = "All"),
      selectInput("perspective", "Perspective", choices = c("Health System","Payer"), selected = "Health System"),
      selectInput("currency", "Currency Mode", choices = currency_modes, selected = "PPP-USD"),
      hr(),
      downloadButton("download_ceabundle", "Download CEA Bundle (Mock CSV)")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Overview",
                 fluidRow(
                   column(6, plotlyOutput("overview_mean_ci")),
                   column(6, plotlyOutput("overview_components"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("overview_tariff_gap"))
                 ),
                 DTOutput("overview_table")
        ),
        tabPanel("Cost Structure & Drivers",
                 fluidRow(
                   column(6, plotlyOutput("drivers_components")),
                   column(6, plotlyOutput("drivers_forest"))
                 )),
        tabPanel("Economic Burden",
                 fluidRow(
                   column(12, plotlyOutput("burden_trend"))
                 ),
                 DTOutput("burden_table")
        ),
        tabPanel("Provincial Variation",
                 fluidRow(
                   column(12, plotlyOutput("prov_caterpillar"))
                 ),
                 DTOutput("prov_table")
        ),
        tabPanel("Service Basket (Cross-Country)",
                 fluidRow(
                   column(6, selectInput("service_sel", "Service", choices = unique(service_df$Service))),
                   column(6, selectInput("country_compare", "Compare with Country", choices = countries, selected = "Philippines"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("service_price")),
                   column(6, plotlyOutput("service_volume"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("service_decomp"))
                 )
        ),
        tabPanel("Subgroups & Equity",
                 fluidRow(
                   column(6, plotlyOutput("subgroup_heat_cost")),
                   column(6, plotlyOutput("subgroup_heat_util"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("equity_payer_mix"))
                 )
        ),
        tabPanel("Clinical Pathways (Non-Causal)",
                 fluidRow(
                   column(12, plotlyOutput("pathways_sankey"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("pathways_outcomes"))
                 )
        ),
        tabPanel("Benchmarking & Trends",
                 fluidRow(
                   column(12, plotlyOutput("league_cost"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("trend_cost"))
                 )
        ),
        tabPanel("Methods & Metadata",
                 h4("Mock Methods/Metadata"),
                 tags$ul(
                   tags$li("Means with 95% CIs for HTA inputs; medians with IQR for skew context."),
                   tags$li("PPP/inflation adjustments applied in mock; replace with real series."),
                   tags$li("GLM forest plot is placeholder; replace with model outputs."),
                   tags$li("Small-cell suppression and privacy rules to be implemented with real data.")
                 ),
                 DTOutput("meta_table")
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # Helper: filter costs_df by sidebar inputs
  filtered_costs <- reactive({
    df <- costs_df %>% filter(Country == input$country, Province == input$province, Year == input$year)
    if (input$setting != "All") df <- df %>% filter(Setting == input$setting)
    if (input$severity != "All") df <- df %>% filter(Severity == input$severity)
    if (input$agegroup != "All") df <- df %>% filter(AgeGroup == input$agegroup)
    if (input$payer_scheme != "All") df <- df %>% filter(PayerScheme == input$payer_scheme)
    if (input$facility_level != "All") df <- df %>% filter(FacilityLevel == input$facility_level)
    df
  })
  
  # Overview: Mean with CI
  output$overview_mean_ci <- renderPlotly({
    df <- filtered_costs() %>%
      group_by(Setting, Severity) %>%
      summarise(MeanCost = mean(MeanCost), CI_L = mean(CI_L), CI_U = mean(CI_U), MedianCost = median(MedianCost), .groups = "drop")
    
    p <- ggplot(df, aes(x = Setting, y = MeanCost, fill = Severity)) +
      geom_col(position = position_dodge(width = 0.7)) +
      geom_errorbar(aes(ymin = CI_L, ymax = CI_U), position = position_dodge(width = 0.7), width = 0.2) +
      labs(title = "Mean Cost per Episode with 95% CI", x = "Setting", y = ifelse(input$currency == "PPP-USD","Cost (PPP-USD)","Cost (Local)")) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Overview: Component shares
  output$overview_components <- renderPlotly({
    df <- filtered_costs() %>%
      select(Setting, Severity, PharmaShare, DiagnosticsShare, ProceduresShare, BedDaysShare, RehabShare, OtherShare) %>%
      group_by(Setting, Severity) %>%
      summarise(across(everything(), mean), .groups = "drop") %>%
      pivot_longer(cols = ends_with("Share"), names_to = "Component", values_to = "Share")
    
    p <- ggplot(df, aes(x = Setting, y = Share, fill = Component)) +
      geom_bar(stat = "identity", position = "fill") +
      facet_wrap(~Severity) +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Component Cost Shares", x = "Setting", y = "Share") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Overview: Observed vs Tariff (Mock: assuming tariff = 0.9 * MeanCost baseline)
  output$overview_tariff_gap <- renderPlotly({
    df <- filtered_costs() %>%
      group_by(Setting, Severity) %>%
      summarise(Observed = mean(MeanCost), Tariff = mean(MeanCost)*0.9, .groups = "drop") %>%
      pivot_longer(cols = c(Observed, Tariff), names_to = "Type", values_to = "Cost")
    
    p <- ggplot(df, aes(x = Setting, y = Cost, color = Type)) +
      geom_point(position = position_dodge(width = 0.5), size = 3) +
      geom_line(aes(group = interaction(Setting, Severity, Type)), position = position_dodge(width = 0.5)) +
      facet_wrap(~Severity) +
      labs(title = "Observed vs Tariff (Mock)", x = "Setting", y = ifelse(input$currency == "PPP-USD","Cost (PPP-USD)","Cost (Local)")) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$overview_table <- renderDT({
    df <- filtered_costs() %>%
      select(Setting, Severity, AgeGroup, PayerScheme, FacilityLevel, N, MeanCost, MedianCost, CI_L, CI_U)
    datatable(df, options = list(pageLength = 10))
  })
  
  # Cost Structure & Drivers
  output$drivers_forest <- renderPlotly({
    effects_df <- data.frame(
      Covariate = c("DHF vs DF","ICU vs Inpatient","Inpatient vs Outpatient","Age 65+ vs 25-44","Private vs UCS"),
      CostRatio = c(2.0, 1.8, 1.4, 1.3, 1.2),
      CI_L = c(1.7, 1.5, 1.2, 1.1, 1.05),
      CI_U = c(2.4, 2.1, 1.6, 1.5, 1.35)
    )
    p <- ggplot(effects_df, aes(x = reorder(Covariate, CostRatio), y = CostRatio)) +
      geom_point() +
      geom_errorbar(aes(ymin = CI_L, ymax = CI_U), width = 0.2) +
      coord_flip() +
      labs(title = "Adjusted Marginal Effects (Mock Cost Ratios)", x = "", y = "Cost Ratio (mean cost)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Economic Burden trend
  output$burden_trend <- renderPlotly({
    df <- burden_df %>% filter(Country == input$country)
    p <- ggplot(df, aes(x = Year, y = TotalSpending, color = interaction(Setting, Severity))) +
      geom_line() +
      geom_ribbon(aes(ymin = UI_L, ymax = UI_U, fill = interaction(Setting, Severity)), alpha = 0.15, color = NA) +
      scale_y_continuous(labels = label_number_si()) +
      labs(title = "Total Direct Medical Spending (Mock)", x = "Year", y = "Total Spending") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$burden_table <- renderDT({
    df <- burden_df %>% filter(Country == input$country, Year == input$year)
    datatable(df, options = list(pageLength = 10))
  })
  
  # Provincial Variation: Caterpillar
  output$prov_caterpillar <- renderPlotly({
    df <- province_df %>% filter(Country == input$country, Year == input$year) %>% arrange(AdjustedIndex)
    p <- ggplot(df, aes(x = reorder(Province, AdjustedIndex), y = AdjustedIndex)) +
      geom_point() +
      geom_errorbar(aes(ymin = CI_L, ymax = CI_U), width = 0.2) +
      coord_flip() +
      labs(title = "Adjusted Provincial Cost Index (Mock)", x = "", y = "Index (Provincial Mean / National Mean)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$prov_table <- renderDT({
    df <- province_df %>% filter(Country == input$country, Year == input$year)
    datatable(df, options = list(pageLength = 10))
  })
  
  # Service Basket: Unit prices and utilization
  output$service_price <- renderPlotly({
    df <- service_df %>% filter(Service == input$service_sel, Year == input$year, Country %in% c(input$country, input$country_compare))
    p <- ggplot(df, aes(x = Country, y = UnitCost_PPPUSD, fill = Country)) +
      geom_col() +
      geom_errorbar(aes(ymin = CI_L, ymax = CI_U), width = 0.2) +
      labs(title = paste("Unit Price (PPP-USD):", input$service_sel), x = "", y = "Unit Price (PPP-USD)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$service_volume <- renderPlotly({
    df <- service_df %>% filter(Service == input$service_sel, Year == input$year, Country %in% c(input$country, input$country_compare))
    p <- ggplot(df, aes(x = Country, y = UtilizationPerEpisode, fill = Country)) +
      geom_col() +
      labs(title = paste("Utilization per Episode:", input$service_sel), x = "", y = "Units per Episode") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Price vs Volume contribution (mock decomposition)
  output$service_decomp <- renderPlotly({
    base <- service_df %>% filter(Service == input$service_sel, Year == input$year, Country == input$country)
    cmp  <- service_df %>% filter(Service == input$service_sel, Year == input$year, Country == input$country_compare)
    if (nrow(base) == 0 || nrow(cmp) == 0) return(NULL)
    price_diff <- cmp$UnitCost_PPPUSD - base$UnitCost_PPPUSD
    vol_diff   <- cmp$UtilizationPerEpisode - base$UtilizationPerEpisode
    # Mock contribution to episode cost difference (linear approximation)
    contrib_price <- price_diff * base$UtilizationPerEpisode
    contrib_vol   <- vol_diff * base$UnitCost_PPPUSD
    df <- data.frame(Component = c("Price Effect","Volume Effect"), Contribution = c(contrib_price, contrib_vol))
    p <- ggplot(df, aes(x = Component, y = Contribution, fill = Component)) +
      geom_col() +
      labs(title = "Decomposition of Episode Cost Difference (Mock)", x = "", y = "Contribution (PPP-USD)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Subgroups & Equity: Heatmaps
  output$subgroup_heat_cost <- renderPlotly({
    df <- filtered_costs() %>%
      group_by(AgeGroup, Severity) %>%
      summarise(MeanCost = mean(MeanCost), .groups = "drop")
    p <- ggplot(df, aes(x = AgeGroup, y = Severity, fill = MeanCost)) +
      geom_tile() +
      scale_fill_viridis_c(labels = label_number_si()) +
      labs(title = "Mean Cost by Age Group and Severity", x = "Age Group", y = "") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$subgroup_heat_util <- renderPlotly({
    df <- filtered_costs() %>%
      group_by(AgeGroup, Severity) %>%
      summarise(LOS = mean(ifelse(Setting %in% c("Inpatient","ICU"), runif(n(), 2, 7), 0)), .groups = "drop")
    p <- ggplot(df, aes(x = AgeGroup, y = Severity, fill = LOS)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Average LOS (Mock) by Age Group and Severity", x = "Age Group", y = "") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Equity: payer mix
  output$equity_payer_mix <- renderPlotly({
    df <- payer_mix_df %>% filter(Country == input$country, Province == input$province, Year == input$year)
    df_long <- df %>% pivot_longer(cols = c(OOP_Share, Gov_Share, Private_Share), names_to = "Payer", values_to = "Share") %>%
      group_by(Payer) %>% summarise(Share = mean(Share), .groups = "drop")
    p <- ggplot(df_long, aes(x = Payer, y = Share, fill = Payer)) +
      geom_col() +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Payer Mix (Shares)", x = "", y = "Share") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Clinical Pathways: Sankey (mock with bar proxy)
  output$pathways_sankey <- renderPlotly({
    df <- pathways_df %>% filter(Country == input$country, Year == input$year)
    p <- ggplot(df, aes(x = Pathway, y = Share, fill = Pathway)) +
      geom_col() +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Pathway Shares (Mock)", x = "", y = "Share of Patients") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$pathways_outcomes <- renderPlotly({
    df <- pathways_df %>% filter(Country == input$country, Year == input$year)
    p <- ggplot(df, aes(x = Pathway, y = MeanCost, fill = Pathway)) +
      geom_col() +
      geom_errorbar(aes(ymin = CI_L, ymax = CI_U), width = 0.2) +
      labs(title = "Pathway Costs with 95% CI (Mock)", x = "", y = "Mean Cost") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Benchmarking & Trends: League table and trend
  output$league_cost <- renderPlotly({
    df <- costs_df %>%
      filter(Year == input$year, Setting == ifelse(input$setting=="All","Inpatient", input$setting), Severity == ifelse(input$severity=="All","DF", input$severity)) %>%
      group_by(Country) %>%
      summarise(MeanCost = mean(MeanCost), CI_L = mean(CI_L), CI_U = mean(CI_U), .groups = "drop") %>%
      arrange(desc(MeanCost))
    p <- ggplot(df, aes(x = reorder(Country, MeanCost), y = MeanCost)) +
      geom_col(fill = "steelblue") +
      geom_errorbar(aes(ymin = CI_L, ymax = CI_U), width = 0.2) +
      coord_flip() +
      labs(title = "League Table: Mean Cost per Episode (Mock)", x = "", y = "Mean Cost") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$trend_cost <- renderPlotly({
    df <- costs_df %>%
      filter(Country == input$country, Setting == ifelse(input$setting=="All","Inpatient", input$setting), Severity == ifelse(input$severity=="All","DF", input$severity)) %>%
      group_by(Year) %>%
      summarise(MeanCost = mean(MeanCost), CI_L = mean(CI_L), CI_U = mean(CI_U), .groups = "drop")
    p <- ggplot(df, aes(x = Year, y = MeanCost)) +
      geom_line(color = "darkorange") +
      geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha = 0.2, fill = "darkorange") +
      labs(title = "Trend in Mean Cost with 95% CI (Mock)", x = "Year", y = "Mean Cost") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Methods & Metadata table
  output$meta_table <- renderDT({
    meta <- data.frame(
      Key = c("Currency adjustment","PPP series","CI method","Model spec","Privacy"),
      Value = c("Inflate to base year; PPP-USD for cross-country",
                "World Bank ICP (placeholder)",
                "Nonparametric bootstrap for means/medians (mock)",
                "GLM log-link, gamma variance (mock)",
                "Aggregate/deidentified; small-cell suppression (to implement)")
    )
    datatable(meta, options = list(dom = 't'))
  })
  
  # Download CEA bundle (mock)
  output$download_ceabundle <- downloadHandler(
    filename = function() {
      paste0("CEA_bundle_", input$country, "_", input$year, ".csv")
    },
    content = function(file) {
      df <- filtered_costs() %>%
        group_by(Setting, Severity, AgeGroup, PayerScheme) %>%
        summarise(
          MeanCost = mean(MeanCost),
          CI_L = mean(CI_L),
          CI_U = mean(CI_U),
          MedianCost = median(MedianCost),
          .groups = "drop"
        )
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# -----------------------------
# LAUNCH APP
# -----------------------------
shinyApp(ui, server)