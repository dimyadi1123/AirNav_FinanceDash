library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

# Custom CSS
css <- function(){
  tags$head(
    tags$title("AirNav Indonesia"),
    tags$style(HTML("
      /* Custom CSS for Finance Dashboard */
.main-header {
  position: fixed;
  top: 0;
  width: 100%;
  z-index: 1000;
  background-color: #007bff;
  box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.1);
  height: 60px;
  display: flex;
  align-items: center;
  padding: 0 20px;
}

.title-container {
  display: flex;
  align-items: center;
}

.title-container img {
  width: 120px;
  height: 36px;
  margin-right: 15px;
}

.title-container h1 {
  font-size: 24px;
  color: #fff;
  margin: 0;
  font-weight: 500;
}

.main-sidebar {
  background-color: #343a40;
  padding-top: 60px;
  width: 250px;
  position: fixed;
  left: 0;
  top: 0;
  height: 100%;
  box-shadow: 2px 0 5px rgba(0, 0, 0, 0.1);
}

.main-sidebar .sidebar-menu {
  list-style: none;
  padding: 0;
}

.main-sidebar .sidebar-menu > li > a {
  color: #adb5bd;
  font-size: 18px;
  padding: 10px 20px;
  display: block;
  text-decoration: none;
  transition: all 0.3s ease;
}

.main-sidebar .sidebar-menu > li > a:hover {
  color: #fff;
  background-color: rgba(255, 255, 255, 0.1);
}

.content-wrapper {
  margin-left: 250px;
  margin-top: 60px;
  padding: 20px;
  background-color: #f4f6f9;
  transition: margin-left 0.3s ease;
}

/* Responsive adjustments */
@media (max-width: 768px) {
  .main-sidebar {
    width: 200px;
  }
  
  .content-wrapper {
    margin-left: 200px;
  }
}

/* Dark mode enhancements */
body.dark-mode .main-header {
  background-color: #1a1a2e;
}

body.dark-mode .main-sidebar {
  background-color: #16213e;
}

body.dark-mode .content-wrapper {
  background-color: #0f3460;
  color: #e3e3e3;
}
    "))
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard AirNav"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("UTAMA", tabName = "utama", icon = icon("money-bill")),
      menuItem("TOP 10", tabName = "top10", icon = icon("rank")),
      menuItem("MASKAPAI", tabName = "maskapai", icon = icon("plane"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "utama",
              h1("Dashboard Utama"),
              fluidRow(
                valueBoxOutput("total_penerimaan_asing", width = 4),  
                valueBoxOutput("total_penerimaan_domestik", width = 4),
                valueBoxOutput("totalENC_produksi", width = 4),
                valueBoxOutput("totalTNC_produksi", width = 4),
                valueBoxOutput("totalENC_penjualan", width = 4),
                valueBoxOutput("totalTNC_penjualan", width = 4),
                valueBoxOutput("totalENC_pendapatan", width = 4),
                valueBoxOutput("totalTNC_pendapatan", width = 4)
              ),
              plotOutput("pie_chart"),  
              plotOutput("line_chart"),
              plotOutput("bar_chart"),
              plotOutput("jual_terima_chart"),
              plotOutput("p3Chart")
      ),
      tabItem(tabName = "top10",
              h1("Dashboard TOP 10")
      ),
      tabItem(tabName = "maskapai",
              h1("Dashboard Maskapai"),
              fluidRow(
                box(title = "Trend Piutang Maskapai",
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotOutput("trend_piutang_chart")  
                )
              ),
              fluidRow(
                box(
                  title = "Total Piutang ASI PUJIASTUTI AVIATION, PT.",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(
                    class = "value-box",  
                    style = "text-align: center;",
                    h3("Restrukturisasi dan Non-Restrukturisasi"),  
                    tags$p("Restrukturisasi: ", textOutput("restruk_value")),
                    tags$p("Non-Restrukturisasi: ", textOutput("nonrestruk_value"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Trend Piutang per Bulan",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("trend_piutang_plot")  
                )
              )
            )
        )
  )
)