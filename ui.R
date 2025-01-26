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
        top: -20;
        width: 100%;
        z-index: 1000;
        background-color: #007bff;
        box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.1);
        height: 60px;
        display: flex;
        align-items: center;
        padding: 0 20px;
        color: white;  /* Menambahkan warna putih untuk teks di header */
      }

      .title-container {
        display: flex;
        align-items: center;
        justify-content: center; /* Menempatkan elemen di tengah secara horizontal */
        color: white; /* Mengubah warna teks di header */
        padding-top: 10px; /* Memberikan jarak ke bawah */
      }

      .title-container h1 {
        font-size: 24px;
        color: white;  /* Mengubah warna teks judul */
        margin: 0;
        font-weight: 500;
      }

      .main-sidebar {
        background-color: #FFFFFF;
        padding-top: 20px;
        padding-bottom: 20px;
        width: 250px;
        position: fixed;
        top: 60px; /* Sidebar starts below the header */
        left: 0;
        height: 100%; /* Full height of the viewport */
        max-height: 100vh; /* Limit to screen height */
        overflow-y: auto; /* Allow sidebar content to scroll */
        box-shadow: 2px 0 5px rgba(0, 0, 0, 0.1);
      }

      .navbar-header {
        order: +1; 
      }
      

      .main-sidebar .sidebar-menu > li > a {
        color: #adb5bd;
        font-size: 22px;
        font-weight: bold;
        padding: 15px 20px;
        display: flex;
        align-items: center;
        justify-content: flex-start;
        text-decoration: none;
        transition: all 0.3s ease;
      }

      .main-sidebar .sidebar-menu > li > a i {
        font-size: 24px;
        margin-right: 15px;
      }

      .main-sidebar .sidebar-menu > li > a:hover {
        color: #fff;
        background-color: rgba(255, 255, 255, 0.1);
        border-radius: 5px;
      }

      

      .content-wrapper {
        margin-left: 250px; /* Offset content to the right to accommodate sidebar */
        margin-top: 60px; /* Offset content below header */
        padding: 20px;
        background-color: #f4f6f9;
        height: calc(100vh - 60px); /* Content fills remaining height */
        overflow-y: auto; /* Allow scrolling of content */
        z-index: 998; /* Ensure content is below header */
      }

      /* Responsive adjustments */
      @media (max-width: 1200px) {
        .main-sidebar {
          width: 220px;
        }

        .content-wrapper {
          margin-left: 220px;
        }
      }

      @media (max-width: 992px) {
        .main-sidebar {
          width: 200px;
        }

        .content-wrapper {
          margin-left: 200px;
        }

        .main-sidebar .sidebar-menu > li > a {
          font-size: 18px;
        }

        .main-sidebar .sidebar-menu > li > a i {
          font-size: 20px;
        }
      }

      @media (max-width: 768px) {
        .main-sidebar {
          width: 180px;
        }

        .content-wrapper {
          margin-left: 180px;
        }

        .main-sidebar .sidebar-menu > li > a {
          font-size: 16px;
        }

        .main-sidebar .sidebar-menu > li > a i {
          font-size: 18px;
        }
      }

      @media (max-width: 576px) {
        .main-sidebar {
          width: 160px;
        }

        .content-wrapper {
          margin-left: 160px;
        }

        .main-sidebar .sidebar-menu > li > a {
          font-size: 14px;
        }

        .main-sidebar .sidebar-menu > li > a i {
          font-size: 16px;
        }
      }
    "))
  )
}


# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      tags$div(class = "title-container", h1("Dashboard AirNav"))
    )
  ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("UTAMA", tabName = "utama", icon = icon("money-bill")),
        menuItem("TOP 10", tabName = "top10", icon = icon("trophy")),
        menuItem("MASKAPAI", tabName = "maskapai", icon = icon("plane"))
      )
    ),
  dashboardBody(
    css(),
    tabItems(
      tabItem(tabName = "utama",
              h1("Dashboard Utama"),
              h2("Penerimaan"),
              fluidRow(
                valueBoxOutput("total_penerimaan_asing", width = 4),  
                valueBoxOutput("total_penerimaan_domestik", width = 4)
              ),
              h2("Total Enroute dan Terminal Navigation"),
              fluidRow(
                valueBoxOutput("totalENC_produksi", width = 4),
                valueBoxOutput("totalTNC_produksi", width = 4),
                valueBoxOutput("totalENC_penjualan", width = 4),
                valueBoxOutput("totalTNC_penjualan", width = 4),
                valueBoxOutput("totalENC_pendapatan", width = 4),
                valueBoxOutput("totalTNC_pendapatan", width = 4)
              ),
              h2("Visualisasi"),
              fluidRow(
                plotOutput("pie_chart"),  
                plotOutput("line_chart"),
                plotOutput("bar_chart"),
                plotOutput("jual_terima_chart"),
                plotOutput("p3Chart") 
              )
      ),
      tabItem(tabName = "top10",
              h1("Dashboard TOP 10"),
              fluidRow(
                tableOutput("top10_piutang_thn")  # Output tabel untuk top 10 customer
              ),
              fluidRow(
                box(
                  title = "Tabel Top 10 Piutang Bulanan",
                  width = 12,
                  dataTableOutput("tabel_top10_piutang_bln")
                )
              ),
              fluidRow(
                box(
                  title = "Tabel Top 10 Penerimaan Bulanan",
                  width = 12,
                  dataTableOutput("tabel_penerimaan_bulanan")
                )
              ),
              fluidRow(
                box(
                  title = "Tabel Top 10 Produksi Maskapai (Desember)",
                  width = 12,
                  dataTableOutput("produksi_top10_bln")
                )
              ),
              fluidRow(
                box(
                  title = "Tabel Top 10 Penjualan Maskapai (Desember)",
                  width = 12,
                  dataTableOutput("penjualan_top10_bln")
                )
              ),
              fluidRow(
                box(
                  title = "Tabel Top 10 Pendapatan Maskapai (Desember)",
                  width = 12,
                  dataTableOutput("pendapatan_top10_bln")
                )
              )
      ),
      tabItem(tabName = "maskapai",
              h1("Dashboard Maskapai"),
              h2("Aging Time"),
              fluidRow(
                valueBoxOutput("late_0_30", width = 3),  
                valueBoxOutput("late_31_180", width = 3),  
                valueBoxOutput("late_181_270", width = 3), 
                valueBoxOutput("late_270_more", width = 3)
              ),
              h2("Status Piutang"),
              fluidRow(
                valueBoxOutput("recent_acp", width = 2),
                valueBoxOutput("kolektabilitas_piutang", width = 2),
                valueBoxOutput("npl_percent", width = 2)
              ),
              h2("Rasio Piutang"),
              fluidRow(
                column(6, tableOutput("rasio_table")),  
                column(6, plotOutput("rasio_plot"))    
              ),
              h2("Trend Visualisasi"),
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
              ),
              fluidRow(
                box(
                  title = "Produksi Enroute (RU)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,  
                  plotOutput("prod_enroute_plot")  
                ),
                box(
                  title = "Produksi Terminal Navigation ",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,  
                  plotOutput("prod_tnc_plot")  
                )
              ),
              fluidRow(
                box(
                  title = "Penjualan Enroute (RU)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,  
                  plotOutput("jual_enroute_plot")  
                ),
                box(
                  title = "Penjualan Terminal Navigation ",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,  
                  plotOutput("jual_tnc_plot")  
                )
              ),
              fluidRow(
                box(
                  title = "Pendapatan Enroute (RU)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,  
                  plotOutput("dapat_enroute_plot")  
                ),
                box(
                  title = "Pendapatan Terminal Navigation ",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,  
                  plotOutput("dapat_tnc_plot")
                )
              )
            )
        )
  )
)