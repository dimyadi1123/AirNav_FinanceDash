library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(reactable)

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
        background-color: #3B7DA5 ;
        box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.1);
        height: 60px;
        display: flex;
        align-items: center;
        padding: 0 20px;
        color: white;  /* Menambahkan warna putih untuk teks di header */
      }

      .title-container {
        text-align: center; /* Pusatkan teks secara horizontal */
        color: white; /* Warna teks */
        padding: 10px 0; /* Tambahkan jarak atas dan bawah */
        position: relative; /* Untuk memungkinkan penyesuaian vertikal */
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
      
      .small-value-box .value {
        font-size: 1.5rem; /* Sesuaikan ukuran teks */
        white-space: nowrap;
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
        tags$div(
          class = "title-container",
          tags$h1(
            tags$a(
              href = "https://www.airnavindonesia.co.id/",  # URL tujuan
              "Dashboard AirNav",                           # Teks yang ditampilkan
              target = "_blank",                            # Buka di tab baru
              style = "text-decoration: none; color: white;" # Hilangkan garis bawah dan ubah warna teks
            )
          )
        )
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
              fluidRow(
                # Left side: Penerimaan section
                column(6,  # Takes up 6/12 of the row width
                       h2("Penerimaan"),  # Header for Penerimaan
                       column(12, valueBoxOutput("total_penerimaan_asing", width = 12)),  # Value Box 1
                       column(12, valueBoxOutput("total_penerimaan_domestik", width = 12))  # Value Box 2
                ),
                
                column(6,  # Takes up 6/12 of the row width
                       h2("Piutang"),  # Header for Piutang
                       plotlyOutput("pie_chart")  # Pie chart
                )
              ),
              h2("Penagihan"),
              fluidRow(
                valueBoxOutput("totalENC_produksi", width = 4),
                valueBoxOutput("totalTNC_produksi", width = 4),
                valueBoxOutput("totalENC_penjualan", width = 4),
                valueBoxOutput("totalTNC_penjualan", width = 4),
                valueBoxOutput("totalENC_pendapatan", width = 4),
                valueBoxOutput("totalTNC_pendapatan", width = 4)
              ),
              h2('Visualisasi'),
              fluidRow(
                column(12,
                       checkboxGroupInput("kepemilikan_filter", 
                                          label = tags$span(style = "font-weight: bold; color: #1B1833;", "Kepemilikan:"),
                                          choices = c("Domestik" = "DOMESTIK", "Asing" = "ASING"),
                                          selected = c("DOMESTIK", "ASING"),
                                          inline = TRUE,
                                          width = '100%'
                       ),
                       tags$style(HTML("
                        .checkbox-inline {
                          margin-right: 15px;
                          padding: 5px;
                          border-radius: 10px;
                          border: 2px solid #FF7F50;
                        }
                        .checkbox-inline input {
                          margin-left: 5px;
                        }
                        .checkbox-group-label {
                          font-weight: bold;
                          font-size: 16px;
                          color: #FF7F50;
                        }
                        #kepemilikan_filter {
                          padding-left: 20px;
                        }
                      "))
                )          
              ),
              fluidRow(
                plotlyOutput("line_chart"),
                plotlyOutput("bar_chart"),
                plotlyOutput("jual_terima_chart"),
                plotlyOutput("p3Chart") 
              )
      ),
      tabItem(tabName = "top10",
              h1("Dashboard TOP 10 Maskapai"),
              # Filter Kepemilikan
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "filter_kepemilikan", 
                    label = "Pilih Kepemilikan:", 
                    choices = c("DOMESTIK", "ASING"), 
                    selected = "DOMESTIK"
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    "filter_tahun_piutang", 
                    "Pilih Tahun:", 
                    choices = c("2020","2021","2022","2023", "2024", "2025"),
                    selected = "2024"
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    "filter_bulan", 
                    "Pilih bulan:", 
                    choices = c("JANUARI","FEBRUARI","MARET","APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"),
                    selected = "DESEMBER"
                  )
                )
              ),
              h2("TOP 10 Piutang Customer Pertahun"),
              fluidRow(
                box(
                  width = 12,
                  dataTableOutput("top10_piutang_thn")
                )
              ),
              h2(textOutput("judul_top10_piutang_thn")),
              fluidRow(
                box(
                  title = "Top 10 Piutang Bulanan",
                  width = 12,
                  dataTableOutput("tabel_top10_piutang_bln")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Penerimaan Bulanan",
                  width = 12,
                  dataTableOutput("tabel_penerimaan_bulanan")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Produksi Maskapai",
                  width = 12,
                  dataTableOutput("produksi_top10_bln")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Penjualan Maskapai",
                  width = 12,
                  dataTableOutput("penjualan_top10_bln")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Pendapatan Maskapai",
                  width = 12,
                  dataTableOutput("pendapatan_top10_bln")
                )
              )
      ),
      tabItem(tabName = "maskapai",
              h1("Dashboard Maskapai"),
              fluidRow(
                column(4,
                       selectizeInput(
                         inputId = "nama_customer",
                         label = "Pilih Maskapai:",
                         choices = NULL,  # Akan diisi server
                         selected = "GARUDA INDONESIA PT.",  
                         multiple = FALSE,
                         options = list(
                           placeholder = "Ketik untuk mencari maskapai...",
                           create = FALSE
                         )
                       )
                )
              ),
              h2("Profil Maskapai"),
              fluidRow(
                column(12, uiOutput("airlineprofile"))
              ),
              # Adding some custom CSS for layout adjustments
              tags$style(HTML("
                .profile-container {
                  display: grid;
                  grid-template-columns: 70% 30%;;  
                  gap: 10px;
                  margin: 20px 0;
                }
                .profile-item {
                  padding: 10px;
                  font-size: 1.2em;
                }
                .profile-item strong {
                  display: inline-block;
                  min-width: 150px;  /* Adjust width for consistent label alignment */
                  font-weight: bold;
                }
                .profile-item .value {
                  word-wrap: break-word;
                }
              ")),
              h2("Total Piutang"),
              fluidRow(
                valueBoxOutput("restruk_value_box", width = 6),
                valueBoxOutput("nonrestruk_value_box", width = 6)
              ), h2("Aging Value"),
              fluidRow(
                valueBoxOutput("late_0_30", width = 3),  
                valueBoxOutput("late_31_180", width = 3),  
                valueBoxOutput("late_181_270", width = 3), 
                valueBoxOutput("late_270_more", width = 3)
              ),
              h2("Status Piutang"),
              fluidRow(
                valueBoxOutput("recent_acp", width = 2),
                valueBoxOutput("kolektabilitas_piutang", width = 4),
                valueBoxOutput("npl_percent", width = 2)
              ),
              h2("Rasio Piutang"),
              fluidRow(
                column(12, reactableOutput("rasio_table"))
              ),
              h2("Trend Visualisasi"),
              fluidRow(
                box(title = "Trend Piutang Maskapai",
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotlyOutput("trend_piutang_chart")  
                )
              ),
              fluidRow(
                box(
                  title = "Trend Piutang per Bulan",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("trend_piutang_plot")  
                )
              ),
              h2("Produksi, Penjualan, dan Pendapatan"),
              tabsetPanel(
                # Tab Visual
                tabPanel("Visual",
                         fluidRow(
                           box(
                             title = "Produksi Enroute (RU)",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,  
                             plotlyOutput("prod_enroute_plot")  
                           ),
                           box(
                             title = "Produksi Terminal Navigation",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,  
                             plotlyOutput("prod_tnc_plot")  
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Penjualan Enroute (RU)",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,  
                             plotlyOutput("jual_enroute_plot")  
                           ),
                           box(
                             title = "Penjualan Terminal Navigation",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,  
                             plotlyOutput("jual_tnc_plot")  
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Pendapatan Enroute (RU)",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,  
                             plotlyOutput("dapat_enroute_plot")  
                           ),
                           box(
                             title = "Pendapatan Terminal Navigation",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,  
                             plotlyOutput("dapat_tnc_plot")
                           )
                         )
                ),
                
                # Tab Tabel
                tabPanel("Tabel",
                         fluidRow(
                           box(
                             title = "Produksi Enroute (RU)",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,  
                             DTOutput("prod_enroute_table")  # Ganti tableOutput dengan DTOutput
                           ),
                           box(
                             title = "Produksi Terminal Navigation",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,  
                             DTOutput("prod_tnc_table")  # Ganti tableOutput dengan DTOutput
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Penjualan Enroute (RU)",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,  
                             DTOutput("jual_enroute_table")
                           ),
                           box(
                             title = "Penjualan Terminal Navigation",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,  
                             DTOutput("jual_tnc_table")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Pendapatan Enroute (RU)",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,  
                             DTOutput("dapat_enroute_table")
                           ),
                           box(
                             title = "Pendapatan Terminal Navigation",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,  
                             DTOutput("dapat_tnc_table")
                           )
                         )
                )
              )
        )
    )
  )
)