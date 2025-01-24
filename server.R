library(shiny)
library(DT)
library(gsheet)
library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggrepel)

# SERVER
server <- function(input, output) {
  
  # LOAD FILE EXCEL
  df_piutang <- reactive({
    read_excel('piutang_usaha.xlsx')
  })
  
  df_detail_tahunan <- reactive({
    read_excel('detail_tahunan.xlsx')  # Pastikan file data ada
  })
  
  df_penjualan <- reactive({
    read_excel('data_penjualan.xlsx')  
  })
  
  df_penerimaan <- reactive({
    read_excel('data_penerimaan.xlsx')  
  })
  
  df_produksi_airlines <- reactive({
    read_excel('produksi_airlines.xlsx')  
  })
  
  df_penjualan_airlines <- reactive({
    read_excel('penjualan_airlines.xlsx')  
  })
  
  df_pendapatan_airlines <- reactive({
    read_excel('pendapatan_airlines.xlsx')  
  })
  
  ## DASHBOARD UTAMA
  
  
  # TOTAL PENERIMAAN
  total_penerimaan_kepemilikan <- reactive({
    df_detail_tahunan() %>%
      group_by(KEPEMILIKAN) %>%
      summarise(total_penerimaan_2024 = sum(PENERIMAAN_2024, na.rm = TRUE))
  })
  
  # OUTPUT PENERIMAAN ASING
  output$total_penerimaan_asing <- renderValueBox({
    data <- total_penerimaan_kepemilikan()
    asing <- data %>% filter(KEPEMILIKAN == "ASING") %>% pull(total_penerimaan_2024)
    
    valueBox(
      value = paste0("Rp ", comma(asing)), 
      subtitle = "Total Penerimaan (ASING)", 
      icon = icon("flag"), 
      color = "aqua"
    )
  })
  
  # OUTPUT PENERIMAAN DOMESTIK
  output$total_penerimaan_domestik <- renderValueBox({
    data <- total_penerimaan_kepemilikan()
    domestik <- data %>% filter(KEPEMILIKAN == "DOMESTIK") %>% pull(total_penerimaan_2024)
    
    valueBox(
      value = paste0("Rp ", comma(domestik)), 
      subtitle = "Total Penerimaan (DOMESTIK)", 
      icon = icon("flag-checkered"), 
      color = "green"
    )
  })
  
  # TOTAL PENDAPATAN AIRLINES
  total_pendapatan_airlines <- reactive({
    
    df <- df_pendapatan_airlines()
    total_enc <- sum(df$ENCDOM_TOT, df$ENCINTL_TOT, df$ENCOFG_TOT, na.rm = TRUE)
    total_tnc <- sum(df$TNCDOM_TOT, df$TNCINTL_TOT, na.rm = TRUE)
    list(ENC = total_enc, TNC = total_tnc)
  })
  
  output$totalENC_pendapatan <- renderValueBox({
    valueBox(
      value = scales::comma(total_pendapatan_airlines()$ENC),
      subtitle = "Total ENC (ENCDOM + ENCINTL + ENCOFG)",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  output$totalTNC_pendapatan <- renderValueBox({
    valueBox(
      value = scales::comma(total_pendapatan_airlines()$TNC),
      subtitle = "Total TNC (TNCDOM + TNCINTL)",
      icon = icon("chart-pie"),
      color = "green"
    )
  })
  
  # TOTAL PENJUALAN AIRLINES
  total_penjualan_airlines <- reactive({
    df <- df_penjualan_airlines()
    total_enc <- sum(df$ENCDOM_TOT, df$ENCINTL_TOT, df$ENCOFG_TOT, na.rm = TRUE)
    total_tnc <- sum(df$TNCDOM_TOT, df$TNCINTL_TOT, na.rm = TRUE)
    list(ENC = total_enc, TNC = total_tnc)
  })
  
  output$totalENC_penjualan <- renderValueBox({
    valueBox(
      value = scales::comma(total_penjualan_airlines()$ENC),
      subtitle = "Total ENC (ENCDOM + ENCINTL + ENCOFG)",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  output$totalTNC_penjualan <- renderValueBox({
    valueBox(
      value = scales::comma(total_penjualan_airlines()$TNC),
      subtitle = "Total TNC (TNCDOM + TNCINTL)",
      icon = icon("chart-pie"),
      color = "green"
    )
  })
  
  # TOTAL PRODUKSI AIRLINES
  total_produksi_airlines <- reactive({
    
    df <- df_produksi_airlines()
    total_enc <- sum(df$ENCDOM_TOT, df$ENCINTL_TOT, df$ENCOFG_TOT, na.rm = TRUE)
    total_tnc <- sum(df$TNCDOM_TOT, df$TNCINTL_TOT, na.rm = TRUE)
    list(ENC = total_enc, TNC = total_tnc)
  })
  
  output$totalENC_produksi <- renderValueBox({
    valueBox(
      value = scales::comma(total_produksi_airlines()$ENC),
      subtitle = "Total ENC (ENCDOM + ENCINTL + ENCOFG)",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  output$totalTNC_produksi <- renderValueBox({
    valueBox(
      value = scales::comma(total_produksi_airlines()$TNC),
      subtitle = "Total TNC (TNCDOM + TNCINTL)",
      icon = icon("chart-pie"),
      color = "green"
    )
  })
  
  # PIECHART PIUTANG
  piechart_piutang <- reactive({
    df_piutang() %>%
      group_by(KEPEMILIKAN) %>%
      summarise(
        total_restrukturasi = sum(as.numeric(RESTRUK_DESEMBER), na.rm = TRUE),
        total_non_restrukturasi = sum(as.numeric(NONRESTRUK_DESEMBER), na.rm = TRUE)
      ) %>%
      pivot_longer(cols = starts_with("total"), names_to = "group", values_to = "value") %>%
      mutate(
        group = recode(group,
                       "total_restrukturasi" = "Restrukturasi",
                       "total_non_restrukturasi" = "Non-Restrukturasi"),
        label = paste(KEPEMILIKAN, group, sep = " - "),
        csum = rev(cumsum(rev(value))),
        pos = value / 2 + lead(csum, default = 0),
        percentage = value / sum(value) * 100,
        rupiah = paste0("Rp", comma(value))
      )
  })
  
  # OUTPUT VISUAL PIECHART PIUTANG
  output$pie_chart <- renderPlot({
    data <- piechart_piutang()
    
    ggplot(data, aes(x = "", y = value, fill = label)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Pastel1") +
      geom_label_repel(
        aes(
          y = pos,
          label = paste0(label, "\n", sprintf("%.2f%%", percentage), "\n", rupiah)
        ),
        size = 4, nudge_x = 1, show.legend = FALSE
      ) +
      guides(fill = guide_legend(title = "Kategori - Kepemilikan")) +
      theme_void() +  # Menghapus grid
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),  
        panel.background = element_rect(fill = "transparent", color = NA), 
        legend.background = element_rect(fill = "transparent", color = NA) 
      ) +
      labs(title = "Proporsi Restrukturasi dan Non-Restrukturasi per Kepemilikan")
  })
  
  
  # TOTAL PIUTANG, PENJUALAN, PENERIMAAN TAHUNAN
  total_p3 <- reactive({
    df <- df_detail_tahunan()
    
    num_cols <- c("PIUTANG_2020", "PIUTANG_2021", "PIUTANG_2022", "PIUTANG_2023", "PIUTANG_2024",
                  "PENJUALAN_2020", "PENJUALAN_2021", "PENJUALAN_2022", "PENJUALAN_2023", "PENJUALAN_2024",
                  "PENERIMAAN_2020", "PENERIMAAN_2021", "PENERIMAAN_2022", "PENERIMAAN_2023", "PENERIMAAN_2024")
    
    df[num_cols] <- lapply(df[num_cols], function(x) {
      x <- gsub("[^0-9.-]", "", x) 
      as.numeric(x) 
    })
    
    data.frame(
      Tahun = c(2020, 2021, 2022, 2023, 2024),
      Piutang = c(
        sum(df$PIUTANG_2020, na.rm = TRUE),
        sum(df$PIUTANG_2021, na.rm = TRUE),
        sum(df$PIUTANG_2022, na.rm = TRUE),
        sum(df$PIUTANG_2023, na.rm = TRUE),
        sum(df$PIUTANG_2024, na.rm = TRUE)
      ),
      Penjualan = c(
        sum(df$PENJUALAN_2020, na.rm = TRUE),
        sum(df$PENJUALAN_2021, na.rm = TRUE),
        sum(df$PENJUALAN_2022, na.rm = TRUE),
        sum(df$PENJUALAN_2023, na.rm = TRUE),
        sum(df$PENJUALAN_2024, na.rm = TRUE)
      ),
      Penerimaan = c(
        sum(df$PENERIMAAN_2020, na.rm = TRUE),
        sum(df$PENERIMAAN_2021, na.rm = TRUE),
        sum(df$PENERIMAAN_2022, na.rm = TRUE),
        sum(df$PENERIMAAN_2023, na.rm = TRUE),
        sum(df$PENERIMAAN_2024, na.rm = TRUE)
      )
    )
  })
  
  total_p3_long <- reactive({
    total_p3() %>%
      gather(key = "Kategori", value = "Total", -Tahun)
  })
  
  # VISUALISASI LINE CHART PIUTANG, PENJUALAN, DAN PENERIMAAN
  output$line_chart <- renderPlot({
    ggplot(total_p3_long(), aes(x = Tahun, y = Total, color = Kategori, group = Kategori)) +
      geom_line(size = 1.5) +  
      geom_point(size = 3) +    
      labs(
        title = "Perbandingan Piutang, Penjualan, dan Penerimaan (2020-2024)",
        x = "Tahun",
        y = "Total",
        color = "Kategori"
      ) +
      theme_minimal() +  
      theme(
        legend.position = "top",   
        plot.title = element_text(hjust = 0.5)  
      )
  })
  
  # TOTAL PIUTANG PERBULAN
  total_piutang_perbulan <- reactive({
    df_piutang() %>%
      pivot_longer(
        cols = starts_with("RESTRUK") | starts_with("NONRESTRUK"),
        names_to = c("kategori", "bulan"),
        names_sep = "_",
        values_to = "nilai"
      ) %>%
      group_by(kategori, bulan) %>%
      summarise(total = sum(as.numeric(nilai), na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = kategori, values_from = total)
  })
  
  urutan_bulan <- c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
  
  piutang_long <- reactive({
    total_piutang_perbulan() %>%
      mutate(bulan = factor(bulan, levels = urutan_bulan)) %>%
      arrange(bulan) %>%
      pivot_longer(
        cols = c("RESTRUK", "NONRESTRUK"),
        names_to = "kategori",
        values_to = "nilai"
      )
  })
  
  # VISUALISASI PIUTANG PERBULAN RESTRUK VS NONRESTRUK
  output$bar_chart <- renderPlot({
    ggplot(piutang_long(), aes(x = bulan, y = nilai, fill = kategori)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_fill_brewer(palette = "Pastel1") +
      labs(
        title = "Total Piutang per Bulan",
        x = "Bulan",
        y = "Total Piutang (IDR)",
        fill = "Kategori"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
  })
  # Mengolah data penjualan
  total_penjualan <- reactive({
    df_penjualan() %>%
      select(JANUARI:DESEMBER) %>%
      summarise(across(JANUARI:DESEMBER, \(x) sum(x, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "bulan", values_to = "total_penjualan")
  })
  
  # Mengolah data penerimaan
  total_penerimaan <- reactive({
    df_penerimaan() %>%
      select(JANUARI:DESEMBER) %>%
      summarise(across(JANUARI:DESEMBER, \(x) sum(x, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "bulan", values_to = "total_penerimaan")
  })
  
  # Menggabungkan total penjualan dan penerimaan
  comb_terima <- reactive({
    total_penjualan() %>%
      inner_join(total_penerimaan(), by = "bulan") %>%
      pivot_longer(
        cols = c(total_penjualan, total_penerimaan),
        names_to = "kategori",
        values_to = "total"
      ) %>%
      mutate(bulan = factor(bulan, levels = c(
        "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
        "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
      )))
  })
  
  # Output untuk bar chart
  output$jual_terima_chart <- renderPlot({
    ggplot(comb_terima(), aes(x = bulan, y = total, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Perbandingan Total Penjualan dan Penerimaan per Bulan",
        x = "Bulan",
        y = "Total",
        fill = "Kategori"
      ) +
      scale_fill_manual(
        values = c(
          "total_penjualan" = "blue", 
          "total_penerimaan" = "green"
        ),
        labels = c("Penjualan", "Penerimaan")
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # TOTAL P3 ENC TNC
  data_vis_p3 <- reactive({
    columns <- c(
      "ENCDOM_JAN", "ENCINTL_JAN", "ENCOFG_JAN", "TNCDOM_JAN", "TNCINTL_JAN",
      "ENCDOM_FEB", "ENCINTL_FEB", "ENCOFG_FEB", "TNCDOM_FEB", "TNCINTL_FEB",
      "ENCDOM_MAR", "ENCINTL_MAR", "ENCOFG_MAR", "TNCDOM_MAR", "TNCINTL_MAR",
      "ENCDOM_APR", "ENCINTL_APR", "ENCOFG_APR", "TNCDOM_APR", "TNCINTL_APR",
      "ENCDOM_MEI", "ENCINTL_MEI", "ENCOFG_MEI", "TNCDOM_MEI", "TNCINTL_MEI",
      "ENCDOM_JUN", "ENCINTL_JUN", "ENCOFG_JUN", "TNCDOM_JUN", "TNCINTL_JUN",
      "ENCDOM_JUL", "ENCINTL_JUL", "ENCOFG_JUL", "TNCDOM_JUL", "TNCINTL_JUL",
      "ENCDOM_AGU", "ENCINTL_AGU", "ENCOFG_AGU", "TNCDOM_AGU", "TNCINTL_AGU",
      "ENCDOM_SEP", "ENCINTL_SEP", "ENCOFG_SEP", "TNCDOM_SEP", "TNCINTL_SEP",
      "ENCDOM_NOV", "ENCINTL_NOV", "ENCOFG_NOV", "TNCDOM_NOV", "TNCINTL_NOV",
      "ENCDOM_DES", "ENCINTL_DES", "ENCOFG_DES", "TNCDOM_DES", "TNCINTL_DES"
    )
    
    df <- df_produksi_airlines()
    missing_columns <- setdiff(columns, colnames(df))
    if (length(missing_columns) > 0) {
      stop(paste("Kolom berikut tidak ditemukan di dataframe:", paste(missing_columns, collapse = ", ")))
    }
    total_values <- colSums(df[, columns], na.rm = TRUE)
    data.frame(
      Month = rep(c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "NOV", "DES"), each = 5),
      Category = rep(c("ENCDOM", "ENCINTL", "ENCOFG", "TNCDOM", "TNCINTL"), times = 11),
      Total = total_values
    )
  })
  
  # VISUALISASI P3 ENC TNC CHART
  output$p3Chart <- renderPlot({
    ggplot(data_vis_p3(), aes(x = Month, y = Total, fill = Category)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Total Per Bulan untuk Setiap Kategori", x = "Bulan", y = "Total") +
      scale_fill_manual(values = c(
        "ENCDOM" = "blue", 
        "ENCINTL" = "red", 
        "ENCOFG" = "green", 
        "TNCDOM" = "orange", 
        "TNCINTL" = "purple"
      )) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$trend_piutang_chart <- renderPlot({
    # Membuat dataset tren piutang untuk maskapai tertentu
    trend_piutang_maskapai <- df_detail_tahunan() %>%
      filter(NAMA_CUSTOMER == "ASI PUJIASTUTI AVIATION, PT.") %>%  
      pivot_longer(
        cols = starts_with("PIUTANG_"),  
        names_to = "tahun",              
        values_to = "piutang"            
      ) %>%
      mutate(tahun = as.numeric(gsub("PIUTANG_", "", tahun))) %>%  
      filter(tahun >= 2020 & tahun <= 2024) %>%  
      select(ID_CUSTOMER, NAMA_CUSTOMER, tahun, piutang)  
    
    # Membuat plot tren piutang
    ggplot(trend_piutang_maskapai, aes(x = tahun, y = piutang)) +
      geom_line(color = "blue", size = 1) +  
      geom_point(color = "red", size = 3) +  
      labs(
        title = "Trend Piutang ASI PUJIASTUTI AVIATION, PT.",
        x = "Tahun",
        y = "Piutang (IDR)"
      ) +
      theme_minimal()  # Menggunakan tema minimal
  })
  
  
  observe({
    req(df_piutang())  
    
    total_piutang <- df_piutang() %>%
      filter(NAMA_CUSTOMER == "ASI PUJIASTUTI AVIATION, PT.") %>%
      summarise(
        total_restrukturasi = sum(c(RESTRUK_JANUARI, RESTRUK_FEBRUARI, RESTRUK_MARET, RESTRUK_APRIL, RESTRUK_MEI, RESTRUK_JUNI,
                                    RESTRUK_JULI, RESTRUK_AGUSTUS, RESTRUK_SEPTEMBER, RESTRUK_OKTOBER, RESTRUK_NOVEMBER, RESTRUK_DESEMBER), na.rm = TRUE),
        total_nonrestruk = sum(c(NONRESTRUK_JANUARI, NONRESTRUK_FEBRUARI, NONRESTRUK_MARET, NONRESTRUK_APRIL, NONRESTRUK_MEI, NONRESTRUK_JUNI,
                                 NONRESTRUK_JULI, NONRESTRUK_AGUSTUS, NONRESTRUK_SEPTEMBER, NONRESTRUK_OKTOBER, NONRESTRUK_NOVEMBER, NONRESTRUK_DESEMBER), na.rm = TRUE)
      )
    
    # Update nilai di dalam value box
    output$restruk_value <- renderText({
      paste0("Rp ", format(total_piutang$total_restrukturasi, big.mark = ","))
    })
    
    output$nonrestruk_value <- renderText({
      paste0("Rp ", format(total_piutang$total_nonrestruk, big.mark = ","))
    })
  })
  
  output$trend_piutang_plot <- renderPlot({
    # Pastikan data tersedia
    req(df_piutang())
    
    # Proses data untuk trend piutang bulanan
    trend_piutang_bulanan <- df_piutang() %>%  
      filter(NAMA_CUSTOMER == "ASI PUJIASTUTI AVIATION, PT.") %>%  
      select(ID_CUSTOMER, NAMA_CUSTOMER, starts_with("TOTAL_")) %>%
      pivot_longer(
        cols = starts_with("TOTAL_"),   
        names_to = "bulan",             
        values_to = "total_piutang"     
      ) %>%
      mutate(bulan = gsub("TOTAL_", "", bulan))  
    
    # Membuat plot menggunakan ggplot2
    ggplot(trend_piutang_bulanan, aes(x = bulan, y = total_piutang, group = 1)) +
      geom_line(color = "blue", size = 1) +  
      geom_point(color = "red", size = 3) +  
      labs(
        title = "Trend Piutang per Bulan (ASI PUJIASTUTI AVIATION, PT.)",
        x = "Bulan",
        y = "Total Piutang (IDR)"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = c(
        "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
        "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
      ))
  })
  
}

