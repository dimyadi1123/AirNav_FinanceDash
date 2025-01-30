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
server <- function(input, output, session) {
  
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
  
  df_npl <- reactive({
    read_excel('npl.xlsx')
  })
  
  df_acp <- reactive({
    read_excel('acp.xlsx')
  })
  
  df_profil <- reactive({
    read_excel('profil_maskapai.xlsx')
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
    ) %>% 
      tagAppendAttributes(class = "small-value-box")
  })
  
  # OUTPUT PENERIMAAN DOMESTIK
  output$total_penerimaan_domestik <- renderValueBox({
    data <- total_penerimaan_kepemilikan()
    domestik <- data %>% filter(KEPEMILIKAN == "DOMESTIK") %>% pull(total_penerimaan_2024)
    
    valueBox(
      value = paste0("Rp ", comma(domestik)), 
      subtitle = "Total Penerimaan (DOMESTIK)", 
      icon = icon("home"), 
      color = "green"
    ) %>% 
      tagAppendAttributes(class = "small-value-box")
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
      labs(title = "Proporsi Piutang secara Keseluruhan")
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
  
  # TOTAL PIUTANG, PENJUALAN, PENERIMAAN TAHUNAN
  total_p3 <- reactive({
    df <- df_detail_tahunan()
    
    num_cols <- c("PIUTANG_2020", "PIUTANG_2021", "PIUTANG_2022", "PIUTANG_2023", "PIUTANG_2024",
                  "PENJUALAN_2020", "PENJUALAN_2021", "PENJUALAN_2022", "PENJUALAN_2023", "PENJUALAN_2024",
                  "PENERIMAAN_2020", "PENERIMAAN_2021", "PENERIMAAN_2022", "PENERIMAAN_2023", "PENERIMAAN_2024")
    
    # Filter berdasarkan pilihan kepemilikan
    if (!("DOMESTIK" %in% input$kepemilikan_filter)) {
      df <- df %>% filter(KEPEMILIKAN != "DOMESTIK")
    }
    if (!("ASING" %in% input$kepemilikan_filter)) {
      df <- df %>% filter(KEPEMILIKAN != "ASING")
    }
    
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
      # Filter berdasarkan kepemilikan
      filter(KEPEMILIKAN %in% input$kepemilikan_filter) %>%
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
      # Filter berdasarkan kepemilikan
      filter(KEPEMILIKAN %in% input$kepemilikan_filter) %>%
      select(JANUARI:DESEMBER) %>%
      summarise(across(JANUARI:DESEMBER, \(x) sum(x, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "bulan", values_to = "total_penjualan")
  })
  
  total_penerimaan <- reactive({
    df_penerimaan() %>%
      # Filter berdasarkan kepemilikan
      filter(KEPEMILIKAN %in% input$kepemilikan_filter) %>%
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
    
    df <- df %>% filter(KEPEMILIKAN %in% input$kepemilikan_filter)
    
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
  
  ## DASHBOARD TABEL TOP 10
  
  # Top 10 Piutang Tahunan
  top_10_piutang_tahunan <- reactive({
    req(df_detail_tahunan(), input$filter_tahun_piutang)
    
    piutang_col <- paste0("PIUTANG_", input$filter_tahun_piutang)
    
    df_filtered <- df_detail_tahunan() %>%
      filter(KEPEMILIKAN == input$filter_kepemilikan) %>%
      select(NAMA_CUSTOMER, KEPEMILIKAN, all_of(piutang_col)) %>%
      rename(PIUTANG_TAHUN = all_of(piutang_col))
    
    df_sorted <- df_filtered %>% arrange(desc(PIUTANG_TAHUN))
    
    top_10 <- head(df_sorted, 10)
    others <- df_sorted[-(1:10), ]
    total_others <- sum(others$PIUTANG_TAHUN, na.rm = TRUE)
    
    top_10 <- rbind(
      top_10,
      data.frame(
        NAMA_CUSTOMER = "Airline Lainnya",
        KEPEMILIKAN = input$filter_kepemilikan,
        PIUTANG_TAHUN = total_others
      )
    )
    
    return(top_10)
  })
  
  output$top10_piutang_thn <- renderDataTable({
    top_10_piutang_tahunan()
  })
  
  output$judul_top10_piutang_thn <- renderText({
    paste("TOP 10 Piutang Customer Tahun", input$filter_tahun_piutang)
  })
  
  # Top 10 Piutang Bulanan
  top10_piutang_bln <- reactive({
    req(df_piutang(), input$filter_bulan)
    
    # Sesuaikan nama kolom berdasarkan bulan
    restruk_col <- paste0("RESTRUK_", input$filter_bulan)
    nonrestruk_col <- paste0("NONRESTRUK_", input$filter_bulan)
    total_col <- paste0("TOTAL_", input$filter_bulan)
    
    df_filtered <- df_piutang() %>%
      filter(KEPEMILIKAN == input$filter_kepemilikan) %>%
      select(NAMA_CUSTOMER, all_of(restruk_col), all_of(nonrestruk_col), all_of(total_col)) %>%
      rename(
        RESTRUK = all_of(restruk_col),
        NONRESTRUK = all_of(nonrestruk_col),
        TOTAL = all_of(total_col)
      )
    
    df_sorted <- df_filtered %>% arrange(desc(TOTAL))
    top_10 <- head(df_sorted, 10)
    others <- df_sorted[-(1:10), ]  
    total_others <- others %>% summarise(
      RESTRUK = sum(RESTRUK, na.rm = TRUE),
      NONRESTRUK = sum(NONRESTRUK, na.rm = TRUE),
      TOTAL = sum(TOTAL, na.rm = TRUE)
    )
    
    others_row <- data.frame(
      NAMA_CUSTOMER = "Airline Lainnya",
      RESTRUK = total_others$RESTRUK,
      NONRESTRUK = total_others$NONRESTRUK,
      TOTAL = total_others$TOTAL
    )
    
    top_10 <- rbind(top_10, others_row)
    
    return(top_10)
  })
  
  output$tabel_top10_piutang_bln <- renderDataTable({
    top10_piutang_bln()
  })
  
  # Top 10 Penerimaan Bulanan
  top10_penerimaan_bln <- reactive({
    req(df_penerimaan(), input$filter_bulan)
    
    bulan_col <- input$filter_bulan
    
    df_filtered <- df_penerimaan() %>%
      filter(KEPEMILIKAN == input$filter_kepemilikan) %>%
      select(NAMA_CUSTOMER, KEPEMILIKAN, all_of(bulan_col)) %>%
      rename(PENERIMAAN = all_of(bulan_col))
    
    df_sorted <- df_filtered %>% arrange(desc(PENERIMAAN))
    top_10 <- head(df_sorted, 10)
    others <- df_sorted[-(1:10), ]  
    total_others <- sum(others$PENERIMAAN, na.rm = TRUE)
    
    others_row <- data.frame(
      NAMA_CUSTOMER = "Airline Lainnya",
      KEPEMILIKAN = input$filter_kepemilikan,
      PENERIMAAN = total_others
    )
    
    top_10 <- rbind(top_10, others_row)
    
    return(top_10)
  })
  
  output$tabel_penerimaan_bulanan <- renderDataTable({
    top10_penerimaan_bln()
  })
  
  # Produksi Bulanan
  produksi_top10 <- reactive({
    req(df_produksi_airlines(), input$filter_bulan)
    
    bulan_abbr <- toupper(substr(input$filter_bulan, 1, 3))
    
    # Seleksi kolom produksi sesuai bulan
    produksi_cols <- c(
      paste0("ENCDOM_", bulan_abbr),
      paste0("ENCINTL_", bulan_abbr),
      paste0("ENCOFG_", bulan_abbr),
      paste0("TNCDOM_", bulan_abbr),
      paste0("TNCINTL_", bulan_abbr)
    )
    
    df_filtered <- df_produksi_airlines() %>%
      filter(KEPEMILIKAN == input$filter_kepemilikan) %>%
      select(NAMA_CUSTOMER, KEPEMILIKAN, all_of(produksi_cols)) %>%
      mutate(TOTAL_PRODUKSI = rowSums(across(all_of(produksi_cols)), na.rm = TRUE))
    
    df_sorted <- df_filtered %>% arrange(desc(TOTAL_PRODUKSI))
    top_10 <- head(df_sorted, 10)
    others <- df_sorted[-(1:10), ]
    
    # Total untuk Airline Lainnya
    total_others <- colSums(others[, produksi_cols], na.rm = TRUE)
    
    others_row <- c(
      NAMA_CUSTOMER = "Airline Lainnya",
      KEPEMILIKAN = input$filter_kepemilikan,
      as.list(total_others),
      TOTAL_PRODUKSI = sum(total_others)
    )
    
    top_10 <- rbind(top_10, others_row)
    
    return(top_10)
  })
  
  output$produksi_top10_bln <- renderDataTable({
    produksi_top10()
  })
  
  penjualan_top10 <- reactive({
    req(df_penjualan_airlines(), input$filter_bulan)
    
    bulan_abbr <- toupper(substr(input$filter_bulan, 1, 3))
    
    penjualan_cols <- c(
      paste0("ENCDOM_", bulan_abbr),
      paste0("ENCINTL_", bulan_abbr),
      paste0("ENCOFG_", bulan_abbr),
      paste0("TNCDOM_", bulan_abbr),
      paste0("TNCINTL_", bulan_abbr)
    )
    
    df_filtered <- df_penjualan_airlines() %>%
      filter(KEPEMILIKAN == input$filter_kepemilikan) %>%
      select(NAMA_CUSTOMER, KEPEMILIKAN, all_of(penjualan_cols)) %>%
      mutate(TOTAL_PENJUALAN = rowSums(across(all_of(penjualan_cols)), na.rm = TRUE))
    
    df_sorted <- df_filtered %>% arrange(desc(TOTAL_PENJUALAN))
    top_10 <- head(df_sorted, 10)
    others <- df_sorted[-(1:10), ]
    
    total_others <- colSums(others[, penjualan_cols, drop = FALSE], na.rm = TRUE)
    
    others_row <- c(
      NAMA_CUSTOMER = "Airline Lainnya",
      KEPEMILIKAN = input$filter_kepemilikan,
      as.list(total_others),
      TOTAL_PENJUALAN = sum(total_others)
    )
    
    top_10 <- rbind(top_10, others_row)
    
    return(top_10)
  })
  
  output$penjualan_top10_bln <- renderDataTable({
    penjualan_top10()
  })
  
  # Pendapatan Bulanan
  pendapatan_top10 <- reactive({
    req(df_pendapatan_airlines(), input$filter_bulan)
    
    bulan_abbr <- toupper(substr(input$filter_bulan, 1, 3))
    
    pendapatan_cols <- c(
      paste0("ENCDOM_", bulan_abbr),
      paste0("ENCINTL_", bulan_abbr),
      paste0("ENCOFG_", bulan_abbr),
      paste0("TNCDOM_", bulan_abbr),
      paste0("TNCINTL_", bulan_abbr)
    )
    
    df_filtered <- df_pendapatan_airlines() %>%
      filter(KEPEMILIKAN == input$filter_kepemilikan) %>%
      select(NAMA_CUSTOMER, KEPEMILIKAN, all_of(pendapatan_cols)) %>%
      mutate(TOTAL_PENDAPATAN = rowSums(across(all_of(pendapatan_cols)), na.rm = TRUE))
    
    df_sorted <- df_filtered %>% arrange(desc(TOTAL_PENDAPATAN))
    top_10 <- head(df_sorted, 10)
    others <- df_sorted[-(1:10), ]
    
    total_others <- colSums(others[, pendapatan_cols, drop = FALSE], na.rm = TRUE)
    
    others_row <- c(
      NAMA_CUSTOMER = "Airline Lainnya",
      KEPEMILIKAN = input$filter_kepemilikan,
      as.list(total_others),
      TOTAL_PENDAPATAN = sum(total_others)
    )
    
    top_10 <- rbind(top_10, others_row)
    
    return(top_10)
  })
  
  output$pendapatan_top10_bln <- renderDataTable({
    pendapatan_top10()
  })
  
  ## DASHBOARD DETAIL MASKAPAI
  # Mengisi pilihan maskapai dari df_profil
  observe({
    updateSelectizeInput(
      session,
      inputId = "nama_customer",
      choices = df_profil()$NAMA_CUSTOMER,
      server = TRUE
    )
  })
  
  # Profil Maskapaoi
  output$airlineprofile <- renderUI({
    req(df_profil())
    
    airline_profile <- df_profil() %>%
      filter(NAMA_CUSTOMER == input$nama_customer)
    
    # Create the profile layout using grid
    tagList(
      wellPanel(
        h3(input$nama_customer),
        div(class = "profile-container", 
            div(class = "profile-item", 
                tags$strong("Nama Customer"), 
                span(class = "value", airline_profile$NAMA_CUSTOMER)
            ),
            div(class = "profile-item", 
                tags$strong("ID Customer"), 
                span(class = "value", airline_profile$ID_CUSTOMER)
            ),
            div(class = "profile-item", 
                tags$strong("Street"), 
                span(class = "value", airline_profile$Street)
            ),
            div(class = "profile-item", 
                tags$strong("Postal Code"), 
                span(class = "value", airline_profile$`Postal Code`)
            ),
            div(class = "profile-item", 
                tags$strong("City"), 
                span(class = "value", airline_profile$City)
            ),
            div(class = "profile-item", 
                tags$strong("Telephone"), 
                span(class = "value", airline_profile$`Telephone 1`)
            ),
            div(class = "profile-item", 
                tags$strong("Email Address"), 
                span(class = "value", airline_profile$`Email Address`)
            )
        )
      )
    )
  })
  
  # ACP
  acp_data <- reactive({
    req(df_acp())  # Ensure the data is loaded
    df_acp() %>%
      filter(NAMA_CUSTOMER == input$nama_customer)
  })
  
  latest_acp <- reactive({
    customer_data <- acp_data()
    
    acp_columns <- grep("^ACP_", names(customer_data), value = TRUE)  
    acp_values <- as.numeric(customer_data[1, acp_columns])  
    
    latest_value <- rev(acp_values[acp_values > 0])[1]
    
    if (is.na(latest_value)) {
      return(0) 
    } else {
      return(latest_value)
    }
  })
  
  output$recent_acp <- renderValueBox({
    valueBox(
      format(latest_acp(), big.mark = ","),
      subtitle = "ACP Terbaru",
      color = "blue"
    )
  })
  
  kolektabilitas <- reactive({
    acp <- latest_acp()
    
    if (acp == 0) {
      return("Tidak Beroperasi")
    } else if (acp < 15) {
      return("Lancar")
    } else if (acp < 46) {
      return("Kurang Lancar")
    } else if (acp < 76) {
      return("Diragukan")
    } else {
      return("Macet")
    }
  })
  
  output$kolektabilitas_piutang <- renderValueBox({
    category <- kolektabilitas()  # Dapatkan kategori kolektabilitas
    
    # Tentukan warna berdasarkan kategori
    box_color <- switch(category,
                        "Tidak Beroperasi" = "lightblue",
                        "Lancar" = "green",
                        "Kurang Lancar" = "yellow",
                        "Diragukan" = "orange",
                        "Macet" = "red")
    
    valueBox(
      value = category,  # Menampilkan kategori kolektabilitas
      subtitle = "Kolektabilitas Piutang",
      color = box_color  # Warna sesuai kategori
    )
  })
  
  rasio_maskapai <- reactive({
    req(df_detail_tahunan())  # Ensure the data is loaded
    df_detail_tahunan() %>%
      filter(NAMA_CUSTOMER == input$nama_customer)
  })
  
  # Data untuk ditampilkan di tabel
  rasio_data <- reactive({
    data <- rasio_maskapai()
    data.frame(
      Tahun = 2021:2024,
      Rasio = c(data$RASIO_2021, data$RASIO_2022, data$RASIO_2023, data$RASIO_2024)
    )
  })
  
  # Output tabel
  output$rasio_table <- renderTable({
    rasio_data()
  }, rownames = FALSE)
  
  # Output plot
  output$rasio_plot <- renderPlot({
    data <- rasio_data()
    plot(data$Tahun, data$Rasio, type = "o", col = "blue", lwd = 2,
         xlab = "Tahun", ylab = "Rasio Perputaran Piutang",
         main = "Tren Rasio Perputaran Piutang")
    grid()
  })
  
  npl_percent <- reactive({
    req(df_npl())  
    npl_filtered <- df_npl() %>% filter(NAMA_CUSTOMER == input$nama_customer)
    npl_filtered$NPL_PERCENT
  })
  
  output$npl_percent <- renderValueBox({
    valueBox(
      format(npl_percent(),  nsmall = 2),
      subtitle = "Non Performing Loan",
      color = "blue"
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
    trend_piutang_maskapai <- df_detail_tahunan() %>%
      filter(NAMA_CUSTOMER == input$nama_customer) %>%  
      pivot_longer(
        cols = starts_with("PIUTANG_"),  
        names_to = "tahun",              
        values_to = "piutang"            
      ) %>%
      mutate(tahun = as.numeric(gsub("PIUTANG_", "", tahun))) %>%  
      filter(tahun >= 2020 & tahun <= 2024) %>%  
      select(ID_CUSTOMER, NAMA_CUSTOMER, tahun, piutang)  
    
    ggplot(trend_piutang_maskapai, aes(x = tahun, y = piutang)) +
      geom_line(color = "blue", size = 1) +  
      geom_point(color = "red", size = 3) +  
      labs(
        title = "Trend Piutang {nama_customer}",
        x = "Tahun",
        y = "Piutang (IDR)"
      ) +
      theme_minimal()  # Menggunakan tema minimal
  })
  
  # Aging
  npl_data <- reactive({
    req(df_npl())  # Ensure the data is loaded
    df_npl() %>%
      filter(NAMA_CUSTOMER == input$nama_customer)
  })
  
  output$late_0_30 <- renderValueBox({
    late_0_30 <- sum(npl_data()$KETERLAMBATAN_0_30_HARI, na.rm = TRUE)
    valueBox(
      value = format(late_0_30, big.mark = ","),
      subtitle = "Keterlambatan 0-30 Hari",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  # Similar modifications for other value boxes
  output$late_31_180 <- renderValueBox({
    late_31_180 <- sum(npl_data()$KETERLAMBATAN_31_180_HARI, na.rm = TRUE)
    valueBox(
      value = format(late_31_180, big.mark = ","),
      subtitle = "Keterlambatan 31-180 Hari",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  # Value Box: Total Keterlambatan 181-270 Hari
  output$late_181_270 <- renderValueBox({
    late_181_270 <- sum(npl_data()$KETERLAMBATAN_181_270_HARI, na.rm = TRUE)
    valueBox(
      value = format(late_181_270, big.mark = ","),
      subtitle = "Keterlambatan 181-270 Hari",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  # Value Box: Total Keterlambatan 270 Hari Lebih
  output$late_270_more <- renderValueBox({
    late_270_more <- sum(npl_data()$KETERLAMBATAN_270_HARI_LEBIH, na.rm = TRUE)
    valueBox(
      value = format(late_270_more, big.mark = ","),
      subtitle = "Keterlambatan > 270 Hari",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  # Piutang maskapai
  observe({
    req(df_piutang())  
    
    total_piutang <- df_piutang() %>%
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      summarise(
        total_restrukturasi = sum(c(RESTRUK_JANUARI, RESTRUK_FEBRUARI, RESTRUK_MARET, RESTRUK_APRIL, RESTRUK_MEI, RESTRUK_JUNI,
                                    RESTRUK_JULI, RESTRUK_AGUSTUS, RESTRUK_SEPTEMBER, RESTRUK_OKTOBER, RESTRUK_NOVEMBER, RESTRUK_DESEMBER), na.rm = TRUE),
        total_nonrestruk = sum(c(NONRESTRUK_JANUARI, NONRESTRUK_FEBRUARI, NONRESTRUK_MARET, NONRESTRUK_APRIL, NONRESTRUK_MEI, NONRESTRUK_JUNI,
                                 NONRESTRUK_JULI, NONRESTRUK_AGUSTUS, NONRESTRUK_SEPTEMBER, NONRESTRUK_OKTOBER, NONRESTRUK_NOVEMBER, NONRESTRUK_DESEMBER), na.rm = TRUE)
      )
    
    format_currency <- function(x) {
      paste0("Rp ", format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    }
    
    output$restruk_value_box <- renderValueBox({
      valueBox(
        value = format_currency(total_piutang$total_restrukturasi),
        subtitle = "Total Piutang Restrukturisasi",
        color = "blue"
      )
    })
    
    output$nonrestruk_value_box <- renderValueBox({
      valueBox(
        value = format_currency(total_piutang$total_nonrestruk),
        subtitle = "Total Piutang Non-Restrukturisasi",
        color = "green"
      )
    })
  })
  
  output$trend_piutang_plot <- renderPlot({
    # Pastikan data tersedia
    req(df_piutang())
    
    # Proses data untuk trend piutang bulanan
    trend_piutang_bulanan <- df_piutang() %>%  
      filter(NAMA_CUSTOMER == input$nama_customer) %>%  
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
        title = "Trend Piutang per Bulan {input$nama_customer}",
        x = "Bulan",
        y = "Total Piutang (IDR)"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = c(
        "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
        "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
      ))
  })
  
  # Data Produksi Enroute
  prod_enroute <- reactive({
    df_produksi_airlines() %>%  # Tambahkan () jika df_produksi_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("ENCDOM"), starts_with("ENCINTL"), starts_with("ENCOFG")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "bulan"), names_sep = "_", values_to = "produksi") %>%
      mutate(kategori = recode(kategori, ENCDOM = "DOM", ENCINTL = "INTL", ENCOFG = "OFG"),
             bulan = factor(bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(bulan))
  })
  
  # Data Produksi Terminal Navigation
  prod_tnc <- reactive({
    df_produksi_airlines() %>%  # Tambahkan () jika df_produksi_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("TNCDOM"), starts_with("TNCINTL")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "bulan"), names_sep = "_", values_to = "produksi") %>%
      mutate(kategori = recode(kategori, TNCDOM = "DOM", TNCINTL = "INTL"),
             bulan = factor(bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(bulan))
  })
  
  # Plot Produksi Enroute
  output$prod_enroute_plot <- renderPlot({
    ggplot(prod_enroute(), aes(x = bulan, y = produksi, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green", "OFG" = "orange")) +
      labs(title = "Produksi Enroute (RU)",
           x = "Bulan", y = "Produksi (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot Produksi Terminal Navigation
  output$prod_tnc_plot <- renderPlot({
    ggplot(prod_tnc(), aes(x = bulan, y = produksi, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green")) +
      labs(title = "Produksi Terminal Navigation (RU)",
           x = "Bulan", y = "Produksi (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Data Penjualan Enroute
  jual_enroute <- reactive({
    df_penjualan_airlines() %>%  # Tambahkan () jika df_penjualan_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("ENCDOM"), starts_with("ENCINTL"), starts_with("ENCOFG")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "bulan"), names_sep = "_", values_to = "penjualan") %>%
      mutate(kategori = recode(kategori, ENCDOM = "DOM", ENCINTL = "INTL", ENCOFG = "OFG"),
             bulan = factor(bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(bulan))
  })
  
  # Data Penjualan Terminal Navigation
  jual_tnc <- reactive({
    df_penjualan_airlines() %>%  # Tambahkan () jika df_penjualan_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("TNCDOM"), starts_with("TNCINTL")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "bulan"), names_sep = "_", values_to = "penjualan") %>%
      mutate(kategori = recode(kategori, TNCDOM = "DOM", TNCINTL = "INTL"),
             bulan = factor(bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(bulan))
  })
  
  # Plot Penjualan Enroute
  output$jual_enroute_plot <- renderPlot({
    ggplot(jual_enroute(), aes(x = bulan, y = penjualan, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green", "OFG" = "orange")) +
      labs(title = "Penjualan Enroute (RU)",
           x = "Bulan", y = "Penjualan (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot Penjualan Terminal Navigation
  output$jual_tnc_plot <- renderPlot({
    ggplot(jual_tnc(), aes(x = bulan, y = penjualan, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green")) +
      labs(title = "Penjualan Terminal Navigation (RU)",
           x = "Bulan", y = "Penjualan (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Data Pendapatan Enroute
  dapat_enroute <- reactive({
    df_pendapatan_airlines() %>%  # Tambahkan () jika df_pendapatan_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("ENCDOM"), starts_with("ENCINTL"), starts_with("ENCOFG")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "bulan"), names_sep = "_", values_to = "pendapatan") %>%
      mutate(kategori = recode(kategori, ENCDOM = "DOM", ENCINTL = "INTL", ENCOFG = "OFG"),
             bulan = factor(bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(bulan))
  })
  
  # Data Pendapatan Terminal Navigation
  dapat_tnc <- reactive({
    df_pendapatan_airlines() %>%  # Tambahkan () jika df_pendapatan_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("TNCDOM"), starts_with("TNCINTL")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "bulan"), names_sep = "_", values_to = "pendapatan") %>%
      mutate(kategori = recode(kategori, TNCDOM = "DOM", TNCINTL = "INTL"),
             bulan = factor(bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(bulan))
  })
  
  # Plot Pendapatan Enroute
  output$dapat_enroute_plot <- renderPlot({
    ggplot(dapat_enroute(), aes(x = bulan, y = pendapatan, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green", "OFG" = "orange")) +
      labs(title = "Pendapatan Enroute (RU)",
           x = "Bulan", y = "Pendapatan (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot Pendapatan Terminal Navigation
  output$dapat_tnc_plot <- renderPlot({
    ggplot(dapat_tnc(), aes(x = bulan, y = pendapatan, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green")) +
      labs(title = "Pendapatan Terminal Navigation (RU)",
           x = "Bulan", y = "Pendapatan (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}