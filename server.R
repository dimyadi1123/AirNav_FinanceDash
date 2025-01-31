library(shiny)
library(DT)
library(gsheet)
library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggrepel)
library(reactable)
library(plotly)

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
        percentage = round(value / sum(value) * 100, 2),
        rupiah = paste0("Rp ", scales::comma(value))
      )
  })
  
  output$pie_chart <- renderPlotly({
    data <- piechart_piutang()
    
    plot_ly(
      data,
      labels = ~label,
      values = ~value,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(label, "<br>", "Rp", scales::comma(value)),
      marker = list(
        colors = c("#F29F58", "#AB4459", "#441752"),  # Ganti warna dengan kode hex
        line = list(color = "#FFFFFF", width = 1)
      )
    ) %>%
      layout(
        showlegend = TRUE,
        paper_bgcolor = 'rgba(0,0,0,0)',   # Transparan
        plot_bgcolor = 'rgba(0,0,0,0)'    # Transparan
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
      value = scales::comma(total_pendapatan_airlines()$ENC, prefix = "Rp ", big.mark = ".", decimal.mark = ","),
      subtitle = "Pendapatan ENC",
      icon = icon("briefcase"),
      color = "blue"
    )
  })
  
  output$totalTNC_pendapatan <- renderValueBox({
    valueBox(
      value = scales::comma(total_pendapatan_airlines()$TNC, prefix = "Rp ", big.mark = ".", decimal.mark = ","),
      subtitle = "Pendapatan TNC",
      icon = icon("money-bill-wave"),
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
      value = scales::comma(total_penjualan_airlines()$ENC, prefix = "Rp ", big.mark = ".", decimal.mark = ","),
      subtitle = "Penjualan ENC",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$totalTNC_penjualan <- renderValueBox({
    valueBox(
      value = scales::comma(total_penjualan_airlines()$TNC, prefix = "Rp ", big.mark = ".", decimal.mark = ","),
      subtitle = "Penjualan TNC",
      icon = icon("dollar-sign"),
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
      subtitle = "Produksi ENC",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  output$totalTNC_produksi <- renderValueBox({
    valueBox(
      value = scales::comma(total_produksi_airlines()$TNC),
      subtitle = "Produksi TNC",
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
  output$line_chart <- renderPlotly({
    # Buat plot dengan warna custom
    p <- ggplot(total_p3_long(), aes(x = Tahun, y = Total, color = Kategori, group = Kategori)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      scale_color_manual(
        values = c("Piutang" = "red", "Penjualan" = "gold", "Penerimaan" = "blue")
      ) +
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
    
    # Konversi ke plotly dan atur axis serta hover
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(
        xaxis = list(title = "Tahun"),
        yaxis = list(
          title = "Total (IDR)",
          tickprefix = "Rp ",
          ticksuffix = ",00",
          tickformat = ",.0f"
        ),
        hovermode = "x unified",
        margin = list(t = 50, b = 80)
      ) %>%
      style(
        hovertemplate = paste(
          "Tahun: %{x}",
          "<br>Kategori: %{color}",
          "<br>Total: Rp%{y:,.0f}",
          "<extra></extra>"
        )
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
  output$bar_chart <- renderPlotly({
    # Membuat plot ggplot
    p <- ggplot(piutang_long(), aes(x = bulan, y = nilai, fill = kategori)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_fill_manual(
        values = c("RESTRUK" = "skyblue", "NONRESTRUK" = "salmon"),
        labels = c("Restrukturasi", "Non-Restrukturasi")
      ) +
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
    
    # Konversi ggplot menjadi plotly untuk interaktivitas
    ggplotly(p) %>%
      layout(
        title = "Total Piutang per Bulan",
        xaxis = list(title = "Bulan"),
        yaxis = list(
          title = "Total Piutang (IDR)", 
          tickprefix = "Rp ",  # Tambahkan prefix Rp
          tickformat = ",.0f"  # Format IDR tanpa desimal
        ),
        hovermode = "closest",
        margin = list(t = 50, b = 80)
      ) %>%
      style(
        hovertemplate = paste(
          "Bulan: %{x}",
          "<br>Kategori: %{legendgroup}",
          "<br>Total: Rp%{y:,.0f}",  # Format Rupiah pada hover
          "<extra></extra>"
        )
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
  output$jual_terima_chart <- renderPlotly({
    # Membuat plot ggplot
    p <- ggplot(comb_terima(), aes(x = bulan, y = total, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Perbandingan Total Penjualan dan Penerimaan per Bulan",
        x = "Bulan",
        y = "Total (IDR)",
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
    
    # Konversi ggplot menjadi plotly untuk interaktivitas
    ggplotly(p) %>%
      layout(
        title = "Perbandingan Total Penjualan dan Penerimaan per Bulan",
        xaxis = list(title = "Bulan"),
        yaxis = list(
          title = "Total (IDR)",
          tickprefix = "Rp ",   # Prefix rupiah
          tickformat = ",.0f"   # Format angka tanpa desimal
        ),
        hovermode = "closest",
        margin = list(t = 50, b = 80)
      ) %>%
      style(
        hovertemplate = paste(
          "Bulan: %{x}",
          "<br>Kategori: %{legendgroup}",
          "<br>Total: Rp%{y:,.0f}",
          "<extra></extra>"
        )
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
  
  # VISUALISASI P3 ENC TNC CHART
  output$p3Chart <- renderPlotly({
    p <- ggplot(data_vis_p3(), aes(x = Month, y = Total, fill = Category)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Total Per Bulan untuk Setiap Kategori",
        x = "Bulan",
        y = "Total (IDR)"
      ) +
      scale_fill_manual(values = c(
        "ENCDOM" = "blue", 
        "ENCINTL" = "red", 
        "ENCOFG" = "green", 
        "TNCDOM" = "orange", 
        "TNCINTL" = "purple"
      )) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Konversi ggplot menjadi plotly
    ggplotly(p) %>%
      layout(
        title = "Total Per Bulan untuk Setiap Kategori",
        xaxis = list(title = "Bulan"),
        yaxis = list(
          title = "Total (IDR)", 
          tickprefix = "Rp ",  # Prefix rupiah di sumbu Y
          tickformat = ",.0f"  # Format angka tanpa desimal
        ),
        hovermode = "closest", 
        margin = list(t = 50, b = 80)
      ) %>%
      style(
        hovertemplate = paste(
          "Bulan: %{x}",
          "<br>Kategori: %{legendgroup}",
          "<br>Total: Rp%{y:,.0f}",
          "<extra></extra>"
        )
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
    top_10 <- top_10_piutang_tahunan()
    
    # Format kolom PIUTANG_TAHUN menjadi Rupiah
    top_10$PIUTANG_TAHUN <- scales::comma(top_10$PIUTANG_TAHUN, prefix = "Rp ", big.mark = ".", decimal.mark = ",")
    
    # Tampilkan DataTable
    datatable(top_10, options = list(pageLength = 11))
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
    top10 <- top10_piutang_bln()
    
    # Format kolom RESTRUK, NONRESTRUK, dan TOTAL menjadi Rupiah
    top10$RESTRUK <- scales::comma(top10$RESTRUK, prefix = "Rp ", big.mark = ".", decimal.mark = ",")
    top10$NONRESTRUK <- scales::comma(top10$NONRESTRUK, prefix = "Rp ", big.mark = ".", decimal.mark = ",")
    top10$TOTAL <- scales::comma(top10$TOTAL, prefix = "Rp ", big.mark = ".", decimal.mark = ",")
    
    # Tampilkan DataTable
    datatable(top10, options = list(pageLength = 11))
  })
  
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
  
  # Render DataTable untuk Penerimaan Bulanan dengan format Rupiah
  output$tabel_penerimaan_bulanan <- renderDataTable({
    top10 <- top10_penerimaan_bln()
    
    # Format kolom PENERIMAAN menjadi Rupiah
    top10$PENERIMAAN <- scales::comma(top10$PENERIMAAN, prefix = "Rp ", big.mark = ".", decimal.mark = ",")
    
    # Tampilkan DataTable
    datatable(top10, options = list(pageLength = 11))
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
  
  # TOP 10 Penjualan
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
    top10 <- penjualan_top10()
    
    # Identifikasi kolom numerik yang perlu diformat
    numerik_cols <- setdiff(colnames(top10), c("NAMA_CUSTOMER", "KEPEMILIKAN"))
    
    # Format setiap kolom numerik menjadi Rupiah
    for (col in numerik_cols) {
      if (col %in% colnames(top10)) {
        top10[[col]] <- as.numeric(top10[[col]])  # Konversi menjadi numerik
        top10[[col]] <- ifelse(is.na(top10[[col]]), 0, top10[[col]])  # Ganti NA dengan 0
        top10[[col]] <- scales::comma(top10[[col]], prefix = "Rp ", big.mark = ".", decimal.mark = ",")  # Format ke Rupiah
      }
    }
    
    # Tampilkan DataTable
    datatable(top10, options = list(pageLength = 11))
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
    top10 <- pendapatan_top10()
    
    # Identifikasi kolom numerik yang perlu diformat
    numerik_cols <- setdiff(colnames(top10), c("NAMA_CUSTOMER", "KEPEMILIKAN"))
    
    # Format setiap kolom numerik menjadi Rupiah
    for (col in numerik_cols) {
      top10[[col]] <- as.numeric(top10[[col]])  # Konversi menjadi numerik
      top10[[col]] <- ifelse(is.na(top10[[col]]), 0, top10[[col]])  # Ganti NA dengan 0
      top10[[col]] <- scales::comma(top10[[col]], prefix = "Rp ", big.mark = ".", decimal.mark = ",")  # Format ke Rupiah
    }
    
    # Tampilkan DataTable
    datatable(top10, options = list(pageLength = 11))
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
      color = "yellow"
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
                        "Tidak Beroperasi" = "lime",
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
    df <- data.frame(
      Tahun = c("Rasio"),
      `2021` = round(data$RASIO_2021, 2),
      `2022` = round(data$RASIO_2022, 2),
      `2023` = round(data$RASIO_2023, 2),
      `2024` = round(data$RASIO_2024, 2)
    )
    setNames(df, c("Tahun", "2021", "2022", "2023", "2024"))
  })
  
  # Output tabel dengan reactable
  output$rasio_table <- renderReactable({
    reactable(rasio_data())
  })
  
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
      value = scales::percent(npl_percent(), accuracy = 0.1),  # Format persentase dengan 1 angka desimal
      subtitle = "Non Performing Loan",
      color = "orange"
    )
  })
  
  output$trend_piutang_chart <- renderPlotly({
    # Data untuk tren piutang maskapai
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
    
    # Membuat plot ggplot
    p <- ggplot(trend_piutang_maskapai, aes(x = tahun, y = piutang)) +
      geom_line(color = "blue", size = 1) +  
      geom_point(color = "red", size = 3) +  
      labs(
        title = paste("Trend Piutang untuk", input$nama_customer),
        x = "Tahun",
        y = "Piutang (IDR)"
      ) +
      theme_minimal()
    
    # Konversi ggplot ke plotly dengan layout dan hover interaktif
    ggplotly(p) %>%
      layout(
        title = paste("Trend Piutang untuk", input$nama_customer),
        xaxis = list(title = "Tahun"),
        yaxis = list(
          title = "Piutang (IDR)", 
          tickprefix = "Rp ",  
          tickformat = ",.0f"  
        ),
        hovermode = "x unified",
        margin = list(t = 50, b = 80)
      ) %>%
      style(
        hovertemplate = paste(
          "Tahun: %{x}",
          "<br>Piutang: Rp%{y:,.0f}",
          "<extra></extra>"
        )
      )
  })
  
  
  output$trend_piutang_plot <- renderPlotly({
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
    
    # Membuat plot dengan ggplot
    p <- ggplot(trend_piutang_bulanan, aes(x = bulan, y = total_piutang, group = 1)) +
      geom_line(color = "blue", size = 1) +  
      geom_point(color = "red", size = 3) +  
      labs(
        title = paste("Trend Piutang per Bulan", input$nama_customer),
        x = "Bulan",
        y = "Total Piutang (IDR)"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = c(
        "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
        "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
      ))
    
    # Konversi ke plotly untuk interaktivitas
    ggplotly(p) %>%
      layout(
        title = paste("Trend Piutang per Bulan", input$nama_customer),
        xaxis = list(title = "Bulan"),
        yaxis = list(
          title = "Total Piutang (IDR)",
          tickprefix = "Rp ",
          tickformat = ",.0f"
        ),
        hovermode = "x unified",
        margin = list(t = 50, b = 80)
      ) %>%
      style(
        hovertemplate = paste(
          "Bulan: %{x}",
          "<br>Total Piutang: Rp%{y:,.0f}",
          "<extra></extra>"
        )
      )
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
      color = "green"
    )
  })
  
  # Similar modifications for other value boxes
  output$late_31_180 <- renderValueBox({
    late_31_180 <- sum(npl_data()$KETERLAMBATAN_31_180_HARI, na.rm = TRUE)
    valueBox(
      value = format(late_31_180, big.mark = ","),
      subtitle = "Keterlambatan 31-180 Hari",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  # Value Box: Total Keterlambatan 181-270 Hari
  output$late_181_270 <- renderValueBox({
    late_181_270 <- sum(npl_data()$KETERLAMBATAN_181_270_HARI, na.rm = TRUE)
    valueBox(
      value = format(late_181_270, big.mark = ","),
      subtitle = "Keterlambatan 181-270 Hari",
      icon = icon("clock"),
      color = "orange"
    )
  })
  
  # Value Box: Total Keterlambatan 270 Hari Lebih
  output$late_270_more <- renderValueBox({
    late_270_more <- sum(npl_data()$KETERLAMBATAN_270_HARI_LEBIH, na.rm = TRUE)
    valueBox(
      value = format(late_270_more, big.mark = ","),
      subtitle = "Keterlambatan > 270 Hari",
      icon = icon("clock"),
      color = "red"
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
  
  # Data Produksi Enroute
  prod_enroute <- reactive({
    df_produksi_airlines() %>%  # Tambahkan () jika df_produksi_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("ENCDOM"), starts_with("ENCINTL"), starts_with("ENCOFG")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "Bulan"), names_sep = "_", values_to = "produksi") %>%
      mutate(kategori = recode(kategori, ENCDOM = "DOM", ENCINTL = "INTL", ENCOFG = "OFG"),
             Bulan = factor(Bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(Bulan))
  })
  
  # Data Produksi Terminal Navigation
  prod_tnc <- reactive({
    df_produksi_airlines() %>%  # Tambahkan () jika df_produksi_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("TNCDOM"), starts_with("TNCINTL")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "Bulan"), names_sep = "_", values_to = "produksi") %>%
      mutate(kategori = recode(kategori, TNCDOM = "DOM", TNCINTL = "INTL"),
             Bulan = factor(Bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(Bulan))
  })
  
  ## Plot Produksi Enroute
  output$prod_enroute_plot <- renderPlotly({
    p <- ggplot(prod_enroute(), aes(x = Bulan, y = produksi, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green", "OFG" = "orange")) +
      labs(title = "Produksi Enroute (RU)",
           x = "Bulan", y = "Produksi (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(
        title = "Produksi Enroute (RU)",
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Produksi (RU)"),
        margin = list(t = 50, b = 80)
      )
  })
  
  # Plot Produksi Terminal Navigation
  output$prod_tnc_plot <- renderPlotly({
    p <- ggplot(prod_tnc(), aes(x = Bulan, y = produksi, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green")) +
      labs(title = "Produksi Terminal Navigation (TMOW)",
           x = "Bulan", y = "Produksi (TMOW)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(
        title = "Produksi Terminal Navigation (TMOW)",
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Produksi (TMOW)"),
        margin = list(t = 50, b = 80)
      )
  })
  
  # Data Penjualan Enroute
  jual_enroute <- reactive({
    df_penjualan_airlines() %>%  # Tambahkan () jika df_penjualan_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("ENCDOM"), starts_with("ENCINTL"), starts_with("ENCOFG")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "Bulan"), names_sep = "_", values_to = "penjualan") %>%
      mutate(kategori = recode(kategori, ENCDOM = "DOM", ENCINTL = "INTL", ENCOFG = "OFG"),
             Bulan = factor(Bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(Bulan))
  })
  
  # Data Penjualan Terminal Navigation
  jual_tnc <- reactive({
    df_penjualan_airlines() %>%  # Tambahkan () jika df_penjualan_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("TNCDOM"), starts_with("TNCINTL")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "Bulan"), names_sep = "_", values_to = "penjualan") %>%
      mutate(kategori = recode(kategori, TNCDOM = "DOM", TNCINTL = "INTL"),
             Bulan = factor(Bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(Bulan))
  })
  
  # Plot Penjualan Enroute
  output$jual_enroute_plot <- renderPlotly({
    p <- ggplot(jual_enroute(), aes(x = Bulan, y = penjualan, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green", "OFG" = "orange")) +
      labs(title = "Penjualan Enroute (RU)",
           x = "Bulan", y = "Penjualan (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(
        yaxis = list(
          title = "Penjualan (RU)",
          tickprefix = "Rp ",
          tickformat = ",.0f"
        )
      ) %>%
      style(hovertemplate = paste("Bulan: %{x}",
                                  "<br>Kategori: %{color}",
                                  "<br>Penjualan: Rp%{y:,.0f}",
                                  "<extra></extra>"))
  })
  
  # Plot Penjualan Terminal Navigation
  output$jual_tnc_plot <- renderPlotly({
    p <- ggplot(jual_tnc(), aes(x = Bulan, y = penjualan, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green")) +
      labs(title = "Penjualan Terminal Navigation (TMOW)",
           x = "Bulan", y = "Penjualan (TMOW)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(
        yaxis = list(
          title = "Penjualan (TMOW)",
          tickprefix = "Rp ",
          tickformat = ",.0f"
        )
      ) %>%
      style(hovertemplate = paste("Bulan: %{x}",
                                  "<br>Kategori: %{color}",
                                  "<br>Penjualan: Rp%{y:,.0f}",
                                  "<extra></extra>"))
  })
  
  # Data Pendapatan Enroute
  dapat_enroute <- reactive({
    df_pendapatan_airlines() %>%  # Tambahkan () jika df_pendapatan_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("ENCDOM"), starts_with("ENCINTL"), starts_with("ENCOFG")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "Bulan"), names_sep = "_", values_to = "pendapatan") %>%
      mutate(kategori = recode(kategori, ENCDOM = "DOM", ENCINTL = "INTL", ENCOFG = "OFG"),
             Bulan = factor(Bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(Bulan))
  })
  
  # Data Pendapatan Terminal Navigation
  dapat_tnc <- reactive({
    df_pendapatan_airlines() %>%  # Tambahkan () jika df_pendapatan_airlines adalah reactive
      filter(NAMA_CUSTOMER == input$nama_customer) %>%
      select(starts_with("TNCDOM"), starts_with("TNCINTL")) %>%
      pivot_longer(cols = everything(), names_to = c("kategori", "Bulan"), names_sep = "_", values_to = "pendapatan") %>%
      mutate(kategori = recode(kategori, TNCDOM = "DOM", TNCINTL = "INTL"),
             Bulan = factor(Bulan, levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"))) %>%
      filter(!is.na(Bulan))
  })
  
  # Plot Pendapatan Enroute
  output$dapat_enroute_plot <- renderPlotly({
    p <- ggplot(dapat_enroute(), aes(x = Bulan, y = pendapatan, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green", "OFG" = "orange")) +
      labs(title = "Pendapatan Enroute (RU)",
           x = "Bulan", y = "Pendapatan (RU)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(
        yaxis = list(
          title = "Pendapatan (RU)",
          tickprefix = "Rp ",
          tickformat = ",.0f"
        )
      ) %>%
      style(hovertemplate = paste("Bulan: %{x}",
                                  "<br>Kategori: %{color}",
                                  "<br>Pendapatan: Rp%{y:,.0f}",
                                  "<extra></extra>"))
  })
  
  # Plot Pendapatan Terminal Navigation
  output$dapat_tnc_plot <- renderPlotly({
    p <- ggplot(dapat_tnc(), aes(x = Bulan, y = pendapatan, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("DOM" = "blue", "INTL" = "green")) +
      labs(title = "Pendapatan Terminal Navigation (TMOW)",
           x = "Bulan", y = "Pendapatan (TMOW)", fill = "Kategori") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(
        yaxis = list(
          title = "Pendapatan (TMOW)",
          tickprefix = "Rp ",
          tickformat = ",.0f"
        )
      ) %>%
      style(hovertemplate = paste("Bulan: %{x}",
                                  "<br>Kategori: %{color}",
                                  "<br>Pendapatan: Rp%{y:,.0f}",
                                  "<extra></extra>"))
  })
  
  output$prod_enroute_table <- renderDT({
    prod_data_wide <- prod_enroute() %>%
      pivot_wider(
        names_from = kategori,
        values_from = produksi,
        values_fill = 0
      ) %>%
      arrange(match(Bulan, c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", 
                             "JUL", "AGU", "SEP", "OKT", "NOV", "DES")))
    
    datatable(prod_data_wide, options = list(pageLength = 12))  # Menambahkan pagination
  })
  
  # Output DataTable untuk Produksi Terminal Navigation
  output$prod_tnc_table <- renderDT({
    prod_data_wide <- prod_tnc() %>%
      pivot_wider(
        names_from = kategori,
        values_from = produksi,
        values_fill = 0
      ) %>%
      arrange(match(Bulan, c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", 
                             "JUL", "AGU", "SEP", "OKT", "NOV", "DES")))
    
    datatable(prod_data_wide, options = list(pageLength = 12))  # Menambahkan pagination
  })
  
  # Output DataTable untuk Penjualan Enroute
  output$jual_enroute_table <- renderDT({
    jual_data_wide <- jual_enroute() %>%
      pivot_wider(
        names_from = kategori,
        values_from = penjualan,
        values_fill = 0
      ) %>%
      arrange(match(Bulan, c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", 
                             "JUL", "AGU", "SEP", "OKT", "NOV", "DES")))
    
    datatable(jual_data_wide, options = list(pageLength = 12)) %>%
      formatCurrency(c("DOM", "INTL", "OFG"), "Rp ", digits = 0)  # Format Rupiah dengan 0 desimal
  })
  
  # Output DataTable untuk Penjualan Terminal Navigation
  output$jual_tnc_table <- renderDT({
    jual_data_wide <- jual_tnc() %>%
      pivot_wider(
        names_from = kategori,
        values_from = penjualan,
        values_fill = 0
      ) %>%
      arrange(match(Bulan, c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", 
                             "JUL", "AGU", "SEP", "OKT", "NOV", "DES")))
    
    datatable(jual_data_wide, options = list(pageLength = 12)) %>%
      formatCurrency(c("DOM", "INTL"), "Rp ", digits = 0)  # Format Rupiah dengan 0 desimal
  })
  
  # Output DataTable untuk Pendapatan Enroute
  output$dapat_enroute_table <- renderDT({
    dapat_data_wide <- dapat_enroute() %>%
      pivot_wider(
        names_from = kategori,
        values_from = pendapatan,
        values_fill = 0
      ) %>%
      arrange(match(Bulan, c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", 
                             "JUL", "AGU", "SEP", "OKTO", "NOV", "DES")))
    
    datatable(dapat_data_wide, options = list(pageLength = 12)) %>%
      formatCurrency(c("DOM", "INTL", "OFG"), "Rp ", digits = 0)  # Format Rupiah dengan 0 desimal
  })
  
  # Output DataTable untuk Pendapatan Terminal Navigation
  output$dapat_tnc_table <- renderDT({
    dapat_data_wide <- dapat_tnc() %>%
      pivot_wider(
        names_from = kategori,
        values_from = pendapatan,
        values_fill = 0
      ) %>%
      arrange(match(Bulan, c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", 
                             "JUL", "AGU", "SEP", "OKT", "NOV", "DES")))
    
    datatable(dapat_data_wide, options = list(pageLength = 12)) %>%
      formatCurrency(c("DOM", "INTL"), "Rp ", digits = 0)  # Format Rupiah dengan 0 desimal
  })
}