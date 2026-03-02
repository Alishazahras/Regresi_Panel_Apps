# ======================================================
# 1. LIBRARY
# ======================================================
library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(ggplot2)
library(dplyr)
library(plm)
library(reshape2)
library(viridis)
library(car)      
library(lmtest)  


# ======================================================
# 2. UI (TAMPILAN)
# ======================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      tags$img(src = "Pneu.png", height = "55px",
               style = "margin-right:2px;"),
      "PneuPanel"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      # 1. MENU TUTORIAL
      menuItem("Tutorial Website", tabName = "tutorial", icon = icon("info-circle")),
      
      # 2. MENU LANDASAN TEORI
      menuItem("Landasan Teori", tabName = "teori", icon = icon("book")),
      
      # 3. DATA & STATISTIK
      menuItem("Data & Statistik", tabName = "data", icon = icon("table")),
      
      # 4. ANALISIS LAINNYA
      menuItem("Visualisasi", tabName = "viz", icon = icon("chart-line")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Pemodelan Panel", tabName = "model", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    withMathJax(),
    
    tags$head(tags$style(HTML("
      .box { margin-bottom: 20px; }
      .dataTables_wrapper { overflow-x: auto; }
      .teori-box { background-color: #f9f9f9; padding: 15px; border-radius: 5px; border-left: 5px solid #3c8dbc; margin-bottom: 10px;}
     
      .row-flex { display: flex; flex-wrap: wrap; }
      .col-flex { display: flex; flex-direction: column; margin-bottom: 20px; }
      .news-card {
        flex: 1; /* Mengisi ruang vertikal */
        border: 1px solid #e1e1e1;
        padding: 15px;
        border-radius: 8px;
        background: white;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        display: flex;
        flex-direction: column;
        justify-content: space-between; /* Mendorong link ke bawah */
      }
      .news-content { margin-bottom: 15px; }
      .news-title { font-weight: bold; color: #2c3e50; font-size: 15px; margin-bottom: 5px; }
      .news-source { font-size: 11px; color: #7f8c8d; margin-bottom: 10px; font-style: italic;}
      .news-link { color: #3498db; font-weight: bold; text-decoration: none; font-size: 12px; align-self: flex-start; }
      .news-link:hover { text-decoration: underline; }
    "))),
    
    tabItems(
      
      # --- MENU 1: TUTORIAL WEBSITE ---
      tabItem(tabName = "tutorial",
              fluidRow(
                box(width = 12, title = "Panduan Penggunaan Website", status = "primary", solidHeader = TRUE,
                    tags$div(style = "font-size: 16px; line-height: 1.8; text-align: justify;",
                             
                             h4(strong("Selamat Datang di Aplikasi Analisis Regresi Data Panel")),
                             
                             p("Aplikasi ini dikembangkan untuk memfasilitasi analisis faktor-faktor yang memengaruhi variabel Y melalui pendekatan statistik Regresi Data Panel (gabungan data cross-section dan time-series)."),
                             
                             p("Untuk memastikan hasil analisis yang akurat dan terstruktur, silakan ikuti tahapan penggunaan aplikasi berikut ini:"),
                             
                             tags$ol(
                               # 1. Menu Tutorial
                               tags$li(style="margin-bottom: 10px;",
                                       strong("Menu Tutorial:"),
                                       " Menu ini menyediakan panduan singkat penggunaan aplikasi dari tahap input data hingga interpretasi hasil regresi panel."
                               ),
                               # 2. Landasan Teori
                               tags$li(style="margin-bottom: 10px;",
                                       strong("Menu Landasan Teori:"),
                                       " Mulailah dengan mempelajari dasar teori terkait Pneumonia serta konsep matematis Regresi Data Panel (CEM, FEM, REM). Bagian ini juga menjelaskan uji pemilihan model (Chow, Hausman, LM) agar Anda memahami konteks metode yang digunakan."
                               ),
                               
                               # 3. Data & Statistik
                               tags$li(style="margin-bottom: 10px;",
                                       strong("Menu Data & Statistik:"),
                                       " Tahap awal analisis teknis. Unggah file data Anda (format Excel .xlsx atau .csv). Sistem akan menyajikan ringkasan statistik deskriptif (seperti Mean, Minimum, Maksimum) untuk memberikan gambaran umum karakteristik data Anda."
                               ),
                               
                               # 4. Visualisasi
                               tags$li(style="margin-bottom: 10px;",
                                       strong("Menu Visualisasi:"),
                                       " Lakukan eksplorasi data visual untuk mengidentifikasi pola awal. Menu ini menampilkan grafik tren kasus antarwilayah, matriks korelasi antarvariabel, serta distribusi variabel dependen (Y) untuk melihat bentuk sebaran data kasus Pneumonia."
                               ),
                               
                               # 5. Uji Asumsi
                               tags$li(style="margin-bottom: 10px;",
                                       strong("Menu Uji Asumsi:"),
                                       " Sebelum pemodelan, validasi data sangat diperlukan. Menu ini menyediakan uji Pengaruh Waktu (One-Way ANOVA)  untuk mendeteksi adanya efek spesifik waktu/tahun serta uji Multikolinearitas (VIF) untuk memastikan antarvariabel bebas tidak memiliki korelasi yang tinggi."
                               ),
                               
                               # 6. Pemodelan Panel
                               tags$li(style="margin-bottom: 10px;",
                                       strong("Menu Pemodelan Panel:"),
                                       " Merupakan inti analisis. Tahap ini mencakup: (1) Penentuan model terbaik melalui uji statistik (Chow, Hausman, LM), dan (2) Estimasi model terpilih untuk menguji signifikansi pengaruh variabel, baik secara Simultan (Uji F) maupun Parsial (Uji t)."
                               )
                             ),
                             
                             br(),
                             p(icon("info-circle"), em("Catatan: Pastikan format data sesuai dengan petunjuk agar proses komputasi berjalan lancar."))
                    )
                )
              )
      ),
      
      # --- MENU 2: LANDASAN TEORI ---
      tabItem(tabName = "teori",
              fluidRow(
                box(width = 12, solidHeader = TRUE,
                    navlistPanel(
                      widths = c(3, 9),
                      "Topik Pembahasan",
                      
                      # --- SUB-MENU: PNEUMONIA ---
                      tabPanel("Pneumonia: Definisi & Berita",
                               h3("Tentang Pneumonia"),
                               
                               div(class = "teori-box",
                                   h4(icon("lungs"), "Definisi Pneumonia"),
                                   p("Pneumonia adalah infeksi saluran pernapasan akut yang menyerang jaringan paru-paru, termasuk alveoli, bronkiolus, dan bronkus, yang menyebabkan peradangan serta penumpukan cairan atau nanah di alveoli.")
                               ),
                               
                               div(class = "teori-box",
                                   h4(icon("list-ol"), "Faktor-Faktor Risiko (Variabel Penelitian)"),
                                   p("Berikut adalah faktor-faktor yang diduga memengaruhi kasus pneumonia:"),
                                   tags$ul(
                                     tags$li(HTML("X<sub>1</sub> : Persentase penduduk usia 35–44 tahun yang merokok.")),
                                     tags$li(HTML("X<sub>2</sub> : Persentase rumah tangga yang memiliki akses terhadap sanitasi layak.")),
                                     tags$li(HTML("X<sub>3</sub> : Persentase bayi kurang dari 6 bulan yang mendapatkan ASI eksklusif.")),
                                     tags$li(HTML("X<sub>4</sub> : Persentase pemberian vitamin A pada balita.")),
                                     tags$li(HTML("X<sub>5</sub> : Persentase Bayi Berat Lahir Rendah (BBLR).")),
                                     tags$li(HTML("X<sub>6</sub> : Persentase bayi gizi rendah (BB/U).")),
                                     tags$li(HTML("X<sub>7</sub> : Persentase penduduk miskin."))
                                   )
                               ),
                               
                               hr(),
                               
                               h4("📰 Berita Terkait Pneumonia"),
                               
                               div(class = "row row-flex",
                                   # Berita 1: WHO
                                   column(width = 6, class = "col-flex",
                                          div(class = "news-card",
                                              div(class = "news-content",
                                                  div(class = "news-title", "WHO: Pneumonia Fact Sheet"),
                                                  div(class = "news-source", "World Health Organization"),
                                                  p("Pneumonia adalah bentuk infeksi pernapasan akut yang menyerang paru-paru dan merupakan penyebab utama kematian menular pada anak-anak di seluruh dunia.")
                                              ),
                                              a(href = "https://www.who.int/news-room/fact-sheets/detail/pneumonia", target="_blank", class="news-link", "Baca Selengkapnya →")
                                          )
                                   ),
                                   # Berita 2: Kemenkes
                                   column(width = 6, class = "col-flex",
                                          div(class = "news-card",
                                              div(class = "news-content",
                                                  div(class = "news-title", "Pemerintah Berkomitmen Turunkan Kematian Akibat Pneumonia"),
                                                  div(class = "news-source", "Kementerian Kesehatan RI"),
                                                  p("Pemerintah Indonesia memperkuat komitmen global untuk melindungi anak-anak dari pneumonia melalui akses imunisasi dan layanan kesehatan yang lebih baik.")
                                              ),
                                              a(href = "https://kemkes.go.id/eng/pemerintah-berkomitmen-turunkan-kasus-kematian-akibat-pneumonia", target="_blank", class="news-link", "Baca Selengkapnya →")
                                          )
                                   )
                               ),
                               
                               
                               div(class = "row row-flex",
                                   # Berita 3: Liputan6
                                   column(width = 6, class = "col-flex",
                                          div(class = "news-card",
                                              div(class = "news-content",
                                                  div(class = "news-title", "Kasus Pneumonia di Indonesia Capai 800 Ribu"),
                                                  div(class = "news-source", "Liputan6 Health"),
                                                  p("Kasus pneumonia di Indonesia mencapai angka yang tinggi, dengan populasi balita dan lansia menjadi kelompok yang paling rentan dan berisiko tinggi.")
                                              ),
                                              a(href = "https://www.liputan6.com/health/read/6106987/kasus-pneumonia-di-indonesia-capai-800-ribu-balita-dan-lansia-berisiko-tinggi", target="_blank", class="news-link", "Baca Selengkapnya →")
                                          )
                                   ),
                                   # Berita 4: RSJRW
                                   column(width = 6, class = "col-flex",
                                          div(class = "news-card",
                                              div(class = "news-content",
                                                  div(class = "news-title", "Setiap Napas Berharga, Yuk Cegah Pneumonia"),
                                                  div(class = "news-source", "RSJ Dr. Radjiman Wediodiningrat"),
                                                  p("Edukasi mengenai pentingnya menjaga kesehatan paru-paru dan langkah-langkah pencegahan pneumonia untuk meningkatkan kualitas hidup masyarakat.")
                                              ),
                                              a(href = "https://rsjrw.id/artikel/setiap-napas-berharga-yuk-cegah-pneumonia", target="_blank", class="news-link", "Baca Selengkapnya →")
                                          )
                                   )
                               )
                      ),
                      
                      # --- SUB-MENU: REGRESI PANEL ---
                      tabPanel("Konsep Regresi Panel",
                               withMathJax(),
                               
                               h3(icon("book"), "Analisis Regresi Data Panel"),
                               p("Regresi data panel adalah metode statistik untuk data gabungan antara cross-section (individu/wilayah) dan time-series (waktu)."),
                               tags$br(),
                               
                               # 1. Common Effect Model (CEM)
                               div(style = "background-color: #ffffff; padding: 20px; border-radius: 5px; border-top: 3px solid #0073b7; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 20px;",
                                   h4(strong("1. Common Effect Model (CEM)")),
                                   p("Model paling sederhana yang mengasumsikan bahwa intersep dan slope konstan antar individu dan waktu (Pooling)."),
                                   
                                   # Rumus CEM
                                   p(strong("Persamaan Model:")),
                                   helpText("$$y_{it} = \\beta_0 + \\beta_1 x_{1,it} + \\beta_2 x_{2,it} + \\dots + \\beta_k x_{k,it} + \\epsilon_{it}$$"),
                                   
                                   # Keterangan
                                   p(strong("Di mana:")),
                                   tags$ul(style = "list-style-type: none; padding-left: 10px;",
                                           tags$li(tags$span(style="width: 30px; display: inline-block;", "\\(y_{it}\\)"), " = Variabel dependen untuk individu ke-i pada waktu ke-t."),
                                           tags$li(tags$span(style="width: 30px; display: inline-block;", "\\(\\beta_0\\)"), " = Konstanta pada model regresi."),
                                           tags$li(tags$span(style="width: 30px; display: inline-block;", "\\(\\beta_k\\)"), " = Koefisien regresi (slope) untuk variabel ke-k."),
                                           tags$li(tags$span(style="width: 30px; display: inline-block;", "\\(x_{k,it}\\)"), " = Variabel independen ke-k untuk individu ke-i pada waktu ke-t."),
                                           tags$li(tags$span(style="width: 30px; display: inline-block;", "\\(\\epsilon_{it}\\)"), " = Error untuk individu ke-i pada waktu ke-t.")
                                   )
                               ),
                               
                               # 2. Fixed Effect Model (FEM)
                               div(style = "background-color: #ffffff; padding: 20px; border-radius: 5px; border-top: 3px solid #00a65a; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 20px;",
                                   h4(strong("2. Fixed Effect Model (FEM)")),
                                   p("Model ini mengasumsikan adanya perbedaan intersep antar individu yang bersifat tetap (fixed) namun slope tetap sama."),
                                   
                                   # Rumus FEM
                                   p(strong("Persamaan Model:")),
                                   helpText("$$y_{it} = \\beta_{0i} + \\beta_1 x_{1,it} + \\beta_2 x_{2,it} + \\dots + \\beta_k x_{k,it} + \\epsilon_{it}$$"),
                                   
                                   # Keterangan
                                   p(strong("Di mana:")),
                                   tags$ul(style = "list-style-type: none; padding-left: 10px;",
                                           tags$li(tags$span(style="width: 30px; display: inline-block;", "\\(\\beta_{0i}\\)"), " = Intersep untuk individu ke-i pada waktu ke-t (Pembeda antar wilayah).")
                                   )
                               ),
                               
                               # 3. Random Effect Model (REM)
                               div(style = "background-color: #ffffff; padding: 20px; border-radius: 5px; border-top: 3px solid #f39c12; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 20px;",
                                   h4(strong("3. Random Effect Model (REM)")),
                                   p("Model ini mengasumsikan perbedaan karakteristik individu dan waktu diakomodasi melalui error term (efek acak)."),
                                   
                                   # Rumus REM
                                   p(strong("Persamaan Model:")),
                                   helpText("$$y_{it} = \\beta_0 + \\beta_1 x_{1,it} + \\dots + \\beta_k x_{k,it} + \\alpha_i + \\epsilon_{it}$$"),
                                   
                                   # Keterangan
                                   p(strong("Di mana:")),
                                   tags$ul(style = "list-style-type: none; padding-left: 10px;",
                                           tags$li(tags$span(style="width: 30px; display: inline-block;", "\\(\\alpha_i\\)"), " = Efek individual yang bersifat acak (Random Effect)."),
                                           tags$li(tags$span(style="width: 30px; display: inline-block;", "\\(\\epsilon_{it}\\)"), " = Error term murni.")
                                   )
                               )
                      ),
                      
                      # --- SUB-MENU: PEMILIHAN MODEL ---
                      tabPanel("Uji Pemilihan Model",
                               withMathJax(),
                               
                               h3(icon("clipboard-check"), "Menentukan Model Terbaik"),
                               p("Dalam regresi data panel, kita perlu memilih model yang paling tepat antara CEM, FEM, atau REM. Proses ini dilakukan melalui tiga tahapan uji statistik berikut:"),
                               
                               p(align="center", style="margin: 20px 0;", ""),
                               
                               tags$br(),
                               
                               # ===============================================================
                               # 1. Uji Chow (CEM vs FEM)
                               # ===============================================================
                               div(style = "background-color: #ffffff; padding: 20px; border-radius: 5px; border-top: 3px solid #0073b7; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 20px;",
                                   h4(strong("1. Uji Chow (Chow Test)")),
                                   p("Uji ini digunakan untuk membandingkan apakah model Common Effect (CEM) atau Fixed Effect (FEM) yang lebih layak digunakan."),
                                   
                                   # Hipotesis
                                   p(strong("Hipotesis:")),
                                   tags$ul(style = "list-style-type: none; padding-left: 10px;",
                                           tags$li("\\(H_0\\): Model Common Effect (CEM) lebih relevan."),
                                           tags$li("\\(H_1\\): Model Fixed Effect (FEM) lebih relevan.")
                                   ),
                                   
                                   # Rumus Chow
                                   p(strong("Statistik Uji:")),
                                   helpText("$$F_{Chow} = \\frac{(RSS_{CEM} - RSS_{FEM}) / (N-1)}{RSS_{FEM} / (N(T-1)-k)}$$"),
                                   
                                   # Keterangan Rumus
                                   tags$small(style="color: #666;",
                                              em("Dimana: RSS = Residual Sum of Squares, N = Jumlah Individu, T = Jumlah Periode, k = Jumlah Variabel Independen.")
                                   ),
                                   br(), br(),
                                   
                                   # Kriteria Keputusan
                                   div(style = "background-color: #f4f4f4; padding: 15px; border-left: 4px solid #0073b7;",
                                       strong("Kriteria Keputusan:"),
                                       p("Jika nilai \\(p-value < \\alpha (0.10)\\), maka Tolak \\(H_0\\)."),
                                       p(style="color: #0073b7; font-weight: bold; margin-bottom: 0;",
                                         "Kesimpulan: Pilih Model Fixed Effect (FEM).")
                                   )
                               ),
                               
                               # ===============================================================
                               # 2. Uji Hausman (FEM vs REM)
                               # ===============================================================
                               div(style = "background-color: #ffffff; padding: 20px; border-radius: 5px; border-top: 3px solid #00a65a; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 20px;",
                                   h4(strong("2. Uji Hausman (Hausman Test)")),
                                   p("Jika Uji Chow memilih FEM, maka selanjutnya dilakukan Uji Hausman untuk membandingkan Fixed Effect (FEM) dengan Random Effect (REM)."),
                                   
                                   # Hipotesis
                                   p(strong("Hipotesis:")),
                                   tags$ul(style = "list-style-type: none; padding-left: 10px;",
                                           tags$li("\\(H_0\\): Model Random Effect (REM) lebih relevan."),
                                           tags$li("\\(H_1\\): Model Fixed Effect (FEM) lebih relevan.")
                                   ),
                                   
                                   # Rumus Hausman
                                   p(strong("Statistik Uji:")),
                                   helpText("$$H = (\\hat{\\beta}_{RE} - \\hat{\\beta}_{FE})' [V_{FE} - V_{RE}]^{-1} (\\hat{\\beta}_{RE} - \\hat{\\beta}_{FE})$$"),
                                   
                                   tags$small(style="color: #666;",
                                              em("Dimana: \\(\\hat{\\beta}\\) = Koefisien estimasi, \\(V\\) = Matriks kovarians.")
                                   ),
                                   br(), br(),
                                   
                                   # Kriteria Keputusan
                                   div(style = "background-color: #f4f4f4; padding: 15px; border-left: 4px solid #00a65a;",
                                       strong("Kriteria Keputusan:"),
                                       p("Jika nilai \\(p-value < \\alpha (0.10)\\), maka Tolak \\(H_0\\)."),
                                       p(style="color: #00a65a; font-weight: bold; margin-bottom: 0;",
                                         "Kesimpulan: Pilih Model Fixed Effect (FEM).")
                                   )
                               ),
                               
                               # ===============================================================
                               # 3. Uji LM (CEM vs REM)
                               # ===============================================================
                               div(style = "background-color: #ffffff; padding: 20px; border-radius: 5px; border-top: 3px solid #f39c12; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 20px;",
                                   h4(strong("3. Uji Lagrange Multiplier (LM Test)")),
                                   p("Jika Uji Chow memilih CEM, maka dilakukan Uji LM untuk memastikan apakah Common Effect (CEM) atau Random Effect (REM) yang lebih baik."),
                                   
                                   # Hipotesis
                                   p(strong("Hipotesis:")),
                                   tags$ul(style = "list-style-type: none; padding-left: 10px;",
                                           tags$li("\\(H_0\\): Model Common Effect (CEM) lebih relevan."),
                                           tags$li("\\(H_1\\): Model Random Effect (REM) lebih relevan.")
                                   ),
                                   
                                   # Rumus LM
                                   p(strong("Statistik Uji:")),
                                   helpText("$$LM = \\frac{NT}{2(T-1)} \\left[ \\frac{\\sum_{i=1}^N (\\sum_{t=1}^T \\epsilon_{it})^2}{\\sum_{i=1}^N \\sum_{t=1}^T \\epsilon_{it}^2} - 1 \\right]^2$$"),
                                   
                                   tags$small(style="color: #666;",
                                              em("Dimana: \\(\\epsilon_{it}\\) = Residual dari model CEM.")
                                   ),
                                   br(), br(),
                                   
                                   # Kriteria Keputusan
                                   div(style = "background-color: #f4f4f4; padding: 15px; border-left: 4px solid #f39c12;",
                                       strong("Kriteria Keputusan:"),
                                       p("Jika nilai \\(p-value < \\alpha (0.10)\\), maka Tolak \\(H_0\\)."),
                                       p(style="color: #d35400; font-weight: bold; margin-bottom: 0;",
                                         "Kesimpulan: Pilih Model Random Effect (REM).")
                                   )
                               )
                      )
                    )
                )
              )
      ),
      
      # --- MENU 3: DATA & STATISTIK ---
      tabItem(tabName = "data",
              fluidRow(
                box(width = 4, title = "1. Upload Data", status = "primary", solidHeader = TRUE,
                    fileInput("file1", "Pilih File Excel (.xlsx)", accept = ".xlsx"),
                    helpText("Sistem otomatis mengonversi data ke Numerik.")
                ),
                box(width = 8, title = "2. Pilih Variabel", status = "warning", solidHeader = TRUE,
                    uiOutput("var_select_ui")
                )
              ),
              fluidRow(
                box(width = 12, title = "3. Preview Data (Variabel Terpilih)", status = "info", solidHeader = TRUE,
                    DTOutput("tbl_preview")
                )
              ),
              fluidRow(
                box(width = 12, title = "4. Statistik Deskriptif", status = "success", solidHeader = TRUE,
                    DTOutput("tbl_desc")
                )
              )
      ),
      
      # --- MENU 4: VISUALISASI ---
      tabItem(tabName = "viz",
              fluidRow(
                box(width = 4, title = "Pilih Visualisasi", status = "warning", solidHeader = TRUE,
                    selectInput("viz_type", "Jenis Grafik:",
                                choices = c("Tren per Wilayah",
                                            "Heatmap Korelasi",
                                            "Distribusi (Histogram)"))
                ),
                box(width = 8, title = "Output Visualisasi", status = "primary", solidHeader = TRUE,
                    uiOutput("viz_output_ui")
                )
              )
      ),
      
      # --- MENU 5: UJI ASUMSI  ---
      tabItem(tabName = "asumsi",
              fluidRow(
                box(width = 4, title = "Pilih Uji Asumsi", status = "warning", solidHeader = TRUE,
                    selectInput("asumsi_type", "Jenis Uji:",
                                choices = c("Uji Pengaruh Waktu (ANOVA)",
                                            "Uji Multikolinearitas (VIF)"))
                ),
                box(width = 8, title = "Hasil Uji & Interpretasi", status = "danger", solidHeader = TRUE,
                    uiOutput("asumsi_dinamis")
                )
              )
      ),
      
      # --- MENU 6: PEMODELAN ---
      tabItem(tabName = "model",
              fluidRow(
                # Box Input (Kiri)
                box(width = 3, title = "Konfigurasi Analisis", status = "warning", solidHeader = TRUE,
                    
                    selectInput("analysis_mode", "Pilih Jenis Analisis:",
                                choices = c("Common Effect Model (Pooling)",
                                            "Fixed Effect Model",
                                            "Random Effect Model",
                                            "Uji Pemilihan Model (Chow/Hausman/LM)"),
                                selectize = FALSE),
                    
                    hr(),
                    helpText("Pilih Model untuk melihat estimasi, atau pilih 'Uji Pemilihan Model' untuk menentukan model terbaik.")
                ),
                
                box(width = 9, title = "Hasil Analisis", status = "primary", solidHeader = TRUE,
                    uiOutput("dynamic_model_ui")
                )
              )
      )
    )
  )
)


# ======================================================
# 3. SERVER
# ======================================================
server <- function(input, output) {
  
  # --- 1. Load & Clean Data ---
  data_raw <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  data_clean <- reactive({
    req(data_raw(), input$var_y, input$var_x)
    df <- data_raw()
    
    vars_to_num <- c(input$var_y, input$var_x)
    for(v in vars_to_num){
      if(v %in% names(df)){
        df[[v]] <- as.numeric(as.character(df[[v]]))
      }
    }
    na.omit(df)
  })
  
  # --- 2.UI Pilihan Variabel ---
  output$var_select_ui <- renderUI({
    req(data_raw())
    cols <- names(data_raw())
    tagList(
      selectInput("var_y", "Variabel Dependen (Y):", choices = cols),
      selectInput("var_id", "Variabel Wilayah (ID):", choices = cols, selected = cols[1]),
      selectInput("var_time", "Variabel Waktu (Tahun):", choices = cols, selected = cols[2]),
      selectInput("var_x", "Variabel Independen (X):", choices = cols, multiple = TRUE)
    )
  })
  
  # --- 3. Preview & Desc Stats ---
  output$tbl_preview <- renderDT({
    req(data_clean(), input$var_id, input$var_time, input$var_y, input$var_x)
    df <- data_clean()
    selected_cols <- c(input$var_id, input$var_time, input$var_y, input$var_x)
    if(all(selected_cols %in% names(df))) {
      datatable(df[, selected_cols, drop = FALSE], options = list(scrollX = TRUE))
    } else { return(NULL) }
  })
  
  output$tbl_desc <- renderDT({
    req(data_clean(), input$var_y, input$var_x)
    df <- data_clean()
    vars <- df[, c(input$var_y, input$var_x), drop = FALSE]
    res <- data.frame(
      Variabel = names(vars),
      Mean = sapply(vars, mean, na.rm=T),
      Min = sapply(vars, min, na.rm=T),
      Max = sapply(vars, max, na.rm=T),
      SD = sapply(vars, sd, na.rm=T),
      Var = sapply(vars, var, na.rm=T)
    )
    datatable(res, rownames = FALSE)
  })
  
  # --- 4. Visualisasi ---
  output$viz_output_ui <- renderUI({
    req(input$viz_type)
    
    if(input$viz_type == "Tren per Wilayah") {
      tagList(
        plotOutput("plot_trend", height = "500px"),
        br(),
        uiOutput("trend_desc")
      )
    } else if (input$viz_type == "Heatmap Korelasi") {
      tagList(
        plotOutput("plot_corr", height = "500px"),
        br(),
        uiOutput("interpretasi_heatmap")
      )
    } else {
      tabPanel("Histogram",
               plotOutput("plot_hist"),
               br(),
               uiOutput("interpretasi_hist")
      )
    }
  })
  
  output$plot_trend <- renderPlot({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    
    req(data_clean(), input$var_y, input$var_id, input$var_time)
    df <- data_clean()
    df$Y <- df[[input$var_y]]; df$ID <- df[[input$var_id]]; df$Time <- df[[input$var_time]]
    ggplot(df, aes(x = Time, y = Y, group = ID)) +
      geom_line(color = "blue", linewidth = 1) + geom_point(size=2) +
      facet_wrap(~ ID, scales = "free_y") + theme_bw() +
      labs(title = paste("Tren", input$var_y))
  })
  
  output$trend_desc <- renderUI({
    req(data_clean(), input$var_y, input$var_id, input$var_time)
    
    df <- data_clean()
    df$Y <- df[[input$var_y]]
    df$ID <- df[[input$var_id]]
    df$Time <- df[[input$var_time]]
    
    library(dplyr)
    
    pola <- df %>%
      group_by(ID) %>%
      arrange(Time) %>%
      summarise(
        kategori = {
          y <- as.numeric(Y)
          if(length(y) < 2) {
            "fluktuasi"
          } else if(all(diff(y) > 0)) {
            "naik"
          } else if(all(diff(y) < 0)) {
            "turun"
          } else {
            "fluktuasi"
          }
        },
        .groups = "drop"
      )
    
    buat_kalimat <- function(wilayah, teks){
      if(length(wilayah) == 0) return(NULL)
      paste0("<b>", paste(wilayah, collapse = ", "),
             "</b> : ", teks)
    }
    
    hasil <- c(
      buat_kalimat(pola$ID[pola$kategori=="naik"],
                   "Kasus Pneumonia mengalami peningkatan setiap tahun."),
      
      buat_kalimat(pola$ID[pola$kategori=="turun"],
                   "Kasus Pneumonia mengalami penurunan setiap tahun."),
      
      buat_kalimat(pola$ID[pola$kategori=="fluktuasi"],
                   "Kasus Pneumonia mengalami fluktuasi selama periode pengamatan.")
    )
    
    hasil <- hasil[!is.null(hasil)]
    
    HTML(paste0(
      "<h4><b>Interpretasi</b></h4>",
      paste(hasil, collapse = "<br/><br/>")
    ))
  })
  
  
  cor_data <- reactive({
    
    req(data_clean(), input$var_y, input$var_x, input$var_time)
    
    df <- data_clean()
    
    cor_long <- df %>%
      group_by(across(all_of(input$var_time))) %>%
      do({
        d <- dplyr::select(., all_of(c(input$var_y, input$var_x)))
        
        if(nrow(d) > 2){
          reshape2::melt(cor(d, use = "complete.obs")) %>%
            dplyr::mutate(
              Tahun = unique(.[[input$var_time]])
            )
        } else {
          data.frame()
        }
      }) %>%
      ungroup()
    
    cor_long
  })
  
  output$plot_corr <- renderPlot({
    
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'.")
    )
    
    df_cor <- cor_data()
    req(nrow(df_cor) > 0)
    
    ggplot(df_cor, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), color = "white") +
      scale_fill_viridis_c(option = "C") +
      facet_wrap(~Tahun) +
      theme_minimal()
  })
  
  interpretasi_korelasi <- function(nilai){
    
    arah <- ifelse(nilai > 0, "positif",
                   ifelse(nilai < 0, "negatif", "tidak ada"))
    
    kekuatan <- dplyr::case_when(
      abs(nilai) < 0.20 ~ "sangat lemah",
      abs(nilai) < 0.40 ~ "lemah",
      abs(nilai) < 0.60 ~ "sedang",
      abs(nilai) < 0.80 ~ "kuat",
      TRUE ~ "sangat kuat"
    )
    
    paste("Hubungan", arah, "dengan kekuatan", kekuatan)
  }
  
  output$interpretasi_heatmap <- renderUI({
    
    df_cor <- cor_data()
    req(nrow(df_cor) > 0)
    
    hasil <- df_cor %>%
      dplyr::filter(Var1 == input$var_y) %>%
      dplyr::group_by(Var2) %>%
      dplyr::arrange(Tahun, .by_group = TRUE) %>%
      dplyr::summarise(
        awal = dplyr::first(value),
        akhir = dplyr::last(value),
        rata2 = mean(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        arah_umum = ifelse(rata2 > 0, "positif",
                           ifelse(rata2 < 0, "negatif", "tidak menunjukkan hubungan")),
        
        tren = dplyr::case_when(
          akhir > awal ~ "cenderung menguat",
          akhir < awal ~ "cenderung melemah",
          TRUE ~ "relatif stabil"
        ),
        
        teks = paste0(
          "<b>", Var2, "</b> menunjukkan hubungan ",
          arah_umum, 
          " yang ", tren,
          " selama periode pengamatan."
        )
      )
    
    HTML(paste(
      "<h4><b>Ringkasan Interpretasi Korelasi</b></h4>",
      paste(hasil$teks, collapse = "<br/><br/>")
    ))
  })
  
  output$plot_hist <- renderPlot({
    
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'.")
    )
    
    req(data_clean(), input$var_y)
    df <- data_clean()
    
    nilai <- df[[input$var_y]]
    
    ggplot(df, aes(x = nilai)) +
      geom_histogram(bins = 20, fill = "#69b3a2", color = "black") +
      geom_vline(aes(xintercept = mean(nilai, na.rm = TRUE)), 
                 linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = median(nilai, na.rm = TRUE)), 
                 linetype = "dotted", linewidth = 1) +
      theme_bw() +
      labs(
        title = paste("Distribusi", input$var_y),
        x = input$var_y,
        y = "Frekuensi"
      )
  })
  
  output$interpretasi_hist <- renderUI({
    
    req(data_clean(), input$var_y)
    df <- data_clean()
    
    nilai <- df[[input$var_y]]
    
    skew <- e1071::skewness(nilai, na.rm = TRUE)
    
    bentuk <- dplyr::case_when(
      abs(skew) < 0.5 ~ "mendekati distribusi normal (simetris)",
      skew >= 0.5 ~ "miring ke kanan (positively skewed)",
      skew <= -0.5 ~ "miring ke kiri (negatively skewed)"
    )
    
    HTML(paste0(
      "<b>Interpretasi Distribusi:</b><br><br>",
      "Nilai skewness sebesar <b>", round(skew, 2), "</b> menunjukkan bahwa distribusi data ",
      input$var_y, " bersifat <b>", bentuk, "</b>."
    ))
  })
  
  
  # --- 5. Uji Asumsi ---
  # A. Container Dinamis (Switch Tampilan)
  output$asumsi_dinamis <- renderUI({
    req(input$asumsi_type)
    
    if(input$asumsi_type == "Uji Pengaruh Waktu (ANOVA)") {
      # --- TAMPILAN ANOVA ---
      tagList(
        h5(strong("Output Statistik:")),
        verbatimTextOutput("anova_output"),
        hr(),
        uiOutput("anova_narasi")
      )
    } else {
      # --- TAMPILAN VIF (DENGAN HIPOTESIS MATEMATIKA) ---
      tagList(
        # 1. Hipotesis VIF
        h5(strong("Hipotesis Pengujian Multikolinearitas:")),
        withMathJax(
          # H0: VIF < 10
          helpText("$$H_0: VIF < 10 \\quad (\\textit{Tidak terdapat multikolinearitas})$$"),
          # H1: VIF >= 10 (\ge adalah kode LaTeX untuk >=)
          helpText("$$H_1: VIF \\ge 10 \\quad (\\textit{Terdapat multikolinearitas})$$")
        ),
        hr(),
        
        # 2. Tabel Output
        h5(strong("Hasil Pengujian:")),
        DTOutput("vif_table_dt"),
        tags$br(),
        helpText("Catatan: Perhatikan kolom Kesimpulan untuk melihat status indikasi multikolinearitas.")
      )
    }
  })
  
  # ------------------------------------------------------
  # LOGIKA ANOVA
  # ------------------------------------------------------
  
  # 1. Output Angka Statistik
  output$anova_output <- renderPrint({
    req(data_clean(), input$var_y, input$var_time)
    df <- data_clean()
    form <- as.formula(paste(input$var_y, "~ factor(", input$var_time, ")"))
    summary(aov(form, data = df))
  })
  
  # 2. Output Hipotesis & Narasi
  output$anova_narasi <- renderUI({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    
    req(data_clean(), input$var_y, input$var_time)
    df <- data_clean()
    
    form <- as.formula(paste(input$var_y, "~ factor(", input$var_time, ")"))
    model_aov <- aov(form, data = df)
    
    p_val <- summary(model_aov)[[1]][["Pr(>F)"]][1]
    
    # Hipotesis
    teks_hipotesis <- tagList(
      h4("Hipotesis Pengujian:"),
      withMathJax(
        helpText(paste0("$$H_0: \\textit{Waktu tidak berpengaruh signifikan terhadap } ", input$var_y, "$$")),
        helpText(paste0("$$H_1: \\textit{Waktu berpengaruh signifikan terhadap } ", input$var_y, "$$"))
      )
    )
    
    if(!is.na(p_val) && p_val < 0.01) {
      # TOLAK H0 (Hijau)
      teks_kesimpulan <- div(style = "color: green; font-weight: bold;",
                             paste("Kesimpulan: Nilai Pr(>F) =", format(p_val, digits=4), "< 0.01."),
                             br(),
                             paste("Keputusan: TOLAK H0. Artinya, Waktu berpengaruh signifikan terhadap", input$var_y, ".")
      )
    } else {
      # GAGAL TOLAK H0 (Merah)
      teks_kesimpulan <- div(style = "color: red; font-weight: bold;",
                             paste("Kesimpulan: Nilai Pr(>F) =", format(p_val, digits=4), "> 0.01."),
                             br(),
                             paste("Keputusan: GAGAL TOLAK H0. Artinya, Waktu tidak berpengaruh signifikan terhadap", input$var_y, ".")
      )
    }
    
    tagList(teks_hipotesis, hr(), h4("Hasil Interpretasi:"), teks_kesimpulan)
  })
  
  # ------------------------------------------------------
  # LOGIKA VIF
  # ------------------------------------------------------
  output$vif_table_dt <- renderDT({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    req(data_clean(), input$var_y, input$var_x)
    
    if(length(input$var_x) < 2) {
      return(datatable(data.frame(Pesan = "Harap pilih minimal 2 Variabel X untuk VIF.")))
    }
    
    df <- data_clean()
    form <- as.formula(paste(input$var_y, "~", paste(input$var_x, collapse=" + ")))
    
    tryCatch({
      model_vif <- lm(form, data = df)
      vif_vals <- car::vif(model_vif)
      
      vif_df <- data.frame(
        Variabel = names(vif_vals),
        VIF = round(as.numeric(vif_vals), 3)
      )
      
      vif_df$Kesimpulan <- ifelse(vif_df$VIF > 10,
                                  "Terdapat Multikolinearitas",
                                  "Tidak Terdapat Multikolinearitas")
      
      
      datatable(vif_df, rownames = FALSE, options = list(dom = 't')) %>%
        
        
        formatStyle(
          'Kesimpulan',
          color = styleEqual(
            c("Terdapat Multikolinearitas", "Tidak Terdapat Multikolinearitas"),
            c("red", "green")
          ),
          fontWeight = 'bold'
        )
      
    }, error = function(e) {
      datatable(data.frame(Error = "Terjadi kesalahan perhitungan."))
    })
  })
  
  # ======================================================
  # PEMODELAN PANEL & PEMILIHAN MODEL
  # ======================================================
  
  # A. DEFINISI MODEL
  # ------------------------------------------------------
  f_str <- reactive({
    # Stop jika Y atau X belum dipilih
    req(input$var_y, input$var_x)
    paste(input$var_y, "~", paste(input$var_x, collapse = " + "))
  })
  
  mod_cem <- reactive({
    req(data_clean(), f_str())
    plm(as.formula(f_str()), data=data_clean(), index=c(input$var_id, input$var_time), model="pooling")
  })
  
  mod_fem <- reactive({
    req(data_clean(), f_str())
    plm(as.formula(f_str()), data=data_clean(), index=c(input$var_id, input$var_time), model="within")
  })
  
  mod_rem <- reactive({
    req(data_clean(), f_str())
    plm(as.formula(f_str()), data=data_clean(), index=c(input$var_id, input$var_time), model="random")
  })
  
  # B. TAMPILAN
  # ------------------------------------------------------
  output$dynamic_model_ui <- renderUI({
    req(input$analysis_mode)
    
    # Logika Tab: Jika "Uji Pemilihan" vs "Model Estimasi"
    if(grepl("Pemilihan", input$analysis_mode)) {
      
      # === TAMPILAN UJI PEMILIHAN ===
      tabsetPanel(
        tabPanel("1. Uji Chow", icon = icon("exchange-alt"), tags$br(), uiOutput("out_chow")),
        tabPanel("2. Uji Hausman", icon = icon("random"), tags$br(), uiOutput("out_hausman")),
        tabPanel("3. Uji LM", icon = icon("check"), tags$br(), uiOutput("out_lm"))
      )
      
    } else {
      
      # === TAMPILAN MODEL ESTIMASI ===
      tabsetPanel(
        # TAB 1: Hasil Estimasi
        tabPanel("Hasil Estimasi Model", icon = icon("list-alt"),
                 tags$br(),
                 h4("Output Ringkasan Statistik (Summary)"),
                 verbatimTextOutput("out_estimasi_raw"),
                 uiOutput("out_extra_effects")
        ),
        
        # TAB 2: Persamaan Model
        tabPanel("Persamaan Model", icon = icon("calculator"),
                 tags$br(),
                 uiOutput("out_persamaan_model")
        ),
        
        # TAB 3: Uji Simultan & Parsial
        tabPanel("Uji Simultan & Parsial", icon = icon("check-double"),
                 tags$br(),
                 uiOutput("out_uji_simultan"),
                 hr(),
                 h4(icon("list-ol"), "Uji Parsial (T-Test)"),
                 withMathJax(
                   helpText("$$H_0: \\beta_j = 0 \\quad (\\textit{Tidak berpengaruh signifikan})$$"),
                   helpText("$$H_1: \\beta_j \\neq 0 \\quad (\\textit{Berpengaruh signifikan})$$")
                 ),
                 uiOutput("out_uji_parsial_dt")
        )
      )
    }
  })
  
  
  # C. OUTPUT TAB 1: ESTIMASI (RAW SUMMARY)
  # ------------------------------------------------------
  output$out_estimasi_raw <- renderPrint({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    
    req(input$analysis_mode)
    
    if(grepl("Common", input$analysis_mode)) {
      summary(mod_cem())
    } else if(grepl("Fixed", input$analysis_mode)) {
      summary(mod_fem())
    } else if(grepl("Random", input$analysis_mode)) {
      summary(mod_rem())
    }
  })
  
  
  
  output$out_extra_effects <- renderUI({
    req(input$analysis_mode)
    
    # Jika modelnya Fixed Effect (FEM), tampilkan nilai intersep per wilayah
    if(grepl("Fixed", input$analysis_mode)) {
      tagList(
        h4("Nilai Fixed Effects (Intersep Individu):"),
        verbatimTextOutput("fem_fixef_val")
      )
    } else {
      NULL
    }
  })
  
  output$fem_fixef_val <- renderPrint({ fixef(mod_fem()) })
  
  
  # D. OUTPUT TAB 2: PERSAMAAN MODEL & INTERPRETASI (MODIFIED)
  # ------------------------------------------------------
  output$out_persamaan_model <- renderUI({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    
    req(input$analysis_mode)
    if(grepl("Pemilihan", input$analysis_mode)) return(NULL)
    
    # 1. Tentukan Model
    if(grepl("Common", input$analysis_mode)) {
      mod <- mod_cem(); model_type <- "CEM"
    } else if(grepl("Fixed", input$analysis_mode)) {
      mod <- mod_fem(); model_type <- "FEM"
    } else {
      mod <- mod_rem(); model_type <- "REM"
    }
    
    # 2. Ambil Koefisien & Format Angka
    # Catatan: Untuk REM, coef(mod) mengambil estimasi Fixed Part (Beta global)
    coefs <- coef(mod)
    var_names <- names(coefs)
    fmt <- function(x) format(round(x, 3), nsmall=3, decimal.mark=",", big.mark=".")
    
    # 3. Interpretasi
    interpretasi_list <- list()
    
    # A. Intersep Global
    if("(Intercept)" %in% var_names) {
      val <- coefs["(Intercept)"]
      interpretasi_list <- c(interpretasi_list, paste0(
        "<li><strong>Intersep:</strong> Koefisien ", fmt(val),
        " menunjukkan bahwa ketika seluruh variabel independen nol, nilai <em>", input$var_y,
        "</em> yang diperkirakan adalah ", fmt(val), ".</li>"))
    }
    
    # B. Variabel Independen (Slope)
    vars_only <- setdiff(var_names, "(Intercept)")
    for(v in vars_only) {
      val <- coefs[v]
      abs_val <- abs(val)
      txt_sign <- ifelse(val >= 0, "meningkatkan", "menurunkan")
      
      txt <- paste0("<li><strong>", v, ":</strong> Koefisien ", fmt(val),
                    " menunjukkan bahwa setiap peningkatan satu satuan <em>", v,
                    "</em> diperkirakan <strong>", txt_sign, "</strong> <em>", input$var_y,
                    "</em> sebesar ", fmt(abs_val), " dengan asumsi variabel lain tetap.</li>")
      
      interpretasi_list <- c(interpretasi_list, txt)
    }
    final_interpretasi_html <- HTML(paste0("<ul>", paste(interpretasi_list, collapse=""), "</ul>"))
    
    
    # 4. TAMPILAN PERSAMAAN
    # =========================================================
    
    if(model_type == "CEM" || model_type == "REM") {
      
      # --- TAMPILAN CEM & REM) ---
      eq_parts <- character()
      if("(Intercept)" %in% var_names) eq_parts <- c(eq_parts, fmt(coefs["(Intercept)"]))
      
      counter <- 0
      for(v in vars_only) {
        val <- coefs[v]; sign <- ifelse(val >= 0, "+", "-")
        term <- paste(sign, fmt(abs(val)), v)
        counter <- counter + 1
        if(counter > 1 && counter %% 3 == 0) term <- paste("\\\\ &", term)
        eq_parts <- c(eq_parts, term)
      }
      
      eq_str <- paste(eq_parts, collapse = " ")
      if(startsWith(trimws(eq_str), "+")) eq_str <- substring(trimws(eq_str), 2)
      
      final_eq <- paste0("$$ \\begin{aligned} ", input$var_y, " &= ", eq_str, " \\end{aligned} $$")
      
      judul_model <- if(model_type == "CEM") "Persamaan Model Common Effect" else "Persamaan Umum Random Effect"
      
      tagList(
        h4(icon("calculator"), paste("1.", judul_model)),
        withMathJax(helpText(final_eq)),
        # Jika REM, tambahkan catatan kecil bahwa ini persamaan umum
        if(model_type == "REM") helpText(em("Catatan: Persamaan di atas menampilkan bagian Fixed Effect dari model REM (mengabaikan error komponen acak/random effects individu).")),
        hr(),
        h4(icon("align-left"), "2. Interpretasi Model"),
        final_interpretasi_html
      )
      
    } else {
      
      # --- TAMPILAN FEM ---
      
      # A. Slope untuk setiap wilayah
      user_vars <- input$var_x
      slope_parts_latex <- character()
      
      legend_elements <- lapply(seq_along(user_vars), function(i) {
        v_real <- user_vars[i]
        tags$li(class="list-group-item",
                style="background: transparent; border: none; padding: 2px 0;",
                withMathJax(paste0("$$ X_{", i, "} = \\text{", v_real, "} $$"))
        )
      })
      
      n_vars <- length(legend_elements)
      mid_point <- ceiling(n_vars / 2)
      col1_items <- legend_elements[1:mid_point]
      col2_items <- if(n_vars > 1) legend_elements[(mid_point + 1):n_vars] else list()
      --
        for(i in seq_along(user_vars)) {
          v_real <- user_vars[i]
          v_alias_latex <- paste0("X_{", i, "}")
          
          if(v_real %in% names(coefs)) {
            val <- coefs[v_real]
            sign <- ifelse(val >= 0, "+", "-")
            slope_parts_latex <- c(slope_parts_latex, paste(sign, fmt(abs(val)), v_alias_latex))
          }
        }
      slope_base_latex <- paste(slope_parts_latex, collapse=" ")
      
      # B. Hitung Intersep per Wilayah (KHUSUS FEM)
      ints <- fixef(mod, type="level")
      regs <- names(ints)
      vals <- as.numeric(ints)
      
      # C. Buat Dataframe
      df_eq <- data.frame(Wilayah = regs, Intersep = vals, stringsAsFactors = FALSE)
      
      df_eq$Persamaan <- sapply(df_eq$Intersep, function(x) {
        paste0("$$ ", input$var_y, " = ", fmt(x), " ", slope_base_latex, " $$")
      })
      
      # D. Output
      tagList(
        h4(icon("table"), "1. Persamaan Model per Wilayah (FEM)"),
        p("Model Fixed Effect menghasilkan intersep yang berbeda untuk setiap wilayah:"),
        
        # 1. Tabel
        datatable(df_eq[, c("Wilayah", "Persamaan")],
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(
                    pageLength = 5,
                    scrollX = TRUE,
                    drawCallback = JS("function(settings) { MathJax.Hub.Queue(['Typeset', MathJax.Hub]); }")
                  )
        ),
        
        # 2. Variabel
        div(style = "margin-top: 20px; padding: 15px; background-color: #f7f7f7; border: 1px solid #e3e3e3; border-radius: 5px;",
            h5(strong(icon("info-circle"), "Keterangan Notasi Variabel:")),
            fluidRow(
              column(6, tags$ul(style = "list-style-type: none; padding-left: 0;", col1_items)),
              column(6, tags$ul(style = "list-style-type: none; padding-left: 0;", col2_items))
            )
        ),
        
        hr(),
        h4(icon("align-left"), "2. Interpretasi Koefisien (Slope)"),
        p("Slope (koefisien regresi) diasumsikan sama untuk semua wilayah:"),
        final_interpretasi_html
      )
    }
  })
  
  
  # ======================================================
  # E. OUTPUT MENU 3: UJI SIMULTAN & PARSIAL
  # ======================================================
  
  # 1. Uji Simultan (F-Test)
  # ------------------------------------------------------
  output$out_uji_simultan <- renderUI({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    
    req(input$analysis_mode)
    if(grepl("Pemilihan", input$analysis_mode)) return(NULL)
    
    if(grepl("Common", input$analysis_mode)) {
      mod <- mod_cem(); summ <- summary(mod)
    } else if(grepl("Fixed", input$analysis_mode)) {
      mod <- mod_fem(); summ <- summary(mod)
    } else {
      mod <- mod_rem(); summ <- summary(mod)
    }
    
    
    f_p <- NA
    if(!is.null(summ$fstatistic)) {
      f_p <- summ$fstatistic$p.value
    } else if(!is.null(summ$wald.statistic)) {
      f_p <- summ$wald.statistic$p.value
    }
    
    r2  <- summ$r.squared["rsq"]
    
    
    alpha <- 0.10
    
    if(!is.na(f_p) && f_p < alpha) {
      tanda <- "<"
      keputusan <- "TOLAK H0"
      warna <- "green"
      
      
      kesimpulan_text <- paste("Terdapat paling sedikit satu variabel independen yang mempengaruhi ", input$var_y)
    } else {
      tanda <- ">"
      keputusan <- "GAGAL TOLAK H0"
      warna <- "red"
      kesimpulan_text <- paste("Variabel independen secara simultan tidak mempengaruhi ", input$var_y)
    }
    
    tagList(
      div(style="background-color: #f7f7f7; padding: 15px; border-radius: 5px; border-left: 5px solid #00c0ef; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);",
          fluidRow(
            column(width = 8,
                   h4(style="margin-top:0;", icon("chart-pie"), strong("Koefisien Determinasi (R-Squared)")),
                   p("Seberapa baik model menjelaskan variasi data.")
            ),
            column(width = 4, class="text-right",
                   h2(style="margin-top:0; color:#00c0ef; font-weight:bold;", paste0(round(r2*100, 1), "%")),
                   span(style="font-size:12px; color:#666;", paste("Nilai:", round(r2, 4)))
            )
          ),
          hr(style="border-top: 1px solid #ddd; margin: 10px 0;"),
          p(style="margin-bottom:0;", icon("check-circle"),
            paste0("Artinya, model ini mampu menjelaskan ", round(r2*100, 2), "% variasi dari variabel dependen."))
      ),
      
      br(),
      
      h4(icon("users"), "Uji Simultan"),
      
      
      # Hipotesis Teks
      h5(strong("Hipotesis:")),
      div(style="margin-bottom: 10px;",
          withMathJax(
            # H0
            helpText(paste0("$$H_0: \\text{Variabel independen secara simultan tidak mempengaruhi ", input$var_y, "}$$")),
            
            # H1
            helpText(paste0("$$H_1: \\text{Terdapat paling sedikit satu variabel independen yang mempengaruhi ", input$var_y, "}$$"))
          )
      ),
      
      # Kesimpulan
      div(style="background-color:#f4f4f4; padding:15px; border-left: 5px solid #3c8dbc; border-radius:5px;",
          h4(strong("Kesimpulan Uji Simultan:")),
          p(paste0("Nilai P-value: ", format(f_p, scientific=TRUE, digits=4))),
          p(strong(paste0("Karena P-value (", format(f_p, digits=4), ") ", tanda, " ", alpha,
                          ", maka Keputusan: ", keputusan))),
          p(style=paste0("color:", warna, "; font-weight:bold; font-size:15px; margin-top:5px;"),
            paste("Artinya:", kesimpulan_text))
      )
    )
  })
  
  
  # 2. Uji Parsial (T-Test) + Kesimpulan
  # ------------------------------------------------------
  output$out_uji_parsial_dt <- renderUI({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    req(input$analysis_mode)
    if(grepl("Pemilihan", input$analysis_mode)) return(NULL)
    
    if(grepl("Common", input$analysis_mode)) mod <- mod_cem()
    else if(grepl("Fixed", input$analysis_mode)) mod <- mod_fem()
    else mod <- mod_rem()
    
    summ <- summary(mod)
    coefs <- coef(summ)
    
    df_t <- data.frame(
      Variabel = rownames(coefs),
      Coef = round(coefs[,1], 4),
      StdError = round(coefs[,2], 4),
      tStat = round(coefs[,3], 4),
      PVal = round(coefs[,4], 4)
    )
    
    df_t$Keterangan <- ifelse(df_t$PVal < 0.10, "Signifikan", "Tidak Signifikan")
    
    # --- LOGIKA KESIMPULAN PARSIAL ---
    # Cari variabel yang P-Val < 0.10 (Kecuali Intercept)
    sig_vars <- df_t$Variabel[df_t$PVal < 0.10]
    sig_vars <- setdiff(sig_vars, "(Intercept)") # Hapus intercept dari daftar
    
    if(length(sig_vars) > 0) {
      # Jika ada variabel yang signifikan
      list_vars <- paste(sig_vars, collapse = ", ")
      kalimat_akhir <- paste("Variabel yang signifikan mempengaruhi", input$var_y, "adalah", list_vars, ".")
      warna_box <- "green"
    } else {
      # Jika tidak ada sama sekali
      kalimat_akhir <- paste("Tidak ada variabel independen yang signifikan mempengaruhi", input$var_y, "pada tingkat alpha 10%.")
      warna_box <- "red"
    }
    
    tagList(
      # 1. Tabel
      datatable(df_t, rownames=FALSE, options=list(dom='t', pageLength=15)) %>%
        formatStyle('Keterangan',
                    color=styleEqual(c("Signifikan","Tidak Signifikan"), c("green","red")),
                    fontWeight='bold'),
      
      br(),
      
      # 2. Teks Kesimpulan di Bawah Tabel
      div(style=paste0("background-color: #f9f9f9; padding: 15px; border-left: 5px solid ", warna_box, "; margin-top: 10px;"),
          strong(icon("info-circle"), "Kesimpulan Uji Parsial:"),
          p(style = "font-size: 14px; margin-top: 5px;", kalimat_akhir)
      )
    )
  })
  
  # ======================================================
  # D. LOGIKA UJI PEMILIHAN MODEL (CHOW, HAUSMAN, LM)
  # ======================================================
  
  alpha <- 0.10
  
  # 1. Uji Chow (CEM vs FEM)
  # ------------------------------------------------------
  output$out_chow <- renderUI({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    chow <- pFtest(mod_fem(), mod_cem())
    pval <- chow$p.value
    
    if(pval < alpha) {
      tanda <- "<"
      keputusan <- "TOLAK H0"
      warna <- "green"
      kesimpulan <- "Model Fixed Effect (FEM) lebih relevan dibandingkan Common Effect (CEM)."
    } else {
      tanda <- ">"
      keputusan <- "GAGAL TOLAK H0"
      warna <- "red"
      kesimpulan <- "Model Common Effect (CEM) lebih relevan dibandingkan Fixed Effect (FEM)."
    }
    
    tagList(
      h4("Hasil Uji Chow:"), verbatimTextOutput("chow_raw"), hr(),
      h4("Interpretasi Hipotesis:"),
      withMathJax(
        helpText("$$H_0: \\text{Model Common Effect (CEM) lebih relevan}$$"),
        helpText("$$H_1: \\text{Model Fixed Effect (FEM) lebih relevan}$$")
      ),
      br(),
      div(style="background-color:#f4f4f4; padding:15px; border-left: 5px solid #3c8dbc; border-radius:5px;",
          h4(strong("Kesimpulan Akhir:")),
          p(paste0("Nilai P-value: ", format(pval, scientific=TRUE, digits=4))),
          p(strong(paste0("Karena P-value (", format(pval, digits=4), ") ", tanda, " ", alpha,
                          ", maka Keputusan: ", keputusan))),
          p(style=paste0("color:", warna, "; font-weight:bold; font-size:15px; margin-top:5px;"),
            paste("Artinya:", kesimpulan))
      )
    )
  })
  output$chow_raw <- renderPrint({ pFtest(mod_fem(), mod_cem()) })
  
  
  # 2. Uji Hausman (FEM vs REM)
  # ------------------------------------------------------
  output$out_hausman <- renderUI({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    haus <- phtest(mod_fem(), mod_rem())
    pval <- haus$p.value
    
    # Logika Keputusan
    if(pval < alpha) {
      tanda <- "<"
      keputusan <- "TOLAK H0"
      warna <- "green"
      kesimpulan <- "Model Fixed Effect (FEM) lebih relevan dibandingkan Random Effect (REM)."
    } else {
      tanda <- ">"
      keputusan <- "GAGAL TOLAK H0"
      warna <- "red" # Merah bukan berarti jelek, tapi memilih REM
      kesimpulan <- "Model Random Effect (REM) lebih relevan dibandingkan Fixed Effect (FEM)."
    }
    
    tagList(
      h4("Hasil Uji Hausman:"), verbatimTextOutput("haus_raw"), hr(),
      h4("Interpretasi Hipotesis:"),
      withMathJax(
        helpText("$$H_0: \\text{Model Random Effect (REM) lebih relevan}$$"),
        helpText("$$H_1: \\text{Model Fixed Effect (FEM) lebih relevan}$$")
      ),
      br(),
      div(style="background-color:#f4f4f4; padding:15px; border-left: 5px solid #3c8dbc; border-radius:5px;",
          h4(strong("Kesimpulan Akhir:")),
          p(paste0("Nilai P-value: ", format(pval, scientific=TRUE, digits=4))),
          p(strong(paste0("Karena P-value (", format(pval, digits=4), ") ", tanda, " ", alpha,
                          ", maka Keputusan: ", keputusan))),
          p(style=paste0("color:", warna, "; font-weight:bold; font-size:15px; margin-top:5px;"),
            paste("Artinya:", kesimpulan))
      )
    )
  })
  output$haus_raw <- renderPrint({ phtest(mod_fem(), mod_rem()) })
  
  
  # 3. Uji Lagrange Multiplier / LM (CEM vs REM)
  # ------------------------------------------------------
  output$out_lm <- renderUI({
    validate(
      need(input$file1, "⚠️ Harap Upload Data Terlebih Dahulu di Menu 'Data & Statistik'."),
    )
    # BP Test
    lm_t <- plmtest(mod_cem(), type="bp")
    pval <- lm_t$p.value
    
    if(pval < alpha) {
      tanda <- "<"
      keputusan <- "TOLAK H0"
      warna <- "green"
      kesimpulan <- "Model Random Effect (REM) lebih relevan dibandingkan Common Effect (CEM)."
    } else {
      tanda <- ">"
      keputusan <- "GAGAL TOLAK H0"
      warna <- "red"
      kesimpulan <- "Model Common Effect (CEM) lebih relevan dibandingkan Random Effect (REM)."
    }
    
    tagList(
      h4("Hasil Uji Lagrange Multiplier (LM):"), verbatimTextOutput("lm_raw"), hr(),
      h4("Interpretasi Hipotesis:"),
      withMathJax(
        helpText("$$H_0: \\text{Model Common Effect (CEM) lebih relevan}$$"),
        helpText("$$H_1: \\text{Model Random Effect (REM) lebih relevan}$$")
      ),
      br(),
      div(style="background-color:#f4f4f4; padding:15px; border-left: 5px solid #3c8dbc; border-radius:5px;",
          h4(strong("Kesimpulan Akhir:")),
          p(paste0("Nilai P-value: ", format(pval, scientific=TRUE, digits=4))),
          p(strong(paste0("Karena P-value (", format(pval, digits=4), ") ", tanda, " ", alpha,
                          ", maka Keputusan: ", keputusan))),
          p(style=paste0("color:", warna, "; font-weight:bold; font-size:15px; margin-top:5px;"),
            paste("Artinya:", kesimpulan))
      )
    )
  })
  output$lm_raw <- renderPrint({ plmtest(mod_cem(), type="bp") })
}


# ======================================================
# 4. RUN APP
# ======================================================
shinyApp(ui, server)



