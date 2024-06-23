library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(RPostgreSQL)
library(DBI)
library(DT)
library(bs4Dash)
library(dplyr)
library(plotly)
library(tidyverse)
library(rvest)
library(readr)
library(bs4Dash)
library(shinyWidgets)

#=============================== DATA ========================================#

# Read the CSV data from the URL
url <- "https://raw.githubusercontent.com/yudheeeeet/mdskel4/main/data/data_online_shop.csv"
data <- read_csv(url)

#=========================== Interface (Front-End) ============================#

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Bungee&display=swap")
  ),
  dashboardPage(
    header = dashboardHeader(
      title = div(
        style = "text-align: center;",
       # img(src = "https://github.com/dataelevate/verifikasi1/raw/main/app/logo4.jpg", height = 170, style = "margin-bottom: -5px;")
      img(src = "https://github.com/windyayupratiwi/project-verifikasi/raw/main/app/logo 12.jpg", height = 190, style = "margin-bottom: 100 px;")
      ),
      textOutput("tanggal")
    ), 
    
    #------------SIDEBAR-----------------#
    sidebar = dashboardSidebar(
      collapsed = FALSE,
      sidebarMenu(
        menuItem(
          text = "Beranda",
          tabName = "home",
          icon = icon("home")
        ),
        menuItem(
          text = "Permintaan",
          tabName = "requests",
          icon = icon("envelope")
        ),
        menuItem(
          text = "Tanda Tangan",
          tabName = "signatures",
          icon = icon("pen")
        ),
        menuItem(
          text = "Dokumen",
          tabName = "Dokumen",
          icon = icon("file"),
          menuSubItem(
            text = "Unggah",
            tabName = "upload"
          ),
          menuSubItem(
            text = "Diunggah",
            tabName = "uploaded"
          ),
          menuSubItem(
            text = "Ditandatangani",
            tabName = "signed"
          )
        ),
        menuItem(
          text = "Pengaturan",
          tabName = "Pengaturan",
          icon = icon("cogs"),
          menuSubItem(
            text = "Sertifikat Digital",
            tabName = "digital_certificates"
          ),
          menuSubItem(
            text = "Gambar Tanda Tangan",
            tabName = "signature_image"
          )
        ), 
        menuItem(
          text = "Verifikasi",
          tabName = "verification",
          icon = icon("check-circle")
        )
      ),
      style = "background-color: #B9D3EE; font-size: 15px; font-weight: bold; padding: 8px; border-radius: 4px;"
    ),
    
    #-----------------BODY-----------------#
    
    body = dashboardBody(
      tabItems(
        #-------------------------Tab beranda ------------------------#
        tabItem(tabName = "home",
                fluidRow(
                  box(title = div(class = "custom-title", "Home Page"), status = "primary", solidHeader = TRUE, width = 12,
                      p("Selamat datang, Rahmi Anadra", style = "font-size: 18px; font-weight: bold;"),
                      p("Silakan pilih kegiatan yang akan Anda lakukan di sistem.", style = "font-size: 16px;")
                  )
                ),
                fluidRow(
                  column(width = 4,
                         box(title = "Unggah dan Tanda Tangan Sendiri", status = "primary", solidHeader = TRUE, width = NULL, height = 220,
                             align = "center",
                             tags$img(src = 'https://img.freepik.com/free-photo/history-icon-front-side-white-background_187299-40163.jpg?t=st=1719129184~exp=1719132784~hmac=f6d51d78df3d177dc5682a014caad164be6b8b1e4d780a491fe11dc8afdf58fe&w=826', height = 120),
                             p("Saya ingin mengunggah dokumen dan saya tanda tangani sendiri", style = "font-size: 18px;")
                         )
                  ),
                  column(width = 4,
                         box(title = "Unggah dan Minta Tanda Tangan Orang Lain", status = "primary", solidHeader = TRUE, width = NULL, height = 220,
                             align = "center",
                             tags$img(src = 'https://img.freepik.com/free-vector/illustration-contract-icon_53876-3305.jpg?t=st=1719129260~exp=1719132860~hmac=f4384c91cbf3c326659da3f5fc11fe7fdda7518a876bc7227d3f4b3b5bb8a3e7&w=826', height = 120),
                             p("Saya ingin mengunggah dokumen dan meminta orang lain menandatangani", style = "font-size: 18px;")
                         )
                  ),
                  column(width = 4,
                         box(title = "Lihat Daftar Dokumen untuk Ditandatangani", status = "primary", solidHeader = TRUE, width = NULL, height = 220,
                             align = "center",
                             tags$img(src = 'https://img.freepik.com/free-vector/check-list-shopping-icon-isolated_18591-82224.jpg?t=st=1719129346~exp=1719132946~hmac=4652b0c09ac42467e1f5d86756462c6cb6509f3b49313f55dde2defc3535215f&w=826', height = 120),
                             p("Saya ingin melihat daftar dokumen yang perlu saya tanda tangani", style = "font-size: 18px;")
                         )
                  )
                )
        ),
        
        #-------------------------Tab Permintaan ------------------------#
        tabItem(tabName = "requests",
                fluidRow(
                  box(title = "Permintaan Penandatanganan", status = "primary", solidHeader = TRUE, width = 12,
                      p("Berikut ini daftar dokumen yang perlu Anda tanda tangani. Anda harus melakukan persetujuan terlebih dahulu untuk dapat mulai menandatangani dokumen.")
                  )
                ),
                fluidRow(
                  box(title = "Peringatan", status = "danger", solidHeader = TRUE, width = 12,
                      p("Anda tidak memiliki sertifikat digital aktif sehingga tidak dapat memberikan tanda tangan. Silakan membuka halaman ", 
                        a("Sertifikat Digital", href = "#shiny-tab-digital_certificates"), " untuk manajemen sertifikat.")
                  )
                ),
                fluidRow(
                  box(title = "Daftar Dokumen", status = "primary", solidHeader = TRUE, width = 12,
                      tableOutput("requestTable")
                  )
                )
        ),
        
        #-------------------------Tab Permintaan  ------------------------#
        tabItem(
          tabName = "product_recommendation",
          h2(" Product Recommendation", align = "center", style = "color: #FF6A6A; font-size: 36px; font-weight: bold; text-shadow: 2px 2px 4px rgba(0,0,0,0.2);"),   # Menambahkan judul tab
          column(12, align = "center",
                 p(HTML("Discover our selection of best-selling products and find your new favorites. &#127775;"))
          ),
          fluidRow(
            # Display tabel 
            box(
              title = "Best-Selling Products by Location",
              background = "white",  # Mengubah warna latar belakang box
              solidHeader = TRUE,
              dataTableOutput("out_tbl8"),
              width = 12,
              status = "primary"
            ),
            box(
              title = "Best-Selling Products by Gender",
              background = "white",  # Mengubah warna latar belakang box
              solidHeader = TRUE,
              dataTableOutput("out_tbl9"),
              width = 12,
              status = "primary"
            ),
            box(
              title = "Best-Selling Products by Category",
              background = "white",  # Mengubah warna latar belakang box
              solidHeader = TRUE,
              dataTableOutput("out_tbl10"),
              width = 12,
              status = "primary"
            )
          )
        ),
        #-------------------------Tab Tanda tangan  ------------------------#
        tabItem(tabName = "signatures",
                fluidRow(
                  box(title = "Tanda Tangan", status = "primary", solidHeader = TRUE, width = 12,
                      "Konten untuk Tanda Tangan.")
                )
        ),
        #-------------------------Tab upload -------------------
        tabItem(tabName = "upload",
                fluidRow(
                  box(title = "Unggah Dokumen", status = "primary", solidHeader = TRUE, width = 12,
                      p("Di halaman ini Anda dapat mengunggah satu atau lebih dokumen untuk ditandatangani oleh satu atau lebih penanda tangan. Orang yang Anda minta untuk menandatangani dokumen akan mendapat notifikasi melalui e-mail. Jika Anda hendak menandatangani suatu dokumen oleh Anda sendiri, gunakan ", 
                        a("halaman ini.", href = "#"), " Ukuran maksimum file: 25 MB.")
                  ),
                  box(title = "Unggah Dokumen", status = "primary", solidHeader = TRUE, width = 12,
                      fileInput("file", "Pilih satu atau lebih berkas ...", multiple = TRUE, accept = c(".pdf", ".docx"))
                  )
                ),
                fluidRow(
                  box(title = "Informasi Dokumen", status = "primary", solidHeader = TRUE, width = 12,
                      textInput("doc_name", "Nama Dokumen", placeholder = "(otomatis berdasarkan nama file)"),
                      checkboxInput("sequential", "Penandatanganan dokumen harus mengikuti urutan", value = FALSE)
                  )
                ),
                fluidRow(
                  box(title = "Daftar Penanda Tangan", status = "primary", solidHeader = TRUE, width = 12,
                      textInput("signer_name", "Cari nama mahasiswa atau pegawai ..."),
                      numericInput("page_number", "No. Halaman", value = 1, min = 1),
                      actionButton("add_signer", "Tambah", icon = icon("plus")),
                      actionButton("remove_signer", "Hapus", icon = icon("minus")),
                      dataTableOutput("signer_table"),
                      actionButton("save", "Simpan", class = "btn btn-success")
                  )
                )
        ),
        
        #-------------------------Tab uploaded ------------------------#
        tabItem(tabName = "uploaded",
                fluidRow(
                  box(title = "Dokumen Diunggah", status = "primary", solidHeader = TRUE, width = 12,
                      p("Seluruh dokumen yang sudah Anda unggah ditampilkan pada halaman ini.")
                  ),
                  box(title = "Daftar Dokumen", status = "primary", solidHeader = TRUE, width = 12,
                      dataTableOutput("uploadedTable")
                  )
                )
        ),
        
        
        
        
        #-------------------------Tab signed ------------------------#
        tabItem(tabName = "signed",
                fluidRow(
                  box(title = "Dokumen Ditandatangani", status = "primary", solidHeader = TRUE, width = 12,
                      p("Seluruh dokumen yang sudah Anda tanda tangani ditampilkan pada halaman ini.")
                  ),
                  box(title = "Daftar Dokumen", status = "primary", solidHeader = TRUE, width = 12,
                      dataTableOutput("signedTable")
                  )
                )
        ),
        
        #-------------------------Tab sertifikat digital ------------------------#
        tabItem(tabName = "digital_certificates",
                fluidRow(
                  box(title = "Sertifikat Digital", status = "primary", solidHeader = TRUE, width = 12,
                      p("Sertifikat digital merupakan identitas Anda di sistem untuk membuat tanda tangan digital. Anda hanya dapat memiliki satu sertifikat yang aktif dalam satu waktu. Jika Anda membuat sertifikat baru, sertifikat lama akan dinonaktifkan (revoked) secara otomatis. Anda juga dapat menonaktifkan suatu sertifikat secara manual.")
                  ),
                  box(title = "Daftar Sertifikat", status = "primary", solidHeader = TRUE, width = 12,
                      actionButton("create_cert", "Buat Sertifikat", class = "btn btn-success"),
                      dataTableOutput("certificateTable")
                  )
                )
        ),
        #-------------------------Tab sertifikat digital ------------------------#
        tabItem(tabName = "signature_image",
                fluidRow(
                  box(title = "Gambar Tanda Tangan", status = "primary", solidHeader = TRUE, width = 12,
                      p("Anda dapat meng-upload gambar hasil scan tanda tangan Anda (JPG atau PNG) pada halaman ini untuk ditempelkan di dokumen. Pastikan gambar tanda tangan cukup jelas, memiliki kontras yang baik, dan berlatar belakang putih bersih atau transparan. Anda juga dapat menggambar tanda tangan menggunakan mouse, pen tablet, atau alat penunjuk lainnya.")
                  )
                ),
                fluidRow(
                  box(title = "Tanda Tangan Saya", status = "primary", solidHeader = TRUE, width = 12,
                      column(6, 
                             p("Tanda tangan"),
                             div(style = "border: 1px solid #ddd; padding: 10px;",
                                 "Anda belum memiliki gambar tanda tangan. Silakan upload atau gambar pada form di bawah."
                             )
                      ),
                      column(6, 
                             p("Paraf"),
                             div(style = "border: 1px solid #ddd; padding: 10px;",
                                 "Anda belum memiliki gambar paraf. Silakan upload atau gambar pada form di bawah."
                             )
                      )
                  )
                ),
                fluidRow(
                  box(title = "Upload Tanda Tangan dan Paraf", status = "primary", solidHeader = TRUE, width = 12,
                      column(6, 
                             fileInput("upload_signature", "Tanda Tangan", accept = c(".jpg", ".jpeg", ".png"))
                      ),
                      column(6, 
                             fileInput("upload_initial", "Paraf", accept = c(".jpg", ".jpeg", ".png"))
                      ),
                      actionButton("upload_button", "Upload", class = "btn btn-success")
                  )
                ),
                fluidRow(
                  box(title = "Coret Tanda Tangan dan Paraf", status = "primary", solidHeader = TRUE, width = 12,
                      column(6,
                             div(style = "border: 1px solid #ddd; height: 200px;",
                                 p("Tanda Tangan"),
                                 tags$canvas(id = "signature_canvas", width = "100%", height = "150px"),
                                 actionButton("clear_signature", "Hapus", class = "btn btn-danger")
                             )
                      ),
                      column(6,
                             div(style = "border: 1px solid #ddd; height: 200px;",
                                 p("Paraf"),
                                 tags$canvas(id = "initial_canvas", width = "100%", height = "150px"),
                                 actionButton("clear_initial", "Hapus", class = "btn btn-danger")
                             )
                      ),
                      actionButton("save_button", "Simpan", class = "btn btn-success")
                  )
                )
        ),
        
        
        #--------------------------Tab verifikasi --------------------------#
        tabItem(
          tabName = "verification",
          fluidRow(
            box(title = NULL, status = "primary", solidHeader = TRUE, width = 12,
                align = "center",
                tags$div(
                  style = "padding: 50px; background-image: url('https://raw.githubusercontent.com/dataelevate/verifikasi1/main/app/background.jpg'); background-size: cover; background-position: center; color: white;",
                  h1("Verifikasi tanda tangan digital"),
                  fileInput("verify_file", "Pilih file PDF untuk diverifikasi", accept = c(".pdf")),
                  actionButton("verify_button", "Verifikasi", class = "btn btn-primary")
                )
            )
          ),
          fluidRow(
            box(title = NULL, status = "primary", solidHeader = TRUE, width = 12,
                fluidRow(
                  column(4, align = "center",
                         tags$div(
                           style = "padding: 20px;",
                           icon("user-check", "fa-5x"),
                           h4("Verifikasi Offline"),
                           p("Klik di sini untuk melihat petunjuk verifikasi dokumen di komputer Anda")
                         )
                  ),
                  column(4, align = "center",
                         tags$div(
                           style = "padding: 20px;",
                           icon("qrcode", "fa-5x"),
                           h4("Scan QR Code"),
                           p("Klik di sini untuk scan QR code yang ada pada tanda tangan")
                         )
                  ),
                  column(4, align = "center",
                         tags$div(
                           style = "padding: 20px;",
                           icon("file-signature", "fa-5x"),
                           h4("Tanda Tangani Dokumen"),
                           p("Klik di sini untuk membuat tanda tangan digital pada dokumen")
                         )
                  )
                )
            )
          )
        )
      )
    ),
    
    #-----------------FOOTER-----------------#
    
    footer = dashboardFooter(
      right = "© 2024 Data Elevate Solution | All Rights Reserved",
      left = "Made with ❤️ by Data Elevate Solution"
      
    )
  )
)
