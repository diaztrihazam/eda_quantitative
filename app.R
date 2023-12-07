library(shiny)
library(shinythemes)
library(shinyjs)
library(colourpicker)
library(plotly)

# data 
stunting <- c(31.2, 21.1, 25.2, 17, 18, 18.6, 19.8, 15.2, 18.5, 15.4, 14.8, 20.2, 20.8, 16.4, 19.2, 20, 8, 32.7,
              35.3, 27.8, 26.9, 24.6, 23.9, 22.1, 20.5, 28.2, 27.2, 27.7, 23.8, 35, 26.1, 26.1, 30, 34.6)
kerja <- c(62, 78, 70, 58, 65, 54, 69, 71, 67, 74, 64, 45, 59, 68, 70, 66, 80, 54, 62, 83, 77, 51, 72, 79, 66, 83,
           63, 67, 61, 71, 64, 59, 76, 67, 59, 64, 70, 73, 67, 56, 42, 56, 91, 48, 81, 92, 46, 82, 52, 92)
lk <- c(159, 160, 161, 175, 180, 160, 163, 175, 179, 170)
pr <- c(149, 160, 165, 165, 155, 155, 155, 159, 164, 162)
TB <- data.frame("Laki2" = lk , "Perempuan" = pr)

not_sel <- "-"

# tab panel tentang
about_page <- tabPanel(
  title = "Tentang",
  titlePanel("Tentang"),
  "Eksplorasi Data Kuantitatif",
  br(),
  "Fradha Intan Arassah G1501221018",
  br(),
  "Diaztri Hazam G1501221032",
  br(),
  "Firda Aulia Maghfiroh G1501222049"
)

# tab panel satu variabel
satu_page <- tabPanel(
  title = "Satu Variabel",
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", strong("Sumber Data:"),
                   choices = c("Unggah Dokumen", "Data Tersedia")),
      conditionalPanel(
        condition = "input.data_source == 'Unggah Dokumen'",
        fileInput("file", strong("Masukkan Dokumen CSV"), multiple = T, accept = ".csv"),
        selectInput("variable", strong("Pilih Variabel"), choices = c(not_sel)),
        
        # garis horizontal
        tags$hr(),
        radioButtons("pl1", strong("Pilih Tipe Plot"), inline = TRUE,
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      ),
      conditionalPanel(
        condition = "input.data_source == 'Unggah Dokumen' & input.pl1 == 'Histogram'",
        colourInput("col", strong("Pilih Warna"), "grey", showColour = "text")
      ),
      conditionalPanel(
        condition = "input.data_source == 'Data Tersedia'",
        selectInput("dataset", strong("Pilih Data"), choices = c("Prevalensi Stunting (%)", "Lama Waktu Bekerja (jam)")),
        
        # garis horizontal
        tags$hr(),
        
        radioButtons("pl2", strong("Pilih Tipe Plot"), inline = TRUE,
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      ),
      conditionalPanel(
        condition = "input.data_source == 'Data Tersedia' & input.pl2 == 'Histogram'",
        colourInput("col2", strong("Pilih Warna"), "grey", showColour = "text")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Statistik Deskriptif",
          br(),
          tableOutput("num1_summary_table")
        ),
        tabPanel(
          title = "Plot",
          br(),
          plotlyOutput("plot"),
          plotOutput("dotplot")
        )
      )
    )
  )
)

# tab panel dua variabel
dua_page <- tabPanel(
  title = "Dua Variabel",
  sidebarLayout(
    sidebarPanel(
      radioButtons("data2_source", strong("Sumber Data:"),
                   choices = c("Unggah Dokumen", "Data Tersedia")),
      conditionalPanel(
        condition = "input.data2_source == 'Unggah Dokumen'",
        fileInput("file2", strong("Masukkan Dokumen CSV"), multiple = T, accept = ".csv"),
        selectInput("var1_fl", strong("Pilih Variabel 1"), choices = c(not_sel)),
        selectInput("var2_fl", strong("Pilih Variabel 2"), choices = c(not_sel)),
        
        # garis horizontal
        tags$hr(),
        
        radioButtons("pl3", strong("Pilih Tipe Plot"), inline = TRUE,
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot")),
        fluidRow(
          column(width = 6, colourInput("col3", strong("Pilih Warna 1"), "grey", showColour = "text")),
          column(width = 6, colourInput("col4", strong("Pilih Warna 2"), "grey", showColour = "text"))
        )
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Data Tersedia'",
        selectInput("dataset2", strong("Pilih Data"), choices = c("Tinggi Badan (cm)")),
        selectInput("var1_dt", strong("Pilih Variabel 1"), choices = c(not_sel)),
        selectInput("var2_dt", strong("Pilih Variabel 2"), choices = c(not_sel)),
        
        # garis horizontal
        tags$hr(),
        radioButtons("pl4", strong("Pilih Tipe Plot"), inline = TRUE,
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot")),
        fluidRow(
          column(width = 6, colourInput("col5", strong("Pilih Warna 1"), "grey", showColour = "text")),
          column(width = 6, colourInput("col6", strong("Pilih Warna 2"), "grey", showColour = "text"))
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Statistik Deskriptif",
          br(),
          tableOutput("num2_summary_table")
        ),
        tabPanel(
          title = "Plot",
          br(),
          plotlyOutput("plot2"),
          plotOutput("dotplot2")
        )
      )
    )
  )
)

# ui
ui <- navbarPage(
  title = strong("Eksplorasi Data Kuantitatif"),
  theme = shinytheme('simplex'),
  useShinyjs(),
  satu_page,
  dua_page,
  about_page
)

# server
server <- function(input, output, session) {
  
  # fungsi reactive untuk sumber data
  dtSource <- reactive({input$data_source})
  dtSource2 <- reactive({input$data2_source})
    
  # unggah dokumen
  ## fungsi reactive untuk pembacaan dokumen
  fileInput <- reactive({
    req(input$file)
    lapply(input$file$datapath, read.csv)
  })
  file2Input <- reactive({
    req(input$file2)
    lapply(input$file2$datapath, read.csv)
  })
  
  ## fungsi observe untuk nama variabel
  observe({updateSelectInput(session, "variable", choices = colnames(fileInput()[[1]]))})
  observe({updateSelectInput(session, "var1_fl", choices = colnames(file2Input()[[1]]))})
  observe({updateSelectInput(session, "var2_fl", choices = colnames(file2Input()[[1]]))})
  
  ## fungsi reactive variabel
  varInput <- reactive({input$variable})
  var1flInput <- reactive({input$var1_fl})
  var2flInput <- reactive({input$var2_fl})
  
  
  # dataset tersedia
  ## fungsi reactive untuk pemilihan dataset tersedia
  datasetInput <- reactive({
    switch(input$dataset,
           "Prevalensi Stunting (%)" = stunting,
           "Lama Waktu Bekerja (jam)" = kerja)
  })
  dataset2Input <- reactive({
    switch(input$dataset2,
           "Tinggi Badan (cm)" = TB)
  })
  
  ## fungsi observe nama variabel
  observe({updateSelectInput(session, "var1_dt", choices = colnames(dataset2Input()))})
  observe({updateSelectInput(session, "var2_dt", choices = colnames(dataset2Input()))})
  
  ## fungsi reactive variabel
  x1 <- reactive({input$var1_dt})
  x2 <- reactive({input$var2_dt})
  
  
  # fungsi reactive untuk memilih tipe plot
  plot1Input <- reactive({input$pl1})
  plot2Input <- reactive({input$pl2})
  plot3Input <- reactive({input$pl3})
  plot4Input <- reactive({input$pl4})
  
  # fungsi observe untuk tampilan plot
  observeEvent(input$pl1,{
    if(input$pl1 == "Histogram"){
      show("plot")
      hide("dotplot")
    } else if (input$pl1 == "Boxplot"){
      show("plot")
      hide("dotplot")
    } else if(input$pl1 == "Dotplot"){
      show("dotplot")
      hide("plot")
    }
  })
  
  observeEvent(input$pl2,{
    if(input$pl2 == "Histogram"){
      show("plot")
      hide("dotplot")
    } else if (input$pl2 == "Boxplot"){
      show("plot")
      hide("dotplot")
    } else if(input$pl2 == "Dotplot"){
      show("dotplot")
      hide("plot")
    }
  })
  
  observeEvent(input$pl3,{
    if(input$pl3 == "Histogram"){
      show("plot2")
      hide("dotplot2")
    } else if (input$pl3 == "Boxplot"){
      show("plot2")
      hide("dotplot2")
    } else if(input$pl3 == "Dotplot"){
      show("dotplot2")
      hide("plot2")
    }
  })
  
  observeEvent(input$pl4,{
    if(input$pl4 == "Histogram"){
      show("plot2")
      hide("dotplot2")
    } else if (input$pl4 == "Boxplot"){
      show("plot2")
      hide("dotplot2")
    } else if(input$pl4 == "Dotplot"){
      show("dotplot2")
      hide("plot2")
    }
  })
  
  # output untuk tabel statistik deskriptif satu variabel
  output$num1_summary_table <- renderTable({
    
    ## untuk pilihan unggah dokumen
    if(dtSource() == "Unggah Dokumen"){
      df <- data.frame(fileInput())
      var <- df[, varInput()]
      ukuran <- length(var)
      rata2 <- mean(var)
      std <- sd(var)
      min <- min(var)
      q1 <- quantile(var, probs = 0.25)
      med <- quantile(var, probs = 0.5)
      q3 <- quantile(var, probs = 0.75)
      max <- max(var)
      data.frame("Ukuran" = ukuran, "Rata2" = rata2, "Dev.Std" = std, "Terkecil" = min,
                 "Q1" = q1, "Nilai.Tengah" = med, "Q3" = q3, "Terbesar" = max)
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      ukuran <- length(dataset)
      rata2 <- mean(dataset)
      std <- sd(dataset)
      min <- min(dataset)
      q1 <- quantile(dataset, probs = 0.25)
      med <- quantile(dataset, probs = 0.5)
      q3 <- quantile(dataset, probs = 0.75)
      max <- max(dataset)
      data.frame("Ukuran" = ukuran, "Rata2" = rata2, "Dev.Std" = std, "Terkecil" = min,
                   "Q1" = q1, "Nilai.Tengah" = med, "Q3" = q3, "Terbesar" = max)
    }
  })
  
  # output plot satu variabel
  output$plot <- renderPlotly({
    
    ## untuk pilihan unggah dokumen
    if(dtSource() == "Unggah Dokumen"){
      df <- data.frame(fileInput())
      var <- df[, varInput()]
      
      ## histogram
      if(plot1Input() == "Histogram")
         plot_ly(data = df, x = var, type = "histogram",
                 marker = list(color = input$col,
                               line = list(color = "black", width = 0.5))) %>%
        layout(title = paste("Histogram", input$variable),
               xaxis = list(title = paste(input$variable), zeroline = FALSE),
               yaxis = list(title = "Frekuensi", zeroline = FALSE))
      
      ## boxplot
      else if(plot1Input() == "Boxplot")
         plot_ly(data = df, x = var, type = "box", fillcolor = input$col, name = " ",
                 line = list(color = "black")) %>%
         layout(title = paste("Boxplot", input$variable))
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      
      ## histogram
      if(plot2Input() == "Histogram")
         plot_ly(x = ~dataset, type = "histogram",
                marker = list(color = input$col2,
                              line = list(color = "black", width = 0.5))) %>%
        layout(title = paste("Histogram", input$dataset),
               xaxis = list(title = paste(input$dataset), zeroline = FALSE),
               yaxis = list(title = "Frekuensi", zeroline = FALSE))
      ## boxplot
      else if(plot2Input() == "Boxplot")
        plot_ly(x = ~dataset, type = "box", fillcolor = input$col2, name = " ",
                line = list(color = "black")) %>%
        layout(title = paste("Boxplot", input$dataset),
               xaxis = list(title = " "))
    }
  })
  
  # output khusus dotplot satu variabel
  output$dotplot <- renderPlot({
    
    ## untuk pilihan unggah dokumen 
    if(dtSource() == "Unggah Dokumen"){
      df <- data.frame(fileInput())
      var <- df[, varInput()]
      if(plot1Input() == "Dotplot")
         stripchart(var, method = "stack", pch = 21, bg = input$col, col = "black", cex = 5, ylim = c(0,10),
                   main = paste("Dotplot", input$variable), xlab = paste(input$variable))
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      if(plot2Input() == "Dotplot")
      stripchart(dataset, method = "stack", pch = 21, bg = input$col2, col = "black", cex = 5, ylim = c(0,5),
                 main = paste("Dotplot", input$dataset), xlab = paste(input$dataset))
    }
  })
  
  # output untuk tabel statistik deskriptif dua variabel
  output$num2_summary_table <- renderTable({
    
    ## untuk pilihan unggah dokumen
    if(dtSource2() == "Unggah Dokumen"){
      df <- data.frame(file2Input())
      var_1 <- df[, var1flInput()]
      ukuran_1 <- length(var_1)
      rata2_1 <- mean(var_1)
      std_1 <- sd(var_1)
      min_1 <- min(var_1)
      q1_1 <- quantile(var_1, probs = 0.25)
      med_1 <- quantile(var_1, probs = 0.5)
      q3_1 <- quantile(var_1, probs = 0.75)
      max_1 <- max(var_1)
      var_2 <- df[, var2flInput()]
      ukuran_2 <- length(var_2)
      rata2_2 <- mean(var_2)
      std_2 <- sd(var_2)
      min_2 <- min(var_2)
      q1_2 <- quantile(var_2, probs = 0.25)
      med_2 <- quantile(var_2, probs = 0.5)
      q3_2 <- quantile(var_2, probs = 0.75)
      max_2 <- max(var_2)
      data.frame("Ukuran" = c(ukuran_1, ukuran_2), "Rata2" = c(rata2_1, rata2_2), "Dev.Std" = c(std_1, std_2),
                 "Terkecil" = c(min_1, min_2), "Q1" = c(q1_1, q1_2), "Nilai.Tengah" = c(med_1, med_2),
                 "Q3" = c(q3_1, q3_2), "Terbesar" = c(max_1, max_2))
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource2() == "Data Tersedia"){
      df2 <- req(dataset2Input())
      xx1 <- req(x1())
      var_1 <- df2[,xx1]
      ukuran_1 <- length(var_1)
      rata2_1 <- mean(var_1)
      std_1 <- sd(var_1)
      min_1 <- min(var_1)
      q1_1 <- quantile(var_1, probs = 0.25)
      med_1 <- quantile(var_1, probs = 0.5)
      q3_1 <- quantile(var_1, probs = 0.75)
      max_1 <- max(var_1)
      xx2 <- req(x2())
      var_2 <- df2[,xx2]
      ukuran_2 <- length(var_2)
      rata2_2 <- mean(var_2)
      std_2 <- sd(var_2)
      min_2 <- min(var_2)
      q1_2 <- quantile(var_2, probs = 0.25)
      med_2 <- quantile(var_2, probs = 0.5)
      q3_2 <- quantile(var_2, probs = 0.75)
      max_2 <- max(var_2)
      data.frame("Ukuran" = c(ukuran_1, ukuran_2), "Rata2" = c(rata2_1, rata2_2), "Dev.Std" = c(std_1, std_2),
                 "Terkecil" = c(min_1, min_2), "Q1" = c(q1_1, q1_2), "Nilai.Tengah" = c(med_1, med_2),
                 "Q3" = c(q3_1, q3_2), "Terbesar" = c(max_1, max_2))
    }
  })
  
  # output plot dua variabel
  output$plot2 <- renderPlotly({
    
    ## untuk pilihan unggah dokumen
    if(dtSource2() == "Unggah Dokumen"){
      df <- data.frame(file2Input())
      df <- stack(df)
      
      ### histogram
      if(plot3Input() == "Histogram"){
        plot_ly()
      }
      else if(plot3Input() == "Boxplot"){
        par(mfrow = c(2, 1))
        boxplot(var1, col = input$col3, main = paste("Boxplot", input$var1_fl), horizontal = TRUE)
        boxplot(var2, col = input$col4, main = paste("Boxplot", input$var2_fl), horizontal = TRUE)
      }
        
    } else if(dtSource2() == "Data Tersedia"){
      df2 <- req(dataset2Input())
      xx1 <- req(x1())
      var_1 <- df2[,xx1]
      xx2 <- req(x2())
      var_2 <- df2[,xx2]
      if(plot4Input() == "Histogram"){
        par(mfrow = c(2, 1))
        hist(var_1, col = input$col5, main = paste("Histogram", input$var1_dt), xlab = paste(input$var1_dt))
        hist(var_2, col = input$col6, main = paste("Histogram", input$var2_dt), xlab = paste(input$var2_dt))
      } else if(plot4Input() == "Boxplot"){
        par(mfrow = c(2, 1))
        boxplot(var_1, col = input$col5, main = paste("Boxplot", input$var1_dt), horizontal = TRUE)
        boxplot(var_2, col = input$col6, main = paste("Boxplot", input$var2_dt), horizontal = TRUE)
      }
    }
  })
  
  # output khusus dotplot dua variabel
  output$dotplot2 <- renderPlot({
    
    ## untuk pilihan unggah dokumen
    if(dtSource2() == "Unggah Dokumen"){
      df <- data.frame(file2Input())
      var1 <- df[, var1flInput()]
      var2 <- df[, var2flInput()]
      if(plot3Input() == "Dotplot"){
        par(mfrow = c(2, 1))
        stripchart(var1, method = "stack", pch = 21, bg = input$col3, col = "black", cex = 3, ylim = c(0,10),
                   main = paste("Dotplot", input$var1_fl), xlab = paste(input$var1_fl))
        stripchart(var2, method = "stack", pch = 21, bg = input$col4, col = "black", cex = 3, ylim = c(0,10),
                   main = paste("Dotplot", input$var2_fl), xlab = paste(input$var2_fl))
      }
    ## untuk pilihan data tersedia
    } else if(plot4Input() == "Dotplot"){
      par(mfrow = c(2, 1))
      stripchart(var_1, method = "stack", pch = 21, bg = input$col5, col = "black", cex = 3, ylim = c(0,10),
                 main = paste("Dotplot", input$var1_dt), xlab = paste(input$var1_dt))
      stripchart(var_2, method = "stack", pch = 21, bg = input$col6, col = "black", cex = 3, ylim = c(0,10),
                 main = paste("Dotplot", input$var2_dt), xlab = paste(input$var2_dt))
    }
  })
}

shinyApp(ui = ui, server = server)