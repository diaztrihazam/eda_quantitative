library(shiny)
library(shinythemes)

stunting <- c(31.2, 21.1, 25.2, 17, 18, 18.6, 19.8, 15.2, 18.5, 15.4, 14.8, 20.2, 20.8, 16.4, 19.2, 20, 8, 32.7,
              35.3, 27.8, 26.9, 24.6, 23.9, 22.1, 20.5, 28.2, 27.2, 27.7, 23.8, 35, 26.1, 26.1, 30, 34.6)
IPM <- c(72.80, 72.71, 73.26 ,73.52, 72.14, 70.90, 72.16, 70.45, 72.24, 76.46, 81.65, 73.12, 72.79, 80.64, 72.75,
         73.32, 76.44, 69.46, 65.90, 68.63, 71.63, 71.84, 77.44, 71.83, 73.81, 70.28, 72.82, 72.23, 69.81, 66.92,
         70.22, 69.47, 65.89, 61.39)


satu_page <- tabPanel(
  title = "Satu Variabel",
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Pilih Dataset", choices = c("Stunting", "IPM")),
      fileInput("file", "Masukkan File CSV", multiple = T, accept = ".csv"),
      textAreaInput("num1", "Observasi")
    ),
    mainPanel(
      fluidRow(
        column(width = 1, tableOutput("num1_summary_table"))
      ),
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

dua_page <- tabPanel(
  title = "Dua Variabel"
)

ui <- navbarPage(
  title = "Eksplorasi Data Kuantitatif",
  theme = shinytheme('simplex'),
  satu_page,
  dua_page
)

server <- function(input, output, session) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Stunting" = stunting,
           "IPM" = IPM)
  })
  
  output$num1_summary_table <- renderTable({
    dataset <- datasetInput()
    ukuran <- length(dataset)
    rata2 <- mean(dataset)
    std <- sd(dataset)
    min <- min(dataset)
    q1 <- quantile(dataset, probs = 0.25)
    med <- quantile(dataset, probs = 0.5)
    q3 <- quantile(dataset, probs = 0.75)
    max <- max(dataset)
    data.frame("Ukuran" = ukuran, "Rata2" = rata2, "Dev Std" = std, "Terkecil" = min,
               "Q1" = q1, "Nilai.Tengah" = med, "Q3" = q3, "Terbesar" = max)
  })
  
  output$plot1 <- renderPlot({
    dataset <- datasetInput()
    hist(dataset, col = 'gray', main = paste("Histogram", input$dataset), xlab = paste(input$dataset))
  })
  
  output$plot2 <- renderPlot({
    dataset <- datasetInput()
    boxplot(dataset, main = paste("Boxplot", input$dataset), horizontal = TRUE)
  })
}

shinyApp(ui = ui, server = server)