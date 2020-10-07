library(shiny)
library(devtools)
#install_github("TimoSchuer/ExmaraldaR", force = TRUE)
library(ExmaraldaR)
ui <- fluidPage(
    titlePanel("ExmaraldaR"),
    sidebarPanel(h2("Select input and define parameters"),
                 fileInput("file", h3("Upload .exb file")),
                 checkboxInput("readAnn", "Read annotations", value= TRUE),
                 checkboxGroupInput("annotation", "annotation format", choices = list(
                     "linear" = "linear",
                     "multilayer" = "multilayer"),
                     selected = 1),
                checkboxInput("addMetaData", "Add Metadata"),
                downloadButton("download", "Download")),
    mainPanel(h1("Imported data"),
              dataTableOutput("ExbData"),
              verbatimTextOutput("test"))
)

server <- function(input, output){
    dataInput <- reactive({read_exb_file(input$file$datapath,readAnn=input$readAnn, annotation= input$annotation,addMetaData= input$addMetaData)})
    output$ExbData <-  renderDataTable(dataInput())
    output$download <- downloadHandler(filename = function(){paste(sub('\\.exb$', '', input$file$name),".csv", sep="")}, content = function(file){write.csv(dataInput(), file, row.names = FALSE)} )
    output$test <- renderPrint(str(input$file))
    }

shinyApp(ui, server)
