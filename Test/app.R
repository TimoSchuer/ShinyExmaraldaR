
library(shiny)
library(devtools)
#devtools::install_github("TimoSchuer/ExmaraldaR", upgrade = "never", force = FALSE)
library(ExmaraldaR)
library(shinydashboard)
library(DT)
#library(curl)
library(XML)
library(stringr)
#library(Matrix)
#library(igraph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
#library(cluster)

ui <- dashboardPage(
    dashboardHeader(title =  "ShinyExmaraldaR"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Daten importieren und Analyse", tabName = "daten")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "daten",
                    fluidRow(
                        box(fileInput("file", "Upload .exb file"),
                            checkboxInput("readAnn", "Read annotations", value= TRUE)),
                        box(checkboxGroupInput("annotation", "annotation format", choices = list(
                            "linear" = "linear",
                            "multilayer" = "multilayer"),
                            selected = "linear"),
                            checkboxInput("addMetaData", "Add Metadata"),
                            checkboxInput("addDescription", "Include description tiers"),
                            actionButton("build", "Daten einlesen"),
                            downloadButton("download", "Download"))
                    ),
                    fluidRow(dataTableOutput("ExbData")),
                    fluidRow(selectInput("vars", "Variablen auswählen für Balken", choices = NULL, multiple = FALSE)),
                    fluidRow(selectInput("varsFill", "Variablen auswählen für Füllung", choices = NULL, multiple = FALSE)),
                    fluidRow(
                        checkboxInput("ExcludeNA", "NA rausfiltern", value = TRUE),
                        checkboxInput("perc", "Prozentuale Angaben", value = TRUE),
                        actionButton("plotData", "Auswahl plotten")),
                    fluidRow(dataTableOutput("dataPlot")),
                    fluidRow(plotOutput("BarPlot"),
                             plotOutput("BarPlotStacked"))


            )
        )
    )
)






server <- function(input, output, session){
    dataInput <- eventReactive(input$build,{
        varN <- names(ExmaraldaR::read_exb_file(path = input$file$datapath, readAnn=input$readAnn, annotation= input$annotation,addMetaData= input$addMetaData) %>% as.data.frame())
        updateSelectInput(session, "vars","Variablen auswählen für Balken", choices = varN)
        updateSelectInput(session, "varsFill","Variablen auswählen für Füllung", choices = varN)
        ExmaraldaR::read_exb_file(path = input$file$datapath, readAnn=input$readAnn, annotation= input$annotation,addMetaData= input$addMetaData) %>% as.data.frame()
    })
    output$ExbData <-  renderDataTable({datatable(dataInput(), filter= "top",selection = list(target = 'row', selected= c(seq(1:nrow(dataInput())))),options = list(lengthChange = TRUE, autoWidth= TRUE, scrollX= TRUE))})
    output$download <- downloadHandler(filename = function(){paste(sub('\\.exb$', '', input$file$name),".csv", sep="")}, content = function(file){write.csv(dataInput(), file, row.names = FALSE)})
    DataPlot <- eventReactive(input$plotData,{
        data <- dataInput() %>% group_by_(input$vars, input$varsFill) %>% count() %>% as.data.frame()
        sum <- sum(data[,3])
        data <- data %>% mutate(perc = n/ sum)
        if(input$ExcludeNA ==TRUE){
            data <- data[which(!is.na(data[,1])),]
            sum <- sum(data[,3])
            data <- data %>% mutate(perc = n/ sum)
        }else{
            sum <- sum(data[,3])
            data <- data %>% mutate(perc = n/ sum)
        }

    })
    output$dataPlot <- renderDataTable(DataPlot())
    output$BarPlot <- renderPlot({
        if(input$perc== TRUE){
            ggplot(DataPlot(), aes(x= DataPlot()[,1], y= DataPlot()[,4], fill= DataPlot()[,2])) + geom_bar(stat="identity", position="dodge")+ xlab(input$vars)+ ylab(input$varsFill)

        }else{

            #data2 <- dataInput() %>% group_by_(input$vars, input$varsFill) %>% count() %>% as.data.frame()
            ggplot(DataPlot(), aes(x= DataPlot()[,1], y= DataPlot()[,3], fill= DataPlot()[,2])) + geom_bar(stat="identity", position="dodge")+ xlab(input$vars)+ ylab(input$varsFill)
        }
        })
    output$BarPlotStacked <- renderPlot({
        if(input$perc== TRUE){
            ggplot(DataPlot(), aes(x= DataPlot()[,1], y= DataPlot()[,4], fill= DataPlot()[,2])) + geom_bar(stat="identity", position="dodge")+ xlab(input$vars)+ ylab(input$varsFill)

        }else{
        ggplot(DataPlot(), aes(x= DataPlot()[,1], y= DataPlot()[,3], fill= DataPlot()[,2])) + geom_bar(stat="identity", position="stack")+ xlab(input$vars)+ ylab(input$varsFill)
        }
    })
}
shinyApp(ui, server)
