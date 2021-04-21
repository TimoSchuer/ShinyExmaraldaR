library(shiny)
ui <-   fluidPage(
    selectInput('selectfile','Select File',choice = list.files('log/')),
    textOutput('fileselected')
)

server <- function(input,output)
{
    output$fileselected <- renderText({
        paste0('You have selected: ', input$selectfile)
    })
}

shinyApp(ui,server)
