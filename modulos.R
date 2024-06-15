library(ggplot2)
library(shiny)


mapaUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
           selectInput(ns("categoria"), NULL, 
                       choices = c("Pobreza", "Economía", "Política"),
                       width = "100%"
           ),
           
           selectInput(ns("variable"), NULL, 
                       choices = NULL,
                       width = "100%"
           ),
           
           textOutput(ns("texto"))
           
    ),
    fluidRow(
      column(12,
             plotOutput(ns("mapa"))
      )
    )
  )
}


mapaServer <- function(id, session, region, mapa) {
  moduleServer(id = id,session = session,
                
    module = function(input, output, session) {
      
      message("modulo")
      
      output$texto <- renderText(input$categoria)
      
      observeEvent(input$categoria, {
          updateSelectInput(session, "variable",
                            choices = paste("preguntas", input$categoria))
        })
      
      output$mapa <-  renderPlot({
        mapa() |>
          ggplot(aes(geometry = geometry)) +
          geom_sf(fill = "grey60", color = "white") +
          # theme_void() +
          labs(title = input$categoria)
      })
    }
  )
}