library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(chilemapas)

source("obtener_mapa_urbano.R")

# ui ----
ui <- fluidPage(

    fluidRow(
      column(12,
             h1("Título")
      )
    ),
    
    fluidRow(
      column(6, 
             h2("Mapa 1"),
             plotOutput("mapa1")
      ),
      
      column(6, 
             h2("Mapa 2"),
             plotOutput("mapa2")
      )
    )
)

# server ----
server <- function(input, output) {

    output$mapa1 <- renderPlot({
      mapa_urbano |> 
      ggplot(aes(geometry = geometry)) +
        geom_sf(fill = "grey60", color = "white") +
        theme_void()
    })
    
    output$mapa2 <- renderPlot({
      mapa_urbano |> 
        ggplot(aes(geometry = geometry)) +
        geom_sf(fill = "grey60", color = "white") +
        theme_void()
    })
}

shinyApp(ui = ui, server = server)
