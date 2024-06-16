library(ggplot2)
library(shiny)


mapaUI <- function(id, fuentes) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
           selectInput(ns("categoria"), NULL, 
                       choices = unique(fuentes$categoria), #el contenido de los selectores viene de fuentes.csv
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
             plotOutput(ns("mapa")),
             hr(),
             div(style = "font-size: 80%",
                 tableOutput(ns("tabla"))
             )
      )
    )
  )
}


mapaServer <- function(id, session, region, mapa, fuentes, variable_elegida, datos) {
  moduleServer(id = id, session = session,
               
               module = function(input, output, session) {
                 
                 # cambiar el selector de variables en base a la categoría elegida
                 observeEvent(input$categoria, {
                   # browser()
                   variables_categoria <- fuentes |> filter(categoria == input$categoria) |> pull(variable) #variables de la categoría elegida
                   # variables_categoria <- variable_fuente()$variable
                   
                   if (length(variables_categoria) > 0) {
                     updateSelectInput(session, "variable", choices = variables_categoria)
                   } else {
                     updateSelectInput(session, "variable", choices = paste("preguntas", input$categoria))
                   }
                   
                   
                   
                 })
                 
                 # sacar la variable elegida al valor reactivo correspondiente (variable_elegida_1 o variable_elegida_2)
                 observe(variable_elegida(input$variable))
                 
                 # título del módulo
                 output$texto <- renderText(input$variable)
                 
                 # une el mapa con los datos, si es que existen
                 mapa_datos <- reactive({
                   if (length(datos()) > 0) {
                     message("modulo: uniendo mapa con datos")
                     
                     mapa_datos <- mapa() |> 
                       # mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
                       mutate(codigo_comuna = as.numeric(codigo_comuna)) |>
                       left_join(datos() |> 
                                   mutate(cut_comuna = as.numeric(cut_comuna)), 
                                          by = c("codigo_comuna" = "cut_comuna"))
                     
                   } else {
                     message("modulo: no hay datos")
                     mapa_datos <- mapa()
                   }
                   return(mapa_datos)
                 })
                 
                 
                 # generar mapa
                 output$mapa <-  renderPlot({
                   req(input$variable != "")
                   message("modulo: generando mapa")
                   
                   # browser()
                   variable_fuente <- fuentes |> filter(variable == input$variable)
                                       
                   p <- mapa_datos() |>
                     ggplot(aes(geometry = geometry))
                   
                   # colorizar fill si existe la variable
                   if ("variable" %in% names(mapa_datos())) {
                     p <- p +
                       geom_sf(aes(fill = variable),
                               color = "black")
                   } else {
                     p <- p +
                       geom_sf(color = "black")
                   }
                   
                   # título del gráfico
                   p <- p +
                     labs(title = str_wrap(variable_fuente$variable, 20),
                          caption = paste("Fuente:", str_wrap(variable_fuente$fuente, 30))
                          )
                   
                   # barra a la izquierda si es el de la izquierda
                   if (id == "mapa_1") {
                     p <- p +
                       theme(legend.position = "left")
                   }
                   
                   # si es celular, que sea arriba y abajo
                   
                   return(p)
                 })
                 
                 # tabla de diagnóstico
                 output$tabla <- renderTable({
                   mapa_datos() |> 
                     select(-geometry) |> 
                     select(-any_of("region"))
                 })
               }
  )
}