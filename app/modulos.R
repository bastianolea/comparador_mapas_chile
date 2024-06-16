library(ggplot2)
library(shiny)


mapaUI <- function(id, fuentes) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12, 
             
             # div(style = "height: 150px;",
             selectInput(ns("categoria"), NULL, 
                         choices = unique(fuentes$categoria), #el contenido de los selectores viene de fuentes.csv
                         selected = sample(unique(fuentes$categoria), 1),
                         width = "100%"
             ),
             
             selectInput(ns("variable"), NULL, 
                         choices = NULL,
                         width = "100%"
                         # )
             ),
             
      )
    ),
    fluidRow(
      column(12, #align = "right",
             style = ifelse(id == "mapa_1",
                            "border: 0px solid blue; padding: 0; padding-left: 10px; margin-left: auto; margin-right: 0;", #izquierdo
                            "border: 0px solid orange; padding: 0; padding-right: 10px; margin-right: auto; margin-left: 4px;" #derecho
             ),
             div(style = "height: 80px; 
                          display: flex; flex-direction: column; overflow: visible;",
                 div(style = "margin-top: auto; font-size: 120%;", #alineado abajo
                     textOutput(ns("titulo"))
                 )
             ),
             
             div(style = "border: 0px solid white; margin: 0;", #margin-left: auto; padding-right: 0; margin-right: 0;",
                 plotOutput(ns("mapa"))  |> withSpinner()
             )
             # hr(),
             # div(style = "max-height: 300px; overflow-y: scroll;",
             #   gt_output(ns("tabla"))
             # )
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
                     # browser()
                     updateSelectInput(session, "variable", 
                                       choices = variables_categoria,
                                       selected = sample(variables_categoria, 1)
                     )
                   } else {
                     updateSelectInput(session, "variable", choices = paste("preguntas", input$categoria))
                   }
                   
                   
                   
                 })
                 
                 # sacar la variable elegida al valor reactivo correspondiente (variable_elegida_1 o variable_elegida_2)
                 observe(variable_elegida(input$variable))
                 
                 # título del módulo
                 output$titulo <- renderText(input$variable)
                 
                 # fuente
                 output$fuente <- renderText(variable_fuente$fuente)
                 
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
                   
                   # # título del gráfico
                   # p <- p +
                   #   labs(title = str_wrap(variable_fuente$variable, 45),
                   #        caption = paste("Fuente:", str_wrap(variable_fuente$fuente, 60))
                   #        )
                   
                   # tema general
                   p <- p +
                     theme(plot.title.position = "plot", 
                           legend.title = element_blank(), legend.key.width = unit(3, "mm")) +
                     theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
                   
                   # barra a la izquierda si es el de la izquierda
                   if (id == "mapa_1") {
                     p <- p +
                       theme(legend.position = "left")
                   }
                   
                   # si es celular, que sea arriba y abajo
                   
                   return(p)
                 }, res = 90)
                 
                 # tabla ----
                 # tabla de diagnóstico
                 # output$tabla <- render_gt({
                 #   color_fondo = "#181818"
                 #   color_detalle = "#505050"
                 #   color_texto = "white"
                 #   color_principal = "#2AA198"
                 #   
                 #   mapa_datos() |> 
                 #     # select(-geometry) |> 
                 #     # select(-any_of("region"))
                 #     select(comuna, variable) |> 
                 #     arrange(desc(variable)) |> 
                 #     gt() |> 
                 #     data_color(
                 #       method = "numeric", 
                 #       palette = c(color_fondo, color_detalle, color_principal),
                 #     ) |> 
                 #     fmt_number(columns = variable, 
                 #                sep_mark = ",",
                 #                drop_trailing_zeros = T, 
                 #                decimals = 2) |> 
                 #     tab_options(table.font.size = 10) |> 
                 #     tab_options(table.font.color = color_texto, table.font.color.light = color_texto,
                 #                 table_body.hlines.color = color_detalle,
                 #                 table_body.vlines.color = color_detalle, 
                 #                 column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle, 
                 #                 table_body.border.bottom.color = color_detalle,
                 #                 table.background.color = color_fondo)
                 # })
               }
  )
}