library(ggplot2)
library(shiny)


mapaUI <- function(id, fuentes) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12, 
             selectInput(ns("categoria"), strong("Categoría:"), 
                         choices = unique(fuentes$categoria), #el contenido de los selectores viene de fuentes.csv
                         selected = sample(unique(fuentes$categoria), 1),
                         width = "100%"
             ),
             
             selectInput(ns("variable"), strong("Variable:"), 
                         choices = NULL,
                         width = "100%"
                         # )
             ),
             
             div(style = "margin-bottom: 4px;",
                 actionButton(ns("aleatorio"), label = "variable al azar")
             )
             
      )
    ),
    fluidRow(
      column(12, #align = "right",
             style = ifelse(id == "mapa_1",
                            "border: 0px solid blue; padding: 0; padding-left: 10px; margin-left: auto; margin-right: 4px;", #izquierdo
                            "border: 0px solid orange; padding: 0; padding-right: 10px; margin-right: auto; margin-left: 4px;" #derecho
             ),
             
             # título
             #alinear texto abajo, para que si crece el texto, suba
             div(style = "height: 90px; display: flex; flex-direction: column; overflow: visible;", 
                 div(style = "margin-top: auto; font-size: 110%;", 
                     # textOutput(ns("titulo"), inline = TRUE)
                     uiOutput(ns("titulo"), inline = TRUE)
                 )
             ),
             
             div(style = "border: 0px solid white; margin: 0;", #margin-left: auto; padding-right: 0; margin-right: 0;",
                 girafeOutput(ns("mapa_interactivo")) |> withSpinner()
             ),
             # fuente
             div(style = "font-size: 70%; text-align: right; opacity: 0.4;",
                 textOutput(ns("fuente"))
             )
             
             # hr(),
             # div(style = "max-height: 300px; overflow-y: scroll;",
             #   gt_output(ns("tabla"))
             # )
      )
      
    )
  )
}


mapaServer <- function(id, session, region, mapa, fuentes, variable_elegida, datos, colores) {
  moduleServer(id = id, session = session,
               
               module = function(input, output, session) {
                 
                 # al elegir una categoría, actualiza el selector de variables para que sean las de la categoría, y elige una variable al azar
                 # cambiar el selector de variables en base a la categoría elegida
                 observeEvent(input$categoria, {
                   variables_categoria <- fuentes |> filter(categoria == input$categoria) |> pull(variable) #variables de la categoría elegida
                   
                   if (length(variables_categoria) > 0) {
                     updateSelectInput(session, "variable", 
                                       choices = variables_categoria,
                                       selected = sample(variables_categoria, 1)
                     )
                   } else {
                     updateSelectInput(session, "variable", choices = paste("preguntas", input$categoria))
                   }
                 })
                 
                 # variable al azar
                 observeEvent(input$aleatorio, {
                   # cambia el selector de categorías, y ese por defecto elige una variable al azar
                   updateSelectInput(session, "categoria", 
                                     selected = sample(unique(fuentes$categoria), 1)
                   )
                 })
                 
                 
                 # variable_elegida ----
                 # sacar la variable elegida al valor reactivo correspondiente (variable_elegida_1 o variable_elegida_2)
                 observe(
                   if (input$variable != "") {
                     message("escribiendo variable elegida: ", input$variable)
                     variable_elegida(input$variable)
                   })
                 
                 # metadatos ----
                 # metadatos de la variable (definido en fuentes.csv)
                 variable_fuente <- reactive(fuentes |> filter(variable == input$variable) |> slice(1))
                 
                 # textos ----
                 
                 # título del módulo
                 # output$titulo <- renderText(input$variable)
                 output$titulo <- renderUI({
                   if (nchar(input$variable) > 80) {
                     div(input$variable, style = "font-size: 85%;")
                   } else {
                     div(input$variable)
                   }
                   })
                 
                 # fuente
                 output$fuente <- renderText({
                   # if (fuentes |> filter(variable == input$variable) |> pull(proyecto) == "siedu") browser()
                   
                   fuente <- paste("Fuente: ", str_wrap(str_squish(variable_fuente()$fuente), 50))
                   return(fuente)
                 })
                 
                 
                 # datos ----
                 # une el mapa con los datos, si es que existen
                 mapa_datos <- reactive({
                   if (length(datos()) > 0) {
                     message("modulo: uniendo mapa con datos")
                     # browser()
                     
                     mapa_datos <- mapa() |> 
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
                 
                 
                 # mapa ----
                 # output$mapa <- renderPlot({
                 # output$mapa_interactivo <- renderGirafe({
                 grafico <- reactive({
                   req(input$variable != "")
                   message("modulo: generando mapa")
                   
                   # browser()
                   # if (variable_fuente()$proyecto == "siedu") browser()
                   
                   ## escalas ----
                   # definir escala de la leyenda del gráfico en base al tipo de variable (definido en fuentes.csv)
                   escala = elegir_escala(variable_fuente()$tipo)
                   
                   # definir si va un texto luego del tooltip, como "(metros)", "(kW/h)"
                   sufijo = ifelse(variable_fuente()$unidad != "ninguna", 
                                   paste0(" (", variable_fuente()$unidad, ")"), 
                                   "")
                   
                   ## gráfico base ----
                   p <- mapa_datos() |>
                     ggplot(aes(geometry = geometry)) +
                     # colores de los polígonos
                     geom_sf_interactive(aes(fill = variable,
                                             # texto de tooltip al posar cursor sobre una comuna
                                             tooltip = paste0(nombre_comuna, ": ", 
                                                              formatear_escala(variable, variable_fuente()$tipo),
                                                              sufijo
                                             ),
                                             data_id = nombre_comuna),
                                         color = colores$fondo, linewidth = 0.6) +
                     # colores del degradado
                     scale_fill_gradient(low = "grey90", #colores$texto, #colores$fondo, 
                                         high = colores$principal, 
                                         na.value = colores$detalle,
                                         labels = escala)
                   
                   ## temas ----
                   p <- p +
                     theme_void(base_family = "sans") +
                     theme(legend.title = element_blank()) +
                     theme(legend.key.width = unit(3, "mm"),
                           legend.key.height = unit(1.6, "cm"),
                           legend.key = element_rect(colour = colores$texto),
                           legend.ticks = element_line(colour = colores$fondo),
                           legend.ticks.length = unit(3, "mm")) +
                     theme(text = element_text(colour = colores$texto)) +
                     theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
                     theme(plot.background = element_rect(fill = colores$fondo, color = colores$fondo),
                           panel.background = element_rect(fill = colores$fondo, color = colores$fondo))
                   
                   # barra a la izquierda si es el de la izquierda
                   if (id == "mapa_1") {
                     p <- p +
                       theme(legend.position = "left")
                   }
                   
                   # si es celular, que sea arriba y abajo
                   
                   return(p)
                 })
                 
                 # interactividad ----
                 # gráfico interactivo con tooltip
                 output$mapa_interactivo <- renderGirafe({
                   message("ggiraph")
                   girafe(ggobj = grafico(), 
                          bg = colores$fondo,
                          width_svg = 7,
                          height_svg = 6,
                          options = list(
                            opts_sizing(rescale = TRUE),
                            opts_toolbar(hidden = "selection", saveaspng = FALSE),
                            opts_hover(css = paste0("fill: ", colores$principal, ";")),
                            opts_tooltip(
                              opacity = 0.8,
                              css = paste0("background-color: ", colores$fondo, "; color: ", colores$texto, ";
                               padding: 4px; max-width: 200px; border-radius: 4px; font-size: 80%;")) 
                          ))
                 })
                 
                 # tabla 
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