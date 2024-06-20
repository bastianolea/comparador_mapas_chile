library(ggplot2)
library(shiny)


mapaUI <- function(id, fuentes) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12, #style = "max-width: 600px;",
             
             # div(style = "height: 150px;",
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
             
      )
    ),
    fluidRow(
      column(12, #align = "right",
             style = ifelse(id == "mapa_1",
                            "border: 0px solid blue; padding: 0; padding-left: 10px; margin-left: auto; margin-right: 4px;", #izquierdo
                            "border: 0px solid orange; padding: 0; padding-right: 10px; margin-right: auto; margin-left: 4px;" #derecho
             ),
             
             # título
             div(style = "height: 80px; display: flex; flex-direction: column; overflow: visible;", #alinear texto abajo, para que si crece el texto, suba
                 div(style = "margin-top: auto; font-size: 110%;", 
                     # max-width: 500px; inline-size: 500px; overflow-wrap: break-word;",
                     textOutput(ns("titulo"), inline = TRUE)
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
                 output$fuente <- renderText({
                   fuente <- fuentes |> filter(variable == input$variable) |> pull(fuente)
                   fuente_2 <- paste("Fuente: ", str_wrap(fuente, 50))
                   return(fuente_2)
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
                   
                   # metadatos de la variable (definido en fuentes.csv)
                   variable_fuente <- fuentes |> filter(variable == input$variable)
                   
                   ## escalas ----
                   # definir escala de la leyenda del gráfico en base al tipo de variable (definido en fuentes.csv)
                   escala = elegir_escala(variable_fuente$tipo)
                   
                   ## colores ----
                   # unique(fuentes$categoria)
                   # color_alto <- case_when(input$categoria == "Elecciones" ~ colores$principal,
                   #                         input$categoria == "Elecciones" ~ colores$principal,
                     
                   # gráfico base ----
                   p <- mapa_datos() |>
                     ggplot(aes(geometry = geometry))
                   
                   # colorizar fill si existe la variable
                   if ("variable" %in% names(mapa_datos())) {
                     p <- p +
                       geom_sf_interactive(aes(fill = variable,
                                               # texto de tooltip al posar cursor sobre una comuna
                                               tooltip = paste0(nombre_comuna, ": ", formatear_escala(variable, variable_fuente$tipo)),
                                               data_id = nombre_comuna),
                               color = "black") +
                       scale_fill_gradient(low = colores$texto,
                                           high = colores$principal, 
                                           na.value = colores$detalle,
                                           labels = escala)

                   } else {
                     p <- p +
                       geom_sf(color = "black")
                   }
                   
                   # temas ----
                   p <- p +
                     theme_void(base_family = "sans") +
                     theme(legend.title = element_blank()) +
                     theme(legend.key.width = unit(3, "mm"),
                           legend.key = element_rect(colour = colores$texto),
                           legend.ticks = element_line(colour = colores$fondo),
                           legend.ticks.length = unit(3, "mm")) +
                     theme(text = element_text(colour = colores$texto)) +
                     theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
                     theme(plot.background = colores$fondo,
                           panel.background = colores$fondo)
                   
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