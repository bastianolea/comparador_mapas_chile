library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(chilemapas) |> suppressPackageStartupMessages()
library(thematic)
library(bslib)

# setup ----
cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")
regiones <- cut_comunas |> select(region, cut_region) |> distinct() |> tibble::deframe()
source("modulos.R")


# tema ----
thematic_shiny()
theme_set(theme_void())
tema <- bs_theme(bg = "#181818", fg = "white", primary = "#2AA198")


ui <- fluidPage(theme = tema,
  
  div(
  titlePanel(h1("Título")),
  ),
  
  fluidRow(
    column(12,
           p("aaaa"),
           # este selector afecta a todos los módulos
           selectInput("region", label = NULL,
                       choices = c("Santiago" = 99, regiones),
                       selected = c("Santiago" = 99),
                       width = "100%"),
           hr()
    )
  ),
  
  fluidRow(
    column(6,
           mapaUI(id = "mapa_1")
    ),
    column(6,
           mapaUI(id = "mapa_2")
    )
  ),
  
  ## firma ----
  fluidRow(
    column(12, style = "opacity: 0.5; font-size: 80%;",
           p("Diseñado y programado por",
             tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
           p("Puedes explorar mis otras",
             tags$a("aplicaciones interactivas sobre datos sociales aquí.",
                    href = "https://bastianolea.github.io/shiny_apps/", target = "_blank")
           ),
           p("Código de fuente de esta app y del procesamiento de los datos",
             tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/comparador_mapas_chile")
           ),
           
           div(style = "height: 40px")
           
    )
  )
)


server <- function(input, output, session) {
  
  
  
  # cargar mapas ----
  mapa_urbano_rm <- reactive({
    message("cargando mapa_urbano_rm")
    readRDS("mapas/mapa_urbano_rm.rds")
  })
  
  mapas_regiones <- reactive({
    message("cargando mapas_regiones")
    readRDS("mapas/mapas_regiones.rds")
  })
  
  region <- reactive(input$region)
  
  mapa <- reactive({
    message("mapa region ", region())
    
    if (region() == 99) {
      return(mapa_urbano_rm())
    } else {
      return(mapas_regiones()[[as.numeric(region())]])
    }
  }) |> bindEvent(region())
  
  
  # modulos ----
  mapaServer("mapa_1",
             session, #para el updateInput
             region = region, #reactive del selector que aplica a los dos modulos
             # datos
             mapa = mapa
  )
  
  mapaServer("mapa_2",
             session,
             region = region,
             # datos
             mapa = mapa
  )
}


shinyApp(ui = ui, server = server)



# library(shiny)
# library(dplyr)
# library(ggplot2)
# library(sf)
# library(chilemapas)
# 
# source("funciones.R")
# 
# cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")
# 
# regiones <- cut_comunas |> select(region, cut_region) |> distinct() |> tibble::deframe()
# 
# # ui ----
# ui <- fluidPage(
#   
#   fluidRow(
#     column(12,
#            h1("Título")
#     )
#   ),
#   
#   fluidRow(
#     column(12,
#            selectInput("region", label = NULL,
#                        choices = c("Santiago" = 99, regiones),
#                        selected = c("Santiago" = 99),
#                        width = "100%")
#     )
#   ),
#   
#   fluidRow(
#     column(6, 
#            h2("Mapa 1"),
#            
#            selector_variables(1),
#            
#            plotOutput("mapa1")
#     ),
#     
#     column(6, 
#            h2("Mapa 2"),
#            
#            selector_variables(2),
#            
#            plotOutput("mapa2")
#     )
#   )
# )
# 
# # server ----
# server <- function(input, output, session) {
#   
#   
#   output$texto_1 <- renderText(input$categoria_1)
#   output$texto_2 <- renderText(input$categoria_2)
#   
#   # observe({
#   #   updateSelectInput(session, "variable_1", 
#   #                     choices = paste("preguntas", input$categoria_1))
#   # }) |> 
#   #   bindEvent(input$categoria_1)
#   
#   # actualizan los selectores de preguntas en base a las categorías seleccionadas
#   actualizador_variables(1, input, session)
#   
#   actualizador_variables(2, input, session)
#   
#   
#   comunas <- reactive({
#     cut_comunas |> filter(codigo_region == input$region) |> pull(comunas)
#   })
#   
#   # cargar mapas ----
#   mapa_urbano_rm <- reactive({
#     message("cargando mapa_urbano_rm")
#     readRDS("mapas/mapa_urbano_rm.rds")
#   })
#   
#   mapas_regiones <- reactive({
#     message("cargando mapas_regiones")
#     readRDS("mapas/mapas_regiones.rds")
#   })
#   
#   # mapas ----
#   mapa <- reactive({
#     message("mapa region ", input$region)
#     
#     if (input$region == 99) {
#       return(mapa_urbano_rm())
#     } else {
#       return(mapas_regiones()[[as.numeric(input$region)]])
#     }
#   }) |> 
#     bindEvent(input$region)
#   
#   # gráficos ----
#   output$mapa1 <- renderPlot({
#     mapa() |> 
#       ggplot(aes(geometry = geometry)) +
#       geom_sf(fill = "grey60", color = "white") +
#       theme_void() +
#       labs(title = input$categoria1)
#   })
#   
#   output$mapa2 <- renderPlot({
#     mapa() |>
#       ggplot(aes(geometry = geometry)) +
#       geom_sf(fill = "grey60", color = "white") +
#       theme_void() +
#       labs(title = input$categoria2)
#   })
# }
# 
# shinyApp(ui = ui, server = server)
