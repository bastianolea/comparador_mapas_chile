library(shiny)
library(dplyr)
library(ggplot2)
# library(sf)
# library(chilemapas) |> suppressPackageStartupMessages()
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


ui <- fluidPage(
  theme = tema,
  
  # header ----
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
  
  # módulos ui ----
  fluidRow(
    column(6,
           mapaUI(id = "mapa_1")
    ),
    column(6,
           mapaUI(id = "mapa_2")
    )
  ),
  
  # firma ----
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

#—----
server <- function(input, output, session) {
  
  
  # variables ----
  variable_elegida_1 <- reactiveVal()
  variable_elegida_2 <- reactiveVal()
  
  observeEvent(variable_elegida_1(), {
    message("variable elegida para mapa 1: ", variable_elegida_1())
  })
  
  observeEvent(variable_elegida_2(), {
    message("variable elegida para mapa 2: ", variable_elegida_2())
  })
  
  
  
  
  # mapas ----
  ## cargar mapas ----
  mapa_urbano_rm <- reactive({
    message("cargando mapa_urbano_rm")
    readRDS("mapas/mapa_urbano_rm.rds")
  })
  
  mapas_regiones <- reactive({
    message("cargando mapas_regiones")
    readRDS("mapas/mapas_regiones.rds")
  })
  
  region <- reactive(input$region)
  
  
  ## selector de mapas ----
  mapa <- reactive({
    message("mapa region ", region())
    
    if (region() == 99) {
      return(mapa_urbano_rm())
    } else {
      return(mapas_regiones()[[as.numeric(region())]])
    }
  }) |> 
    bindEvent(region())
  
  
  
  
  # datos ----
  
  ## datos individuales ----
  d_plebiscito_22 <- reactive({
    readr::read_csv2("datos/resultados_plebiscito_2022_comuna.csv", show_col_types = F) |> 
      group_by(cut_comuna) |> 
      mutate(total = sum(votos, na.rm = T),
             porcentaje = votos/total)
  })
  
  d_plebiscito_22_apruebo <- reactive({
    d_plebiscito_22() |> 
      filter(tolower(opciones) == "apruebo") |> 
      mutate(variable = porcentaje)
  })
  
  d_plebiscito_22_rechazo <- reactive({
    d_plebiscito_22() |> 
      filter(tolower(opciones) == "rechazo") |> 
      mutate(variable = porcentaje)
  })
  
  d_plebiscito_23 <- reactive({
    readr::read_csv2("datos/resultados_plebiscito_2023_comuna.csv", show_col_types = F) |> 
      group_by(cut_comuna) |> 
      mutate(total = sum(votos, na.rm = T),
             porcentaje = votos/total)
  })
  
  d_plebiscito_23_encontra <- reactive({
    d_plebiscito_23() |> 
      filter(tolower(opciones) == "en contra") |> 
      mutate(variable = porcentaje)
  })
  
  d_plebiscito_23_afavor <- reactive({
    d_plebiscito_23() |> 
      filter(tolower(opciones) == "a favor") |> 
      mutate(variable = porcentaje)
  })
  
  
  
  ## cargador de datos ----
  datos_1 <- eventReactive(variable_elegida_1(), {
    message("eligiendo datos para mapa 1")
    variable <- variable_elegida_1()
    
    if (variable == "Plebiscito 2022: apruebo") return(d_plebiscito_22_apruebo())
    if (variable == "Plebiscito 2022: rechazo") return(d_plebiscito_22_rechazo())
    if (variable == "Plebiscito 2023: en contra") return(d_plebiscito_23_encontra())
    if (variable == "Plebiscito 2023: a favor") return(d_plebiscito_23_afavor())
  })
  
  datos_2 <- eventReactive(variable_elegida_2(), {
    message("eligiendo datos para mapa 2")
    variable <- variable_elegida_2()
    
    if (variable == "Plebiscito 2022: apruebo") return(d_plebiscito_22_apruebo())
    if (variable == "Plebiscito 2022: rechazo") return(d_plebiscito_22_rechazo())
    if (variable == "Plebiscito 2023: en contra") return(d_plebiscito_23_encontra())
    if (variable == "Plebiscito 2023: a favor") return(d_plebiscito_23_afavor())
  })
  
  
  
  
  
  # modulos ----
  
  mapaServer("mapa_1",
             session, #para el updateInput
             region = region, #reactive del selector que aplica a los dos modulos
             # datos
             mapa = mapa,
             variable_elegida = variable_elegida_1,
             datos = datos_1
  )
  
  mapaServer("mapa_2",
             session,
             region = region,
             # datos
             mapa = mapa,
             variable_elegida = variable_elegida_2,
             datos = datos_2
  )
}


shinyApp(ui = ui, server = server)