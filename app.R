library(shiny)
library(dplyr)
library(ggplot2)
library(thematic)
library(bslib)
library(stringr)

# setup ----
cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")
fuentes <- read.csv2("fuentes.csv") #manda el contenido de los selectores y la carga de sus datos
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
           mapaUI(id = "mapa_1", fuentes)
    ),
    column(6,
           mapaUI(id = "mapa_2", fuentes)
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
  
  # dataframe de 1 fila donde sale la metadata de la variable elegida, desde fuentes.csv
  variable_fuente_1 <- reactive({
    req(variable_elegida_1() != "")
    fuentes |> filter(variable == variable_elegida_1()) |> try()
  })
  
  variable_fuente_2 <- reactive({
    req(variable_elegida_2() != "")
    fuentes |> filter(variable == variable_elegida_2()) |> try()
  })
  
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
  # cada uno de estos reactives carga una fuente de datos distinta,
  # que corresponde a una variable del selector de variables
  # para agregar una al selector de variables, de categorias y al mecanismo que los carga, hay que editar fuentes.R
  
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
  
  
  ### sinim ----
  d_sinim <- reactive({
    readRDS("datos/datos_sinim.rds")
  })
  
  d_sinim_1 <- reactive(d_sinim()[[1]])
  d_sinim_2 <- reactive(d_sinim()[[2]])
  d_sinim_3 <- reactive(d_sinim()[[3]])
  d_sinim_4 <- reactive(d_sinim()[[4]])
  d_sinim_5 <- reactive(d_sinim()[[5]])
  d_sinim_6 <- reactive(d_sinim()[[6]])
  d_sinim_7 <- reactive(d_sinim()[[7]])
  d_sinim_8 <- reactive(d_sinim()[[8]])
  d_sinim_9 <- reactive(d_sinim()[[9]])
  d_sinim_10 <- reactive(d_sinim()[[10]])
  d_sinim_11 <- reactive(d_sinim()[[11]])
  d_sinim_12 <- reactive(d_sinim()[[12]])
  d_sinim_13 <- reactive(d_sinim()[[13]])
  d_sinim_14 <- reactive(d_sinim()[[14]])
  d_sinim_15 <- reactive(d_sinim()[[15]])
  d_sinim_16 <- reactive(d_sinim()[[16]])
  d_sinim_17 <- reactive(d_sinim()[[17]])
  d_sinim_18 <- reactive(d_sinim()[[18]])
  d_sinim_19 <- reactive(d_sinim()[[19]])
  d_sinim_20 <- reactive(d_sinim()[[20]])
  d_sinim_21 <- reactive(d_sinim()[[21]])
  
  
  ### paes ----
  
  d_paes <- reactive({
    read.csv2("datos/puntajes_paes_comuna_2024.csv")
  })
  
  d_paes_lectura <- reactive({
    d_paes() |> 
      rename(variable = paes_complectora)
  })
  
  d_promedio_notas <- reactive({
    d_paes() |> 
      rename(variable = promedio_notas)
  })
  
  d_paes_mate1 <- reactive({
    d_paes() |> 
      rename(variable = paes_matematica1)
  })
  
  d_paes_mate2 <- reactive({
    d_paes() |> 
      rename(variable = paes_matematica2)
  })
  
  d_paes_historia <- reactive({
    d_paes() |> 
      rename(variable = paes_histciesoc)
  })
  
  d_paes_ciencias <- reactive({
    d_paes() |> 
      rename(variable = paes_ciencias)
  })
  
  
  ### cead ----
  
  d_delinc_total_cantidad <- reactive(read.csv2("datos/delincuencia_total_cantidad.csv") |> rename(variable = delitos))
  
  d_delinc_mcs_cantidad <- reactive(read.csv2("datos/delincuencia_total_cantidad.csv") |> rename(variable = delitos))
  
  d_delinc_total_aumento <- reactive(read.csv2("datos/delincuencia_total_aumento.csv"))
  
  d_delinc_mcs_aumento <- reactive(read.csv2("datos/delincuencia_mcs_aumento.csv"))
  
  d_delinc_total_aumento_2a <- reactive(d_delinc_total_aumento() |> rename(variable = delitos_aumento_2a))
  d_delinc_total_aumento_3a <- reactive(d_delinc_total_aumento() |> rename(variable = delitos_aumento_3a))
  d_delinc_total_aumento_5a <- reactive(d_delinc_total_aumento() |> rename(variable = delitos_aumento_5a))
  
  d_delinc_mcs_aumento_2a <- reactive(d_delinc_mcs_aumento() |> rename(variable = delitos_mcs_aumento_2a))
  d_delinc_mcs_aumento_3a <- reactive(d_delinc_mcs_aumento() |> rename(variable = delitos_mcs_aumento_3a))
  d_delinc_mcs_aumento_5a <- reactive(d_delinc_mcs_aumento() |> rename(variable = delitos_mcs_aumento_5a))
  
  d_delinc_mcs_porcentaje <- reactive(read.csv2("datos/delincuencia_mcs_porcentaje.csv") |> rename(variable = delitoc_mcs_p))
  
  d_delinc_total_tasa <- reactive(read.csv2("datos/delincuencia_total_tasa.csv") |> rename(variable = tasa))
  
  d_delinc_mcs_tasa <- reactive(read.csv2("datos/delincuencia_mcs_tasa.csv") |> rename(variable = tasa))
  
  
  
  ### casen ----
  # son tantas variables que vamos a usar un método distinto, se carga solo el df y en el cargador de datos se le dice qué variable quiere
  d_casen <- reactive(read.csv2("datos/casen_comunas.csv"))

  
  
  ## cargador de datos ----
  datos_1 <- eventReactive(variable_elegida_1(), {
    message("eligiendo datos para mapa 1")
    variable_elegida <- variable_elegida_1()
    variable_fuente <- variable_fuente_1()
    
    req(variable_elegida != "", nrow(variable_fuente) == 1)
    
    # buscar la variable entre las fuentes de datos y obtener el nombre del objeto reactive qeu cargaría sus datos
    objeto <- fuentes |> filter(variable == variable_elegida) |> pull(objeto)
    
    # cargar el objeto a partir del nombre de variable cruzado con lo que indica fuentes.csv
    if (variable_fuente$proyecto == "casen") {
      objeto_reactive <- casen_variable(d_casen(), variable_fuente$objeto) |> tibble()
      
    } else {
      # transformar el nombre del objeto en el objeto mismo (magia)
      if (length(objeto) == 1) objeto_reactive <- eval(as.symbol(objeto))() #wtf pero funciona
    }
    
    # retornarlo si existe
    if (length(objeto) == 1) return(objeto_reactive) 
  })
  
  
  datos_2 <- eventReactive(variable_elegida_2(), {
    message("eligiendo datos para mapa 2")
    variable_elegida <- variable_elegida_2()
    variable_fuente <- variable_fuente_2()
    
    req(variable_elegida != "", nrow(variable_fuente) == 1)
    
    # buscar la variable entre las fuentes de datos y obtener el nombre del objeto reactive qeu cargaría sus datos
    objeto <- fuentes |> filter(variable == variable_elegida) |> pull(objeto)
    
    # cargar el objeto a partir del nombre de variable cruzado con lo que indica fuentes.csv
    if (variable_fuente$proyecto == "casen") {
      objeto_reactive <- casen_variable(d_casen(), variable_fuente$objeto) |> tibble()
      
    } else {
      # transformar el nombre del objeto en el objeto mismo (magia)
      if (length(objeto) == 1) objeto_reactive <- eval(as.symbol(objeto))() #wtf pero funciona
    }
    
    # retornarlo si existe
    if (length(objeto) == 1) return(objeto_reactive) 
  })
  
  
  
  
  
  # modulos ----
  
  mapaServer("mapa_1",
             session, #para el updateInput
             region, #reactive del selector que aplica a los dos modulos
             mapa,
             fuentes,
             variable_elegida = variable_elegida_1,
             # variable_fuente = variable_fuente_1,
             datos = datos_1
  )
  
  mapaServer("mapa_2",
             session,
             region,
             mapa,
             fuentes,
             variable_elegida = variable_elegida_2,
             # variable_fuente = variable_fuente_2,
             datos = datos_2
  )
}


shinyApp(ui = ui, server = server)