library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(sf) |> suppressPackageStartupMessages()
# library(thematic)
library(bslib)
library(stringr)
library(shinyjs) |> suppressPackageStartupMessages()
library(gt)
library(shinycssloaders)

# setup ----
cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")
fuentes <- read.csv2("fuentes.csv") #manda el contenido de los selectores y la carga de sus datos
regiones <- cut_comunas |> select(region, cut_region) |> distinct() |> tibble::deframe()

# setwd("app")
source("funciones.R")
source("modulos.R")


# tema ----
# thematic_shiny()
color_fondo = "#181818"
color_detalle = "#505050"
color_texto = "white"
color_principal = "#2AA198"
colores <- list("fondo" = color_fondo, "detalle" = color_detalle, "texto" = color_texto, "principal" = color_principal)
tema <- bs_theme(bg = color_fondo, fg = color_texto, primary = color_principal)
options(spinner.type = 8, spinner.color = color_principal)


ui <- fluidPage(
  title = "Datos comunales comparados", 
  lang = "es",
  theme = tema,
  shinyjs::useShinyjs(),
  
  tags$head(tags$style(type="text/css", "text {font-family: sans-serif}")), #corregir tipografía de ggiraph
  
  # header ----
  div(
    titlePanel(h1("Datos comunales comparados")),
    
    div(markdown("[Bastián Olea Herrera](https://bastianolea.github.io/shiny_apps/)"), style = "opacity: 0.4;"),
    
    markdown("Esta aplicación permite visualizar interactivamente las **diferencias y desigualdades territoriales** de Chile a través de mapas."),
    
    p("Seleccione una región del país, y luego elija dos variables para compararlas a nivel comunal. Por defecto, la aplicación le mostrará categorías y variables al azar, esperando que surja una relación interesante."),
    
    p("Puedes elegir entre más de 90 datos sociales, organizados en 10 categorías, que incluyen datos de salud, educación, ingresos, seguridad, delincuencia, urbanismo, y otros."),
    
    hr()
  ),
  
  fluidRow(
    column(12,
           
           # este selector afecta a todos los módulos
           selectInput("region", label = strong("Elija una región:"),
                       choices = c("Santiago" = 99, regiones),
                       selected = c("Santiago" = 99),
                       width = "100%"),
           hr()
    )
  ),
  
  # módulos ui ----
  fluidRow(
    column(6, #align = "right",
           style = "border: 0px solid green;", #display: flex;", # padding-right: 0; margin-right: 0;",
           mapaUI(id = "mapa_1", fuentes)
    ),
    column(6, 
           style = "border: 0px solid green;", # padding-left: 0; margin-left: 0;",
           mapaUI(id = "mapa_2", fuentes)
    )
  ),
  
  # gráfico dispersión ----
  fluidRow(
    column(12,
           hr(),
           h4("Relación entre datos"),
           
           div(style = "max-width: 700px; margin-left: auto; margin-right: auto;",
               girafeOutput("grafico_dispersion") |> withSpinner()
           )
    )
  ),
  
  
  # tabla ----
  fluidRow(
    column(12,
           hr(),
           h4("Comparación de datos"),
           
           div(style = "max-height: 400px; overflow-y: scroll;",
               gt_output("tabla_comparativa") |> withSpinner()
           )
    )
  ),
  
  # firma ----
  fluidRow(
    column(12, style = "opacity: 1; font-size: 80%;",
           hr(),
           markdown("Desarrollado y programado por [Bastián Olea Herrera,](https://bastian.olea.biz) usando el lenguaje de programación estadístico R."),
           
           markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
           
           markdown("Datos, código de fuente de esta app, y código del procesamiento de los datos [disponible en el repositorio de GitHub.](https://github.com/bastianolea/comparador_mapas_chile)"),
           
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
  
  
  
  
  ## selector de mapas ----
  # si la región elegida es el gran santiago, pasa como 99 y elige el mapa específico;
  # de lo contrario, simplemente carga el mapa de la región correspondiente
  mapa <- reactive({
    message("mapa region ", input$region)
    
    if (input$region == 99) {
      return(mapa_urbano_rm())
    } else {
      # elegir región filtrando la lista
      mapa_region <- mapas_regiones()[[input$region]]
      return(mapa_region)
    }
  }) |> 
    bindEvent(input$region)
  
  
  # input region, pero considerando al gran santiago como region 13 en vez de 99
  region <- reactive({
    region <- as.numeric(input$region)
    
    region_2 <- ifelse(region == 99, 13, region)
    
    return(region_2)
  })
  
  
  
  # datos ----
  
  ## datos individuales ----
  # cada uno de estos reactives carga una fuente de datos distinta,
  # que corresponde a una variable del selector de variables
  # para agregar una al selector de variables, de categorias y al mecanismo que los carga, hay que editar fuentes.csv
  # en fuentes.csv hay una columna que son los nombres de los objetos que se hacen a continuación, y que son cargados por el cargador de datos
  
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
  
  
  ### población ----
  d_poblacion <- reactive(read.csv2("datos/poblacion_chile_comunas.csv"))
  d_poblacion_aumento <- reactive(read.csv2("datos/poblacion_chile_comunas_crecimiento.csv"))
  
  d_poblacion_2017 <- reactive(d_poblacion() |> filter(año == 2017) |> rename(variable = población))
  d_poblacion_2024 <- reactive(d_poblacion() |> filter(año == 2024) |> rename(variable = población))
  d_poblacion_2030 <- reactive(d_poblacion() |> filter(año == 2030) |> rename(variable = población))
  
  d_poblacion_aumento_5a <- reactive(d_poblacion_aumento() |> rename(variable = pob_crecimiento_5a))
  d_poblacion_aumento_10a <- reactive(d_poblacion_aumento() |> rename(variable = pob_crecimiento_10a))
  d_poblacion_aumento_20a <- reactive(d_poblacion_aumento() |> rename(variable = pob_crecimiento_20a))
  
  
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
  d_sinim_22 <- reactive(d_sinim()[[22]])
  
  
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
  
  
  ### cuarteles carabineros ----
  
  d_cuarteles_carabineros <- reactive(read.csv2("datos/cuarteles_carabineros_itrend.csv"))
  
  d_cuarteles_carabineros_region <- reactive(d_cuarteles_carabineros() |> filter(cut_region == region()))
  
  d_cuarteles_carabineros_total <- reactive({
    d_cuarteles_carabineros_region() |> 
      group_by(cut_comuna) |> 
      summarize(variable = sum(n))
  })
  
  d_cuarteles_carabineros_comisaria <- reactive({
    d_cuarteles_carabineros_region() |> 
      filter(tipo == "Comisaria") |> 
      rename(variable = n)
  })
  
  d_cuarteles_carabineros_reten <- reactive({
    d_cuarteles_carabineros_region() |> 
      filter(tipo == "Reten") |> 
      rename(variable = n)
  })
  
  
  ## cargador de datos ----
  datos_1 <- eventReactive(variable_elegida_1(), {
    message("eligiendo datos para mapa 1")
    variable_elegida <- variable_elegida_1()
    variable_fuente <- variable_fuente_1()
    
    req(variable_elegida != "", nrow(variable_fuente) == 1)
    
    # buscar la variable entre las fuentes de datos y obtener el nombre del objeto reactive qeu cargaría sus datos
    objeto <- fuentes |> filter(variable == variable_elegida) |> pull(objeto)
    
    # cargar el objeto a partir del nombre de variable cruzado con lo que indica fuentes.csv
    # si es de casen, el proceso es levemente distinto
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
             datos = datos_1,
             colores = colores
  )
  
  mapaServer("mapa_2",
             session,
             region,
             mapa,
             fuentes,
             variable_elegida = variable_elegida_2,
             # variable_fuente = variable_fuente_2,
             datos = datos_2,
             colores = colores
  )
  
  # une las dos variables elegidas en un dataframe
  datos_unidos <- reactive({
    req(variable_elegida_1() != "",
        variable_elegida_2() != "")
    
    dato_1 <- datos_1() |> select(cut_comuna, variable_1 = variable) |> mutate(cut_comuna = as.numeric(cut_comuna))
    dato_2 <- datos_2() |> select(cut_comuna, variable_2 = variable) |> mutate(cut_comuna = as.numeric(cut_comuna))
    
    datos <- left_join(dato_1,
                       dato_2, by = "cut_comuna")
    
    datos_2 <- datos |> 
      left_join(cut_comunas, by = "cut_comuna") |> 
      filter(cut_region == region()) |>
      ungroup() |> 
      select(comuna, starts_with("variable"))
    
    return(datos_2)
  })
  
  
  
  # gráfico dispersión ----
  
  grafico_dispersion <- reactive({
    req(nrow(datos_unidos() > 1))
    # browser()
    
    plot <- datos_unidos() |> 
      # mutate(variable_1_etiqueta = formatear_escala(variable_2, variable_fuente_1()$tipo))
      ggplot(aes(x = variable_1, y = variable_2)) +
      stat_smooth(method = "lm", 
                  se = TRUE, fullrange = TRUE, 
                  alpha = .1, color = colores$detalle) +
      # geom_point(color = colores$principal,
      #            size = 3, alpha = .8) +
      geom_point_interactive(color = colores$principal,
                             size = 3.5, alpha = .8,
                             aes(
                               # texto de tooltip al posar cursor sobre una comuna
                               tooltip = paste0(comuna, ":\n", 
                                                "Vertical: ", formatear_escala(variable_2, variable_fuente_2()$tipo), "\n", 
                                                "Horizontal: ", formatear_escala(variable_1, variable_fuente_1()$tipo)
                               ),
                               data_id = comuna)) +
      # escalas
      scale_x_continuous(labels = elegir_escala(variable_fuente_1()$tipo), expand = c(0, 0)) +
      scale_y_continuous(labels = elegir_escala(variable_fuente_2()$tipo), expand = c(0, 0)) +
      coord_cartesian(clip = "off") +
      # textos
      labs(x = variable_elegida_1() |> str_wrap(70),
           y = variable_elegida_2() |> str_wrap(45), 
           caption = paste0("Fuentes: ", 
                            "Horizontal: ", variable_fuente_1()$fuente |> str_wrap(90), "\n",
                            "Vertical: ", variable_fuente_2()$fuente |> str_wrap(90))) +
      theme(plot.caption = element_text(color = "#707070", size = 10, margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 7)),
            axis.title.x = element_text(margin = margin(t = 6)),
            axis.ticks = element_blank()) +
      theme(text = element_text(colour = colores$texto),
            axis.text = element_text(colour = "#808080")) +
      theme(plot.background = element_rect(fill = colores$fondo, colour = colores$fondo, linewidth = 0),
            panel.background = element_rect(fill = "#252525"),
            panel.grid = element_line(colour = colores$fondo)
      )
    return(plot)
  })
  
  # gráfico interactivo con tooltip
  output$grafico_dispersion <- renderGirafe({
    girafe(ggobj = grafico_dispersion(), 
           bg = colores$fondo,
           width_svg = 8,
           height_svg = 7,
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
  
  # tabla comparativa ----
  output$tabla_comparativa <- render_gt({
    req(nrow(datos_unidos() > 1))
    
    datos_unidos() |> 
      arrange(desc(variable_1)) |> 
      gt() |> 
      data_color(
        method = "numeric", 
        palette = c(color_fondo, color_detalle, color_principal),
      ) |> 
      # decimales
      fmt_number(columns = starts_with("variable"),
                 sep_mark = ",",
                 drop_trailing_zeros = T, 
                 decimals = 2) |> 
      # alineación
      cols_align(align = "center",
                 columns = starts_with("variable")) |>
      #bordes de celdas
      tab_style(
        style = cell_borders(color = color_fondo, side = "right", weight = px(3), style = "solid"),
        locations = cells_body()) |> 
      #nombre de columnas
      tab_style(style = cell_text(weight = "bold", size = px(12)),
                locations = cells_column_labels(everything())
      ) |> 
      # primera columna
      tab_style(style = cell_text(weight = "bold", size = px(12)),
                locations = cells_body(column = "comuna")
      ) |> 
      tab_options(table.font.size = 11) |> 
      tab_options(table.font.color = color_texto, table.font.color.light = color_texto,
                  table_body.hlines.color = color_detalle,
                  table_body.vlines.color = color_detalle, 
                  column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle, 
                  table_body.border.bottom.color = color_detalle,
                  table.background.color = color_fondo) |> 
      cols_label(comuna = "Comuna",
                 variable_1 = variable_elegida_1(),
                 variable_2 = variable_elegida_2()) |> 
      tab_options(table_body.hlines.width = 3,
                  table_body.hlines.color = color_fondo,
                  table_body.vlines.width = 3,
                  table_body.vlines.color = color_fondo)
  })
}


shinyApp(ui = ui, server = server)