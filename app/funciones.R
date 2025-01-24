

obtener_mapa <- function(region = 13) {
  if (region == 13) {
    return(mapa_urbano_rm)
  } else {
    return(mapas_regiones[[region]])
  }
}

# la lógica de las dos funciones siguientes es de poder definir una sola vez los selectores de variables y que se puedan usar varias veces

# input: pseudo modulo que contiene un selector de categoría de variables, un selector de variables que se va a filtrar por la categoría
selector_variables <- function(id = 1) {
  tagList(
    selectInput(paste0("categoria_", id), NULL, 
                choices = c("Pobreza", "Economía", "Política")
    ),
    
    selectInput(paste0("variable_", id), NULL, 
                choices = NULL
    ),
    
    textOutput(paste0("texto_", id))
  )
}

# server: pseudo modulo que va en server, y actualiza el selector de variables en base a la categoría seleccionada
actualizador_variables <- function(id, input, session) {
  observe({
    updateSelectInput(session, paste0("variable_", id), 
                      choices = paste("preguntas", input[[paste0("categoria_", id)]]))
  }) |> 
    bindEvent(input[[paste0("categoria_", id)]])
}


# como esta fuente tiene tantas variables, el método de carga es distinto: se carga la base una sola vez y se selecciona la columna por la variable seleccionada
casen_variable <- function(casen, variable_elegida) {
  # casen <- read.csv2("datos/casen_comunas.csv") |> tibble()
  
  variable_elegida <- variable_elegida |> str_remove("^casen_")
  
  casen_filtrada <- casen |> 
    select(comuna, cut_comuna, 
           variable = any_of(variable_elegida))
  
  return(casen_filtrada)
}


# como esta fuente tiene tantas variables, el método de carga es distinto: se carga la base una sola vez y se filtra por la variable seleccionada
siedu_variable <- function(siedu, variable_elegida) {
  # siedu <- read.csv2("datos/indicadores_desarrollo_urbano_siedu.csv") |> tibble()
  
  siedu_filtrada <- siedu |> 
    select(cut_comuna, variable, cifra) |> 
    filter(variable == variable_elegida) |> 
    select(cut_comuna, variable = cifra)
  
  return(siedu_filtrada)
}


# recibe tipo, y devuelve función de formateo
elegir_escala <- function(tipo = variable_fuente$tipo) {
  # browser()
  if (tipo == "porcentaje") {
    escala = scales::percent
  } else if (tipo == "porcentaje * 100") {
    escala = scales::label_comma(accuracy = 0.1, suffix = "%", big.mark = ".", decimal.mark = ",")
  } else if (tipo == "numero decimal") {
    escala = scales::label_comma(accuracy = 0.1,  big.mark = ".", decimal.mark = ",")
  } else {
    escala = scales::label_comma(accuracy = 1, big.mark = ".", decimal.mark = ",")
  }
  return(escala)
}

# recibe numero y tipo, retorna numero formateado
formatear_escala <- function(variable, tipo) {
  case_when(tipo == "porcentaje" ~ scales::percent(variable, accuracy = 0.1),
            tipo == "porcentaje * 100" ~ scales::comma(variable, accuracy = 0.1, suffix = "%", big.mark = ".", decimal.mark = ","),
            tipo == "numero decimal" ~ scales::comma(variable, accuracy = 0.1, big.mark = ".", decimal.mark = ","),
            tipo == "numero grande" ~ scales::comma(variable, accuracy = 1, big.mark = ".", decimal.mark = ","),
            .default = scales::comma(variable, accuracy = 0.1, big.mark = ".", decimal.mark = ",")
  )
}