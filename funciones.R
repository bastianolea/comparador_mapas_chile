

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