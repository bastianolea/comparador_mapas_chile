library(dplyr)

# setwd("app")
# fuentes <- read.csv2("app/fuentes.csv") |> tibble()
fuentes <- read.csv2("fuentes.csv")

fuentes |> 
  count(categoria) |> 
  arrange(desc(n))

# tabla de variables
tabla_variables <- fuentes |> 
  select(variable, categoria, fuente) |> 
  arrange(desc(categoria)) |> 
  distinct(variable, .keep_all = TRUE)

# sacar tabla en formato markdown
tabla_variables_markdown <- tabla_variables |> 
  knitr::kable(format = "markdown")

clipr::write_clip(tabla_variables_markdown) #copiar