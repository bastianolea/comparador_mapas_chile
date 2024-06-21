fuentes <- read.csv2("fuentes.csv")

fuentes |> 
  count(categoria) |> 
  arrange(desc(n))
