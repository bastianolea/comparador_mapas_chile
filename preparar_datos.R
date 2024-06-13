library(lubridate)
library(tidyr)

datos_delinc <- arrow::read_parquet("datos/cead_delincuencia.parquet") #delincuencia_chile
datos_poblac <- arrow::read_parquet("datos/censo_proyecciones_año.parquet") #censo_proyecciones


# delincuencia ----
delitos_mayor_connotacion <- c("Homicidios",
                               "Hurtos", 
                               "Lesiones menos graves, graves o gravísimas",
                               "Robo con violencia o intimidación", 
                               "Robo en lugar habitado",
                               "Robo de vehículo motorizado",
                               "Robo por sorpresa",
                               "Otros robos con fuerza",
                               "Abusos sexuales y otros delitos sexuales",
                               "Violaciones",
                               "Violencia intrafamiliar a adulto mayor",
                               "Violencia intrafamiliar a hombre",
                               "Violencia intrafamiliar a mujer",
                               "Violencia intrafamiliar a niño",
                               "Violencia intrafamiliar no clasificado")

# delitos 2023
datos_delinc_anual <- datos_delinc |> 
  filter(year(fecha) == 2023) |> 
  group_by(cut_comuna, comuna, cut_region, region) |> 
  summarize(delitos = sum(delito_n))

# delitos por año
datos_delinc_años <- datos_delinc |> 
  mutate(año = year(fecha)) |> 
  group_by(cut_comuna, comuna, cut_region, region, año) |> 
  summarize(delitos = sum(delito_n)) |> 
  pivot_wider(names_from = año, values_from = delitos, names_prefix = "delitos_")

# aumento de delitos entre años
datos_delinc_aumento <- datos_delinc_años |> 
mutate(delitos_aumento_2a = delitos_2023/delitos_2021,
       delitos_aumento_3a = delitos_2023/delitos_2020,
       delitos_aumento_5a = delitos_2023/delitos_2019,
       delitos_aumento_10a = delitos_2023/delitos_2013) |> 
  select(1:4, matches("aumento"))


# delitos 2023 de mayor connotación social
datos_delinc_mcs_anual <- datos_delinc |> 
  filter(year(fecha) == 2023) |> 
  filter(delito %in% delitos_mayor_connotacion) |> 
  group_by(cut_comuna, comuna, cut_region, region) |> 
  summarize(delitos_mcs = sum(delito_n))

# porcentaje de delitos que son de mayor connotación social
datos_delinc_anual |> 
  left_join(datos_delinc_mcs_anual) |> 
  mutate(delitoc_mcs_p = delitos/delitos_mcs)
  


# delitos de mayor connotación social por año
datos_delinc_mcs_años <- datos_delinc |> 
  filter(delito %in% delitos_mayor_connotacion) |> 
  mutate(año = year(fecha)) |> 
  group_by(cut_comuna, comuna, cut_region, region, año) |> 
  summarize(delitos_mcs = sum(delito_n)) |> 
  pivot_wider(names_from = año, values_from = delitos_mcs, names_prefix = "delitos_mcs_")

# aumento de delitos de mayor connotación social entre años
datos_delinc_mcs_aumento <- datos_delinc_mcs_años |> 
  mutate(delitos_mcs_aumento_2a = delitos_mcs_2023/delitos_mcs_2021,
         delitos_mcs_aumento_3a = delitos_mcs_2023/delitos_mcs_2020,
         delitos_mcs_aumento_5a = delitos_mcs_2023/delitos_mcs_2019,
         delitos_mcs_aumento_10a = delitos_mcs_2023/delitos_mcs_2013) |> 
  select(1:4, matches("aumento"))



# población ----

# población por años
datos_poblac_años <- datos_poblac |> 
  pivot_wider(names_from = año, values_from = población, names_prefix = "poblacion_")

# crecimiento poblacional
datos_poblac_crecim <- datos_poblac_años |> 
  mutate(pob_crecimiento_5a = poblacion_2023/poblacion_2019,
         pob_crecimiento_10a = poblacion_2023/poblacion_2013,
         pob_crecimiento_20a = poblacion_2023/poblacion_2003) |> 
  select(1:6, matches("crecimiento"))
