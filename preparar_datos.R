library(lubridate)
library(tidyr)

# setwd("..")
# datos_delinc <- arrow::read_parquet("app/datos/cead_delincuencia.parquet") #delincuencia_chile
# datos_poblac <- arrow::read_parquet("app/datos/censo_proyecciones_año.parquet") #censo_proyecciones


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
  summarize(delitos = sum(delito_n)) |> 
  ungroup()

write.csv2(datos_delinc_anual, "datos/delincuencia_total_cantidad.csv")

# delitos por año
datos_delinc_años <- datos_delinc |> 
  mutate(año = year(fecha)) |> 
  group_by(cut_comuna, comuna, cut_region, region, año) |> 
  summarize(delitos = sum(delito_n)) |> 
  pivot_wider(names_from = año, values_from = delitos, names_prefix = "delitos_")

# aumento de delitos entre años
datos_delinc_aumento <- s |> 
mutate(delitos_aumento_2a = delitos_2023/delitos_2021,
       delitos_aumento_3a = delitos_2023/delitos_2020,
       delitos_aumento_5a = delitos_2023/delitos_2019,
       delitos_aumento_10a = delitos_2023/delitos_2013) |> 
  mutate(across(starts_with("delitos_"), ~.x - 1)) |> 
  select(1:4, matches("aumento"))

write.csv2(datos_delinc_aumento, "app/datos/delincuencia_total_aumento.csv")


# delitos 2023 de mayor connotación social
datos_delinc_mcs_anual <- datos_delinc |> 
  filter(year(fecha) == 2023) |> 
  filter(delito %in% delitos_mayor_connotacion) |> 
  group_by(cut_comuna, comuna, cut_region, region) |> 
  summarize(delitos_mcs = sum(delito_n))

write.csv2(datos_delinc_mcs_anual, "app/datos/delincuencia_mcs_cantidad.csv")

# porcentaje de delitos que son de mayor connotación social
datos_delinc_mcs_porcentaje <- datos_delinc_anual |> 
  left_join(datos_delinc_mcs_anual) |> 
  mutate(delitoc_mcs_p = delitos/delitos_mcs)
  
write.csv2(datos_delinc_mcs_porcentaje, "app/datos/delincuencia_mcs_porcentaje.csv")


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
  mutate(across(starts_with("delitos_"), ~.x - 1)) |> 
  select(1:4, matches("aumento"))

write.csv2(datos_delinc_mcs_aumento, "app/datos/delincuencia_mcs_aumento.csv")
# ok ----


# delitos per capita ----
datos_delinc_total_tasa <- datos_delinc_anual |> 
  mutate(año = 2023) |> 
  left_join(datos_poblac |> select(-comuna), by = join_by(cut_comuna, año)) |> 
  group_by(cut_comuna, comuna, año) |> 
  summarize(delitos = sum(delitos), 
            poblacion = first(población)) |> 
  ungroup() |> 
  mutate(tasa = (delitos / poblacion) * 1000)

write.csv2(datos_delinc_total_tasa, "datos/delincuencia_total_tasa.csv")

datos_delinc_mcs_tasa <- datos_delinc_mcs_anual |> 
  mutate(año = 2023) |> 
  left_join(datos_poblac |> select(-comuna), by = join_by(cut_comuna, año)) |> 
  group_by(cut_comuna, comuna, año) |> 
  summarize(delitos_mcs = sum(delitos_mcs), 
            poblacion = first(población)) |> 
  ungroup() |> 
  mutate(tasa = (delitos_mcs / poblacion) * 1000)

write.csv2(datos_delinc_mcs_tasa, "datos/delincuencia_mcs_tasa.csv")




# población ----

# población por años
datos_poblac_años <- datos_poblac |>
  pivot_wider(names_from = año, values_from = población, names_prefix = "poblacion_")

# datos_poblac_2024 <- datos_poblac |> 
#   filter(año == 2024)
# 
# datos_poblac_2017 <- datos_poblac |> 
#   filter(año == 2017)
# 
# datos_poblac_2030 <- datos_poblac |> 
#   filter(año == 2030)
  
write.csv2(datos_poblac, "datos/poblacion_chile_comunas.csv")

# crecimiento poblacional
datos_poblac_crecim <- datos_poblac_años |>
  mutate(pob_crecimiento_5a = poblacion_2024/poblacion_2019,
         pob_crecimiento_10a = poblacion_2024/poblacion_2013,
         pob_crecimiento_20a = poblacion_2024/poblacion_2003) |>
  mutate(across(starts_with("pob_"), ~.x - 1)) |> 
  select(1:6, matches("crecimiento"))

write.csv2(datos_poblac_crecim, "app/datos/poblacion_chile_comunas_crecimiento.csv")




# casen ----

# # casen viene sin cut_comunas
# casen <- read.csv2("datos/casen_comunas.csv") |> 
#   tibble() |> 
#   select(-region) |> 
#   left_join(cut_comunas, by = "comuna") |> 
#   relocate(cut_comuna, .after = comuna)
# 
# write.csv2(casen, "datos/casen_comunas.csv")

# variables_casen <- list(
#   "Viviendas" = c(
#     #"Hogares con jefatura femenina" = "hogar_jefatura_femenina_p",
#     "Hogares en situación de hacinamiento (4 personas o más)" = "hacinamiento_p",
#     "Hogares con personas menores de 18 años" = "men18c_p",
#     "Hogares con personas mayores de 60 años" = "may60c_p",
#     "Viviendas propias" = "vivienda_propia_p",
#     "Viviendas arrendadas" = "vivienda_arrendada_p",
#     "Metros cuadrados aproximados de la vivienda" = "v12mt", #ojo con esta que es numérica
#     "Viviendas pequeñas (30m cuadrados o menos)" = "vivienda_pequeña_p",
#     "Hogares en sectores en mal estado" = "sector_malo_p",
#     "Hogares en sectores con mucho daño deliberado a la propiedad" = "sector_dañado_p",
#     "Hogares en zonas rurales" = "rural_p",
#     "Viviendas en mal estado" = "carencia_estado_vivienda_p",
#     "Viviendas carentes de servicios básicos" = "carencia_servicios_basicos_p",
#     "Hogares con ingresos percapita menores a la mediana ($450.000)" = "ingreso_percapita_hogar_menor_mediana_p",
#     "Hogares con ingresos totales menores a la mediana ($1.110.000)" = "ingreso_total_hogar_menor_mediana_p",
#     "Hogares con ingresos producto del trabajo menores a la mediana ($700.000)" = "ingreso_trabajo_hogar_menor_mediana_p"
#   ),
#   
#   #numéricas
#   "Ingresos de las personas" = c(
#     "Ingresos promedio" = "ytotcor",
#     "Ingreso ocupación principal" = "yoprcor",
#     "Años de escolaridad promedio" = "esc",
#     "Edad promedio" = "edad",
#     "Ingreso del trabajo" = "ytrabajocor",
#     "Jubilación o pensión de vejez" = "y2803",
#     "Decil autónomo regional" = "dautr",
#     "Quintil autónomo regional" = "qautr"
#   ),
#   
#   "Ingresos de la población" = c(
#     "Ingresos personales por ocupación principal menores a la mediana ($500.000)" = "ingreso_ocup_princ_menor_mediana_p",
#     "Ingresos personales por ocupación principal menores a $1.000.000" = "ingreso_ocup_princ_menor_2medianas_p",
#     "Ingresos personales producto del trabajo menores a la mediana ($500.000)" = "ingreso_ytrabajocor_menor_mediana_p",
#     "Ingresos personales producto del trabajo menores a $1.000.000" = "ingreso_ytrabajocor_menor_2medianas_p"
#   ),
#   
#   #numéricas
#   "Ingresos de los hogares" = c(
#     "Ingresos promedio de los hogares" = "ytotcorh",
#     "Ingreso del trabajo del hogar" = "ytrabajocorh",
#     "Ingreso autónomo per cápita" = "ypchautcor",
#     "Numero de personas en el hogar" = "numper",
#     "Ingreso total per cápita del hogar" = "ypc"
#   ),
#   
#   "Trabajo" = c(
#     "Trabajadores independientes" = "independientes_p",
#     "Personas en situación laboral inactiva" = "inactivos_p",
#     "Personas en situación laboral desocupada" = "desocupados_p",
#     "Trabaja como familiar no remunerado" = "trabajo_familiar_no_remunerado_p",
#     "Trabaja en servicio doméstico puertas adentro o afuera" = "trabajo_servicio_domestico_p",
#     "Trabaja como empleador" = "trabajo_empleador_p",
#     "Trabajadores del hogar o servicio doméstico" = "trabajo_domestico_p"
#   ),
#   
#   "Pensiones" = c(
#     "Jubilación o pensión menor o igual a la mediana ($230.000)" = "pension_menor_mediana_p",
#     "Jubilación o pensión menor o igual al salario mínimo ($500.000)" = "pension_menor_salario_minimo_p"
#   ),
#   
#   "Educación" = c(
#     "Personas con estudios superiores (técnico o profesional)" = "estudios_superiores_p",
#     "Nivel educacional máximo: básica o menor" = "estudios_basica_o_menos_p",
#     "Nivel educacional máximo: media incompleta o menor" = "estudios_sin_media_o_menos_p",
#     "Nivel educacional máximo: estudios superiores incompletos" = "estudios_superiores_incompletos_p"
#   ),
#   
#   "Pobreza" = c(
#     "Personas en situación de pobreza" = "pobreza_p",
#     "Personas en situación de pobreza multidimensional" = "pobreza_multi_p"
#   ),
#   
#   "Demografía" = c(
#     "Personas pertenecientes a pueblos originarios" = "originario_p",
#     "Personas de origen extranjero" = "extranjero_p"
#   ),
#   
#   "Salud" = c(
#     "Afiliados a previsión de salud Fonasa" = "fonasa_p",
#     "Afiliados a previsión de salud Isapre" = "isapre_p"
#   )
# )
# 
# variables_casen |> 
#   unname() |> 
#   unlist() |> 
#   tibble::enframe() |> 
#   mutate(value = paste0("casen_", value)) |> 
#   View()

