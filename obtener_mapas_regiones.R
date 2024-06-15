library(dplyr)
library(ggplot2)
library(sf)
library(chilemapas)

# mapas de regiones ----
mapas_regiones <- chilemapas::mapa_comunas |> 
  left_join(chilemapas::codigos_territoriales |> 
              select(matches("comuna")), 
            by = "codigo_comuna") |> 
  mutate(codigo_region = as.numeric(codigo_region)) |> 
  filter(!codigo_comuna %in% c("05104", "05201")) #excluir islas

# separar en una lista por región
mapas_regiones_split <- mapas_regiones |>
  group_by(codigo_region) |> 
  group_split()


# correcciones manuales ----
library(smoothr)

# eliminar islas muy chicas
mapas_regiones_split[[5]] <- mapas_regiones_split[[5]] |> 
  mutate(geometry = drop_crumbs(geometry, threshold = units::set_units(30, km^2)))

# eliminar islas muy chicas
mapas_regiones_split[[12]] <- mapas_regiones_split[[12]] |>
  mutate(geometry = drop_crumbs(geometry, threshold = units::set_units(100, km^2)))


# guardar ----
saveRDS(mapas_regiones_split, "mapas/mapas_regiones.rds", compress = FALSE)



# revisar regiones con islas ----
# mapas_regiones_split[[5]] |> 
#   # filter(codigo_comuna == "05101") |> 
#   # eliminar islas muy chicas
#   mutate(geometry = drop_crumbs(geometry, threshold = units::set_units(30, km^2))) |>
#   ggplot(aes(geometry = geometry)) +
#   geom_sf()
# 
# # area_thresh <- units::set_units(200, km^2)
# # p_dropped <- drop_crumbs(p, threshold = area_thresh)
# 
# mapas_regiones_split[[5]] |> 
#   filter(codigo_comuna != "05101") |> 
#   print(n=Inf) |> 
#   ggplot(aes(geometry = geometry)) +
#   geom_sf(aes(fill = codigo_comuna)) +
#   geom_sf_text(aes(label = codigo_comuna), check_overlap = F) +
#   coord_sf(clip = "off") +
#   theme(legend.position = "bottom")


# mapas_regiones_split[[12]] |>
#   # eliminar islas muy chicas
#   mutate(geometry = drop_crumbs(geometry, threshold = units::set_units(100, km^2))) |>
#   ggplot(aes(geometry = geometry)) +
#   geom_sf()
