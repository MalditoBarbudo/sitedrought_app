### polygons creation script
# there is a problem here. I don't have the original code to create the all polygons file
# because Oleguer did it on QGIS.

all_polygons <- sf::st_read("data-raw/shapefiles/all_polygons.shp") |>
  # rmapshaper::ms_simplify(0.2) |>
  sf::st_transform(4326)

all_parques <- sf::st_read("data-raw/shapefiles/all_polygons.shp") |>
  dplyr::filter(Tipus == "OR" | Tipus == "AT" | Tipus == "peri_OR" | Tipus == "peri_AT" ) |>
  # rmapshaper::ms_simplify(0.5) |>
  sf::st_transform(4326)


#  PARQUES / PROVINCIAS
provincias <- all_polygons |> dplyr::filter(Tipus == "PROV") |>
  sf::st_make_valid()
catalunya <- all_polygons |> dplyr::filter(Cod_CCAA == "09") |>
  sf::st_make_valid()

parques <- all_parques |> dplyr::filter(Tipus == "OR" | Tipus == "AT") |>
  sf::st_make_valid()
ordesa <- all_parques |> dplyr::filter(Tipus == "OR") |>
  sf::st_make_valid()
aiguestortes <- all_parques |> dplyr::filter(Tipus == "AT") |>
  sf::st_make_valid()

# PERIMETRE
peri_total <- all_parques  |> dplyr::filter(Tipus %in% c("peri_OR", "peri_AT")) |>
  sf::st_make_valid()
peri_ordesa <- all_parques  |> dplyr::filter(Tipus == "peri_OR") |>
  sf::st_make_valid()
peri_aiguestortes <- all_parques  |> dplyr::filter(Tipus == "peri_AT") |>
  sf::st_make_valid()

ordesa_big <- sf::st_combine(rbind(ordesa, peri_ordesa)) |>
  sf::st_make_valid() |>
  sf::st_union(is_coverage = TRUE) |>
  sf::st_as_sf()

aiguestortes_big <- sf::st_combine(rbind(aiguestortes, peri_aiguestortes)) |>
  sf::st_make_valid() |>
  sf::st_union(is_coverage = TRUE) |>
  sf::st_as_sf()

parks_big <- sf::st_combine(rbind(parques, peri_total)) |>
  sf::st_make_valid() |>
  sf::st_union(is_coverage = TRUE) |>
  sf::st_as_sf()
