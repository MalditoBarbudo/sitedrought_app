library(tidyverse)

source('data-raw/translations.R')
source('data-raw/polygon_objects_creation.R')
source('data-raw/palette_builder.R')

usethis::use_data(
  # polygons needed
  all_polygons, all_parques, provincias, catalunya, parques, ordesa, aiguestortes, peri_total,
  peri_ordesa, peri_aiguestortes, ordesa_big, aiguestortes_big, parks_big,
  # thesuruses
  palettes_dictionary, language_dictionary, sitedrought_var_thes,
  # options
  internal = TRUE, overwrite = TRUE
)
