# script to create the list for building palettes
palettes_dictionary <- list(
  # drought vars
  REW = list(min = 0, max = 100, pal = viridis::mako, rev = TRUE),
  DDS = list(min = 0, max = 100, pal = viridis::rocket, rev = FALSE),
  # climate
  PET = list(min = 0, max = 15, pal = viridis::cividis, rev = TRUE),
  Precipitation = list(min = 0, max = 100, pal = viridis::cividis, rev = TRUE),
  # fire
  LFMC = list(min = 0, max = 100, pal = viridis::mako, rev = TRUE),
  DFMC = list(min = 0, max = 100, pal = viridis::mako, rev = TRUE),
  SFP = list(min = 0, max = 9, pal = viridis::rocket, rev = FALSE),
  CFP = list(min = 0, max = 9, pal = viridis::rocket, rev = FALSE),
  # quantiles
  REW_q = list(min = 0, max = 100, pal = viridis::mako, rev = TRUE),
  DDS_q = list(min = 0, max = 100, pal = viridis::rocket, rev = FALSE),
  LFMC_q = list(min = 0, max = 100, pal = viridis::mako, rev = TRUE)
)
