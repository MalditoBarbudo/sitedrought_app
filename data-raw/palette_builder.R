# script to create the list for building palettes
palettes_dictionary <- list(
  # drought vars
  REW = list(min = 0, max = 100, pal = viridis::mako, rev = FALSE),
  DDS = list(min = 0, max = 100, pal = viridis::rocket, rev = FALSE),
  # climate
  PET = list(min = 0, max = 15, pal = viridis::cividis, rev = TRUE),
  Precipitation = list(min = 0, max = 100, pal = viridis::cividis, rev = TRUE),
  # fire
  LFMC = list(min = 0, max = 100, pal = viridis::mako, rev = FALSE),
  DFMC = list(min = 0, max = 100, pal = viridis::mako, rev = FALSE),
  SFP = list(min = 0, max = 9, pal = viridis::rocket, rev = FALSE),
  CFP = list(min = 0, max = 9, pal = viridis::rocket, rev = FALSE),
  # quantiles
  REW_q = list(min = 0, max = 100, pal = viridis::mako, rev = FALSE),
  DDS_q = list(min = 0, max = 100, pal = viridis::rocket, rev = FALSE),
  LFMC_q = list(min = 0, max = 100, pal = viridis::mako, rev = FALSE)
)


# palettes_dictionary <- list(   
#   
#   PET = list(pal = viridis::viridis(100) ),
#   Precipitation = list(pal = viridis::cividis(100)),
#   REW = list(pal = viridis::plasma(100)),
#   DDS = list(max = 100 , min = 0,pal = viridis::inferno(100)),
#   
#   LFMC = list(pal = viridis::inferno(100)),
#   DFMC = list(pal = viridis::inferno(100)),
#   SFP = list(pal = c("#b50f04","red","orange","#e7f734","green")),
#   CFP = list(pal = c("#b50f04","red","orange","#e7f734","green")),
#   
#   REW_q = list(max = 10 , min = 0,pal = "RdYlBu"),
#   DDS_q = list(max = 10 , min = 0,pal = c("#0f0396","#4e57fc","#949af7","white","#f7979f","#f54c4c","#960303")),
#   LFMC_q = list(max = 10 , min = 0,pal = "RdYlBu")
# 
# 
# )

# usethis::use_data(
#   palettes_dictionary,
#   
#   internal = TRUE, overwrite = TRUE
# )


 