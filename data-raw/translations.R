## Script for creating the translations


tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,

  # ........ SITEDROUGHT .........
  # ..............................
  
  #       .) Creo los TABS de la 1ra y 2nd PESTAÑA
  #       .) Seran de IGUAL valor (cat,eng,spa)
  #       .) Y diferente ID
  
  #       .) Diferentes LABELS de la APP
  #       .) Son los ID de la función TRANSLATE_APP
  
  
  # .......... VARIABLES .........
  # ..............................
  
  "Precipitation", "Precipitació (mm/dia)", "Precipitation (mm/day)", "Precipitación (mm/día)",
  "PET", "Evapo-transpiració potencial (mm/dia)", "Potential evapo-transpiration (mm/day)", "Evap-transpiración potencial (mm/día)",
  
  "REW", "Aigua disponible sòl (%)", "Available soil water (%)", "Agua disponible suelo (%)",
  "REW_q", "Percentil aigua disponible sòl (%)", "Percentile available soil water (%)", "Percentil agua disponible suelo (%)",
  
  "DDS", "Estrés de la vegetació (%)", "Vegetation stress (%)", "Estrés de la vegetación (%)",
  "DDS_q", "Percentil estrés de la vegetació  (%)", "Percentile vegetation stress (%)", "Percentil estrés de la vegetación (%)",
  
  "LFMC", "Contingut d’humitat de combustible viu (%)", "Live fuel moisture content (%)","Contendio de humedad de combustible vivo (%)",
  "LFMC_q", "Percentil contingut d’humitat de combustible viu (%)", "Percentile live fuel moisture content (%)","Percentil contendio de humedad de combustible vivo (%)",
  
  "DFMC", "Contingut d’humitat de combustible mort (%)", "Dead fuel moisture content (%)","Contendio de humedad de combustible muerto (%)",
  "SFP","Potencial foc de superfície [0-9]","Surface fire potential [0-9]","Potencial fuego de superficie [0-9]",
  "CFP","Potencial foc de capçada [0-9]","Crown fire potential [0-9]","Potencial fuego de copa [0-9]",
  
 
  
  # .......... VARIABLES SHORT .........
  # ..............................
  
  # "short_Precipitation", "Precipitació", "Precipitation", "Precipitación ",
  # "short_PET", "Evapo-transpiració", "Evapo-transpiration", "Evap-transpiración potencial (mm/día)",
  # 
  # "short_Percentile", "Percentil Històric", "Historical Percentile", "Percentil Histórico",
  # "short_REW", "Aigua disponible", "Available soil", "Agua disponible",
  # "short_DDS", "Estrés vegetació", "Stress vegetation", "Estrés vegetación",
  # "short_LFMC", "H.Combustible Viu", "L.Fuel Moisture","H.Combustible Vivo",
  # 
  # "short_DFMC", "H.Combustible Mort", "D.Fuel Moisture","H. Combustible Muerto",
  # "short_SFP","P.Foc Superfície","P.Surface Fire","P.Fuego Superficie",
  # "short_CFP","P.Foc Capçada","P.Crown Fire","P.Fuego Copa",
  
  "short_Precipitation", "Precip.", "Precip.", "Precip. ",
  "short_PET", "E-T", "E-T", "E-T",
  
  "short_Percentile", "P.Històric", "Historical.P", "P.Histórico",
  "short_REW", "Aigua dis.", "Avai.water", "Agua dis.",
  "short_DDS", "Estrés veg.", "Vegetation Stres.", "Estrés veg.",
  "short_LFMC", "H.C.Viu", "L.F.Content","H.C.Vivo",
  
  "short_DFMC", "H.C.Mort", "D.F.Content","H.C.Muerto",
  "short_SFP","P.Foc.Sup.","Surf.Fire.P","P.Fuego.Sup.",
  "short_CFP","P.Foc.Cap.","Crown.Fire.P","P.Fuego.Copa",


  # ....... COMBO VARIABLES ......
  # ..............................
  

  'climate variables', "Variables Climàtiques", 'Climatic Variables', "Variables Climaticas",
  'drought variables','Indicadors Sequera','Drought Indicators','Indicadores Sequía',
  'fire variables',"Indicadors Risc Incendi", "Fire Risk Indicators", "Indicadores Riesgo Incendio",
  'Percentiles variables','Comparació Històrica (1981 - 2020)','Historical Comparison (1981 - 2020)','Comparación Histórica (1981 - 2020)',

  # ........... TÍTULOS ..........
  # ..............................
  
  "plot_origin_label", 'Conjunt de Parcel·les', 'Set of Plots', 'Conjunto de Parcelas',
  "var_daily_label", 'Tria la variable', 'Choose variable', 'Elige la variable',
  "main_tab_translation", "Explora Parceles", "Plots Explore", "Explora Parcelas",
  "data_translation", "Dades", "Data", "Datos",
  "map_translation", "Mapa", "Map", "Mapa",
  "save_translation", "Guardar", "Save", "Guardar",
  'help_translation','Ajuda','Help','Ayuda',
  "tech_specs_translation", "Especificacions tècniques", "Technical specifications", "Especificaciones técnicas",
  "date_daily_label", 'Data', 'Date', 'Fecha',
  'series_tab_translation', "Sèries temporals", "Time series", "Series temporales",
  "type_legend_label", "Configurar paleta","Config palette", "Configurar paleta",
  
  'percentiles_axis_label','Percentils Comparació Històrica (1981 - 2020)','Percentiles Historical Comparison (1981 - 2020)','Percentiles Comparación Histórica (1981 - 2020)',
  "save_main_label","Selecciona format per descargar les dades","Select format for downloading data","Selecciona formato para descargas los datos",
  "save_map_button","Guardar el mapa", "Save the map", "Guardar el mapa", 
  "save_table_button", "Guardar la taula","Save the table", "Guardar la tabla", 
  'help_description','Descripció : ','Description : ','Descripción : ',
  'units_description','Unitats : ','Units : ','Unidades : ',
  # 'reverse_legend','Invertir Llegenda', 'Reverse Legend','Invertir Leyenda',
  'reverse_legend', "Invertir la paleta?","Reverse the palette?", "¿Invertir la paleta?", 
  
  # ........... SAVE .............
  # ..............................
  
  "col_vis",  "Columnes visibles","Visible columns", "Columnas visibles",
  "col_all", "Totes les columnes","All columns", "Todas las columnas", 
  
  "data_colum_label","¿Totes les dades o només les columnes visibles?", "All data or visible data?", "¿Todos los datos o solo las columnas visibles?", 
  "select_format_label", "Selecciona el format","Choose the output format", "Selecciona el formato", 
  "csv", "Text (csv)","Text (csv)", "Texto (csv)", 
  "xlsx", "MS Excel (xlsx)","MS Excel (xlsx)", "MS Excel (xlsx)",
  
  
  # ....... SCREEN WAITING .......
  # ..............................
  
  "progress_plots", "Obtenció de dades", "Obtaining data", "Obteniendo datos",
  "progress_detail_plots", "Això pot trigar una mica", "This may take some time", "Esto puede llevar algo de tiempo",
  
  "progress_detail_raster", "Això pot trigar una mica", "This may take some time", "Esto puede llevar algo de tiempo",
  "progress_ts", "Càlcul de les sèries temporals", "Calculating the time series", "Calculando las series temporales",
  "progress_detail_ts", "Això pot trigar una mica, en funció del nombre i / o la mida dels objectes espacials", "This may take some time, depending on the number and/or size of the spatial objects", "Esto puede llevar algún tiempo, dependiendo de número y/o tamaño de los objetos espaciales",
  
  
  # ........ PLOT ORIGEN .........
  # ..............................

  "PN", "Parcs Nacionals","Nationals Parks","Parques Nacionales",
  "P", "IFN 4", "NFI 4" ,"IFN 4",
  "A", "Parc Nacional d'Aigüestortes i Estany de Sant Maurici","Aigüestortes i Estany de Sant Maurici National Park","Parque Nacional de Aigüestortes i Estany de Sant Maurici",
  "S", "Matollar","Scrubland","Matorral",
  "T", "Totes les Parcel·les","All Plots","Todas las Parcelas",
  'O',"Parc Nacional d'Ordesa y Monte Perdido","Ordesa y Monte Perdido National Park","Parque Nacional Ordesa y Monte Perdido",

  # ....... TIPUS LLEGENDA .......
  # ..............................
  
  "estandard_label","Estàndar","Standard","Estándar",
  "1st_label", "Tipus A", "Type A" ,"Tipo A", 
  "2nd_label", "Tipus B", "Type B" ,"Tipo B",
  
 
  
) %>%
  {.} -> language_dictionary  



# usethis::use_data(
#   language_dictionary,
#   
#   internal = TRUE, overwrite = TRUE
# )
