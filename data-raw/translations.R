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
  
  "plot_origin_label", 'Conjunt de parcel·les', 'Set of plots', 'Conjunto de parcelas',
  "var_daily_label", 'Tria la variable', 'Choose variable', 'Elige la variable',
  "main_tab_translation", "Explorador de parcel·les", "Plot explorer", "Explorador de parcelas",
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
  "save_table_button", "Descarrega","Download", "Descarga", 
  'help_description','Descripció : ','Description : ','Descripción : ',
  'units_description','Unitats : ','Units : ','Unidades : ',
  'reverse_legend', "Invertir la paleta?","Reverse the palette?", "¿Invertir la paleta?", 
  
  # ........... SAVE .............
  # ..............................
  
  "col_vis",  "Columnes visibles","Visible columns", "Columnas visibles",
  "col_all", "Totes les columnes","All columns", "Todas las columnas", 
  
  "data_colum_label","¿Totes les dades o només les columnes visibles?", "All data or visible data?", "¿Todos los datos o solo las columnas visibles?", 
  "select_format_label", "Selecciona el format","Choose the output format", "Selecciona el formato", 
  "select_by_date","Rang de dates","Date range","Rango de fechas",
  "csv", "Text (csv)","Text (csv)", "Texto (csv)", 
  # "xlsx", "MS Excel (xlsx)","MS Excel (xlsx)", "MS Excel (xlsx)",
  "gpkg", "Espacial (gpkg)","Spatial (gpkg)", "Espacial (gpkg)",
  "ddbb","Any natural","Calendar year","Año natural",
  "day","Dia seleccionat","Day selected","Día seleccionado",
  
  
  # ....... SCREEN WAITING .......
  # ..............................
  
  "progress_ddbb", "Descargant tota la base de dades", "Downloading the complete database", "Descargando toda la base de datos",
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
  "S", "Matollar","Shrubland","Matorral",
  "T", "Totes les parcel·les","All plots","Todas las parcelas",
  'O',"Parc Nacional d'Ordesa y Monte Perdido","Ordesa y Monte Perdido National Park","Parque Nacional Ordesa y Monte Perdido",

  # ....... TIPUS LLEGENDA .......
  # ..............................
  
  "estandard_label","Estàndar","Standard","Estándar",
  "1st_label", "Tipus A", "Type A" ,"Tipo A", 
  "2nd_label", "Tipus B", "Type B" ,"Tipo B",
  
  # ........ MAPAS FONDO .........
  # ..............................
  
  "Relief","Relleu","Relief","Relieve",
  "Imagery","Satèl·lit","Satellite","Satélite",
  "OSM","OSM","OSM","OSM",
  
 
  
) %>%
  {.} -> language_dictionary  


tibble::tribble(
  ~num, ~var_id, ~var_table, ~var_description_cat, ~var_description_eng, ~var_description_spa, ~var_description_help_cat, ~var_description_help_eng, ~var_description_help_spa, ~var_units_cat, ~var_units_eng, ~var_units_spa,
  
  "1","REW","data_day","Aigua disponible sòl","Available soil water","Agua disponible suelo","Percentatge d’aigua disponible al sòl, en relació al total d’aigua que pot ser extreta per les plantes.","Percentage of available water in the soil, in relation to the total water that can be extracted by plants.","Porcentaje de agua disponible en el suelo, en relación al total de agua que puede ser extraída por las plantas.","(%)","(%)","(%)",
  "2","DDS","data_day","Estrés de la vegetació","Vegetation Stress","Estrés de la vegetación","Índex d’estrés de la vegetació, que mesura el grau de tancament dels estomes de les plantes al bosc.","Vegetation stress index, which measures the degree of stomata closure of plants in the forest.","Índice de estrés de la vegetación, que mide el grado de cierre de los estomas de las plantas en el bosque.","(%)","(%)","(%)",
  "3","PET","data_day","Evapo-transpiració potencial","Potential evapo-transpiration","Evap-transpiración potencial","Demanda evaporativa potencial de l’atmosfera, mesurada mitjançant l’index d’evapo-transpiració potencial de Penman.","Potential evaporative demand of the atmosphere, measured by Penman's potential evapotranspiration index.","Demanda evaporativa potencial de la atmósfera, medida mediante el índice de evapo-transpiración potencial de Penman.","(mm/dia)","(mm/day)","(mm/día)",
  "4","Precipitation","data_day","Precipitació","Precipitation","Precipitación","Precipitació diària, en forma de pluja o neu.","Daily precipitation, in the form of rain or snow.","Precipitación diaria, en forma de lluvia o nieve.","(mm/dia)","(mm/day)","(mm/día)",
  "5","LFMC","data_day","Contingut d’Humitat de Combustible Viu","Live Fuel Moisture Content","Contendio de Humedad de Combustible Vivo","Contingut d’aigua del combustible fi viu (fulles i branquillons), en relació al seu pes sec.","Water content of live fine fuel (leaves and twigs), in relation to their dry weight.","Contenido de agua del combustible fino vivo (hojas y ramitas), en relación a su peso seco.","(%)","(%)","(%)",
  "6","DFMC","data_day","Contingut d’Humitat de Combustible Mort","Dead Fuel Moisture Content","Contendio de Humedad de Combustible Muerto","Contingut d’aigua del combustible fi mort (fulles i branquillons), en relació al seu pes sec.","Water content of dead fine fuel (leaves and twigs), in relation to their dry weight.","Contenido de agua del combustible fino muerto (hojas y ramitas), en relación a su peso seco.","(%)","(%)","(%)",
  "7","SFP","data_day","Potencial Foc de Superfície","Surface Fire Potential","Potencial Fuego de Superficie","Índex de comportament potencial del foc de superfície, segons el sistema Fuel Characteristics Classification System (FCCS). ","Surface fire potential behavior index, according to the Fuel Characteristics Classification System (FCCS).","Índice de comportamiento potencial del fuego de superficie, según el sistema Fuel Characteristics Classification System (FCCS).","[0-9]","[0-9]","[0-9]",
  "8","CFP","data_day","Potencial Foc de Capçada","Crown Fire Potential","Potencial Fuego de Copa","Índex de comportament potencial del foc de capçada, segons el sistema Fuel Characteristics Classification System (FCCS). ","Crown fire potential  behavior index, according to the Fuel Characteristics Classification System (FCCS).","Índice de comportamiento potencial del fuego de copa, según el sistema Fuel Characteristics Classification System (FCCS).","[0-9]","[0-9]","[0-9]",
  "9","REW_q","data_day","Percentil Aigua disponible sòl","Percentile Available soil water","Percentil Agua disponible suelo","Percentil d’aigua disponible al sòl, en relació a la distribució de valors de la sèrie històrica (1981-2020).","Percentage of available water in the soil, in relation to the distribution of values of the historical series (1981-2020).","Percentil de agua disponible en el suelo, en relación a la distribución de valores de la serie histórica (1981-2020).","(%)","(%)","(%)",
  "10","DDS_q","data_day","Percentil Estrés de la vegetació ","Percentile Stress on vegetation","Percentil Estrés de la vegetación","Percentil d’estrés de la vegetació, en relació a la distribució de valors de la sèrie històrica (1981-2020).","Vegetation stress percentile, in relation to the distribution of values of the historical series (1981-2020).","Percentil de estrés de la vegetación, en relación con la distribución de valores de la serie histórica (1981-2020).","(%)","(%)","(%)",
  "11","LFMC_q","data_day","Percentil Contingut d’Humitat de Combustible Viu","Percentile Live Fuel Moisture Content","Percentil Contendio de Humedad de Combustible Vivo","Percentil de contingut d’humitat del combustible viu, en relació a la distribució de valors de la sèrie històrica (1981-2020).","Percentage of moisture content of live fuel, in relation to the distribution of values of the historical series (1981-2020).","Percentil de contenido de humedad del combustible vivo, en relación a la distribución de valores de la serie histórica (1981-2020).","(%)","(%)","(%)",
  
) %>%
  {.} -> sitedrought_var_thes 


# usethis::use_data(
#   language_dictionary,
#   
#   internal = TRUE, overwrite = TRUE
# )
