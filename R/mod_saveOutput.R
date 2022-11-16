#' @title mod_saveOutput and siteDrought_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_saveOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_save_container')
    )
  )
}

#' siteDrought_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @export
mod_save <- function(
  input, output, session,
  main_data_reactives, data_reactives, lang
   
) {

  # renderUI ####
  output$mod_save_container <- shiny::renderUI({
    
    
    # ......... INICIALIZAR .............
    # ...................................
    
    #       .) NS = IDs únicos
    #       .) LANG = F(x) definida en APP.R
    #       .) DATES_LANG = Cambio de nomenclatura de lengua
    
    ns <- session$ns
    lang_declared <- lang()
    dates_lang <- switch(
      lang_declared,
      'cat' = 'ca',
      'spa' = 'es',
      'eng' = 'en'              
    )
    
    
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # -----------------------------    RENDER UI   ---------------------------------
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    # ........ ESTRUCTURA HTML5 .........
    # ...................................
    
    #       .) TAGLIST crea una definición de etiqueta HTML
    #       .) Creamos los elementos HTML5 con TAGS
    #       .) DROPDOWNS (SelectIntpu),...
     
    shiny::tagList(
      
        
        shiny::fluidRow(translate_app("save_main_label", lang_declared), style = "text-align: center;"),  
        shiny::br(),
        shiny::fluidRow(
          shiny::fluidRow( align = 'center',
                           
              # ....... DOWNLOAD BUTTON ...........
              # ...................................
              
              #       .) Button que DESCARGA los datos
              #       .) Puede ser en formato CSV/XLSX/GPKG
              #       .) Puede tener TODAS COLUMNAS VARIABLES / UN COLUMNA VARIABLE              
              
              shiny::downloadButton(ns("table_save"), translate_app("save_table_button",lang_declared)),
          ),
          
          shiny::br(),
          shiny::fluidRow( align = 'center',
              
              # ........ COLUMNS BUTTONS ..........
              # ...................................
              
              #       .) Botones selección COLUMNAS
              #       .) Dos tipos = TODAS COLUMNAS VARIABLE / UNA COLUMNA VARIABLE
              
              
              shinyWidgets::prettyRadioButtons(
                ns("data_columns"),translate_app("data_colum_label", lang_declared),
                shiny_set_names(c("col_vis" = "col_vis",
                                  "col_all" = "col_all"),lang_declared),
                status = 'success', fill = TRUE, shape = 'round'
                
              ),
              
              # ......... FORMAT BUTTONS ..........
              # ...................................
              
              #       .) Botones selección FORMATO
              #       .) Tres tipos = CSV / XLSX / GPKG
              
              shinyWidgets::prettyRadioButtons(
                ns("data_format"),translate_app("select_format_label", lang_declared),
                shiny_set_names(c("csv"  = "csv",
                                  "xlsx" = "xlsx",
                                  "gpkg" = "gpkg"),lang_declared),
                status = 'success', fill = TRUE, shape = 'round'
              ),
              
              # ......... RANGE BUTTONS ...........
              # ...................................
              
              #       .) Botones selección RANGO DATOS
              #       .) Dos tipos = ALL DATABASE / ONE DAY
              
              shinyWidgets::prettyRadioButtons(
                ns("day_table"),translate_app("select_day_table", lang_declared),
                shiny_set_names(c("day"  = "day",
                                  "ddbb" = "ddbb"),lang_declared),
                status = 'success', fill = TRUE, shape = 'round'
              )

          ) # end fluid row
        )   # end fluid row  
      )     # end of tagList

  })  
  
  

  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # -----------------------------   DOWNLOAD  -----------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  #      .) Los botones DOWNLOAD funciona con 2 ESTRUCTURAS

  #               .) REACTIVE         = Nos da los datos a descargar
  #               .) DOWNLOADHANDLER  = Función de descargar
  
  #      .) Usaremos 2 Reactives para obtener datos
  
  #               .) REACTIVE Database ONE DAY =
  #               .) REACTIVE all Database     = 


  
  # .......... DATABASE ONLY ONE DAY .............
  # ..............................................
  
  #      .) REACTIVE para DESCARGA de SOLO UN DÍA
  #      .) CREAMOS en el data set 4 columnas
  #               .) lat/long (ETRS89)
  #               .) lat/long (WGS84)
  
  
  #      .) PRIMER IF  
  #               .) con TODAS COLUMNAS
  #               .) con SOLO COLUMNA VARIABLE SELECCIONADA
  
  #      .) SEGUNDO IF  
  #               .) Si NO es GPKG
  #                  .) Eliminamos columna GEOMETRY
  #                  .) Ya que en el EXCEL y CSV no aporta info
  #               .) Si es GPKG
  #                  .) Manetnemos columna GEOMETRY
  #                  .) Es necesaria para el GPKG
  

  datasetInput <- reactive({
    
    fecha <- data_reactives$fecha_reactive
    sf <- main_data_reactives$data_day  

    variable <- data_reactives$variable_reactive
    num_i <- as.numeric(match(variable,names(sf)))
    selected_var <- as.symbol(names(sf)[num_i])
    
    
    
    if( input$data_columns == "col_all" ) {
      sf_data_columns <- sf  %>%
        dplyr::filter(date == fecha) %>%
        dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
                      lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
                      lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
                      lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
        )
    } else if (input$data_columns == "col_vis") {
      sf_data_columns <- sf %>%
        dplyr::filter(date == fecha) %>%
        dplyr::select(plot_id, selected_var, date, plot_origin) %>%  
        dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
                      lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
                      lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
                      lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
        )
    } 
    
    if(input$data_format == "gpkg"){
      sf_data_columns %>% data.frame()
    } else {
      sf_data_columns %>% data.frame() %>% dplyr::select(-geometry)
    }

    
  })
  
  
  # ..............  DOWNLOAD all DATABASE .............
  # ...................................................
  
  #      .) Función para download TODA la BBDD
  
  #      .) PRIMER IF 
  #               .) Tanto si seleccionamos ALL Colums / Columna VISIBLE
  #               .) Se descarga TODA la BBDD
  #               .) Creamos 4 columna Lat / Lon
  
  #      .) SEGUNDO IF
  #               .) Si NO es GPKG
  #                  .) Eliminamos columna GEOMETRY
  #                  .) Ya que en el EXCEL y CSV no aporta info
  #               .) Si es GPKG
  #                  .) Manetnemos columna GEOMETRY
  #                  .) Es necesaria para el GPKG
  
  databaseInput <- reactive({
    
    sf <- main_data_reactives$data_day

    if( input$data_columns == "col_all" | input$data_columns == "col_vis" ) {
      sf_data <- sf  %>%
        dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
                      lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
                      lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
                      lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
        )
    } 


    if(input$data_format == "gpkg"){
      sf_data %>% data.frame()
    } else {
      sf_data %>% data.frame() %>% dplyr::select(-geometry)
    }
    
    
  })
  
  
  # %%%%%%%%%%%%%%%%  DOWNLOADHANDLER  %%%%%%%%%%%%%%%%%
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #      .) NECESSITA
  #           .) FILENAME Function  => crea ek nombre de archivo
  #           .) CONTENT  Function  => f(x) DOWNLOAD
  
  #      .) FILENAME
  #           .) Crearemos 3 f(x) que nos ayudaran
  #              .) FORMAT_SELECTED   
  #              .) COLUMN_SELECTED   
  #              .) DATE_STR
  

  
  
  # ............... f(x) FROMATO .................
  # ..............................................
  
  #      .) Función que asigna EXTENCIÓN del archivo
  #      .) CSV o XLSX
  
  format_selected <- function() {
    switch (input$data_format,
            "csv" = format <- ".csv", 
            "xlsx" = format <- ".xlsx",
            "gpkg" = format <- ".gpkg"
    )
  }
  
  # ............... f(x) COLUMNA .................
  # ..............................................
  
  #      .) Función que asigna parte del nombre
  #      .) Del archivo a descargar
  
  column_selected <- function() {
    
    if(input$data_format == "gpkg") {
      
      switch (input$data_columns,
              "col_vis" = format <- "_map_specific_columns", 
              "col_all" = format <- "_map_all_columns" 
      )
      
    } else {
      
      switch (input$data_columns,
              "col_vis" = format <- "_specific_columns", 
              "col_all" = format <- "_all_columns" 
      )
    }
    
  }
  
  # ........... f(x) DATA to STRING ..............
  # ..............................................
  
  #      .) Función que asigna crea STRING DATE
  #      .) Une Year/month/day en un solo STRING sin guiones
  
  date_str <- toString(Sys.Date()) %>% 
    stringr::str_split(.,"-") %>% .[[1]] %>%
    stringr::str_c(., collapse = "")
  
  
  
  # %%%%%%%%%%%%%%%%  DOWNLOADHANDLER  %%%%%%%%%%%%%%%%%
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  #      .) Es un OUTPUT
  #      .) Apunta al ID = TABLE_SAVE
  #      .) ESTRUCUTRA:
  #               .) FILENAME Function  => crea nombre de archivo
  #               .) CONTENT  Function  => f(x) DOWNLOAD 
  
  

   output$table_save <- downloadHandler(
    filename = function() {
      
      if (input$day_table == "ddbb"){
        paste(date_str,'_all_ddbb.csv', sep = "")
      } else {
        paste(date_str,column_selected(),format_selected(), sep = "")
      }
     
    },
    content = function(file) {
      
      # ....... WRITE TYPE ...........
      # .............................
      
      #      .) en f(x) del formato
      #      .) lo DESCARGA en diferentes formatos (CSV, XLSX, GPKG)
      
      #      .) PRIMERO Diferenciamos
      #              .) SI es DDBB => Descarga TODA la BBDD solo en CSV
      #              .) NO es DDBB => Puedes descargar en CSV, XLSL o GPKG
      
      if (input$day_table == "ddbb" ) {
        
        
        # ....... WAITER / HOSTESS ..........
        # ...................................
        
        #       .) El descargar TODA la BBDD en CSV tarda 2 min o mas
        #       .) Por eso usamos el WAITER para que el usuario vea la progresión del procesos
        
        #       .) PRIMERO:
        #              .) creamos OBJECTO con la classe = WAITER::HOSTESS$new
        #       .) SEGUNDO
        #              .) SET_LOADER  
        #              .) Especificamos: image SVG, tipo progress y fill direction
        
        
        
        hostess_plots <- waiter::Hostess$new(infinite = TRUE)
        hostess_plots$set_loader(waiter::hostess_loader(
          svg = 'images/hostess_image.svg',
          progress_type = 'fill',
          fill_direction = 'btt'
        ))
        
        # ....... WAITER / HOSTESS ..........
        # ...................................
        
        #       .) TERCERO:
        #       .) Definir LUGAR de aparición = ID ( en APP.R / mainPanel)
        #       .) Definir => Get_Loader() definido anteriormente + HTML H3 + P
        
        
        waiter_ddbb <- waiter::Waiter$new(
          id = 'overlay_div',
          html = shiny::tagList(
            hostess_plots$get_loader(),
            shiny::h3(translate_app("progress_ddbb", lang())),
            shiny::p(translate_app("progress_detail_plots", lang()))
          ),
          color = "#E8EAEB"  # color del fondo
        )
        
        #       .) CUARTO: Show WAITER + Star HOSTESS
        #       .) QUINTO: Definir EXIT Hostess / WAITER 
        
        
        waiter_ddbb$show()
        hostess_plots$start()
        on.exit(hostess_plots$close(), add = TRUE)
        on.exit(waiter_ddbb$hide(), add = TRUE)
        
       
        # ...... FUNCIÓN CREAR CSV ..........
        # ...................................
        
        #       .) Función de crear CSV
        #       .) Tarda unos 2 min
        #       .) Por esto añadimos previamente el WAITER
        
        write.csv(databaseInput(), file, row.names = FALSE)
        
        # switch (input$data_format,
        #         "csv" = write.csv(databaseInput(), file, row.names = FALSE),
        #         "xlsx" = writexl::write_xlsx(databaseInput(), file),
        #         "gpkg" = sf::st_write(databaseInput(), file)
        # )
        
      } else {
        
        switch (input$data_format,
                "csv" = write.csv(datasetInput(), file, row.names = FALSE),
                "xlsx" = writexl::write_xlsx(datasetInput(), file),
                "gpkg" = sf::st_write(datasetInput(), file)
        )
      }


    }
  )
   

}



