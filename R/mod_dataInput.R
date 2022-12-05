#' @title mod_dataInput and siteDrought_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataInput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_data_container')
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
siteDrought_data <- function(   
  input, output, session,
  siteDroughtdb, lang, main_data_reactives  
   
) {
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ------------------------   RENDER UI  ------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  output$mod_data_container <- shiny::renderUI({
    
    
    # //////////////////////////////////
    # --      INICIALIZAR DATOS       --
    # //////////////////////////////////
    
    
    # ... INICIALIZAR NS / LANGUAGE .....
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
    

    # ....... INICIALIZAR FECHAS ........
    # ...................................
    
    #       .) Queremos el rango de fechas de toda la base de datos
    #       .) Pero MINETRAS de DESCARGA TODA la BBDD
    #       .) Le tenemos que indicar una fecha (la actual)
    
    #       .) Una vez DESCARGADA la BBDD 
    #       .) Ya podemos cargar TODO el RANGO de fechas de la BBDD
    #       .) El OBSERVER EVENT con UPDATEDATEINIPUT lo hace
    


    date_max <- Sys.Date() - 1
    date_min <- Sys.Date() - 2

    
    # ...... VARIABLE SELECTINPUT .......
    # ...................................
    
    #       .) Variables según MIQUEL
    #           .) sequía:              REW, DDS
    #           .) variable climáticas: PET, Precipitation
    #           .) variables incendio:  "LFMC","DFMC","SFP","CFP"
    #           .) quantiles :          "REW_q","DDS_q","LFMC_q"
    
    

    drought_vars <- c("REW","DDS") %>%
      magrittr::set_names(translate_app(., lang_declared))
    climate_vars <- c("PET", "Precipitation") %>%
      magrittr::set_names(translate_app(., lang_declared))
    fire_vars <- c("LFMC","DFMC","SFP","CFP") %>%
      magrittr::set_names(translate_app(., lang_declared))
    quantiles_vars <- c("REW_q","DDS_q","LFMC_q") %>%
      magrittr::set_names(translate_app(., lang_declared))
    
    
    # //////////////////////////////////
    # --        ETIQUETAS HTML5       --
    # //////////////////////////////////
    
    
    #       .) TAGLIST crea una definición de etiqueta HTML
    #       .) Creamos los elementos HTML5 con TAGS
    #       .) DROPDOWNS (SelectIntpu),...
     
    shiny::tagList(
   
        # ........... SELECCION VARIABLE ..........
        # .........................................
        
        #      .) Queremos un SELECTINPUT que varie en f(x) de ORIGEN
        #      .) Si el ORIGEN es = MATOLLAR
        #      .) No tiene que aparecer la variable CFP
      
        #      .) Pero INCIALMENTE asignamos TODAS las VARIABLES
        #      .) y si SE SELECCCIONE MATOLLAR 
        #      .) Eliminaremos CFP
        #      .) El OBSERVER EVENT con UPDATESELECTINIPUT lo hace
      
        
      
        shiny::selectInput(
          ns('variable'), translate_app('var_daily_label', lang_declared),
          choices = shiny_set_names(list(
            'drought variables' = drought_vars,
            'climate variables' = climate_vars,
            'fire variables' = fire_vars,
            'Percentiles variables' = quantiles_vars
          ), lang_declared)
        ),
      
      # ........ PROBLEMA FECHA ...........
      # ...................................
      
      #       .) PROBLEMA
      #       .) Aveces el desplegable de DATE queda debajo del NAV
      #       .) Para solucionar-lo
      #           .) https://developer.mozilla.org/es/docs/Web/CSS/z-index
      #           .) Uso los Z-INDEX del CSS
      #           .) El Z-INDEX indica PRIORIDA de aparecer ENCIMA
      #           .) Como MAYOR el Z-INDEX mas encima de todo
      #       .) El cambio lo hago en archivo CSS (siteDrought_settings.R)
    
        
      shiny::dateInput(
        ns("fecha"), translate_app('date_daily_label', lang_declared),
        value = date_max,
        format = "yyyy/mm/dd",
        max = date_max,
        min = date_min
      ),
      
      
      # ...... SELECCION ORIGEN PLOT ......
      # ...................................
      
      shiny::selectInput(
        ns('origen'), translate_app('plot_origin_label', lang_declared),
        shiny_set_names(c(
          "T"="T",
          "P"="P",
          "PN"="PN",
          "A"="A",
          "O"="O",
          "S"="S"), lang_declared)
      ),
      
      shiny::fluidRow(
        shiny::column(6, align = 'center',
                      
            # ..... RADIO BUT LEGEND COLOR ......
            # ...................................
            
            #      .) Dejo COMENTADA el CANVIO de COLOR de LEYENDA
            #      .) Me espero a hablar-lo con Miquel y Víctor
                      
            shinyWidgets::radioGroupButtons(
              ns("legend_modify"),
              translate_app("type_legend_label", lang_declared),
              size = 'normal',
              choices = shiny_set_names(c("estandard_label" = "estandard",
                                          "1st_label" = "tip_1",
                                          "2nd_label" = "tip_2"),lang_declared),
              selected = 'estandard', direction = 'vertical',
              status = 'lfc_radiogroupbuttons'
            )
        ),
        
        shiny::column(6, align = 'center',
                
            # ... CHEK BUTTON LEGEND INVERT .....
            # ...................................
            
            #      .) Check Button
            #      .) Para invertir Leyenda
            
            shinyWidgets::prettyCheckbox(
              ns('legend_check'),
              translate_app('reverse_legend', lang_declared),
              status = 'success', shape = 'curve', fill = TRUE
            )
        )
      ) # end of Fluid Row
   ) # end of tagList
}) # end RENDER UI         

  

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # --------------------   OBSERVE EVENTS  ------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  # ........ O.E. MATOLLAR ............
  # ...................................
  
  #      .) Es un OBSERVEREVENT de => ORIGEN 
  #      .) Activa un UPDATESELECTINPUT
  
  #      .) Cada vez que variamos ORIGEN
  #      .) MODIFICA el SELECT INPUT Original
  #              .) Si seleccionamos MATOLLAR
  #              .) No aparecerá FOC CAPAÇADA (CFP)
  

  shiny::observeEvent(
    eventExpr = input$origen,
    handlerExpr = {
      
      origen <- input$origen
      
      lang_declared <- lang()
      dates_lang <- switch(
        lang_declared,
        'cat' = 'ca',
        'spa' = 'es',
        'eng' = 'en'
      )
      
      switch (origen,
              "S" = fire_variables <- c("LFMC","DFMC","SFP"),
              fire_variables <- c("LFMC","DFMC","SFP","CFP")
      )
      
      
      drought_vars <- c("REW","DDS") %>%
        magrittr::set_names(translate_app(., lang_declared))
      climate_vars <- c("PET", "Precipitation") %>%
        magrittr::set_names(translate_app(., lang_declared))
      fire_vars <- fire_variables %>%
        magrittr::set_names(translate_app(., lang_declared))
      quantiles_vars <- c("REW_q","DDS_q","LFMC_q") %>%
        magrittr::set_names(translate_app(., lang_declared))
      
      
      
      shiny::updateSelectInput(
        session,
        'variable',
        choices = shiny_set_names(list(
          'drought variables' = drought_vars,
          'climate variables' = climate_vars,
          'fire variables' = fire_vars,
          'Percentiles variables' = quantiles_vars
        ), lang_declared) 
      )
      
      
    })
  
  
  
  # ........ O.E. FECHA ..............
  # ...................................
  
  #      .) Es un OBSERVEREVENT de => MAIN_DATA_REACTIVES$DATA_DAY 
  #      .) Activa un UPDATEDATEINPUT
  
  #      .) Cada vez que variamos DATA_DAY (solo AL inicializar la APP)
  #      .) MODIFICA el DATE INPUT Original
  #              .) Obtenemos TODO el rango de fechas
  #              .) Indicamos el Max y el Mín
  
  
  shiny::observeEvent(
    eventExpr = main_data_reactives$data_day,
    handlerExpr = {
      
      data_day <- main_data_reactives$data_day
        
      date_max <- max(data_day$date)
      date_min <- min(data_day$date)  
        
      
      updateDateInput(
        session, 
        "fecha",
           value = date_max,
           min   = date_min,
           max   = date_max
      )

  })
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ----------------------   REACTIVES  --------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  #      .) Creamos DATA REACTIVE
  #      .) ASSIGNAMOS los OBERSVERS
  
  # ...... DATA REACTIVE .........
  # ..............................
  
  #      .) Es la variable que ALMACENA TODOS los REACTVES
  #      .) Cada reactive se ALMACENA con un $
  
  
  data_reactives <- shiny::reactiveValues()
  
  # ...... DATA OBSERVE ..........
  # ..............................
  
  #      .) Creamos dentro de DATA_REACTIVE
  #      .) Todos los diferentes apartados con $
  
  shiny::observe({  
    
    data_reactives$fecha_reactive  <- input$fecha
    data_reactives$variable_reactive <- input$variable
    data_reactives$origen_reactive <- input$origen
    data_reactives$legend_check <- input$legend_check
    data_reactives$legend_modify_reactive <- input$legend_modify
    
  })
  
  # ......... RETURN .............
  # ..............................
  
  #      .) Quiero tener constantemente 4 valores CONTROLADOS
  #      .) FECHA / VARIABLE / ORIGEN / LEGEND MODIFY
  #      .) Cuando VARIAN
  #      .) Las RETORNO para que otros MÓDULOS los aprovechen (el MAPS de LEAFLET)


  return(data_reactives)
}
