#' mod_tsOutput and mod_ts
#'
#' @description A shiny module to generate and populate the time series output
#'
#' @param id shiny id
#'
#' @export
mod_tsOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    dygraphs::dygraphOutput(ns('timeseries_daily')),
    shiny::uiOutput(
      ns('percentiles_container')
    )
  )
}

#' mod_ts server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,main_data_reactives reactives needed
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_tsOutput
mod_ts <- function(
  input, output, session,
  data_reactives, main_data_reactives,
  lang
) {
  
  # ........... OUTPUT / GRAFICO ...........
  # ........................................
  
  #      .) OUTPUT que muestra GRAFICO
  #      .) Es el TIMESERIE del plot seleccionado

  output$timeseries_daily <- dygraphs::renderDygraph({

    shiny::validate(
      shiny::need(data_reactives$variable_reactive, 'no var data yet')
    ) 
    
    var <- data_reactives$variable_reactive
    timeseries_data <- main_data_reactives$timeserie 
    
  })
  
  # ......... OUTPUT / EXPLICACIÓN .........
  # ........................................
  
  #      .) DIV que muestra explicacion sobre percentiles históricos
  #      .) Solo se visualiza con las variables que tiene percentil
  
  output$percentiles_container <- shiny::renderUI({
    
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
    
    shiny::div(
      id = ns('explanation_percentiles'),
      shiny::HTML(translate_app('expl_percentiles', lang_declared))
    )
    
  })
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ----------------------------    OBVSERVE    ---------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  # ....... SHOW/HIDDEN EXPLICACION ........
  # ........................................
  
  #      .) Activar menú explicativo
  #      .) SOLO cuando haya gráficos de PERCENTILES HISTÓRICOS
  #      .) Por lo tanto cuando la variable es c("REW_q","DDS_q","LFMC_q","REW","DDS","LFMC")
  
  
  shiny::observe({
    
    shiny::validate(
      shiny::need(data_reactives$variable_reactive, 'no variable')
    )
    varialbe <- data_reactives$variable_reactive
  
    var_quantile <- c("REW_q","DDS_q","LFMC_q","REW","DDS","LFMC")
    
    if (varialbe %in% var_quantile) {
      shinyjs::show('explanation_percentiles')
    } else {
      shinyjs::hide('explanation_percentiles')
    }
    
    
  })

}
