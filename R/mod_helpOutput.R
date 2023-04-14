#' @title mod_helpOutput and siteDrought_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_helpOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(ns('mod_help_container'))
  )
}

#' siteDrought_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#' 
#' @param main_data_reactives reactives needed from other modules
#'
#' @param lang lang reactive
#'
#' @export
mod_help <- function(
  input, output, session,
  main_data_reactives, lang
) {

  # renderUI ####
  output$mod_help_container <- shiny::renderUI({
    
    ns <- session$ns
    lang_declared <- lang()
    
    # input choices
    variable_help_choices <- create_var_list_for_input(lang_declared)
     
    shiny::tagList(
      shiny::selectInput(
        ns('variable_help'), translate_app('var_daily_label', lang_declared),
        choices = variable_help_choices
      ),
      # help text
      shiny::uiOutput(
        ns('help_text')
      )
    )
  })
  
  output$help_text <- shiny::renderUI({
    
    lang_declared <- lang()
    var_selected <- input$variable_help
    
    shiny::tagList(
      shiny::p(
        translate_app('help_description', lang_declared),
        translate_thesaurus_app(var_selected, lang_declared, 'description_help')
      ),
      shiny::br(),
      shiny::p(
        translate_app('units_description', lang_declared),
        translate_thesaurus_app(var_selected, lang_declared, 'units')
      )
    )
  })
}
