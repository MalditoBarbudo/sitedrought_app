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
#' @param main_data main_data table
#' @param main_date_range absolute date range in main data
#' @param lang lang reactive
#'
#' @export
mod_data <- function(   
  input, output, session,
  main_data, main_date_range, lang  
   
) {
  
  # render UI
  output$mod_data_container <- shiny::renderUI({
    ## ns and date language
    ns <- session$ns
    lang_declared <- lang()
    dates_lang <- switch(
      lang_declared,
      'cat' = 'ca',
      'spa' = 'es',
      'eng' = 'en'              
    )
    
    ## inputs choices
    # variables
    var_daily_choices <- list(
      c("REW","DDS") %>%
        purrr::set_names(translate_app(., lang_declared)),
      c("PET", "Precipitation") %>%
        purrr::set_names(translate_app(., lang_declared)),
      c("LFMC","DFMC","SFP","CFP") %>%
        purrr::set_names(translate_app(., lang_declared)),
      c("REW_q","DDS_q","LFMC_q") %>%
        purrr::set_names(translate_app(., lang_declared))
    ) |>
      purrr::set_names(
        translate_app(c("drought_vars", "climate_vars", "fire_vars", "quantiles"), lang_declared)
      )
    
    # plots
    plot_origin_choices <- c("T", "P", "PN", "A", "O", "S") |>
      purrr::set_names(translate_app(c("T", "P", "PN", "A", "O", "S"), lang_declared))
    
    ## ui taglist
    # variables, dates, plots and palettes
    shiny::tagList(
      ## vars
      shiny::selectInput(
        ns("var_daily"), translate_app("var_daily_label", lang_declared),
        choices = var_daily_choices
      ),
      ## dates
      shiny::dateInput(
        ns("date_daily"), translate_app("date_daily_label", lang_declared),
        # format = "yyyy/mm/dd",
        value = main_date_range["max"],
        max = main_date_range["max"],
        min = main_date_range["min"],
        weekstart = 1, language = dates_lang
      ),
      ## plots
      shiny::selectInput(
        ns("plot_origin"), translate_app("plot_origin_label", lang_declared),
        choices = plot_origin_choices
      ),
      
      # row for the palette selector
      shiny::fluidRow(
        shiny::column(
          6, align = 'center',
          # low, normal or high palette
          shinyWidgets::radioGroupButtons(
            ns('viz_pal_config'),
            translate_app('viz_pal_config_input', lang_declared),
            size = 'sm',
            choices = c('high', 'normal', 'low') %>%
              purrr::set_names(c(
                translate_app('pal_high', lang_declared),
                translate_app('pal_normal', lang_declared),
                translate_app('pal_low', lang_declared)
              )),
            selected = 'normal',
            direction = 'vertical',
            checkIcon = list(
              yes = shiny::icon('tree-deciduous', lib = 'glyphicon')
            ),
            status = 'lfc_radiogroupbuttons'
          )
        ),
        shiny::column(
          6, align = "center",
          # reverse palette
          shinyWidgets::awesomeCheckbox(
            ns('viz_pal_reverse'),
            label = translate_app('viz_pal_reverse_input', lang_declared),
            value = FALSE, status = 'info'
          )
        )
      ) # end of Fluid Row
    ) # end of tagList
  }) # end RENDER UI     

  # observers
  
  # observe plot_origin, as if is the shrubs dataset we need to remove CFP var from fire vars
  shiny::observeEvent(
    eventExpr = input$plot_origin,
    handlerExpr = {
      
      lang_declared <- lang()
      plot_origin <- input$plot_origin
      var_daily_choices_updated <- list(
        c("REW","DDS") %>%
          purrr::set_names(translate_app(., lang_declared)),
        c("PET", "Precipitation") %>%
          purrr::set_names(translate_app(., lang_declared)),
        c("LFMC","DFMC","SFP","CFP") %>%
          purrr::set_names(translate_app(., lang_declared)),
        c("REW_q","DDS_q","LFMC_q") %>%
          purrr::set_names(translate_app(., lang_declared))
      ) |>
        purrr::set_names(
          translate_app(c("drought_vars", "climate_vars", "fire_vars", "quantiles"), lang_declared)
        )
      
      if (plot_origin == "S") {
        # remove CFP
        purrr::pluck(var_daily_choices_updated, 3, 4) <- NULL
        purrr::pluck(var_daily_choices_updated, 3, 4) <- purrr::zap()
      }
      
      shiny::updateSelectInput(session, "var_daily", choices = var_daily_choices_updated)
    }
  )
  
  # returning inputs
  # reactive values to use in app and other modules
  data_reactives <- shiny::reactiveValues()
  
  shiny::observe({
    # user inputs
    data_reactives$var_daily <- input$var_daily
    data_reactives$date_daily <- input$date_daily
    data_reactives$plot_origin <- input$plot_origin
    # palette
    data_reactives$viz_pal_config <- input$viz_pal_config
    data_reactives$viz_pal_reverse <- input$viz_pal_reverse
  })
  
  return(data_reactives)
}
