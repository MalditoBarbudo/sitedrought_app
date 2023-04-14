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
    dygraphs::dygraphOutput(ns('timeseries_plot')),
    shiny::uiOutput(ns('dygraph_explanation'))
  )
}

#' mod_ts server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,main_data_reactives,map_reactives reactives needed
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_tsOutput
mod_ts <- function(
  input, output, session,
  data_reactives, main_data_reactives, map_reactives,
  lang
) {
  
  # timeseries
  output$timeseries_plot <- dygraphs::renderDygraph({

    shiny::validate(
      shiny::need(map_reactives$map_daily_shape_click, 'no var data yet'),
      shiny::need(main_data_reactives$data_ts, 'no ts data')
    )
    
    data_ts <- main_data_reactives$data_ts
    var_daily_sel <- data_reactives$var_daily
    vars_translated <- translate_app(names(data_ts), lang())
    
    dygraph_domain <- c(
      palettes_dictionary[[var_daily_sel]]$min,
      palettes_dictionary[[var_daily_sel]]$max
    )
    
    if (var_daily_sel %in% c("Precipitation", "LFMC", "DFMC")) {
      dygraph_domain <- c(
        0, max(data_ts[,1])
      )
    }
    
    # strokes should be dependent on the var selected. If is quantile, stroke should be 2 for the
    # second axis and one for first.
    quantile_modifier <- 0
    if (stringr::str_detect(var_daily_sel, "_q$")) {
      quantile_modifier <- 1
    }
    
    dygraph_title <- glue::glue(
      "{map_reactives$map_daily_shape_click$id} [Lng: {map_reactives$map_daily_shape_click$lng}, Lat: {map_reactives$map_daily_shape_click$lat}]"
    )
    
    dygraph_output <- data_ts |>
      dygraphs::dygraph(main = dygraph_title) |>
      dygraphs::dySeries(label = vars_translated[1], axis = 'y', strokeWidth = 2 - quantile_modifier) |> 
      dygraphs::dyAxis("y", label = vars_translated[1], valueRange = dygraph_domain, rangePad = 5) |>
      dygraphs::dyOptions(fillGraph = TRUE, fillAlpha = 0.1) |> 
      dygraphs::dyEvent(data_reactives$date_daily, data_reactives$date_daily, labelLoc = "top") |> 
      dygraphs::dyLegend(show = "follow")
    
    if (ncol(data_ts) > 1) {
      dygraph_output <- dygraph_output |>
        dygraphs::dySeries(label = vars_translated[2], axis = 'y2', strokeWidth = 1 + quantile_modifier) |> 
        dygraphs::dyAxis(
          "y2",
          label = translate_app("percentiles_axis_label", lang()),
          valueRange = dygraph_domain, rangePad = 5
        )
        
    }
    # return the dygraph
    dygraph_output
  })
  
  # explanation div
  output$dygraph_explanation <- shiny::renderUI({
    ns <- session$ns
    shiny::div(
      id = ns('explanation_percentiles'),
      shiny::HTML(translate_app('expl_percentiles', lang()))
    )
  })
  
  ## Observers
  # show/hide the quantiles explanation
  shiny::observe({
    shiny::validate(
      shiny::need(main_data_reactives$data_ts, 'no data')
    )
    
    if (ncol(main_data_reactives$data_ts > 1)) {
      shinyjs::show('explanation_percentiles')
    } else {
      shinyjs::hide('explanation_percentiles')
    }
  })

}
