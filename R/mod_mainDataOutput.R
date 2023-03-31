#' @title mod_mainDataOutput and mod_mainData
#'
#' @description Shiny module to get the data
#'
#' @param id
#'
#' @export
mod_mainDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_mainData server function
#'
#' @details mod_mainData always return the data in the 3043 projection
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,map_reactives reactives from modules
#' @param main_data main_data table
#' @param lang lang selected
#'
#' @importFrom dplyr n
#'
#' @export
#'
#' @rdname mod_mainDataOuput
mod_mainData <- function(
  input, output, session,
  data_reactives, map_reactives,
  main_data, lang
) {
  
  ## waiter and hostess progress
  # set a progress with waiter. We will use infinite = TRUE, that way we dont need to calculate
  # the, on the other hand unknown, step durations
  hostess_plots <- waiter::Hostess$new(infinite = TRUE)
  hostess_plots$set_loader(waiter::hostess_loader(
    svg = 'images/hostess_image.svg',
    progress_type = 'fill',
    fill_direction = 'btt'
  ))
  
  ## reactives
  # data for the selected day
  data_day <- shiny::eventReactive(
    eventExpr = data_reactives$date_daily,
    valueExpr = {
      # validations
      shiny::validate(
        shiny::need(data_reactives$date_daily, 'No date selected')
      )
      
      # waiter for data filtering
      # waiter_map <- waiter::Waiter$new(
      #   id = 'overlay_div',
      #   html = shiny::tagList(
      #     hostess_plots$get_loader(),
      #     shiny::h3(translate_app("progress_plots", lang())),
      #     shiny::p(translate_app("progress_detail_plots", lang()))
      #   ),
      #   color = "#E8EAEB"
      # )
      # waiter_map$show()
      # hostess_plots$start()
      # on.exit(hostess_plots$close(), add = TRUE)
      # on.exit(waiter_map$hide(), add = TRUE)
      
      data_day <- main_data |>
        dplyr::filter(date == data_reactives$date_daily)
      
      return(data_day)
    }
  )
  
  data_day_origin <- shiny::reactive({
    # validations
    shiny::validate(
      shiny::need(data_day, 'No data'),
      shiny::need(data_reactives$plot_origin, 'No plot origin selected')
    )
    
    plot_origin_sel <- switch(
      data_reactives$plot_origin,
      "T" = "T",
      "PN" = c("aiguestortes","ordesa"),
      "P"  = "ifn",
      "A"  = "aiguestortes",
      "S"  = "matollar",
      "O"  = "ordesa"
    )
    data_day_origin <- data_day()
    
    # if not all, filter the data
    if (plot_origin_sel != "T") {
      data_day_origin <- data_day_origin |>
        dplyr::filter(plot_origin %in% plot_origin_sel)
    }
    
    return(data_day_origin)
  })
  
  # timeseries reactive
  # data_ts <- shiny::reactive({
  #   browser("data_ts")
  #   # validations
  #   shiny::validate(
  #     shiny::need(data_reactives$var_daily, 'No var selected'),
  #     shiny::need(data_reactives$date_daily, 'No var selected'),
  #     shiny::need(map_reactives$map_daily_shape_click, 'no map click')
  #   )
  #   
  #   plot_id_sel <- map_reactives$map_daily_shape_click$id
  #   var_daily_sel <- data_reactives$var_daily
  #   
  #   plot_data <- main_data |>
  #     dplyr::filter(plot_id == plot_id_sel)
  #   
  #   data_ts <- plot_data |>
  #     dplyr::select(dplyr::contains(var_daily_sel)) |>
  #     xts::as.xts(order.by = ts_data$date)
  #   
  #   return(data_ts)
  # })
  
  
  # returning inputs
  # reactive values to use in app and other modules
  main_data_reactives <- shiny::reactiveValues()
  
  shiny::observe({
    # user inputs
    main_data_reactives$data_day <- data_day()
    main_data_reactives$data_day_origin <- data_day_origin()
    # main_data_reactives$data_ts <- data_ts()
  })
  
  return(main_data_reactives)

}
