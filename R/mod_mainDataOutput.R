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
    
    
    ## TODO
    ## when selecting aiguestortes (A o PN), IFN plots inside the park must also be added.
    ## Doing that here with an st_covered_by with the park perimeter is not cost-efficient, so
    ## is better to add a column in the data table when preparing the raw data.
    plot_origin_sel <- data_reactives$plot_origin
    data_day_origin <- switch(
      plot_origin_sel,
      "PN" = data_day() |> sf::st_join(parks_big, left = FALSE, join = sf::st_covered_by),
      "A" = data_day() |> sf::st_join(aiguestortes_big, left = FALSE, join = sf::st_covered_by),
      "O" = data_day() |> sf::st_join(ordesa_big, left = FALSE, join = sf::st_covered_by),
      "T" = data_day(),
      "P" = data_day() |> dplyr::filter(plot_origin == "ifn"),
      "S" = data_day() |> dplyr::filter(plot_origin == "matollar")
    )
    
    return(data_day_origin)
  })
  
  # timeseries reactive
  data_ts <- shiny::reactive({
    # validations
    shiny::validate(
      shiny::need(data_reactives$var_daily, 'No var selected'),
      shiny::need(data_reactives$date_daily, 'No var selected'),
      shiny::need(map_reactives$map_daily_shape_click, 'no map click')
    )

    plot_id_sel <- map_reactives$map_daily_shape_click$id
    var_daily_sel <- data_reactives$var_daily
    
    # If we select a quantile variable, we need to select also the non-quantile version to show
    # in the timeseries. We get the base name of the variable and use that as selecting filter.
    stripped_var <- stringr::str_remove_all(var_daily_sel, "_q$")

    plot_data <- main_data |>
      dplyr::filter(plot_id == plot_id_sel) |>
      dplyr::as_tibble() |>
    dplyr::select(date, dplyr::contains(stripped_var))
    
    data_ts <- plot_data |>
      dplyr::select(dplyr::contains(stripped_var)) |>
      as.matrix() |>
      xts::as.xts(order.by = plot_data$date)
    
    return(data_ts)
  })
  
  
  # returning inputs
  # reactive values to use in app and other modules
  main_data_reactives <- shiny::reactiveValues()
  
  shiny::observe({
    # user inputs
    main_data_reactives$data_day <- data_day()
    main_data_reactives$data_day_origin <- data_day_origin()
    main_data_reactives$data_ts <- data_ts()
  })
  
  return(main_data_reactives)

}
