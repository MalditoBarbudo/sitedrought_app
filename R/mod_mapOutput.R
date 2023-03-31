#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#'
#' @export
#'
#'
mod_mapOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(ns("map_daily"), height = 600),
    shiny::uiOutput(ns('map_container'))
  )
}

#' mod_map server function
#'
#' @details mod_map is in charge of setting the points/polygons (sf) and rasters
#'   in the leaflet projection.
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,main_data_reactives reactives
#' @param parent_session session object to change active tab
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_mapOutput
#'
mod_map <- function(
  input, output, session,
  data_reactives, main_data_reactives, 
  parent_session, lang
) {
  
  output$map_daily <- leaflet::renderLeaflet({
    
    # we need at least the data and the variable selected to continue
    # shiny::req(
    #   main_data_reactives$data_day_origin,
    #   data_reactives$var_daily
    # )
    # data
    # var_daily_sel <- data_reactives$var_daily
    # data_map <- main_data_reactives$data_day_origin |>
    #   dplyr::select(var_daily_sel, plot_id)
    # 
    # dynamic_radius <- base_size()
    
    # base map
    leaflet::leaflet() |>
      leaflet::setView(2.2018256,41.089058, zoom = 7) |>
      leaflet.extras::addResetMapButton() |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldShadedRelief,
        group = translate_app('Relief', lang())
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = translate_app('Imagery', lang())
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$OpenStreetMap,
        group = translate_app('OSM', lang())
      ) |>
      leaflet::addLayersControl(
        baseGroups = c(
          translate_app('Relief', lang()),
          translate_app('Imagery', lang()),
          translate_app('OSM', lang())
        ),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::addMapPane('admin_divs', zIndex = 410) |>
      leaflet::addMapPane('plots', zIndex = 420) #|>
      # leaflet::addCircles(
      #   data = data_map,
      #   group = "plots_layer",
      #   layerId = ~plot_id,
      #   # label = "",
      #   # labelOptions = leaflet::labelOptions(interactive = TRUE),
      #   radius = dynamic_radius,
      #   stroke = FALSE,
      #   fillOpacity = 0.7,
      #   fillColor = rlang::new_formula(lhs = NULL, rhs = rlang::sym(var_daily_sel)),
      #   options = leaflet::pathOptions(pane = "plots")
      # ) |>
      # leaflet::clearControls() #|>
      # leaflet::addLegend(
      #   position = "bottomright",
      #   title = translate_app(var_daily_sel, lang_declared),
      #   pal = pal_legend,
      #   values = value_legend,
      #   labFormat = leaflet::labelFormat(transform = function(x) rev(x)),
      #   opacity = 1
      # )
    
  })
  
  ## reactives ####
  # zoom-size transformation. Logic is as follows:
  #   - In closer zooms (10) go to the base size of 750. In far zooms increase
  #     accordingly, until zoom 7 and further, with a max size of 1500
  base_size <- shiny::reactive({
    # we need zoom level to calculate this
    shiny::req(input$map_daily_zoom)
    current_zoom <- input$map_daily_zoom
    
    if (current_zoom <= 7) {
      current_zoom <- 7
    }
    if (current_zoom >= 10) {
      current_zoom <- 10
    }
    
    size_transformed <- 750 + ((10 - current_zoom) * 250)
    
    return(size_transformed)
  })
  
  ## observers ####
  # data and points update
  shiny::observe({
    # we need at least the data and the variable selected to continue
    shiny::req(
      data_reactives$var_daily,
      data_reactives$viz_pal_config,
      # data_reactives$viz_pal_reverse,
      main_data_reactives$data_day_origin,
      base_size
    )
    # data
    var_daily_sel <- data_reactives$var_daily
    data_map <- main_data_reactives$data_day_origin |>
      dplyr::select(var_daily_sel, plot_id)

    # radius
    dynamic_radius <- base_size()
    
    # palettes
    viz_pal_config <- data_reactives$viz_pal_config
    viz_pal_reverse <- data_reactives$viz_pal_reverse
    palette_domain <- c(
      palettes_dictionary[[var_daily_sel]]$min,
      palettes_dictionary[[var_daily_sel]]$max
    )
    
    if (var_daily_sel %in% c("Precipitation", "LFMC", "DFMC")) {
      palette_domain <- c(
        0, max(data_map[[var_daily_sel]])
      )
    }
    
    # browser("points_map_obs")
    
    pal <- switch(
      viz_pal_config,
      "low" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          palettes_dictionary[[var_daily_sel]]$pal(9), c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
        ),
        domain = palette_domain,
        reverse = palettes_dictionary[[var_daily_sel]]$rev, na.color = 'gray'
      ),
      "high" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          palettes_dictionary[[var_daily_sel]]$pal(9), c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        ),
        domain = palette_domain,
        reverse = palettes_dictionary[[var_daily_sel]]$rev, na.color = 'gray'
      ),
      "normal" = leaflet::colorNumeric(
        palettes_dictionary[[var_daily_sel]]$pal(256),
        domain = palette_domain,
        reverse = palettes_dictionary[[var_daily_sel]]$rev, na.color = 'gray'
      )
    )
    
    pal_legend <- switch(
      viz_pal_config,
      "low" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          palettes_dictionary[[var_daily_sel]]$pal(9), c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
        ),
        domain = palette_domain,
        reverse = !palettes_dictionary[[var_daily_sel]]$rev, na.color = 'gray'
      ),
      "high" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          palettes_dictionary[[var_daily_sel]]$pal(9), c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        ),
        domain = palette_domain,
        reverse = !palettes_dictionary[[var_daily_sel]]$rev, na.color = 'gray'
      ),
      "normal" = leaflet::colorNumeric(
        palettes_dictionary[[var_daily_sel]]$pal(256),
        domain = palette_domain,
        reverse = !palettes_dictionary[[var_daily_sel]]$rev,
        na.color = 'gray'
      )
    )
    
    labels <- glue::glue(
      "<strong>{translate_app('PLOT', lang())}</strong>
       <br/> {data_map$plot_id} <br/>
       {var_daily_sel} = {round(data_map[[var_daily_sel]], 2)}"
    ) |>
      purrr::map(
        .f = \(label) {
          shiny::HTML(label)
        }
      )
    
    leaflet::leafletProxy("map_daily") |>
      leaflet::clearGroup("plots_layer") |>
      leaflet::addCircles(
        data = data_map,
        group = "plots_layer",
        layerId = ~plot_id,
        label = labels,
        labelOptions = leaflet::labelOptions(interactive = TRUE),
        radius = dynamic_radius,
        stroke = FALSE,
        fillOpacity = 0.7,
        fillColor = pal(data_map[[var_daily_sel]]),
        options = leaflet::pathOptions(pane = "plots")
      ) |>
      leaflet::clearControls() |>
      leaflet::addLegend(
        position = "bottomright",
        title = translate_app(var_daily_sel, lang()),
        pal = pal_legend,
        values = c(palettes_dictionary[[var_daily_sel]]$min, data_map[[var_daily_sel]], palettes_dictionary[[var_daily_sel]]$max),
        labFormat = leaflet::labelFormat(transform = function(x) {sort(x, decreasing = TRUE)}),
        na.label = '',
        opacity = 1
      )
    
  })
  
  
}
