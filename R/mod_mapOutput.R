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
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldGrayCanvas,
        group = translate_app('WorldGrayCanvas', lang())
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.PositronNoLabels,
        group = translate_app('PositronNoLabels', lang())
      ) |>
      leaflet::addLayersControl(
        baseGroups = c(
          translate_app('Relief', lang()),
          translate_app('Imagery', lang()),
          translate_app('OSM', lang()),
          translate_app('WorldGrayCanvas', lang()),
          translate_app('PositronNoLabels', lang())
        ),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::addMapPane('admin_divs', zIndex = 410) |>
      leaflet::addMapPane('plots', zIndex = 420)
    
  })
  
  ## reactives ####
  # zoom-size transformation. Logic is as follows:
  #   - In closer zooms (10) go to the base size of 750. In far zooms increase
  #     accordingly, until zoom 7 and further, with a max size of 2000
  base_size <- shiny::reactive({
    # we need zoom level to calculate this. Also, plot origin to get defaults
    shiny::req(input$map_daily_zoom, data_reactives$plot_origin)
    
    # default_size <- dplyr::case_match(
    #   data_reactives$plot_origin,
    #   # Matollar, bigger points as they are scattered dn difficult to see
    #   "S" ~ 5000,
    #   # Parks, bigger than normal, smaller than matollar because there is more closer points
    #   c("O", "A", "PN") ~ 500,
    #   # default 1000, for the rest of origins
    #   .default = 1000
    # )
    default_size <- 1000
    
    # step, which is calculated by max size (at zoom 7) minus default size (zoom 10) divided by
    # 10 - 7 = 3 steps
    # step_zoom_increase <- dplyr::case_match(
    #   data_reactives$plot_origin,
    #   # Matollar, bigger points as they are scattered dn difficult to see
    #   "S" ~ ((12500 - default_size) / 3),
    #   # Parks, bigger than normal, smaller than matollar because there is more closer points
    #   c("O", "A", "PN") ~ ((4000 - default_size) / 3),
    #   # default 1000, for the rest of origins
    #   .default = ((3000 - default_size) / 3)
    # )
    step_zoom_increase <- ((3000 - default_size) / 3)
    
    current_zoom <- dplyr::case_when(
      input$map_daily_zoom < 7 ~ 7,
      input$map_daily_zoom > 10 ~ 10,
      .default = input$map_daily_zoom
    )
    
    # size calculation
    size_transformed <- default_size + ((10 - current_zoom) * step_zoom_increase)
    
    return(size_transformed)
  })
  
  ## observers ####
  # data and points update (also polygons to plot)
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
    palette_domain <- c(
      palettes_dictionary[[var_daily_sel]]$min,
      palettes_dictionary[[var_daily_sel]]$max
    )
    viz_pal_reverse_sel <- data_reactives$viz_pal_reverse
    viz_pal_reverse_default <- palettes_dictionary[[var_daily_sel]]$rev
    # if sel and default are the same (FALSE-FALSE or TRUE-TRUE), then don't reverse the palette (FALSE)
    # because is not needed. If they are different (TRUE-FALSE, FALSE-TRUE), then change the palette
    # (TRUE), because default or user want it changed.
    viz_pal_reverse <- !identical(viz_pal_reverse_sel, viz_pal_reverse_default)
    
    if (var_daily_sel %in% c("Precipitation", "LFMC", "DFMC", "PET")) {
      palette_domain <- c(
        0, max(data_map[[var_daily_sel]], na.rm = TRUE)
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
        reverse = viz_pal_reverse, na.color = 'transparent'
      ),
      "high" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          palettes_dictionary[[var_daily_sel]]$pal(9), c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        ),
        domain = palette_domain,
        reverse = viz_pal_reverse, na.color = 'transparent'
      ),
      "normal" = leaflet::colorNumeric(
        palettes_dictionary[[var_daily_sel]]$pal(256),
        domain = palette_domain,
        reverse = viz_pal_reverse,
        na.color = 'transparent'
      )
    )
    
    pal_legend <- switch(
      viz_pal_config,
      "low" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          palettes_dictionary[[var_daily_sel]]$pal(9), c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
        ),
        domain = palette_domain,
        reverse = !viz_pal_reverse, na.color = 'transparent'
      ),
      "high" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          palettes_dictionary[[var_daily_sel]]$pal(9), c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        ),
        domain = palette_domain,
        reverse = !viz_pal_reverse, na.color = 'transparent'
      ),
      "normal" = leaflet::colorNumeric(
        palettes_dictionary[[var_daily_sel]]$pal(256),
        domain = palette_domain,
        reverse = !viz_pal_reverse,
        na.color = 'transparent'
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
    
    # polygons
    admin_divs <- switch(
      data_reactives$plot_origin,
      "T" = all_polygons,
      "A" = aiguestortes,
      "PN" = parques,
      "O" = ordesa,
      "P" = catalunya, 
      "S" = provincias
    )
    
    color_polygon <- dplyr::case_match(
      data_reactives$plot_origin,
      c("A", "PN", "O") ~ "#c41606",
      .default = "grey"
    )
    
    
    add_parks_perimeters <- function(map, origin) {
      
      # get perimeters if any
      park_perimeters <- switch(
        origin,
        "A" = peri_aiguestortes,
        "PN" = peri_total,
        "O" = peri_ordesa,
        NULL
      )
      
      # if no perimeter, return the map
      if (is.null(park_perimeters)) {
        return(map)
      }
      
      # add polygons for the perimeter, with less weight and more transparency
      map |>
        leaflet::addPolygons(
          data = park_perimeters,
          opacity = 0.3,
          weight = 1,
          fill = FALSE,
          color = "#004202",
          group = "plots_layer",
          options = leaflet::pathOptions(pane = "admin_divs")
        )
    }
    
    # Update the map with points and polygons
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
      leaflet::addPolygons(
        data = admin_divs,
        opacity = 0.7,
        weight = 1.5,
        fill = FALSE,
        color = color_polygon,
        group = "plots_layer",
        options = leaflet::pathOptions(pane = "admin_divs")
      ) |>
      add_parks_perimeters(data_reactives$plot_origin) |>
      leaflet::clearControls() |>
      leaflet::addLegend(
        position = "bottomright",
        title = translate_app(var_daily_sel, lang()),
        pal = pal_legend,
        values = c(palette_domain[1], data_map[[var_daily_sel]], palette_domain[2]),
        labFormat = leaflet::labelFormat(transform = function(x) {sort(x, decreasing = TRUE)}),
        na.label = '',
        opacity = 1
      )
  })
  
  # tab update
  shiny::observeEvent(
    eventExpr = input$map_daily_shape_click,
    handlerExpr = {
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    }
  )
  
  # set view reset
  shiny::observeEvent(
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    eventExpr = data_reactives$plot_origin,
    handlerExpr = {
      
      setview_options <- switch(
        data_reactives$plot_origin,
        "P" = list(lng = 1.7458675, lat = 41.6922353, zoom = 8),
        "PN" = list(lng = 0.502806, lat = 42.533565, zoom = 10),
        "A" = list(lng = 0.9313699, lat = 42.5709769, zoom = 11),
        "O" = list(lng = 0.0519259, lat = 42.6598781, zoom = 11),
        list(lng = 2.2018256, lat = 41.089058, zoom = 7)
      )
      
      # update map with the corresponding view
      leaflet::leafletProxy("map_daily") |>
        leaflet::setView(
          lng = setview_options[["lng"]],
          lat = setview_options[["lat"]],
          zoom = setview_options[["zoom"]]
        )
    }
  )
  
  # returning inputs
  # reactive values to use in app and other modules
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$map_daily_shape_click <- input$map_daily_shape_click
    map_reactives$map_daily_zoom <- input$map_daily_zoom
    map_reactives$map_daily_shape_mouseover <- input$map_daily_shape_mouseover
  })
  return(map_reactives)
}
