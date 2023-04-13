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
#' @param data_reactives,main_data_reactives necessary reactives form other modules
#' @param main_data main_data table
#'
#' @param lang lang reactive
#'
#' @export
mod_save <- function(
  input, output, session,
  data_reactives, main_data_reactives, main_data,
  lang
) {

  # renderUI ####
  output$mod_save_container <- shiny::renderUI({
    
    ns <- session$ns
    lang_declared <- lang()
    
    # renderUI
    shiny::tagList(
      shiny::fluidRow(
        align = "center", shiny::h3(translate_app("save_main_label", lang_declared))
      ),
      shiny::br(), shiny::br(),
      shiny::fluidRow(
        shiny::fluidRow(
          align = 'center',
          shinyWidgets::prettyRadioButtons(
            ns("data_columns"), translate_app("data_colum_label", lang_declared),
            choices = purrr::set_names(
              c("col_vis", "col_all"),
              translate_app(c("col_vis", "col_all"), lang_declared)
            ),
            status = 'success', fill = TRUE, shape = 'round'
          ),
          shinyWidgets::prettyRadioButtons(
            ns("data_day"), translate_app("select_by_date", lang_declared),
            choices = purrr::set_names(
              c("day", "ddbb"),
              translate_app(c("day", "ddbb"), lang_declared)
            ),
            status = 'success', fill = TRUE, shape = 'round'
          ),
          shinyWidgets::prettyRadioButtons(
            ns("data_format"), translate_app("select_format_label", lang_declared),
            choices = purrr::set_names(
              c("csv", "gpkg"),
              translate_app(c("csv", "gpkg"), lang_declared)
            ),
            status = 'success', fill = TRUE, shape = 'round'
          )

        ), # end fluid row
        shiny::br(), shiny::br(),
        shiny::fluidRow(
          align = 'center',
          shiny::downloadButton(ns("table_save"), translate_app("save_table_button",lang_declared))
        )
      ) # end fluid row  
    ) # end of tagList
  })

  ## Downolad handlers
  output$table_save <- shiny::downloadHandler(
    filename = function() {
      
      data_columns_sel <- input$data_columns
      data_day_sel <- input$data_day
      data_format_sel <- input$data_format
      
      if (data_columns_sel == "col_vis") {
        data_columns_sel <- data_reactives$var_daily
      } else {
        data_columns_sel <- "data"
      }
      
      if (data_day_sel == "day") {
        data_day_sel <- as.character(data_reactives$date_daily) |>
          stringr::str_remove_all("-")
      } else {
        data_day_sel <- "all_dates"
      }
      
      file_name <- glue::glue(
        "sitedrought_{data_columns_sel}_{data_day_sel}.{data_format_sel}"
      )
      
      return(file_name)
    },
    # content function
    content = function(file) {
      
      data_columns_sel <- input$data_columns
      data_day_sel <- input$data_day
      data_format_sel <- input$data_format
      
      if (data_columns_sel == "col_vis") {
        data_columns_sel <- c("plot_id", "date", "plot_origin", data_reactives$var_daily)
      } else {
        data_columns_sel <- names(main_data)
      }
      
      data_little_helper <- function(data_day_sel) {
        if (data_day_sel == "ddbb") {
          return(main_data)
        }
        return(main_data_reactives$data_day)
      }
      
      writing_little_helper <- function(object, data_format_sel, file) {
        if (data_format_sel == "csv") {
          readr::write_csv(object, file)
        } else {
          sf::st_write(object, file)
        }
        return(invisible(TRUE))
      }
      
      data_little_helper(data_day_sel) |>
        dplyr::select(dplyr::any_of(data_columns_sel)) |>
        writing_little_helper(data_format_sel, file)
    }
  )
  
  
  # datasetInput <- reactive({
  #   
  #   fecha <- data_reactives$fecha_reactive
  #   sf <- main_data_reactives$data_day  
  # 
  #   variable <- data_reactives$variable_reactive
  #   num_i <- as.numeric(match(variable,names(sf)))
  #   selected_var <- as.symbol(names(sf)[num_i])
  #   
  #   
  #   
  #   if( input$data_columns == "col_all" ) {
  #     sf_data_columns <- sf  %>%
  #       dplyr::filter(date == fecha) %>%
  #       dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
  #                     lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
  #                     lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
  #                     lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
  #       )
  #   } else if (input$data_columns == "col_vis") {
  #     sf_data_columns <- sf %>%
  #       dplyr::filter(date == fecha) %>%
  #       dplyr::select(plot_id, selected_var, date, plot_origin) %>%  
  #       dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
  #                     lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
  #                     lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
  #                     lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
  #       )
  #   } 
  #   
  #   if(input$data_format == "gpkg"){
  #     sf_data_columns %>% data.frame()
  #   } else {
  #     sf_data_columns %>% data.frame() %>% dplyr::select(-geometry)
  #   }
  # 
  #   
  # })
  # 
  # 
  # # ..............  DOWNLOAD all DATABASE .............
  # # ...................................................
  # 
  # #      .) Función para download TODA la BBDD
  # 
  # #      .) PRIMER IF 
  # #               .) Tanto si seleccionamos ALL Colums / Columna VISIBLE
  # #               .) Se descarga TODA la BBDD
  # #               .) Creamos 4 columna Lat / Lon
  # 
  # #      .) SEGUNDO IF
  # #               .) Si NO es GPKG
  # #                  .) Eliminamos columna GEOMETRY
  # #                  .) Ya que en el EXCEL y CSV no aporta info
  # #               .) Si es GPKG
  # #                  .) Manetnemos columna GEOMETRY
  # #                  .) Es necesaria para el GPKG
  # 
  # databaseInput <- reactive({
  #   
  #   sf <- main_data_reactives$data_day
  # 
  #   if( input$data_columns == "col_all" | input$data_columns == "col_vis" ) {
  #     sf_data <- sf  %>%
  #       dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
  #                     lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
  #                     lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
  #                     lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
  #       )
  #   } 
  # 
  # 
  #   if(input$data_format == "gpkg"){
  #     sf_data %>% data.frame()
  #   } else {
  #     sf_data %>% data.frame() %>% dplyr::select(-geometry)
  #   }
  #   
  #   
  # })
  # 
  # 
  # # %%%%%%%%%%%%%%%%  DOWNLOADHANDLER  %%%%%%%%%%%%%%%%%
  # # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 
  # #      .) NECESSITA
  # #           .) FILENAME Function  => crea ek nombre de archivo
  # #           .) CONTENT  Function  => f(x) DOWNLOAD
  # 
  # #      .) FILENAME
  # #           .) Crearemos 3 f(x) que nos ayudaran
  # #              .) FORMAT_SELECTED   
  # #              .) COLUMN_SELECTED   
  # #              .) DATE_STR
  # 
  # 
  # 
  # 
  # # ............... f(x) FROMATO .................
  # # ..............................................
  # 
  # #      .) Función que asigna EXTENCIÓN del archivo
  # #      .) CSV o XLSX
  # 
  # format_selected <- function() {
  #   switch (input$data_format,
  #           "csv" = format <- ".csv", 
  #           "gpkg" = format <- ".gpkg"
  #   )
  # }
  # 
  # # ............... f(x) COLUMNA .................
  # # ..............................................
  # 
  # #      .) Función que asigna parte del nombre
  # #      .) Del archivo a descargar
  # 
  # column_selected <- function() {
  #   
  #   if(input$data_format == "gpkg") {
  #     
  #     switch (input$data_columns,
  #             "col_vis" = format <- "_map_specific_columns", 
  #             "col_all" = format <- "_map_all_columns" 
  #     )
  #     
  #   } else {
  #     
  #     switch (input$data_columns,
  #             "col_vis" = format <- "_specific_columns", 
  #             "col_all" = format <- "_all_columns" 
  #     )
  #   }
  #   
  # }
  # 
  # # ............... f(x) DAY .....................
  # # ..............................................
  # 
  # #      .) Función que asigna DIA o AÑO
  # #      .) Del archivo a descargar
  # 
  # day_selected <- function() {
  #   
  #   switch (input$data_day,
  #           "day" = format <- "_day", 
  #           "ddbb" = format <- "_year" 
  #   )
  # 
  # }
  # 
  # 
  # # ........... f(x) DATA to STRING ..............
  # # ..............................................
  # 
  # #      .) Función que asigna crea STRING DATE
  # #      .) Une Year/month/day en un solo STRING sin guiones
  # 
  # date_str <- toString(Sys.Date()) %>% 
  #   stringr::str_split(.,"-") %>% .[[1]] %>%
  #   stringr::str_c(., collapse = "")
  # 
  # 
  # 
  # # %%%%%%%%%%%%%%%%  DOWNLOADHANDLER  %%%%%%%%%%%%%%%%%
  # # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 
  # 
  # #      .) Es un OUTPUT
  # #      .) Apunta al ID = TABLE_SAVE
  # #      .) ESTRUCUTRA:
  # #               .) FILENAME Function  => crea nombre de archivo
  # #               .) CONTENT  Function  => f(x) DOWNLOAD 
  # 
  # 
  # 
  #  output$table_save <- downloadHandler(
  #   filename = function() {
  #     
  #     paste(date_str,day_selected(),column_selected(),format_selected(), sep = "")
  #    
  #   },
  #   content = function(file) {
  #     
  #     # ....... WRITE TYPE ...........
  #     # .............................
  #     
  #     #      .) en f(x) del formato
  #     #      .) lo DESCARGA en diferentes formatos (CSV, XLSX, GPKG)
  #     
  #     #      .) PRIMERO Diferenciamos
  #     #              .) SI es DDBB => Descarga TODA la BBDD solo en CSV
  #     #              .) NO es DDBB => Puedes descargar en CSV, XLSL o GPKG
  #     
  #     if (input$data_day == "ddbb" ) {
  #       
  #       
  #       # ....... WAITER / HOSTESS ..........
  #       # ...................................
  #       
  #       #       .) El descargar TODA la BBDD en CSV tarda 2 min o mas
  #       #       .) Por eso usamos el WAITER para que el usuario vea la progresión del procesos
  #       
  #       #       .) PRIMERO:
  #       #              .) creamos OBJECTO con la classe = WAITER::HOSTESS$new
  #       #       .) SEGUNDO
  #       #              .) SET_LOADER  
  #       #              .) Especificamos: image SVG, tipo progress y fill direction
  #       
  #       
  #       
  #       hostess_plots <- waiter::Hostess$new(infinite = TRUE)
  #       hostess_plots$set_loader(waiter::hostess_loader(
  #         svg = 'images/hostess_image.svg',
  #         progress_type = 'fill',
  #         fill_direction = 'btt'
  #       ))
  #       
  #       # ....... WAITER / HOSTESS ..........
  #       # ...................................
  #       
  #       #       .) TERCERO:
  #       #       .) Definir LUGAR de aparición = ID ( en APP.R / mainPanel)
  #       #       .) Definir => Get_Loader() definido anteriormente + HTML H3 + P
  #       
  #       
  #       waiter_ddbb <- waiter::Waiter$new(
  #         id = 'overlay_div',
  #         html = shiny::tagList(
  #           hostess_plots$get_loader(),
  #           shiny::h3(translate_app("progress_ddbb", lang())),
  #           shiny::p(translate_app("progress_detail_plots", lang()))
  #         ),
  #         color = "#E8EAEB"  # color del fondo
  #       )
  #       
  #       #       .) CUARTO: Show WAITER + Star HOSTESS
  #       #       .) QUINTO: Definir EXIT Hostess / WAITER 
  #       
  #       
  #       waiter_ddbb$show()
  #       hostess_plots$start()
  #       on.exit(hostess_plots$close(), add = TRUE)
  #       on.exit(waiter_ddbb$hide(), add = TRUE)
  #       
  #      
  #       # ...... FUNCIÓN CREAR CSV ..........
  #       # ...................................
  #       
  #       #       .) Función de crear CSV
  #       #       .) Tarda unos 2 min
  #       #       .) Por esto añadimos previamente el WAITER
  #       
  #       # write.csv(databaseInput(), file, row.names = FALSE)
  #       
  #       switch (input$data_format,
  #               "csv" = write.csv(databaseInput(), file, row.names = FALSE),
  #               "gpkg" = sf::st_write(databaseInput(), file)
  #       )
  #       
  #     } else {
  #       
  #       switch (input$data_format,
  #               "csv" = write.csv(datasetInput(), file, row.names = FALSE),
  #               # "xlsx" = writexl::write_xlsx(datasetInput(), file),
  #               "gpkg" = sf::st_write(datasetInput(), file)
  #       )
  #     }
  # 
  # 
  #   }
  # )
   

}



