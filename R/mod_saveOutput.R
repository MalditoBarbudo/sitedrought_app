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
      
      # we need a waiter to show the progress due to size of files when saving whole database
      hostess_plots <- waiter::Hostess$new(infinite = TRUE)
      hostess_plots$set_loader(
        waiter::hostess_loader(
          svg = 'images/hostess_image.svg',
          progress_type = 'fill',
          fill_direction = 'btt'
        )
      )
      
      waiter_ddbb <- waiter::Waiter$new(
        id = 'overlay_div',
        html = shiny::tagList(
          hostess_plots$get_loader(),
          shiny::h3(translate_app("progress_ddbb", lang())),
          shiny::p(translate_app("progress_detail_plots", lang()))
        ),
        color = "#1C1C20"
      )
      
      waiter_ddbb$show()
      hostess_plots$start()
      on.exit(hostess_plots$close(), add = TRUE)
      on.exit(waiter_ddbb$hide(), add = TRUE)
      
      
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
}
