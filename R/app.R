#' SiteDrought app function
#' @export
sitedrought_app <- function() {
  
  # DB access ---------------------------------------------------------------
  sddb <- lfcdata::siteDrought()
  
  # JS code needed ----------------------------------------------------------
  # keep alive to avoid shinyproxy and shiny grey screen
  keep_alive_script <- shiny::HTML(
    " var socket_timeout_interval;
      var n = 0;

      $(document).on('shiny:connected', function(event) {
        socket_timeout_interval = setInterval(function() {
          Shiny.onInputChange('alive_count', n++)
        }, 10000);
      });

      $(document).on('shiny:disconnected', function(event) {
        clearInterval(socket_timeout_interval)
      }); "
  )
  
  # Language input ----------------------------------------------------------
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'sitedroughtapp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )
  
  # UI ------------------------------------------------------------------------------------------
  ui <- shiny::tagList(
    
    # needed initializations
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    # waiter::use_hostess(),
    
    # show waiter on load
    waiter::waiter_show_on_load(
      color = 'transparent',
      html = waiter::spin_chasing_dots()
    ),
    
    # head
    shiny::tags$head(
      # js scripts
      shiny::tags$script(keep_alive_script),
      # ci and custom css
      shiny::includeCSS(system.file("apps_css", "corp_image.css", package = "lfcdata")),
      shiny::includeCSS(system.file("apps_css", "sitedrought.css", package = "lfcdata"))
    ),
    
    navbarPageWithInputs(
      # opts
      title = 'SiteDrought App',
      id = 'nav', collapsible = TRUE,
      
      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),
      
      # footer
      footer = shiny::tags$footer(
        shiny::fluidRow(
          shiny::column(
            width = 12, align = "right",
            shiny::HTML(glue::glue(
              '<img src="images/emf_white_logo.svg" width="120px" class="d-inline-block" alt="" loading="lazy">
              <img src="images/creaf_white_logo.svg" width="135px" class="d-inline-block" alt="" loading="lazy">
              <span>({lubridate::year(Sys.Date())})</span>'
            ))
          )
        )
      ),
      
      # navbarPage contents
      # main tab
      shiny::tabPanel(
        title = mod_tab_translateOutput("main_tab_translation"),
        
        # sidebar with modules
        shiny::sidebarLayout(
          sidebarPanel = shiny::sidebarPanel(
            width = 4,
            # this is gonna be a tabsetPanel, for data sel/viz, saving and help.
            # tabsetpanel
            shiny::tabsetPanel(
              id = "sidebar_tabset", type = "pills",
              # data panel
              shiny::tabPanel(
                title = mod_tab_translateOutput("data_translation"),
                value = "data_inputs_panel",
                mod_dataInput("mod_dataInput")
              ),
              # save panel
              shiny::tabPanel(
                title = mod_tab_translateOutput("save_translation"),
                value = "save_panel",
                mod_saveOutput("mod_saveOutput")
              ),
              # help panel
              shiny::tabPanel(
                title = mod_tab_translateOutput("help_translation"),
                value = "help_panel",
                mod_helpOutput("mod_helpOutput")
              )
            )
          ), # end of sidebarPanel
          # main panel, for map and time series, so a tabset panel. We also use an overlay div for
          # placing waiter output
          mainPanel = shiny::mainPanel(
            width = 8,
            shiny::div(
              id = "overlay_div",
              ########################################################### debug ####
              # shiny::absolutePanel(
              #  id = 'debug', class = 'panel panel-default', fixed = TRUE,
              #  draggable = TRUE, width = 640, height = 'auto',
              #  # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
              #  # top = 'auto', left = 'auto', right = 100, bottom = 100,
              #  top = 60, left = 'auto', right = 50, bottom = 'auto',
              # 
              #  shiny::textOutput('debug1'),
              #  shiny::textOutput('debug2'),
              #  shiny::textOutput('debug3')
              # ),
              ####################################################### end debug ####
              shiny::tabsetPanel(
                id  = "main_panel_tabset", type = "pills",
                # map
                shiny::tabPanel(
                  title = mod_tab_translateOutput("map_translation"),
                  value = "map_panel",
                  mod_mapOutput("mod_mapOutput")
                ),
                # ts
                shiny::tabPanel(
                  title = mod_tab_translateOutput('series_tab_translation'),
                  value = 'series_panel',
                  mod_tsOutput('mod_tsOutput')
                )
              )
            )
          ) # end of mainPanel
          
          
          
        ) # end of sidebarLayout
      ) # end of main tab
      
      # technical specifications tab
      # commented for now until new tech specs are written
      # shiny::tabPanel(
      #   title = mod_tab_translateOutput('tech_specs_translation'),
      #   value = 'tech_spec_panel',
      #   mod_techSpecsOutput('mod_techSpecsOutput')
      # )
    ) # enf od navbar 
  ) # end of UI
  
  
  # Server --------------------------------------------------------------------------------------
  server <- function(input, output, session) {
    
    # lang reactive
    lang <- shiny::reactive({
      input$lang
    })
    
    # hostess init
    waiter::waiter_update(
      html = shiny::tagList(
        shiny::br(), shiny::br(),
        waiter::hostess_loader(
          "loader",
          svg = 'images/hostess_image.svg',
          progress_type = 'fill',
          fill_direction = 'btt',
          center_page = TRUE
        ),
        shiny::br(), shiny::br(),
        shiny::h4("Estem carregant 6,5 milions de registres (un any de dades diàries), això trigarà uns quants minuts (3~5 minuts)")
      )
    )
    hostess_init <- waiter::Hostess$new('loader', infinite = TRUE)
    hostess_init$start()
    # close init
    on.exit(hostess_init$close(), add = TRUE)
    on.exit(waiter::waiter_hide(), add = TRUE)
    
    # Static data
    # Main table is static, we only need to load it once, so better at the
    # beginning. We also calculate the min and max date already
    tictoc::tic()
    main_data <- sddb$get_data()
    main_date_range <- c(
      max = main_data$date |> max(na.rm = TRUE),
      min = main_data$date |> min(na.rm = TRUE)
    )
    tictoc::toc()
    
    # send an alarm when loading app or changes langs
    # shiny::observeEvent(
    #   lang(),
    #   {
    #     shinyWidgets::show_alert(
    #       title = translate_app('under_construction_title', lang()),
    #       text = translate_app('under_construction_text', lang())
    #     )
    #   }
    # )
    
    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   map_reactives$map_daily_shape_click
    # })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_daily_zoom
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_daily_shape_mouseover
    # })
    
    # modules
    # data input
    data_reactives <- shiny::callModule(mod_data, "mod_dataInput", main_data, main_date_range, lang)
    # map
    map_reactives <- shiny::callModule(
      mod_map, "mod_mapOutput", data_reactives, main_data_reactives, session, lang
    )
    # main_data reactives
    main_data_reactives <- shiny::callModule(
      mod_mainData, "mod_mainDataOutput", data_reactives, map_reactives, main_data, lang
    )
    # ts
    shiny::callModule(mod_ts, 'mod_tsOutput', data_reactives, main_data_reactives, map_reactives, lang)
    # help
    shiny::callModule(mod_help, "mod_helpOutput", main_data_reactives, lang)
    # save
    shiny::callModule(mod_save, 'mod_saveOutput', data_reactives, main_data_reactives, main_data, lang)
    # tech specs
    shiny::callModule(mod_techSpecs, 'mod_techSpecsOutput', lang)
    
    # translation modules
    c(
      "main_tab_translation", "data_translation", "save_translation", "help_translation",
      "map_translation", "series_tab_translation", "tech_specs_translation"
    ) |>
      purrr::map(
        .f = \(id) {
          shiny::callModule(mod_tab_translate, id, id, lang)
        }
      )
    
  } # end of server
  
  # run the app
  sitedroughtapp <- shiny::shinyApp(ui = ui, server = server)
  return(sitedroughtapp)
}