#' function to launch the siteDrought app
#'
#' @importFrom magrittr %>%
#'
#' @export
siteDrought_app <- function() {
  
  ### DB access ################################################################
  siteDroughtdb <- lfcdata::siteDrought()

  ## JS code needed ############################################################
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



  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'siteDroughtApp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )
  
   
  # ++++++++++++++++++++++++++++++++ //  UI // ++++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## UI ####

  ui <- shiny::tagList(

    # ............... INICIALIZAR ................
    # ............................................

    #       .) SHINYJS / Waiter / Hostess
    #       .) Head:
    #             .) JS files
    #             .) CSS files

    # shinyjs
    shinyjs::useShinyjs(),
    # waiter
    waiter::use_waiter(),
    waiter::use_hostess(),

    # css
  
    shiny::tags$head(
      # js script,
      shiny::tags$script(keep_alive_script),
      
      # custom css
      shiny::includeCSS(system.file('resources', 'siteDrought.css', package = 'siteDroughtApp')),
      # corporative image css
      shiny::includeCSS(system.file('resources', 'corp_image.css', package = 'siteDroughtApp')),
      # setting css sitedrought
      shiny::includeCSS(system.file('resources','siteDrought_settings.css', package = 'siteDroughtApp')),
      
      # # custom css
      # shiny::includeCSS("inst/resources/siteDrought.css"),
      # # corporative image css
      # shiny::includeCSS("inst/resources/corp_image.css"),

      # # responsive css
      # shiny::includeCSS("inst/resources/siteDrought_responsive_v2.css"),
      
    ),
    
  
    # **************************************************************************************
    # ------------------------------  //  NAVBAR   // --------------------------------------
    # **************************************************************************************

    #       .) Titulo
    #       .) 2 Pesta??as   = tabPanel
    #                 .) Explora
    #                 .) Especificaci??n T??cnica
    #       .) Dropdown Lenguas  =pickerInput
    

    navbarPageWithInputs(
      # opts
      title = 'SiteDrought App',
      id = 'nav', collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector

      # ............ Dropdown Lenguas ..............
      # ............................................

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
      
      
      
      # **************************************************************************************
      # ----------------------  //  Pesta??a PLOTS Data_Day   // -------------------------------
      # **************************************************************************************
      
      #       .) Tiene dos parte:
      #       .) MEN?? Izq = sidebarPanel
      #       .) MAPA     = mainPanel BLUE
      
      
      # navbarPage contents
      shiny::tabPanel(
        title = mod_tab_translateOutput('main_tab_translation'),
        shiny::sidebarLayout(
          ## options
          # position = 'left', fluid = TRUE,

          # //////////////////////////////////////////////
          # ------------    MEN?? IZQUIERDA   -------------
          # //////////////////////////////////////////////

          #       .) 3 Pesta??as
          #              .) DATOS
          #              .) GUARDAR
          #              .) AYUDA

          sidebarPanel = shiny::sidebarPanel(
            width = 4,
            # this is gonna be a tabsetPanel, for data selection, save and help.
            # tabset panel
            shiny::tabsetPanel(
                  # id = 'sidebar_tabset', type = 'pills',
                  id = 'menu_izq', type = 'pills',

                  # ............... Pesta??a PLOTS ..............
                  # ............................................
                  
                  #       .) Pesta??a que visualiza PLOTS
                  #       .) En funci??n de FECHA / VARIABLE
                  
                  
                  # data panel
                  shiny::tabPanel(
                    title = mod_tab_translateOutput('data_translation'),
                    value = 'data_inputs_panel',
                    mod_dataInput('siteDrought_DATA')
                  ), # end of data panel

                  # .............. Pesta??a GUARDAR .............
                  # ............................................
                  
                  #       .) Pesta??a para GUARDAR
                  #       .) Guardamos lo visualizado
                  #       .) En diferente formato
                  
                  # save panel
                  shiny::tabPanel(
                    title = mod_tab_translateOutput('save_translation'),
                    value = 'save_panel',
                    mod_saveOutput('mod_saveOutput')
                  ),
                  
                  # .............. Pesta??a AYUDA ...............
                  # ............................................
                  
                  #       .) Pesta??a de AYUDA
                  #       .) Ofrece descripci??n de diferentes VARIABLES
                  #       .) Es una descripci??n corta
                  
                  # help panel
                  shiny::tabPanel(
                    # title = mod_tab_translateOutput('help_translation'),
                    title = mod_tab_translateOutput('help_translation'),
                    value = 'help_panel',
                    mod_helpInput('help_data')
                  )
            )
          ), # end of sidebarPanel
          
          # //////////////////////////////////////////////
          # -------------    MAPA DERECHA   --------------
          # //////////////////////////////////////////////

          #       .) 2 Pesta??as
          #                 .) MAPA
          #                 .) SERIES TEMPORALS

          mainPanel = shiny::mainPanel(
            width = 8,
            shiny::div(
              id = 'overlay_div',
              shiny::tabsetPanel(
                id = 'main_panel_tabset_plots', type = 'pills',

                # ......... MAPA .........
                # ........................
                shiny::tabPanel(
                  # 'map',
                  title = mod_tab_translateOutput('map_translation'),
                  value = 'map_panel',
                  mod_mapOutput('mod_mapOutput')
                ),

                # .... SERIE TEMPORAL ....
                # ........................
                shiny::tabPanel(
                  title = mod_tab_translateOutput('series_tab_translation'),
                  value = 'series_panel',
                  mod_tsOutput('mod_tsOutput')
                )
              )
            )
          )
        )
      ),   

      # .................. Pesta??a ESPECIFICACIONES TECNICAS ...................
      # ........................................................................

      #       .) Tiene UNA parte:
      #       .) Ecplicacion de la APP

      shiny::tabPanel(
        title = mod_tab_translateOutput('tech_specs_translation'),
        value = 'tech_spec_panel',
        mod_techSpecsOutput('mod_techSpecsOutput')
        
        
      )


    ) # end of navbar
  ) # end of UI


  # ++++++++++++++++++++++++++++++ // SERVER // +++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## SERVER ####
  server <- function(input, output, session) {

    # .......... LANGUAGE REACTIVE ...............
    # ............................................

    #       .) Detecta el cambio de Lenguaje
    #       .) Lo M??dulos depende este

    # lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    # ............... ACTIVAR FUNCIONES DE MODULOS .................
    # ..............................................................

    # ....... DATA INPUTS ..........
    # ..............................

    #       .) siteDrought_data  = f(x) a LLAMAR ( principal del M??dulo )
    #       .) siteDrought_DATA  = Es el ID usaremos a la UI cuando llamemos la f(x)
    #       .) siteDroughtdb     = DDBB del LfcData
    #       .) Lang          = lenguaje del REACTIVE

    
    # ....... 1ra PESTA??A ..........
    # ..............................
    
    # data_input
    data_reactives <- shiny::callModule(
      siteDrought_data ,'siteDrought_DATA', siteDroughtdb, 
      lang,  main_data_reactives
     
    )
    
    # help_input
    help_reactives <- shiny::callModule(
      help_data ,'help_data', siteDroughtdb, 
      lang,  main_data_reactives
      
    )
    
    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapOutput',
      data_reactives, main_data_reactives,
      session, lang
    )
    
    # main data
    main_data_reactives <- shiny::callModule(
      mod_mainData, 'mod_mainDataOutput',
      data_reactives, map_reactives,
      siteDroughtdb, lang
    )
    
   
    # ....... TIMESERIES ...........
    # ..............................
    
    timseries_reactives <- shiny::callModule(
      mod_ts, 'mod_tsOutput',
      data_reactives, main_data_reactives,
      lang
    )
    
    # ........... SAVE .............
    # ..............................
    
    shiny::callModule(
      mod_save, 'mod_saveOutput',
      main_data_reactives, data_reactives, lang
    )
    
    # technical specifications module
    shiny::callModule(
      mod_techSpecs, 'mod_techSpecsOutput',
      lang
    )

     
    # ..... TABS TRANSLATIONS ......
    # ..............................
    
    #       .) Uso la funci??n => CALL_MODULE_FUNCTION
    #       .) Funci??n creada en HELPERS.R
    
    #       .) Necesita 2 ATRIBUTOS 
    #                 .) TABS = Todas la etiquetas a traducir
    #                 .) LANG = Lengua seleccionada
    
    
    # TODAS las etiquetas a TRADUCIR
    tabs <- c('main_tab_translation','data_translation',
              'map_translation','series_tab_translation','save_translation',
              'save_translation','tech_specs_translation','help_translation',
              'mod_saveOutput')
    
    # Funcion que llama a TODOS los CALL MODULES
    callModule_function(tabs,lang)
    
  

  } # end of server function

  # Run the application
  siteDroughtApp <- shiny::shinyApp(
    ui = ui, server = server
  )

  # shiny::runApp(nfi_app)
  return(siteDroughtApp)

}


