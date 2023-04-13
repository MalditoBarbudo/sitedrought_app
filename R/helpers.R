# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)


navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {
  
  if (length(id) > 1) {
    res <- purrr::map_chr(
      id,
      .f = \(.id) {
        translate_app(.id, lang)
      }
    )
    return(res)
  }
  
  id_row <- language_dictionary |>
    dplyr::filter(text_id == id)
  
  if (nrow(id_row) < 1) {
    return("")
  }
  
  return(dplyr::pull(id_row, glue::glue("translation_{lang}")))
}

 
translate_thesaurus_app <- function(id, lang, type) {
  
  if (length(id) > 1) {
    res <- purrr::map_chr(
      id,
      .f = \(.id) {
        translate_thesaurus_app(.id, lang, type)
      }
    )
    return(res)
  }
  
  var_row <- sitedrought_var_thes |>
    dplyr::filter(var_id == id)
  
  # check if found something, if not return empty string
  if (nrow(var_row) < 1) {
    return("")
  }
  
  # return the desired translation based on type and lang
  return(dplyr::pull(var_row, glue::glue("var_{type}_{lang}")))
}





# ........ FUNCION CALL MODULES ..........
# ...........................................

#       .) Cada elemento de la APP a traducir
#       .) Necesita un CALLMODULE
#       .) Hago una FUNCIÓN para automatizar y no reescribir de nuevo

#       .) Para pasar de un STRING a CODE R usamos
#       .) GLUE + EVAL + PARSE (TEXT) + EVAL

callModule_function <- function(tabs,lang){
  
  tabs <- tabs
  
  # Función CREA CallModule de STRING a R CODE
  CM <- function(tabs,a){
    glue::glue("
        shiny::callModule(
          mod_tab_translate, '",tabs[a],"',
          '",tabs[a],"', lang
        )") %>%
          eval() %>%
           parse(text = .) %>%
             eval()
  }
  
  
  # Repetición de TODOS los CALL MODULES
  for (i in 1:length(tabs)) {
    CM(tabs,i)
    
  }
  
}








