# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)


navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {
  
  # recursive call for vectors
  if (length(id) > 1) {
    res <- purrr::map_chr(
      id,
      .f = \(.id) {
        translate_app(.id, lang)
      }
    )
    return(res)
  }
  
  # get id translations
  id_row <- language_dictionary |>
    dplyr::filter(text_id == id)
  
  # return raw id if no matching id found
  if (nrow(id_row) < 1) {
    return(id)
  }
  
  # get the lang translation
  return(dplyr::pull(id_row, glue::glue("translation_{lang}")))
}

 
translate_thesaurus_app <- function(id, lang, type) {
  
  # recursive call for id vectors
  if (length(id) > 1) {
    res <- purrr::map_chr(
      id,
      .f = \(.id) {
        translate_thesaurus_app(.id, lang, type)
      }
    )
    return(res)
  }
  
  # get id translations
  var_row <- sitedrought_var_thes |>
    dplyr::filter(var_id == id)
  
  # check if found something, if not return raw id
  if (nrow(var_row) < 1) {
    return(id)
  }
  
  # return the desired translation based on type and lang
  return(dplyr::pull(var_row, glue::glue("var_{type}_{lang}")))
}

# we repeat several times the creation and translation of variables list, so se create a function
# to do it
create_var_list_for_input <- function(lang_declared) {
  list(
    c("REW","DDS") |> 
      purrr::set_names(translate_app(c("REW","DDS"), lang_declared)),
    c("PET", "Precipitation") |> 
      purrr::set_names(translate_app(c("PET", "Precipitation"), lang_declared)),
    c("LFMC","DFMC","SFP","CFP") |> 
      purrr::set_names(translate_app(c("LFMC","DFMC","SFP","CFP"), lang_declared)),
    c("REW_q","DDS_q","LFMC_q") |> 
      purrr::set_names(translate_app(c("REW_q","DDS_q","LFMC_q"), lang_declared))
  ) |>
    purrr::set_names(
      translate_app(c("drought_vars", "climate_vars", "fire_vars", "quantiles"), lang_declared)
    )
}