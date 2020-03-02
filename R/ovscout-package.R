#' \pkg{ovscout}
#'
#' Functions for video synchronisation, scout file editing, and more from volleyball match files.
#'
#' @name ovscout
#' @docType package
#' @importFrom assertthat assert_that has_name is.flag is.string
#' @importFrom datavolley read_dv dv_example_file dv_write
#' @importFrom dplyr .data bind_rows group_by_at left_join mutate tibble
#' @importFrom htmltools HTML tagList tags
#' @importFrom shiny actionButton column downloadButton downloadHandler fluidPage fluidRow isolate isTruthy modalDialog numericInput observeEvent observe onStop reactive reactiveValues removeModal renderUI showModal sliderInput textInput uiOutput
#' @importFrom stats na.omit setNames
#' @importFrom uuid UUIDgenerate
#' @importFrom utils head tail
NULL
