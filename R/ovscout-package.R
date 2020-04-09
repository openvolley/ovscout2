#' \pkg{ovscout}
#'
#' Functions for video synchronisation, scout file editing, and more from volleyball match files.
#'
#' @name ovscout
#' @docType package
#' @importFrom assertthat assert_that has_name is.flag is.string
#' @importFrom datavolley read_dv dv_example_file dv_xy dv_write ggcourt
#' @importFrom dplyr .data bind_rows case_when group_by_at lag lead left_join mutate row_number tibble
#' @importFrom ggplot2 aes_string geom_polygon geom_text ggplot scale_x_reverse scale_y_reverse coord_flip
#' @importFrom htmltools HTML tagList tags
#' @importFrom shiny actionButton callModule column downloadButton downloadHandler fluidPage fluidRow isolate isTruthy modalDialog NS numericInput observeEvent observe onStop plotOutput reactive reactiveVal reactiveValues removeModal renderPlot renderUI selectInput showModal sliderInput tabPanel tabsetPanel textInput uiOutput updateSelectInput updateTextInput wellPanel
#' @importFrom stats na.omit setNames
#' @importFrom stringr str_c str_count str_detect str_match str_pad str_remove str_split str_sub str_to_upper str_trim
#' @importFrom uuid UUIDgenerate
#' @importFrom utils head tail write.csv
NULL
