#' \pkg{ovscout}
#'
#' Functions for video synchronisation, scout file editing, and more from volleyball match files.
#'
#' @name ovscout
#' @docType package
#' @importFrom assertthat assert_that has_name is.flag is.string
#' @importFrom datavolley read_dv dv_example_file dv_xy dv_write ggcourt
#' @importFrom dplyr .data bind_rows case_when group_by_at left_join mutate tibble
#' @importFrom ggplot2 aes_string geom_polygon geom_text ggplot scale_x_reverse scale_y_reverse
#' @importFrom htmltools HTML tagList tags
#' @importFrom shiny actionButton column downloadButton downloadHandler fluidPage fluidRow isolate isTruthy modalDialog numericInput observeEvent observe onStop plotOutput reactive reactiveVal reactiveValues removeModal renderPlot renderUI selectInput showModal sliderInput tabPanel tabsetPanel textInput uiOutput updateSelectInput updateTextInput wellPanel
#' @importFrom stats na.omit setNames
#' @importFrom uuid UUIDgenerate
#' @importFrom utils head tail write.csv
NULL
