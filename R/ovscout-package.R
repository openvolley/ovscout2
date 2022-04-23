#' \pkg{ovscout}
#'
#' Functions for video synchronisation, scout file editing, and more from volleyball match files.
#'
#' @name ovscout
#' @docType package
#' @importFrom assertthat assert_that has_name is.flag is.string
#' @importFrom datavolley dv_read dv_example_file dv_cone2xy dv_cone_polygons dv_flip_xy dv_xy dv_write ggcourt read_dv skill_evaluation_decoder
#' @importFrom dplyr .data as_tibble bind_cols bind_rows case_when group_by_at lag lead left_join mutate row_number tibble tribble
#' @importFrom ggplot2 aes aes_string arrow coord_flip element_blank element_rect geom_label geom_path geom_point geom_polygon geom_segment geom_text ggplot scale_size_continuous scale_x_continuous scale_x_reverse scale_y_continuous scale_y_reverse theme unit xlim ylim
#' @importFrom htmltools HTML tagList tags
#' @importFrom shiny actionButton callModule column downloadButton downloadHandler fluidPage fluidRow icon isolate isTruthy modalDialog NS numericInput observeEvent observe onStop plotOutput reactive reactiveVal reactiveValues removeModal renderPlot renderUI selectInput showModal sliderInput tabPanel tabsetPanel textInput uiOutput updateSelectInput updateTextInput wellPanel req renderText verbatimTextOutput h4 withTags checkboxInput
#' @importFrom stats na.omit setNames
#' @importFrom stringr str_c str_count str_detect str_match str_pad str_remove str_split str_sub str_to_upper str_trim
#' @importFrom uuid UUIDgenerate
#' @importFrom utils capture.output flush.console head packageVersion tail write.csv
#' @importFrom rintrojs introjsUI introjs introBox
NULL
