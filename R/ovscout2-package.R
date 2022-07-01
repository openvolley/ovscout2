#' \pkg{ovscout2}
#'
#' Functions for scouting volleyball match files.
#'
#' @name ovscout2
#' @docType package
#' @importFrom assertthat assert_that has_name is.flag is.string
#' @importFrom datavolley dv_read dv_example_file dv_cone2xy dv_cone_polygons dv_flip_xy dv_index2xy dv_xy dv_xy2index dv_xy2subzone dv_xy2zone dv_write ggcourt read_dv skill_evaluation_decoder
#' @importFrom dplyr %>% .data as_tibble bind_cols bind_rows case_when desc group_by group_by_at lag lead left_join mutate row_number tibble tribble ungroup
#' @importFrom ggplot2 aes aes_string arrow coord_flip element_blank element_rect geom_label geom_path geom_point geom_polygon geom_segment geom_text ggplot scale_size_continuous scale_x_continuous scale_x_reverse scale_y_continuous scale_y_reverse theme unit xlim ylim
#' @importFrom htmltools HTML tagList tags
#' @importFrom shiny actionButton callModule checkboxInput column downloadButton downloadHandler fixedRow fluidPage fluidRow icon isolate isTruthy modalDialog NS numericInput observeEvent observe onStop plotOutput reactive reactiveVal reactiveValues reactiveValuesToList removeModal renderPlot renderText renderUI req selectInput showModal sliderInput tabPanel tabsetPanel textInput uiOutput updateSelectInput updateTextInput verbatimTextOutput wellPanel withTags
#' @importFrom stats aggregate na.omit setNames
#' @importFrom stringr str_c str_count str_detect str_match str_pad str_remove str_split str_sub str_to_upper str_trim
#' @importFrom uuid UUIDgenerate
#' @importFrom utils capture.output flush.console head modifyList packageVersion str tail write.csv
#' @importFrom rintrojs introjsUI introjs introBox
NULL
