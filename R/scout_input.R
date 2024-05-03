## functions related to the typing input

## helper function to retrieve the time-stamped keypresses
ref_dt <- as.POSIXct("1970-01-01", tz = format(Sys.time(), "%Z"))
get_scout_input_times <- function(scout_input_times) {
    if (!is.null(scout_input_times)) {
        tryCatch({
            this <- as.data.frame(matrix(scout_input_times, ncol = 3, byrow = TRUE))
            names(this) <- c("key", "time", "video_time")
            this$time <- as.numeric(this$time) / 1e3 + ref_dt;
            this$video_time <- as.numeric(this$video_time)
            ## we don't care about modifier keys here
            this <- this[!this$key %in% c("Alt", "Shift", "Control", "Meta", "Tab"), ]
            i <- 1
            while (i < nrow(this)) {
                if (i > 1 && this$key[i] %eq% "Backspace") {
                    this <- this[-c(i, i - 1L), ]
                } else if (i < nrow(this) && this$key[i] %eq% "Delete") {
                    this <- this[-c(i, i + 1L), ]
                    i <- i + 1L
                } else {
                    i <- i + 1L
                }
            }
            this
        }, error = function(e) NULL)
    } else {
        NULL
    }
}

## the modal popup used to review the rally codes before accepting them
review_rally_modal <- function(rcodes) {
    rctxt <- codes_from_rc_rows(rcodes)
    print(rctxt)
    showModal(vwModalDialog(title = "Review rally codes", footer = NULL, width = 100,
                            tags$p(tags$strong("Tab"), ", ", tags$strong("Shift-Tab"), "to move between code boxes.",
                                   tags$strong("Enter"), "or", tags$strong("Continue"), "to accept all and start next rally.",
                                   tags$strong("Esc"), "or", tags$strong("Cancel"), "to cancel the end of rally."),
                            tags$p(tags$strong("Rally actions")),
                            fluidRow(column(6, do.call(tagList, lapply(seq_along(rctxt), function(i) {
                                textInput(paste0("rcedit_", i), label = NULL, value = rctxt[i])
                            })))),
                            tags$br(), tags$hr(),
                            fixedRow(column(2, actionButton("redit_cancel", "Cancel", class = "cancel fatradio")),
                                     column(2, offset = 8, actionButton("redit_ok", "Continue", class = "continue fatradio")))
                            ))
    focus_to_modal_element("rcedit_1")
}
