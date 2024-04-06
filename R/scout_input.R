## functions related to the typing input

## helper function to retrieve the time-stamped keypresses
ref_dt <- as.POSIXct("1970-01-01", tz = format(Sys.time(), "%Z"))
get_scout_input_times <- function() {
    input <- getvar("input")
    if (!is.null(input$scout_input_times)) {
        tryCatch({
            this <- as.data.frame(matrix(input$scout_input_times, ncol = 3, byrow = TRUE))
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

