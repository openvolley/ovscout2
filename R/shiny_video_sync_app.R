## not exported

ov_shiny_video_sync_ui <- function(app_data) {
    ## some startup stuff
    running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
    video_src <- app_data$dvw$meta$video$file[1]

    have_lighttpd <- FALSE
    video_server_port <- sample.int(4000, 1) + 8000 ## random port from 8001
    if (.Platform$OS.type == "unix") {
        tryCatch({
            chk <- sys::exec_internal("lighttpd", "-version")
            have_lighttpd <- TRUE
        }, error = function(e) warning("could not find the lighttpd executable, install it with e.g. 'apt install lighttpd'. Using \"servr\" video option"))
    }
    video_serve_method <- if (have_lighttpd) "lighttpd" else "servr"
    if (video_serve_method == "lighttpd") {
        ## build config file to pass to lighttpd
        lighttpd_conf_file <- tempfile(fileext = ".conf")
        cat("server.document-root = \"", dirname(video_src), "\"\nserver.port = \"", video_server_port, "\"\n", sep = "", file = lighttpd_conf_file, append = FALSE)
        lighttpd_pid <- sys::exec_background("lighttpd", c("-D", "-f", lighttpd_conf_file), std_out = FALSE) ## start lighttpd not in background mode
        lighttpd_cleanup <- function() {
            message("cleaning up lighttpd")
            try(tools::pskill(lighttpd_pid), silent = TRUE)
        }
        onStop(function() try({ lighttpd_cleanup() }, silent = TRUE))
    } else {
        ## start servr instance serving from the video source directory
        servr::httd(dir = dirname(video_src), port = video_server_port)
        onStop(function() {
            message("cleaning up servr")
            servr::daemon_stop()
        })
    }
    video_server_base_url <- paste0("http://localhost:", video_server_port)
    message(paste0("video server ", video_serve_method, " on port: ", video_server_port))
    fluidPage(theme=if (running_locally) "spacelab.css" else "https://cdnjs.cloudflare.com/ajax/libs/bootswatch/3.3.7/spacelab/bootstrap.min.css",
              if (!running_locally) htmltools::htmlDependency("bootstrap", "3.3.7",
                                                              src = c(href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/"),
                                                              script = "js/bootstrap.min.js", stylesheet = "css/bootstrap.min.css"),
              shinyjs::useShinyjs(),
              tags$head(tags$style("body{font-size:15px} .well{padding:15px;} .myhidden {display:none;} table {font-size: small;} #headerblock h1,#headerblock h2,#headerblock h3,#headerblock h4 {font-weight: normal; color:#ffffff;} h2, h3, h4 {font-weight: bold;} .shiny-notification { height: 100px; width: 400px; position:fixed; top: calc(50% - 50px); left: calc(50% - 200px); } .code_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:70%;} .clet {color: red;}"),
                        ##tags$style("table {font-size: 10px; line-height: 1.0;"),
                        ##tags$script("$(document).on('shiny:sessioninitialized', function(){ document.getElementById('main_video').addEventListener('focus', function(){ this.blur(); }, false); });"),
                        tags$script("$(document).on('keypress', function (e) { Shiny.onInputChange('cmd', e.which + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('keydown', function (e) { Shiny.onInputChange('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('shiny:sessioninitialized',function(){ Shiny.onInputChange('window_height', $(window).innerHeight()); Shiny.onInputChange('window_width', $(window).innerWidth()); });"),
                        tags$script("var rsztmr; $(window).resize(function() { clearTimeout(rsztmr); rsztmr = setTimeout(doneResizing, 500); }); function doneResizing(){ Shiny.onInputChange('window_height', $(window).innerHeight()); Shiny.onInputChange('window_width', $(window).innerWidth()); }"),
                        tags$title("Volleyball video sync")
                        ),
              fluidRow(id = "headerblock", style = "border-radius:4px;padding:10px;margin-bottom:10px;min-height:160px;color:white;background: #ffffff url(\"https://untan.gl/images/bgrev.jpg\") 0 0/100% auto no-repeat;", ## background image in header block
                       column(6, offset = 2, tags$h2("Volleyball video sync")),
                       column(4,tags$div(style="float:right;padding:10px;", tags$a(href = "https://untan.gl", tags$img(style = "width:16em;max-width:100%", src = "https://untan.gl/images/su_title-w.png"))))
                       ),
              fluidRow(column(7, tags$video(id = "main_video", style = "border: 1px solid black; width: 90%;", src = file.path(video_server_base_url, basename(video_src)), controls = "controls", autoplay = "false"),
                              fluidRow(column(3, actionButton("all_video_from_clock", label = "Open video/clock time operations menu")),
                                       column(3, uiOutput("save_file_ui")),
                                       column(4, offset = 2, uiOutput("current_event"))),
                              fluidRow(column(6, tags$p(tags$strong("Keyboard controls")), tags$ul(tags$li("[r or 5] sync selected event video time"),
                                                                                          tags$li("[i or 8] move to previous skill row"),
                                                                                          tags$li("[k or 2] move to next skill row"),
                                                                                          tags$li("[e or E] edit current code"),
                                                                                          tags$li("[del] delete current code"),
                                                                                          tags$li("[ins] insert new code below current")
                                                                                          ),
                                              tags$p(tags$strong("Other options")),
                                              tags$span("Decimal places on video time:"),
                                              numericInput("video_time_decimal_places", label = NULL, value = 0, min = 0, max = 2, step = 1, width = "6em"),
                                              uiOutput("vtdp_ui")),
                                       column(6, tags$p(tags$strong("Video controls")), sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1), tags$ul(tags$li("[l or 6] forward 2s, [; or ^] forward 10s, [m or 3] forwards 0.1s"), tags$li("[j or 4] backward 2s, [h or $] backward 10s, [n or 1] backwards 0.1s"), tags$li("[q or 0] pause video"), tags$li("[g or #] go to currently-selected event")))
                                       )),
                       column(5, DT::dataTableOutput("playslist", width = "98%"),
                              uiOutput("error_message"))
                       )
              )
    ## find negative time intervals and fix them
}

ov_shiny_video_sync_server <- function(app_data) {
    function(input, output, session) {
        rdata <- reactiveValues(dvw = app_data$dvw)
        editing <- reactiveValues(active = NULL)
        video_state <- reactiveValues(paused = FALSE)
        ##handlers <- reactiveValues() ## a more general way of handling asynchronous js callback events, but not needed (yet)
        dv_read_args <- app_data$dv_read_args
        done_first_playlist_render <- FALSE
        running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
        debug <- 0L
        `%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)
        plays_cols_to_show <- c("error_icon", "clock_time", "video_time", "set_number", "home_team_score", "visiting_team_score", "code")
        plays_col_renames <- c(Set = "set_number", "Home score" = "home_team_score", "Visiting score" = "visiting_team_score")
        is_skill <- function(z) !is.na(z) & (!z %in% c("Timeout", "Technical timeout", "Substitution"))

        code_bits_tbl <- dplyr::tribble(~bit, ~width,
                                        "team", 1,
                                        "number", 2,
                                        "skill", 1,
                                        "type", 1,
                                        "eval", 1,
                                        "combo", 2,
                                        "target", 1,
                                        "start_zone", 1,
                                        "end_zone", 1,
                                        "end_subzone", 1,
                                        "skill_type", 1,
                                        "players", 1,
                                        "special", 1,
                                        "custom", 5)

        output$vtdp_ui <- renderUI({
            if (input$video_time_decimal_places > 0) {
                tags$div(class = "alert alert-danger", "Note: files with non-integer video times may not be openable in DataVolley")
            } else {
                NULL
            }
        })

        ## handler-based dispatch, not used
        ##observeEvent(input$video_time, {
        ##    ##cat("input$video_time: "); cat(str(input$video_time))
        ##    temp <- strsplit(input$video_time, split = "&", fixed = TRUE)[[1]]
        ##    this_handler_id <- temp[2]
        ##    ##cat("running handler: ", this_handler_id, "\n")
        ##    handlers[[this_handler_id]](as.numeric(temp[1]))
        ##    ## then clear it
        ##    ##handlers[[this_handler_id]] <- NULL
        ##})

        observeEvent(input$all_video_from_clock, {
            current_video_time <- selected_event()$video_time
            current_clock_time <- selected_event()$time
            all_clock_times <- rdata$dvw$plays$time
            current_is_no_good <- is.null(current_video_time) || is.na(current_video_time) || is.null(current_clock_time) || is.na(current_clock_time)
            showModal(modalDialog(
                title = "Video times from clock times",
                easyClose = TRUE, size = "l",
                if (all(is.na(all_clock_times))) {
                    tags$div(class = "alert alert-danger", "Your file has no clock times, so this tool can't do anything.")
                } else {
                    tags$div(tags$h4("Options:"),
                             fluidRow(column(8, tags$strong("Set missing video times"), "of events based on their clock times, and the video and clock time of the currently-selected event."),
                                      column(4, if (current_is_no_good) tags$div(class = "alert alert-danger", "The currently-selected event needs to have a video time AND clock time set before using this option.") else actionButton("infer_missing_video_from_current", label = tags$span("Infer MISSING video times", tags$br(), "relative to the current event")))),
                             tags$hr(),
                             fluidRow(column(8, tags$strong("Set the video times of ALL events"), "based on their clock times, and the video and clock time of the currently-selected event. This applies to ALL events, whether they are missing their video time or not."),
                                      column(4, if (current_is_no_good) tags$div(class = "alert alert-danger", "The currently-selected event needs to have a video time AND clock time set before using this option.") else actionButton("infer_all_video_from_current", label = tags$span("Infer ALL video times", tags$br(), "relative to the current event")))),
                             tags$hr(),
                             fluidRow(column(8, tags$strong("Set missing video times"), "of events based on their clock times, and the video and clock time of surrounding events."),
                                      column(4, "Not implemented yet.")##actionButton("infer_missing_video_from_surrounding", label = tags$span("Infer MISSING video times", tags$br(), "relative to surrounding events")))
                                      ),
                             tags$hr(),
                             fluidRow(column(8, tags$strong("Set the clock time"), "of the currently-selected event."),
                                      column(4, actionButton("set_selected_clock_time", label = tags$span("Set clock time of", tags$br(), "selected event.")))),
                             tags$hr(),
                             fluidRow(column(8, tags$strong("Set missing clock times"), "of events based on their video times, and the video and clock time of the currently-selected event."),
                                      column(4, if (current_is_no_good) tags$div(class = "alert alert-danger", "The currently-selected event needs to have a video time AND clock time set before using this option.") else actionButton("infer_missing_clock_from_current", label = tags$span("Infer MISSING clock times", tags$br(), "relative to the current event")))),
                             tags$hr(),
                             fluidRow(column(8, tags$strong("Set the clock times of ALL events"), "based on their video times, and the video and clock time of the currently-selected event. This applies to ALL events, whether they are missing their clock time or not."),
                                      column(4, if (current_is_no_good) tags$div(class = "alert alert-danger", "The currently-selected event needs to have a video time AND clock time set before using this option.") else actionButton("infer_all_clock_from_current", label = tags$span("Infer ALL clock times", tags$br(), "relative to the current event"))))
                             )
                }
            ))
        })

        ## video/clock time sync functions
        observe({
            if (isTruthy(input$infer_all_video_from_current) || isTruthy(input$infer_missing_video_from_current)) {
                isolate({
                    removeModal()
                    this_clock_time <- selected_event()$time
                    this_video_time <- selected_event()$video_time
                    if (is.null(this_clock_time) || is.na(this_clock_time) || is.null(this_video_time) || is.na(this_video_time)) {
                        stop("selected event is missing video or clock time")
                    }
                    clock_time_diff <- difftime(rdata$dvw$plays$time, this_clock_time, units = "secs")
                    midx <- if (isTruthy(input$infer_all_video_from_current)) rep(TRUE, nrow(rdata$dvw$plays)) else is.na(rdata$dvw$plays$video_time)
                    new_video_time <- this_video_time + clock_time_diff[midx]
                    rdata$dvw$plays$video_time[midx] <- round(new_video_time, digits = input$video_time_decimal_places)
                })
            }
        })
        observe({
            if (isTruthy(input$infer_all_clock_from_current) || isTruthy(input$infer_missing_clock_from_current)) {
                isolate({
                    removeModal()
                    this_time <- selected_event()$time
                    this_video_time <- selected_event()$video_time
                    if (is.null(this_time) || is.na(this_time) || is.null(this_video_time) || is.na(this_video_time)) {
                        stop("selected event is missing video or clock time")
                    }
                    video_time_diff <- rdata$dvw$plays$video_time - this_video_time
                    midx <- if (isTruthy(input$infer_all_clock_from_current)) rep(TRUE, nrow(rdata$dvw$plays)) else is.na(rdata$dvw$plays$time)
                    rdata$dvw$plays$time[midx] <- this_time + video_time_diff[midx]
                })
            }
        })

        observeEvent(input$set_selected_clock_time, {
            removeModal()
            if (is.null(selected_event())) {
                showModal(modalDialog(title = "Error", tags$div(class = "alert alert-danger", "No event selected.")))
            } else {
                showModal(modalDialog(
                    title = "Set clock time of selected event",
                    easyClose = TRUE, size = "l",
                    tags$div(shinyTime::timeInput("selected_clocktime", label = "Time:", value = if (!is.na(selected_event()$time)) selected_event()$time else NULL), actionButton("do_set_clocktime", "Set time"))
                ))
            }
        })
        observeEvent(input$do_set_clocktime, {
            removeModal()
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx) && !is.na(ridx)) {
                ##cat("x time: "); cat(str(rdata$dvw$plays$time))
                tm <- input$selected_clocktime
                ##cat("time:"); cat(str(tm))
                if (inherits(rdata$dvw$plays$time, "POSIXct")) tm <- as.POSIXct(tm, tz = lubridate::tz(rdata$dvw$plays$time))
                ##cat("time cast:"); cat(str(tm))
                rdata$dvw$plays$time[ridx] <- tm
            }
        })

        ## sync the selected event to the current video time
        sync_single_video_time <- function() {
            if (FALSE) {
                ## handler-based dispatch on callback, not used
                myfid <- UUIDgenerate()
                handlers[[myfid]] <- handler_sync_single_video_time
                do_video("get_time_fid", myfid)
            } else {
                ridx <- input$playslist_rows_selected
                if (!is.null(ridx)) {
                    do_video("set_current_video_time", ridx)
                }
            }
        }

        observeEvent(input$set_current_video_time, {
            temp <- strsplit(input$set_current_video_time, split = "&", fixed = TRUE)[[1]]
            ridx <- as.integer(temp[2])
            tm <- as.numeric(temp[1])
            if (!is.null(ridx) && !is.na(ridx) && ridx > 0 && ridx <= nrow(rdata$dvw$plays)) {
                rdata$dvw$plays$video_time[ridx] <- round(tm, digits = input$video_time_decimal_places)
                skip <- 1
                if (rdata$dvw$plays$skill[ridx] %eq% "Attack" && ridx < nrow(rdata$dvw$plays) && rdata$dvw$plays$skill[ridx+1] %eq% "Block") {
                    ## give the block the same time
                    rdata$dvw$plays$video_time[ridx+1] <- round(tm, digits = input$video_time_decimal_places)
                    skip <- 2
                }
                ## advance to the next skill row
                if (ridx < nrow(rdata$dvw$plays)) {
                    next_skill_row <- find_next_skill_row(ridx, step = skip)
                    if (length(next_skill_row) > 0) playslist_select_row(next_skill_row)
                }
            }
        })

        ## not used
        ##handler_sync_single_video_time <- function(tm) {
        ##    ##cat("handler_sync_single_video_time: "); cat(tm); cat("\n")
        ##    ridx <- input$playslist_rows_selected
        ##    ##cat("rowidx: ", ridx, "\n")
        ##    if (!is.null(ridx)) {
        ##        rdata$dvw$plays$video_time[ridx] <- round(tm, digits = input$video_time_decimal_places)
        ##        ## advance to the next skill row
        ##        if (ridx < nrow(rdata$dvw$plays)) {
        ##            next_skill_row <- find_next_skill_row(ridx, step = skip)
        ##            if (length(next_skill_row) > 0) playslist_select_row(next_skill_row)
        ##        }
        ##    }
        ##    NULL
        ##}

        selected_event <- reactive({
            if (length(input$playslist_rows_selected) == 1) {
                rdata$dvw$plays[input$playslist_rows_selected, ]
            } else {
                NULL
            }
        })

        output$current_event <- renderUI({
            tags$span(style = "font-size: large;", tags$strong("Current: "), selected_event()$code)
        })

        observe({
            if (!is.null(rdata$dvw) && nrow(rdata$dvw$plays) > 0 && !"error_message" %in% names(rdata$dvw$plays)) {
                rdata$dvw <- preprocess_dvw(rdata$dvw)
            }
        })

        plays_do_rename <- function(z) names_first_to_capital(dplyr::rename(z, plays_col_renames))
        ## the plays display in the RHS table
        output$playslist <- DT::renderDataTable({
            isolate(mydat <- rdata$dvw$plays) ## render once, then isolate from further renders - will be done by replaceData below
            if (!is.null(input$window_height) && !is.na(input$window_height)) {
                plh <- input$window_height*0.6
            } else {
                plh <- 200
            }
            if (!is.null(mydat)) {
                isolate({
                    first_skill_row <- find_next_skill_row(-1)
                    sel <- list(mode = "single")
                    if (length(first_skill_row) > 0) {
                        sel$target <- "row"
                        sel$selected <- first_skill_row
                    }
                })
                mydat$is_skill <- is_skill(mydat$skill)
                mydat$set_number <- as.factor(mydat$set_number)
                cols_to_hide <- which(c(plays_cols_to_show, "is_skill") %in% c("is_skill"))-1 ## 0-based because no row names
                cnames <- names(plays_do_rename(mydat[1, c(plays_cols_to_show, "is_skill"), drop = FALSE]))
                cnames[plays_cols_to_show == "error_icon"] <- ""
                out <- DT::datatable(mydat[, c(plays_cols_to_show, "is_skill"), drop = FALSE], rownames = FALSE, colnames = cnames,
                                     extensions = "Scroller",
                                     escape = FALSE, filter = "top",
                                     selection = sel, options = list(scroller = TRUE,
                                                                     lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = TRUE, "scrollY" = paste0(plh, "px"), ordering = FALSE, ##autoWidth = TRUE,
                                                                     columnDefs = list(list(targets = cols_to_hide, visible = FALSE))
                                                                     ##list(targets = 0, width = "20px")) ## does nothing
                                                                     ))
                out <- DT::formatStyle(out, "is_skill", target = "row", backgroundColor = DT::styleEqual(c(FALSE, TRUE), c("#f0f0e0", "lightgreen"))) ## colour skill rows green
                out <- DT::formatStyle(out, "error_icon", color = "red")
                out
            } else {
                NULL
            }
        }, server = TRUE)
        playslist_proxy <- DT::dataTableProxy("playslist")
        playslist_select_row <- function(rw) {
            if (!is.null(rw)) DT::selectRows(playslist_proxy, rw)
            scroll_playlist()
        }

        ##only trigger scroll on: do_edit, keyboard move, set video time and move to next. Not just on selecting row
        ## is happening auto when dvw updated, below
        scroll_playlist <- function() {
            if (!is.null(input$playslist_rows_selected)) {
                ## scrolling works on the VISIBLE row index, so it depends on any column filters that might have been applied
                visible_rowidx <- which(input$playslist_rows_all == input$playslist_rows_selected)
                scrollto <- max(visible_rowidx-1-5, 0) ## -1 for zero indexing, -5 to keep the selected row 5 from the top
                dojs(sprintf("$('#playslist').find('.dataTable').DataTable().scroller.toPosition(%s);", scrollto))
                ##dojs(sprintf("$('#playslist').find('.dataTable').DataTable().row(%s).node().scrollIntoView();", max(0, rdata$plays_row_to_select-1)))
                ##dojs(sprintf("console.dir($('#playslist').find('.dataTable').DataTable().row(%s).node())", max(0, rdata$plays_row_to_select-1)))
                ##dojs(sprintf("$('#playslist').find('.dataTables_scroll').animate({ scrollTop: $('#playslist').find('.dataTable').DataTable().row(%s).node().offsetTop }, 2000);", max(0, rdata$plays_row_to_select-1)))
            }
        }

        observe({
            ##        cat("replacing DT data\n")
            mydat <- rdata$dvw$plays
            mydat$is_skill <- is_skill(mydat$skill)
            mydat$set_number <- as.factor(mydat$set_number)
            DT::replaceData(playslist_proxy, data = mydat[, c(plays_cols_to_show, "is_skill"), drop = FALSE], rownames = FALSE, clearSelection = "none")##, resetPaging = FALSE)
            ## and scroll to selected row
            ##dojs(sprintf("$('#playslist').find('.dataTable').DataTable().row(%s).scrollTo();", max(0, rdata$plays_row_to_select-1)))
        })

        find_next_skill_row <- function(current_row_idx = NULL, step = 1, respect_filters = TRUE) {
            ## if respect_filters is TRUE, find the next row that is shown in the table (i.e. passing through any column filters that have been applied)
            ## if FALSE, just find the next skill row in the data, ignoring table filters
            if (is.null(current_row_idx)) current_row_idx <- input$playslist_rows_selected
            skill_rows <- which(is_skill(rdata$dvw$plays$skill))
            if (respect_filters) skill_rows <- intersect(skill_rows, input$playslist_rows_all)
            next_skill_row <- skill_rows[skill_rows > current_row_idx]
            next_skill_row[min(step, length(next_skill_row))]
        }

        find_prev_skill_row <- function(current_row_idx = NULL, step = 1, respect_filters = TRUE) {
            ## if respect_filters is TRUE, find the previous row that is shown in the table (i.e. passing through any column filters that have been applied)
            ## if FALSE, just find the previous skill row in the data, ignoring table filters
            if (is.null(current_row_idx)) current_row_idx <- input$playslist_rows_selected
            skill_rows <- which(is_skill(rdata$dvw$plays$skill))
            if (respect_filters) skill_rows <- intersect(skill_rows, input$playslist_rows_all)
            prev_skill_row <- rev(skill_rows[skill_rows < current_row_idx])
            prev_skill_row[min(step, length(prev_skill_row))]
        }

        output$error_message <- renderUI({
            if (is.null(selected_event()) || is.na(selected_event()$error_message)) {
                NULL
            } else {
                tags$div(class = "alert alert-danger", HTML(selected_event()$error_message))
            }
        })

        observeEvent(input$playback_rate, {
            if (!is.null(input$playback_rate)) do_video("playback_rate", input$playback_rate)
        })

        observeEvent(input$cmd, {
            mycmd <- sub("@.*", "", input$cmd)
            if (!is.null(mycmd)) {
                ## mycmd comes in as a character representation of the ascii code like "65" or "32"
                mykey <- intToUtf8(as.numeric(mycmd))
                ## note that if cmdbox is an INPUT and focus is cmdbox then the document$keypress event doesn't get fired, because it gets grabbed by the cmdbox event handler
                ignore_keys <- NULL ## placeholder for keys handled elsewhere in code (e.g. 37, 39 might not trigger here, may depend on browser)
                if (debug > 1) cat("input: ", mycmd, "\n")
                if (mycmd %in% ignore_keys) {
                    if (debug > 1) cat(" (ignored)")
                } else if (!is.null(editing$active)) {
                    ## if editing is in progress, don't process the usual navigation etc keys
                    if (mycmd %eq% "13") {
                        ## if editing or inserting, treat as update
                        code_make_change()
                    } else if (mycmd %eq% "27") {
                        ## not sure if this will be detected by keypress, maybe only keydown (may be browser specific)
                        ## esc
                        editing$active <- NULL
                        removeModal()
                    }
                } else {
                    ## editing not active
                    if (mycmd %in% utf8ToInt("eE")) {
                        ## open code editing dialog
                        edit_current_code()
                    } else if (mycmd %eq% "45") {
                        ## insert new row below current
                        insert_data_row()
                    } else if (mycmd %eq% "38") {
                        ## 38 (up arrow)
                    } else if (mycmd %eq% "40") {
                        ## 40 (down arrow)
                    } else if (mycmd %eq% "8") {
                        ## backspace
                    } else if (mycmd %eq% "46") {
                        ## delete key
                    } else if (mycmd %in% utf8ToInt("i8")) {
                        ## prev skill row
                        psr <- find_prev_skill_row()
                        if (length(psr) > 0) playslist_select_row(psr)
                    } else if (mycmd %in% utf8ToInt("k2")) {
                        ## next skill row
                        nsr <- find_next_skill_row()
                        if (length(nsr) > 0) playslist_select_row(nsr)
                    } else if (mycmd %in% utf8ToInt("qQ0")) {
                        do_video("toggle_pause")
                    } else if (mycmd %in% utf8ToInt("gG#")) {
                        ## video go to currently-selected event
                        ev <- selected_event()
                        if (!is.null(ev)) do_video("set_time", ev$video_time)
                    } else if (mycmd %in% utf8ToInt("nm13jhl;46$^")) {
                        ## video forward/backward nav
                        vidcmd <- if (tolower(mykey) %in% c("1", "n", "h", "j", "4", "$")) "rew" else "ff"
                        dur <- if (tolower(mykey) %in% c("h", "$", ";", "^")) 10 else if (tolower(mykey) %in% c("n", "m", "1", "3")) 0.1 else 2
                        do_video(vidcmd, dur)
                        ##} else if (mycmd %in% as.character(33:126)) {
                        ##    cat("queued: ", mycmd, "\n")
                        ##    ## add to cmd queue
                        ##    rdata$cmd <- paste0(rdata$cmd, intToUtf8(mycmd))
                        ##    output$cmdbox <- renderText(rdata$cmd)
                    } else if (mykey %in% c("r", "R", "5")) {
                        ## set the video time of the current event
                        sync_single_video_time()
                    }
                }
                if (debug > 1) cat("\n")
            }
        })
        observeEvent(input$controlkey, {
            ## keys that might not get detected by keypress but do by keydown?
            ## if we are editing a code or inserting a new one, update the code_entry_guide
            mycmd <- sub("@.*", "", input$controlkey)
            if (debug > 1) cat("control key: ", mycmd, "\n")
            mycmd <- strsplit(mycmd, "|", fixed = TRUE)[[1]] ## e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which
            if (length(mycmd) == 5) {
                ky <- mycmd[5]
                if (ky %eq% "27") {
                    ## esc
                    editing$active <- NULL
                    removeModal()
                } else if (ky %eq% "45") {
                    ## insert new row below current
                    insert_data_row()
                } else if (ky %eq% "46") {
                    ## delete current row
                    delete_data_row()
                }
            }
        })

        edit_current_code <- function() {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "edit"
                showModal(modalDialog(title = "Edit code", size = "l", footer = actionButton("code_edit_cancel", label = "Cancel (or press Esc)"),
                                      "Edit code either in the top text box or in the individual boxes (but not both)",
                                      textInput("code_entry", label = "Code:", value = thiscode),
                                      "or",
                                      uiOutput("code_entry_guide"),
                                      actionButton("code_do_edit", label = "Update code (or press Enter)")))
                if (!is_skill(rdata$dvw$plays$skill[ridx])) {
                    ## if it's a non-skill code then focus into the code_entry textbox with cursor at end of input
                    dojs("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById(\"code_entry\"); el.selectionStart = el.selectionEnd = el.value.length; el.focus(); });")
                } else {
                    ## TODO otherwise focus into the appropriate code_entry_guide sub-box
                }
            }
        }
        observeEvent(input$code_edit_cancel, {
            editing$active <- NULL
            removeModal()
        })
        observeEvent(input$code_do_edit, {
            code_make_change()
        })
        observeEvent(input$code_do_delete, {
            code_make_change()
        })
        code_make_change <- function() {
            removeModal()
            if (is.null(editing$active)) {
                ## not triggered from current editing activity, huh?
                warning("code_make_change entered but editing not active")
            } else {
                ridx <- input$playslist_rows_selected
                if (!is.null(ridx)) {
                    if (editing$active %in% c("edit", "insert")) {
                        current_code <- rdata$dvw$plays$code[ridx]
                        ## user has changed EITHER input$code_entry or used the code_entry_guide
                        ## infer code from code_entry_guide elements
                        newcode1 <- lapply(seq_len(nrow(code_bits_tbl)), function(bi) {
                            val <- input[[paste0("code_entry", code_bits_tbl$bit[bi])]]
                            wid <- code_bits_tbl$width[bi]
                            if (nchar(val) < wid) val <- stringr::str_pad(val, wid, side = "right", pad = "~")
                            val
                        })
                        newcode1 <- sub("~+$", "", paste(newcode1, collapse = ""))## trim trailing ~'s
                        newcode2 <- input$code_entry
                        changed1 <- !newcode1 %eq% current_code && nzchar(newcode1)
                        changed2 <- !newcode2 %eq% current_code && nzchar(newcode2)
                        if (!changed1 && changed2) {
                            newcode <- newcode2
                        } else if (!changed2 && changed1) {
                            newcode <- newcode1
                        } else {
                            ## both changed?
                            newcode <- NULL
                            warning("BOTH CHANGED: to do")
                        }
                    }
                    if (editing$active %eq% "edit" && !is.null(newcode)) {
                        ## update the code in the current row
                        rdata$dvw$plays$code[ridx] <- newcode
                    } else if (editing$active %eq% "insert" && !is.null(newcode)) {
                        ## insert new line
                        newline <- rdata$dvw$plays[ridx, ]
                        newline$code <- newcode
                        rdata$dvw$plays <- bind_rows(rdata$dvw$plays[seq(1, ridx, by = 1), ], newline, rdata$dvw$plays[seq(ridx+1, nrow(rdata$dvw$plays), by = 1), ])
                        ## set the newly-inserted line as the active row
                        nsr <- find_next_skill_row()
                        if (length(nsr) > 0) playslist_select_row(nsr)
                    } else if (editing$active %eq% "delete") {
                        if (is.logical(ridx)) ridx <- which(ridx)
                        rdata$dvw$plays <- rdata$dvw$plays[-ridx, ]
                    }
                    ## reparse the dvw
                    rdata$dvw <- reparse_dvw(rdata$dvw, dv_read_args = dv_read_args)
                }
            }
            editing$active <- NULL
            scroll_playlist()
        }
        insert_data_row <- function() {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "insert"
                showModal(modalDialog(title = "Insert new code", size = "l", footer = actionButton("code_edit_cancel", label = "Cancel (or press Esc)"),
                                      "Enter new code either in the top text box or in the individual boxes (but not both)",
                                      textInput("code_entry", label = "Code:", value = ""),
                                      "or",
                                      uiOutput("code_entry_guide"),
                                      actionButton("code_do_edit", label = "Insert code (or press Enter)")))
            }
        }
        delete_data_row <- function() {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "delete"
                showModal(modalDialog(title = "Delete code", size = "l", footer = actionButton("code_edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("code_do_delete", label = "Confirm delete code (or press Enter)")))
            }
        }

        ## video helper functions
        do_video <- function(what, ..., id = "main_video") {
            getel <- paste0("document.getElementById('", id, "')")
            myargs <- list(...)
            if (what == "pause") {
                if (video_state$paused) {
                    dojs(paste0(getel, ".play();"))
                    video_state$paused <- FALSE
                } else {
                    dojs(paste0(getel, ".pause();"))
                    video_state$paused <- TRUE
                }
                NULL
            } else if (what == "toggle_pause") {
                dojs(paste0("if (", getel, ".paused == true) { ", getel, ".play(); } else { ", getel, ".pause(); }"))
            } else if (what == "get_time") {
                dojs(paste0("Shiny.onInputChange('video_time', ", getel, ".currentTime)"))
            } else if (what == "get_time_fid") {
                dojs(paste0("Shiny.onInputChange('video_time', ", getel, ".currentTime + '&", myargs[[1]], "')"))
            } else if (what == "set_time") {
                dojs(paste0(getel, ".currentTime='", myargs[[1]], "';"))
            } else if (what == "set_current_video_time") {
                dojs(paste0("Shiny.onInputChange('set_current_video_time', ", getel, ".currentTime + '&", myargs[1], "')"))
            } else if (what == "rew") {
                dojs(paste0(getel, ".currentTime=", getel, ".currentTime - ", myargs[[1]], ";"))
            } else if (what == "ff") {
                dojs(paste0(getel, ".currentTime=", getel, ".currentTime + ", myargs[[1]], ";"))
            } else if (what == "playback_rate") {
                dojs(paste0(getel, ".playbackRate=", myargs[[1]], ";"))
            } else {
                NULL
            }
        }

        ## save file
        output$save_file_ui <- renderUI({
            if (is.null(rdata$dvw)) {
                NULL
            } else {
                downloadButton("save_file_button", "Save file")
            }
        })
        output$save_file_button <- downloadHandler(
            filename = reactive(
                if (!is.null(rdata$dvw$meta$filename) && !is.na(rdata$dvw$meta$filename)) basename(rdata$dvw$meta$filename) else "myfile.dvw"
            ),
            content = function(file) {
                tryCatch(dv_write(rdata$dvw, file = file),
                         error = function(e) {
                             rds_ok <- FALSE
                             if (running_locally) {
                                 ## this only makes sense if running locally, not deployed on a remote server
                                 tf <- tempfile(fileext = ".rds")
                                 try({
                                     saveRDS(rdata$dvw, file = tf)
                                     rds_ok <- file.exists(tf) && file.size(tf) > 0
                                 }, silent = TRUE)
                             }
                             showModal(modalDialog(title = "Save error",
                                                   tags$div(class = "alert alert-danger", "Sorry, the save failed. The error message was:", tags$br(), tags$pre(conditionMessage(e)), tags$br(), if (rds_ok) paste0("The edited datavolley object has been saved to ", tf, ". You might be able to recover your edited information from that (contact the package authors for assistance)."))))
                             NULL
                         })
            }
        )
        output$code_entry_guide <- renderUI({
            if (is.null(editing$active) || !editing$active %in% c("insert", "edit")) {
                NULL
            } else {
                ## get current code
                ## some local markup to make the helper entries easier here
                ## | = <br />
                ## {thing} = <strong>thing</strong>
                ## [thing] = <span class=\"clet\">thing</span>
                ## (thing) = <em>thing</em>
                ## --- = <br /><hr />
                gsubf <- function(...) gsub(..., fixed = TRUE)
                mu2html <- function(z) gsubf("[", "<span class=\"clet\">", gsubf("]", "</span>", gsubf("{", "<strong>", gsubf("}", "</strong>", gsubf("|", "<br />", gsubf("(", "<em>", gsubf(")", "</em>", gsubf("---", "<br /><hr />", z))))))))
                special_helper <- function(skill, evaln) {
                    mu2html(paste0("{Special}---",
                                   case_when(skill %eq% "A" & evaln %eq% "#" ~ "(Attk kill)|Blk out [S]ide|Blk out l[O]ng|Blk on [F]loor|[X] Direct|on floor",
                                             skill %eq% "A" & evaln %eq% "=" ~ "(Attk err)|Out [S]ide|Out l[O]ng|In [N]et|[I] net contct|[A]ntenna|[Z] ref call",
                                             skill %eq% "A" ~ "(Attk)|blk [C]ontrol|[N] let",
                                             skill %eq% "B" & evaln %in% c("=", "/") ~ "(Blk err)|Out [S]ide|Out l[O]ng|Ball on [F]lr|[X] between|hands|[N] net touch|[A]ntenna|[P] no jump|[T] pos error|[Z] ref call",
                                             skill %eq% "R" ~ "(Rcv)|[U]nplayable|[X] body err|[P]os err|No [E]ffort|[Z] ref call",
                                             skill %eq% "S" & evaln %eq% "#" ~ "(Srv ace)|[N] let",
                                             skill %eq% "S" & evaln %eq% "=" ~ "(Srv err)|Out l[O]ng|Out [L]eft|Out [R]ight|In [N]et|[Z] ref call",
                                             skill %eq% "S" ~ "(Srv)|[N] let",
                                             skill %eq% "E" & evaln %eq% "=" ~ "(Set err)|[U]nhittable|[I] net tch|[Z] ref call",
                                             skill %eq% "Dig" & evaln %eq% "=" ~ "(Dig err)|[U]nplayable|[X] body err|[P]os err|[Z] Ref call|Ball on [F]lr|Ball [O]ut|No [E]ffort",
                                             skill %eq% "Freeball" & evaln %eq% "=" ~ "(Fr err)|[U]nplayable|[X] body err|[P]os err|[Z] Ref call")))
                }
                skill_type_helper <- function(skill, evaln) {
                    mu2html(paste0("{Skill|type}---",
                                   case_when(skill %eq% "A" ~ "(Attk)|[H]ard|[P] soft|[T]ip",
                                             skill %eq% "R" ~ "(Rec)|[L]eft|[R]ight|lo[W]|[O]vrhnd|[M]idline",
                                             skill %eq% "E" ~ "(Set)|[1] hand|[2] hands|[3] bump|[4] othr|[5] uhand",
                                             skill %eq% "D" ~ "(Dig)|[S] on spk|[C] spk|cover|[B] aftr|block|[E] emerg")))
                }
                num_players_helper <- function(skill, evaln) {
                    mu2html(paste0("{Num|plyrs}---",
                                   case_when(skill %in% c("A", "B") ~ "(Attk|Blk)|[0]..[3]|[4] hole|block",
                                             skill %eq% "R" ~ "(Rcv)|[1] 2p,L|[2] 2p,R|[3] 3p,L|[4] 3p,M|[5] 3p,R|[6] 4p,L|[7] 4p,LC|[8] 4p,RC|[9] 4p,R")))
                }
                bitstbl <- code_bits_tbl
                bitstbl$helper <- c(mu2html("{Team}---[*]&nbsp;H|[a]&nbsp;V"), ## team
                                    mu2html("{Plyr|num}"), ## number
                                    mu2html("{Skill}---[S]rv|[R]ec|[A]ttk|[B]lk|[D]ig|s[E]t|[F]reeb"), ## skill
                                    mu2html("{Tempo}---[H]igh|[M]ed|[Q]uick|[T]ense|s[U]per|[N] fast|[O]ther"), ## type
                                    mu2html("{Eval}---[#|+|!|-|/|=]"), ## eval
                                    mu2html("{Combo}---(Atk code)|[X.]|[C.]|etc||(Set call)|[K.]"), ## combo
                                    mu2html("{Target}---[F]ront|[C]ntr|[B]ack|[P]ipe|[S]etr"), ## target
                                    mu2html("{Start|zone}---(Attk)|[1..9]||(Srv)|[57691]"), ##start_zone
                                    mu2html("{End zone}---[1..9]||{End cone}|(Attk)|[1..8]"), ##end_zone
                                    mu2html("{End|subzn}---[ABCD]"), ##end_subzone
                                    skill_type_helper, ##skill_type
                                    num_players_helper, ##players
                                    special_helper, ##special
                                    mu2html("{Custom}---")) ##custom
                bitstbl$start <- cumsum(dplyr::lag(bitstbl$width, default = 0))+1L
                bitstbl$end <- bitstbl$start+bitstbl$width-1L
                isolate({
                    ridx <- input$playslist_rows_selected
                    if (!is.null(ridx) && editing$active %eq% "edit" && is_skill(rdata$dvw$plays$skill[ridx])) {
                        ## only with skill, not timeout/sub/etc
                        thiscode <- rdata$dvw$plays$code[ridx]
                        bitstbl$value <- vapply(seq_len(nrow(bitstbl)), function(z) substr(thiscode, bitstbl$start[z], bitstbl$end[z]), FUN.VALUE = "", USE.NAMES = FALSE)
                    } else {
                        bitstbl$value <- ""
                    }
                })
                bitstbl$value <- gsub("~", "", bitstbl$value)
                cbitInput <- function (bitname, value = "", width = 2, helper = "") {
                    tags$div(style = paste0("display:inline-block; vertical-align:top;"), tags$input(id = paste0("code_entry", bitname), type = "text", value = value, size = width, maxlength = width, class = "input-small"),
                             tags$div(class = "code_entry_guide", HTML(helper)))
                }
                tags$div(style = "padding: 8px;", do.call(shiny::fixedRow, lapply(seq_len(nrow(bitstbl)), function(z) {
                    this_skill <- bitstbl$value[bitstbl$bit %eq% "skill"]
                    this_ev <- bitstbl$value[bitstbl$bit %eq% "eval"]
                    cbitInput(bitstbl$bit[z], value = bitstbl$value[z], width = bitstbl$width[z], helper = if (is.function(bitstbl$helper[[z]])) bitstbl$helper[[z]](this_skill, this_ev) else bitstbl$helper[[z]])
                })))
            }
        })
    }
}

dojs <- function(jscmd) {
    ##cat("js: ", jscmd, "\n")
    shinyjs::runjs(jscmd)
}

names_first_to_capital <- function(x, fun) {
    setNames(x, var2fc(if (missing(fun)) names(x) else vapply(names(x), fun, FUN.VALUE = "", USE.NAMES = FALSE)))
}

var2fc <- function(x) {
    vapply(x, function(z) gsub("_", " ", paste0(toupper(substr(z, 1, 1)), substr(z, 2, nchar(z)))), FUN.VALUE = "", USE.NAMES = FALSE)
}

reparse_dvw <- function(x, dv_read_args = list()) {
    tf <- tempfile()
    on.exit(unlink(tf))
    dv_write(x, tf)
    dv_read_args$filename <- tf
    out <- do.call(read_dv, dv_read_args)
    out$meta$filename <- x$meta$filename ## preserve this
    preprocess_dvw(out)
}

preprocess_dvw <- function(x) {
    x$plays <- mutate(x$plays, clock_time = format(.data$time, "%H:%M:%S"))
    msgs <- dplyr::summarize(group_by_at(x$messages, "file_line_number"), error_message = paste0(.data$message, collapse = "<br />"))
    x$plays <- left_join(x$plays, msgs, by = "file_line_number")
    x$plays$error_icon <- ifelse(is.na(x$plays$error_message), "", HTML(as.character(shiny::icon("exclamation-triangle"))))
    x
}
