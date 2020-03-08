## not exported

ov_shiny_video_sync_ui <- function(data) {
    ## some startup stuff
    running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
    video_src <- data$dvw$meta$video$file[1]

    have_lighttpd <- FALSE
    video_server_port <- sample.int(4000, 1) + 8000 ## random port from 8001
    if (.Platform$OS.type == "unix") {
        tryCatch({
            chk <- sys::exec_internal("lighttpd", "-version")
            have_lighttpd <- TRUE
        }, error = function(e) warning("could not find the lighttpd executable, install it with e.g. 'apt install lighttpd'. Using \"standalone\" video option"))
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
              tags$head(tags$style("body{font-size:15px} .well{padding:15px;} .myhidden {display:none;} table {font-size: small;} #headerblock h1,#headerblock h2,#headerblock h3,#headerblock h4 {font-weight: normal; color:#ffffff;} h2, h3, h4 {font-weight: bold;} .shiny-notification { height: 100px; width: 400px; position:fixed; top: calc(50% - 50px); left: calc(50% - 200px); }"),
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

if (getRversion() >= "2.15.1")  utils::globalVariables("SHINY_DATA") ## avoid check complaints

ov_shiny_video_sync_server <- function(input, output, session) {
    things <- reactiveValues(dvw = SHINY_DATA$dvw)
    editing <- reactiveValues(active = NULL)
    video_state <- reactiveValues(paused = FALSE)
    ##handlers <- reactiveValues() ## a more general way of handling asynchronous js callback events, but not needed (yet)
    dv_read_args <- SHINY_DATA$dv_read_args
    done_first_playlist_render <- FALSE
    debug <- 0L
    `%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)
    plays_cols_to_show <- c("error_icon", "clock_time", "video_time", "set_number", "home_team_score", "visiting_team_score", "code")
    plays_col_renames <- c(Set = "set_number", "Home score" = "home_team_score", "Visiting score" = "visiting_team_score")
    is_skill <- function(z) !is.na(z) & (!z %in% c("Timeout", "Technical timeout", "Substitution"))

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
        all_clock_times <- things$dvw$plays$time
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
                clock_time_diff <- difftime(things$dvw$plays$time, this_clock_time, units = "secs")
                midx <- if (isTruthy(input$infer_all_video_from_current)) rep(TRUE, nrow(things$dvw$plays)) else is.na(things$dvw$plays$video_time)
                new_video_time <- this_video_time + clock_time_diff[midx]
                things$dvw$plays$video_time[midx] <- round(new_video_time, digits = input$video_time_decimal_places)
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
                video_time_diff <- things$dvw$plays$video_time - this_video_time
                midx <- if (isTruthy(input$infer_all_clock_from_current)) rep(TRUE, nrow(things$dvw$plays)) else is.na(things$dvw$plays$time)
                things$dvw$plays$time[midx] <- this_time + video_time_diff[midx]
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
            ##cat("x time: "); cat(str(things$dvw$plays$time))
            tm <- input$selected_clocktime
            ##cat("time:"); cat(str(tm))
            if (inherits(things$dvw$plays$time, "POSIXct")) tm <- as.POSIXct(tm, tz = lubridate::tz(things$dvw$plays$time))
            ##cat("time cast:"); cat(str(tm))
            things$dvw$plays$time[ridx] <- tm
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
        if (!is.null(ridx) && !is.na(ridx) && ridx > 0 && ridx <= nrow(things$dvw$plays)) {
            things$dvw$plays$video_time[ridx] <- round(tm, digits = input$video_time_decimal_places)
            skip <- 1
            if (things$dvw$plays$skill[ridx] %eq% "Attack" && ridx < nrow(things$dvw$plays) && things$dvw$plays$skill[ridx+1] %eq% "Block") {
                ## give the block the same time
                things$dvw$plays$video_time[ridx+1] <- round(tm, digits = input$video_time_decimal_places)
                skip <- 2
            }
            ## advance to the next skill row
            if (ridx < nrow(things$dvw$plays)) {
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
    ##        things$dvw$plays$video_time[ridx] <- round(tm, digits = input$video_time_decimal_places)
    ##        ## advance to the next skill row
    ##        if (ridx < nrow(things$dvw$plays)) {
    ##            next_skill_row <- find_next_skill_row(ridx, step = skip)
    ##            if (length(next_skill_row) > 0) playslist_select_row(next_skill_row)
    ##        }
    ##    }
    ##    NULL
    ##}

    selected_event <- reactive({
        if (length(input$playslist_rows_selected) == 1) {
            things$dvw$plays[input$playslist_rows_selected, ]
        } else {
            NULL
        }
    })

    output$current_event <- renderUI({
        tags$span(style = "font-size: large;", tags$strong("Current: "), selected_event()$code)
    })

    observe({
        if (!is.null(things$dvw) && nrow(things$dvw$plays) > 0 && !"error_message" %in% names(things$dvw$plays)) {
            things$dvw <- preprocess_dvw(things$dvw)
        }
    })

    plays_do_rename <- function(z) names_first_to_capital(dplyr::rename(z, plays_col_renames))
    ## the plays display in the RHS table
    output$playslist <- DT::renderDataTable({
        isolate(mydat <- things$dvw$plays) ## render once, then isolate from further renders - will be done by replaceData below
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
            scrollto <- max(input$playslist_rows_selected-1-5, 0) ## -1 for zero indexing, -5 to keep the selected row 5 from the top
            dojs(sprintf("$('#playslist').find('.dataTable').DataTable().scroller.toPosition(%s);", scrollto))
            ##dojs(sprintf("$('#playslist').find('.dataTable').DataTable().row(%s).node().scrollIntoView();", max(0, things$plays_row_to_select-1)))
            ##dojs(sprintf("console.dir($('#playslist').find('.dataTable').DataTable().row(%s).node())", max(0, things$plays_row_to_select-1)))
            ##dojs(sprintf("$('#playslist').find('.dataTables_scroll').animate({ scrollTop: $('#playslist').find('.dataTable').DataTable().row(%s).node().offsetTop }, 2000);", max(0, things$plays_row_to_select-1)))
        }
    }

    observe({
##        cat("replacing DT data\n")
        mydat <- things$dvw$plays
        mydat$is_skill <- is_skill(mydat$skill)
        DT::replaceData(playslist_proxy, data = mydat[, c(plays_cols_to_show, "is_skill"), drop = FALSE], rownames = FALSE, clearSelection = "none")##, resetPaging = FALSE)
        ## and scroll to selected row
        ##dojs(sprintf("$('#playslist').find('.dataTable').DataTable().row(%s).scrollTo();", max(0, things$plays_row_to_select-1)))
    })

    find_next_skill_row <- function(current_row_idx = NULL, step = 1) {
        if (is.null(current_row_idx)) current_row_idx <- input$playslist_rows_selected
        next_skill_row <- which(is_skill(things$dvw$plays$skill))
        next_skill_row <- next_skill_row[next_skill_row > current_row_idx]
        next_skill_row[min(step, length(next_skill_row))]
    }

    find_prev_skill_row <- function(current_row_idx = NULL, step = 1) {
        if (is.null(current_row_idx)) current_row_idx <- input$playslist_rows_selected
        next_skill_row <- which(is_skill(things$dvw$plays$skill))
        next_skill_row <- rev(next_skill_row[next_skill_row < current_row_idx])
        next_skill_row[min(step, length(next_skill_row))]
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
                    ##    things$cmd <- paste0(things$cmd, intToUtf8(mycmd))
                    ##    output$cmdbox <- renderText(things$cmd)
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
            } else if (ky %eq% "113") {
                ## insert new setting actions
                insert_setting_data_row()
            }  else if (ky %eq% "115") {
                ## delete all setting actions
                delete_setting_data_row()
            } 
        }
    })

    edit_current_code <- function() {
        ridx <- input$playslist_rows_selected
        if (!is.null(ridx)) {
            thiscode <- things$dvw$plays$code[ridx]
            editing$active <- "edit"
            showModal(modalDialog(title = "Edit code", size = "l", footer = actionButton("code_edit_cancel", label = "Cancel (or press Esc)"),
                                  textInput("code_entry", label = "Code:", value = thiscode),
                                  actionButton("code_do_edit", label = "Update code (or press Enter)")))
            ## focus to this textbox with cursor at end of input
            dojs("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById(\"code_entry\"); el.selectionStart = el.selectionEnd = el.value.length; el.focus(); });")
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
        } else if (editing$active %eq% "delete all setting actions"){
            ridx <- things$dvw$plays %>% mutate(rowN = row_number()) %>% dplyr::filter(skill %eq% "Set") %>% select(rowN) %>% pull()
            if (!is.null(ridx)) {
            if (is.logical(ridx)) ridx <- which(ridx)
            print(ridx)
            things$dvw$plays <- things$dvw$plays[-ridx, ]
            things$dvw <- reparse_dvw(things$dvw, dv_read_args = dv_read_args)
            }
        } else if(editing$active %eq% "insert setting actions") {
            ridx_set <- things$dvw$plays %>% mutate(add_set_before = case_when(skill == "Attack" & !(stringr::str_sub(code, 7,8) %in% c("PR", "PP", "P2") & !(lead(skill) %in% "Set")) ~ TRUE, 
                                                                               TRUE ~ FALSE), rowN = row_number()) %>% dplyr::filter(add_set_before %eq% TRUE) %>% select(rowN) %>% pull()
            if (!is.null(ridx)) {
                set_code <- things$dvw$plays %>% dplyr::filter(row_number() %in% ridx_set)  %>% 
                    mutate(team_oncourt_setter_number = case_when(team %in% home_team ~ case_when(home_setter_position == 1 ~ home_p1,
                                                                                                  home_setter_position == 2 ~ home_p2,
                                                                                                  home_setter_position == 3 ~ home_p3,
                                                                                                  home_setter_position == 4 ~ home_p4,
                                                                                                  home_setter_position == 5 ~ home_p5,
                                                                                                  home_setter_position == 6 ~ home_p6,
                                                                                                  TRUE ~ NA_real_),
                                                                  team %in% visiting_team ~ case_when(visiting_setter_position == 1 ~ visiting_p1,
                                                                                                      visiting_setter_position == 2 ~ visiting_p2,
                                                                                                      visiting_setter_position == 3 ~ visiting_p3,
                                                                                                      visiting_setter_position == 4 ~ visiting_p4,
                                                                                                      visiting_setter_position == 5 ~ visiting_p5,
                                                                                                      visiting_setter_position == 6 ~ visiting_p6,
                                                                                                      TRUE ~ NA_real_),
                                                                  TRUE ~ NA_real_)) %>%
                    mutate(set_code = paste0(str_sub(code,1,1), # Team
                                             team_oncourt_setter_number, # setter player_number
                                             "E", # set skill 
                                             str_sub(code, 5,5), # hitting tempo
                                             "+")) %>%
                    mutate(set_code = case_when(str_sub(code,7,8) %in% c("X1") ~ paste0(set_code, "K1"),
                                                str_sub(code,7,8) %in% c("X2") ~ paste0(set_code, "K2"),
                                                str_sub(code,7,8) %in% c("X7") ~ paste0(set_code, "K7"),
                                                #ts_pass_quality %in% c("Poor", "OK") ~ paste0(set_code, "KE"),
                                                TRUE ~ set_code))  %>%
                    mutate(set_code = case_when(str_sub(code,7,8) %in% c("X1") ~ paste0(set_code, "C3"),
                                                str_sub(code,7,8) %in% c("X2") ~ paste0(set_code, "C3"),
                                                str_sub(code,7,8) %in% c("X7") ~ paste0(set_code, "C3"),
                                                str_sub(code,7,8) %in% c("X5") ~ paste0(set_code, "~~F4"),
                                                str_sub(code,7,8) %in% c("V5") ~ paste0(set_code, "~~F4"),
                                                str_sub(code,7,8) %in% c("X6") ~ paste0(set_code, "~~F2"),
                                                str_sub(code,7,8) %in% c("V6") ~ paste0(set_code, "~~F2"),
                                                str_sub(code,7,8) %in% c("X8") ~ paste0(set_code, "~~B9"),
                                                str_sub(code,7,8) %in% c("V8") ~ paste0(set_code, "~~B9"),
                                                str_sub(code,7,8) %in% c("XP") ~ paste0(set_code, "~~P8"),
                                                str_sub(code,7,8) %in% c("VP") ~ paste0(set_code, "~~P8"),
                                                TRUE ~ set_code)) %>% select(set_code) %>% pull()
                newline <- things$dvw$plays[ridx_set, ]
                newline$code <- set_code
                newline$team_touch_id <- newline$team_touch_id - 0.5
                things$dvw$plays <- bind_rows(things$dvw$plays, newline) %>% arrange(set_number, point_id, team_touch_id)
            }
        } else {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                if (editing$active %eq% "edit") {
                    ## update the code in the current row
                    things$dvw$plays$code[ridx] <- input$code_entry
                } else if (editing$active %eq% "insert") {
                    ## insert new line
                    newline <- things$dvw$plays[ridx, ]
                    newline$code <- input$code_entry
                    things$dvw$plays <- bind_rows(things$dvw$plays[seq(1, ridx, by = 1), ], newline, things$dvw$plays[seq(ridx+1, nrow(things$dvw$plays), by = 1), ])
                } else if (editing$active %eq% "insert setting") {
                    ## insert new line
                    newline <- things$dvw$plays[ridx, ]
                    newline$code <- input$code_entry
                    things$dvw$plays <- bind_rows(things$dvw$plays[seq(1, ridx, by = 1), ], newline, things$dvw$plays[seq(ridx+1, nrow(things$dvw$plays), by = 1), ])
                } else if (editing$active %eq% "delete") {
                    if (is.logical(ridx)) ridx <- which(ridx)
                    things$dvw$plays <- things$dvw$plays[-ridx, ]
                }
                ## reparse the dvw
                things$dvw <- reparse_dvw(things$dvw, dv_read_args = dv_read_args)
            }
        }
        editing$active <- NULL
        scroll_playlist()
    }
    insert_data_row <- function() {
        ridx <- input$playslist_rows_selected
        if (!is.null(ridx)) {
            thiscode <- things$dvw$plays$code[ridx]
            editing$active <- "insert"
            showModal(modalDialog(title = "Insert new code", size = "l", footer = actionButton("code_edit_cancel", label = "Cancel (or press Esc)"),
                                  textInput("code_entry", label = "Code:", value = ""),
                                  actionButton("code_do_edit", label = "Insert code (or press Enter)")))
            ## focus to this textbox with cursor at end of input
            dojs("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById(\"code_entry\"); el.selectionStart = el.selectionEnd = el.value.length; el.focus(); });")
        }
    }
    delete_data_row <- function() {
        ridx <- input$playslist_rows_selected
        if (!is.null(ridx)) {
            thiscode <- things$dvw$plays$code[ridx]
            editing$active <- "delete"
            showModal(modalDialog(title = "Delete code", size = "l", footer = actionButton("code_edit_cancel", label = "Cancel (or press Esc)"),
                                  actionButton("code_do_delete", label = "Confirm delete code (or press Enter)")))
        }
    }

    insert_setting_data_row <- function() {
        #ridx <- input$playslist_rows_selected
        ridx_set <- things$dvw$plays %>% mutate(add_set_before = case_when(skill == "Attack" & !(stringr::str_sub(code, 7,8) %in% c("PR", "PP", "P2") & !(lead(skill) %in% "Set")) ~ TRUE, 
                                                                       TRUE ~ FALSE),
                                                rowN = row_number()) %>% 
            dplyr::filter(add_set_before %eq% TRUE) %>% select(rowN) %>% pull() # idx row number to insert setting action
        
        if (!is.null(ridx_set)) {
            editing$active <- "insert setting actions"
            showModal(modalDialog(title = "Insert setting codes", size = "l", footer = actionButton("code_edit_cancel", label = "Cancel (or press Esc)"),
                                  actionButton("code_do_edit", label = "Confirm insert setting codes (or press Enter)")))
            
        }
    }
    delete_setting_data_row <- function() {
        ridx <- things$dvw$plays %>% mutate(rowN = row_number()) %>% dplyr::filter(skill %eq% "Set") %>% select(rowN) %>% pull()
            if (!is.null(ridx)) {
            thiscode <- things$dvw$plays$code[ridx]
            editing$active <- "delete all setting actions"
            showModal(modalDialog(title = "Delete all setting codes", size = "l", footer = actionButton("code_edit_cancel", label = "Cancel (or press Esc)"),
                                  actionButton("code_do_delete", label = "Confirm delete all setting codes (or press Enter)")))
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
    if (is.null(things$dvw)) {
        NULL
    } else {
        downloadButton("save_file_button", "Save file")
    }
    })
    output$save_file_button <- downloadHandler(
        filename = reactive(
            if (!is.null(things$dvw$meta$filename) && !is.na(things$dvw$meta$filename)) basename(things$dvw$meta$filename) else "myfile.dvw"
        ),
        content = function(file) dv_write(things$dvw, file = file)
    )

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
