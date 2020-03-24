## not exported

ov_shiny_video_sync_ui <- function(app_data) {
    ## some startup stuff
    running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
    video_src <- app_data$dvw$meta$video$file[1]
    if (!fs::file_exists(video_src)) {
        ## can't find the file, go looking for it
        chk <- ovideo::ov_find_video_file(dvw_filename = app_data$dvw_filename, video_filename = video_src)
        if (!is.na(chk)) video_src <- chk
    }

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
              tags$head(tags$style("body{font-size:15px} .well{padding:15px;} .myhidden {display:none;} table {font-size: small;} #headerblock h1,#headerblock h2,#headerblock h3,#headerblock h4 {font-weight: normal; color:#ffffff;} h2, h3, h4 {font-weight: bold;} .shiny-notification { height: 100px; width: 400px; position:fixed; top: calc(50% - 50px); left: calc(50% - 200px); } .code_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:70%;} .clet {color: red;} .iconbut { font-size: 150%; }"),
                        tags$style("#hroster {padding-left: 0px; padding-right: 0px; background-color: #bfefff; padding: 12px;} #vroster {padding-left: 0px; padding-right: 0px; background-color: #bcee68; padding: 12px;}"),
                        ##tags$style("table {font-size: 10px; line-height: 1.0;"),
                        ##tags$script("$(document).on('shiny:sessioninitialized', function() { document.getElementById('main_video').addEventListener('focus', function() { this.blur(); }, false); });"),
                        ##key press handling
                        tags$script("$(document).on('keypress', function (e) { var el = document.activeElement; var len = -1; if (typeof el.value != 'undefined') { len = el.value.length; }; Shiny.onInputChange('cmd', e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('keydown', function (e) { var el = document.activeElement; var len = -1; if (typeof el.value != 'undefined') { len = el.value.length; }; Shiny.onInputChange('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('shiny:sessioninitialized',function() { Shiny.onInputChange('window_height', $(window).innerHeight()); Shiny.onInputChange('window_width', $(window).innerWidth()); });"),
                        tags$script("var rsztmr; $(window).resize(function() { clearTimeout(rsztmr); rsztmr = setTimeout(doneResizing, 500); }); function doneResizing() { Shiny.onInputChange('window_height', $(window).innerHeight()); Shiny.onInputChange('window_width', $(window).innerWidth()); }"),
                        tags$title("Volleyball scout and video sync")
                        ),
              fluidRow(id = "headerblock", style = "border-radius:4px;padding:10px;margin-bottom:10px;min-height:160px;color:white;background: #ffffff url(\"https://untan.gl/images/bgrev.jpg\") 0 0/100% auto no-repeat;", ## background image in header block
                       column(6, offset = 2, tags$h2("Volleyball scout and video sync")),
                       column(4,tags$div(style="float:right;padding:10px;", tags$a(href = "https://untan.gl", tags$img(style = "width:16em;max-width:100%", src = "https://untan.gl/images/su_title-w.png"))))
                       ),
              fluidRow(column(7, tags$video(id = "main_video", style = "border: 1px solid black; width: 90%;", src = file.path(video_server_base_url, basename(video_src)), controls = "controls", autoplay = "false"),
                              fluidRow(column(8, actionButton("all_video_from_clock", label = "Open video/clock time operations menu"),
                                              actionButton("edit_match_data_button", "Edit match data"),
                                              actionButton("edit_teams_button", "Edit teams"),
                                              uiOutput("save_file_ui", inline = TRUE)),
                                       column(4, uiOutput("current_event"))),
                              tags$div(style = "height: 14px;"),
                              fluidRow(column(6,
                                              tags$p(tags$strong("Video controls")), sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1), tags$ul(tags$li("[l or 6] forward 2s, [; or ^] forward 10s, [m or 3] forwards 0.1s"), tags$li("[j or 4] backward 2s, [h or $] backward 10s, [n or 1] backwards 0.1s"), tags$li("[q or 0] pause video"), tags$li("[g or #] go to currently-selected event")),
                                              tags$p(tags$strong("Keyboard controls")), tags$ul(tags$li("[r or 5] sync selected event video time"),
                                                                                                tags$li("[i or 8] move to previous skill row"),
                                                                                                tags$li("[k or 2] move to next skill row"),
                                                                                                tags$li("[e or E] edit current code"),
                                                                                                tags$li("[del] delete current code"),
                                                                                                tags$li("[ins] insert new code above current"),
                                                                                                tags$li("[F2] insert setting codes before every attack"),
                                                                                                tags$li("[F4] delete all setting codes"),
                                                                                                tags$li("[F6] insert digging codes after every attack"),
                                                                                                tags$li("[F8] delete all digging codes")
                                                                                                ),
                                              tags$p(tags$strong("Other options")),
                                              tags$span("Decimal places on video time:"),
                                              numericInput("video_time_decimal_places", label = NULL, value = 0, min = 0, max = 2, step = 1, width = "6em"),
                                              uiOutput("vtdp_ui")),
                                       column(6, wellPanel(
                                                     fluidRow(column(3, id = "hroster", uiOutput("htrot_ui")),
                                                              column(6, plotOutput("court_inset"), actionButton("court_inset_swap", label = "\u21f5", class = "iconbut")),
                                                              column(3, id = "vroster", uiOutput("vtrot_ui")))))
                                       )
                              ),
                       column(5, DT::dataTableOutput("playslist", width = "98%"),
                              uiOutput("error_message"))
                       )
              )
    ## find negative time intervals and fix them
}

ov_shiny_video_sync_server <- function(app_data) {
    function(input, output, session) {
        styling <- list(h_court_colour = "#bfefff", ## lightblue1
                        h_court_highlight = "darkblue",
                        v_court_colour = "#bcee68", ## darkolivegreen2
                        v_court_highlight = "darkgreen")

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
        no_set_attacks <- c("PR", "PP", "P2") ## attacks that don't need a set inserted before them
        default_set_evaluation <- "+" ## for inserted sets
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

        ## some local markup to make the helper entries easier here
        ## | = <br />
        ## {thing} = <strong>thing</strong>
        ## [thing] = <span class=\"clet\">thing</span>
        ## (thing) = <em>thing</em>
        ## --- = <br /><hr />
        gsubf <- function(...) gsub(..., fixed = TRUE)
        mu2html <- function(z) gsubf("[", "<span class=\"clet\">", gsubf("]", "</span>", gsubf("{", "<strong>", gsubf("}", "</strong>", gsubf("|", "<br />", gsubf("(", "<em>", gsubf(")", "</em>", gsubf("---", "<br /><hr />", z))))))))
        paste0_noNA <- function(...) do.call(paste0, Filter(Negate(is.na), list(...)))
        special_helper <- function(skill, evaln) {
            htext <- NA_character_
            if (!is.null(skill) && !is.null(evaln)) {
                htext <- case_when(skill %eq% "A" & evaln %eq% "#" ~ "(Attk kill)|Blk out [S]ide|Blk out l[O]ng|Blk on [F]loor|[X] Direct|on floor",
                                   skill %eq% "A" & evaln %eq% "=" ~ "(Attk err)|Out [S]ide|Out l[O]ng|In [N]et|[I] net contct|[A]ntenna|[Z] ref call",
                                   skill %eq% "A" ~ "(Attk)|blk [C]ontrol|[N] let",
                                   skill %eq% "B" & evaln %in% c("=", "/") ~ "(Blk err)|Out [S]ide|Out l[O]ng|Ball on [F]lr|[X] between|hands|[N] net touch|[A]ntenna|[P] no jump|[T] pos error|[Z] ref call",
                                   skill %eq% "R" ~ "(Rcv)|[U]nplayable|[X] body err|[P]os err|No [E]ffort|[Z] ref call",
                                   skill %eq% "S" & evaln %eq% "#" ~ "(Srv ace)|[N] let",
                                   skill %eq% "S" & evaln %eq% "=" ~ "(Srv err)|Out l[O]ng|Out [L]eft|Out [R]ight|In [N]et|[Z] ref call",
                                   skill %eq% "S" ~ "(Srv)|[N] let",
                                   skill %eq% "E" & evaln %eq% "=" ~ "(Set err)|[U]nhittable|[I] net tch|[Z] ref call",
                                   skill %eq% "Dig" & evaln %eq% "=" ~ "(Dig err)|[U]nplayable|[X] body err|[P]os err|[Z] Ref call|Ball on [F]lr|Ball [O]ut|No [E]ffort",
                                   skill %eq% "Freeball" & evaln %eq% "=" ~ "(Fr err)|[U]nplayable|[X] body err|[P]os err|[Z] Ref call")
            }
            mu2html(paste0_noNA("{Special}---", htext))
        }
        skill_type_helper <- function(skill, evaln) {
            htext <- NA_character_
            if (!is.null(skill)) {
                htext <- case_when(skill %eq% "A" ~ "(Attk)|[H]ard|[P] soft|[T]ip",
                                   skill %eq% "R" ~ "(Rec)|[L]eft|[R]ight|lo[W]|[O]vrhnd|[M]idline",
                                   skill %eq% "E" ~ "(Set)|[1] hand|[2] hands|[3] bump|[4] othr|[5] uhand",
                                   skill %eq% "D" ~ "(Dig)|[S] on spk|[C] spk|cover|[B] aftr|block|[E] emerg")
            }
            mu2html(paste0_noNA("{Skill|type}---", htext))
        }
        num_players_helper <- function(skill, evaln) {
            htext <- NA_character_
            if (!is.null(skill)) {
                htext <- case_when(skill %in% c("A", "B") ~ "(Attk|Blk)|[0]..[3]|[4] hole|block",
                                   skill %eq% "R" ~ "(Rcv)|[1] 2p,L|[2] 2p,R|[3] 3p,L|[4] 3p,M|[5] 3p,R|[6] 4p,L|[7] 4p,LC|[8] 4p,RC|[9] 4p,R")
            }
            mu2html(paste0_noNA("{Num|plyrs}---", htext))
        }
        end_zone_helper <- function(skill, evaln) {
            if (!is.null(skill) && skill %eq% "A" && rdata$dvw$meta$match$zones_or_cones %eq% "C") {
                mu2html("{End cone}|(Attk)|[1..8]")
            } else {
                mu2html("{End zone}---[1..9]")
            }
        }
        code_bits_tbl$helper <- c(mu2html("{Team}---[*]&nbsp;H|[a]&nbsp;V"), ## team
                                  mu2html("{Plyr|num}"), ## number
                                  mu2html("{Skill}---[S]rv|[R]ec|[A]ttk|[B]lk|[D]ig|s[E]t|[F]reeb"), ## skill
                                  mu2html("{Tempo}---[H]igh|[M]ed|[Q]uick|[T]ense|s[U]per|[N] fast|[O]ther"), ## type
                                  mu2html("{Eval}---[#|+|!|-|/|=]"), ## eval
                                  mu2html("{Combo}---(Atk code)|[X.]|[C.]|etc||(Set call)|[K.]"), ## combo
                                  mu2html("{Target}---[F]ront|[C]ntr|[B]ack|[P]ipe|[S]etr"), ## target
                                  mu2html("{Start|zone}---(Attk)|[1..9]||(Srv)|[57691]"), ##start_zone
                                  end_zone_helper, ##end_zone
                                  mu2html("{End|subzn}---[ABCD]"), ##end_subzone
                                  skill_type_helper, ##skill_type
                                  num_players_helper, ##players
                                  special_helper, ##special
                                  mu2html("{Custom}---")) ##custom
        ## note that if any other helpers are turned into functions, they need extra code added below to handle them (see ADD HANDLERS HERE)
        code_bits_tbl$start <- cumsum(dplyr::lag(code_bits_tbl$width, default = 0))+1L
        code_bits_tbl$end <- code_bits_tbl$start+code_bits_tbl$width-1L

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
                rdata$dvw$plays$video_time[ridx] <- if (input$video_time_decimal_places < 1) floor(tm) else round(tm, digits = input$video_time_decimal_places)
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
            if (!is.null(input$cmd)) {
                temp <- strsplit(input$cmd, "@")[[1]]
                ## elements are keyid element_class element_id cursor_position field_length time
                mycmd <- temp[1]
                myclass <- temp[2]
                if (!is.null(myclass) && nzchar(myclass) && myclass %in% c("form-control")) {
                    ## don't process these - they are e.g. key events in DT filter boxes
                    mycmd <- NULL
                }
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
                            if (!editing$active %eq% "teams") code_make_change()
                            ## but not for team editing, because pressing enter in the DT fires this too
                        } else if (mycmd %eq% "27") {
                            ## not sure if this will be detected by keypress, maybe only keydown (may be browser specific)
                            ## esc
                            if (!editing$active %eq% "teams") {
                                editing$active <- NULL
                                removeModal()
                            }
                        }
                    } else {
                        ## editing not active
                        if (mycmd %in% utf8ToInt("eE")) {
                            ## open code editing dialog
                            edit_current_code()
                        } else if (mycmd %eq% "45") {
                            ## insert new row below current
                            insert_data_row()
                        } else if (mycmd %eq% "8") {
                            ## backspace
                        } else if (mycmd %eq% "46") {
                            ## delete key, handled via input$controlkey
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
            }
        })
        observeEvent(input$controlkey, {
            ## keys that might not get detected by keypress but do by keydown?
            if (!is.null(input$controlkey)) {
                temp <- strsplit(input$controlkey, "@")[[1]]
                ## elements are modifiers_and_key element_class element_id cursor_position field_length time
                mycmd <- temp[1]
                myclass <- temp[2]
                myid <- temp[3]
                suppressWarnings({
                    curpos <- as.integer(temp[4])
                    fieldlen <- as.integer(temp[5])
                })
                if (!is.null(myclass) && nzchar(myclass) && myclass %in% c("form-control")) {
                    ## don't process these - they are e.g. key events in DT filter boxes
                    mycmd <- NULL
                }
                if (!is.null(mycmd)) {
                    if (debug > 1) cat("control key: ", mycmd, "\n")
                    mycmd <- strsplit(mycmd, "|", fixed = TRUE)[[1]] ## e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which
                    if (length(mycmd) == 5) {
                        ky <- mycmd[5]
                        if (ky %eq% "27") {
                            ## esc
                            if (is.null(editing$active) || !editing$active %eq% "teams") {
                                editing$active <- NULL
                                removeModal()
                            }
                        } else if (ky %eq% "45" && is.null(editing$active)) {
                            ## insert new row below current
                            insert_data_row()
                        } else if (ky %eq% "46" && is.null(editing$active)) {
                            ## delete current row
                            delete_data_row()
                        } else if (ky %eq% "113") {
                            ## insert new setting actions
                            insert_setting_data_row()
                        }  else if (ky %eq% "115") {
                            ## delete all setting actions
                            delete_setting_data_row()
                        }  else if (ky %eq% "117") {
                            ## insert new digging actions
                            insert_dig_data_row()
                        }  else if (ky %eq% "119") {
                            ## delete all digging actions
                            delete_dig_data_row()
                        } else if (ky %eq% "37") {
                            ## 37 (left arrow)
                            if (curpos %eq% 0L && grepl("shiny-bound-input", myclass, fixed = TRUE)) {
                                ## find prev code_entry_* element
                                myid <- sub("^code_entry_", "", myid)
                                prevel <- which(code_bits_tbl$bit %eq% myid)-1
                                if (length(prevel) == 1 && prevel >= 1) focus_to_element(paste0("code_entry_", code_bits_tbl$bit[prevel]))
                            }
                        } else if (ky %eq% "38") {
                            ## 38 (up arrow)
                        } else if (ky %eq% "39") {
                            ## 39 (right arrow)
                            if (curpos %eq% fieldlen && grepl("shiny-bound-input", myclass, fixed = TRUE)) {
                                ## find next code_entry_* element
                                myid <- sub("^code_entry_", "", myid)
                                nextel <- which(code_bits_tbl$bit %eq% myid)+1
                                if (length(nextel) == 1 && nextel <= nrow(code_bits_tbl)) focus_to_element(paste0("code_entry_", code_bits_tbl$bit[nextel]))
                            }
                        } else if (ky %eq% "40") {
                            ## 40 (down arrow)
                        }
                    }
                }
            }
        })

        edit_current_code <- function() {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "edit"
                showModal(modalDialog(title = "Edit code", size = "l", footer = tags$div(actionButton("edit_commit", label = "Update code (or press Enter)"), actionButton("edit_cancel", label = "Cancel (or press Esc)")),
                                      "Edit code either in the top text box or in the individual boxes (but not both)",
                                      textInput("code_entry", label = "Code:", value = thiscode),
                                      "or",
                                      build_code_entry_guide("edit", rdata$dvw$plays[ridx, ])
                                      ))
                if (!is_skill(rdata$dvw$plays$skill[ridx])) {
                    ## if it's a non-skill code then focus into the code_entry textbox with cursor at end of input
                    focus_in_code_entry("code_entry")
                } else {
                    ## otherwise focus into the appropriate code_entry_guide sub-box
                    this_skill <- rdata$dvw$plays$skill[ridx]
                    if (this_skill %in% c("Serve", "Reception")) {
                        if (is.na(rdata$dvw$plays$start_zone[ridx])) {
                            focus_in_code_entry("code_entry_start_zone")
                        } else {
                            focus_in_code_entry("code_entry_end_zone")
                        }
                    } else if (this_skill %in% c("Attack")) {
                        focus_in_code_entry("code_entry_end_zone")
                    } else if (this_skill %in% c("Dig")) {
                        focus_in_code_entry("code_entry_eval")
                    } else {
                        focus_in_code_entry("code_entry_skill")
                    }
                }
            }
        }
        focus_in_code_entry <- function(id, highlight_all = TRUE) {
            ## function to set the cursor focus to a particular entry box
            if (!highlight_all) {
                dojs(paste0("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById('", id, "'); el.selectionStart = el.selectionEnd = el.value.length; el.focus(); });"))
            } else {
                dojs(paste0("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById('", id, "'); el.selectionStart = 0; el.selectionEnd = el.value.length; el.focus(); });"))
            }
        }
        focus_to_element <- function(id, highlight_all = TRUE) {
            ## function to set the cursor focus to a particular entry box
            if (!highlight_all) {
                dojs(paste0("var el = document.getElementById('", id, "'); el.selectionStart = el.selectionEnd = el.value.length; el.focus();"))
            } else {
                dojs(paste0("var el = document.getElementById('", id, "'); el.selectionStart = 0; el.selectionEnd = el.value.length; el.focus();"))
            }
        }
        observeEvent(input$edit_cancel, {
            if (editing$active %in% "teams") {
                htdata_edit(NULL)
                vtdata_edit(NULL)
            }
            editing$active <- NULL
            removeModal()
        })
        observeEvent(input$edit_commit, {
            code_make_change()
        })
        code_make_change <- function() {
            removeModal()
            do_reparse <- FALSE
            if (is.null(editing$active)) {
                ## not triggered from current editing activity, huh?
                warning("code_make_change entered but editing not active")
            } else if (editing$active %eq% "teams") {
                ## update from all the input$ht_edit_name/id/coach/assistant inputs
                htidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
                rdata$dvw$meta$teams$team[htidx] <- input$ht_edit_name
                rdata$dvw$meta$teams$team_id[htidx] <- input$ht_edit_id
                rdata$dvw$meta$teams$coach[htidx] <- input$ht_edit_coach
                rdata$dvw$meta$teams$assistant[htidx] <- input$ht_edit_assistant
                if (!is.null(htdata_edit())) {
                    rdata$dvw$meta$players_h <- htdata_edit()
                    rdata$dvw$meta$players_h$name <- paste(rdata$dvw$meta$players_h$firstname, rdata$dvw$meta$players_h$lastname)
                }
                ## and visiting team
                vtidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
                rdata$dvw$meta$teams$team[vtidx] <- input$vt_edit_name
                rdata$dvw$meta$teams$team_id[vtidx] <- input$vt_edit_id
                rdata$dvw$meta$teams$coach[vtidx] <- input$vt_edit_coach
                rdata$dvw$meta$teams$assistant[vtidx] <- input$vt_edit_assistant
                if (!is.null(vtdata_edit())) {
                    rdata$dvw$meta$players_v <- vtdata_edit()
                    rdata$dvw$meta$players_v$name <- paste(rdata$dvw$meta$players_v$firstname, rdata$dvw$meta$players_v$lastname)
                }
                do_reparse <- TRUE
            } else if (editing$active %eq% "match_data") {
                rdata$dvw$meta$match$date <- input$match_edit_date
                rdata$dvw$meta$match$time <- tryCatch(lubridate::hms(input$match_edit_time), error = function(e) lubridate::as.period(NA))
                rdata$dvw$meta$match$season <- input$match_edit_season
                rdata$dvw$meta$match$league <- input$match_edit_league
                rdata$dvw$meta$match$phase <- input$match_edit_phase
                rdata$dvw$meta$match$home_away <- input$match_edit_home_away
                rdata$dvw$meta$match$day_number <- input$match_edit_day_number
                rdata$dvw$meta$match$match_number <- input$match_edit_match_number
                rdata$dvw$meta$match$regulation <- input$match_edit_regulation
                rdata$dvw$meta$match$zones_or_cones <- input$match_edit_zones_or_cones
                do_reparse <- TRUE
            } else if (editing$active %eq% "delete all setting actions") {
                ridx <- dplyr::filter(mutate(rdata$dvw$plays, rowN = row_number()), .data$skill %eq% "Set")$rowN
                if (length(ridx) > 0) {
                    if (is.logical(ridx)) ridx <- which(ridx)
                    rdata$dvw$plays <-rdata$dvw$plays[-ridx, ]
                    do_reparse <- TRUE
                }
            } else if (editing$active %eq% "insert setting actions") {
                ridx_set <- mutate(rdata$dvw$plays, rowN = row_number(),
                                   add_set_before = case_when(.data$skill %eq% "Attack" & !(str_sub(.data$code, 7, 8) %in% no_set_attacks) & !(lag(.data$skill) %eq% "Set") ~ TRUE,
                                                              TRUE ~ FALSE))
                ridx_set <- ridx_set$rowN[which(ridx_set$add_set_before)]
                if (length(ridx_set) > 0) {
                    set_code <- mutate(rdata$dvw$plays, passQ = case_when(lag(.data$skill) %in% c("Reception", "Dig") ~ lag(.data$evaluation)))
                    set_code <- mutate(dplyr::filter(set_code, row_number() %in% ridx_set),
                                       team_oncourt_setter_number = case_when(.data$team %eq% .data$home_team ~ case_when(.data$home_setter_position == 1 ~ .data$home_p1,
                                                                                                                          .data$home_setter_position == 2 ~ .data$home_p2,
                                                                                                                          .data$home_setter_position == 3 ~ .data$home_p3,
                                                                                                                          .data$home_setter_position == 4 ~ .data$home_p4,
                                                                                                                          .data$home_setter_position == 5 ~ .data$home_p5,
                                                                                                                          .data$home_setter_position == 6 ~ .data$home_p6),
                                                                              .data$team %eq% .data$visiting_team ~ dplyr::case_when(.data$visiting_setter_position == 1 ~ .data$visiting_p1,
                                                                                                                                     .data$visiting_setter_position == 2 ~ .data$visiting_p2,
                                                                                                                                     .data$visiting_setter_position == 3 ~ .data$visiting_p3,
                                                                                                                                     .data$visiting_setter_position == 4 ~ .data$visiting_p4,
                                                                                                                                     .data$visiting_setter_position == 5 ~ .data$visiting_p5,
                                                                                                                                     .data$visiting_setter_position == 6 ~ .data$visiting_p6)),
                                       set_code = paste0(str_sub(.data$code, 1, 1), ## Team
                                                         .data$team_oncourt_setter_number, ## setter player_number
                                                         "E", # set skill
                                                         str_sub(.data$code, 5, 5), # hitting tempo
                                                         default_set_evaluation),
                                       TEMP_attack_code = str_sub(.data$code, 7, 8),
                                       setter_call = case_when(.data$TEMP_attack_code %eq% "X1" ~ "K1",
                                                               .data$TEMP_attack_code %eq% "X2" ~ "K2",
                                                               .data$TEMP_attack_code %eq% "X7" ~ "K7",
                                                               grepl("^(OK|Negative)", .data$passQ) ~ "KE",
                                                               grepl("^(Perfect|Positive)", .data$passQ) ~ "KK",
                                                               is.na(.data$passQ) ~ "KK",
                                                               TRUE ~ "~~"),
                                       set_code = paste0(.data$set_code, .data$setter_call))
                    ## TODO, these could be taken from the dvw$meta$attacks table, and then they would keep up with user configuration
                    set_code <- mutate(set_code, set_code = dplyr::case_when(.data$TEMP_attack_code %eq% "X1" ~ paste0(.data$set_code, "C3"),
                                                                             .data$TEMP_attack_code %eq% "X2" ~ paste0(.data$set_code, "C3"),
                                                                             .data$TEMP_attack_code %eq% "X7" ~ paste0(.data$set_code, "C3"),
                                                                             .data$TEMP_attack_code %eq% "X5" ~ paste0(.data$set_code, "F4"),
                                                                             .data$TEMP_attack_code %eq% "V5" ~ paste0(.data$set_code, "F4"),
                                                                             .data$TEMP_attack_code %eq% "X6" ~ paste0(.data$set_code, "B2"),
                                                                             .data$TEMP_attack_code %eq% "V6" ~ paste0(.data$set_code, "B2"),
                                                                             .data$TEMP_attack_code %eq% "X8" ~ paste0(.data$set_code, "B9"),
                                                                             .data$TEMP_attack_code %eq% "V8" ~ paste0(.data$set_code, "B9"),
                                                                             .data$TEMP_attack_code %eq% "XP" ~ paste0(.data$set_code, "P8"),
                                                                             .data$TEMP_attack_code %eq% "VP" ~ paste0(.data$set_code, "P8"),
                                                                             TRUE ~ .data$set_code))$set_code
                    rdata$dvw$plays <- mutate(rdata$dvw$plays, tmp_row_number = row_number())
                    newline <- rdata$dvw$plays[ridx_set, ]
                    newline$code <- set_code
                    newline$video_time <- newline$video_time - 2
                    newline$tmp_row_number <- newline$tmp_row_number - 0.5
                    rdata$dvw$plays <- dplyr::arrange(bind_rows(rdata$dvw$plays, newline), .data$tmp_row_number)##.data$set_number, .data$point_id, .data$team_touch_id, .data$tmp_row_number)
                    rdata$dvw$plays <- dplyr::select(rdata$dvw$plays, -"tmp_row_number")
                    do_reparse <- TRUE
                }
            } else if (editing$active %eq% "delete all digging actions") {
                ridx <- dplyr::filter(mutate(rdata$dvw$plays, rowN = row_number()), .data$skill %eq% "Dig")$rowN
                if (length(ridx) > 0) {
                    if (is.logical(ridx)) ridx <- which(ridx)
                    rdata$dvw$plays <- rdata$dvw$plays[-ridx, ]
                    do_reparse <- TRUE
                }
            } else if (editing$active %eq% "insert digging actions") {
                ## find attacks that remained in play, and which were not followed by a dig, nor followed by a block then a dig
                ridx_dig <- mutate(rdata$dvw$plays, rowN = row_number(),
                                   attack_in_play = .data$skill %eq% "Attack" & grepl("^(Positive|Poor|Blocked for reattack|Spike in play)", .data$evaluation, ignore.case = TRUE),
                                   add_dig_after = case_when(.data$attack_in_play & !lead(.data$skill) %in% c("Dig", "Block") ~ TRUE, ## attack in play, not followed by dig or block
                                                             lag(.data$attack_in_play) & .data$skill %eq% "Block" & !.data$evaluation %in% c("Error", "Invasion", "Winning block") & !lead(.data$skill) %eq% "Dig" ~ TRUE, ## block touch after attack in play, not followed by dig
                                                             TRUE ~ FALSE))
                ridx_dig <- ridx_dig$rowN[which(ridx_dig$add_dig_after)]
                if (length(ridx_dig) > 0) {
                    dig_code <- mutate(dplyr::filter(rdata$dvw$plays, row_number() %in% ridx_dig),
                                       dig_team = case_when(.data$evaluation %eq% "Blocked for reattack" & .data$team == .data$home_team ~ "*",
                                                            .data$evaluation %eq% "Blocked for reattack" & .data$team == .data$visiting_team ~ "a",
                                                            grepl("^(Positive|Poor|Spike in play)", .data$evaluation) & .data$team == .data$home_team ~ "a",
                                                            grepl("^(Positive|Poor|Spike in play)", .data$evaluation) & .data$team == .data$visiting_team ~ "*"),
                                       dig_eval_code = case_when(.data$skill %eq% "Attack" & grepl("^Positive", .data$evaluation) ~ "-", ## negative dig on positive attack
                                                                 TRUE ~ "+"), ## positive dig otherwise
                                       dig_code = paste0(.data$dig_team, ## Team
                                                         "00", ## dig player_number
                                                         "D", ## dig skill
                                                         str_sub(.data$code, 5, 5), # hitting tempo
                                                         .data$dig_eval_code))
                    dig_code <- dig_code$dig_code
                    rdata$dvw$plays <- mutate(rdata$dvw$plays, tmp_row_number = row_number())
                    newline <- rdata$dvw$plays[ridx_dig, ]
                    newline$code <- dig_code
                    newline$video_time <- newline$video_time + 1
                    newline$tmp_row_number <- newline$tmp_row_number + 0.5
                    rdata$dvw$plays <- dplyr::arrange(bind_rows(rdata$dvw$plays, newline), .data$tmp_row_number) ##.data$set_number, .data$point_id, .data$team_touch_id, .data$tmp_row_number)
                    rdata$dvw$plays <- dplyr::select(rdata$dvw$plays, -"tmp_row_number")
                    do_reparse <- TRUE
                }
            } else {
                ridx <- input$playslist_rows_selected
                if (!is.null(ridx)) {
                    if (editing$active %in% c("edit", "insert")) {
                        current_code <- rdata$dvw$plays$code[ridx]
                        ## user has changed EITHER input$code_entry or used the code_entry_guide
                        ## infer code from code_entry_guide elements
                        newcode1 <- lapply(seq_len(nrow(code_bits_tbl)), function(bi) {
                            val <- input[[paste0("code_entry_", code_bits_tbl$bit[bi])]]
                            if (is.null(val)) val <- ""
                            wid <- code_bits_tbl$width[bi]
                            if (nchar(val) < wid) val <- str_pad(val, wid, side = "right", pad = "~")
                            val
                        })
                        newcode1 <- sub("~+$", "", paste(newcode1, collapse = ""))## trim trailing ~'s
                        newcode2 <- input$code_entry
                        changed1 <- (!newcode1 %eq% current_code) && nzchar(newcode1)
                        changed2 <- (!newcode2 %eq% current_code) && nzchar(newcode2)
                        if (!changed1 && changed2) {
                            newcode <- newcode2
                        } else if (!changed2 && changed1) {
                            newcode <- newcode1
                        } else if (!changed1 && !changed2) {
                            ## neither changed, nothing to do
                            newcode <- NULL
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
                        rdata$dvw$plays <- bind_rows(if (ridx > 1) rdata$dvw$plays[seq(1, ridx-1L, by = 1), ], newline, rdata$dvw$plays[seq(ridx, nrow(rdata$dvw$plays), by = 1), ])
                        ## set the newly-inserted line as the active row
                        ##nsr <- find_next_skill_row()
                        ##if (length(nsr) > 0) playslist_select_row(nsr)
                    } else if (editing$active %eq% "delete") {
                        if (is.logical(ridx)) ridx <- which(ridx)
                        rdata$dvw$plays <- rdata$dvw$plays[-ridx, ]
                    }
                    do_reparse <- TRUE
                }
            }
            if (do_reparse) {
                ## reparse the dvw
                rdata$dvw <- reparse_dvw(rdata$dvw, dv_read_args = dv_read_args)
                scroll_playlist()
            }
            editing$active <- NULL
        }
        insert_data_row <- function() {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                if (ridx > 1) ridx <- ridx-1L ## we are inserting above the selected row, so use the previous row to populate this one
                editing$active <- "insert"
                showModal(modalDialog(title = "Insert new code", size = "l", footer = tags$div(actionButton("edit_commit", label = "Insert code (or press Enter)"), actionButton("edit_cancel", label = "Cancel (or press Esc)")),
                                      "Enter new code either in the top text box or in the individual boxes (but not both)",
                                      textInput("code_entry", label = "Code:", value = ""),
                                      "or",
                                      build_code_entry_guide("insert", rdata$dvw$plays[ridx, ])
                                      ))
                focus_in_code_entry("code_entry")
            }
        }
        delete_data_row <- function() {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "delete"
                showModal(modalDialog(title = "Delete code", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = "Confirm delete code (or press Enter)")))
            }
        }

        insert_setting_data_row <- function() {
            ridx_set <- mutate(rdata$dvw$plays, rowN = row_number(),
                               add_set_before = case_when(.data$skill %eq% "Attack" & !(str_sub(.data$code, 7, 8) %in% no_set_attacks) & !(lag(.data$skill) %eq% "Set") ~ TRUE,
                                                          TRUE ~ FALSE))
            ridx_set <- ridx_set$rowN[which(ridx_set$add_set_before)]
            if (length(ridx_set) > 0) {
                editing$active <- "insert setting actions"
                showModal(modalDialog(title = "Insert setting codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = paste0("Confirm insert ", length(ridx_set), " setting codes (or press Enter)"))))
            }
        }

        delete_setting_data_row <- function() {
            ridx <- dplyr::filter(mutate(rdata$dvw$plays, rowN = row_number()), .data$skill %eq% "Set")$rowN
            if (length(ridx) > 0) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "delete all setting actions"
                showModal(modalDialog(title = "Delete all setting codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = paste0("Confirm delete all (", length(ridx), ") setting codes (or press Enter)"))))
            }
        }

        insert_dig_data_row <- function() {
            ridx_dig <- mutate(rdata$dvw$plays, rowN = row_number(),
                               attack_in_play = .data$skill %eq% "Attack" & grepl("^(Positive|Poor|Blocked for reattack|Spike in play)", .data$evaluation, ignore.case = TRUE),
                               add_dig_after = case_when(.data$attack_in_play & !lead(.data$skill) %in% c("Dig", "Block") ~ TRUE, ## attack in play, not followed by dig or block
                                                         lag(.data$attack_in_play) & .data$skill %eq% "Block" & !.data$evaluation %in% c("Error", "Invasion", "Winning block") & !lead(.data$skill) %eq% "Dig" ~ TRUE, ## block touch after attack in play, not followed by dig
                                                         TRUE ~ FALSE))
            ridx_dig <- ridx_dig$rowN[which(ridx_dig$add_dig_after)]
            if (length(ridx_dig) > 0) {
                editing$active <- "insert digging actions"
                showModal(modalDialog(title = "Insert digging codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = paste0("Confirm insert ", length(ridx_dig), " digging codes (or press Enter)"))))
            }
        }
        delete_dig_data_row <- function() {
            ridx <- dplyr::filter(mutate(rdata$dvw$plays, rowN = row_number()), .data$skill %eq% "Dig")$rowN
            if (length(ridx) > 0) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "delete all digging actions"
                showModal(modalDialog(title = "Delete all digging codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = paste0("Confirm delete all (", length(ridx), ") digging codes (or press Enter)"))))
            }
        }

        ## video functions
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
        build_code_entry_guide <- function(mode, thisrow) {
            mode <- match.arg(mode, c("edit", "insert"))
            bitstbl <- code_bits_tbl
            if (mode %eq% "edit" && is_skill(thisrow$skill)) {
                ## only with skill, not timeout/sub/etc
                thiscode <- thisrow$code
                bitstbl$value <- vapply(seq_len(nrow(bitstbl)), function(z) substr(thiscode, bitstbl$start[z], bitstbl$end[z]), FUN.VALUE = "", USE.NAMES = FALSE)
            } else {
                bitstbl$value <- ""
            }
            bitstbl$value <- gsub("~", "", bitstbl$value)
            cbitInput <- function (bitname, value = "", width = 2, helper = "") {
                tags$div(style = paste0("display:inline-block; vertical-align:top;"), tags$input(id = paste0("code_entry_", bitname), type = "text", value = value, size = width, maxlength = width, class = "input-small"),
                         ##HTML(paste0("<input id=\"code_entry_", bitname, "\" type=\"text\" value=\"", value, "\" size=\"", width, "\" maxlength=\"", width, "\" class=\"input-small\"", if (bitname == "end_zone") " autofocus=\"autofocus\"", " />")),
                         tags$div(class = "code_entry_guide", helper))
            }
            tags$div(style = "padding: 8px;", do.call(shiny::fixedRow, lapply(seq_len(nrow(bitstbl)), function(z) {
                this_skill <- bitstbl$value[bitstbl$bit %eq% "skill"]
                this_ev <- bitstbl$value[bitstbl$bit %eq% "eval"]
                cbitInput(bitstbl$bit[z], value = bitstbl$value[z], width = bitstbl$width[z], helper = if (is.function(bitstbl$helper[[z]])) uiOutput(paste0("code_entry_helper_", bitstbl$bit[z], "_ui")) else HTML(bitstbl$helper[[z]]))
            })))
        }
        ## the helpers that are defined as functions in code_bits_tbl are dynamic, they depend on skill/evaluation
        ## ADD HANDLERS HERE
        output$code_entry_helper_skill_type_ui <- renderUI({
            HTML(skill_type_helper(input$code_entry_skill, input$code_entry_eval))
        })
        output$code_entry_helper_num_players_ui <- renderUI({
            HTML(num_players_helper(input$code_entry_skill, input$code_entry_eval))
        })
        output$code_entry_helper_special_ui <- renderUI({
            HTML(special_helper(input$code_entry_skill, input$code_entry_eval))
        })
        output$code_entry_helper_end_zone_ui <- renderUI({
            HTML(end_zone_helper(input$code_entry_skill, input$code_entry_eval))
        })

        ## match data editing
        observeEvent(input$edit_match_data_button, {
            editing$active <- "match_data"
            match_time <- if (!is.na(rdata$dvw$meta$match$time)) {
                              as.POSIXct(rdata$dvw$meta$match$time, origin = "1970-01-01")
                          } else {
                              NULL
                          }
            showModal(modalDialog(title = "Edit match data", size = "l", footer = tags$div(actionButton("edit_commit", label = "Update match data (or press Enter)"), actionButton("edit_cancel", label = "Cancel (or press Esc)")),
                                  tags$div(
                                           fluidRow(column(4, shiny::dateInput("match_edit_date", label = "Match date:", value = rdata$dvw$meta$match$date)),
                                                    column(4, textInput("match_edit_time", label = "Start time:", value = match_time, placeholder = "HH:MM:SS")),
                                                    column(4, textInput("match_edit_season", label = "Season:", value = rdata$dvw$meta$match$season))),
                                           fluidRow(column(4, textInput("match_edit_league", label = "League:", value = rdata$dvw$meta$match$league)),
                                                    column(4, textInput("match_edit_phase", label = "Phase:", value = rdata$dvw$meta$match$phase)),
                                                    column(4, shiny::selectInput("match_edit_home_away", label = "Home/away:", choices = c("", "Home", "Away"), selected = rdata$dvw$meta$match$home_away))),
                                           fluidRow(column(4, textInput("match_edit_day_number", "Day number:", value = rdata$dvw$meta$match$day_number)),
                                                    column(4, textInput("match_edit_match_number", "Match number:", value = rdata$dvw$meta$match$match_number)),
                                                    ##column(2, shiny::selectInput("match_edit_regulation", "Regulation:", choices = c("indoor sideout", "indoor rally point", "beach rally point"), selected = rdata$dvw$meta$match$regulation)),
                                                    column(4, shiny::selectInput("match_edit_zones_or_cones", "Zones or cones:", choices = c("C", "Z"), selected = rdata$dvw$meta$match$zones_or_cones), tags$span(style = "font-size:small", "Note: changing cones/zones here will only change the indicator in the file header, it will not convert a file recorded with zones into one recorded with cones, or vice-versa. Don't change this unless you know what you are doing!")))
                                       )
                                  ))
        })

        ## team data editing
        observeEvent(input$edit_teams_button, {
            editing$active <- "teams"
            htidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
            vtidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
            showModal(modalDialog(title = "Edit teams", size = "l", footer = tags$div(actionButton("edit_commit", label = "Update teams data"), actionButton("edit_cancel", label = "Cancel")),
                                  tabsetPanel(
                                      tabPanel("Home team",
                                               fluidRow(column(4, textInput("ht_edit_name", label = "Team name:", value = rdata$dvw$meta$teams$team[htidx])),
                                                        column(4, textInput("ht_edit_id", label = "Team ID:", value = rdata$dvw$meta$teams$team_id[htidx])),
                                                        column(4, textInput("ht_edit_coach", label = "Coach:", value = rdata$dvw$meta$teams$coach[htidx])),
                                                        column(4, textInput("ht_edit_assistant", label = "Assistant:", value = rdata$dvw$meta$teams$assistant[htidx]))),
                                               DT::dataTableOutput("ht_edit_team"),
                                               wellPanel(
                                                   fluidRow(column(2, textInput("ht_new_id", label = "ID:", placeholder = "ID")),
                                                            column(1, textInput("ht_new_number", label = "Number:", placeholder = "Number")),
                                                            column(3, textInput("ht_new_lastname", label = "Last name:", placeholder = "Last name")),
                                                            column(3, textInput("ht_new_firstname", label = "First name:", placeholder = "First name")),
                                                            column(2, selectInput("ht_new_role", label = "Role", choices = c("", "libero", "outside", "opposite", "middle", "setter", "unknown"))),
                                                            column(1, selectInput("ht_new_special", label = "Special", choices = c("", "L", "C")))),
                                                   fluidRow(column(3, offset = 9, actionButton("ht_add_player_button", "Add player")))
                                               ),
                                               uiOutput("ht_delete_player_ui")
                                               ),
                                      tabPanel("Visiting team",
                                               fluidRow(column(4, textInput("vt_edit_name", label = "Team name:", value = rdata$dvw$meta$teams$team[vtidx])),
                                                        column(4, textInput("vt_edit_id", label = "Team ID:", value = rdata$dvw$meta$teams$team_id[vtidx])),
                                                        column(4, textInput("vt_edit_coach", label = "Coach:", value = rdata$dvw$meta$teams$coach[vtidx])),
                                                        column(4, textInput("vt_edit_assistant", label = "Assistant:", value = rdata$dvw$meta$teams$assistant[vtidx]))),
                                               DT::dataTableOutput("vt_edit_team"),
                                               wellPanel(
                                                   fluidRow(column(2, textInput("vt_new_id", label = "ID:", placeholder = "ID")),
                                                            column(1, textInput("vt_new_number", label = "Number:", placeholder = "Number")),
                                                            column(3, textInput("vt_new_lastname", label = "Last name:", placeholder = "Last name")),
                                                            column(3, textInput("vt_new_firstname", label = "First name:", placeholder = "First name")),
                                                            column(2, selectInput("vt_new_role", label = "Role", choices = c("", "libero", "outside", "opposite", "middle", "setter", "unknown"))),
                                                            column(1, selectInput("vt_new_special", label = "Special", choices = c("", "L", "C")))),
                                                   fluidRow(column(3, offset = 9, actionButton("vt_add_player_button", "Add player")))
                                               ),
                                               uiOutput("vt_delete_player_ui")
                                               )
                                  )
                                  ))
        })
        htdata_edit <- reactiveVal(NULL)
        output$ht_edit_team <- DT::renderDataTable({
            if (is.null(htdata_edit())) htdata_edit(rdata$dvw$meta$players_h)
            if (!is.null(htdata_edit())) {
                cols_to_hide <- which(!names(htdata_edit()) %in% c("player_id", "number", "lastname", "firstname", "role", "special_role"))-1L ## 0-based because no row names
                cnames <- names(names_first_to_capital(htdata_edit()))
                DT::datatable(htdata_edit(), rownames = FALSE, colnames = cnames, selection = "single", editable = TRUE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE, columnDefs = list(list(targets = cols_to_hide, visible = FALSE))))
            } else {
                NULL
            }
        }, server = TRUE)
        ht_edit_team_proxy <- DT::dataTableProxy("ht_edit_team")
        observeEvent(input$ht_edit_team_cell_edit, {
            info <- input$ht_edit_team_cell_edit
            isolate(temp <- htdata_edit())
            temp[info$row, info$col+1L] <- DT::coerceValue(info$value, temp[[info$row, info$col+1L]]) ## no row names so +1 on col indices
            DT::replaceData(ht_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
            htdata_edit(temp)
        })
        output$ht_delete_player_ui <- renderUI({
            if (!is.null(input$ht_edit_team_rows_selected)) {
                actionButton("ht_delete_player_button", "Delete selected player")
            } else {
                NULL
            }
        })
        observeEvent(input$ht_delete_player_button, {
            ridx <- input$ht_edit_team_rows_selected
            if (!is.null(ridx)) {
                temp <- htdata_edit()
                temp <- temp[-ridx, ]
                DT::replaceData(ht_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
                htdata_edit(temp)
            }
        })
        observeEvent(input$ht_add_player_button, {
            chk <- list(input$ht_new_id, input$ht_new_number, input$ht_new_lastname, input$ht_new_firstname)
            if (!any(vapply(chk, is_nnn, FUN.VALUE = TRUE))) {
                try({
                    newrow <- tibble(number = as.numeric(input$ht_new_number), player_id = input$ht_new_id, lastname = input$ht_new_lastname, firstname = input$ht_new_firstname, role = if (nzchar(input$ht_new_role)) input$ht_new_role else NA_character_, special_role = if (nzchar(input$ht_new_special)) input$ht_new_special else NA_character_)
                    newrow$name <- paste(newrow$firstname, newrow$lastname)
                    temp <- bind_rows(htdata_edit(), newrow)
                    temp <- dplyr::arrange(temp, .data$number)
                    DT::replaceData(ht_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
                    htdata_edit(temp)
                    ## clear inputs
                    updateTextInput(session, "ht_new_number", value = "")
                    updateTextInput(session, "ht_new_id", value = "")
                    updateTextInput(session, "ht_new_lastname", value = "")
                    updateTextInput(session, "ht_new_firstname", value = "")
                    updateSelectInput(session, "ht_new_role", selected = "")
                    updateSelectInput(session, "ht_new_special", selected = "")
                })
            }
        })
        vtdata_edit <- reactiveVal(NULL)
        output$vt_edit_team <- DT::renderDataTable({
            if (is.null(vtdata_edit())) vtdata_edit(rdata$dvw$meta$players_v)
            if (!is.null(vtdata_edit())) {
                cols_to_hide <- which(!names(vtdata_edit()) %in% c("player_id", "number", "lastname", "firstname", "role", "special_role"))-1L ## 0-based because no row names
                cnames <- names(names_first_to_capital(vtdata_edit()))
                DT::datatable(vtdata_edit(), rownames = FALSE, colnames = cnames, selection = "single", editable = TRUE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE, columnDefs = list(list(targets = cols_to_hide, visible = FALSE))))
            } else {
                NULL
            }
        }, server = TRUE)
        vt_edit_team_proxy <- DT::dataTableProxy("vt_edit_team")
        observeEvent(input$vt_edit_team_cell_edit, {
            info <- input$vt_edit_team_cell_edit
            isolate(temp <- vtdata_edit())
            temp[info$row, info$col+1L] <- DT::coerceValue(info$value, temp[[info$row, info$col+1L]]) ## no row names so +1 on col indices
            DT::replaceData(vt_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
            vtdata_edit(temp)
        })
        output$vt_delete_player_ui <- renderUI({
            if (!is.null(input$vt_edit_team_rows_selected)) {
                actionButton("vt_delete_player_button", "Delete selected player")
            } else {
                NULL
            }
        })
        observeEvent(input$vt_delete_player_button, {
            ridx <- input$vt_edit_team_rows_selected
            if (!is.null(ridx)) {
                temp <- vtdata_edit()
                temp <- temp[-ridx, ]
                DT::replaceData(vt_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
                vtdata_edit(temp)
            }
        })
        observeEvent(input$vt_add_player_button, {
            chk <- list(input$vt_new_id, input$vt_new_number, input$vt_new_lastname, input$vt_new_firstname)
            if (!any(vapply(chk, is_nnn, FUN.VALUE = TRUE))) {
                try({
                    newrow <- tibble(number = as.numeric(input$vt_new_number), player_id = input$vt_new_id, lastname = input$vt_new_lastname, firstname = input$vt_new_firstname, role = if (nzchar(input$vt_new_role)) input$vt_new_role else NA_character_, special_role = if (nzchar(input$vt_new_special)) input$vt_new_special else NA_character_)
                    newrow$name <- paste(newrow$firstname, newrow$lastname)
                    temp <- bind_rows(vtdata_edit(), newrow)
                    temp <- dplyr::arrange(temp, .data$number)
                    DT::replaceData(vt_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
                    vtdata_edit(temp)
                    ## clear inputs
                    updateTextInput(session, "vt_new_number", value = "")
                    updateTextInput(session, "vt_new_id", value = "")
                    updateTextInput(session, "vt_new_lastname", value = "")
                    updateTextInput(session, "vt_new_firstname", value = "")
                    updateSelectInput(session, "vt_new_role", selected = "")
                    updateSelectInput(session, "vt_new_special", selected = "")
                })
            }
        })
        ## court inset showing rotation and team lists
        disambig_names <- function(last, first) {
            firstinit <- substr(first, 1, 1)
            didx <- which(last %in% last[duplicated(last)])
            last[didx] <- paste(firstinit[didx], last[didx])
            last
        }
        names2roster <- function(pm) {
            pm <- dplyr::arrange(pm, .data$number)
            pm$lastname <- disambig_names(pm$lastname, pm$firstname)
            lc <- paste(ifelse(grepl("L", pm$special_role), "L", ""), ifelse(grepl("C", pm$special_role), "C", ""), sep = ",")
            lc <- sub("^,", "", sub(",$", "", lc))
            lc[nzchar(lc)] <- paste0(" (", lc[nzchar(lc)], ")")
            pm$lastname <- paste0(pm$lastname, lc)
            str_trim(paste0(pm$number, " ", pm$lastname))
        }
        output$htrot_ui <- renderUI({
            re <- names2roster(rdata$dvw$meta$players_h)
            do.call(tags$div, c(list(tags$strong("Home team"), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
        })
        output$vtrot_ui <- renderUI({
            re <- names2roster(rdata$dvw$meta$players_v)
            do.call(tags$div, c(list(tags$strong("Home team"), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
        })
        court_inset_home_team_end <- reactiveVal("lower")
        output$court_inset <- renderPlot({
            p <- ggplot(data = data.frame(x = c(-0.25, 4.25, 4.25, -0.25), y = c(-0.25, -0.25, 7.25, 7.25)), mapping = aes_string("x", "y")) +
                geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = c(0.5, 0.5, 3.5, 3.5)), fill = styling$h_court_colour) +
                geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = 3 + c(0.5, 0.5, 3.5, 3.5)), fill = styling$v_court_colour) +
                ggcourt(labels = NULL, show_zones = FALSE, show_zone_lines = TRUE, court_colour = "indoor")
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                this_pn <- rdata$dvw$plays$player_number[ridx] ## player in the selected row
                htrot <- tibble(player_id = as.character(rdata$dvw$plays[ridx, paste0("home_player_id", 1:6)]), team_id = rdata$dvw$plays$home_team_id[ridx])
                htrot <- dplyr::left_join(htrot, rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], by = "player_id")
                vtrot <- tibble(player_id = as.character(rdata$dvw$plays[ridx, paste0("visiting_player_id", 1:6)]), team_id = rdata$dvw$plays$visiting_team_id[ridx])
                vtrot <- dplyr::left_join(vtrot, rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], by = "player_id")
                plxy <- cbind(dv_xy(1:6, end = "lower"), htrot)
                plxy$court_num <- unlist(rdata$dvw$plays[ridx, paste0("home_p", 1:6)]) ## the on-court player numbers in the play-by-play data
                ## player names and circles
                ## home team
                p <- p + geom_polygon(data = court_circle(cz = 1:6, end = "lower"), aes_string(group = "id"), fill = styling$h_court_colour, colour = styling$h_court_highlight)
                ## highlighted player
                if (rdata$dvw$plays$team[ridx] %eq% rdata$dvw$plays$home_team[ridx] && sum(this_pn %eq% plxy$court_num) == 1) {
                    p <- p + geom_polygon(data = court_circle(cz = which(this_pn %eq% plxy$court_num), end = "lower"), fill = "yellow", colour = "black")
                }
                p <- p + geom_text(data = plxy, aes_string("x", "y", label = "court_num"), size = 6, fontface = "bold", vjust = 0) +
                    geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
                ## visiting team
                plxy <- cbind(dv_xy(1:6, end = "upper"), vtrot)
                plxy$court_num <- unlist(rdata$dvw$plays[ridx, paste0("visiting_p", 1:6)]) ## the on-court player numbers in the play-by-play data
                p <- p + geom_polygon(data = court_circle(cz = 1:6, end = "upper"), aes_string(group = "id"), fill = styling$v_court_colour, colour = styling$v_court_highlight)
                if (rdata$dvw$plays$team[ridx] %eq% rdata$dvw$plays$visiting_team[ridx] && sum(this_pn %eq% plxy$court_num) == 1) {
                    p <- p + geom_polygon(data = court_circle(cz = which(this_pn %eq% plxy$court_num), end = "upper"), fill = "yellow", colour = "black")
                }
                p <- p + geom_text(data = plxy, aes_string("x", "y", label = "court_num"), size = 6, fontface = "bold", vjust = 0) +
                    geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
            }
            if (court_inset_home_team_end() != "lower") p <- p + scale_x_reverse() + scale_y_reverse()
            p
        })
        observeEvent(input$court_inset_swap, {
            court_inset_home_team_end(other_end(court_inset_home_team_end()))
            dojs("document.getElementById('court_inset_swap').blur();") ## un-focus from button
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
    msgs <- dplyr::filter(x$messages, !is.na(.data$file_line_number))
    msgs <- dplyr::summarize(group_by_at(msgs, "file_line_number"), error_message = paste0(.data$message, collapse = "<br />"))
    x$plays <- left_join(x$plays, msgs, by = "file_line_number")
    x$plays$error_icon <- ifelse(is.na(x$plays$error_message), "", HTML(as.character(shiny::icon("exclamation-triangle"))))
    x
}
