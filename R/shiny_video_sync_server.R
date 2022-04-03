## not exported
ov_shiny_video_sync_server <- function(app_data) {
    function(input, output, session) {
        auto_playlist_updates <- TRUE ## temporary while testing - if TRUE allow the playslist table to update automatically (i.e. normal reactive behaviour, which gives slightly simpler code). If FALSE then control these updates manually, which might help avoid unnecessary redraws
        reactive_scrolling <- FALSE ## testing, not sure it helps. In principle if multiple scroll requests get lined up before the first has actually been initiated, then it'll skip to just the last
        styling <- list(h_court_colour = "#bfefff", ## lightblue1
                        h_court_highlight = "darkblue",
                        v_court_colour = "#bcee68", ## darkolivegreen2
                        v_court_highlight = "darkgreen")

        rdata <- reactiveValues(dvw = app_data$dvw)
        tag_data <- reactiveValues(events = tibble(tag_video_time = numeric(), tag = character()))
        editing <- reactiveValues(active = NULL)
        video_state <- reactiveValues(paused = FALSE)
        dv_read_args <- app_data$dv_read_args
        done_first_playlist_render <- FALSE
        running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
        debug <- 0L
        plays_cols_to_show <- c("error_icon", "clock_time", "video_time", "set_number", "code", "home_setter_position", "visiting_setter_position", "phase_type", "Score", "is_skill")
        plays_col_renames <- c(Set = "set_number", hs = "home_setter_position", as = "visiting_setter_position")
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
                                        "num_players", 1,
                                        "special", 1,
                                        "custom", 5)

        lineup_bits_tbl <- dplyr::tribble(~bit, ~width,
                                        "P1", 2,
                                        "P2", 2,
                                        "P3", 2,
                                        "P4", 2,
                                        "P5", 2,
                                        "P6", 2)

        sub_bits_tbl <- dplyr::tribble(~bit, ~width,
                                       "OUT", 2,
                                       "IN", 2)

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
        code_bits_tbl$start <- cumsum(lag(code_bits_tbl$width, default = 0))+1L
        code_bits_tbl$end <- code_bits_tbl$start+code_bits_tbl$width-1L

        output$vtdp_ui <- renderUI({
            if (input$video_time_decimal_places > 0) {
                tags$div(class = "alert alert-danger", "Note: files with non-integer video times may not be openable in DataVolley")
            } else {
                NULL
            }
        })

        ## court inset showing rotation and team lists
        court_inset <- callModule(mod_courtrot, id = "courtrot", rdata = rdata, rowidx = reactive(playslist_current_row()), styling = styling)
        rotateTeams <- reactive(court_inset$rt)
        accept_ball_coords <- court_inset$accept_ball_coords ## the "accept" button

        observe({
            if (nrow(court_inset$click_points$queue) > 1 && !is.null(playslist_current_row()) && !is.na(playslist_current_row())) {
                js_show2("courtrot-validate_ball_coords")
                js_show2("courtrot-cancel_ball_coords")
            } else {
                js_hide2("courtrot-validate_ball_coords")
                js_hide2("courtrot-cancel_ball_coords")
            }
        })

        observe({
            rtn <- rotateTeams()
            if (rtn$home > 0) {
                home_force_rotate()
                rtn$home <- 0L
            }
            if (rtn$visiting > 0) {
                visiting_force_rotate()
                rtn$visiting <- 0L
            }
        })

        observeEvent(accept_ball_coords(), {
            if (accept_ball_coords() > 0) { ## ignore the initial triggering of this on app startup
                ridx <- playslist_current_row()
                do_reparse = FALSE
                if (!is.null(ridx) && !is.na(ridx)) {
                    if (nrow(court_inset$click_points$queue) >= 2) {
                        ## if we have at least two points, then we can add coordinates
                        do_reparse <- TRUE
                        ## start coordinate
                        thisxy <- court_inset$click_points$queue[1, ]
                        rdata$dvw$plays$start_coordinate_x[ridx] <- thisxy$x
                        rdata$dvw$plays$start_coordinate_y[ridx] <- thisxy$y
                        if (is.na(thisxy$x)) {
                            rdata$dvw$plays$start_coordinate[ridx] <- NA
                        } else {
                            rdata$dvw$plays$start_coordinate[ridx] <- datavolley::dv_xy2index(thisxy)
                        }
                        ## mid coordinate, which will be missing if we only clicked two points
                        thisxy <- if (nrow(court_inset$click_points$queue) > 2) court_inset$click_points$queue[2, ] else data.frame(x = NA_real_, y = NA_real_)
                        rdata$dvw$plays$mid_coordinate_x[ridx] <- thisxy$x
                        rdata$dvw$plays$mid_coordinate_y[ridx] <- thisxy$y
                        if (is.na(thisxy$x)) {
                            rdata$dvw$plays$mid_coordinate[ridx] <- NA
                        } else {
                            rdata$dvw$plays$mid_coordinate[ridx] <- datavolley::dv_xy2index(thisxy)
                        }
                        ## end coordinate
                        thisxy <- if (nrow(court_inset$click_points$queue) > 2) court_inset$click_points$queue[3, ] else court_inset$click_points$queue[2, ]
                        rdata$dvw$plays$end_coordinate_x[ridx] <- thisxy$x
                        rdata$dvw$plays$end_coordinate_y[ridx] <- thisxy$y
                        if (is.na(thisxy$x)) {
                            rdata$dvw$plays$end_coordinate[ridx] <- NA
                        } else {
                            rdata$dvw$plays$end_coordinate[ridx] <- datavolley::dv_xy2index(thisxy)
                        }
                    }
                }
                if (do_reparse) {
                    playslist_needs_scroll(TRUE)
                    if (!auto_playlist_updates) replace_playlist_data()
                }
                ## and clear the clicked coordinates queue
                court_inset$clear_click_queue()
                editing$active <- NULL
            }
        })

        observeEvent(input$show_shortcuts, {
            showModal(modalDialog(title = "Keyboard shortcuts", easyClose = TRUE, size = "l",
                                  if (app_data$with_video) tagList(tags$p(tags$strong("Video controls")), tags$ul(tags$li("[l or 6] forward 2s, [; or ^] forward 10s, [m or 3] forwards 0.1s, [, or 9] forwards 1 frame"), tags$li("[j or 4] backward 2s, [h or $] backward 10s, [n or 1] backwards 0.1s, [b or 7] backwards 1 frame"), tags$li("[q or 0] pause video"), tags$li("[g or #] go to currently-selected event"))),
                                  fluidRow(column(6, tags$strong("Keyboard controls"),
                                           tags$ul(tags$li("[r or 5] sync selected event video time"),
                                                   tags$li("[i or 8] move to previous skill row"),
                                                   tags$li("[k or 2] move to next skill row"),
                                                   tags$li("[e or E] edit current code"),
                                                   tags$li("[del] delete current code"),
                                                   tags$li("[ins] insert new code above current"),
                                                   tags$li("[F1] home team rotate +1"),
                                                   tags$li("[F2] insert setting codes before every attack"),
                                                   tags$li("[F4] delete all setting codes (except errors)"),
                                                   tags$li("[F6] insert digging codes after every attack"),
                                                   tags$li("[F8] delete all digging codes"),
                                                   tags$li("[F10] visiting team rotate +1"),
                                                   )),
                                           column(6, if (app_data$with_video) tagList(tags$strong("Tagging"), tags$ul(tags$li("[left-click the court inset then press 't'] add a tag with the clicked court location. Alternatively, the location can be entered by left-clicking the video, if the court reference data has been provided"),
                                                                                     tags$li("[T] open the tag manager (download or clear tag data)"))),
                                                  tags$strong("Ball coordinates"), tags$ul(tags$li("[left-click the court inset] register the start/mid/end ball positions"),
                                                                                           tags$li("[accept ball coordinates] to add coordinates to the currently selected item"))))
                                  ))
        })

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
                    ## Only update video time of events happening after current event
                    cidx <- which(clock_time_diff[midx] >= 0)
                    rdata$dvw$plays$video_time[midx][cidx] <- round(new_video_time[cidx], digits = input$video_time_decimal_places)
                    playslist_needs_scroll(TRUE)
                    if (!auto_playlist_updates) replace_playlist_data()
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
                    new_clock_time <- this_time + video_time_diff[midx]
                    # Only update clock time of events happening after current event
                    cidx <- which(video_time_diff[midx] >= 0)
                    rdata$dvw$plays$time[midx][cidx] <- new_clock_time[cidx]
                    playslist_needs_scroll(TRUE)
                    if (!auto_playlist_updates) replace_playlist_data()
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
            ridx <- playslist_current_row()
            if (!is.null(ridx) && !is.na(ridx)) {
                ##cat("x time: "); cat(str(rdata$dvw$plays$time))
                tm <- input$selected_clocktime
                ##cat("time:"); cat(str(tm))
                ##cat("Original time:"); cat(str(rdata$dvw$plays$time[ridx]))
                tm = as.POSIXct(paste(format(rdata$dvw$plays$time[ridx], "%Y-%m-%d"), format(tm, "%H:%M:%S")))
                if (inherits(rdata$dvw$plays$time[ridx], "POSIXct")) tm <- as.POSIXct(tm, tz = lubridate::tz(rdata$dvw$plays$time[ridx]))
                ##cat("time cast:"); cat(str(tm))
                rdata$dvw$plays$time[ridx] <- tm
                ##cat("rdata time:"); cat(str(rdata$dvw$plays$time[ridx]))
                playslist_needs_scroll(TRUE)
                if (!auto_playlist_updates) replace_playlist_data()
            }
        })

        ## sync the selected event to the current video time
        sync_single_video_time <- function() {
            ridx <- playslist_current_row()
            if (!is.null(ridx)) {
                do_video("set_current_video_time", ridx)
            }
        }

        observeEvent(input$set_current_video_time, {
            temp <- strsplit(input$set_current_video_time, split = "&", fixed = TRUE)[[1]]
            ridx <- as.integer(temp[2])
            tm <- as.numeric(temp[1])
            if (!is.null(ridx) && !is.na(ridx) && ridx > 0 && ridx <= nrow(rdata$dvw$plays)) {
                tm <- if (input$video_time_decimal_places < 1) round(tm) else round(tm, digits = input$video_time_decimal_places)
                rdata$dvw$plays$video_time[ridx] <- tm
                rdata$dvw <- preprocess_dvw(rdata$dvw)
                skip <- 1
                if (ridx < nrow(rdata$dvw$plays) && ((rdata$dvw$plays$skill[ridx] %eq% "Attack" && rdata$dvw$plays$skill[ridx+1] %eq% "Block") || (rdata$dvw$plays$skill[ridx] %eq% "Serve" && rdata$dvw$plays$skill[ridx+1] %eq% "Reception"))) {
                    ## give the block the same time as the attack / reception the same time as the serve
                    rdata$dvw$plays$video_time[ridx+1] <- tm
                    skip <- 2
                } else if (rdata$dvw$plays$skill[ridx] %eq% "Attack" && ridx < nrow(rdata$dvw$plays) && rdata$dvw$plays$skill[ridx+1] %eq% "Dig") {
                    ## give the dig a +1s time
                    rdata$dvw$plays$video_time[ridx+1] <- tm+1
                    skip <- 2
                }
                ## advance to the next skill row
                if (ridx < nrow(rdata$dvw$plays)) {
                    next_skill_row <- find_next_skill_row(ridx, step = skip)
                    if (length(next_skill_row) > 0) {
                        ## update the current row
                        playslist_current_row(next_skill_row)
                    }
                }
                ## update the table data, and it will automatically trigger the scroll to the current row once it has finished drawing
                playslist_needs_scroll(TRUE)
                replace_playlist_data() ## always do this here, otherwise if a row is synced with the same existing time that it already has, it won't scroll (?)
            }
        })

        selected_event <- reactive({
            if (!is.null(playslist_current_row())) {
                rdata$dvw$plays[playslist_current_row(), ]
            } else {
                NULL
            }
        })

        output$current_event <- renderUI({
            tags$div(id = "currentevent", style = if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) paste0("margin-top: ", input$vo_offset - 50, "px") else "", tags$strong("Current: "), selected_event()$code)
        })

        observe({
            ## parse on reload (without "error_message" in column names)
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
                mydat$phase_type <- case_when(mydat$phase %eq% "Serve" ~ "S",
                                              mydat$phase %eq% "Reception" ~ "R",
                                              mydat$phase %eq% "Transition" ~ "T")
                mydat$Score <- paste(mydat$home_team_score, mydat$visiting_team_score, sep = "-")
                cols_to_hide <- which(plays_cols_to_show %in% c("is_skill"))-1 ## 0-based because no row names
                cnames <- names(plays_do_rename(mydat[1, plays_cols_to_show, drop = FALSE]))
                cnames[plays_cols_to_show == "error_icon"] <- ""
                out <- DT::datatable(mydat[, plays_cols_to_show, drop = FALSE], rownames = FALSE, colnames = cnames,
                                     extensions = "Scroller",
                                     escape = FALSE, filter = "top",
                                     selection = sel, options = list(scroller = TRUE,
                                                                     lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = TRUE, "scrollY" = paste0(plh, "px"), ordering = FALSE, ##autoWidth = TRUE,
                                                                     columnDefs = list(list(targets = cols_to_hide, visible = FALSE)),
                                                                     drawCallback = DT::JS("function(settings) { Shiny.setInputValue('playlist_redrawn', new Date().getTime()); }")
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
        playslist_needs_scroll <- reactiveVal(FALSE)
        playslist_scroll_target <- reactiveVal(-99L)
        observeEvent(input$playlist_redrawn, {
            ## when the table has finished being drawn, scroll it if necessary
            if (playslist_needs_scroll()) {
                playslist_needs_scroll(FALSE)
                if (reactive_scrolling) playslist_scroll_target(playslist_current_row()) else scroll_playlist(playslist_current_row())
            }
            ## and mark current row as selected in the table, but don't re-scroll to it
            playslist_select_row(playslist_current_row(), scroll = FALSE)
        })

        ## keep track of selected playslist row as a reactiveVal
        ##   when updating e.g. video time, set this reactiveVal, then wait for DT to redraw THEN scroll
        playslist_current_row <- reactiveVal(NULL)
        ## the playslist_select_row function just changes the visible selection in the table, and optionally scrolls to it, but does not change playslist_current_row() value
        playslist_select_row <- function(rw, scroll = TRUE) {
            DT::selectRows(playslist_proxy, rw)
            if (isTRUE(scroll)) {
                if (reactive_scrolling) playslist_scroll_target(rw) else scroll_playlist(rw)
            }
        }
        ## when the user changes the selected row, update playslist_current_row
        observeEvent(input$playslist_rows_selected, playslist_current_row(input$playslist_rows_selected))

        observe({
            if (reactive_scrolling && !is.null(playslist_scroll_target()) && !is.na(playslist_scroll_target()) && playslist_scroll_target() > 0) {
                scroll_playlist(playslist_scroll_target())
            }
        })

        scroll_playlist <- function(rw) {
            if (!is.null(rw)) {
                ## scrolling works on the VISIBLE row index, so it depends on any column filters that might have been applied
                visible_rowidx <- which(input$playslist_rows_all == rw)
                scrollto <- max(visible_rowidx-1-5, 0) ## -1 for zero indexing, -5 to keep the selected row 5 from the top
                ##dojs(paste0("$('#playslist').find('.dataTable').DataTable().scroller.toPosition(", scrollto, ");")) ## with anim, laggy
                dojs(paste0("$('#playslist').find('.dataTable').DataTable().scroller.toPosition(", scrollto, ", false);")) ## no anim, faster

                ## using the jquery scrollTo extension, enable in UI
                ##stid <- paste0("$('#DataTables_Table_1 > tbody tr:nth-child(", scrollto+1, ")')")
                ##dojs(paste0("$('.dataTables_scrollBody').scrollTo(", stid, ");"))

                ## not tried yet https://github.com/rstudio/DT/issues/519
                ## table.DataTable().row([row_id]).scrollTo(); ## (without scroller?)

                ## other attempts
                ##dojs(sprintf("$('#playslist').find('.dataTable').DataTable().row(%s).node().scrollIntoView();", max(0, rdata$plays_row_to_select-1)))
                ##dojs(sprintf("console.dir($('#playslist').find('.dataTable').DataTable().row(%s).node())", max(0, rdata$plays_row_to_select-1)))
                ##dojs(sprintf("$('#playslist').find('.dataTables_scroll').animate({ scrollTop: $('#playslist').find('.dataTable').DataTable().row(%s).node().offsetTop }, 2000);", max(0, rdata$plays_row_to_select-1)))
            }
        }

        ## if auto_playlist_updates is TRUE
        observe({
            blah <- rdata$dvw
            if (auto_playlist_updates) replace_playlist_data()
        })
        ## replace_playlist_data is used if auto_playlist_updates is FALSE
        replace_playlist_data <- function() {
            mydat <- rdata$dvw$plays
            mydat$is_skill <- is_skill(mydat$skill)
            mydat$set_number <- as.factor(mydat$set_number)
            mydat$phase_type <- case_when(mydat$phase %eq% "Serve" ~ "S",
                                          mydat$phase %eq% "Reception" ~ "R",
                                          mydat$phase %eq% "Transition" ~ "T")
            mydat$Score <- paste(mydat$home_team_score, mydat$visiting_team_score, sep = "-")
            DT::replaceData(playslist_proxy, data = mydat[, plays_cols_to_show, drop = FALSE], rownames = FALSE, clearSelection = "none")
        }

        find_next_skill_row <- function(current_row_idx = NULL, step = 1, respect_filters = TRUE) {
            ## if respect_filters is TRUE, find the next row that is shown in the table (i.e. passing through any column filters that have been applied)
            ## if FALSE, just find the next skill row in the data, ignoring table filters
            if (is.null(current_row_idx)) current_row_idx <- playslist_current_row()
            skill_rows <- which(is_skill(rdata$dvw$plays$skill))
            if (respect_filters) skill_rows <- intersect(skill_rows, input$playslist_rows_all)
            next_skill_row <- skill_rows[skill_rows > current_row_idx]
            next_skill_row[min(step, length(next_skill_row))]
        }

        find_prev_skill_row <- function(current_row_idx = NULL, step = 1, respect_filters = TRUE) {
            ## if respect_filters is TRUE, find the previous row that is shown in the table (i.e. passing through any column filters that have been applied)
            ## if FALSE, just find the previous skill row in the data, ignoring table filters
            if (is.null(current_row_idx)) current_row_idx <- playslist_current_row()
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
                            ## if editing/tagging/inserting, treat as update
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
                        } else if(mycmd %eq% "83") {
                            insert_sub()
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
                        } else if (mycmd %in% utf8ToInt("qQ0")) { ## video navigation
                            do_video("toggle_pause")
                        } else if (mycmd %in% utf8ToInt("gG#")) {
                            ## video go to currently-selected event
                            ev <- selected_event()
                            if (!is.null(ev)) do_video("set_time", ev$video_time)
                        } else if (mycmd %in% utf8ToInt("nm13jhl;46$^b,79")) {
                            ## video forward/backward nav
                            vidcmd <- if (tolower(mykey) %in% c("1", "n", "h", "j", "4", "$", "b", "7")) "rew" else "ff"
                            dur <- if (tolower(mykey) %in% c("h", "$", ";", "^")) 10 else if (tolower(mykey) %in% c("n", "m", "1", "3")) 0.1 else if (tolower(mykey) %in% c("b", "7", ",", "9")) 1/30 else 2
                            do_video(vidcmd, dur)
                        } else if (mykey %in% c("r", "R", "5")) {
                            ## set the video time of the current event
                            if (app_data$with_video) sync_single_video_time()
                        } else if (mykey %eq% "t") {
                            ## tag event at current time
                            if (app_data$with_video) add_tagged_event()
                        } else if (mykey %eq% "T") {
                            ## pop up the tag manager dialog
                            tag_manager()
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
                        } else if (ky %eq% "83" && is.null(editing$active)) {
                            insert_sub()
                        } else if (ky %eq% "113") {
                            ## insert new setting actions
                            insert_setting_data_row()
                        # }  else if(ky %eq% "112") {
                        #     home_force_rotate()
                        # } else if(ky %eq% "121") {
                        #     visiting_force_rotate()
                        } else if (ky %eq% "115") {
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
            ridx <- playslist_current_row()
            if (!is.null(ridx)) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "edit"
                showModal(modalDialog(title = "Edit code", size = "l", 
                                      footer = tags$div(actionButton("edit_commit", label = "Update code (or press Enter)"), actionButton("edit_cancel", label = "Cancel (or press Esc)")),
                                      withTags({
                                          fluidRow(
                                              column(12, "Edit code either in the top text box or in the individual boxes (but not both)",
                                                     textInput("code_entry", label = "Code:", value = thiscode),
                                                     "or",
                                                     build_code_entry_guide("edit", rdata$dvw$plays[ridx, ])))
                                      })
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
        focus_in_sub_entry <- function(id, highlight_all = TRUE) {
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
            if (!is.null(editing$active) && editing$active %in% "teams") {
                htdata_edit(NULL)
                vtdata_edit(NULL)
            }
            editing$active <- NULL
            removeModal()
        })
        observeEvent(input$edit_commit, {
            if (!is.null(editing$active)) code_make_change()
        })
        code_make_change <- function() {
            removeModal()
            do_reparse <- FALSE
            if (is.null(editing$active)) {
                ## not triggered from current editing activity, huh?
                warning("code_make_change entered but editing not active")
            } else if (editing$active %eq% "tagging") {
                ## add tag
                txt <- if (is.null(input$tag_text)) "" else input$tag_text
                do_video("tag_current_video_time", if (nzchar(txt)) base64enc::base64encode(charToRaw(txt)) else "")
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
                ## currently disabled rdata$dvw$meta$match$regulation <- input$match_edit_regulation
                rdata$dvw$meta$match$zones_or_cones <- input$match_edit_zones_or_cones
                do_reparse <- TRUE
            } else if (editing$active %eq% "change starting lineup") {
                if(input$ht_set_number != "" && input$ht_P1  != ""  && input$ht_P2 != "" &&
                   input$ht_P3 != "" &&  input$ht_P4 != "" && input$ht_P5 != "" &&
                   input$ht_P6 != "" && input$ht_libero != "" && input$ht_setter != ""){
                    team = datavolley::home_team(rdata$dvw)
                    setnumber = input$ht_set_number
                    new_setter = input$ht_setter
                    new_rotation = c(input$ht_P1,input$ht_P2,input$ht_P3,input$ht_P4,input$ht_P5,input$ht_P6)
                    # Change meta data in terms of starting rotation
                    rdata$dvw$meta$players_h[,paste0("starting_position_set", setnumber)] <- as.character(match(rdata$dvw$meta$players_h$number, new_rotation))
                    ## Change libero to "*" in meta
                    ## BR not sure if this is needed, it was commented out
                    ##rdata$dvw$meta$players_h[rdata$dvw$meta$players_h$number %eq% input$ht_libero,paste0("starting_position_set", setnumber)] <- "*"
                    # Change in play rotation 
                    rdata$dvw <- dv_change_startinglineup(rdata$dvw, team, setnumber, new_rotation, new_setter)
                }
                if(input$vt_set_number != "" && input$vt_P1  != ""  && input$vt_P2 != "" &&
                   input$vt_P3 != "" &&  input$vt_P4 != "" && input$vt_P5 != "" && 
                   input$vt_P6 != "" && input$vt_libero != "" && input$vt_setter != ""){
                    team = datavolley::visiting_team(rdata$dvw)
                    setnumber = input$vt_set_number
                    new_setter = input$vt_setter
                    new_rotation = c(input$vt_P1,input$vt_P2,input$vt_P3,input$vt_P4,input$vt_P5,input$vt_P6)
                    # Change meta data in terms of starting rotation
                    rdata$dvw$meta$players_v[,paste0("starting_position_set", setnumber)] <- as.character(match(rdata$dvw$meta$players_v$number, new_rotation))
                    ## Change libero to "*" in meta
                    ## BR not sure if this is needed, it was commented out
                    ##rdata$dvw$meta$players_v[rdata$dvw$meta$players_v$number %eq% input$vt_libero,paste0("starting_position_set", setnumber)] <- "*"
                    # Change in play rotation 
                    rdata$dvw <- dv_change_startinglineup(rdata$dvw, team, setnumber, new_rotation, new_setter)
                }
                do_reparse <- TRUE
            } else if (editing$active %eq% "delete all setting actions") {
                ridx <- dplyr::filter(mutate(rdata$dvw$plays, rowN = row_number()), .data$skill %eq% "Set" & !.data$evaluation %eq% "Error")$rowN
                if (length(ridx) > 0) {
                    if (is.logical(ridx)) ridx <- which(ridx)
                    rdata$dvw$plays <- rdata$dvw$plays[-ridx, ]
                    do_reparse <- TRUE
                }
            } else if (editing$active %eq% "insert setting actions") {
                ridx_set <- dv_insert_sets_check(rdata$dvw, no_set_attacks = no_set_attacks)
                if (length(ridx_set) > 0) {
                    rdata$dvw <- dv_insert_sets(rdata$dvw, no_set_attacks = no_set_attacks, default_set_evaluation = default_set_evaluation, ridx = ridx_set)
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
                ridx_dig <- dv_insert_digs_check(rdata$dvw)
                if (length(ridx_dig) > 0) {
                    rdata$dvw <- dv_insert_digs(rdata$dvw, ridx = ridx_dig)
                    do_reparse <- TRUE
                }
            } else {
                ridx <- playslist_current_row()
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
                            ## if we entered via the text box, then run this through the code parser
                            newcode <- sub("~+$", "", ov_code_interpret(newcode))
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
                        if (length(newcode) == 1) {
                            ## update the code in the current row
                            rdata$dvw$plays$code[ridx] <- newcode
                        } else if (length(newcode) == 2) {
                            ## hmm, have we entered a compound code?
                            ## can't handle that yet, is it even sensible to support?
                            warning("compound codes can't be used when editing an existing code")
                        }
                    } else if (editing$active %eq% "insert" && !is.null(newcode)) {
                        ## insert new line
                        if (is.logical(ridx)) ridx <- which(ridx)
                        newline <- rdata$dvw$plays[rep(ridx, length(newcode)), ]
                        newline$code <- newcode
                        rdata$dvw$plays <- bind_rows(if (ridx > 1) rdata$dvw$plays[seq(1, ridx-1L, by = 1), ], newline, rdata$dvw$plays[seq(ridx, nrow(rdata$dvw$plays), by = 1), ])
                        ## set the newly-inserted line as the active row
                        ##nsr <- find_next_skill_row()
                        ##if (length(nsr) > 0) playslist_select_row(nsr)
                    } else if (editing$active %eq% "delete") {
                        if (is.logical(ridx)) ridx <- which(ridx)
                        rdata$dvw$plays <- rdata$dvw$plays[-ridx, ]
                    } else if (editing$active %eq% "substitution"){
                        if(input$ht_inplayer != "" && input$ht_outplayer != ""){
                            teamSelect = datavolley::home_team(rdata$dvw)
                            rdata$dvw <- dv_create_substitution(rdata$dvw, ridx, team = teamSelect, in_player = input$ht_inplayer, out_player = input$ht_outplayer, 
                                                                new_setter = input$ht_new_setter)
                        }
                        if(input$vt_inplayer != "" && input$vt_outplayer != ""){
                            teamSelect = datavolley::visiting_team(rdata$dvw)
                            rdata$dvw <- dv_create_substitution(rdata$dvw, ridx, team = teamSelect, in_player = input$vt_inplayer, out_player = input$vt_outplayer,
                                                                new_setter = input$vt_new_setter)
                        }
                    } else if (editing$active %eq% "home_force_rotation"){
                        if (is.logical(ridx)) ridx <- which(ridx)
                        rdata$dvw <- dv_force_rotation(rdata$dvw, team = datavolley::home_team(rdata$dvw), ridx, direction = 1)
                    } else if (editing$active %eq% "visiting_force_rotation"){
                        if (is.logical(ridx)) ridx <- which(ridx)
                        rdata$dvw <- dv_force_rotation(rdata$dvw, team = datavolley::visiting_team(rdata$dvw), ridx, direction = 1)
                    }
                    do_reparse <- TRUE
                }
            }
            if (do_reparse) {
                ## reparse the dvw
                rdata$dvw <- reparse_dvw(rdata$dvw, dv_read_args = dv_read_args)
                playslist_needs_scroll(TRUE)
                if (!auto_playlist_updates) replace_playlist_data()
            }
            editing$active <- NULL
        }
        insert_data_row <- function() {
            ridx <- playslist_current_row()
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
            ridx <- playslist_current_row()
            if (!is.null(ridx)) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "delete"
                showModal(modalDialog(title = "Delete code", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = "Confirm delete code (or press Enter)")))
            }
        }

        home_force_rotate <- function() {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                editing$active <- "home_force_rotation"
                code_make_change()
            }
        }
        visiting_force_rotate <- function() {
            ridx <- input$playslist_rows_selected
            if (!is.null(ridx)) {
                editing$active <- "visiting_force_rotation"
                code_make_change()
            }
        }

        ## Create a substitution
        insert_sub_old <- function() {
            ridx <- playslist_current_row()
            if (!is.null(ridx)) {
                if (ridx > 1) ridx <- ridx-1L ## we are inserting above the selected row, so use the previous row to populate this one
                editing$active <- "substitution"
                showModal(modalDialog(title = "Substitution", size = "l", footer = tags$div(actionButton("edit_commit", label = "Validate substitution"), actionButton("edit_cancel", label = "Cancel")),
                                      tabsetPanel(
                                          tabPanel("Home team",
                                                   tags$style("#ht_display_team {border: 2px solid #bfefff;}"),
                                                   DT::dataTableOutput("ht_display_team"),
                                                   wellPanel(
                                                       fluidRow(
                                                           column(1,
                                                                  tags$style("#ht_outplayer {border: 2px solid #dd4b39;}"),
                                                                  textInput("ht_outplayer", label = "OUT", placeholder = "OUT")),
                                                           column(1,
                                                                  tags$style("#ht_inplayer {border: 2px solid #168a52;}"),
                                                                  textInput("ht_inplayer", label = "IN", placeholder = "IN")),
                                                           column(2,
                                                                  tags$style("#ht_new_setter {border: 2px solid #f5ed0c;}"),
                                                                  textInput("ht_new_setter", label = "New Setter", placeholder = "NS"))
                                                           ),
                                                       style = "background: #bfefff"
                                                   )
                                          ),
                                          tabPanel("Visiting team",
                                                   tags$style("#vt_display_team {border: 2px solid #bcee68;}"),
                                                   DT::dataTableOutput("vt_display_team"),
                                                   wellPanel(
                                                       fluidRow(column(1, 
                                                                       tags$style("#vt_outplayer {border: 2px solid #dd4b39;}"),
                                                                       textInput("vt_outplayer", label = "OUT", placeholder = "OUT")),
                                                                column(1, 
                                                                       tags$style("#vt_inplayer {border: 2px solid #168a52;}"),
                                                                       textInput("vt_inplayer", label = "IN", placeholder = "IN")),
                                                                column(2,
                                                                       tags$style("#vt_new_setter {border: 2px solid #f5ed0c;}"),
                                                                       textInput("vt_new_setter", label = "New Setter", placeholder = "NS"))
                                                                ),
                                                       style = "background: #bcee68"
                                                   )
                                          )
                                      )
                ))
            }
        }
        
        insert_sub <- function() {
            ridx <- playslist_current_row()
            if (!is.null(ridx)) {
                if (ridx > 1) ridx <- ridx-1L ## we are inserting above the selected row, so use the previous row to populate this one
                editing$active <- "substitution"
                showModal(modalDialog(title = "Substitution", size = "l", footer = tags$div(actionButton("edit_commit", label = "Validate substitution"), actionButton("edit_cancel", label = "Cancel")),
                                      wellPanel(
                                          fluidRow(class = "ht_roster_court_vt_roster",
                                              column(3,id = "hroster", uiOutput("htroster")),
                                              column(6,plotOutput("court_inset", height = "200px")),
                                              column(3, id = "vroster", uiOutput("vtroster"))),
                                          fluidRow(class = "ht_sub_vt_sub",
                                              column(1,id = "hroster",
                                                     tags$style("#ht_outplayer {border: 2px solid #dd4b39;}"),
                                                     textInput("ht_outplayer", label = "OUT", placeholder = "OUT")),
                                              column(1,id = "hroster",
                                                     tags$style("#ht_inplayer {border: 2px solid #168a52;}"),
                                                     textInput("ht_inplayer", label = "IN", placeholder = "IN")),
                                              column(2,id = "hroster",
                                                     tags$style("#ht_new_setter {border: 2px solid #f5ed0c;}"),
                                                     textInput("ht_new_setter", label = "New Setter", placeholder = "NS")),
                                              column(4),
                                              column(2,id = "vroster",
                                                     tags$style("#vt_new_setter {border: 2px solid #f5ed0c;}"),
                                                     textInput("vt_new_setter", label = "New Setter", placeholder = "NS")),
                                              column(1, id = "vroster",
                                                     tags$style("#vt_inplayer {border: 2px solid #168a52;}"),
                                                     textInput("vt_inplayer", label = "IN", placeholder = "IN")),
                                              column(1, id = "vroster",
                                                     tags$style("#vt_outplayer {border: 2px solid #dd4b39;}"),
                                                     textInput("vt_outplayer", label = "OUT", placeholder = "OUT")))
                                      )
                )
                )
            }
        }
        
        output$htroster <- renderUI({
            re <- names2roster(rdata$dvw$meta$players_h)
            do.call(tags$div, c(list(tags$strong("Home team"), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
        })
        output$vtroster <- renderUI({
            re <- names2roster(rdata$dvw$meta$players_v)
            do.call(tags$div, c(list(tags$strong("Visiting team"), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
        })
        output$court_inset <- renderPlot({
            p <- ggplot(data = data.frame(x = c(-0.25, 4.25, 4.25, -0.25), y = c(-0.25, -0.25, 7.25, 7.25)), mapping = aes_string("x", "y")) +
                geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = c(0.5, 0.5, 3.5, 3.5)), fill = styling$h_court_colour) +
                geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = 3 + c(0.5, 0.5, 3.5, 3.5)), fill = styling$v_court_colour) +
                ggcourt(labels = NULL, show_zones = FALSE, show_zone_lines = TRUE, court_colour = "indoor")
            ridx <- playslist_current_row()
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
                    geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)  + coord_flip()
            }
            p
        })
        # Insert setting
        
        insert_setting_data_row <- function() {
            ridx_set <- dv_insert_sets_check(rdata$dvw, no_set_attacks = no_set_attacks)
            if (length(ridx_set) > 0) {
                editing$active <- "insert setting actions"
                showModal(modalDialog(title = "Insert setting codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = paste0("Confirm insert ", length(ridx_set), " setting codes (or press Enter)"))))
            } else {
                showModal(modalDialog(title = "Insert setting codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      "No setting codes to insert."))
            }
        }

        delete_setting_data_row <- function() {
            ridx <- dplyr::filter(mutate(rdata$dvw$plays, rowN = row_number()), .data$skill %eq% "Set" & !.data$evaluation %eq% "Error")$rowN
            if (length(ridx) > 0) {
                thiscode <- rdata$dvw$plays$code[ridx]
                editing$active <- "delete all setting actions"
                showModal(modalDialog(title = "Delete all non-error setting codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = paste0("Confirm delete (", length(ridx), ") setting codes (or press Enter)"))))
            }
        }
        insert_dig_data_row <- function() {
            ridx_dig <- dv_insert_digs_check(rdata$dvw)
            if (length(ridx_dig) > 0) {
                editing$active <- "insert digging actions"
                showModal(modalDialog(title = "Insert dig codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      actionButton("edit_commit", label = paste0("Confirm insert ", length(ridx_dig), " dig codes (or press Enter)"))))
            } else {
                showModal(modalDialog(title = "Insert dig codes", size = "l", footer = actionButton("edit_cancel", label = "Cancel (or press Esc)"),
                                      "No dig codes to insert."))
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

        ## tagging
        add_tagged_event <- function() {
            editing$active <- "tagging"
            showModal(modalDialog(title = "Add tag at current video time", size = "l", footer = actionButton("tagging_cancel", label = "Cancel (or press Esc)"),
                                  tags$div(textInput("tag_text", "Tag text:"), actionButton("do_add_tag", "Add tag (or press Enter)"))
            ))
            ## focus
            dojs("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById(\"tag_text\"); el.selectionStart = el.selectionEnd = el.value.length; el.focus(); });")
        }
        observeEvent(input$tagging_cancel, {
            editing$active <- NULL
            removeModal()
        })
        observeEvent(input$do_add_tag, {
            code_make_change()
        })
        observeEvent(input$tag_current_video_time, {
            temp <- strsplit(input$tag_current_video_time, split = "&", fixed = TRUE)[[1]]
            tagtxt <- if (length(temp) >= 2) rawToChar(base64enc::base64decode(temp[2])) else ""
            tm <- as.numeric(temp[1])
            extra <- selected_event()
            ## add match_id regardless of whether there is a selected event
            this_match_id <- rdata$dvw$meta$match_id
            if (!is.null(extra)) extra <- extra[, setdiff(names(extra), "match_id")]
            thisxy <- data.frame(x = NA_real_, y = NA_real_)
            if (nrow(court_inset$click_points$queue) > 0) {
                thisxy <- tail(court_inset$click_points$queue, 1)
                thisxy <- cbind(thisxy, crt_to_vid(thisxy))
            }
            tag_data$events <- bind_rows(tag_data$events, bind_cols(tibble(match_id = this_match_id, tag_video_time = tm, tag = tagtxt), thisxy, extra))
            ## clear the ball coords data
            court_inset$clear_click_queue()
        })
        remember_include_all_pbp <- reactiveVal(FALSE)
        tag_manager <- function() {
            editing$active <- "tagging"
            showModal(modalDialog(
                title = "Tag manager",
                size = "l", footer = actionButton("tagging_cancel", label = "Cancel (or press Esc)"),
                ##    ##tags$div(textInput("tag_text", "Tag text:"), actionButton("do_add_tag", "Add tag (or press Enter)"))
                fluidRow(column(4, downloadButton("download_tags"), checkboxInput("tags_include_all_pbp", "Include all play-by-play data columns?", value = remember_include_all_pbp())), column(4, actionButton("clear_tags", "Clear tag data")))
            ))
#### focus
            ##dojs("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById(\"tag_text\"); el.selectionStart = el.selectionEnd = el.value.length; el.focus(); });")
        }
        observeEvent(input$tags_include_all_pbp, remember_include_all_pbp(input$tags_include_all_pbp))
        observeEvent(input$clear_tags, {
            tag_data$events <- tibble(tag_video_time = numeric(), tag = character())
            editing$active <- NULL
            removeModal()
        })
        output$download_tags <- downloadHandler(
            filename = function() "tags.csv",
            content = function(file) {
                editing$active <- NULL
                this <- tag_data$events
                this <- this[, setdiff(names(this), c("error_message", "error_icon"))] ## don't export these
                if (!isTRUE(remember_include_all_pbp())) {
                    ## include just some key columns
                    ## note that if we tagged without a selected row, there are no additional data for that row
                    this <- this[, intersect(names(this), c("match_id", "set_number", "file_line_number", "video_time", "tag", "tag_video_time", "image_x", "image_y", "x", "y"))]
                }
                write.csv(this, file, row.names = FALSE, na = "")
                removeModal()
            }
        )

        ## video functions
        do_video <- function(what, ..., id = "main_video") {
            if (!app_data$with_video) return(NULL)
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
                dojs(paste0("Shiny.onInputChange('set_current_video_time', ", getel, ".currentTime + '&", myargs[1], "&' + new Date().getTime())"))
            } else if (what == "tag_current_video_time") {
                dojs(paste0("Shiny.onInputChange('tag_current_video_time', ", getel, ".currentTime + '&", myargs[1], "')"))
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
        htdata_display <- reactiveVal(NULL)
        output$ht_display_team <- DT::renderDataTable({
            if (is.null(htdata_display())) htdata_display(rdata$dvw$meta$players_h)
            if (!is.null(htdata_display())) {
                cols_to_hide <- which(!names(htdata_display()) %in% c("player_id", "number", "lastname", "firstname", "role", "special_role"))-1L ## 0-based because no row names
                cnames <- names(names_first_to_capital(htdata_display()))
                DT::datatable(htdata_display(), rownames = FALSE, colnames = cnames, selection = "single", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE, columnDefs = list(list(targets = cols_to_hide, visible = FALSE))))
            } else {
                NULL
            }
        }, server = TRUE)
        ht_display_team_proxy <- DT::dataTableProxy("ht_display_team")
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
        vtdata_display <- reactiveVal(NULL)
        output$vt_display_team <- DT::renderDataTable({
            if (is.null(vtdata_display())) vtdata_display(rdata$dvw$meta$players_v)
            if (!is.null(vtdata_display())) {
                cols_to_hide <- which(!names(vtdata_display()) %in% c("player_id", "number", "lastname", "firstname", "role", "special_role"))-1L ## 0-based because no row names
                cnames <- names(names_first_to_capital(vtdata_display()))
                DT::datatable(vtdata_display(), rownames = FALSE, colnames = cnames, selection = "single", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE, columnDefs = list(list(targets = cols_to_hide, visible = FALSE))))
            } else {
                NULL
            }
        }, server = TRUE)
        vt_display_team_proxy <- DT::dataTableProxy("vt_display_team")
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
        
        
        ## starting line up editing
        observeEvent(input$edit_lineup_button, {
            editing$active <- "change starting lineup"
            htidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
            vtidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
            showModal(modalDialog(title = "Edit starting line up", size = "l", footer = tags$div(actionButton("edit_commit", label = "Update teams lineups"), actionButton("edit_cancel", label = "Cancel")),
                                  tabsetPanel(
                                      tabPanel("Home team",
                                               tags$style("#ht_display_team {border: 2px solid #bfefff;}"),
                                               DT::dataTableOutput("ht_display_team"),
                                               wellPanel(
                                                   fluidRow(
                                                       column(1, textInput("ht_set_number", label = "Set", placeholder = "Set number")),
                                                       column(1, textInput("ht_P1", label = "P1", placeholder = "P1")),
                                                       column(1, textInput("ht_P2", label = "P2", placeholder = "P2")),
                                                       column(1, textInput("ht_P3", label = "P3", placeholder = "P3")),
                                                       column(1, textInput("ht_P4", label = "P4", placeholder = "P4")),
                                                       column(1, textInput("ht_P5", label = "P5", placeholder = "P5")),
                                                       column(1, textInput("ht_P6", label = "P6", placeholder = "P6"))),
                                                   fluidRow(
                                                       column(1, textInput("ht_setter", label = "Setter", placeholder = "Setter")),
                                                       column(1, textInput("ht_libero", label = "Libero", placeholder = "Libero"))
                                                       ),
                                                   style = "background: #bfefff"
                                               ),
                                               uiOutput("ht_delete_player_ui")
                                      ),
                                      tabPanel("Visiting team",
                                               tags$style("#vt_display_team {border: 2px solid #bcee68;}"),
                                               DT::dataTableOutput("vt_display_team"),
                                               wellPanel(
                                                   fluidRow(
                                                       column(1, textInput("vt_set_number", label = "Set", placeholder = "Set number")),
                                                       column(1, textInput("vt_P1", label = "P1", placeholder = "P1")),
                                                       column(1, textInput("vt_P2", label = "P2", placeholder = "P2")),
                                                       column(1, textInput("vt_P3", label = "P3", placeholder = "P3")),
                                                       column(1, textInput("vt_P4", label = "P4", placeholder = "P4")),
                                                       column(1, textInput("vt_P5", label = "P5", placeholder = "P5")),
                                                       column(1, textInput("vt_P6", label = "P6", placeholder = "P6"))),
                                                   fluidRow(
                                                       column(1, textInput("vt_setter", label = "Setter", placeholder = "Setter")),
                                                       column(1, textInput("vt_libero", label = "Libero", placeholder = "Libero"))
                                                   ),
                                                   style = "background: #bcee68"
                                               ),
                                               uiOutput("vt_delete_player_ui")
                                      )
                                  )
            ))
        })

        ## General help
        observeEvent(input$general_help, introjs(session, options = list("nextLabel"="Next", "prevLabel"="Previous", "skipLabel"="Skip")))

        ## height of the video player element
        vo_height <- reactiveVal("auto")
        observe({
            if (app_data$with_video) {
                if (!is.null(input$dv_height) && as.numeric(input$dv_height) > 0) {
                    this <- as.numeric(input$dv_height)
                    vo_height(this)
                    dojs(paste0("document.getElementById('video_overlay').style.height = '", this, "px';"))
                    dojs(paste0("document.getElementById('video_overlay_img').style.height = '", this, "px';"))
                } else {
                    vo_height("auto")
                    dojs(paste0("document.getElementById('video_overlay').style.height = '400px';"))
                    dojs(paste0("document.getElementById('video_overlay_img').style.height = '400px';"))
                }
            }
        })
        ## width of the video player element
        vo_width <- reactiveVal("auto")
        observe({
            if (app_data$with_video) {
                if (!is.null(input$dv_width) && as.numeric(input$dv_width) > 0) {
                    this <- as.numeric(input$dv_width)
                    vo_width(this)
                    dojs(paste0("document.getElementById('video_overlay').style.width = '", this, "px';"))
                    dojs(paste0("document.getElementById('video_overlay_img').style.width = '", this, "px';"))
                } else {
                    vo_width("auto")
                    dojs(paste0("document.getElementById('video_overlay').style.width = '600px';"))
                    dojs(paste0("document.getElementById('video_overlay_img').style.width = '600px';"))
                }
            }
        })
        ## height of the video player container, use as negative vertical offset on the overlay element
        observe({
            if (app_data$with_video) {
                if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) {
                    dojs(paste0("document.getElementById('currentevent').style.marginTop = '-", input$vo_voffset - 50, "px';"))
                    dojs(paste0("document.getElementById('video_overlay').style.marginTop = '-", input$vo_voffset, "px';"))
                    dojs(paste0("document.getElementById('video_overlay_img').style.marginTop = '-", input$vo_voffset, "px';"))
                } else {
                    dojs("document.getElementById('currentevent').style.marginTop = '-50px';")
                    dojs("document.getElementById('video_overlay').style.marginTop = '0px';")
                    dojs("document.getElementById('video_overlay_img').style.marginTop = '0px';")
                }
            }
        })
        ## video overlay
        output$show_overlay_ui <- renderUI(if (!is.null(app_data$court_ref)) checkboxInput("show_overlay", "Show court overlay?", value = FALSE) else NULL)

        gg_tight <- list(theme(legend.position = "none", panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0, "null"), plot.margin = rep(unit(0, "null"), 4), axis.ticks = element_blank(), axis.ticks.length = unit(0, "null"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()), scale_x_continuous(limits = c(0, 1), expand = c(0, 0)), scale_y_continuous(limits = c(0, 1), expand = c(0, 0)))
        overlay_images <- reactiveVal(list(zones = NULL, cones_L = NULL, cones_M = NULL, cones_R = NULL, image_dir = NULL))
        ## generate static overlay images
        observe({
            blah <- list(input$dv_height, input$dv_width) ## reactive to these ## previously also included rdata$dvw
            isolate(img_dir <- overlay_images()$image_dir)
            if (is.null(img_dir)) {
                img_dir <- tempfile()
                dir.create(img_dir)
                shiny::addResourcePath(prefix = "courtimg", img_dir)
            } else {
            }
            if (isTRUE(input$show_overlay) && !is.null(app_data$court_ref) && !is.null(input$dv_width) && as.numeric(input$dv_width) > 0 && !is.null(input$dv_height) && as.numeric(input$dv_height) > 0) {
                overlay_images(list(zones = gen_overlay_img(polytype = "zones", width = input$dv_width, height = input$dv_height, outdir = img_dir),
                                    cones_L = gen_overlay_img(polytype = "cones", sz = "L", width = input$dv_width, height = input$dv_height, outdir = img_dir),
                                    cones_M = gen_overlay_img(polytype = "cones", sz = "M", width = input$dv_width, height = input$dv_height, outdir = img_dir),
                                    cones_R = gen_overlay_img(polytype = "cones", sz = "R", width = input$dv_width, height = input$dv_height, outdir = img_dir),
                                    image_dir = img_dir))
            } else {
                overlay_images(list(zones = NULL, cones_L = NULL, cones_M = NULL, cones_R = NULL, image_dir = img_dir))
            }
        })
        gen_overlay_img <- function(polytype, sz, width, height, outdir) {
            ## outer lines
            cxy <- data.frame(x = c(rep(0.5, 3), 0.5, 3.5), xend = c(rep(3.5, 3), 0.5, 3.5),
                              y = c(0.5, 3.5, 6.5, 0.5, 0.5),
                              yend = c(0.5, 3.5, 6.5, 6.5, 6.5),
                              width = 1.0)
            ## serve zones
            szlen <- 0.25 ## length of serve zone lines - make this at least 1 once clipping is implemented
            sxy <- data.frame(x = c(0.5, 1.1, 1.7, 2.3, 2.9, 3.5),
                              xend = c(0.5, 1.1, 1.7, 2.3, 2.9, 3.5),
                              y = rep(0.5, 6), yend = rep(0.5-szlen, 6), width = 0.75)
            cxy <- bind_rows(cxy, sxy)
            sxy[, c("x", "y")] <- dv_flip_xy(sxy[, c("x", "y")])
            sxy[, c("xend", "yend")] <- dv_flip_xy(sxy[, c("xend", "yend")])
            cxy <- bind_rows(cxy, sxy)
            if (polytype %eq% "cones") {
                sznum <- if (sz == "L") 4L else if (sz == "M") 3L else if (sz == "R") 2L else NA_integer_
                polyxy <- dv_cone_polygons(zone = sz, end = "upper")
                Nc <- max(polyxy$cone_number)
                polyxy$cone_number <- paste0(polyxy$cone_number, "U")
                polyxy <- bind_rows(polyxy, mutate(dv_cone_polygons(zone = sz, end = "lower"), cone_number = paste0(.data$cone_number, "L")))
                ## labels
                labxy <- mutate(dv_cone2xy(sznum, end_cones = seq_len(Nc), end = "upper", xynames = c("x", "y")), label = row_number())
                labxy <- bind_rows(labxy, mutate(dv_cone2xy(sznum, end_cones = seq_len(Nc), end = "lower", xynames = c("x", "y")), label = row_number()))
            } else {
                ## 3m and other zone lines
                cxy <- bind_rows(cxy, data.frame(x = c(0.5, 0.5, 0.5, 0.5, 1.5, 2.5), xend = c(3.5, 3.5, 3.5, 3.5, 1.5, 2.5),
                                                 y = c(2.5, 4.5, 1.5, 5.5, 0.5, 0.5), yend = c(2.5, 4.5, 1.5, 5.5, 6.5, 6.5),
                                                 width = 0.75))
                polyxy <- NULL
                labxy <- data.frame(x = rep(c(1, 2, 3), 6), y = as.numeric(matrix(1:6, nrow = 3, ncol = 6, byrow = TRUE)),
                                    label = c(5, 6, 1, 7, 8, 9, 4, 3, 2, 2, 3, 4, 9, 8, 7, 1, 6, 5))
            }
            cxy[, c("x", "y")] <- ovideo::ov_transform_points(cxy[, c("x", "y")], ref = app_data$court_ref, direction = "to_image")
            cxy[, c("xend", "yend")] <- setNames(ovideo::ov_transform_points(cxy[, c("xend", "yend")], ref = app_data$court_ref, direction = "to_image"), c("xend", "yend"))
            labxy[, c("x", "y")] <- ovideo::ov_transform_points(labxy[, c("x", "y")], ref = app_data$court_ref, direction = "to_image")
            if (!is.null(polyxy)) {
                polyxy[, c("x", "y")] <- ovideo::ov_transform_points(polyxy[, c("x", "y")], ref = app_data$court_ref, direction = "to_image")
            }
            p <- ggplot(cxy, aes_string("x", "y", xend = "xend", yend = "yend", size = "width")) + geom_segment(color = "blue") + gg_tight + scale_size_continuous(range = c(0.5, 1.0))
            if (!is.null(polyxy)) p <- p + geom_polygon(data = polyxy, aes_string(x = "x", y = "y", group = "cone_number"), inherit.aes = FALSE, color = "blue", fill = NA)
            p + geom_label(data = labxy, aes_string(x = "x", y = "y", label = "label"), inherit.aes = FALSE, color = "blue", hjust = 0.5, vjust = 0.5)#, size = 1.5)
            fname <- tempfile(tmpdir = outdir, fileext = ".png")
            ggplot2::ggsave(fname, p, device = "png", width = width/100, height = height/100, dpi = 100, units = "in", bg = "transparent")
            ##message(fname)
            basename(fname)
        }

        observe({
            if (!app_data$with_video || !isTRUE(input$show_overlay)) {
                dojs(paste0("document.getElementById('video_overlay_img').setAttribute('src', '');"))
            } else {
                ridx <- playslist_current_row()
                polytype <- "zones"
                if (!is.null(ridx) && !is.na(ridx)) {
                    try({
                        if (rdata$dvw$plays$skill[ridx] %eq% "Attack" && rdata$dvw$meta$match$zones_or_cones %eq% "C" && rdata$dvw$plays$start_zone[ridx] %in% 1:9) polytype <- "cones"
                    })
                }
                if (polytype %eq% "cones") {
                    sz <- if (rdata$dvw$plays$start_zone[ridx] %in% c(4, 7, 5)) "L" else if (rdata$dvw$plays$start_zone[ridx] %in% c(3, 8, 6)) "M" else "R"
                    polytype <- paste0("cones_", sz)
                }
                if (!is.null(overlay_images()[[polytype]])) {
                    dojs(paste0("document.getElementById('video_overlay_img').setAttribute('src', '/courtimg/", overlay_images()[[polytype]], "');"))
                } else {
                    dojs(paste0("document.getElementById('video_overlay_img').setAttribute('src', '');"))
                }
            }
        })

        ## other overlay plotting can be done here?
        observe({
            output$video_overlay <- renderPlot({
                if (!app_data$with_video) return(NULL)
                ## test - red diagonal line across the overlay plot
                ##ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes_string("x", "y")) + geom_path(color = "red") + gg_tight
                ## for tagging, need to plot SOMETHING else we don't get correct coordinates back
                this <- selected_event()
                ok <- FALSE
                try({
                    if (court_inset$ball_coords() && !is.null(this) && nrow(this) == 1 && !is.null(app_data$court_ref) && !is.na(this$start_coordinate_x) && !is.na(this$end_coordinate_x)) {
                        thisxy <- data.frame(x = as.numeric(this[, c("start_coordinate_x", "mid_coordinate_x", "end_coordinate_x")]),
                                             y = as.numeric(this[, c("start_coordinate_y", "mid_coordinate_y", "end_coordinate_y")]))
                        thisxy <- setNames(ovideo::ov_transform_points(thisxy, ref = app_data$court_ref, direction = "to_image"), c("image_x", "image_y"))
                        p <- ggplot(mapping = aes_string("image_x", "image_y")) + geom_point(data = thisxy[1, ], shape = 16, col = "green", size = 5) +
                            geom_point(data = thisxy[3, ], shape = 16, col = "red", size = 5) +
                            geom_path(data = na.omit(thisxy), arrow = arrow(length = unit(0.01, "npc"), ends = "last"))
                        ok <- TRUE
                    }
                }, silent = TRUE)
                if (!ok) {
                    p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes_string("x", "y"))
                }
                p + gg_tight
                ##NULL
            }, bg = "transparent", width = vo_width(), height = vo_height())
        })

        vid_to_crt <- function(obj) {
            courtxy <- data.frame(x = NA_real_, y = NA_real_)
            if (!is.null(app_data$court_ref)) {
                vxy <- c(obj$x, obj$y)
                if (length(vxy) == 2 && !any(is.na(vxy))) {
                    courtxy <- ovideo::ov_transform_points(vxy[1], vxy[2], ref = app_data$court_ref, direction = "to_court")
                }
            }
            courtxy
        }
        crt_to_vid <- function(obj) {
            imagexy <- data.frame(image_x = NA_real_, image_y = NA_real_)
            if (!is.null(app_data$court_ref)) {
                vxy <- c(obj$x, obj$y)
                if (length(vxy) == 2 && !any(is.na(vxy))) {
                    imagexy <- setNames(ovideo::ov_transform_points(vxy[1], vxy[2], ref = app_data$court_ref, direction = "to_image"), c("image_x", "image_y"))
                }
            }
            imagexy
        }

        ## single click the video to register a tag location, or starting ball coordinates
        observeEvent(input$video_click, {
            if (app_data$with_video) {
                courtxy <- vid_to_crt(input$video_click)
                court_inset$add_to_click_queue(courtxy)
            }
        })
    }
}
