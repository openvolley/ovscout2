ov_scouter_server <- function(app_data) {
    function(input, output, session) {
        debug <- 1L

        styling <- list(h_court_colour = "#bfefff", ## lightblue1
                        h_court_highlight = "darkblue",
                        v_court_colour = "#bcee68", ## darkolivegreen2
                        v_court_highlight = "darkgreen",
                        continue = "#10C424", cancel = "#D41024")

        plays_cols_to_show <- c("error_icon", "video_time", "set_number", "code", "home_setter_position", "visiting_setter_position", "Score", "is_skill")
        plays_cols_renames <- c(Set = "set_number", hs = "home_setter_position", as = "visiting_setter_position")

        ## this is temporary stupidity
        app_data$dvw$plays <- plays2_to_plays(app_data$dvw$plays2, dvw = app_data$dvw, evaluation_decoder = app_data$evaluation_decoder)

        if (is.null(app_data$dvw$meta$match$regulation)) stop("dvw does not have regulation information")
        app_data$is_beach <- is_beach(app_data$dvw)

        atbl <- app_data$dvw$meta$attacks
        atbl <- bind_cols(atbl[, setdiff(names(atbl), c("start_x", "start_y"))], setNames(dv_index2xy(atbl$start_coordinate), c("start_x", "start_y")))
        app_data$dvw$meta$attacks <- atbl

        rdata <- reactiveValues(dvw = app_data$dvw)

        pseq <- if (app_data$is_beach) 1:2 else 1:6

        ## court inset showing rotation and team lists
        court_inset <- callModule(mod_courtrot2, id = "courtrot", rdata = rdata, game_state = reactive(game_state), rally_codes = rally_codes, styling = styling, with_ball_coords = FALSE)
        rotateTeams <- reactive(court_inset$rt)
        accept_ball_coords <- court_inset$accept_ball_coords ## the "accept" button
        teamslists <- callModule(mod_teamslists, id = "teamslists", rdata = rdata)

        observe({
            if (nrow(court_inset$click_points$queue) > 1) {## && !is.null(playslist_current_row()) && !is.na(playslist_current_row())) {
                js_show2("courtrot-validate_ball_coords")
                js_show2("courtrot-cancel_ball_coords")
            } else {
                js_hide2("courtrot-validate_ball_coords")
                js_hide2("courtrot-cancel_ball_coords")
            }
        })

        observeEvent(accept_ball_coords(), {
            if (accept_ball_coords() > 0) { ## ignore the initial triggering of this on app startup
                warning("ball coords not implemented here yet")
            }
            ## and clear the clicked coordinates queue
            court_inset$clear_click_queue()
        })

        output$switch_serving_ui <- renderUI({
            if (rally_state() %in% c("click or unpause the video to start", "click serve start")) {
                actionButton("switch_serving", "Switch serving team")
            } else {
                ## can't switch serving team once the rally has started
                NULL
            }
        })
        observeEvent(input$switch_serving, {
            game_state$serving <- other(game_state$serving)
            game_state$current_team <- game_state$serving
        })

        observeEvent(input$edit_lineup_button, {
            editing$active <- "change starting lineup"
            htidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
            vtidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
            ht_setter <- get_setter(game_state, team = "*")
            if (is.null(ht_setter) || is.na(ht_setter) || ht_setter %eq% 0L) ht_setter <- ""
            vt_setter <- get_setter(game_state, team = "a")
            if (is.null(vt_setter) || is.na(vt_setter) || vt_setter %eq% 0L) vt_setter <- ""
            showModal(
                vwModalDialog(
                    title = "Edit starting line up", size = "l", footer = tags$div(uiOutput("edit_teams_commit_ui", inline = TRUE), actionButton("edit_cancel", label = "Cancel", style = paste0("background-color:", styling$cancel))),
                    tabsetPanel(
                        tabPanel("Home team",
                                 tags$style("#ht_display_team {border: 2px solid #bfefff;}"),
                                 DT::dataTableOutput("ht_display_team"),
                                 wellPanel(
                                     fluidRow(
                                         ##column(1, textInput("ht_set_number", label = "Set", placeholder = "Set number")),
                                         column(1, textInput("ht_P1", label = "P1", value = if (!is.null(game_state$home_p1) && !is.na(game_state$home_p1)) game_state$home_p1 else "", placeholder = "P1")),
                                         column(1, textInput("ht_P2", label = "P2", value = if (!is.null(game_state$home_p2) && !is.na(game_state$home_p2)) game_state$home_p2 else "", placeholder = "P2")),
                                         column(1, textInput("ht_P3", label = "P3", value = if (!is.null(game_state$home_p3) && !is.na(game_state$home_p3)) game_state$home_p3 else "", placeholder = "P3")),
                                         column(1, textInput("ht_P4", label = "P4", value = if (!is.null(game_state$home_p4) && !is.na(game_state$home_p4)) game_state$home_p4 else "", placeholder = "P4")),
                                         column(1, textInput("ht_P5", label = "P5", value = if (!is.null(game_state$home_p5) && !is.na(game_state$home_p5)) game_state$home_p5 else "", placeholder = "P5")),
                                         column(1, textInput("ht_P6", label = "P6", value = if (!is.null(game_state$home_p6) && !is.na(game_state$home_p6)) game_state$home_p6 else "", placeholder = "P6"))),
                                     fluidRow(
                                         column(1, textInput("ht_setter", label = "Setter", value = ht_setter, placeholder = "Setter")),
                                         column(1, textInput("ht_libero", label = "Libero", placeholder = "Libero"))
                                     ),
                                     style = "background: #bfefff"
                                 )),
                        tabPanel("Visiting team",
                                 tags$style("#vt_display_team {border: 2px solid #bcee68;}"),
                                 DT::dataTableOutput("vt_display_team"),
                                 wellPanel(
                                     fluidRow(
                                         ##column(1, textInput("vt_set_number", label = "Set", placeholder = "Set number")),
                                         column(1, textInput("vt_P1", label = "P1", value = if (!is.null(game_state$visiting_p1) && !is.na(game_state$visiting_p1)) game_state$visiting_p1 else "", placeholder = "P1")),
                                         column(1, textInput("vt_P2", label = "P2", value = if (!is.null(game_state$visiting_p2) && !is.na(game_state$visiting_p2)) game_state$visiting_p2 else "", placeholder = "P2")),
                                         column(1, textInput("vt_P3", label = "P3", value = if (!is.null(game_state$visiting_p3) && !is.na(game_state$visiting_p3)) game_state$visiting_p3 else "", placeholder = "P3")),
                                         column(1, textInput("vt_P4", label = "P4", value = if (!is.null(game_state$visiting_p4) && !is.na(game_state$visiting_p4)) game_state$visiting_p4 else "", placeholder = "P4")),
                                         column(1, textInput("vt_P5", label = "P5", value = if (!is.null(game_state$visiting_p5) && !is.na(game_state$visiting_p5)) game_state$visiting_p5 else "", placeholder = "P5")),
                                         column(1, textInput("vt_P6", label = "P6", value = if (!is.null(game_state$visiting_p6) && !is.na(game_state$visiting_p6)) game_state$visiting_p6 else "", placeholder = "P6"))),
                                     fluidRow(
                                         column(1, textInput("vt_setter", label = "Setter", value = vt_setter, placeholder = "Setter")),
                                         column(1, textInput("vt_libero", label = "Libero", placeholder = "Libero"))
                                     ),
                                     style = "background: #bcee68"
                                 ))
                    )
                ))
        })
        output$edit_teams_commit_ui <- renderUI({
            htok <- nzchar(input$ht_P1) && nzchar(input$ht_P2)
            if (!app_data$is_beach) htok <- htok && nzchar(input$ht_P3) && nzchar(input$ht_P4) && nzchar(input$ht_P5) && nzchar(input$ht_P6) && nzchar(input$ht_setter)
            vtok <- nzchar(input$vt_P1) && nzchar(input$vt_P2)
            if (!app_data$is_beach) vtok <- vtok && nzchar(input$vt_P3) && nzchar(input$vt_P4) && nzchar(input$vt_P5) && nzchar(input$vt_P6) && nzchar(input$vt_setter)
            if (htok && vtok) actionButton("edit_commit", label = "Update teams lineups", style = paste0("background-color:", styling$continue)) else NULL
        })

        playslist_mod <- callModule(mod_playslist, id = "playslist", rdata = rdata, plays_cols_to_show = plays_cols_to_show, plays_cols_renames = plays_cols_renames)

        video_state <- reactiveValues(paused = TRUE) ## starts paused
        editing <- reactiveValues(active = NULL)

        ## height of the video player element
        vo_height <- reactiveVal("auto")
        observe({
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
        })
        vo_width <- reactiveVal("auto")
        observe({
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
        })
        ## height of the video player container, use as negative vertical offset on the overlay element
        observe({
            if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) {
                ##dojs(paste0("document.getElementById('rallystate').style.marginTop = '-", input$vo_voffset - 50, "px';"))
                dojs(paste0("document.getElementById('video_overlay').style.marginTop = '-", input$vo_voffset, "px';"))
                dojs(paste0("document.getElementById('video_overlay_img').style.marginTop = '-", input$vo_voffset, "px';"))
            } else {
                ##dojs("document.getElementById('rallystate').style.marginTop = '-50px';")
                dojs("document.getElementById('video_overlay').style.marginTop = '0px';")
                dojs("document.getElementById('video_overlay_img').style.marginTop = '0px';")
            }
        })

        ## video functions
        do_video <- function(what, ..., id = "main_video") {
            getel <- paste0("document.getElementById('", id, "')")
            myargs <- list(...)
            if (what == "pause") {
                if (video_state$paused) {
                    dojs(paste0(getel, ".play();"))
                    video_state$paused <- FALSE
                    if (rally_state() == "click or unpause the video to start") rally_state("click serve start")
                } else {
                    dojs(paste0(getel, ".pause();"))
                    video_state$paused <- TRUE
                }
                NULL
            } else if (what == "play") {
                dojs(paste0(getel, ".play();"))
                video_state$paused <- FALSE
            } else if (what == "toggle_pause") {
                ## careful using this, because it doesn't update video_state$paused
                dojs(paste0("if (", getel, ".paused == true) { ", getel, ".play(); } else { ", getel, ".pause(); }"))
            } else if (what == "get_time") {
                dojs(paste0("Shiny.setInputValue('video_time', ", getel, ".currentTime)"))
            } else if (what == "get_time_fid") {
                dojs(paste0("Shiny.setInputValue('video_time', ", getel, ".currentTime + '&", myargs[[1]], "')"))
            } else if (what == "set_time") {
                dojs(paste0(getel, ".currentTime='", myargs[[1]], "';"))
            } else if (what == "set_current_video_time") {
                dojs(paste0("Shiny.setInputValue('set_current_video_time', ", getel, ".currentTime + '&", myargs[1], "&' + new Date().getTime())"))
            } else if (what == "tag_current_video_time") {
                dojs(paste0("Shiny.setInputValue('tag_current_video_time', ", getel, ".currentTime + '&", myargs[1], "')"))
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

        observeEvent(input$playback_rate, {
            if (!is.null(input$playback_rate)) do_video("playback_rate", input$playback_rate)
        })

        ## match data editing
        match_data_edit_mod <- callModule(mod_match_data_edit, id = "match_data_editor", rdata = rdata, editing = editing, styling = styling)
        ## team data editing
        team_edit_mod <- callModule(mod_team_edit, id = "team_editor", rdata = rdata, editing = editing, styling = styling)

        observeEvent(input$edit_cancel, {
            if (!is.null(editing$active) && editing$active %in% "teams") {
                team_edit_mod$htdata_edit(NULL)
                team_edit_mod$vtdata_edit(NULL)
            }
            editing$active <- NULL
            removeModal()
        })

        observeEvent(input$edit_commit, {
            if (!is.null(editing$active)) {
                changed <- code_make_change(editing$active, game_state = game_state, dvw = rdata$dvw, input = input, htdata_edit = team_edit_mod$htdata_edit(), vtdata_edit = team_edit_mod$vtdata_edit())
                rdata$dvw <- changed$dvw
                if (changed$do_reparse) {
                    ## we don't need to reparse (??), but (might) need to adjust game_state, e.g. if we've changed lineups
                    temp <- as.list(tail(rdata$dvw$plays2, 1))
                    for (nm in intersect(c("home_setter_position", "visiting_setter_position", paste0("home_p", 1:6), paste0("visiting_p", 1:6)), names(temp))) game_state[[nm]] <- temp[[nm]]
                }
                editing$active <- NULL
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
                if (!is.null(myclass) && nzchar(myclass) && myclass %in% c("form-control")) {
                    ## don't process these - they are e.g. key events in DT filter boxes
                    mycmd <- NULL
                }
                if (!is.null(mycmd)) {
                    if (debug > 1) cat("control key: ", mycmd, "\n")
                    mycmd <- strsplit(mycmd, "|", fixed = TRUE)[[1]] ## e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which
                    if (length(mycmd) == 5) {
                        ky <- mycmd[5] ## key pressed, as ASCII code
                        if (debug > 1) cat("key: ", ky, "\n")
                        if (ky %eq% 27) {
                            ## esc
                            if (isTRUE(scout_modal_active())) {
                                ## if we have a scouting modal showing, treat this as cancel and rewind
                                do_video("rew", 3)
                                do_video("play")
                                remove_scout_modal()
                            } else if (is.null(editing$active) || !editing$active %in% "teams") {
                                do_unpause <- !is.null(editing$active) && editing$active %eq% "admin"
                                editing$active <- NULL
                                removeModal()
                                if (do_unpause) do_video("pause")
                            }
                        } else if (ky %eq% 13) {
                            ## enter
                            ## if editing, treat as update
                            if (!is.null(editing$active) && !editing$active %eq% "teams") {
                                changed <- code_make_change(editing$active, game_state = game_state, dvw = rdata$dvw, input = input, htdata_edit = team_edit_mod$htdata_edit(), vtdata_edit = team_edit_mod$vtdata_edit())
                                rdata$dvw <- changed$dvw
                                if (changed$do_reparse) {
                                    ## we don't need to reparse (??), but (might) need to adjust game_state, e.g. if we've changed lineups
                                    temp <- as.list(tail(rdata$dvw$plays2, 1))
                                    for (nm in intersect(c("home_setter_position", "visiting_setter_position", paste0("home_p", 1:6), paste0("visiting_p", 1:6)), names(temp))) game_state[[nm]] <- temp[[nm]]
                                }
                                editing$active <- NULL
                            }
                            ## but not for team editing, because pressing enter in the DT fires this too
                        } else if (ky %in% c(90, 122)) {
                            ## z
                            ## temporarily hide the modal, so the video can be seen
                            dojs("$('#shiny-modal-wrapper').hide(); $('.modal-backdrop').hide();")
                        } else if (ky %in% utf8ToInt("qQ0")) {
                            ## video pause/unpause
                            ## don't allow unpause if we have a scouting modal shown
                            if (isTRUE(scout_modal_active())) {
                                ## but do allow pause, if somehow it isn't already
                                if (!video_state$paused) do_video("pause")
                            } else {
                                if (video_state$paused) {
                                    ## we are paused
                                    if (is.null(editing$active)) {
                                        ## just unpause
                                        do_video("pause")
                                    } else if (editing$active %eq% "admin") {
                                        ## otherwise, and only if we have the admin modal showing, dismiss it and unpause
                                        editing$active <- NULL
                                        removeModal()
                                        do_video("pause")
                                    }
                                } else {
                                    ## not paused, so pause and show admin modal
                                    do_video("pause")
                                    editing$active <- "admin"
                                    show_admin_modal()
                                }
                            }
                        } else if (ky %in% utf8ToInt("nm13jhl;46$^b,79")) {
                            if (is.null(editing$active)) {
                                ## video forward/backward nav
                                ## same as for other ovscout interface, although the fine control is not needed here?
                                vidcmd <- if (ky %in% utf8ToInt("1nhj4$b7")) "rew" else "ff"
                                dur <- if (ky %in% utf8ToInt("h$;^")) 10 else if (ky %in% utf8ToInt("nm13")) 0.1 else if (ky %in% utf8ToInt("b7,9")) 1/30 else 2
                                do_video(vidcmd, dur)
                            }
                        }
                    }
                }
            }
        })
        observeEvent(input$controlkeyup, {
            ## keys that might not get detected by keypress but do by keydown?
            if (!is.null(input$controlkey)) {
                temp <- strsplit(input$controlkey, "@")[[1]]
                ## elements are modifiers_and_key element_class element_id cursor_position field_length time
                mycmd <- temp[1]
                myclass <- temp[2]
                myid <- temp[3]
                if (!is.null(myclass) && nzchar(myclass) && myclass %in% c("form-control")) {
                    ## don't process these - they are e.g. key events in DT filter boxes
                    mycmd <- NULL
                }
                if (!is.null(mycmd)) {
                    if (debug > 1) cat("control key up: ", mycmd, "\n")
                    mycmd <- strsplit(mycmd, "|", fixed = TRUE)[[1]] ## e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which
                    if (length(mycmd) == 5) {
                        ky <- mycmd[5]
                        if (ky %in% c("90", "122")) {
                            ## z
                            ## temporarily hide the modal, so the video can be seen
                            dojs("$('#shiny-modal-wrapper').show(); $('.modal-backdrop').show();")
                        }
                    }
                }
            }
        })


        gg_tight <- list(theme(legend.position = "none", panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0, "null"), plot.margin = rep(unit(0, "null"), 4), axis.ticks = element_blank(), axis.ticks.length = unit(0, "null"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()), scale_x_continuous(limits = c(0, 1), expand = c(0, 0)), scale_y_continuous(limits = c(0, 1), expand = c(0, 0)))
        overlay_points <- reactiveVal(NULL)
        observe({
            output$video_overlay <- renderPlot({
                ## test - red diagonal line across the overlay plot
                ##ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes_string("x", "y")) + geom_path(color = "red") + gg_tight
                ## need to plot SOMETHING else we don't get correct coordinates back
                ##this <- selected_event()
                p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes_string("x", "y")) + gg_tight
                if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                    ixy <- setNames(crt_to_vid(overlay_points()), c("x", "y"))
                    p <- p + geom_point(data = ixy, fill = "dodgerblue", pch = 21, col = "white", size = 6)
                }
                p
            }, bg = "transparent", width = vo_width(), height = vo_height())
        })
        vid_to_crt <- function(obj) {
            courtxy <- data.frame(x = rep(NA_real_, length(obj$x)), y = rep(NA_real_, length(obj$x)))
            if (!is.null(app_data$court_ref)) {
                if (length(obj$x) > 0) courtxy <- ovideo::ov_transform_points(obj$x, obj$y, ref = app_data$court_ref, direction = "to_court")
            }
            courtxy
        }
        crt_to_vid <- function(obj) {
            imagexy <- data.frame(image_x = rep(NA_real_, length(obj$x)), image_y = rep(NA_real_, length(obj$x)))
            if (!is.null(app_data$court_ref)) {
                if (length(obj$x) > 0) imagexy <- setNames(ovideo::ov_transform_points(obj$x, obj$y, ref = app_data$court_ref, direction = "to_image"), c("image_x", "image_y"))
            }
            imagexy
        }
        flash_screen <- function() dojs("$('#video_overlay_img').css('background-color', '#FFFF0080'); setTimeout(function() { $('#video_overlay_img').css('background-color', ''); }, 50);")

        loop_trigger <- reactiveVal(0L)
        observeEvent(input$video_click, {
            ## when video clicked, get the corresponding video time and trigger the loop
            flash_screen() ## visual indicator that click has registered
            time_uuid <- uuid()
            game_state$current_time_uuid <- time_uuid
            do_video("get_time_fid", time_uuid) ## make asynchronous request
            loop_trigger(loop_trigger() + 1L)
            ## TODO MAYBE also propagate the click to elements below the overlay?
        })

        ## video times are a pain, because we get asynchronous replies from the browser via input$video_time
        video_times <- list()
        observeEvent(input$video_time, {
            ## when a time comes in, stash it under its uuid
            temp <- input$video_time
            this_uuid <- sub(".*&", "", temp)
            if (nzchar(this_uuid)) video_times[[this_uuid]] <<- as.numeric(sub("&.+", "", temp))
        })
        retrieve_video_time <- function(id) {
            if (is_uuid(id)) {
                if (nzchar(id) && id %in% names(video_times)) video_times[[id]] else NA_real_
            } else {
                id
            }
        }

        ## rally_codes is a reactive that returns a tibble with columns team, pnum, skill, tempo, eval, combo, target, sz, ez, esz, x_type, num_p, special, custom, t, start_x, start_y, end_x, end_y
        ## rally_codes are the actions in the current rally

        ## keep track of whether we have a modal up or not, so that pause behaviour can be modified
        scout_modal_active <- reactiveVal(FALSE)
        show_scout_modal <- function(...) {
            scout_modal_active(TRUE)
            showModal(...)
        }
        remove_scout_modal <- function() {
            scout_modal_active(FALSE)
            removeModal()
        }

        ## single click the video to register a tag location, or starting ball coordinates
        observeEvent(loop_trigger(), {
            if (loop_trigger() > 0) {
                courtxy <- vid_to_crt(input$video_click)
                ##court_inset$add_to_click_queue(courtxy)
                if (rally_state() == "click or unpause the video to start") {
                    do_video("play")
                    rally_state("click serve start")
                } else if (rally_state() == "click serve start") {
                    ## click was the serve position
                    game_state$start_x <- courtxy$x[1]
                    game_state$start_y <- courtxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy)
                    ## add placeholder serve code, will get updated on next click
                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    ## serve type should have been selected in the preselect
                    st <- if (!is.null(input$serve_preselect_type)) input$serve_preselect_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    ## time probably won't have resolved yet, so add it after next click
                    rally_codes(code_trow(team = game_state$serving, pnum = sp, skill = "S", tempo = st, sz = sz, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table))
                    rally_state("click serve end")
                } else if (rally_state() == "click serve end") {
                    do_video("pause")
                    ## click was the end-of-serve position, either error or reception
                    game_state$end_x <- courtxy$x[1]
                    game_state$end_y <- courtxy$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    overlay_points(rbind(overlay_points(), courtxy))
                    ## pop up to find either serve error, or passing player
                    ## passing player options
                    pass_pl_opts <- guess_pass_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)
                    names(pass_pl_opts$choices) <- player_nums_to(pass_pl_opts$choices, team = other(game_state$current_team), dvw = rdata$dvw)
                    pass_pl_opts$choices <- c(pass_pl_opts$choices, Unknown = "Unknown")

                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    chc <- app_data$options$skill_tempo_map %>% dplyr::filter(.data$skill == "Serve") %>% mutate(tempo = sub(" serve", "", .data$tempo))
                    chc <- setNames(chc$tempo_code, chc$tempo)
                    serve_type_buttons <- make_fat_radio_buttons(choices = chc, selected = input$serve_preselect_type, input_var = "serve_type")
                    passer_buttons <- make_fat_radio_buttons(choices = pass_pl_opts$choices, selected = pass_pl_opts$selected, input_var = "select_passer")
                    serve_error_buttons <- make_fat_buttons(choices = c("Serve error" = "=", "Serve error (in net)" = "=N", "Serve error (foot fault)" = "=Z", "Serve error (long)" = "=O", "Serve error (out left)" = "=L", "Serve error (out right)" = "=R"), input_var = "was_serve_error")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Serve type:")),
                                            do.call(fixedRow, lapply(serve_type_buttons, function(but) column(2, but))),
                                            tags$hr(),
                                            tags$div("AND"),
                                            tags$br(),
                                            do.call(fixedRow, lapply(serve_error_buttons, function(but) column(2, but))),
                                            tags$br(),
                                            tags$div("OR"),
                                            tags$br(),
                                            tags$p(tags$strong("Select passer:")),
                                            do.call(fixedRow, lapply(passer_buttons, function(but) column(1, but))),
                                            fixedRow(column(2, actionButton("was_serve_ace", "Reception error (serve ace)", style = paste0("width:100%; height:7vh;")))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", style = paste0("width:100%; height:7vh; background-color:", styling$cancel))),
                                                     column(2, offset = 8, actionButton("assign_passer", "Continue", style = paste0("width:100%; height:7vh; background-color:", styling$continue))))
                                            ))
                } else if (rally_state() == "enter serve outcome") {
                    sp <- if (!is.null(input$serve_preselect_player)) input$serve_preselect_player else if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    game_state$current_team <- other(game_state$serving)
                    st <- if (!is.null(input$serve_type)) input$serve_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    pp <- input$select_passer
                    remove_scout_modal()
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    esz <- paste(dv_xy2subzone(game_state$end_x, game_state$end_y), collapse = "")
                    start_t <- retrieve_video_time(game_state$start_t)
                    end_t <- retrieve_video_time(game_state$end_t)
                    rc <- rally_codes()
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, esz = esz, t = start_t, end_x = game_state$end_x, end_y = game_state$end_y)
                    }
                    rally_codes(bind_rows(rc, code_trow(team = other(game_state$serving), pnum = pp, skill = "R", tempo = st, sz = sz, esz = esz, t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, default_scouting_table = app_data$default_scouting_table)))
                    rally_state("click second contact")
                    do_video("rew", app_data$play_overlap); do_video("play")
                } else if (rally_state() == "serve ace") {
                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    pp <- input$select_passer
                    st <- if (!is.null(input$serve_type)) input$serve_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    remove_scout_modal()
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    esz <- paste(dv_xy2subzone(game_state$end_x, game_state$end_y), collapse = "")
                    start_t <- retrieve_video_time(game_state$start_t)
                    end_t <- retrieve_video_time(game_state$end_t)
                    rc <- rally_codes()
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) {
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, eval = "#", ez = substr(esz, 1, 1), esz = substr(esz, 2, 2), t = start_t, end_x = game_state$end_x, end_y = game_state$end_y)
                    }
                    rally_codes(bind_rows(rc, code_trow(team = other(game_state$serving), pnum = pp, skill = "R", eval = "=", tempo = st, sz = sz, esz = esz, t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, default_scouting_table = app_data$default_scouting_table)))

                    game_state$current_team <- game_state$serving
                    game_state$point_won_by <- game_state$serving
                    rally_state("rally ended")
                    do_video("play")
                } else if (rally_state() == "serve error") {
                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    serve_err_type <- if (!is.null(input$was_serve_error)) input$was_serve_error else "="
                    st <- if (!is.null(input$serve_type)) input$serve_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    remove_scout_modal()
                    special_code <- substr(serve_err_type, 2, 2)
                    esz <- paste(dv_xy2subzone(game_state$end_x, game_state$end_y), collapse = "")
                    rc <- rally_codes()
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, eval = "=", esz = esz, special = if (nzchar(special_code)) special_code else "~", t = retrieve_video_time(game_state$start_t), end_x = game_state$end_x, end_y = game_state$end_y)
                    }
                    rally_codes(rc)
                    rally_state("rally ended")
                    game_state$point_won_by <- other(game_state$serving)
                    do_video("play")
                } else if (rally_state() == "click second contact") {
                    ## set (play continues), setter dump, set error, P2 attack, or freeball over (by the receiving team)
                    ## or PR, dig/freeball dig by opposition
                    ## we get a clue if it's the receiving/digging team or their opposition by the side of the court that has been clicked
                    do_video("pause")
                    ## click was the set contact position, or the freeball start position
                    game_state$start_x <- courtxy$x[1]
                    game_state$start_y <- courtxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy)
                    ## popup
                    ## TODO maybe also setter call here
                    c2_buttons <- make_fat_radio_buttons(
                        choices = c(Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball attack" = "P2", "Freeball over" = "F", ## rcv team actions
                                    "Opp. dig" = "aD", "Opp. dig error" = "aD=", "Opp. overpass attack" = "aPR"), ## opp actions
                        selected = "E", input_var = "c2")
                    if (app_data$is_beach) {
                        stop("setter for beach")
                        ## choose the player who didn't pass
                    }
                    sp <- get_setter(game_state)
                    sp <- c(sp, setdiff(get_players(game_state, dvw = rdata$dvw), sp), get_liberos(game_state, dvw = rdata$dvw))
                    names(sp) <- player_nums_to(sp, team = game_state$current_team, dvw = rdata$dvw)
                    sp <- c(sp, Unknown = "Unknown")
                    setter_buttons <- make_fat_radio_buttons(choices = sp, input_var = "c2_player")
                    opp <- c(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw), get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_buttons <- make_fat_radio_buttons(choices = opp, selected = NA, input_var = "c2_opp_player")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Second contact:")),
                                            do.call(fixedRow, lapply(c2_buttons[1:5], function(but) column(2, but))),
                                            tags$br(),
                                            tags$p("by player"),
                                            tags$br(),
                                            do.call(fixedRow, lapply(setter_buttons, function(but) column(1, but))),
                                            tags$br(),
                                            tags$div("OR"),
                                            tags$br(),
                                            do.call(fixedRow, lapply(c2_buttons[6:8], function(but) column(2, but))),
                                            tags$br(),
                                            tags$p("by player"),
                                            tags$br(),
                                            do.call(fixedRow, lapply(opp_buttons, function(but) column(1, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", style = paste0("width:100%; height:7vh; background-color:", styling$cancel))),
                                                     column(2, offset = 8, actionButton("assign_c2", "Continue", style = paste0("width:100%; height:7vh; background-color:", styling$continue))))
                                            ))
                } else if (rally_state() == "second contact details") {
                    ## set uses end position for zone/subzone
                    esz <- paste(dv_xy2subzone(game_state$start_x, game_state$start_y), collapse = "")
                    passq <- guess_pass_quality(game_state, dvw = rdata$dvw)
                    ## TODO, this better
                    rc <- rally_codes()
                    rc$eval[rc$skill %eq% "R"] <- passq
                    ## find corresponding serve evaluation code
                    seval <- app_data$compound_table$code[app_data$compound_table$skill %eq% "S" & app_data$compound_table$compound_skill %eq% "R" & app_data$compound_table$compound_code %eq% passq]
                    if (nchar(seval) != 1) seval <- "~"
                    rc$eval[rc$skill %eq% "S"] <- seval
                    start_t <- retrieve_video_time(game_state$start_t)
                    ## possible values for input$c2 are: Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball attack" = "P2", "Freeball over" = "F",
                    ##                                   "Opp. dig" = "aD", error "aD=", "Opp. overpass attack" = "aPR"
                    if (input$c2 %in% c("E", "E=", "PP", "P2", "F")) {
                        sp <- input$c2_player
                        if (input$c2 == "E") {
                            rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "E", esz = esz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                            rally_state("click third contact")
                        } else if (input$c2 == "E=") {
                            ## set error
                            rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "E", eval = "=", esz = esz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                            rally_state("rally ended")
                            game_state$point_won_by <- other(game_state$current_team)
                        } else if (input$c2 %in% c("PP", "P2")) {
                            ## setter dump or second ball attack
                            sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                            ## NOTE we have hard-coded PP, P2 here, TODO fix to allow other codes to be used
                            trg <- if (input$c2 == "PP") "S" else "~"
                            rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "A", tempo = "O", combo = input$c2, target = trg, sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                            rally_state("click attack end point")
                            game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                        } else if (input$c2 == "F") {
                            ## freeball over
                            sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                            rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "F", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                            ## TODO add end pos to this on next contact
                            game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                            rally_state("click attack end point") ## TODO don't use that popup, have a dedicated one
                        }
                    } else if (input$c2 %eq% "aPR") {
                        ## opposition overpass attack
                        op <- if (!is.null(input$c2_opp_player)) input$c2_opp_player else 0L
                        rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "A", tempo = "O", combo = "PR", sz = substr(esz, 1, 1), t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                        rally_state("click attack end point")
                    } else if (input$c2 %in% c("aD", "aD=")) {
                        ## opposition dig on overpass
                        op <- if (!is.null(input$c2_opp_player)) input$c2_opp_player else 0L
                        rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "D", eval = if (input$c2 %eq% "aD=") "=" else "~", sz = substr(esz, 1, 1), t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                        if (input$c2 %eq% "aD=") {
                            game_state$point_won_by <- game_state$current_team
                            rally_state("rally ended")
                        } else {
                            game_state$current_team <- other(game_state$current_team)
                            rally_state("click second contact")
                        }
                    }
                    remove_scout_modal()
                    if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
                    do_video("play")
                } else if (rally_state() == "click third contact") {
                    ## attack, freeball over (by the setting team)
                    ## or dig/freeball dig by on overset, or PR
                    do_video("pause")
                    ## click was the attack contact position, or the freeball start position
                    game_state$start_x <- courtxy$x[1]
                    game_state$start_y <- courtxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy)
                    ## popup
                    ac <- c(guess_attack_code(game_state, dvw = rdata$dvw), "Other attack")
                    c3_buttons <- make_fat_radio_buttons(choices = c(setNames(ac, ac), c("Opp. dig" = "aD", "Opp. dig error" = "aD=", "Opp. overpass attack" = "aPR")), input_var = "c3")
                    #ap <- get_players(game_state, dvw = rdata$dvw)

                    attack_pl_opts <- guess_attack_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)

                    ap <- attack_pl_opts$choices

                    ## TODO default selection better based on rotation
                    names(ap) <- player_nums_to(ap, team = game_state$current_team, dvw = rdata$dvw)
                    ap <- c(ap, Unknown = "Unknown")

                    attacker_buttons <- make_fat_radio_buttons(choices = ap, selected = attack_pl_opts$selected, input_var = "c3_player")
                    if (isTRUE(app_data$options$nblockers)) nblocker_buttons <- make_fat_radio_buttons(choices = c("No block" = 0, "Single block" = 1, "Double block" = 2, "Triple block" = 3), selected = if (!is.null(app_data$options$default_nblockers)) app_data$options$default_nblockers, input_var = "nblockers")
                    ## attack error, blocked, replay will be scouted on next entry
                    ## TODO other special codes ?
                    ## TODO "F" freeball
                    opp <- c(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw), get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_player_buttons <- make_fat_radio_buttons(choices = opp, selected = NA, input_var = "c3_opp_player")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Attack:")),
                                            do.call(fixedRow, lapply(c3_buttons[seq_along(ac)], function(but) column(2, but))),
                                            tags$br(), tags$p("by player"), tags$br(),
                                            do.call(fixedRow, lapply(attacker_buttons, function(but) column(1, but))),
                                            if (isTRUE(app_data$options$nblockers)) tags$div(tags$br(), "with", tags$br()),
                                            if (isTRUE(app_data$options$nblockers)) do.call(fixedRow, lapply(nblocker_buttons, function(but) column(2, but))),
                                            tags$br(), tags$hr(), tags$div("OR"), tags$br(),
                                            do.call(fixedRow, lapply(tail(c3_buttons, 3), function(but) column(2, but))),
                                            tags$br(), tags$p("by player"), tags$br(),
                                            do.call(fixedRow, lapply(opp_player_buttons, function(but) column(1, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", style = paste0("width:100%; height:7vh; background-color:", styling$cancel))),
                                                     column(2, offset = 8, actionButton("assign_c3", "Continue", style = paste0("width:100%; height:7vh; background-color:", styling$continue))))
                                            ))
                } else if (rally_state() == "third contact details") {
                    ## possible values for input$c3 are: an attack code, Freeball
                    ##    "Opp. dig" = "aD", "Opp. overpass attack" = "aPR"
                    start_t <- retrieve_video_time(game_state$start_t)
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                    if (input$c3 %eq% "aPR") {
                        ## opposition overpass attack
                        op <- if (!is.null(input$c3_opp_player)) input$c3_opp_player else 0L
                        rally_codes(bind_rows(rally_codes(), code_trow(team = other(game_state$current_team), pnum = op, skill = "A", tempo = "O", combo = "PR", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                        rally_state("click attack end point")
                    } else if (input$c3 %in% c("aD", "aD=")) {
                        ## opposition dig on overpass
                        op <- if (!is.null(input$c3_opp_player)) input$c3_opp_player else 0L
                        rally_codes(bind_rows(rally_codes(), code_trow(team = other(game_state$current_team), pnum = op, skill = "D", eval = if (input$c3 %eq% "aD=") "=" else "~", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                        if (input$c3 %eq% "aD=") {
                            game_state$point_won_by <- game_state$current_team
                            rally_state("rally ended")
                        } else {
                            game_state$current_team <- other(game_state$current_team)
                            rally_state("click second contact")
                        }
                    } else {
                        if (input$c3 == "F") {
                            ## freeball over
                            rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = if (!is.null(input$c3_player)) input$c3_player else 0L, skill = "F", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                            ## TODO add end pos to this on next contact
                            game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                            rally_state("click attack end point") ## TODO don't use that popup, have a dedicated one
                        } else {
                            ## only an attack code or "Other attack" for the time being
                            ap <- if (!is.null(input$c3_player)) input$c3_player else 0L
                            ac <- input$c3
                            if (nchar(ac) == 2) {
                                tempo <- tryCatch(rdata$dvw$meta$attacks$type[rdata$dvw$meta$attacks$code %eq% input$c3], error = function(e) "~")
                                targ <- tryCatch(rdata$dvw$meta$attacks$set_type[rdata$dvw$meta$attacks$code %eq% input$c3], error = function(e) "~")
                            } else {
                                ## other attack
                                ac <- "~~" ## no combo code
                                tempo <- "H"
                                targ <- "~"
                            }
                            if (nchar(tempo) != 1) tempo <- "~"
                            if (nchar(targ) != 1 || targ %eq% "-") targ <- "~"
                            nb <- input$nblockers
                            if (is.null(nb) || !nb %in% 0:3) nb <- "~"
                            ##if (nchar(input$c3) == 2) {
                            rally_codes(bind_rows(rally_codes(), code_trow(team = game_state$current_team, pnum = ap, skill = "A", tempo = tempo, combo = ac, target = targ, sz = sz, num_p = nb, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, default_scouting_table = app_data$default_scouting_table)))
                            rally_state("click attack end point")
                            game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                        }
                    }
                    remove_scout_modal()
                    if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
                    do_video("play")
                } else if (rally_state() == "click attack end point") {
                    ## dig, dig error (attack kill), attack error, blocked, blocked for replay, block touch (attack kill)
                    ## or block touch and play continues
                    ## allow attack kill with no dig error?
                    do_video("pause")
                    ## click was the dig or attack kill or error position, or the freeball start position
                    game_state$end_x <- courtxy$x[1]
                    game_state$end_y <- courtxy$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    overlay_points(courtxy)
                    ## popup
                    c1_buttons <- make_fat_radio_buttons(choices = c("Attack kill" = "A#", "Attack error" = "A=", "Dig" = "D", "Dig error" = "D=", "Block fault" = "B/"), input_var = "c1")

                    ## Identify defending players
                    dig_pl_opts <- guess_dig_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)
                    digp <- dig_pl_opts$choices
                    names(digp) <- player_nums_to(digp, team = game_state$current_team, dvw = rdata$dvw)
                    digp <- c(digp, Unknown = "Unknown")

                    # digp <- c(get_players(game_state, dvw = rdata$dvw), get_liberos(game_state, dvw = rdata$dvw)) ## TODO better based on rotation
                    # names(digp) <- player_nums_to(digp, team = game_state$current_team, dvw = rdata$dvw)
                    # digp <- c(digp, Unknown = "Unknown")
                    dig_player_buttons <- make_fat_radio_buttons(choices = digp, selected = dig_pl_opts$selected, input_var = "c1_def_player")
                    blockp <- get_players(game_state, team = game_state$current_team, dvw = rdata$dvw)
                    if (length(blockp) == 6) blockp <- blockp[2:4] ## front-row
                    names(blockp) <- player_nums_to(blockp, team = game_state$current_team, dvw = rdata$dvw)
                    blockp <- c(blockp, Unknown = "Unknown")
                    block_player_buttons <- make_fat_radio_buttons(choices = blockp, selected = NA, input_var = "c1_block_touch_player")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Attack outcome:")),
                                            do.call(fixedRow, lapply(c1_buttons[1:2], function(but) column(2, but))),
                                            tags$br(), tags$hr(), tags$div("OR", tags$strong("Defence outcome:")),
                                            do.call(fixedRow, lapply(c1_buttons[3:5], function(but) column(2, but))),
                                            tags$br(), tags$p("by player"), tags$br(),
                                            do.call(fixedRow, lapply(dig_player_buttons, function(but) column(1, but))),
                                            tags$br(), tags$hr(), tags$div("WITH", tags$strong("Block touch"), "by player"), tags$br(),
                                            do.call(fixedRow, lapply(block_player_buttons, function(but) column(2, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", style = paste0("width:100%; height:7vh; background-color:", styling$cancel))),
                                                     column(2, offset = 8, actionButton("assign_c1", "Continue", style = paste0("width:100%; height:7vh; background-color:", styling$continue))))
                                            ))
                } else if (rally_state() == "first contact details") {
                    ## possible values for input$c1 are currently: A#, A=, D, D=, B/
                    ## any of these except B/ can be preceded by a block touch
                    if (!is.null(input$c1_block_touch_player)) {
                        if (!input$c1 %eq% "B/") {
                            beval <- if (input$c1 %eq% "A#") "=" else app_data$default_scouting_table$evaluation_code[app_data$default_scouting_table$skill == "B"]
                            rc <- rally_codes()
                            Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                            rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = input$c1_block_touch_player, skill = "B", eval = beval, tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, default_scouting_table = app_data$default_scouting_table))) ## TODO x,y?
                        }
                    }
                    esz <- paste(dv_xy2subzone(game_state$end_x, game_state$end_y), collapse = "")
                    if (input$c1 %in% c("A#", "A=")) {
                        ##end_t <- retrieve_video_time(game_state$end_t)
                        eval <- substr(input$c1, 2, 2)
                        ## find the attack, should be either the previous skill, or one previous to that with a block in between
                        rc <- rally_codes()
                        Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                        if (!is.na(Aidx)) {
                            rc$esz[Aidx] <- esz
                            rc$end_x[Aidx] <- game_state$end_x
                            rc$end_y[Aidx] <- game_state$end_y
                            rc$eval[Aidx] <- eval
                            rally_codes(rc)
                        }
                        rally_state("rally ended")
                        ## "current" team here is the digging team
                        game_state$point_won_by <- if (eval == "#") other(game_state$current_team) else game_state$current_team
                    } else if (input$c1 %eq% "B/") {
                        rc <- rally_codes()
                        Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                        ## block fault player should be in input$c1_def_player, but we'll take input$b1_block_touch_player otherwise
                        bp <- if (!is.na(input$c1_def_player)) input$c1_def_player else if (!is.na(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
                        rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "/", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, default_scouting_table = app_data$default_scouting_table))) ## TODO x,y?
                        rally_state("rally ended")
                        game_state$point_won_by <- other(game_state$current_team) ## "current" team here is the digging team
                    } else {
                        ## D or D=
                        digp <- input$c1_def_player
                        end_t <- retrieve_video_time(game_state$end_t)
                        rc <- rally_codes()
                        ## was the previous skill an attack, or one previous to that an attack with a block in between
                        Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                        if (!is.na(Aidx)) {
                            rc$esz[Aidx] <- esz
                            rc$end_x[Aidx] <- game_state$end_x
                            rc$end_y[Aidx] <- game_state$end_y
                            tempo <- rc$tempo[Aidx]
                        } else {
                            tempo <- "~"
                        }
                        if (input$c1 == "D=") {
                            ## TODO use compound codes here?
                            if (!is.na(Aidx)) rc$eval[Aidx] <- "#"
                            eval <- "="
                        } else {
                            ## TODO use compound codes here?
                            if (!is.na(Aidx)) rc$eval[Aidx] <- "-"
                            eval <- "+"
                        }
                        ## TODO CHECK is the dig start zone the same as the attack start zone, or its end zone?
                        rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "D", eval = eval, tempo = tempo, sz = substr(esz, 1, 1), t = end_t, start_x = game_state$end_x, start_y = game_state$end_y, default_scouting_table = app_data$default_scouting_table)))
                        if (input$c1 == "D=") {
                            rally_state("rally ended")
                            game_state$point_won_by <- other(game_state$current_team)
                        } else {
                            if (!isTRUE(app_data$options$transition_sets)) {
                                rally_state("click third contact")
                            } else {
                                rally_state("click second contact")
                            }
                        }
                    }
                    remove_scout_modal()
                    if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
                    do_video("play")
                } else {
                    stop("unknown rally state: ", rally_state())
                }
            }
            if (debug > 0) {
                if (nrow(rally_codes())) {
                    cat("rally codes:\n")
                    print_rally_codes(rally_codes())
                }
                cat("rally state: ", rally_state(), "\n")
            }
            if (rally_state() == "rally ended") {
                ## add rally codes to scout object now
                rdata$dvw$plays2 <- bind_rows(rdata$dvw$plays2, make_plays2(rally_codes(), game_state = game_state, rally_ended = TRUE, dvw = rdata$dvw))
                do_rally_end_things()
            }
        })

        do_rally_end_things <- function() {
            ## update game_state
            do_rot <- game_state$point_won_by != game_state$serving
            if (game_state$point_won_by == "*") {
                game_state$home_score_start_of_point <- game_state$home_score_start_of_point + 1L
            } else {
                game_state$visiting_score_start_of_point <- game_state$visiting_score_start_of_point + 1L
            }
            if (do_rot) {
                if (game_state$point_won_by == "*") {
                    game_state$home_setter_position <- rotpos(game_state$home_setter_position, n = length(pseq))
                    temp <- rotvec(as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)]))
                    for (i in pseq) game_state[[paste0("home_p", i)]] <- temp[i]
                    poscode <- paste0("*z", game_state$home_setter_position)
                } else {
                    game_state$visiting_setter_position <- rotpos(game_state$visiting_setter_position, n = length(pseq))
                    temp <- rotvec(as.numeric(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)]))
                    for (i in pseq) game_state[[paste0("visiting_p", i)]] <- temp[i]
                    poscode <- paste0("az", game_state$visiting_setter_position)
                }
                rdata$dvw$plays2 <- bind_rows(rdata$dvw$plays2, make_plays2(poscode, game_state = game_state, dvw = rdata$dvw))
            }
            ## reset for next rally
            game_state$serving <- game_state$current_team <- game_state$point_won_by
            rally_codes(empty_rally_codes)
            game_state$start_x <- game_state$start_y <- game_state$end_x <- game_state$end_y <- NA_real_
            game_state$current_time_uuid <- ""
            game_state$point_won_by <- NA_character_
            rally_state("click serve start")
        }

        ## convert the rally codes into plays2 rows, and build plays from plays2
        observe({
            temp_rally_plays2 <- if (nrow(rally_codes()) > 0) make_plays2(rally_codes(), game_state = game_state, dvw = rdata$dvw) else NULL
            ##            cat(str(temp_rally_plays2))
            ##            cat(str(rdata$dvw$plays2))
            rdata$dvw$plays <- plays2_to_plays(bind_rows(rdata$dvw$plays2, temp_rally_plays2), dvw = rdata$dvw, evaluation_decoder = app_data$evaluation_decoder)
            playslist_mod$scroll_playslist(nrow(rdata$dvw$plays))
        })

        observeEvent(input$cancelrew, {
            do_video("rew", 3)
            do_video("play")
            remove_scout_modal()
        })

        observeEvent(input$was_serve_ace, {
            rally_state("serve ace")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$was_serve_error, {
            rally_state("serve error")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$assign_passer, {
            rally_state("enter serve outcome")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$assign_c1, {
            rally_state("first contact details")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$assign_c2, {
            rally_state("second contact details")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$assign_c3, {
            rally_state("third contact details")
            loop_trigger(loop_trigger() + 1L)
        })

        output$rally_state <- renderUI({
            tags$div(id = "rallystate", ##style = if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) paste0("margin-top: ", input$vo_offset - 50, "px") else "",
                     tags$strong("Rally state: "), rally_state())
        })
        observeEvent(rally_state(), {
            if (rally_state() == "click serve start") {
                ## show the serve player and tempo pre-select buttons
                sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                ## sp should be the serving player
                ## other players that could be serving, if the rotation is somehow wrong
                other_sp <- get_players(game_state, team = game_state$serving, dvw = rdata$dvw)
                serve_player_buttons <- make_fat_radio_buttons(choices = c(sp, setdiff(other_sp, sp)), selected = sp, input_var = "serve_preselect_player")
                ## default serve type is either the most common serve type by this player, or the default serve type
                st_default <- get_player_serve_type(px = rdata$dvw$plays, serving_player_num = sp, game_state = game_state, opts = app_data$options)
                if (is.na(st_default)) st_default <- app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                chc <- app_data$options$skill_tempo_map %>% dplyr::filter(.data$skill == "Serve") %>% mutate(tempo = sub(" serve", "", .data$tempo))
                chc <- setNames(chc$tempo_code, chc$tempo)
                serve_type_buttons <- make_fat_radio_buttons(choices = chc, selected = st_default, input_var = "serve_preselect_type")
                output$serve_preselect <- renderUI(
                    tags$div(tags$strong("Serve type:"), do.call(fixedRow, lapply(serve_type_buttons, function(but) column(2, but))),
                             tags$strong("Serve player:"), do.call(fixedRow, lapply(serve_player_buttons, function(but) column(2, but))))
                )
            } else {
                output$serve_preselect <- NULL
            }
        })

        ## initialize the game state
        rally_state <- reactiveVal("click or unpause the video to start")
        rally_codes <- reactiveVal(empty_rally_codes)
        temp <- as.list(tail(app_data$dvw$plays2, 1))
        if (!"serving" %in% names(temp) || is.na(temp$serving)) temp$serving <- "*" ## default to home team serving - maybe allow this as a parm to ov_scouter (maybe TODO)
        temp$current_team <- temp$serving
        temp$start_x <- temp$start_y <- temp$end_x <- temp$end_y <- NA_real_
        temp$current_time_uuid <- ""
        game_state <- do.call(reactiveValues, temp)
        court_inset$home_team_end("upper") ## home team end defaults to upper

        observe({
            game_state$home_end <- court_inset$home_team_end()
        })

        show_admin_modal <- function() {
            ## home player sub buttons
            ht_on <- get_players(game_state, team = "*", dvw = rdata$dvw)
            ht_other <- setdiff(rdata$dvw$meta$players_h$number, ht_on)
            ht_other <- setdiff(ht_other, get_liberos(game_state, team = "*", dvw = rdata$dvw))
            ht_sub_out <- make_fat_radio_buttons(choices = ht_on, selected = NA, input_var = "ht_sub_out")
            ht_sub_in <- make_fat_radio_buttons(choices = ht_other, selected = NA, input_var = "ht_sub_in")
            ## visiting player sub buttons
            vt_on <- get_players(game_state, team = "a", dvw = rdata$dvw)
            vt_other <- setdiff(rdata$dvw$meta$players_v$number, vt_on)
            vt_other <- setdiff(vt_other, get_liberos(game_state, team = "a", dvw = rdata$dvw))
            vt_sub_out <- make_fat_radio_buttons(choices = vt_on, selected = NA, input_var = "vt_sub_out")
            vt_sub_in <- make_fat_radio_buttons(choices = vt_other, selected = NA, input_var = "vt_sub_in")

            showModal(vwModalDialog(title = "Miscellaneous", footer = NULL,
                                    if (!is.null(rdata$dvw$plays2)) {
                                        tags$div(tags$p(tags$strong("File operations")), downloadButton("save_file_button", "Save file"))
                                    },
                                    tags$hr(),
                                    tags$p(tags$strong("Match actions")),
                                    ## TODO consider if all of these buttons should be available mid-rally or not (e.g. timeouts)
                                    fluidRow(column(6, tags$strong(home_team(rdata$dvw), "(home)")),
                                             column(6, tags$strong(visiting_team(rdata$dvw), "(visiting)"))),
                                    fluidRow(column(2, make_fat_buttons(choices = c("Won current rally" = "*p"), input_var = "manual_code")),
                                             column(2, make_fat_buttons(choices = c(Timeout = "*T"), input_var = "manual_code")),
                                             column(2, offset = 2, make_fat_buttons(choices = c("Won current rally" = "ap"), input_var = "manual_code")),
                                             column(2, make_fat_buttons(choices = c(Timeout = "aT"), input_var = "manual_code"))),
                                    fluidRow(column(6, tags$p("Player out"),
                                                    do.call(fixedRow, lapply(ht_sub_out, function(but) column(2, but))),
                                                    tags$p("Player in"),
                                                    do.call(fixedRow, lapply(ht_sub_in, function(but) column(if (length(ht_other) <= 6) 2 else 1, but)))),
                                             column(6, tags$p("Player out"),
                                                    do.call(fixedRow, lapply(vt_sub_out, function(but) column(2, but))),
                                                    tags$p("Player in"),
                                                    do.call(fixedRow, lapply(vt_sub_in, function(but) column(if (length(vt_other) <= 6) 2 else 1, but))))
                                             ),
                                    fluidRow(column(2, offset = 4, make_fat_buttons(choices = c("Make substitution" = "*C"), input_var = "manual_code")),
                                             column(2, offset = 4, make_fat_buttons(choices = c("Make substitution" = "aC"), input_var = "manual_code"))),
                                    tags$hr(),
                                    fixedRow(column(2, offset = 10, actionButton("admin_dismiss", "Return to scouting", style = paste0("width:100%; height:7vh; background-color:", styling$continue))))
                                    ))
        }
        observeEvent(input$admin_dismiss, {
            ## dismiss the admin modal and unpause the video
            editing$active <- NULL
            removeModal()
            do_video("pause")
        })
        output$save_file_button <- downloadHandler(
            filename = reactive(
                if (!is.null(rdata$dvw$meta$filename) && !is.na(rdata$dvw$meta$filename)) basename(rdata$dvw$meta$filename) else "myfile.dvw"
            ),
            content = function(file) {
                tryCatch(dv_write2(rdata$dvw, file = file),
                         error = function(e) {
                             rds_ok <- FALSE
                             if (!nzchar(Sys.getenv("SHINY_PORT"))) {
                                 ## this only makes sense if running locally, not deployed on a remote server
                                 ## if no port defined, assumed running under shiny within R, not under shiny server
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

        observeEvent(input$manual_code, {
            ok <- TRUE
            if (!is.null(input$manual_code)) {
                if (input$manual_code %in% c("*T", "aT")) {
                    rdata$dvw$plays2 <- bind_rows(rdata$dvw$plays2, make_plays2(input$manual_code, game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw))
                } else if (input$manual_code %in% c("*p", "ap")) {
                    game_state$point_won_by <- substr(input$manual_code, 1, 1)
                    rdata$dvw$plays2 <- bind_rows(rdata$dvw$plays2, make_plays2(character(), game_state = game_state, rally_ended = TRUE, dvw = rdata$dvw))
                    do_rally_end_things()
                } else if (input$manual_code %in% c("*C", "aC")) {
                    ## substitution
                    if (input$manual_code %eq% "*C") {
                        p_out <- as.numeric(input$ht_sub_out)
                        p_in <- as.numeric(input$ht_sub_in)
                    } else {
                        p_out <- as.numeric(input$vt_sub_out)
                        p_in <- as.numeric(input$vt_sub_in)
                    }
                    if (length(p_out) == 1 && length(p_in) == 1) {
                        game_state <- game_state_make_substitution(game_state, team = substr(input$manual_code, 1, 1), player_out = p_out, player_in = p_in, dvw = rdata$dvw)
                        rdata$dvw$plays2 <- bind_rows(rdata$dvw$plays2, make_plays2(paste0(substr(input$manual_code, 1, 1), "C", p_out, ".", p_in), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw))
                    } else {
                        ## players in/out not selected, ignore
                        ok <- FALSE
                    }
                }
            }
            if (ok) {
                editing$active <- NULL
                removeModal()
                do_video("pause")
            }
        })
    }
}
