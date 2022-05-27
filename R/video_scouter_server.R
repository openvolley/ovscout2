ov_scouter_server <- function(app_data) {
    function(input, output, session) {
        debug <- 1L

        shiny::addResourcePath("css", system.file("extdata/css", package = "ovscout2"))
        shiny::addResourcePath("js", system.file("extdata/js", package = "ovscout2"))

        plays_cols_to_show <- c("error_icon", "video_time", "set_number", "code", "Score") ##"home_setter_position", "visiting_setter_position", "is_skill"
        plays_cols_renames <- c(Set = "set_number")##, hs = "home_setter_position", as = "visiting_setter_position")

        if (is.null(app_data$dvw$meta$match$regulation)) stop("dvw does not have regulation information")
        app_data$is_beach <- is_beach(app_data$dvw)

        atbl <- app_data$dvw$meta$attacks
        atbl <- bind_cols(atbl[, setdiff(names(atbl), c("start_x", "start_y"))], setNames(dv_index2xy(atbl$start_coordinate), c("start_x", "start_y")))
        app_data$dvw$meta$attacks <- atbl

        rdata <- reactiveValues(dvw = app_data$dvw)

        pseq <- if (app_data$is_beach) 1:2 else 1:6

        ## initialize the game state
        rally_state <- reactiveVal("click or unpause the video to start")
        rally_codes <- reactiveVal(empty_rally_codes)
        if ("game_state" %in% names(app_data$dvw) && !is.null(app_data$dvw$game_state)) {
            ## saved as an rds, so re-use this
            temp <- app_data$dvw$game_state
        } else {
            temp <- as.list(tail(app_data$dvw$plays2, 1))
        }
        if (!"serving" %in% names(temp) || is.na(temp$serving)) temp$serving <- "*" ## default to home team serving - maybe allow this as a parm to ov_scouter (maybe TODO)
        temp$current_team <- temp$serving
        temp$start_x <- temp$start_y <- temp$end_x <- temp$end_y <- NA_real_
        temp$current_time_uuid <- ""
        ## liberos
        if (!"ht_lib1" %in% names(temp)) temp$ht_lib1 <- NA_character_
        if (!"ht_lib2" %in% names(temp)) temp$ht_lib2 <- NA_character_
        if (!"vt_lib1" %in% names(temp)) temp$vt_lib1 <- NA_character_
        if (!"vt_lib2" %in% names(temp)) temp$vt_lib2 <- NA_character_
        ## initial scores
        ## if we haven't played any points yet, these will be NA
        if (nrow(app_data$dvw$plays2) < 1 || !any(grepl("^[a\\*]p[[:digit:]]", app_data$dvw$plays2$code))) {
            temp$home_score_start_of_point <- temp$visiting_score_start_of_point <- 0L
        }
        ## get the correct set_number
        nsets <- grep("^\\*\\*[[:digit:]]set", app_data$dvw$plays2$code)
        temp$set_number <- length(nsets) + 1L
        if (!"home_team_end" %in% names(temp)) temp$home_team_end <- "upper" ## home team end defaults to upper
        game_state <- do.call(reactiveValues, temp)

        ## court inset showing rotation and team lists
        court_inset <- callModule(mod_courtrot2, id = "courtrot", rdata = rdata, game_state = game_state, rally_codes = rally_codes, rally_state = rally_state, styling = app_data$styling, with_ball_coords = app_data$ball_path)
        ## force a team rotation
        rotate_teams <- reactive(court_inset$rt)
        observe({
            rtn <- rotate_teams()
            poscode <- c()
            if (rtn$home > 0) {
                game_state$home_setter_position <- rotpos(game_state$home_setter_position, n = length(pseq))
                temp <- rotvec(as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)]))
                for (i in pseq) game_state[[paste0("home_p", i)]] <- temp[i]
                poscode <- c(poscode, paste0("*z", game_state$home_setter_position))
                rtn$home <- 0L
            }
            if (rtn$visiting > 0) {
                game_state$visiting_setter_position <- rotpos(game_state$visiting_setter_position, n = length(pseq))
                temp <- rotvec(as.numeric(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)]))
                for (i in pseq) game_state[[paste0("visiting_p", i)]] <- temp[i]
                poscode <- c(poscode, paste0("az", game_state$visiting_setter_position))
                rtn$visiting <- 0L
            }
            if (length(poscode)) rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(poscode, game_state = game_state, dvw = rdata$dvw)))
        })

        teamslists <- callModule(mod_teamslists, id = "teamslists", rdata = rdata)
        detection_ref <- reactiveVal({
            if (!is.null(app_data$court_ref)) app_data$court_ref else NULL
        })
        courtref <- callModule(mod_courtref, id = "courtref", rdata = rdata, app_data = app_data, detection_ref = detection_ref, styling = app_data$styling)
        if (app_data$scoreboard) {
            tsc_mod <- callModule(mod_teamscores, id = "tsc", game_state = game_state, rdata = rdata)
        }
        ## court module clicking not used here yet
        ##accept_ball_coords <- court_inset$accept_ball_coords ## the "accept" button
        ##observe({
        ##    if (nrow(court_inset$click_points$queue) > 1) {## && !is.null(playslist_mod$current_row()) && !is.na(playslist_mod$current_row())) {
        ##        js_show2("courtrot-validate_ball_coords")
        ##        js_show2("courtrot-cancel_ball_coords")
        ##    } else {
        ##        js_hide2("courtrot-validate_ball_coords")
        ##        js_hide2("courtrot-cancel_ball_coords")
        ##    }
        ##})
        ##observeEvent(accept_ball_coords(), {
        ##    if (accept_ball_coords() > 0) { ## ignore the initial triggering of this on app startup
        ##        warning("ball coords not implemented here yet")
        ##    }
        ##    ## and clear the clicked coordinates queue
        ##    court_inset$clear_click_queue()
        ##})

        playslist_mod <- callModule(mod_playslist, id = "playslist", rdata = rdata, plays_cols_to_show = plays_cols_to_show, plays_cols_renames = plays_cols_renames)

        video_state <- reactiveValues(paused = TRUE, muted = TRUE) ## starts paused and muted
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
            getel <- "vidplayer"
            myargs <- list(...)
            if (what == "pause") {
                dojs(paste0(getel, ".pause();"))
                video_state$paused <- TRUE
                NULL
            } else if (what == "play") {
                dojs(paste0(getel, ".play();"))
                video_state$paused <- FALSE
                if (rally_state() == "click or unpause the video to start") rally_state("click serve start")
            } else if (what == "toggle_pause") {
                ## careful using this, because there are situations where we don't want to allow unpausing - see deal_with_pause()
                if (video_state$paused) {
                    dojs(paste0(getel, ".play();"))
                    video_state$paused <- FALSE
                    if (rally_state() == "click or unpause the video to start") rally_state("click serve start")
                } else {
                    dojs(paste0(getel, ".pause();"))
                    video_state$paused <- TRUE
                }
                NULL
            } else if (what == "get_time") {
                dojs(paste0("Shiny.setInputValue('video_time', ", getel, ".currentTime())"))
            } else if (what == "get_time_fid") {
                dojs(paste0("Shiny.setInputValue('video_time', ", getel, ".currentTime() + '&", myargs[[1]], "')"))
            } else if (what == "set_time") {
                dojs(paste0(getel, ".currentTime(", myargs[[1]], ");"))
            } else if (what == "set_current_video_time") {
                dojs(paste0("Shiny.setInputValue('set_current_video_time', ", getel, ".currentTime() + '&", myargs[1], "&' + new Date().getTime())"))
            } else if (what == "tag_current_video_time") {
                dojs(paste0("Shiny.setInputValue('tag_current_video_time', ", getel, ".currentTime() + '&", myargs[1], "')"))
            } else if (what == "rew") {
                dojs(paste0(getel, ".currentTime(", getel, ".currentTime() - ", myargs[[1]], ");"))
            } else if (what == "ff") {
                dojs(paste0(getel, ".currentTime(", getel, ".currentTime() + ", myargs[[1]], ");"))
            } else if (what == "playback_rate") {
                dojs(paste0(getel, ".playbackRate(", myargs[[1]], ");"))
            } else if (what == "get_volume") {
                dojs(paste0(getel, ".volume();"))
            } else if (what == "set_volume") {
                dojs(paste0(getel, ".volume(", myargs[[1]], ");"))
            } else if (what == "mute") {
                dojs(paste0(getel, ".muted(true);"))
                video_state$muted <- TRUE
            } else if (what == "unmute") {
                dojs(paste0(getel, ".muted(false);"))
                video_state$muted <- FALSE
            } else if (what == "muted") {
                video_state$muted
            } else if (what == "toggle_mute") {
                if (video_state$muted) {
                    shiny::updateActionButton(session, "video_toggle_mute", label = "Mute")
                    do_video("unmute")
                } else {
                    shiny::updateActionButton(session, "video_toggle_mute", label = "Unmute")
                    do_video("mute")
                }
            } else {
                NULL
            }
        }

        observeEvent(input$playback_rate, {
            if (!is.null(input$playback_rate)) do_video("playback_rate", input$playback_rate)
        })

        ## match, team, and lineup data editing
        match_data_edit_mod <- callModule(mod_match_data_edit, id = "match_data_editor", rdata = rdata, editing = editing, styling = app_data$styling)
        team_edit_mod <- callModule(mod_team_edit, id = "team_editor", rdata = rdata, editing = editing, styling = app_data$styling)
        team_select_mod <- callModule(mod_team_select, id = "team_selector", rdata = rdata, editing = editing, styling = app_data$styling)
        lineup_edit_mod <- callModule(mod_lineup_edit, id = "lineup_editor", rdata = rdata, game_state = game_state, editing = editing, video_state = video_state, styling = app_data$styling)

        observeEvent(input$edit_cancel, {
            if (!is.null(editing$active) && editing$active %in% "teams") {
                team_edit_mod$htdata_edit(NULL)
                team_edit_mod$vtdata_edit(NULL)
            }
            if (!is.null(editing$active) && editing$active %in% "select_teams") {
                team_select_mod$htdata_select(NULL)
                team_select_mod$vtdata_select(NULL)
            }
            editing$active <- NULL
            removeModal()
        })

        observeEvent(input$edit_commit, {
            do_edit_commit()
        })

        do_edit_commit <- function() {
            if (!is.null(editing$active)) {
                if (editing$active %in% c("edit", "insert above", "insert below")) {
                    ## user has changed EITHER input$code_entry or used the code_entry_guide
                    if (nzchar(input$code_entry)) {
                        newcode <- input$code_entry
                        if (grepl("^[a\\*]?[[:digit:]][[:digit:]][SREABDF]", newcode)) {
                            ## this is a skill code
                            ## we are ignoring these, at least for now
                            ##newcode <- sub("~+$", "", ov_code_interpret(input$code_entry))
                            warning("ignoring skill code: ", newcode)
                            newcode <- NULL
                        }
                    } else {
                        ## build code from code_entry_guide elements
                        newcode1 <- lapply(seq_len(nrow(code_bits_tbl)), function(bi) {
                            val <- input[[paste0("code_entry_", code_bits_tbl$bit[bi])]]
                            if (is.null(val)) val <- ""
                            wid <- code_bits_tbl$width[bi]
                            if (nchar(val) < wid) val <- str_pad(val, wid, side = "right", pad = "~")
                            val
                        })
                        newcode <- sub("~+$", "", paste(newcode1, collapse = ""))## trim trailing ~'s
                    }
                    if (editing$active %eq% "insert below" && !is.null(newcode)) {
                        handle_manual_code(newcode)
                    } else if (editing$active %in% c("edit", "insert above")) {
                        ## not handled yet
                    }
                } else {
                    changed <- code_make_change(editing$active, game_state = game_state, dvw = rdata$dvw, input = input,
                                                htdata_edit = team_edit_mod$htdata_edit(), vtdata_edit = team_edit_mod$vtdata_edit(),
                                                htdata_select = team_select_mod$htdata_select(),vtdata_select = team_select_mod$vtdata_select())
                    rdata$dvw <- changed$dvw
                    if (changed$do_reparse) {
                        ## we don't need to reparse (??), but (might) need to adjust game_state, e.g. if we've changed lineups
                        temp <- as.list(tail(rdata$dvw$plays2, 1))
                        for (nm in intersect(c("home_setter_position", "visiting_setter_position", paste0("home_p", 1:6), paste0("visiting_p", 1:6)), names(temp))) game_state[[nm]] <- temp[[nm]]
                    }
                    ## need to ensure that lineups, rosters etc are valid before restarting, especially if we have started with an incomplete dvw
                    ## will happen in deal_with_pause below
                }
                editing$active <- NULL
                removeModal()
                deal_with_pause()
            }
        }

        meta_is_valid <- reactiveVal(TRUE)
        observe({
            notnn <- function(z) !is.null(z) && !is.na(z)
            ## check rosters
            ## TODO better
            rosters_ok <- !is.null(rdata$dvw$meta$players_h) && length(na.omit(rdata$dvw$meta$players_h$number)) >= length(pseq) &&
                !is.null(rdata$dvw$meta$players_v) && length(na.omit(rdata$dvw$meta$players_v$number)) >= length(pseq)
            ## check lineups
            lineups_ok <- TRUE
            ## need to see non-NA, non-NULL entries in game_state
            for (pp in pseq) lineups_ok <- lineups_ok && notnn(game_state[[paste0("home_p", pp)]]) && notnn(game_state[[paste0("visiting_p", pp)]])
            if (!app_data$is_beach) lineups_ok <- lineups_ok && notnn(game_state$home_setter_position) && notnn(game_state$visiting_setter_position)
            ## also need to see >LUp lines for this set in plays2
            temp_set_idx <- rdata$dvw$plays2$set_number %eq% game_state$set_number
            lineups_ok <- lineups_ok && (sum(temp_set_idx & grepl("^\\*P[[:digit:]]+>LUp", rdata$dvw$plays2$code), na.rm = TRUE) >= 1)
            lineups_ok <- lineups_ok && (sum(temp_set_idx & grepl("^\\*z[[:digit:]]+>LUp", rdata$dvw$plays2$code), na.rm = TRUE) >= 1)
            lineups_ok <- lineups_ok && (sum(temp_set_idx & grepl("^aP[[:digit:]]+>LUp", rdata$dvw$plays2$code), na.rm = TRUE) >= 1)
            lineups_ok <- lineups_ok && (sum(temp_set_idx & grepl("^az[[:digit:]]+>LUp", rdata$dvw$plays2$code), na.rm = TRUE) >= 1)
            ## check courtref
            courtref_ok <- !is.null(detection_ref()$court_ref)
            ok <- lineups_ok && rosters_ok && courtref_ok
            meta_is_valid(ok)
            output$problem_ui <- renderUI({
                if (!ok) {
                    tags$div(class = "alert alert-info",
                             tags$h2("Information needed"),
                             tags$ul(
                                      if (!courtref_ok) tags$li("Use the 'Court reference' button to define the court reference."),
                                      if (!rosters_ok) tags$li("Use the 'Edit teams' button to enter the team rosters."),
                                      if (!lineups_ok) tags$li(paste0("Use the 'Edit lineups' to enter starting lineups", if (!is.null(game_state$set_number) && !is.na(game_state$set_number)) paste0(" for set ", game_state$set_number), "."))
                                  ),
                             tags$hr(),
                             tags$p("Scouting cannot start until this information has been entered.")
                             )
                } else {
                    NULL
                }
            })
        })

        ## exteral video control buttons
        ##observeEvent(input$pause_trigger, deal_with_pause())
        observeEvent(input$video_pause, deal_with_pause())
        observeEvent(input$video_rew_10, do_video("rew", 10))
        observeEvent(input$video_ff_10, do_video("ff", 10))
        observeEvent(input$video_volume, if (!is.null(input$video_volume)) do_video("set_volume", input$video_volume))
        observeEvent(input$video_toggle_mute, do_video("toggle_mute"))

        deal_with_pause <- function(show_modal = TRUE) {
            ## don't allow unpause if we have a scouting modal shown
            if (isTRUE(scout_modal_active())) {
                ## but do allow pause, if somehow it isn't already
                do_video("pause")
            } else if (meta_is_valid()) {
                ## don't allow unpause if the lineups are not valid, else it'll crash
                if (video_state$paused) {
                    ## we are paused
                    if (is.null(editing$active)) {
                        ## just unpause
                        do_video("play")
                    } else if (editing$active %eq% "admin") {
                        ## otherwise, and only if we have the admin modal showing, dismiss it and unpause
                        editing$active <- NULL
                        removeModal()
                        do_video("play")
                    }
                } else {
                    ## not paused, so pause and show admin modal
                    do_video("pause")
                    if (show_modal) {
                        editing$active <- "admin"
                        show_admin_modal()
                    }
                }
            }
        }

        ## input$cmd reflects keypress events
        ## input$controlkey reflects keydown, use for keys that might not get detected by keypress but do by keydown. This returns a less convenient format, so use keypress for standard alpha/numeric/punct keys
        observeEvent(input$cmd, {
            mycmd <- NULL
            if (!is.null(input$cmd)) {
                temp <- strsplit(input$cmd, "@")[[1]]
                ## elements are keyid element_class element_id cursor_position field_length time
                mycmd <- temp[1]
                myclass <- temp[2]
                if (!is.null(myclass) && nzchar(myclass) && myclass %in% c("form-control")) {
                    ## don't process these - they are e.g. key events in DT filter boxes
                    mycmd <- NULL
                }
            }
            if (!is.null(mycmd)) {
                ky <- intToUtf8(as.numeric(mycmd))
                if (ky %in% c("z", "Z")) {
                    ## temporarily hide the modal, so the video can be seen
                    ## but only for the admin, lineup modal or the ones that pop up during the rally, not the editing modals for teams or rosters
                    if (is.null(editing$active) || editing$active %in% c("admin", "change starting lineup")) dojs("$('#shiny-modal-wrapper').hide(); $('.modal-backdrop').hide();")
                } else if (ky %in% c("q", "Q", "0")) {
                    ## only accept this if we are not editing, or it's the admin modal being shown
                    if (is.null(editing$active) || editing$active %eq% "admin") {
                        ## video pause/unpause
                        ## Q (uppercase) does just pause, with no admin modal
                        deal_with_pause(show_modal = ky != "Q")
                    }
                } else if (is.null(editing$active) && !courtref$active()) {
                    ## none of these should be allowed to happen if we are e.g. editing lineups or teams or doing the court ref
                    if (ky %in% c("g", "G", "#")) {
                        ## video go to currently-selected event
                        ## NB this is the current game_state time, not the time of the currently-selected event
                        ## TO FIX
                        vt <- game_state$video_time
                        if (is.null(vt) || is.na(vt)) {
                            vt <- max(rdata$dvw$plays$video_time, na.rm = TRUE)
                        }
                        if (!is.null(vt) && !is.na(vt)) {
                            if (debug > 1) cat("jumping to video time: ", vt, "\n")
                            do_video("set_time", vt)
                        }
                    } else if (ky %in% c("u", "U")) {
                        ## undo
                        do_undo()
                    } else if (ky %in% strsplit("nm13jhl;46$^b,79", "")[[1]]) {
                        if (is.null(editing$active)) {
                            ## video forward/backward nav
                            ## same as for other ovscout interface, although the fine control is not needed here?
                            vidcmd <- if (ky %in% strsplit("1nhj4$b7", "")[[1]]) "rew" else "ff"
                            dur <- if (ky %in% strsplit("h$;^", "")[[1]]) 10 else if (ky %in% strsplit("nm13", "")[[1]]) 0.1 else if (ky %in% strsplit("b7,9", "")[[1]]) 1/30 else 2
                            do_video(vidcmd, dur)
                        }
                    }
                }
            }
        })
        observeEvent(input$controlkey, {
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
                        ## NOTE that we get the ascii code for the base key (i.e. upper-case letter, or number) AND the modifier
                        ## so for "#" we'd get ky == utf8ToInt("3") (which is 51) plus mycmd[3] == "true" (shift)
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
                                courtref$active(FALSE)
                                removeModal()
                                if (do_unpause) do_video("play")
                            }
                        } else if (ky %eq% 13) {
                            ## enter
                            ## if editing, treat as update
                            if (!is.null(editing$active) && !editing$active %eq% "teams") {
                                do_edit_commit()
                            }
                            ## but not for team editing, because pressing enter in the DT fires this too
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
                            ## re-show the modal after temporarily hiding
                            dojs("$('#shiny-modal-wrapper').show(); $('.modal-backdrop').show();")
                        }
                    }
                }
            }
        })

        ## video overlay
        output$show_courtref_ui <- renderUI(if (!is.null(detection_ref()$court_ref)) checkboxInput("show_courtref", "Show court reference?", value = FALSE) else NULL)

        overlay_points <- reactiveVal(NULL)
        observe({
            output$video_overlay <- renderPlot({
                ## test - red diagonal line across the overlay plot
                ##ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes_string("x", "y")) + geom_path(color = "red") + gg_tight
                ## need to plot SOMETHING else we don't get correct coordinates back
                ##this <- selected_event()
                p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes_string("x", "y")) + gg_tight
                if (isTRUE(input$show_courtref) && !is.null(courtref$crox())) {
                    p <- p + geom_segment(data = courtref$crox()$courtxy, aes_string(x = "image_x", y = "image_y", xend = "xend", yend = "yend"), color = app_data$styling$court_lines_colour)
                }
                if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                    ixy <- setNames(crt_to_vid(overlay_points()), c("x", "y"))
                    p <- p + geom_point(data = ixy, fill = "dodgerblue", pch = 21, col = "white", size = 6)
                }
                p
            }, bg = "transparent", width = vo_width(), height = vo_height())
        })
        vid_to_crt <- function(obj) {
            courtxy <- data.frame(x = rep(NA_real_, length(obj$x)), y = rep(NA_real_, length(obj$x)))
            if (!is.null(detection_ref()$court_ref)) {
                if (length(obj$x) > 0) courtxy <- ovideo::ov_transform_points(obj$x, obj$y, ref = detection_ref()$court_ref, direction = "to_court")
            }
            courtxy
        }
        crt_to_vid <- function(obj) {
            imagexy <- data.frame(image_x = rep(NA_real_, length(obj$x)), image_y = rep(NA_real_, length(obj$x)))
            if (!is.null(detection_ref()$court_ref)) {
                if (length(obj$x) > 0) imagexy <- setNames(ovideo::ov_transform_points(obj$x, obj$y, ref = detection_ref()$court_ref, direction = "to_image"), c("image_x", "image_y"))
            }
            imagexy
        }

        courtxy <- reactiveVal(list(x = NA_real_, y = NA_real_))
        loop_trigger <- reactiveVal(0L)
        observeEvent(input$video_click, {
            ## when video clicked, get the corresponding video time and trigger the loop
            flash_screen() ## visual indicator that click has registered
            time_uuid <- uuid()
            game_state$current_time_uuid <- time_uuid
            do_video("get_time_fid", time_uuid) ## make asynchronous request
            courtxy(vid_to_crt(input$video_click))
            loop_trigger(loop_trigger() + 1L)
            ## TODO MAYBE also propagate the click to elements below the overlay?
        })
        observeEvent(court_inset$click(), {
            ## when video clicked, get the corresponding video time and trigger the loop
            flash_screen() ## visual indicator that click has registered
            time_uuid <- uuid()
            game_state$current_time_uuid <- time_uuid
            do_video("get_time_fid", time_uuid) ## make asynchronous request
            courtxy(court_inset$click())
            loop_trigger(loop_trigger() + 1L)
        })

        ## video times are a pain, because we get asynchronous replies from the browser via input$video_time
        video_times <- list()
        observeEvent(input$video_time, {
            ## when a time comes in, stash it under its uuid
            temp <- input$video_time
            this_uuid <- sub(".*&", "", temp)
            if (nzchar(this_uuid)) video_times[[this_uuid]] <<- round(as.numeric(sub("&.+", "", temp)), 2) ## video times to 2 dec places
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
            if (isTRUE(app_data$review_pane)) show_review_pane()
        }
        remove_scout_modal <- function() {
            scout_modal_active(FALSE)
            removeModal()
            if (isTRUE(app_data$review_pane)) hide_review_pane()
        }

        ## single click the video to register a tag location, or starting ball coordinates
        observeEvent(loop_trigger(), {
            if (loop_trigger() > 0) {
                if (rally_state() == "click or unpause the video to start") {
                    if (meta_is_valid()) {
                        do_video("play")
                        rally_state("click serve start")
                    }
                } else if (rally_state() == "click serve start") {
                    ## click was the serve position
                    game_state$start_x <- courtxy()$x[1]
                    game_state$start_y <- courtxy()$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy())
                    ## add placeholder serve code, will get updated on next click
                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    ## serve type should have been selected in the preselect
                    st <- if (!is.null(input$serve_preselect_type)) input$serve_preselect_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    ## time probably won't have resolved yet, so add it after next click
                    rally_codes(bind_rows(rally_codes(), code_trow(team = game_state$serving, pnum = sp, skill = "S", tempo = st, sz = sz, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                    game_state$current_team <- other(game_state$serving)
                    rally_state("click serve end")
                } else if (rally_state() == "click serve end") {
                    do_video("pause")
                    ## click was the end-of-serve position, either error or reception
                    game_state$end_x <- courtxy()$x[1]
                    game_state$end_y <- courtxy()$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    overlay_points(rbind(overlay_points(), courtxy()))
                    ## pop up to find either serve error, or passing player
                    ## passing player options
                    ## game_state$current_team here is the receiving team
                    pass_pl_opts <- guess_pass_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)
                    names(pass_pl_opts$choices) <- player_nums_to(pass_pl_opts$choices, team = game_state$current_team, dvw = rdata$dvw)
                    pass_pl_opts$choices <- c(pass_pl_opts$choices, Unknown = "Unknown")

                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    chc <- app_data$options$skill_tempo_map %>% dplyr::filter(.data$skill == "Serve") %>% mutate(tempo = sub(" serve", "", .data$tempo))
                    chc <- setNames(chc$tempo_code, chc$tempo)
                    serve_type_buttons <- make_fat_radio_buttons(choices = chc, selected = input$serve_preselect_type, input_var = "serve_type")
                    guess_was_err <- NA
                    if (game_state$end_x < 0.5) {
                        ## out left or right
                        guess_was_err <- if ((game_state$serving %eq% "*" && game_state$home_team_end %eq% "lower") || (game_state$serving %eq% "a" && game_state$home_team_end %eq% "upper")) "=L" else "=R"
                    } else if (game_state$end_x > 3.5) {
                        ## out left or right
                        guess_was_err <- if ((game_state$serving %eq% "*" && game_state$home_team_end %eq% "lower") || (game_state$serving %eq% "a" && game_state$home_team_end %eq% "upper")) "=R" else "=L"
                    } else if (game_state$end_y < 0.5 || game_state$end_y > 6.5) {
                        ## out long
                        guess_was_err <- "=O"
                    } else if ((((game_state$serving %eq% "*" && game_state$home_team_end %eq% "lower") || (game_state$serving %eq% "a" && game_state$home_team_end %eq% "upper")) && (game_state$end_y > 3.3 && game_state$end_y <= 3.55)) ||
                               (((game_state$serving %eq% "a" && game_state$home_team_end %eq% "lower") || (game_state$serving %eq% "*" && game_state$home_team_end %eq% "upper")) && (game_state$end_y < 3.7 && game_state$end_y >= 3.45))) {
                        ## ball end point is near the net, either an error into the net or hit the net and dropped over without being an error
                        ## if it's on the serving team's side, or only just over, assume it was an error
                        guess_was_err <- "=N"
                    }
                    ## TODO possibly also guess foot fault, although that will be confusing because the (legal) serve contact might be inside the baseline
                    ## we pre-select either the passer, or the error type, depending on whether we thought it was an error or not
                    serve_outcome_initial_buttons <- make_fat_radio_buttons(choices = c("Serve error" = "=", "Reception error (serve ace)" = "S#", "Reception in play" = "R~"), input_var = "serve_initial_outcome", selected = if (!is.na(guess_was_err)) "=" else "R~")
                    serve_error_type_buttons <- make_fat_radio_buttons(choices = c("In net" = "=N", "Foot fault/referee call" = "=Z", "Out long" = "=O", "Out left" = "=L", "Out right" = "=R"), selected = if (!is.na(guess_was_err)) guess_was_err else NA, input_var = "serve_error_type", as_radio = "blankable")
                    passer_buttons <- make_fat_radio_buttons(choices = pass_pl_opts$choices, selected = pass_pl_opts$selected, input_var = "pass_player")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                                   tags$p(tags$strong("Serve type:")),
                                                   do.call(fixedRow, lapply(serve_type_buttons, function(but) column(2, but))),
                                                   tags$hr(),
                                                   tags$div("AND"),
                                                   tags$br(),
                                                   do.call(fixedRow, lapply(serve_outcome_initial_buttons, function(but) column(2, but))),
                                                   tags$hr(),
                                                   tags$div(id = "serve_error_type_ui", style = if (is.na(guess_was_err)) "display:none;", tags$p(tags$strong("Error type:")),
                                                            do.call(fixedRow, lapply(serve_error_type_buttons, function(but) column(2, but)))),
                                                   tags$div(id = "passers_ui", style = if (!is.na(guess_was_err)) "display:none;", tags$p(tags$strong("Select passer:")),
                                                            do.call(fixedRow, lapply(passer_buttons, function(but) column(1, but)))),
                                                   tags$hr(),
                                                   fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", class = "cancel fatradio")),
                                                            column(2, offset = 8, actionButton("assign_serve_outcome", "Continue", class = "continue fatradio")))
                                                   ))
                } else if (rally_state() == "click second contact") {
                    ## set (play continues), setter dump, set error, P2 attack, or freeball over (by the receiving team)
                    ## or PR, dig/freeball dig by opposition
                    ## we get a clue if it's the receiving/digging team or their opposition by the side of the court that has been clicked
                    do_video("pause")
                    ## click was the set contact position, or the freeball start position
                    game_state$start_x <- courtxy()$x[1]
                    game_state$start_y <- courtxy()$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy())
                    ## popup
                    ## TODO maybe also setter call here
                    ## allow user to override auto-assigned reception quality
                    passq <- guess_pass_quality(game_state, dvw = rdata$dvw, home_end = game_state$home_team_end)
                    c2_pq_buttons <- make_fat_radio_buttons(choices = c(Overpass = "/", Poor = "-", OK = "!", Good = "+", Perfect = "#"), selected = passq, input_var = "c2_pq")
                    c2_buttons <- make_fat_radio_buttons(
                        choices = c(Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball<br />attack" = "P2", "Freeball over" = "F", "Reception error<br />(serve ace)" = "R=", ## rcv team actions
                                    "Opp. dig" = "aD", "Opp. dig error" = "aD=", "Opp. overpass attack" = "aPR"), ## opp actions
                        selected = "E", input_var = "c2")
                    if (app_data$is_beach) {
                        stop("setter for beach")
                        ## choose the player who didn't pass
                    }
                    soc <- get_setter(game_state)
                    sp <- c(sort(get_players(game_state, dvw = rdata$dvw)), sort(get_liberos(game_state, dvw = rdata$dvw)))
                    names(sp) <- player_nums_to(sp, team = game_state$current_team, dvw = rdata$dvw)
                    sp <- c(sp, Unknown = "Unknown")
                    setter_buttons <- make_fat_radio_buttons(choices = sp, selected = soc, input_var = "c2_player")
                    opp <- c(sort(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw)), sort(get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw)))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_buttons <- make_fat_radio_buttons(choices = opp, selected = NA, input_var = "c2_opp_player")
                    if (isTRUE(input$shiftkey)) {
                        ## accept set by setter on court, with no popup
                        esz <- as.character(dv_xy2subzone(game_state$start_x, game_state$start_y))
                        passq <- guess_pass_quality(game_state, dvw = rdata$dvw, home_end = game_state$home_team_end)
                        rc <- rally_codes()
                        rc$eval[rc$skill %eq% "R"] <- passq
                        ## find corresponding serve evaluation code
                        seval <- app_data$compound_table$code[app_data$compound_table$skill %eq% "S" & app_data$compound_table$compound_skill %eq% "R" & app_data$compound_table$compound_code %eq% passq]
                        if (nchar(seval) != 1) seval <- "~"
                        rc$eval[rc$skill %eq% "S"] <- seval
                        start_t <- retrieve_video_time(game_state$start_t)
                        rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = soc, skill = "E", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                        rally_state("click third contact")
                        do_video("play")
                    } else {
                        show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                                       do.call(fixedRow, c(list(column(2, tags$strong("Reception quality"))), lapply(c2_pq_buttons, function(but) column(1, but)))),
                                                       tags$br(), tags$hr(),
                                                       tags$p(tags$strong("Second contact:")),
                                                       do.call(fixedRow, lapply(c2_buttons[1:6], function(but) column(if (isTRUE(app_data$review_pane)) 1 else 2, but))),
                                                       tags$br(),
                                                       tags$div(id = "c2_more_ui", tags$p("by player"),
                                                                tags$br(),
                                                                do.call(fixedRow, lapply(setter_buttons, function(but) column(1, but))),
                                                                tags$br(),
                                                                tags$div("OR"),
                                                                tags$br(),
                                                                do.call(fixedRow, lapply(c2_buttons[7:9], function(but) column(2, but))),
                                                                tags$br(),
                                                                tags$p("by player"),
                                                                tags$br(),
                                                                do.call(fixedRow, lapply(opp_buttons, function(but) column(1, but)))),
                                                       tags$hr(),
                                                       fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", class = "cancel fatradio")),
                                                                column(2, offset = 8, actionButton("assign_c2", "Continue", class = "continue fatradio")))
                                                       ))
                    }
                } else if (rally_state() == "click third contact") {
                    ## attack, freeball over (by the setting team)
                    ## or dig/freeball dig by on overset, or PR
                    do_video("pause")
                    ## click was the attack contact position, or the freeball start position
                    game_state$start_x <- courtxy()$x[1]
                    game_state$start_y <- courtxy()$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy())
                    ## popup
                    ## figure current phase
                    if (nrow(rally_codes()) > 0) {
                        temp <- make_plays2(rally_codes(), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)
                        ph <- tail(temp$phase, 1)
                    } else {
                        ph <- NA_character_
                    }
                    ac <- guess_attack_code(game_state, dvw = rdata$dvw, home_end = game_state$home_team_end, opts = app_data$options)
                    if (!isTRUE(app_data$options$transition_sets) && ph %eq% "Transition") {
                        ac <- head(ac, if (isTRUE(app_data$review_pane)) 4 else 7) ## wow, we don't have a lot here if we need to leave room for the three below plus space for the attack review pane
                        ## if we aren't scouting transition sets, then this "third" contact could be a setter dump or second-ball attack
                        ac <- c(ac, if (!is.null(app_data$options$setter_dump_code)) app_data$options$setter_dump_code else "PP", if (!is.null(app_data$options$second_ball_attack_code)) app_data$options$second_ball_attack_code else "P2")
                    } else {
                        ac <- head(ac, if (isTRUE(app_data$review_pane)) 6 else 9)
                    }
                    ac <- c(ac, "Other attack")
                    ac <- c(setNames(ac, ac), "Freeball over" = "F")
                    if (!isTRUE(app_data$options$transition_sets)) ac <- c(ac, "Set error" = "E=")
                    c3_buttons <- make_fat_radio_buttons(choices = c(ac, c("Opp. dig" = "aD", "Opp. dig error" = "aD=", "Opp. overpass attack" = "aPR")), input_var = "c3")
                    attack_pl_opts <- guess_attack_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)
                    ap <- sort(attack_pl_opts$choices)
                    names(ap) <- player_nums_to(ap, team = game_state$current_team, dvw = rdata$dvw)
                    ap <- c(ap, Unknown = "Unknown")
                    ## since we have a freeball over option here, it could be done by a libero
                    libs <- sort(get_liberos(game_state, team = game_state$current_team, dvw = rdata$dvw))
                    ap <- c(ap, setNames(libs, player_nums_to(libs, team = game_state$current_team, dvw = rdata$dvw)))
                    attacker_buttons <- make_fat_radio_buttons(choices = ap, selected = attack_pl_opts$selected, input_var = "c3_player")
                    ## do we want to support "hole" block?
                    if (isTRUE(app_data$options$nblockers)) nblocker_buttons <- make_fat_radio_buttons(choices = c("No block" = 0, "Single block" = 1, "Double block" = 2, "Triple block" = 3), selected = if (!is.null(app_data$options$default_nblockers)) app_data$options$default_nblockers, input_var = "nblockers")
                    ## attack error, blocked, replay will be scouted on next entry
                    ## TODO other special codes ?
                    opp <- c(sort(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw)), sort(get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw)))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_player_buttons <- make_fat_radio_buttons(choices = opp, selected = NA, input_var = "c3_opp_player")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Attack or freeball over:")),
                                            do.call(fixedRow, lapply(c3_buttons[seq_along(ac)], function(but) column(1, but))),
                                            tags$br(), tags$p("by player"), tags$br(),
                                            do.call(fixedRow, lapply(attacker_buttons, function(but) column(1, but))),
                                            if (isTRUE(app_data$options$nblockers)) tags$div(tags$br(), "with", tags$br()),
                                            if (isTRUE(app_data$options$nblockers)) do.call(fixedRow, lapply(nblocker_buttons, function(but) column(2, but))),
                                            tags$br(), tags$hr(), tags$div("OR"), tags$br(),
                                            do.call(fixedRow, lapply(tail(c3_buttons, 3), function(but) column(2, but))),
                                            tags$br(), tags$p("by player"), tags$br(),
                                            do.call(fixedRow, lapply(opp_player_buttons, function(but) column(1, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", class = "cancel fatradio")),
                                                     column(2, offset = 8, actionButton("assign_c3", "Continue", class = "continue fatradio")))
                                            ))
                } else if (rally_state() == "click attack end point") {
                    ## dig, dig error (attack kill), attack error, blocked, blocked for replay, block touch (attack kill)
                    ## or block touch and play continues
                    ## allow attack kill with no dig error?
                    do_video("pause")
                    ## click was the dig or attack kill or error position
                    game_state$end_x <- courtxy()$x[1]
                    game_state$end_y <- courtxy()$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    overlay_points(courtxy())
                    ## popup
                    ## note that we can't currently cater for a block kill with cover-dig error (just scout as block kill without the dig error)
                    c1_buttons <- make_fat_radio_buttons(choices = c("Attack kill (without dig error)" = "A#", "Attack error" = "A=", "Blocked for reattack (play continues)" = "A!", "Dig" = "D", "Dig error (attack kill)" = "D=", "Block kill" = "B#", "Block fault" = "B/"), input_var = "c1") ## defaults to attack kill without dig error
                    ## TODO smarter guessing of that
                    ae_buttons <- make_fat_radio_buttons(choices = c("Out long" = "O", "Out side" = "S", "In net" = "N", "Net contact" = "I", Antenna = "A", "Other/referee cal" = "Z"), selected = NA, input_var = "attack_error_type")
                    ## Identify defending players
                    dig_pl_opts <- guess_dig_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)
                    digp <- dig_pl_opts$choices
                    names(digp) <- player_nums_to(digp, team = game_state$current_team, dvw = rdata$dvw)
                    digp <- c(digp, Unknown = "Unknown")
                    dig_player_buttons <- make_fat_radio_buttons(choices = digp, selected = dig_pl_opts$selected, input_var = "c1_def_player")
                    ## covering players (attacking team)
                    cover_pl_opts <- guess_cover_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)
                    coverp <- cover_pl_opts$choices
                    names(coverp) <- player_nums_to(coverp, team = other(game_state$current_team), dvw = rdata$dvw)
                    coverp <- c(coverp, Unknown = "Unknown")
                    cover_player_buttons <- make_fat_radio_buttons(choices = coverp, selected = cover_pl_opts$selected, input_var = "c1_cover_player")
                    ## blocking players
                    blockp <- get_players(game_state, team = game_state$current_team, dvw = rdata$dvw)
                    if (length(blockp) == 6) blockp <- blockp[2:4] ## front-row only
                    blockp <- sort(blockp)
                    names(blockp) <- player_nums_to(blockp, team = game_state$current_team, dvw = rdata$dvw)
                    blockp <- c(blockp, Unknown = "Unknown")
                    block_player_buttons <- make_fat_radio_buttons(choices = blockp, selected = NA, input_var = "c1_block_touch_player")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Attack outcome:")),
                                            do.call(fixedRow, lapply(c1_buttons[1:3], function(but) column(2, but))),
                                            tags$br(), tags$div(id = "ae_ui", style = "display:none;", do.call(fixedRow, lapply(ae_buttons, function(but) column(2, but)))),
                                            tags$div("OR", tags$strong("Defence outcome:")),
                                            do.call(fixedRow, lapply(c1_buttons[4:7], function(but) column(2, but))),
                                            tags$br(), tags$hr(),
                                            ## either dig players (defending team)
                                            tags$div(id = "c1_digp_ui", style = "display:none;", tags$p(tags$strong("by player")),
                                                     do.call(fixedRow, lapply(dig_player_buttons, function(but) column(1, but)))),
                                            ## or cover players (attacking team)
                                            tags$div(id = "c1_coverp_ui", style = "display:none;", tags$p(tags$strong("Cover dig by player")),
                                                     do.call(fixedRow, lapply(cover_player_buttons, function(but) column(1, but)))),
                                            tags$br(), tags$hr(), tags$div("WITH", tags$strong("Block touch"), "by player"), tags$br(),
                                            do.call(fixedRow, lapply(block_player_buttons, function(but) column(2, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", class = "cancel fatradio")),
                                                     column(2, offset = 8, actionButton("assign_c1", "Continue", class = "continue fatradio")))
                                            ))
                } else if (rally_state() == "click freeball end point") {
                    ## freeball dig, freeball dig error, freeball error (in theory could be blocked, blocked for replay, block touch (freeball kill))
                    do_video("pause")
                    ## click was the dig or freeball end
                    game_state$end_x <- courtxy()$x[1]
                    game_state$end_y <- courtxy()$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    overlay_points(courtxy())
                    ## popup
                    ## note that we can't currently cater for a block kill with cover-dig error (just scout as block kill without the dig error)
                    f1_buttons <- make_fat_radio_buttons(choices = c("Freeball over error" = "F=", "Freeball dig" = "FD", "Freeball dig error" = "FD="), selected = "FD", input_var = "f1")
                    ## Identify defending players
                    ## TODO use dedicated freeball-dig guessing?
                    dig_pl_opts <- guess_dig_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)
                    digp <- dig_pl_opts$choices
                    names(digp) <- player_nums_to(digp, team = game_state$current_team, dvw = rdata$dvw)
                    digp <- c(digp, Unknown = "Unknown")
                    dig_player_buttons <- make_fat_radio_buttons(choices = digp, selected = dig_pl_opts$selected, input_var = "f1_def_player")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Freeball outcome:")),
                                            do.call(fixedRow, lapply(f1_buttons, function(but) column(2, but))),
                                            tags$br(), tags$hr(),
                                            ## freeball dig players (defending team)
                                            tags$div(id = "f1_digp_ui", tags$p(tags$strong("by player")),
                                                     do.call(fixedRow, lapply(dig_player_buttons, function(but) column(1, but)))),
                                            tags$br(), tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", class = "cancel fatradio")),
                                                     column(2, offset = 8, actionButton("assign_f1", "Continue", class = "continue fatradio")))
                                            ))
                } else {
                    stop("unknown rally state: ", rally_state())
                }
            }
            if (debug > 0) {
                if (nrow(rally_codes())) {
                    cat("rally codes:\n")
                    print_rally_codes(rally_codes())
                    ##cat(str(rally_codes()))
                }
                cat("rally state: ", rally_state(), "\n")
            }
        })


        rally_ended <- function() {
            ## add rally codes to scout object now
            rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(rally_codes(), game_state = game_state, rally_ended = TRUE, dvw = rdata$dvw)))
            do_rally_end_things()
            ## check for end of set
            scores <- c(game_state$home_score_start_of_point, game_state$visiting_score_start_of_point)
            end_of_set <- if (app_data$is_beach) {
                              max(scores) >= 21 && abs(diff(scores)) >= 2
                          } else {
                              ((max(scores) >= 25 && game_state$set_number < 5) || (max(scores) >= 15 && game_state$set_number == 5)) && abs(diff(scores)) >= 2
                          }
            if (end_of_set) {
                show_scout_modal(
                    modalDialog(title = "End of set", easyClose = FALSE, footer = NULL,
                                paste0("Confirm end of set ", game_state$set_number, "?"),
                                tags$hr(),
                                fixedRow(column(2, actionButton("end_of_set_cancel", "Cancel", class = "cancel fatradio")),
                                         column(2, offset = 8, actionButton("end_of_set_confirm", "Confirm", class = "continue fatradio")))
                                ))
                do_video("pause")
                rally_state("confirm end of set")
            }
        }

        observe({
            if (is.null(input$serve_initial_outcome)) {
                js_hide2("serve_error_type_ui")
                js_hide2("passers_ui")
            } else if (input$serve_initial_outcome %eq% "=") {
                js_show2("serve_error_type_ui")
                js_hide2("passers_ui")
            } else {
                js_hide2("serve_error_type_ui")
                js_show2("passers_ui")
            }
        })

        observe({
            if (!is.null(input$c2) && input$c2 %eq% "R=") js_hide2("c2_more_ui") else js_show2("c2_more_ui")
        })

        observe({
            if (!is.null(input$c1)) {
                if (input$c1 %eq% "A!") {
                    js_show2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                } else if (input$c1 %in% c("D", "D=", "B#", "B/")) {
                    js_hide2("c1_coverp_ui")
                    js_show2("c1_digp_ui")
                } else {
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                }
                if (input$c1 %eq% "A=") js_show2("ae_ui") else js_hide2("ae_ui")
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
                rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(poscode, game_state = game_state, dvw = rdata$dvw)))
            }
            ## reset for next rally
            game_state$serving <- game_state$current_team <- game_state$point_won_by
            rally_codes(empty_rally_codes)
            game_state$start_x <- game_state$start_y <- game_state$end_x <- game_state$end_y <- NA_real_
            game_state$current_time_uuid <- ""
            game_state$point_won_by <- NA_character_
            rally_state("click serve start")
        }

        ## for the playslist table, convert the rally codes into plays2 rows, and build plays from plays2
        observe({
            temp_rally_plays2 <- if (nrow(rally_codes()) > 0) make_plays2(rally_codes(), game_state = game_state, dvw = rdata$dvw) else NULL
            ##print(dplyr::glimpse(temp_rally_plays2))
            ##print(temp_rally_plays2$phase)
            ##cat(str(rdata$dvw$plays2))
            rdata$dvw$plays <- plays2_to_plays(rp2(bind_rows(rdata$dvw$plays2, temp_rally_plays2)), dvw = rdata$dvw, evaluation_decoder = app_data$evaluation_decoder)
        })

        observeEvent(input$cancelrew, {
            do_video("rew", 3)
            do_video("play")
            remove_scout_modal()
        })

        observeEvent(input$cancel, {
            do_video("play")
            remove_scout_modal()
        })

        observeEvent(input$end_of_set_confirm, {
            game_state$set_number <- game_state$set_number + 1L ## should be incremented in this plays2 line
            rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(paste0("**", game_state$set_number - 1L, "set"), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
            game_state$home_score_start_of_point <- game_state$visiting_score_start_of_point <- 0L
            game_state$home_team_end <- other_end(game_state$home_team_end) ## TODO deal with 5th set
            ## TODO choose the correct serving team
            ## update match metadata
            rdata$dvw <- update_meta(rp2(rdata$dvw))
            remove_scout_modal()
            rally_state("click serve start")
        })
        observeEvent(input$end_of_set_cancel, {
            do_video("play")
            remove_scout_modal()
            rally_state("click serve start")
        })

        observeEvent(input$assign_serve_outcome, {
            if (is.null(input$serve_initial_outcome)) {
                ## wtf? should not happen
            } else {
                if (input$serve_initial_outcome %eq% "=") {
                    ## serve error
                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    serve_err_type <- if (!is.null(input$serve_error_type)) input$serve_error_type else "="
                    st <- if (!is.null(input$serve_type)) input$serve_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    remove_scout_modal()
                    special_code <- substr(serve_err_type, 2, 2)
                    if (special_code %eq% "N" && app_data$options$end_convention %eq% "actual") game_state$end_y <- 3.5 ## exactly on net
                    esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
                    rc <- rally_codes()
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, eval = "=", ez = esz[1], esz = esz[2], special = if (nzchar(special_code)) special_code else "~", t = retrieve_video_time(game_state$start_t), end_x = game_state$end_x, end_y = game_state$end_y)
                    }
                    rally_codes(rc)
                    game_state$point_won_by <- other(game_state$serving)
                    rally_ended()
                } else if (input$serve_initial_outcome %eq% "S#") {
                    ## serve ace
                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    pp <- if (!is.null(input$pass_player)) input$pass_player else 0L
                    st <- if (!is.null(input$serve_type)) input$serve_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    remove_scout_modal()
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
                    start_t <- retrieve_video_time(game_state$start_t)
                    end_t <- retrieve_video_time(game_state$end_t)
                    rc <- rally_codes()
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) {
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, eval = "#", ez = esz[1], esz = esz[2], t = start_t, end_x = game_state$end_x, end_y = game_state$end_y)
                    }
                    rally_codes(bind_rows(rc, code_trow(team = other(game_state$serving), pnum = pp, skill = "R", eval = "=", tempo = st, sz = sz, ez = esz[1], esz = esz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))

                    game_state$current_team <- game_state$serving
                    game_state$point_won_by <- game_state$serving
                    rally_ended()
                } else {
                    ## reception in play
                    sp <- if (!is.null(input$serve_preselect_player)) input$serve_preselect_player else if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    st <- if (!is.null(input$serve_type)) input$serve_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    pp <- if (!is.null(input$pass_player)) input$pass_player else 0L
                    remove_scout_modal()
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
                    start_t <- retrieve_video_time(game_state$start_t)
                    end_t <- retrieve_video_time(game_state$end_t)
                    rc <- rally_codes()
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, ez = esz[1], esz = esz[2], t = start_t, end_x = game_state$end_x, end_y = game_state$end_y)
                    }
                    rally_codes(bind_rows(rc, code_trow(team = other(game_state$serving), pnum = pp, skill = "R", tempo = st, sz = sz, ez = esz[1], esz = esz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                    rally_state("click second contact")
                    do_video("rew", app_data$play_overlap)
                }
                if (rally_state() != "confirm end of set") do_video("play")
            }
        })

        observeEvent(input$assign_c1, {
            ## possible values for input$c1 are currently: A#, A=, A!, D, D=, B/, B#
            ## A#, A=, D, D= can be preceded by a block touch (but not B#, B/ and A!). A= is unlikely but theoretically possible
            mid_xy <- c(NA_real_, NA_real_)
            if (!is.null(input$c1_block_touch_player)) {
                if (input$c1 %in% c("A#", "A=", "D", "D=")) {
                    beval <- if (input$c1 %eq% "A#") "=" else app_data$default_scouting_table$evaluation_code[app_data$default_scouting_table$skill == "B"]
                    rc <- rally_codes()
                    Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                    mid_xy <- infer_mid_coords(game_state = game_state)
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = input$c1_block_touch_player, skill = "B", eval = beval, tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table))) ## TODO x,y?
                }
            }
            esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
            if (input$c1 %in% c("A#", "A=")) {
                ##end_t <- retrieve_video_time(game_state$end_t)
                eval <- substr(input$c1, 2, 2)
                ## find the attack, should be either the previous skill, or one previous to that with a block in between
                rc <- rally_codes()
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                if (!is.na(Aidx)) {
                    rc$ez[Aidx] <- esz[1]
                    rc$esz[Aidx] <- esz[2]
                    rc$end_x[Aidx] <- game_state$end_x
                    rc$end_y[Aidx] <- game_state$end_y
                    rc$mid_x[Aidx] <- mid_xy[1]
                    rc$mid_y[Aidx] <- mid_xy[2]
                    rc$eval[Aidx] <- eval
                    if (!is.null(input$attack_error_type)) rc$special[Aidx] <- input$attack_error_type
                    rally_codes(rc)
                }
                ## "current" team here is the digging team
                game_state$point_won_by <- if (eval == "#") other(game_state$current_team) else game_state$current_team
                rally_ended()
            } else if (input$c1 %eq% "A!") {
                ## blocked for reattack
                end_t <- retrieve_video_time(game_state$end_t)
                rc <- rally_codes()
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else NA_integer_
                ## TODO if we already have a block skill here, don't add a new one, just update the existing one ... though there should never already be block skill here
                ## block fault player should be in input$c1_def_player, but we'll take input$b1_block_touch_player otherwise
                bp <- if (!is.na(input$c1_def_player)) input$c1_def_player else if (!is.na(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
                if (!is.na(Aidx)) {
                    ## adjust the attack row
                    rc$eval[Aidx] <- "!"
                    rc$ez[Aidx] <- esz[1]
                    rc$esz[Aidx] <- esz[2]
                    rc$mid_x[Aidx] <- mid_xy[1]
                    rc$mid_y[Aidx] <- mid_xy[2]
                    rc$end_x[Aidx] <- game_state$end_x
                    rc$end_y[Aidx] <- game_state$end_y
                }
                ## "current" team here is the digging/blocking team
                rally_codes(bind_rows(rc,
                                      ## the block
                                      code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "!", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table),
                                      ## and the dig cover
                                      code_trow(team = other(game_state$current_team), pnum = if (!is.na(input$c1_cover_player)) input$c1_cover_player else 0L, skill = "D", eval = app_data$default_scouting_table$evaluation_code[app_data$default_scouting_table$skill == "D"], sz = esz[1], t = end_t, start_x = game_state$end_x, start_y = game_state$end_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)
                                      ))
                game_state$current_team <- other(game_state$current_team) ## attacking team now playing
                rally_state(if (isTRUE(app_data$options$transition_sets)) "click second contact" else "click third contact")
            } else if (input$c1 %eq% "B/") {
                rc <- rally_codes()
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc)  else NA_integer_
                ## block fault player should be in input$c1_def_player, but we'll take input$b1_block_touch_player otherwise
                bp <- if (!is.na(input$c1_def_player)) input$c1_def_player else if (!is.na(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
                if (!is.na(Aidx)) {
                    ## adjust the attack row
                    rc$ez[Aidx] <- esz[1]
                    rc$esz[Aidx] <- esz[2]
                    rc$end_x[Aidx] <- game_state$end_x
                    rc$end_y[Aidx] <- game_state$end_y
                }
                ## TODO if we already have a block skill here, don't add a new one, just update the existing one ... though there should never already be block skill here
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "/", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table))) ## TODO x,y?
                game_state$point_won_by <- other(game_state$current_team) ## "current" team here is the digging team
                rally_ended()
            } else if (input$c1 %eq% "B#") {
                rc <- rally_codes()
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else NA_integer_
                ## block player should be in input$c1_def_player, but we'll take input$b1_block_touch_player otherwise
                bp <- if (!is.na(input$c1_def_player)) input$c1_def_player else if (!is.na(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
                if (!is.na(Aidx)) {
                    ## adjust the attack row
                    rc$ez[Aidx] <- esz[1]
                    rc$esz[Aidx] <- esz[2]
                    rc$end_x[Aidx] <- game_state$end_x
                    rc$end_y[Aidx] <- game_state$end_y
                    rc$mid_x[Aidx] <- mid_xy[1]
                    rc$mid_y[Aidx] <- mid_xy[2]
                    rc$eval[Aidx] <- "/"
                }
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "#", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table))) ## TODO x,y?
                game_state$point_won_by <- game_state$current_team ## "current" team here is the digging/blocking team
                rally_ended()
            } else {
                ## D or D=
                digp <- if (!is.null(input$c1_def_player)) input$c1_def_player else 0L
                end_t <- retrieve_video_time(game_state$end_t)
                rc <- rally_codes()
                ## was the previous skill an attack, or one previous to that an attack with a block in between
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                if (!is.na(Aidx)) {
                    rc$ez[Aidx] <- esz[1]
                    rc$esz[Aidx] <- esz[2]
                    rc$end_x[Aidx] <- game_state$end_x
                    rc$end_y[Aidx] <- game_state$end_y
                    rc$mid_x[Aidx] <- mid_xy[1]
                    rc$mid_y[Aidx] <- mid_xy[2]
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
                ## dig location
                ## TODO CHECK is the dig start zone the same as the attack start zone, or its end zone?
                dx <- game_state$end_x
                dy <- game_state$end_y
                dz <- esz[1]
                if (!is.null(input$c1_block_touch_player) && app_data$options$end_convention %eq% "intended") {
                    ## if we are scouting intended attack directions, and there was a block touch, then don't use a dig location
                    dx <- dy <- NA_real_
                    dz <- "~"
                }
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "D", eval = eval, tempo = tempo, sz = dz, t = end_t, start_x = dx, start_y = dy, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                if (input$c1 == "D=") {
                    game_state$point_won_by <- other(game_state$current_team)
                    rally_ended()
                } else {
                    rally_state(if (isTRUE(app_data$options$transition_sets)) "click second contact" else "click third contact")
                }
            }
            if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
            if (rally_state() != "confirm end of set") {
                remove_scout_modal()
                do_video("play")
            }
        })

        observeEvent(input$assign_f1, {
            ## possible values for input$f1 are currently: F=, FD, FD=
            esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
            rc <- rally_codes()
            Fidx <- if (rc$skill[nrow(rc)] == "F") nrow(rc) else NA_integer_
            if (input$f1 %eq% "F=") {
                ## find the freeball, should be the previous skill
                if (!is.na(Fidx)) {
                    rc$ez[Fidx] <- esz[1]
                    rc$esz[Fidx] <- esz[2]
                    rc$end_x[Fidx] <- game_state$end_x
                    rc$end_y[Fidx] <- game_state$end_y
                    rc$eval[Fidx] <- "="
                    rally_codes(rc)
                }
                ## "current" team here is the digging team
                game_state$point_won_by <- game_state$current_team
                rally_ended()
            } else {
                ## FD or FD=
                digp <- if (!is.null(input$f1_def_player)) input$f1_def_player else 0L
                eval <- app_data$default_scouting_table$evaluation_code[app_data$default_scouting_table$skill == "F"]
                end_t <- retrieve_video_time(game_state$end_t)
                if (!is.na(Fidx)) {
                    rc$ez[Fidx] <- esz[1]
                    rc$esz[Fidx] <- esz[2]
                    rc$end_x[Fidx] <- game_state$end_x
                    rc$end_y[Fidx] <- game_state$end_y
                    rc$eval[Fidx] <- if (input$f1 %eq% "FD") "-" else "+"
                }
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "F", eval = eval, sz = esz[1], t = end_t, start_x = game_state$end_x, start_y = game_state$end_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                if (input$f1 == "FD=") {
                    game_state$point_won_by <- other(game_state$current_team)
                    rally_ended()
                } else {
                    rally_state(if (isTRUE(app_data$options$transition_sets)) "click second contact" else "click third contact")
                }
            }
            if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
            if (rally_state() != "confirm end of set") {
                remove_scout_modal()
                do_video("play")
            }
        })

        observeEvent(input$assign_c2, {
            ## set uses end position for zone/subzone
            esz <- as.character(dv_xy2subzone(game_state$start_x, game_state$start_y))
            passq <- if (!is.null(input$c2_pq)) input$c2_pq else guess_pass_quality(game_state, dvw = rdata$dvw, home_end = game_state$home_team_end)
            rc <- rally_codes()
            rc$eval[rc$skill %eq% "R"] <- passq
            ## find corresponding serve evaluation code
            seval <- app_data$compound_table$code[app_data$compound_table$skill %eq% "S" & app_data$compound_table$compound_skill %eq% "R" & app_data$compound_table$compound_code %eq% passq]
            if (nchar(seval) != 1) seval <- "~"
            rc$eval[rc$skill %eq% "S"] <- seval
            start_t <- retrieve_video_time(game_state$start_t)
            ## possible values for input$c2 are: Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball attack" = "P2", "Freeball over" = "F", R= rec error
            ##                                   "Opp. dig" = "aD", error "aD=", "Opp. overpass attack" = "aPR"
            if (input$c2 %in% c("E", "E=", "PP", "P2", "F", "R=")) {
                sp <- input$c2_player
                if (input$c2 == "E") {
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "E", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                    rally_state("click third contact")
                } else if (input$c2 == "E=") {
                    ## set error
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "E", eval = "=", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                    game_state$point_won_by <- other(game_state$current_team)
                    rally_ended()
                } else if (input$c2 %in% c("PP", "P2")) {
                    ## setter dump or second ball attack
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                    ## although we use PP and P2 in the R code here, we can use different combo codes in the dvw, following app_data$options$setter_dump_code and app_data$options$second_ball_attack_code
                    trg <- if (input$c2 == "PP") "S" else "~"
                    ## update target in the preceding set row, if there was one
                    ##if (tail(rc$skill, 1) == "E") rc$target[nrow(rc)] <- trg
                    ## these only seem to be populated when setter calls are used TODO
                    cmb <- if (input$c2 == "PP") app_data$options$setter_dump_code else app_data$options$second_ball_attack_code
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "A", tempo = "O", combo = cmb, sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                    rally_state("click attack end point")
                    game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                } else if (input$c2 == "F") {
                    ## freeball over
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "F", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                    ## TODO add end pos to this on next contact
                    game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                    rally_state("click freeball end point")
                } else if (input$c2 == "R=") {
                    ## delayed reception error (e.g. shanked pass)
                    ## just adjust the S & R evaluations and end the point
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) rc[Sidx, ] <- update_code_trow(rc[Sidx, ], eval = "#")
                    Ridx <- which(rc$skill == "R")
                    if (length(Ridx) == 1) rc[Ridx, ] <- update_code_trow(rc[Ridx, ], eval = "=")
                    rally_codes(rc)
                    game_state$current_team <- game_state$serving
                    game_state$point_won_by <- game_state$serving
                    rally_ended()
                }
            } else if (input$c2 %eq% "aPR") {
                ## opposition overpass attack
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/", if it was a set then "-"
                if (tail(rc$skill, 1) %in% c("R", "D", "E") && tail(rc$team, 1) %eq% game_state$current_team) {
                    new_eval <- if (tail(rc$skill, 1) %eq% "E") "-" else "/"
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = new_eval)
                }
                op <- if (!is.null(input$c2_opp_player)) input$c2_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "A", tempo = "O", combo = app_data$options$overpass_attack_code, sz = esz[1], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                rally_state("click attack end point")
            } else if (input$c2 %in% c("aD", "aD=")) {
                ## opposition dig on overpass
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/"
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/", if it was a set then "-"
                if (tail(rc$skill, 1) %in% c("R", "D", "E") && tail(rc$team, 1) %eq% game_state$current_team) {
                    new_eval <- if (tail(rc$skill, 1) %eq% "E") "-" else "/"
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = new_eval)
                }
                op <- if (!is.null(input$c2_opp_player)) input$c2_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "D", eval = if (input$c2 %eq% "aD=") "=" else "~", sz = esz[1], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                if (input$c2 %eq% "aD=") {
                    game_state$point_won_by <- game_state$current_team
                    rally_ended()
                } else {
                    game_state$current_team <- other(game_state$current_team)
                    rally_state(if (isTRUE(app_data$options$transition_sets)) "click second contact" else "click third contact")
                }
            }
            if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
            if (rally_state() != "confirm end of set") {
                remove_scout_modal()
                do_video("play")
            }
        })

        observeEvent(input$assign_c3, {
            ## possible values for input$c3 are: an attack code, Other attack, "F" Freeball or "E=" set error (if not scouting transition sets)
            ##    "Opp. dig" = "aD", "Opp. overpass attack" = "aPR"
            start_t <- retrieve_video_time(game_state$start_t)
            sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
            rc <- rally_codes()
            if (input$c3 %in% c("aPR", "aD", "aD=")) {
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/", if it was a set then "-"
                ## but we can only do this if we are scouting transition sets, otherwise we can be sure if it was e.g. D/ or a set over
                if (isTRUE(app_data$options$transition_sets) && tail(rc$skill, 1) %in% c("R", "D", "E") && tail(rc$team, 1) %eq% game_state$current_team) {
                    new_eval <- if (tail(rc$skill, 1) %eq% "E") "-" else "/"
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = new_eval)
                }
            }
            if (input$c3 %eq% "aPR") {
                ## opposition overpass attack
                op <- if (!is.null(input$c3_opp_player)) input$c3_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "A", tempo = "O", combo = app_data$options$overpass_attack_code, sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                rally_state("click attack end point")
            } else if (input$c3 %in% c("aD", "aD=")) {
                ## opposition dig on overpass
                op <- if (!is.null(input$c3_opp_player)) input$c3_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "D", eval = if (input$c3 %eq% "aD=") "=" else "~", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                if (input$c3 %eq% "aD=") {
                    game_state$point_won_by <- game_state$current_team
                    rally_ended()
                } else {
                    game_state$current_team <- other(game_state$current_team)
                    rally_state(if (isTRUE(app_data$options$transition_sets)) "click second contact" else "click third contact")
                }
            } else if (input$c3 == "F") {
                ## freeball over
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = if (!is.null(input$c3_player)) input$c3_player else 0L, skill = "F", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                ## TODO add end pos to this on next contact
                game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                rally_state("click freeball end point")
            } else if (input$c3 == "E=") {
                ## set error
                esz <- as.character(dv_xy2subzone(game_state$start_x, game_state$start_y))
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = if (!is.null(input$c3_player)) input$c3_player else 0L, skill = "E", eval = "=", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                game_state$point_won_by <- other(game_state$current_team)
                rally_ended()
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
                ## update target in the preceding set row, if there was one
                ##if (tail(rc$skill, 1) == "E") rc$target[nrow(rc)] <- targ
                ## these only seem to be populated when setter calls are used TODO
                nb <- input$nblockers
                if (is.null(nb) || !nb %in% 0:3) nb <- "~"
                ##if (nchar(input$c3) == 2) {
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = ap, skill = "A", tempo = tempo, combo = ac, sz = sz, num_p = nb, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), current_team = game_state$current_team, default_scouting_table = app_data$default_scouting_table)))
                rally_state("click attack end point")
                game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
            }
            if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
            if (rally_state() != "confirm end of set") {
                remove_scout_modal()
                do_video("play")
            }
        })

        output$rally_state <- renderUI({
            tags$div(id = "rallystate", tags$strong("Rally state: "), rally_state())
        })

        ## handle the pre-selection of serve player and type
        observeEvent(rally_state(), {
            if (rally_state() == "click serve start") {
                ## show the serve player and tempo pre-select buttons
                do_serve_preselect()
            } else {
                output$serve_preselect <- NULL
            }
        })

        do_serve_preselect <- function() {
            sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
            ## sp should be the serving player
            ## other players that could be serving, if the rotation is somehow wrong
            other_sp <- get_players(game_state, team = game_state$serving, dvw = rdata$dvw) ## includes sp in here too
            names(other_sp) <- player_nums_to(other_sp, team = game_state$serving, dvw = rdata$dvw)
            serve_player_buttons <- make_fat_radio_buttons(choices = sort(other_sp), selected = sp, input_var = "serve_preselect_player")
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
        }

        show_admin_modal <- function() {
            ## home player sub buttons
            ht_on <- sort(get_players(game_state, team = "*", dvw = rdata$dvw))
            ht_other <- setdiff(rdata$dvw$meta$players_h$number, ht_on)
            ht_other <- setdiff(ht_other, get_liberos(game_state, team = "*", dvw = rdata$dvw))
            ht_sub_out <- make_fat_radio_buttons(choices = ht_on, selected = NA, input_var = "ht_sub_out")
            ht_sub_in <- make_fat_radio_buttons(choices = ht_other, selected = NA, input_var = "ht_sub_in")
            ## visiting player sub buttons
            vt_on <- sort(get_players(game_state, team = "a", dvw = rdata$dvw))
            vt_other <- setdiff(rdata$dvw$meta$players_v$number, vt_on)
            vt_other <- setdiff(vt_other, get_liberos(game_state, team = "a", dvw = rdata$dvw))
            vt_sub_out <- make_fat_radio_buttons(choices = vt_on, selected = NA, input_var = "vt_sub_out")
            vt_sub_in <- make_fat_radio_buttons(choices = vt_other, selected = NA, input_var = "vt_sub_in")
            ht_can_sub <- length(ht_sub_in) > 0
            vt_can_sub <- length(vt_sub_in) > 0

            showModal(vwModalDialog(title = "Miscellaneous", footer = NULL,
                                    tags$p(tags$strong("Match actions")),
                                    fluidRow(column(2, actionButton("undo", "Undo last rally action", class = "undo fatradio")),
                                             ## only partially implemented
                                             column(2, actionButton("enter_code", "Enter scout code", class = "fatradio"), tags$span(style = "font-size:small;", "Only non-skill codes are supported")),
                                             column(2, actionButton("end_of_set_confirm", "End of set", class = "fatradio"))),
                                    tags$br(),
                                    ## TODO consider if all of these buttons should be available mid-rally or not (e.g. timeouts)
                                    fluidRow(column(6, tags$strong(datavolley::home_team(rdata$dvw), "(home)")),
                                             column(6, tags$strong(datavolley::visiting_team(rdata$dvw), "(visiting)"))),
                                    fluidRow(column(2, make_fat_buttons(choices = c("Won current rally" = "*p"), input_var = "manual_code")),
                                             column(2, make_fat_buttons(choices = c(Timeout = "*T"), input_var = "manual_code")),
                                             column(2, offset = 2, make_fat_buttons(choices = c("Won current rally" = "ap"), input_var = "manual_code")),
                                             column(2, make_fat_buttons(choices = c(Timeout = "aT"), input_var = "manual_code"))),
                                    tags$br(),
                                    fluidRow(column(6, if (ht_can_sub) tags$div(tags$p(tags$strong("Substitution"), "Player out"),
                                                                                do.call(fixedRow, lapply(ht_sub_out, function(but) column(2, but))),
                                                                                tags$p("Player in"),
                                                                                do.call(fixedRow, lapply(ht_sub_in, function(but) column(if (length(ht_other) <= 6) 2 else 1, but))))),
                                             column(6, if (vt_can_sub) tags$div(tags$p(tags$strong("Substitution"), "Player out"),
                                                                                do.call(fixedRow, lapply(vt_sub_out, function(but) column(2, but))),
                                                                                tags$p("Player in"),
                                                                                do.call(fixedRow, lapply(vt_sub_in, function(but) column(if (length(vt_other) <= 6) 2 else 1, but)))))
                                             ),
                                    fluidRow(column(2, offset = 4, if (ht_can_sub) make_fat_buttons(choices = c("Make substitution" = "*C"), input_var = "manual_code")),
                                             column(2, offset = 4, if (vt_can_sub) make_fat_buttons(choices = c("Make substitution" = "aC"), input_var = "manual_code"))),
                                    tags$hr(),
                                    fixedRow(column(2, offset = 10, actionButton("admin_dismiss", "Return to scouting", class = "continue fatradio")))
                                    ))
        }
        observeEvent(input$admin_dismiss, {
            ## dismiss the admin modal and unpause the video
            editing$active <- NULL
            removeModal()
            do_video("play")
        })

        show_review_pane <- function() {
            ## use the current video time from the main video
            ## construct the playlist js by hand, because we need to inject the current video time
            temp <- paste0("var start_t=vidplayer.currentTime()-2; revpl.set_playlist_and_play([{'video_src':'", file.path(app_data$video_server_base_url, basename(app_data$video_src)), "','start_time':start_t,'duration':4,'type':'local'}], 'review_player', 'local', true); revpl.set_playback_rate(1.4);")
            dojs(temp)
            js_show2("review_pane")
        }
        hide_review_pane <- function() {
            js_hide2("review_pane")
            dojs("revpl.video_stop();")
        }

        observeEvent(input$undo, {
            do_undo()
            removeModal()
            do_video("play")
        })

        do_undo <- function() {
            rc <- rally_codes()
            if (nrow(rc) > 0) {
                restore_rally_state <- tail(rc$rally_state, 1)
                restore_current_team <- tail(rc$current_team, 1)
                restore_t <- head(rc$t, 1)
                rc <- head(rc, -1)
                rally_codes(rc)
                ## reset the rally_state back to what it was for the last-existing code
                rally_state(restore_rally_state)
                ## and the current team
                game_state$current_team <- restore_current_team
                ## rewind the video to the time of the last action, or if we've removed all actions then the prior time of the first one
                do_video("set_time", if (nrow(rc) > 0) tail(rc$t, 1) else restore_t)
            } else {
                ## undo the last code in plays2
                ## this is trickier
                ## if it's lineups, remove all lineups
                ## otherwise if it's a point end, do we remove that plus the prior action?
                ## and need to figure out what rally state to reset to, which if we're continuing scouting of a partial file, we won't have
                ## TODO
            }
            if (nrow(rally_codes()) > 0 && !all(is.na(rally_codes()$t))) {
                ## set time to last action minus play_overlap
                new_vt <- max(rally_codes()$t, na.rm = TRUE) - app_data$play_overlap
                do_video("set_time", new_vt)
            }
        }

        ## for manual/direct code entry
        observeEvent(input$enter_code, {
           insert_data_row("below")
        })

        insert_data_row <- function(where) {
            if (missing(where)) where <- "above"
            where <- tolower(where)
            if (!where %in% c("above", "below")) where <- "above" ## default
            ridx <- playslist_mod$current_row()
            if (!is.null(ridx)) {
                ##if (where == "above" && ridx > 1) ridx <- ridx-1L ## we are inserting above the selected row, so use the previous row to populate this one
                ## otherwise (if inserting below) use the current row (ridx) as the template
                editing$active <- paste0("insert ", where)
                show_manual_code_modal(editing$active)
            }
        }

        ## op "edit" will also need the row number passed to build_code_entry, not yet implemented
        show_manual_code_modal <- function(op, entry_guide = FALSE) {
            op <- match.arg(op, c("insert below", "insert above", "edit"))
            showModal(modalDialog(title = if (grepl("insert", op)) paste0("Insert new code ", sub("insert ", "", op), " current row") else "Edit code", size = "l", footer = tags$div(actionButton("edit_commit", label = paste0(if (grepl("insert", op)) "Insert" else "Update", " code (or press Enter)")), actionButton("edit_cancel", label = "Cancel (or press Esc)")),
                                  if (entry_guide) "Enter code either in the top text box or in the individual boxes (but not both)",
                                  textInput("code_entry", label = "Code:", value = ""), if (entry_guide) "or", if (entry_guide) build_code_entry_guide(sub(" .*", "", op))
                                  ))
            focus_in_code_entry("code_entry")
        }

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
            HTML(end_zone_helper(input$code_entry_skill, input$code_entry_eval, dvw = rdata$dvw))
        })

        save_file_basename <- reactive({
            if (!is.null(rdata$dvw$meta$filename) && !is.na(rdata$dvw$meta$filename) && nchar(rdata$dvw$meta$filename)) {
                fs::path_ext_remove(basename(rdata$dvw$meta$filename))
            } else if (!is.null(rdata$dvw$meta$video) && nrow(rdata$dvw$meta$video) > 0 && length(na.omit(rdata$dvw$meta$video$file)) > 0 && nchar(na.omit(rdata$dvw$meta$video$file)[1])) {
                paste0(basename(fs::path_ext_remove(na.omit(rdata$dvw$meta$video$file)[1])))
            } else {
                "myfile"
            }
        })

        output$save_dvw_button <- downloadHandler(
            filename = function() paste0(save_file_basename(), ".dvw"),
            content = function(file) {
                tryCatch({
                    ## TODO flush any rally codes to plays2 - but note that then we won't have the right rally_state when we restart
                    ## so might not be able to do this
                    dv_write2(update_meta(rp2(rdata$dvw)), file = file)
                }, error = function(e) {
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

        output$save_rds_button <- downloadHandler(
            filename = function() paste0(save_file_basename(), ".rds"),
            content = function(file) {
                tryCatch({
                    ## TODO flush any rally codes to plays2 - but note that then we won't have the right rally_state when we restart
                    ## so might not be able to do this
                    out <- update_meta(rp2(rdata$dvw))
                    out$plays <- NULL ## don't save this
                    out$game_state <- isolate(reactiveValuesToList(game_state))
                    saveRDS(out, file)
                }, error = function(e) {
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
            ok <- handle_manual_code(input$manual_code)
            if (ok) {
                editing$active <- NULL
                removeModal()
                do_video("play")
            }
        })

        handle_manual_code <- function(code) {
            ok <- TRUE
            if (!is.null(code)) {
                if (grepl("^>", code)) {
                    ## comment, or perhaps >LUp
                    rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(code, game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
                } else if (code %in% c("*T", "aT")) {
                    rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(code, game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
                } else if (code %in% c("*p", "ap")) {
                    game_state$point_won_by <- substr(code, 1, 1)
                    if (length(rally_codes()) > 0) {
                        ## add any already-entered rally codes
                        rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(rally_codes(), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
                    }
                    ## and then the point-won-by code
                    rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(character(), game_state = game_state, rally_ended = TRUE, dvw = rdata$dvw)))
                    do_rally_end_things()
                } else if (code %in% c("*C", "aC")) {
                    ## substitution
                    if (code %eq% "*C") {
                        p_out <- as.numeric(input$ht_sub_out)
                        p_in <- as.numeric(input$ht_sub_in)
                    } else {
                        p_out <- as.numeric(input$vt_sub_out)
                        p_in <- as.numeric(input$vt_sub_in)
                    }
                    if (length(p_out) == 1 && length(p_in) == 1) {
                        game_state <- game_state_make_substitution(game_state, team = substr(code, 1, 1), player_out = p_out, player_in = p_in, dvw = rdata$dvw)
                        rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(paste0(substr(code, 1, 1), "C", p_out, ".", p_in), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
                        ## if we just substituted the player about to serve, we need to update the serve preselect buttons
                        do_serve_preselect()
                    } else {
                        ## players in/out not selected, ignore
                        ok <- FALSE
                    }
                }
            }
            ok
        }
##        ## auto save
##        shinyFiles::shinyFileSave(input, id = "auto_save_file", roots = c(root = '/'))
##        observe({
##            ## input$auto_save_file will have components
##            ##List of 4
##            ## $ name: chr "temp.dvw"
##            ## $ type: Named list()
##            ## $ path:List of 2
##            ##  ..$ : chr ""
##            ##  ..$ : chr "tmp"
##            ## $ root: chr "root"
##        })

##        observe({
##            cat("game_state:\n")
##            cat(str(reactiveValuesToList(game_state)))
##        })

        observeEvent(input$general_help, introjs(session, options = list("nextLabel"="Next", "prevLabel"="Previous", "skipLabel"="Skip")))
        observeEvent(input$show_shortcuts, {
            showModal(modalDialog(title = "Keyboard shortcuts", easyClose = TRUE, size = "l",
                                  if (app_data$with_video) tagList(tags$p(tags$strong("Video controls")), tags$ul(tags$li("[l or 6] forward 2s, [; or ^] forward 10s, [m or 3] forwards 0.1s, [, or 9] forwards 1 frame"), tags$li("[j or 4] backward 2s, [h or $] backward 10s, [n or 1] backwards 0.1s, [b or 7] backwards 1 frame"), tags$li("[q or 0] pause video")##, tags$li("[g or #] go to currently-selected event")
                                                                                                                  )),
                                  ## none of these are relevant yet
                                  ##fluidRow(column(6, tags$strong("Keyboard controls"),
                                  ##         tags$ul(tags$li("[r or 5] sync selected event video time"),
                                  ##                 tags$li("[i or 8] move to previous skill row"),
                                  ##                 tags$li("[k or 2] move to next skill row"),
                                  ##                 tags$li("[e or E] edit current code"),
                                  ##                 tags$li("[del] delete current code"),
                                  ##                 tags$li("[ins] insert new code above current"),
                                  ##                 tags$li("[Shift-ins] insert new code below current"),
                                  ##                 tags$li("[F1] home team rotate +1"),
                                  ##                 tags$li("[F2] insert setting codes before every attack"),
                                  ##                 tags$li("[F4] delete all setting codes (except errors)"),
                                  ##                 tags$li("[F6] insert digging codes after every attack"),
                                  ##                 tags$li("[F8] delete all digging codes"),
                                  ##                 tags$li("[F10] visiting team rotate +1"),
                                  ##                 )),
                                  ##         column(6, if (app_data$with_video) tagList(tags$strong("Tagging"), tags$ul(tags$li("[left-click the court inset then press 't'] add a tag with the clicked court location. Alternatively, the location can be entered by left-clicking the video, if the court reference data has been provided"),
                                  ##                                                   tags$li("[T] open the tag manager (download or clear tag data)"))),
                                  ##                tags$strong("Ball coordinates"), tags$ul(tags$li("[left-click the court inset] register the start/mid/end ball positions"),
                                  ##                                                         tags$li("[accept ball coordinates] to add coordinates to the currently selected item"))))
                                  ))
        })

        ## disaster recovery
        shiny::onSessionEnded(function() {
            tryCatch({
                dvw <- isolate(rdata$dvw)
                dvw$game_state <- isolate(reactiveValuesToList(game_state))
                tf <- tempfile(fileext = ".rds")
                saveRDS(dvw, tf)
                message("working file has been saved to:", tf)
            }, error = function(e) {
                message("could not save working file on exit (error message was: ", conditionMessage(e))
            })
        })
        ## seek to video time on startup
        if ("video_time" %in% names(app_data$dvw$plays2) && nrow(app_data$dvw$plays2) > 0) {
            temp_vt <- na.omit(app_data$dvw$plays2$video_time)
            if (length(temp_vt) > 0) do_video("set_time", max(temp_vt))
        }
    }
}
