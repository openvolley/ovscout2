ov_scouter_server <- function(app_data) {
    function(input, output, session) {
        debug <- 0L
        cstr <- function(z) capture.output(str(z))
        app_data$have_warned_auto_save <- FALSE
        .am_in_server <- TRUE ## for eval_in_server and getsv functions

        ## the active UI element, used in typing mode to keep track of where the focus should be. Possible values "" (uninitialilzed), "scout_bar", "playslist"
        active_ui <- reactiveVal("")
        if (debug > 0) observeEvent(active_ui(), cat("active_ui:", active_ui(), "\n"))
        observeEvent(input$scout_in_click, focus_to_scout_bar())

        extra_db_con <- NULL
        if (!is.null(app_data$extra_db)) {
            if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE) || !requireNamespace("dbplyr", quietly = TRUE)) {
                ## somehow extra_db was set but these aren't available?
                message("The duckdb, dbplyr, and DBI packages are required in order to use ball tracking outputs")
                app_data$extra_db <- NULL
                app_data$extra_ball_tracking <- FALSE
            } else {
                extra_db_con <-  tryCatch(DBI::dbConnect(duckdb::duckdb(app_data$extra_db)), error = function(e) NULL)
            }
        }
        shiny::onSessionEnded(function() {
            if (!is.null(extra_db_con)) try(DBI::dbDisconnect(extra_db_con, shutdown = TRUE), silent = TRUE)
            shiny::stopApp()
        })
        ## on startup, clean up old auto-saved files
        try({
            d <- fs::dir_info(file.path(app_data$user_dir, "autosave"))
            d <- d$path[difftime(Sys.time(), d$modification_time, units = "days") > 7] ## older than 7 days
            if (length(d)) fs::file_delete(d)
        })

        if (is.null(app_data$dvw$meta$more$scout) || is.na(app_data$dvw$meta$more$scout) || !nzchar(app_data$dvw$meta$more$scout)) app_data$dvw$meta$more$scout <- isolate(app_data$scout_name)

        plays_cols_to_show <- c("error_icon", "video_time", "set_number", "code", "Score") ##"home_setter_position", "visiting_setter_position", "is_skill"
        plays_cols_renames <- c(Set = "set_number")##, hs = "home_setter_position", as = "visiting_setter_position")

        if (is.null(app_data$dvw$meta$match$regulation)) stop("dvw does not have regulation information")
        app_data$is_beach <- is_beach(app_data$dvw)

        app_data$styling$scout_modal_width <- 100 - app_data$styling$review_pane_width

        atbl <- app_data$dvw$meta$attacks
        atbl <- bind_cols(atbl[, setdiff(names(atbl), c("start_x", "start_y"))], setNames(dv_index2xy(atbl$start_coordinate), c("start_x", "start_y")))
        app_data$dvw$meta$attacks <- atbl

        if (app_data$options$attacks_by %eq% "codes") {
            if (is.null(app_data$options$setter_dump_code)) app_data$options$setter_dump_code <- "PP"
            if (is.null(app_data$options$second_ball_attack_code)) app_data$options$second_ball_attack_code <- "P2"
        }

        if (is.null(app_data$dvw$video2_offset)) {
            ## keep the video2 details in app_data$dvw (and rdata$dvw), so it gets saved and reloaded in the .ovs file
            if (is.null(app_data$video2_offset)) {
                ## not provided as parm to ov_scouter call
                app_data$dvw$video2_offset <- 0
            } else {
                app_data$dvw$video2_offset <- app_data$video2_offset
            }
        }
        if (!is.null(app_data$video_src2)) app_data$dvw$video_file2 <- app_data$video_src2

        if (app_data$scout_mode == "click") {
            ## if we are click-scouting, we can only do attack directions by zones
            app_data$dvw$meta$match$zones_or_cones <- "Z"
        }
        rdata <- reactiveValues(dvw = app_data$dvw, options = app_data$options)
        prefs <- reactiveValues(scout_name = app_data$scout_name, show_courtref = app_data$show_courtref, scoreboard = app_data$scoreboard, pause_on_type = app_data$pause_on_type, ball_path = app_data$ball_path, playlist_display_option = app_data$playlist_display_option, review_pane = app_data$review_pane)
        if (!is.null(app_data$playback_rate) && isTRUE(app_data$playback_rate > 0)) updateSliderInput(session, "playback_rate", value = app_data$playback_rate)

        pseq <- if (app_data$is_beach) 1:2 else 1:6

        ## maybe TODO add button to change scout mode from click (guided) to typing
        ## when changing mode:
        ## - change the #main_video height css to 85vh (click) or 50vh (typing)
        ## - change the active set of shortcuts

        ## set up for typing mode
        ## we can modify app_data$pause_on_type here without affecting the prefs reactive, so that it can be disabled if we aren't using video in this session but still retain it as a preference
        if (!app_data$with_video) app_data$pause_on_type <- 0L
        if (app_data$scout_mode == "type") {
            ## send shortcuts to js
            if (length(app_data$type_shortcuts) > 0) dojs(paste0("sk_shortcut_map = ", make_js_keymap(app_data$type_shortcuts), ";"))
            if (length(app_data$remapping) > 0) dojs(paste0("sk_key_map = ", make_js_keymap(app_data$remapping), ";"))
            app_data$shortcuts <- app_data$type_shortcuts ## this is the active set
        } else {
            app_data$shortcuts <- app_data$click_shortcuts ## this is the active set
        }
        output$zones_cones <- renderUI({
            if (app_data$scout_mode == "type") {
                tags$div(style = "float:right; font-size:small;", "You are scouting attack directions with:", if (rdata$dvw$meta$match$zones_or_cones %eq% "C") "cones" else if (rdata$dvw$meta$match$zones_or_cones %eq% "Z") "zones" else "unknown")
            } else {
                NULL
            }
        })
        have_second_video <- !is.null(app_data$video_src2)
        current_video_src <- reactiveVal(1L) ## start with video 1
        preview_video_src <- reactiveVal(1L)
        observe({
            if (app_data$with_video) {
                chk <- is.null(input$video_width) || is.na(input$video_width) || is.null(input$video_height) || is.na(input$video_height) ||
                    ## zero width or height is also invalid, except if it's a YT video
                    (current_video_src() == 1L && !is_youtube_url(app_data$video_src) && input$video_width < 1) || (current_video_src() == 2L && !is_youtube_url(app_data$video_src2) && input$video_height < 1)
                if (chk) {
                    dojs("Shiny.setInputValue('video_width', vidplayer.videoWidth()); Shiny.setInputValue('video_height', vidplayer.videoHeight());")
                    shiny::invalidateLater(200)
                }
            }
        })
        observeEvent(input$switch_video, {
            do_switch_video(have_second_video = have_second_video, current_video_src = current_video_src, rdata = rdata, app_data = app_data, video_state = video_state)
        })
        ## video 2 offset tweak. This would be better with side-by-side videos, but couldn't get that working reliably
        ## maybe side-by-side still frames would be even better (though difficult to do with remote videos)
        observeEvent(input$v2_offset, {
            prevsrc <- get_video_source_type(if (preview_video_src() == 1L) app_data$video_src else app_data$video_src2, base_url = app_data$video_server_base_url)
            editing$active <- "video offset"
            show_video_setup_modal(prevsrc = prevsrc, dvw = rdata$dvw)
            dojs("videojs('video_preview');")

        })

        observeEvent(input$preview_dismiss, {
            dojs("videojs('video_preview').dispose();")
            editing$active <- NULL
            removeModal()
        })

        output$preview_header <- renderUI({
            tags$p(tags$strong("Showing video: ", preview_video_src()))
        })

        observeEvent(input$switch_preview, do_switch_preview(preview_video_src = preview_video_src, app_data = app_data, rdata = rdata))

        observeEvent(input$v2_offset_value, {
            ## TODO need this to react straight away, not after the value has been entered (the input box has been exited)
            if (!is.null(input$v2_offset_value) && !is.na(input$v2_offset_value)) rdata$dvw$video2_offset <<- input$v2_offset_value
        })

        ## initialize the game state
        app_data$click_to_start_msg <- paste0(if (app_data$scout_mode != "type") "click or ", "unpause the video to start") ## this is a bit unnecessary since the rally state message isn't shown in type mode
        rally_state <- reactiveVal(app_data$click_to_start_msg)
        rally_codes <- reactiveVal(empty_rally_codes)
        if ("game_state" %in% names(app_data$dvw) && !is.null(app_data$dvw$game_state)) {
            ## saved as an rds, so re-use this
            temp <- app_data$dvw$game_state
        } else {
            temp <- as.list(tail(app_data$dvw$plays2, 1))
        }
        if (!"serving" %in% names(temp) || is.na(temp$serving)) temp$serving <- "*" ## default to home team serving
        temp$current_team <- temp$serving
        temp$rally_started <- FALSE
        temp$start_x <- temp$start_y <- temp$mid_x <- temp$mid_y <- temp$end_x <- temp$end_y <- NA_real_
        temp$startxy_valid <- temp$midxy_valid <- temp$endxy_valid <- FALSE
        temp$current_time_uuid <- ""
        for (i in pseq) {
            if (is.null(temp[[paste0("home_p", i)]])) temp[[paste0("home_p", i)]] <- NA_integer_
            if (is.null(temp[[paste0("visiting_p", i)]])) temp[[paste0("visiting_p", i)]] <- NA_integer_
        }
        ## liberos
        if (!"ht_lib1" %in% names(temp)) temp$ht_lib1 <- NA_integer_
        if (!"ht_lib2" %in% names(temp)) temp$ht_lib2 <- NA_integer_
        if (!"vt_lib1" %in% names(temp)) temp$vt_lib1 <- NA_integer_
        if (!"vt_lib2" %in% names(temp)) temp$vt_lib2 <- NA_integer_
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

        if (app_data$scout_mode == "type") populate_server(game_state)

        playslist_mod <- callModule(mod_playslist, id = "playslist", rdata = rdata, plays_cols_to_show = plays_cols_to_show, plays_cols_renames = plays_cols_renames,
                                    display_option = reactive(prefs$playlist_display_option))
        observeEvent(playslist_mod$clicked(), { if (isTRUE(playslist_mod$clicked() > 0)) focus_to_playslist() })

        ## court inset showing rotation and team lists
        court_inset <- callModule(mod_courtrot2_base, id = "courtrot", rdata = rdata, game_state = game_state, rally_codes = rally_codes, rally_state = rally_state, current_video_src = current_video_src, styling = app_data$styling, with_ball_path = reactive(prefs$ball_path), current_plays_row = reactive(playslist_mod$current_row()))

        ## handle court module home team buttons
        ## note that these should only be available to the user if there is not a rally in progress
        observeEvent(court_inset$rotate_home(), do_rotate("home"))
        observeEvent(court_inset$timeout_home(), handle_non_skill_code("*T"))
        observeEvent(court_inset$p1pt_home(), {
            game_state$home_score_start_of_point <- max(0L, game_state$home_score_start_of_point + 1L) ## max not strictly necessary here
        })
        observeEvent(court_inset$m1pt_home(), {
            game_state$home_score_start_of_point <- max(0L, game_state$home_score_start_of_point - 1L)
        })
        observeEvent(court_inset$substitution_home(), show_substitution_pane("*c"))
        ## visiting team buttons
        observeEvent(court_inset$rotate_visiting(), do_rotate("visiting"))
        observeEvent(court_inset$timeout_visiting(), handle_non_skill_code("aT"))
        observeEvent(court_inset$p1pt_visiting(), {
            game_state$visiting_score_start_of_point <- max(0L, game_state$visiting_score_start_of_point + 1L) ## max not strictly necessary here
        })
        observeEvent(court_inset$m1pt_visiting(), {
            game_state$visiting_score_start_of_point <- max(0L, game_state$visiting_score_start_of_point - 1L)
        })
        observeEvent(court_inset$substitution_visiting(), show_substitution_pane("ac"))

        teamslists <- callModule(mod_teamslists, id = "teamslists", rdata = rdata, two_cols = app_data$scout_mode != "type")
        detection_ref1 <- reactiveVal({ if (!is.null(app_data$court_ref)) app_data$court_ref else NULL })
        ## for the court reference modules, pass the video file as well as the URL. Video will be shown, but the metadata can be stored/retrieved from the file
        courtref1 <- callModule(mod_courtref, id = "courtref1", video_file = if (!is_url(app_data$video_src)) app_data$video_src else NULL, video_url = if (is_url(app_data$video_src)) app_data$video_src else file.path(app_data$video_server_base_url, basename(app_data$video_src)), detection_ref = detection_ref1, main_video_time_js = "vidplayer.currentTime()", styling = app_data$styling)
        detection_ref2 <- reactiveVal({ if (!is.null(app_data$court_ref2)) app_data$court_ref2 else NULL })
        courtref2 <- if (have_second_video) callModule(mod_courtref, id = "courtref2", video_file = if (!is_url(app_data$video_src2)) app_data$video_src2 else NULL, video_url = if (is_url(app_data$video_src2)) app_data$video_src2 else file.path(app_data$video_server_base_url, basename(app_data$video_src2)), detection_ref = detection_ref2, main_video_time_js = "vidplayer.currentTime()", styling = app_data$styling) else NULL
        detection_ref <- reactive(if (current_video_src() < 2) detection_ref1() else detection_ref2()) ## whichever is associated with the current view
        courtref_active <- reactive({
            isTRUE(courtref1$active()) || (!is.null(courtref2) && isTRUE(courtref2$active()))
        })
        tsc_mod <- callModule(mod_teamscores, id = "tsc", game_state = game_state, rdata = rdata, styling = app_data$styling, visible = reactive(prefs$scoreboard))

        have_asked_end_of_set <- reactiveVal(FALSE) ## only ask once per set: if the user says no, then don't keep popping the modal up after every point (it's perhaps a drill or training scrimmage)

        video_state <- reactiveValues(paused = TRUE, muted = TRUE) ## starts paused and muted
        editing <- reactiveValues(active = NULL)

        observeEvent(input$playback_rate, {
            if (!is.null(input$playback_rate)) do_video("playback_rate", input$playback_rate)
        })

        ## match, team, and lineup data editing
        match_data_edit_mod <- callModule(mod_match_data_edit, id = "match_data_editor", rdata = rdata, editing = editing, app_data = app_data)
        team_edit_mod <- callModule(mod_team_edit, id = "team_editor", rdata = rdata, editing = editing, styling = app_data$styling)
        team_select_mod <- callModule(mod_team_select, id = "team_selector", rdata = rdata, editing = editing, app_data = app_data)
        lineup_edit_mod <- callModule(mod_lineup_edit, id = "lineup_editor", rdata = rdata, game_state = game_state, editing = editing, video_state = video_state, styling = app_data$styling)

        observeEvent(input$edit_cancel, {
            do_focus_to_playslist <- !is.null(editing$active) && editing$active %eq% "edit" && app_data$scout_mode == "type" ## if cancelled editing a code, return to playslist
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
            if (do_focus_to_playslist) focus_to_playslist()
        })

        observeEvent(input$edit_commit, {
            do_edit_commit()
        })

        ## this is a workaround to deal with input values not being updated in time if the user presses Enter quickly while editing codes (in e.g. manual code entry)
        observeEvent(input$code_entries, {
            req(input$code_entries)
            ##cat("ces:", capture.output(str(input$code_entries)), "\n")
            nc2 <- input$code_entries$text
            nc1 <- paste(lapply(seq_len(nrow(code_bits_tbl)), function(bi) {
                val <- input$code_entries[[code_bits_tbl$bit[bi]]]
                if (is.null(val)) val <- ""
                wid <- code_bits_tbl$width[bi]
                if (nchar(val) < wid) val <- str_pad(val, wid, side = "right", pad = "~")
                val
            }), collapse = "")
            do_edit_commit(nc1, nc2)
        })

        ## hide the code edit buttons if there is no selected row in the plays table
        observe({
            if (is.null(playslist_mod$current_row())) {
                js_disable("edit_code_insert")
                js_disable("edit_code_delete")
                js_disable("edit_code_edit")
                js_disable("edit_code_edit_coords")
            } else {
                js_enable("edit_code_insert")
                js_enable("edit_code_delete")
                js_enable("edit_code_edit")
                js_enable("edit_code_edit_coords")
            }
        })
        ## handle code edit button presses
        observeEvent(input$edit_code_edit, edit_data_row())
        observeEvent(input$edit_code_delete, delete_data_row())
        observeEvent(input$edit_code_insert, insert_data_row("above"))
        observeEvent(input$edit_code_edit_coords, {
            coord_edit_row(playslist_mod$current_row()) ## note the current row that we are editing the coords of
            output$code_edit_dialog <- renderUI(code_edit_dialog_content("coord_click_start"))
            editing$active <- "coord_click_start"
        })

        do_edit_commit <- function(newcode1, newcode2) {
            if (!is.null(editing$active)) {
                dismiss_modal <- TRUE
                if (editing$active %in% c("edit", "insert above", "insert below")) {
                    ## user has changed EITHER input$code_entry or used the code_entry_guide
                    ## infer code from code_entry_guide elements
                    if (missing(newcode1)) {
                        newcode1 <- lapply(seq_len(nrow(code_bits_tbl)), function(bi) {
                            val <- input[[paste0("code_entry_", code_bits_tbl$bit[bi])]]
                            if (is.null(val)) val <- ""
                            wid <- code_bits_tbl$width[bi]
                            if (nchar(val) < wid) val <- str_pad(val, wid, side = "right", pad = "~")
                            val
                        })
                        newcode1 <- paste(newcode1, collapse = "")
                    }
                    if (missing(newcode2) || is.null(newcode2)) newcode2 <- input$code_entry
                    newcode1 <- sub("~+$", "", newcode1) ## trim trailing ~'s
                    newcode2 <- sub("~+$", "", newcode2)
                    ridx <- playslist_mod$current_row()
                    if (editing$active %eq% "edit" && !is.null(ridx) && !is.na(ridx)) {
                        crc <- get_current_rally_code(playslist_mod = playslist_mod, rdata = rdata, rally_codes = rally_codes) ## NOTE this won't have the actual scout code unless it was a non-skill code
                        if (!is.null(crc)) {
                            old_code <- if (is.null(crc$code) || is.na(crc$code)) codes_from_rc_rows(crc) else crc$code
                            ## user has changed EITHER input$code_entry or used the code_entry_guide
                            changed1 <- (!newcode1 %eq% old_code) && nzchar(newcode1)
                            changed2 <- (!newcode2 %eq% old_code) && nzchar(newcode2)
                            if (!changed1 && changed2) {
                                newcode <- newcode2
                                ## if we entered via the text box, then run this through the code parser
                                home_setter_num <- game_state[[paste0("home_p", game_state$home_setter_position)]]
                                visiting_setter_num <- game_state[[paste0("visiting_p", game_state$visiting_setter_position)]]
                                newcode <- sub("~+$", "", ov_code_interpret(newcode, attack_table = rdata$options$attack_table, compound_table = rdata$options$compound_table, default_scouting_table = rdata$options$default_scouting_table, home_setter_num = home_setter_num, visiting_setter_num = visiting_setter_num))
                            } else if (!changed1 && !changed2) {
                                ## neither changed, nothing to do
                                newcode <- NULL
                            } else {
                                newcode <- newcode1
                            }
                            if (!is.null(newcode)) {
                                if (app_data$scout_mode == "type") focus_to_playslist() ## focus here so that focus returns to the playslist after it is re-rendered
                                crc$code <- newcode
                                ## so we have a new code, but the (changed) information in that won't be in the other columns of crc yet
                                temp <- parse_code_minimal(newcode)[[1]]
                                ## if the new code was a compound code, then that will resolve to two codes. We are only using the first because
                                ##  it's not clear how to deal with the second code. It can't necessarily inherit all details from the edited code
                                if (!is.null(temp)) {
                                    ## should not be NULL, that's only for non-skill rows
                                    ## put whatever got updated back into crc
                                    crc <- bind_cols(crc[, setdiff(names(crc), names(temp))], temp)[, names(crc)]
                                    ## if ridx is greater than the length of plays2 rows, then put this in rally_codes()
                                    if (ridx <= nrow(rdata$dvw$plays2)) {
                                        newrc <- make_plays2(crc, game_state = crc$game_state[[1]], rally_ended = FALSE, dvw = rdata$dvw)
                                        rdata$dvw$plays2 <- transfer_scout_details(from_row = newrc, to_df = rdata$dvw$plays2, row_idx = ridx, dvw = rdata$dvw)
                                        ## TODO should this have an rp2() wrapper?
                                    } else if ((ridx - nrow(rdata$dvw$plays2)) <= nrow(rally_codes())) {
                                        rc <- rally_codes()
                                        rcidx <- ridx - nrow(rdata$dvw$plays2)
                                        rally_codes(transfer_scout_details(from_row = crc, to_df = rc, row_idx = rcidx, dvw = rdata$dvw))
                                    }
                                }
                            }
                        }
                    } else if (editing$active %in% c("insert above", "insert below") && !is.null(ridx) && !is.na(ridx)) {
                        ## inserting new code above or below
                        ## note that we are either inserting into the plays2 dataframe, or into rally_codes (the current rally). The playslist shows both (latter appended to former)
                        if (editing$active == "insert above") {
                            insert_ridx <- ridx
                        } else {
                            insert_ridx <- ridx + 1L ## this can be one greater than the current row count of plays2 plus rally_codes, in which case it will be appended to rally_codes
                        }
                        ## we will insert the new row at row insert_ridx. Anything on or after that row in the existing plays2 or rally_codes gets shifted down
                        if (debug) cat("  selected row:", ridx, "\n  inserting at:", insert_ridx, "\n")
                        ## and take details from the row immediately prior to where we are inserting
                        ## since we are inserting a code, the current game_state is not necessarily valid. Retrieve the game state from the row prior to where we are inserting
                        details_from_idx <- max(1L, insert_ridx - 1L)
                        if (details_from_idx <= nrow(rdata$dvw$plays2)) {
                            if (debug) cat("  details being taken from plays2, row:", details_from_idx, "\n")
                            details_from <- rdata$dvw$plays2[details_from_idx, ]$rally_codes[[1]]
                            gs <- sanitize_game_state(details_from$game_state[[1]])
                            ## NOTE though that if we are inserting at the start of a rally, the details_from row might have a NULL game_state because it's a non-skill row like a lineup code
                            if (is.null(gs)) gs <- sanitize_game_state(rdata$dvw$plays2[insert_ridx, ]$rally_codes[[1]]$game_state[[1]]) ## TODO fix properly. Perhaps define a fallback empty gs?
                        } else {
                            rcidx <- details_from_idx - nrow(rdata$dvw$plays2)
                            details_from <- rally_codes()[rcidx, ]
                            gs <- sanitize_game_state(details_from$game_state[[1]])
                            if (debug) cat("  details being taken from rally_codes, row:", rcidx, "\n")
                        }
                        ## cat("details from:", str(details_from), "\n")
                        insert_clock_time <- details_from$time ## TODO check, in previous versions plays2 had clock time but rally_codes generally did not
                        if (is.null(insert_clock_time)) insert_clock_time <- as.POSIXct(NA) ## fallback if time is completely missing
                        insert_video_time <- details_from$t
                        insert_rs <- details_from$rally_state ## TODO CHECK

                        code <- if (nzchar(newcode1)) newcode1 else newcode2
                        if (grepl("^[TpczPCS]", code)) code <- paste0("*", code)
                        cat("to process: ", code, "\n")
                        if (app_data$scout_mode == "type") focus_to_playslist() ## focus here so that focus returns to the playslist after it is re-rendered
                        if (grepl("^(\\*p|ap|\\*c|ac|\\*z|az|\\*S|aS)", code) || ## sub, setter position, assign serving team
                            grepl("^[a\\*]C[[:digit:]]+[:\\.][[:digit:]]+", code) || ## substitution ensuring it can't be an attack combo code
                            (grepl("^[a\\*]P[[:digit:]]", code) && !isTRUE(gs$rally_started))) { ## Px can be a setter assignment or an attack combo code ("P2" is particularly ambiguous). Treat as setter assignment if the rally has not yet started
                            ## can't handle these as edits, at least not yet
                            ## TODO warn
                        } else if (grepl("^[a\\*]?L", code)) {
                            ## starting lineup
                            assign_lineup_from_manual(code, rdata = rdata, game_state = game_state, app_data = app_data)
                        } else if (grepl("^(>|\\*T|aT)", code)) { ## timeout, comment
                            newrow <- make_plays2(code, game_state = gs, rally_ended = FALSE, dvw = rdata$dvw)
                            newrow$time <- insert_clock_time
                            newrow$video_time <- insert_video_time
                            ## check: any other details to transfer? TODO
                            ## cat("gs:", str(gs), "\n")
                            ## cat("newrow:", str(newrow), "\n")
                            if (insert_ridx <= nrow(rdata$dvw$plays2)) {
                                cat(nrow(rdata$dvw$plays2), "\n")
                                rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2[seq_len(insert_ridx - 1L), ],
                                                                  newrow,
                                                                  rdata$dvw$plays2[insert_ridx:nrow(rdata$dvw$plays2), ]))
                                cat(nrow(rdata$dvw$plays2), "\n")
                            } else {
                                ## can't insert into rally_codes, that only holds skill rows!
                                warning("can't insert into current rally")
                            }
                        } else {
                            code <- augment_code_attack_details(code, game_state = game_state, opts = rdata$options) ## deal with code shorthands
                            home_setter_num <- game_state[[paste0("home_p", game_state$home_setter_position)]]
                            visiting_setter_num <- game_state[[paste0("visiting_p", game_state$visiting_setter_position)]]
                            newcode <- sub("~+$", "", ov_code_interpret(code, attack_table = rdata$options$attack_table, compound_table = rdata$options$compound_table, default_scouting_table = rdata$options$default_scouting_table, home_setter_num = home_setter_num, visiting_setter_num = visiting_setter_num))
                            cat("code after interpretation:", newcode, "\n")
                            ## code can be length > 1 now, because code might have been a compound code
                            ptemp <- parse_code_minimal(newcode) ## convert to a list of tibble row(s)
                                ## this has to be added to plays2 OR rally codes, depending on the insertion point
                                ## remember that the playstable is showing plays2 with rally_codes converted to plays2 format and appended
                                ## in either case, convert to rally_codes format first
                            newrc <- bind_rows(lapply(ptemp, function(temp) {
                                if (!is.null(temp)) {
                                    ## should not be NULL, that's only for non-skill rows
                                    code_trow(team = temp$team, pnum = temp$pnum, skill = temp$skill, tempo = temp$tempo, eval = temp$eval, combo = temp$combo, target = temp$target, sz = temp$sz, ez = temp$ez, esz = temp$esz, start_zone_valid = TRUE, endxy_valid = TRUE, t = insert_video_time, time = insert_clock_time, rally_state = insert_rs, game_state = gs, default_scouting_table = rdata$options$default_scouting_table)
                                }
                            }))
                            ## TODO check row updating here
                            if (insert_ridx <= nrow(rdata$dvw$plays2)) {
                                newrow <- make_plays2(newrc, game_state = gs, rally_ended = FALSE, dvw = rdata$dvw)
                                temp <- bind_rows(rdata$dvw$plays2[seq_len(insert_ridx - 1L), ],
                                                  newrow,
                                                  rdata$dvw$plays2[insert_ridx:nrow(rdata$dvw$plays2), ])
                                ## update surrounding rows
                                temp <- transfer_scout_details(from_row = newrow, to_df = temp, row_idx = insert_ridx, dvw = rdata$dvw)
                                rdata$dvw$plays2 <- rp2(temp)
                            } else {
                                rc <- rally_codes()
                                rcidx <- insert_ridx - nrow(rdata$dvw$plays2)
                                rc <- bind_rows(rc[seq_len(rcidx - 1L), ], newrc, rc[rcidx:nrow(rc), ])
                                ## update surrounding rows and update rally_codes()
                                rally_codes(transfer_scout_details(from_row = newrc, to_df = rc, row_idx = rcidx, dvw = rdata$dvw))
                            }
                        }
                    }
                } else {
                    changed <- code_make_change(editing$active, game_state = game_state, dvw = rdata$dvw, input = input,
                                                htdata_edit = team_edit_mod$htdata_edit(), vtdata_edit = team_edit_mod$vtdata_edit(),
                                                htdata_select = team_select_mod$htdata_select(), vtdata_select = team_select_mod$vtdata_select())
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
                if (dismiss_modal) removeModal()
            }
        }

        meta_is_valid <- reactiveVal(TRUE)
        observe({
            notnn <- function(z) !is.null(z) && !is.na(z)
            ## check teams
            teams_ok <- !is.null(rdata$dvw$meta$teams) ## && !any(tolower(rdata$dvw$meta$teams$team) %in% c("home team", "visiting team"))
            ## check rosters
            ## TODO better
            rosters_ok <- !is.null(rdata$dvw$meta$players_h) && length(na.omit(rdata$dvw$meta$players_h$number)) >= length(pseq) &&
                !is.null(rdata$dvw$meta$players_v) && length(na.omit(rdata$dvw$meta$players_v$number)) >= length(pseq)
            if (rosters_ok) {
                ## more checks
                rtxt <- character()
                if (any(duplicated(rdata$dvw$meta$players_h$number) & !is.na(rdata$dvw$meta$players_h$number))) rtxt <- c(rtxt, "At least one home team player number is duplicated.")
                if (any(duplicated(rdata$dvw$meta$players_v$number) & !is.na(rdata$dvw$meta$players_v$number))) rtxt <- c(rtxt, "At least one visiting team player number is duplicated.")
                rosters_ok <- if (length(rtxt)) paste(rtxt, sep = " ") else TRUE
            }
            ## check lineups
            lineups_ok <- TRUE
            ## need to see non-NA, non-NULL entries in game_state
            for (pp in pseq) lineups_ok <- lineups_ok && notnn(game_state[[paste0("home_p", pp)]]) && notnn(game_state[[paste0("visiting_p", pp)]])
            ## also need to see >LUp lines for this set in plays2
            temp_set_idx <- rdata$dvw$plays2$set_number %eq% game_state$set_number
            lineups_ok <- lineups_ok && (sum(temp_set_idx & grepl("^\\*P[[:digit:]]+>LUp", rdata$dvw$plays2$code), na.rm = TRUE) >= 1)
            lineups_ok <- lineups_ok && (sum(temp_set_idx & grepl("^\\*z[[:digit:]]+>LUp", rdata$dvw$plays2$code), na.rm = TRUE) >= 1)
            lineups_ok <- lineups_ok && (sum(temp_set_idx & grepl("^aP[[:digit:]]+>LUp", rdata$dvw$plays2$code), na.rm = TRUE) >= 1)
            lineups_ok <- lineups_ok && (sum(temp_set_idx & grepl("^az[[:digit:]]+>LUp", rdata$dvw$plays2$code), na.rm = TRUE) >= 1)
            if (lineups_ok) {
                ltxt <- character()
                if (!app_data$is_beach) {
                    if (!notnn(game_state$home_setter_position)) ltxt <- c(ltxt, "The home setter has not been specified (or is not in the lineup).")
                    if (!notnn(game_state$visiting_setter_position)) ltxt <- c(ltxt, "The visiting setter has not been specified (or is not in the lineup).")
                    ## make sure libero is not listed on court
                    for (pp in pseq) {
                        if (!is.na(game_state$ht_lib1) && game_state$ht_lib1 >= 0 && !is.null(game_state[[paste0("home_p", pp)]]) && game_state[[paste0("home_p", pp)]] %eq% game_state$ht_lib1) ltxt <- c(ltxt, "Home team libero 1 is also listed in the on-court lineup.")
                        if (!is.na(game_state$ht_lib2) && game_state$ht_lib2 >= 0 && !is.null(game_state[[paste0("home_p", pp)]]) && game_state[[paste0("home_p", pp)]] %eq% game_state$ht_lib2) ltxt <- c(ltxt, "Home team libero 2 is also listed in the on-court lineup.")
                        if (!is.na(game_state$vt_lib1) && game_state$vt_lib1 >= 0 && !is.null(game_state[[paste0("visiting_p", pp)]]) && game_state[[paste0("visiting_p", pp)]] %eq% game_state$vt_lib1) ltxt <- c(ltxt, "Visiting team libero 1 is also listed in the on-court lineup.")
                        if (!is.na(game_state$vt_lib2) && game_state$vt_lib2 >= 0 && !is.null(game_state[[paste0("visiting_p", pp)]]) && game_state[[paste0("visiting_p", pp)]] %eq% game_state$vt_lib2) ltxt <- c(ltxt, "Visiting team libero 2 is also listed in the on-court lineup.")
                    }
                }
                lineups_ok <- if (length(ltxt)) paste(ltxt, sep = " ") else TRUE
            }
            courtref_ok <- video_media_ok <- TRUE
            if (app_data$with_video) {
                ## check courtref
                courtref_ok <- !is.null(detection_ref1()$court_ref)
                if (have_second_video && courtref_ok && is.null(detection_ref2()$court_ref)) courtref_ok <- "Use the 'Video setup' button to define the court reference for video 2."
                ## check video media. Need to know the dv_width and height and the video_width and height. BUT we don't get video width and height with youtube, so don't wait for it
                video_media_ok <- !is.null(input$dv_width) && !is.null(input$dv_height) && !is.na(input$dv_width) && !is.na(input$dv_height) && input$dv_width > 0 && input$dv_height > 0
                video_media_ok <- video_media_ok &&
                    (((current_video_src() == 1L && is_youtube_url(app_data$video_src)) || (current_video_src() == 2L && is_youtube_url(app_data$video_src2)))
                        ||
                        (!is.null(input$video_width) && !is.null(input$video_height) && !is.na(input$video_width) && !is.na(input$video_height) && input$video_width > 0 && input$video_height > 0))
                ##            cat("input$dv_width:", cstr(input$dv_width), "\ninput$dv_height:", cstr(input$dv_height), "\ninput$video_width:", cstr(input$video_width), "\ninput$video_height:", cstr(input$video_height), "\ninput$dv_width:", cstr(input$dv_width), "\ninput$dv_height:", cstr(input$dv_height), "\n")
            }
            ok <- teams_ok && isTRUE(lineups_ok) && isTRUE(rosters_ok) && isTRUE(courtref_ok) && video_media_ok
            meta_is_valid(ok)
            output$problem_ui <- renderUI({
                if (!ok) {
                    rally_state("fix required information before scouting can begin")
                    tags$div(class = "alert alert-info",
                             tags$h2("Information needed"),
                             tags$ul(
                                      if (!isTRUE(courtref_ok)) tags$li(if (is.character(courtref_ok)) courtref_ok else paste0("Use the '", if (have_second_video) "Video setup" else "Court reference", "' button to define the court reference.")),
                                      if (!teams_ok) tags$li("Use the 'Select teams' button to choose from existing teams, or 'Edit teams' to enter new ones."),
                                      if (!isTRUE(rosters_ok)) tags$li(if (is.character(rosters_ok)) paste0(rosters_ok, " ", collapse = ", "), "Use the 'Edit teams' button to enter or adjust the team rosters."),
                                      if (!isTRUE(lineups_ok)) tags$li(if (is.character(lineups_ok)) paste0(lineups_ok, " ", collapse = ", "), paste0("Use the 'Edit lineups' to enter or adjust the starting lineups", if (!is.null(game_state$set_number) && !is.na(game_state$set_number)) paste0(" for set ", game_state$set_number), ".")),
                                      if (!video_media_ok) tags$li("Wait for the video media information to be loaded.")
                                  ),
                             tags$hr(),
                             tags$p("Scouting cannot start until this information has been entered.")
                             )
                } else {
                    if (rally_state() == "fix required information before scouting can begin") rally_state(if (video_state$paused) app_data$click_to_start_msg else "click serve start")
                    NULL
                }
            })
        })

        ## exteral video control buttons
        observeEvent(input$video_pause, deal_with_pause(scout_modal_active = scout_modal_active, video_state = video_state, editing = editing, game_state = game_state, rdata = rdata, app_data = app_data))
        observeEvent(input$video_rew_10, do_video("rew", 10))
        observeEvent(input$video_rew_2, do_video("rew", 2))
        observeEvent(input$video_ff_2, do_video("ff", 2))
        observeEvent(input$video_ff_10, do_video("ff", 10))
        observeEvent(input$video_volume, if (!is.null(input$video_volume)) do_video("set_volume", input$video_volume))
        observeEvent(input$video_toggle_mute, do_video("toggle_mute"))

        ## handling of keyboard events outside of the scout entry bar
        observeEvent(input$controlkey, {
            if (!is.null(input$controlkey)) {
                k <- decode_keypress(input$controlkey, debug)
                ky <- k$key
                ## PREVIOUSLY we get the ascii code for the base key (i.e. upper-case letter, or number) AND the modifier
                ## so for "#" we'd get ky == utf8ToInt("3") (which is 51) plus mycmd[3] == "true" (shift)
                ## NOW for "#" we get ky == "#" plus mycmd[3] == "true" (shift)
                if ((k$class %eq% "modal-open" || grepl("scedit-modal", k$class)) && !is.null(editing$active) && editing$active %eq% "edit_shortcuts") {
                    sc_newvalue(key_as_text(k))
                    if (!ky %in% c("Alt", "Shift", "Meta", "Control")) output$scedit_out <- renderUI(tags$code(key_as_text(k)))
                } else if (grepl("playslist-tbl-i", k$id)) {
                    ## key pressed in playslist table
                    if (tolower(ky) %in% app_data$playstable_shortcuts$edit_code) {
                        edit_data_row()
                    } else if (tolower(ky) %in% app_data$playstable_shortcuts$delete_code) {
                        delete_data_row()
                    } else if (tolower(ky) %in% app_data$playstable_shortcuts$insert_code) {
                        insert_data_row("above")
                    } else if (tolower(ky) %in% c(app_data$playstable_shortcuts$up, app_data$playstable_shortcuts$down)) {
                        ## navigate up/down in playslist table
                        playslist_mod$select(playslist_mod$current_row() + if (tolower(ky) == app_data$playstable_shortcuts$up) -1L else 1L)
                    } else if (tolower(ky) %eq% app_data$playstable_shortcuts$switch_windows && app_data$scout_mode == "type") {
                        focus_to_scout_bar() ## go to scout bar
                        playslist_mod$redraw_select("last") ## change redraw behaviour (keep the last row selected, including when new row added)
                        playslist_mod$select_last() ## select last row
                    } else if (ky %in% app_data$playstable_shortcuts$go_to_time) {
                        ## video go to currently-selected event
                        ridx <- playslist_mod$current_row()
                        vt <- if (!is.na(ridx) && !is.na(ridx)) rdata$dvw$plays2$video_time[ridx] else NA
                        if (!is.null(vt) && !is.na(vt)) {
                            if (debug > 1) cat("jumping to video time: ", vt, "\n")
                            do_video("set_time", rebase_time(vt, time_to = current_video_src(), time_from = 1, rdata = rdata)) ## TODO check that this works when viewing video 2
                        }
                    }
                } else {
                    if (ky %eq% "Escape") {
                        ## esc
                        if (isTRUE(scout_modal_active())) {
                            ## if we have a scouting modal showing, treat this as cancel and rewind
                            do_cancel_rew()
                        } else if (courtref_active()) {
                            ## do nothing
                        } else if (!is.null(editing$active) && editing$active %eq% "rally_review") {
                            do_cancel_rally_review(editing = editing, app_data = app_data)
                        } else if (is.null(editing$active) || !editing$active %in% "teams") {
                            if (grepl("scout_in", k$id) && "escape" %in% app_data$shortcuts$pause) {
                                ## wackiness here if we want to use the escape key as a pause shortcut
                                ## let the scout shortcut code handle it
                            } else {
                                do_unpause <- !is.null(editing$active) && editing$active %eq% "admin" && app_data$with_video
                                do_focus_to_playslist <- !is.null(editing$active) && editing$active %in% c("delete", "edit") && app_data$scout_mode == "type"
                                editing$active <- NULL
                                removeModal()
                                if (do_unpause) do_video("play")
                                if (do_focus_to_playslist) focus_to_playslist()
                            }
                        }
                    } else if (ky %eq% "Enter") {
                        ## enter
                        if (!is.null(editing$active) && editing$active %eq% "rally_review") {
                            ## TODO we might have another race condition here where the text input does not update in the shiny server in time TODO CHECK
                            focus_to_modal_element("redit_ok", highlight_all = FALSE)
                            apply_rally_review(editing = editing, rally_codes = rally_codes, game_state = game_state, input = input, rdata = rdata, app_data = app_data)
                        } else if (!is.null(editing$active) && !editing$active %eq% "teams") {
                            ## if editing, treat as update
                            ## but not for team editing, because pressing enter in the DT fires this too
                            ## if this is the code editing modal, we need to focus out of the text entry box first otherwise changes there won't be seen in the corresponding input$xyz variable
                            if (editing$active %in% c("insert below", "insert above", "edit")) {
                                ## if the user has pressed enter quickly after editing text in the code_entry field (without tabbing out of that, or clicking on the commit button) the input$code_entry variable will not have been updated
                                ## workaround: manually collect all the inputs and send them in a different input variable, and then
                                ##   trigger do_edit_commit on that (yuck)
                                dojs("Shiny.setInputValue('code_entries', {'text': $('#code_entry').val(),
                                                                           'team': $('#code_entry_team').val(),
                                                                           'number': $('#code_entry_number').val(),
                                                                           'skill': $('#code_entry_skill').val(),
                                                                           'type': $('#code_entry_type').val(),
                                                                           'eval': $('#code_entry_eval').val(),
                                                                           'combo': $('#code_entry_combo').val(),
                                                                           'target': $('#code_entry_target').val(),
                                                                           'start_zone': $('#code_entry_start_zone').val(),
                                                                           'end_zone': $('#code_entry_end_zone').val(),
                                                                           'end_subzone': $('#code_entry_end_subzone').val(),
                                                                           'skill_type': $('#code_entry_skill_type').val(),
                                                                           'num_players': $('#code_entry_num_players').val(),
                                                                           'special': $('#code_entry_special').val(),
                                                                           'custom': $('#code_entry_custom').val() });")
                            } else if (editing$active %eq% "delete") {
                                do_delete_code()
                            }
                        } else if (isTRUE(scout_modal_active())) {
                            ## if we have a scouting modal showing, and a valid accept_fun entry, run that function
                            if (!is.null(accept_fun())) try(get(accept_fun(), mode = "function")())
                        }
                        ## need to stop this propagating to the browser, else it risks e.g. re-firing the most recently used button - done in UI code
                    } else if (ky %in% app_data$shortcuts$hide_popup) {
                        ## temporarily hide the modal, so the video can be seen
                        ## but only for the admin, lineup modal or the ones that pop up during the rally, not the editing modals for teams or rosters
                        if (is.null(editing$active) || editing$active %in% c("admin", "change starting lineup")) hide_popup(review_pane_active())
                    } else if (ky %in% c(app_data$shortcuts$pause, app_data$shortcuts$pause_no_popup)) {
                        ## only accept this if we are not doing a courtref, not editing, or it's the admin modal being shown
                        if ((is.null(editing$active) || editing$active %eq% "admin") && !courtref_active()) {
                            ## video pause/unpause
                            ## Q (uppercase) does just pause, with no admin modal
                            deal_with_pause(scout_modal_active = scout_modal_active, video_state = video_state, editing = editing, game_state = game_state, rdata = rdata, app_data = app_data, show_modal = !ky %in% app_data$shortcuts$pause_no_popup)
                        }
                    } else if (ky %eq% "Tab") {
                        if (grepl("scout_in", k$id)) {
                            ## tab from scout bar, go to playslist table
                            ## currently handled in switch_windows shortcut below, TODO rationalize this
                        } else if (app_data$scout_mode == "type" && is.null(editing$active)) {
                            ## otherwise (noting that we are not in a modal and not in the playslist table here) if we are in typing mode, switch to the input bar
                            focus_to_scout_bar()
                        }
                    } else if (is.null(editing$active) && !courtref_active()) {
                        ## none of these should be allowed to happen if we are e.g. editing lineups or teams or doing the court ref
                        if (ky %in% app_data$shortcuts$undo) {
                            ## undo
                            do_undo()
                        } else if (ky %in% app_data$shortcuts$switch_video) {
                            ## switch video
                            do_switch_video(have_second_video = have_second_video, current_video_src = current_video_src, rdata = rdata, app_data = app_data, video_state = video_state)
                        } else if (ky %in% app_data$shortcuts$contact) {
                            ## player contact with the ball
                            do_contact()
                        } else if (ky %in% app_data$shortcuts$video_faster) {
                            ## update the slider, and the observer will see it and send the video command
                            if (!is.null(input$playback_rate) && !is.na(input$playback_rate)) updateSliderInput(session, "playback_rate", value = input$playback_rate + 0.1)
                        } else if (ky %in% app_data$shortcuts$video_slower) {
                            if (!is.null(input$playback_rate) && !is.na(input$playback_rate) && isTRUE(input$playback_rate > 0.1)) updateSliderInput(session, "playback_rate", value = max(input$playback_rate - 0.1, 0.1, na.rm = TRUE))
                        } else if (ky %in% unlist(app_data$shortcuts[grepl("^video_(forward|rewind)", names(app_data$shortcuts))])) {
                            if (is.null(editing$active)) {
                                ## video forward/backward nav
                                ## same as for other ovscout interface, although the fine control is not needed here?
                                vidcmd <- if (ky %in% unlist(app_data$shortcuts[grepl("^video_rewind", names(app_data$shortcuts))])) "rew" else "ff"
                                dur <- if (ky %in% unlist(app_data$shortcuts[grepl("^video_(forward|rewind)_10", names(app_data$shortcuts))])) 10 else if (ky %in% unlist(app_data$shortcuts[grepl("^video_(forward|rewind)_0.1", names(app_data$shortcuts))])) 0.1 else if (ky %in% unlist(app_data$shortcuts[grepl("^video_(forward|rewind)_1_30", names(app_data$shortcuts))])) 1/30 else 2
                                do_video(vidcmd, dur)
                            }
                        }
                    }
                }
            }
        })
        observeEvent(input$controlkeyup, {
            if (!is.null(input$controlkeyup)) {
                k <- decode_keypress(input$controlkeyup, debug)
                if (debug > 2) cat("control key up: ", cstr(k), "\n")
                if (k$key %in% app_data$shortcuts$hide_popup) {
                    ## z
                    ## re-show the modal after temporarily hiding
                    unhide_popup(review_pane_active())
                }
            }
        })

        ## deal with what's being typed into the scout entry box
        observeEvent(input$scout_shortcut, {
            if (!is.null(input$scout_shortcut)) {
                if (debug > 2) cat("scout shortcut:", input$scout_shortcut, "\n")
                if (input$scout_shortcut %in% c("pause", "pause_no_popup")) {
                    ## handle pause here rather than in the main keydown code, because by default we want to use the escape key as a pause shortcut
                    if (!courtref_active()) {
                        if (!is.null(editing$active) && editing$active %eq% "admin") {
                            dismiss_admin_modal(editing = editing, scout_mode = app_data$scout_mode)
                        } else {
                            if (video_state$paused) {
                                ## we are paused
                                do_video("play")
                            } else {
                                do_video("pause")
                                if (input$scout_shortcut == "pause") {
                                    editing$active <- "admin"
                                    show_admin_modal(game_state = game_state, dvw = rdata$dvw)
                                }
                            }
                        }
                    }
                } else if (input$scout_shortcut %in% c("video_forward_2", "video_forward_10", "video_rewind_2", "video_rewind_10")) {
                    by <- as.numeric(sub("[^[:digit:]]+", "", input$scout_shortcut))
                    do_video(if (grepl("forward", input$scout_shortcut)) "ff" else "rew", by)
                } else if (input$scout_shortcut %in% c("assign_point_top", "assign_point_bottom")) {
                    ## any code remaining in the scout bar should be applied before actually ending the rally and assigning the point
                    rally_won_code <- if (input$scout_shortcut == "assign_point_top") {
                                          if (game_state$home_team_end == "upper") "*p" else "ap"
                                      } else {
                                          if (game_state$home_team_end == "upper") "ap" else "*p"
                                      }
                    ## add this to the end of whatever is in the scout bar (which might be empty) and process the lot
                    dojs(paste0("Shiny.setInputValue('scout_input_leftovers', scout_in_el.val() + ' ", rally_won_code, "', { priority: 'event' });"))
                    ## and then things are handled in the observeEvent(input$scout_input_leftovers, { ... }) block below
                } else if (input$scout_shortcut %in% c("undo")) {
                    do_undo()
                } else if (input$scout_shortcut %in% c("switch_windows")) {
                    ## switch to the playslist table
                    focus_to_playslist()
                    playslist_mod$redraw_select("keep") ## keep whatever row is selected when the table is re-rendered
                } else if (input$scout_shortcut %eq% "save_file") {
                    dojs("$('#save_rds_button')[0].click();")
                    ## saves, but in firefox at least the download dropdown dialog is shown. Programmatically focusing back to the scout bar does not seem to get rid of it. But you can press escape to dismiss it
                }
            }
        })

        observeEvent(input$scout_input_leftovers, {
            ## we've assigned the point (either by scout bar shortcut or button)
            ## input$scout_input_leftovers contains whatever was in the scout bar plus the appropriate point assignment code
            handle_scout_codes(input$scout_input_leftovers)
        })

        ## handle the buttons around the scout bar
        observeEvent(input$undoType, do_undo())

        observeEvent(input$pt_home, {
            ## add this to the end of whatever is in the scout bar (which might be empty) and process the lot
            dojs(paste0("Shiny.setInputValue('scout_input_leftovers', scout_in_el.val() + ' *p', { priority: 'event' });"))
        })

        observeEvent(input$pt_away, {
            ## add this to the end of whatever is in the scout bar (which might be empty) and process the lot
            dojs(paste0("Shiny.setInputValue('scout_input_leftovers', scout_in_el.val() + ' ap', { priority: 'event' });"))
        })

        handle_scout_codes <- function(codes) {
            ## split on spaces
            codes <- strsplit(str_trim(codes), "[[:space:]]+")[[1]]
            ## also get the time stamps
            keypress_times <- get_scout_input_times(input$scout_input_times)
            ## and split on spaces
            if (!is.null(keypress_times)) keypress_times <- split(keypress_times, cumsum(keypress_times$key %eq% " "))
            for (i in seq_along(codes)) {
                code <- codes[i]
                if (grepl("^[TpczPC]", code)) code <- paste0("*", code)
                cat("to process: ", code, "\n")
                if (grepl("^(>|\\*T|aT|\\*p|ap|\\*c|ac)", code) || ## timeout, point assignment, sub
                    grepl("^[a\\*]C[[:digit:]]+[:\\.][[:digit:]]+", code) || ## substitution ensuring it can't be an attack combo code
                    (grepl("^[a\\*]P[[:digit:]]", code) && !isTRUE(game_state$rally_started))) { ## Px can be a setter assignment or an attack combo code ("P2" is particularly ambiguous). Treat as setter assignment if the rally has not yet started
                    res <- handle_non_skill_code(code, process_rally_end = FALSE) ## FALSE so that this does not automatically handle end-of-rally ap, *p codes
                    if (code %in% c("*p", "ap")) {
                        review_rally(editing = editing, app_data = app_data, rally_codes = rally_codes)
                    }
                    if (!res$ok) {
                        ## TODO something
                    } else if (isTRUE(res$end_of_set)) {
                        ## handle_non_skill_code will have called rally_ended if appropriate, which also detects end of set (shows the modal to confirm)
                        ## what else needs to happen? TODO
                    }
                } else if (grepl("^[a\\*]?L", code)) {
                    ## starting lineup
                    ## should already have been handled, should not get here
                } else if (grepl("^[a\\*]?S$", code)) {
                    ## set serving team
                    srv <- if (grepl("^a", code)) "a" else "*"
                    if (!game_state$serving %eq% srv) {
                        game_state$serving <- srv
                        game_state$current_team <- game_state$serving
                        populate_server(game_state)
                    }
                } else if (grepl("^[a\\*]z[[:digit:]]", code)) {
                    ## setter position, TODO test
                    pos <- suppressWarnings(as.integer(substr(code, 2, 2)))
                    if (!is.na(pos)) {
                        if (grepl("^a", code)) game_state$visiting_setter_position <- pos else game_state$home_setter_position <- pos
                        rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(code, game_state = game_state, dvw = rdata$dvw)))
                    }
                } else {
                    home_setter_num <- game_state[[paste0("home_p", game_state$home_setter_position)]]
                    visiting_setter_num <- game_state[[paste0("visiting_p", game_state$visiting_setter_position)]]
                    code <- augment_code_attack_details(code, game_state = game_state, opts = rdata$options) ## deal with code shorthands
                    ## add to rally codes
                    newcode <- sub("~+$", "", ov_code_interpret(code, attack_table = rdata$options$attack_table, compound_table = rdata$options$compound_table, default_scouting_table = rdata$options$default_scouting_table, home_setter_num = home_setter_num, visiting_setter_num = visiting_setter_num, serving_team = game_state$serving))
                    cat("code after interpretation:", newcode, "\n")
                    ## code can be length > 1 now, because the scout might have entered a compound code
                    ptemp <- parse_code_minimal(newcode) ## convert to a list of tibble row(s)
                    this_clock_time <- time_but_utc()
                    this_video_time <- NA_real_
                    ## use clock and video times from the input$scout_input_times time-logged keypresses if we can
                    this_keypress_times <- NULL ## default
                    if (!is.null(keypress_times)) {
                        ## TODO check case sensitivity on this, if we use e.g. 'a' but remap it to 'A' via the key remapping
                        this_skill <- if (length(ptemp) > 0) ptemp[[1]]$skill else NA_character_ ## for compound codes, we use the first one
                        if (this_skill %eq% "E") {
                            ## set skill, so need to look for either the "E" key or "K" key for a setter call
                            if (any(tolower(keypress_times[[i]]$key) %eq% "e")) {
                                this_keypress_times <- keypress_times[[i]][tolower(keypress_times[[i]]$key) %eq% "e", ]
                            } else if (any(tolower(keypress_times[[i]]$key) %eq% "k")) {
                                ## setter call
                                this_keypress_times <- keypress_times[[i]][tolower(keypress_times[[i]]$key) %eq% "k", ]
                            }
                        } else if (this_skill %eq% "A") {
                            ## look for either the "A" key or combo code
                            cmb_idx <- NULL
                            if (is.data.frame(rdata$options$attack_table) && nrow(rdata$options$attack_table) > 0) {
                                temp <- na.omit(stringr::str_locate(tolower(paste0(keypress_times[[i]]$key, collapse = "")), tolower(rdata$options$attack_table$code))[, 1])
                                if (length(temp) > 0) cmb_idx <- temp[1]
                            }
                            if (any(tolower(keypress_times[[i]]$key) %eq% "a")) {
                                this_keypress_times <- keypress_times[[i]][tolower(keypress_times[[i]]$key) %eq% "a", ]
                            } else if (length(cmb_idx) == 1) {
                                ## attack combo code
                                this_keypress_times <- keypress_times[[i]][cmb_idx, ]
                            }
                        } else {
                            ## match key to skill
                            this_keypress_times <- keypress_times[[i]][tolower(keypress_times[[i]]$key) %eq% tolower(this_skill), ]
                        }
                    }
                    if (!is.null(this_keypress_times) && nrow(this_keypress_times) == 1) {
                        this_clock_time <- this_keypress_times$time
                        this_video_time <- this_keypress_times$video_time
                    }
                    for (temp in ptemp) {
                        if (!is.null(temp)) {
                            ## should not be NULL, that's only for non-skill rows
                            rc <- rally_codes()
                            newrc <- code_trow(team = temp$team, pnum = temp$pnum, skill = temp$skill, tempo = temp$tempo, eval = temp$eval, combo = temp$combo, target = temp$target, sz = temp$sz, ez = temp$ez, esz = temp$esz, start_zone_valid = TRUE, endxy_valid = TRUE, t = this_video_time, time = this_clock_time, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)
                            ## update the preceding rally_codes rows if new info has been provided
                            prev_touch <- NULL
                            nrc <- nrow(rc)
                            if (nrc > 0) {
                                rc[nrc, ] <- transfer_scout_row_details(from = newrc, to = rc[nrc, ])
                                ## equivalently but less efficiently rc <- transfer_scout_details(from_row = newrc, to_df = rc, row_idx = nrc + 1L, which = -1L, dvw = rdata$dvw)
                                prev_touch <- rc[nrc, ]
                            }
                            rally_codes(bind_rows(rc, newrc)) ## start_x = NA_real_, start_y = NA_real_
                            ## update game state and other things
                            ## we also update rally_state here (even though in scout_mode = "type" we don't use them) because having them set will ease things if the scout switches from typing to clicking
                            if (temp$skill %in% c("S", "R", "E", "A", "B", "D", "F")) game_state$rally_started <- TRUE ## technically only need to check serve here, but since the scout can enter arbitrary things we will just blanket all of them
                            if (temp$skill %eq% "S") {
                                game_state$serving <- temp$team
                                game_state$current_team <- other(temp$team)
                                rally_state("click serve end")
                            } else if (temp$skill %eq% "R") {
                                ## if R/ then current team is the serving team, otherwise it's the receiving team
                                if (temp$eval %eq% "/") {
                                    game_state$current_team <- game_state$serving
                                    rally_state("click freeball end point") ## we would treat next contact as a freeball dig if we were click-scouting
                                } else {
                                    game_state$current_team <- other(game_state$serving)
                                    rally_state("click second contact")
                                }
                            } else if (temp$skill %eq% "E") {
                                rally_state("click third contact")
                            } else if (temp$skill %eq% "A") {
                                rally_state("click attack end point")
                                if (!temp$eval %eq% "!") {
                                    ## next touch will be by other team
                                    game_state$current_team <- other(game_state$current_team)
                                    ## this is a bit out of whack with the click-scouting flow, because in that we would already have clicked the attack end point before assigning the ! outcome. Needs testing TODO
                                }
                            } else if (temp$skill %eq% "B") {
                                rally_state("click attack end point")
                                if (!temp$eval %eq% "!") {
                                    ## blocked back to the attacking team. With some scouts B- would also indicate this
                                    game_state$current_team <- other(game_state$current_team)
                                }
                            } else if (temp$skill %eq% "D") {
                                if (!temp$eval %eq% "/") {
                                    rally_state("click freeball end point")
                                    game_state$current_team <- other(game_state$current_team)
                                } else if (isTRUE(rdata$options$transition_sets)) {
                                    rally_state("click second contact")
                                } else {
                                    rally_state("click third contact")
                                }
                            } else if (temp$skill %eq% "F") {
                                ## freeball. If the opposing team made the preceding touch then it's a freeball dig
                                try({
                                    if (!is.null(prev_touch) && isTRUE(prev_touch$team != temp$team)) {
                                        ## this was a freeball dig
                                        game_state$current_team <- other(game_state$current_team)
                                    }
                                })
                            } ## game_state$current_team will remain as it is unless changed above
                        }
                    }
                }
            }
        }
        observeEvent(input$scout_input, {
            codes <- input$scout_input
            cat("code is: "); print(codes)
            if (grepl("^[a\\*]?L", codes)) {
                ## handle these here not in handle_scout_codes because the whitespace splitting behaviour is different
                assign_lineup_from_manual(codes, rdata = rdata, game_state = game_state, app_data = app_data)
            } else {
                handle_scout_codes(codes)
            }
        })

        observeEvent(input$redit_ok, apply_rally_review(editing = editing, rally_codes = rally_codes, game_state = game_state, input = input, rdata = rdata, app_data = app_data))
        observeEvent(input$redit_cancel, do_cancel_rally_review(editing = editing, app_data = app_data))

        ## options
        ## TODO other prefs to add: scout_mode, season_dir, auto_save_dir
        ## not playback_rate, that's set directly by the slider but saved along with the other prefs anyway
        observeEvent(input$preferences, {
            editing$active <- "preferences"
            show_prefcuts_modal(prefs = prefs, opts = rdata$options)
        })
        observeEvent(input$just_cancel, {
            editing$active <- NULL
            removeModal()
            if (app_data$scout_mode == "type") focus_to_scout_bar() ## TODO check
        })
        observeEvent(input$prefs_save, {
            thisprefs <- list(scout_name = if (is.null(input$prefs_scout) || is.na(input$prefs_scout)) "" else input$prefs_scout,
                              show_courtref = isTRUE(input$prefs_show_courtref),
                              scoreboard = isTRUE(input$prefs_scoreboard),
                              pause_on_type = if (is.null(input$prefs_pause_on_type) || is.na(as.integer(input$prefs_pause_on_type))) 0 else as.integer(input$prefs_pause_on_type),
                              ball_path = isTRUE(input$prefs_ball_path),
                              playlist_display_option = input$prefs_playlist_display_option,
                              review_pane = input$prefs_review_pane)
            if (!is.null(input$playback_rate)) thisprefs$playback_rate <- input$playback_rate ## add playback rate here, but it's set directly by the slider not by this popup
            this_opts <- list(end_convention = input$scopts_end_convention,
                              nblockers = input$scopts_nblockers,
                              default_nblockers = as.numeric(input$scopts_default_nblockers),
                              transition_sets = input$scopts_transition_sets,
                              attacks_by = input$scopts_attacks_by,
                              zones_cones = input$scopts_zones_cones,
                              team_system = input$scopts_team_system,
                              setter_dump_code = if (nzchar(input$scopts_setter_dump_code)) input$scopts_setter_dump_code else ov_scouting_options()$setter_dump_code,
                              second_ball_attack_code = if (nzchar(input$scopts_second_ball_attack_code)) input$scopts_second_ball_attack_code else ov_scouting_options()$second_ball_attack_code,
                              overpass_attack_code = if (nzchar(input$scopts_overpass_attack_code)) input$scopts_overpass_attack_code else ov_scouting_options()$overpass_attack_code)

            ## TODO this_click_sc <- xxx ## click-mode shortcuts
            ## TODO this_type_sc <- xxx ## type-mode shortcuts
            ## TODO this_pl_sc <- xxx ## playslist shortcuts

            ## save prefs
            ## TODO load the shortcuts components and just re-save them?
            tryCatch({
                saveRDS(list(app_prefs = thisprefs##, click_shortcuts = this_click_sc, type_shortcuts = this_type_sc, playstable_shortcuts = this_pl_sc
                             ),
                        file = app_data$options_file)
            }, error = function(e) warning("could not save preferences to file"))
            ## transfer to active prefs object
            for (nm in names(thisprefs)) prefs[[nm]] <- thisprefs[[nm]]
            ## apply any that require immediate action
            if (is.null(rdata$dvw$meta$more$scout) || is.na(rdata$dvw$meta$more$scout) || !nzchar(rdata$dvw$meta$more$scout)) rdata$dvw$meta$more$scout <- prefs$scout_name

            ## save scouting options
            tryCatch({ saveRDS(this_opts, file = app_data$scouting_options_file) }, error = function(e) warning("could not save scouting convention preferences to file"))
            ## apply
            for (nm in names(this_opts)) rdata$options[[nm]] <- this_opts[[nm]]

            editing$active <- NULL
            removeModal()
            if (app_data$scout_mode == "type") focus_to_scout_bar() ## TODO check
        })

        overlay_points <- reactiveVal(NULL)
        overlay_court_lines <- reactive({
            if (!is.null(detection_ref()$court_ref)) {
                oxy <- ovideo::ov_overlay_data(zones = FALSE, serve_zones = FALSE, space = "image", court_ref = detection_ref()$court_ref, crop = TRUE)$courtxy
                dplyr::rename(oxy, image_x = "x", image_y = "y")
            } else {
                NULL
            }
        })

        observe({
            if (!isTRUE(input$overlay_nocanvas > 0)) {
                ## draw directly with canvas
                w <- input$dv_width; w <- if (identical(w, "auto")) 600L else as.numeric(w)
                h <- input$dv_height; h <- if (identical(h, "auto")) 400L else as.numeric(h)
                ## these are NULL on startup, so make sure they are populated before drawing
                if (length(w) == 1 && !is.na(w) && length(h) == 1 && !is.na(h)) {
                    cc <- canvas_drawing$new(id = "video_overlay_canvas", width = w, height = h, on_fail = "Shiny.setInputValue('overlay_nocanvas', 1);")
                    ## if context fails, fall back to base plotting
                    if (isTRUE(prefs$show_courtref) && !is.null(overlay_court_lines())) {
                        oxy <- overlay_court_lines()
                        ## account for aspect ratios
                        oxy$image_x <- ar_fix_x(oxy$image_x, input = input, current_video_src = current_video_src, app_data = app_data)
                        oxy$xend <- ar_fix_x(oxy$xend, input = input, current_video_src = current_video_src, app_data = app_data)
                        oxy$image_y <- ar_fix_y(oxy$image_y, input = input, current_video_src = current_video_src, app_data = app_data)
                        oxy$yend <- ar_fix_y(oxy$yend, input = input, current_video_src = current_video_src, app_data = app_data)
                        cc$lines(x0 = oxy$image_x, y0 = oxy$image_y, x1 = oxy$xend, y1 = oxy$yend, col = app_data$styling$court_lines_colour, unit = "npc")
                    }
                    if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                        ixy <- setNames(crt_to_vid(overlay_points(), detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data), c("x", "y"))
                        if (any(overlay_points()$valid)) {
                            cc$circles(x = ixy$x[overlay_points()$valid], y = ixy$y[overlay_points()$valid], r = 0.01, col = "white", fill_col = "dodgerblue", unit = "npc")
                        }
                        if (!all(overlay_points()$valid)) {
                            cc$circles(x = ixy$x[!overlay_points()$valid], y = ixy$y[!overlay_points()$valid], r = 0.01, col = "white", fill_col = "firebrick", unit = "npc")
                        }
                    }
                    cc$draw()
                }
            } else {
                ## do the overlay by base plotting, but this is slow
                output$video_overlay <- renderPlot({
                    ## test - red diagonal line across the overlay plot
                    ##ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(.data$x, .data$y)) + geom_path(color = "red") + gg_tight
                    opar <- par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
                    plot(c(0, 1), c(0, 1), xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = NA, ylab = NA, axes = FALSE, xaxs = "i", yaxs = "i")
                    if (isTRUE(prefs$show_courtref) && !is.null(overlay_court_lines())) {
                        oxy <- overlay_court_lines()
                        ## account for aspect ratios
                        oxy$image_x <- ar_fix_x(oxy$image_x, input = input, current_video_src = current_video_src, app_data = app_data)
                        oxy$xend <- ar_fix_x(oxy$xend, input = input, current_video_src = current_video_src, app_data = app_data)
                        oxy$image_y <- ar_fix_y(oxy$image_y, input = input, current_video_src = current_video_src, app_data = app_data)
                        oxy$yend <- ar_fix_y(oxy$yend, input = input, current_video_src = current_video_src, app_data = app_data)
                        segments(x0 = oxy$image_x, y0 = oxy$image_y, x1 = oxy$xend, y1 = oxy$yend, col = app_data$styling$court_lines_colour)
                    }
                    if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                        ixy <- setNames(crt_to_vid(overlay_points(), detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data), c("x", "y"))
                        points(ixy$x[overlay_points()$valid], ixy$y[overlay_points()$valid], bg = "dodgerblue", pch = 21, col = "white", cex = 2.5)
                        points(ixy$x[!overlay_points()$valid], ixy$y[!overlay_points()$valid], bg = "firebrick", pch = 21, col = "white", cex = 2.5)
                    }
                    par(opar)
                }, bg = "transparent", width = as.numeric(input$dv_width), height = as.numeric(input$dv_height))
            }
        })

        ## use this function to set the coordinate in the current row (e.g. we are editing a row, and we've clicked a court location on the video pane or court diagram
        coord_edit_row <- reactiveVal(NULL) ## which row we are editing the coords of
        set_coord <- function(ridx, which, xy) {
            if (missing(ridx)) ridx <- isolate(coord_edit_row())
            do_set_coord(ridx = ridx, which = which, xy = xy)
            if (!is.null(ridx) && which == "end") do_after_coord_edit()
        }
        clear_coord <- function(ridx, which) {
            if (missing(ridx)) ridx <- isolate(coord_edit_row())
            do_set_coord(ridx = ridx, which = which, xy = NULL)
        }
        do_set_coord <- function(ridx, which, xy, around = TRUE) {
            ## around = TRUE means to also set the reception coord if we are setting serve, etc
            if (!is.null(ridx)) {
                which <- match.arg(which, c("start", "mid", "end"))
                if (is.null(xy)) xy <- list(x = NA_real_, y = NA_real_)
                xcol <- paste0(which, "_x")
                ycol <- paste0(which, "_y")
                ccol <- paste0(which, "_coordinate")
                if (ridx < 1 || ((ridx > nrow(rdata$dvw$plays2)) && ((ridx - nrow(rdata$dvw$plays2)) > nrow(rally_codes())))) {
                    ## ridx isn't valid with respect to either rdata$dvw$plays2 or rally_codes. Shouldn't happen but check just in case
                } else {
                    ridx0 <- ridx ## value that indexes into plays2 or (if it's larger than nrow(plays2)) into rally_codes. Save it because we'll need to pass it to the do_set_coord calls in the `around` block
                    if (ridx <= nrow(rdata$dvw$plays2)) {
                        ## editing in the plays2 dataframe
                        rdata$dvw$plays2$rally_codes[[ridx]][[xcol]] <- xy$x
                        rdata$dvw$plays2$rally_codes[[ridx]][[ycol]] <- xy$y
                        rdata$dvw$plays2[[ccol]][ridx] <- dv_xy2index(as.numeric(xy$x), as.numeric(xy$y))
                        temp_nr <- nrow(rdata$dvw$plays2); temp_skill <- rdata$dvw$plays2$skill ## for checking `around`, below
                    } else if ((ridx - nrow(rdata$dvw$plays2)) <= nrow(rally_codes())) {
                        ## editing in the current rally that is not yet part of plays2
                        rc <- rally_codes()
                        ridx <- ridx - nrow(rdata$dvw$plays2)
                        ## prev_xy <- c(rc[[xcol]][ridx], rc[[ycol]][ridx]) ## previous value, about to overwrite this
                        rc[[xcol]][ridx] <- xy$x
                        rc[[ycol]][ridx] <- xy$y
                        ## also need to change the coords in game_state
                        rc$game_state[[ridx]][xcol] <- xy$x
                        rc$game_state[[ridx]][ycol] <- xy$y
                        rally_codes(rc)
                        temp_nr <- nrow(rc); temp_skill <- rc$skill ## for checking `around`, below
                    }
                    if (around) {
                        if (isTRUE(temp_skill[ridx] == "S" && ridx < temp_nr && temp_skill[ridx + 1] == "R") ||
                            isTRUE(temp_skill[ridx] == "A" && ridx < temp_nr && temp_skill[ridx + 1] == "D")) {
                            ## if we are editing the serve coordinates, also change the reception coordinates to match; or if editing attack, also change dig
                            ## NOTE that if the scouting is by typing, or even from other scouting software, the coordinate conventions might be entirely different, hence TODO offer option to turn this "around" behaviour off
                            do_set_coord(ridx = ridx0 + 1, which = which, xy = xy, around = FALSE)
                        } else if (isTRUE(temp_skill[ridx] == "R" && ridx > 1 && temp_skill[ridx - 1] == "S") ||
                                   isTRUE(temp_skill[ridx] == "D" && ridx > 1 && temp_skill[ridx - 1] == "A")) {
                            ## editing reception, also change serve; or dig after attack
                            do_set_coord(ridx = ridx0 - 1, which = which, xy = xy, around = FALSE)
                        } else if (isTRUE(temp_skill[ridx] == "A" && ridx < (temp_nr - 1) && temp_skill[ridx + 1] == "B" && temp_skill[ridx + 2] == "D")) {
                            ## attack - block touch - dig
                            do_set_coord(ridx = ridx0 + 2, which = which, xy = xy, around = FALSE)
                        } else if (isTRUE(temp_skill[ridx] == "D" && ridx > 2 && temp_skill[ridx - 1] == "B" && temp_skill[ridx - 2] == "A")) {
                            ## dig after block touch after attack
                            do_set_coord(ridx = ridx0 - 2, which = which, xy = xy, around = FALSE)
                        }
                    }
                }
            }
        }
        observeEvent(input$edit_coord_clear, {
            if (editing$active %in% c("coord_click_start", "coord_click_mid", "coord_click_end")) {
                clear_coord(which = sub("coord_click_", "", editing$active))
                if (editing$active == "coord_click_start") {
                    output$code_edit_dialog <- renderUI(code_edit_dialog_content("coord_click_mid"))
                    editing$active <- "coord_click_mid"
                } else if (editing$active == "coord_click_mid") {
                    output$code_edit_dialog <- renderUI(code_edit_dialog_content("coord_click_end"))
                    editing$active <- "coord_click_end"
                } else {
                    do_after_coord_edit()
                }
            } else {
                do_after_coord_edit()
            }
        })
        observeEvent(input$edit_coord_cancel, do_after_coord_edit())
        do_after_coord_edit <- function() {
            editing$active <- NULL
            coord_edit_row(NULL)
            output$code_edit_dialog <- renderUI(NULL)
            refocus_to_ui(active_ui()) ## we just clicked away from the active UI element, so go back to it
        }

        courtxy <- reactiveVal(list(x = NA_real_, y = NA_real_)) ## keeps track of click locations (in court x, y space)
        loop_trigger <- reactiveVal(0L)
        observeEvent(input$video_click, priority = 99, {
            ## when video clicked, get the corresponding video time and trigger the loop
            flash_screen() ## visual indicator that click has registered
            ## calculate the normalized x,y coords
            this_click <- if (length(input$video_click) > 4) list(x = input$video_click[1] / input$video_click[3], y = 1 - input$video_click[2] / input$video_click[4])
            if (app_data$scout_mode == "type" || (!is.null(editing$active) && editing$active %in% c("coord_click_start", "coord_click_mid", "coord_click_end"))) {
                thisxy <- vid_to_crt(this_click, detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data)
                if (is.null(editing$active) || editing$active %eq% "coord_click_start") {
                    playslist_mod$redraw_select("keep") ## keep whatever row is selected when the table is re-rendered
                    set_coord(which = "start", xy = thisxy) ## first click is the start coord
                    output$code_edit_dialog <- renderUI(code_edit_dialog_content("coord_click_mid"))
                    editing$active <- "coord_click_mid"
                } else if (editing$active %eq% "coord_click_mid") {
                    playslist_mod$redraw_select("keep")
                    set_coord(which = "mid", xy = thisxy)
                    output$code_edit_dialog <- renderUI(code_edit_dialog_content("coord_click_end"))
                    editing$active <- "coord_click_end"
                } else if (editing$active %eq% "coord_click_end") {
                    playslist_mod$redraw_select("keep")
                    set_coord(which = "end", xy = thisxy)
                }
            } else {
                ## video time
                time_uuid <- uuid()
                game_state$current_time_uuid <- time_uuid
                click_time <- as.numeric(input$video_click[5])
                if (!is.na(click_time) && click_time >= 0) {
                    ## got the video time as part of the click packet, stash it
                    this_timebase <- current_video_src()
                    if (length(this_timebase) != 1 || !this_timebase %in% c(1, 2)) this_timebase <- 1
                    video_times[[time_uuid]] <<- round(rebase_time(click_time, time_from = this_timebase, rdata = rdata), 2) ## video times to 2 dec places
                } else {
                    ## invalid time received, ask again
                    warning("invalid click time\n")
                    do_video("get_time_fid", paste0(time_uuid, "@", current_video_src())) ## make asynchronous request, noting which video is currently being shown (@)
                }
                if (rally_state() != app_data$click_to_start_msg) courtxy(vid_to_crt(this_click, detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data))
                playslist_mod$redraw_select("last")
                loop_trigger(loop_trigger() + 1L)
                ## 6th element of input$video_click gives status of shift key during click
                shiftclick <- (length(input$video_click) > 5) && isTRUE(input$video_click[6] > 0)
                process_action(shiftclick)
                ## TODO MAYBE also propagate the click to elements below the overlay?
            }
        })
        observeEvent(court_inset$click(), {
            if (!is.na(court_inset$click()$x) && !is.na(court_inset$click()$y)) {
                flash_screen() ## visual indicator that click has registered
                if (app_data$scout_mode == "type" || (!is.null(editing$active) && editing$active %in% c("coord_click_start", "coord_click_mid", "coord_click_end"))) {
                    thisxy <- court_inset$click()
                    if (is.null(editing$active) || editing$active %eq% "coord_click_start") {
                        playslist_mod$redraw_select("keep") ## keep whatever row is selected when the table is re-rendered
                        set_coord(which = "start", xy = thisxy) ## first click is the start coord
                        output$code_edit_dialog <- renderUI(code_edit_dialog_content("coord_click_mid"))
                        editing$active <- "coord_click_mid"
                    } else if (editing$active %eq% "coord_click_mid") {
                        playslist_mod$redraw_select("keep")
                        set_coord(which = "mid", xy = thisxy)
                        output$code_edit_dialog <- renderUI(code_edit_dialog_content("coord_click_end"))
                        editing$active <- "coord_click_end"
                    } else if (editing$active %eq% "coord_click_end") {
                        playslist_mod$redraw_select("keep") ## keep whatever row is selected when the table is re-rendered
                        ## end coord
                        set_coord(which = "end", xy = thisxy)
                    }
                } else {
                    ## when court diagram clicked, treat it as if it were a click on the video: get the corresponding video time and trigger the loop
                    time_uuid <- uuid()
                    game_state$current_time_uuid <- time_uuid
                    do_video("get_time_fid", paste0(time_uuid, "@", current_video_src())) ## make asynchronous request, noting which video is currently being shown (@1 or @2)
                    courtxy(court_inset$click())
                    playslist_mod$redraw_select("last")
                    loop_trigger(loop_trigger() + 1L)
                    process_action()
                }
            }
        })
        observeEvent(input$contact, {
            flash_screen() ## visual indicator that click has registered
            time_uuid <- uuid()
            game_state$current_time_uuid <- time_uuid
            click_time <- as.numeric(input$contact[1])
            ##cat("contact click_time: ", click_time, "\n")
            if (!is.na(click_time) && click_time >= 0) {
                ## got the video time as part of the click packet, stash it
                this_timebase <- current_video_src()
                if (length(this_timebase) != 1 || !this_timebase %in% c(1, 2)) this_timebase <- 1
                click_time <- round(rebase_time(click_time, time_from = this_timebase, rdata = rdata), 2) ## video times to 2 dec places
                video_times[[time_uuid]] <<- click_time
            } else {
                ## invalid time received, ask again
                ## note that we won't be able to get an x,y position below either
                click_time <- NULL
                warning("invalid click time\n")
                do_video("get_time_fid", paste0(time_uuid, "@", current_video_src())) ## make asynchronous request, noting which video is currently being shown (@1 or @2)
            }
            ## get xy position
            thisx <- thisy <- NA_real_
            if (isTRUE(app_data$extra_ball_tracking) && !is.null(click_time)) {
                ## extract from db
                try({
                    this <- tbl(extra_db_con, "ball_track") %>% dplyr::filter(abs(.data$video_time - click_time) < 0.25) %>% collect
                    if (nrow(this) > 1) this <- this[which.min(abs(this$video_time - click_time)), ]
                    if (nrow(this) == 1) {
                        thisx <- this$x
                        thisy <- this$y
                    }
                })
            }
            courtxy(data.frame(x = thisx, y = thisy))
            loop_trigger(loop_trigger() + 1L)
            process_action()
        })

        ## video times are a pain, because we get asynchronous replies from the browser via input$video_time
        video_times <- list()
        observeEvent(input$video_time, {
            ## when a time comes in, stash it under its uuid
            temp <- input$video_time
            this_timebase <- as.numeric(sub(".*@", "", temp))
            if (length(this_timebase) != 1 || !this_timebase %in% c(1, 2)) this_timebase <- current_video_src()
            if (length(this_timebase) != 1 || !this_timebase %in% c(1, 2)) this_timebase <- 1
            this_uuid <- sub("@.*", "", sub(".*&", "", temp))
            if (nzchar(this_uuid)) video_times[[this_uuid]] <<- round(rebase_time(as.numeric(sub("&.+", "", temp)), time_from = this_timebase, rdata = rdata), 2) ## video times to 2 dec places
        })

        ## rally_codes is a reactive that returns a tibble with columns team, pnum, skill, tempo, eval, combo, target, sz, ez, esz, x_type, num_p, special, custom, t, start_x, start_y, end_x, end_y
        ## rally_codes are the actions in the current rally

        ## modal things
        ## keep track of whether we have a modal up or not, so that pause behaviour can be modified
        scout_modal_active <- reactiveVal(FALSE)
        mcols <- reactive({
            ##if (isTRUE(prefs$review_pane) || isTRUE(app_data$extra_ball_tracking)) 8L else 12L
            12L
        })

        fatradio_class_uuids <- reactiveValues()
        attack_other_opts <- reactiveVal(NULL)

        ## helpers
        default_skill_tempo <- function(skill) rdata$options$default_scouting_table$tempo[rdata$options$default_scouting_table$skill == skill]
        default_skill_eval <- function(skill) rdata$options$default_scouting_table$evaluation_code[rdata$options$default_scouting_table$skill == skill]

        ## if we need the court xy position, but it doesn't exist, this function does something sensible. Prompts the user to enter it? Uses a placeholder position?
        resolve_courtxy <- function() {
            xy <- as.data.frame(courtxy())
            xy$valid <- TRUE
            ## if inputs are NA or missing, then valid is FALSE and placeholder point goes in middle of team's court (or middle of baseline if a serve)
            ## this should only ever be a single row?
            xy <- xy[1, , drop = FALSE]
            naidx <- is.na(xy$x) | is.na(xy$y)
            xy$valid[naidx] <- FALSE
            xy$x[naidx] <- 2.0
            if (rally_state() == "click serve start") {
                ## point on baseline
                xy$y[naidx] <- if ((game_state$current_team == "*" && game_state$home_team_end == "upper") || (game_state$current_team == "a" && game_state$home_team_end == "lower")) 6.5 else 0.5
            } else {
                ## middle of court
                xy$y[naidx] <- if ((game_state$current_team == "*" && game_state$home_team_end == "upper") || (game_state$current_team == "a" && game_state$home_team_end == "lower")) 5.0 else 2.0
            }
            xy
        }

        process_action_type_mode <- function(was_shift_click = FALSE) {
            ## to be used when scout_mode is "type"
            NULL
        }
        accept_fun <- reactiveVal(NULL) ## use this to determine what function should be run when the "Continue" button on a modal is clicked, or the enter key is used to shortcut it
        ## single click the video to register a tag location, or starting ball coordinates
        process_action <- function(was_shift_click = FALSE) {
            if (app_data$scout_mode == "type") return(process_action_type_mode(was_shift_click = was_shift_click))
            if (loop_trigger() > 0 && rally_state() != "fix required information before scouting can begin") {
                if (rally_state() == app_data$click_to_start_msg) {
                    if (meta_is_valid()) {
                        do_video("play")
                        rally_state("click serve start")
                    }
                } else if (rally_state() == "click serve start") {
                    ## click was the serve position
                    sxy <- resolve_courtxy()
                    game_state$start_x <- sxy$x[1]
                    game_state$start_y <- sxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    game_state$startxy_valid <- sxy$valid[1]
                    sxy$start_end <- "start"
                    overlay_points(sxy)
                    ## add placeholder serve code, will get updated on next click
                    sp <- if (!is.null(input$serve_preselect_player)) input$serve_preselect_player else if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    ## serve type should have been selected in the preselect
                    st <- if (!is.null(input$serve_preselect_type)) input$serve_preselect_type else default_skill_tempo("S")
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    ## video time might not have resolved yet if it is coming from the asynchronous handler, so add it after next click
                    rally_codes(bind_rows(rally_codes(), code_trow(team = game_state$serving, pnum = sp, skill = "S", tempo = st, sz = sz, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                    game_state$current_team <- other(game_state$serving)
                    game_state$rally_started <- TRUE
                    rally_state("click serve end")
                } else if (rally_state() == "click serve end") {
                    do_video("pause")
                    ## click was the end-of-serve position, either error or reception
                    sxy <- resolve_courtxy()
                    game_state$end_x <- sxy$x[1]
                    game_state$end_y <- sxy$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    game_state$endxy_valid <- sxy$valid[1]
                    sxy$start_end <- "end"
                    overlay_points(rbind(overlay_points(), sxy))
                    ## pop up to find either serve error, or passing player
                    ## passing player options
                    ## game_state$current_team here is the receiving team
                    pass_pl_opts <- guess_pass_player_options(game_state, dvw = rdata$dvw, system = rdata$options$team_system)
                    names(pass_pl_opts$choices) <- player_nums_to(pass_pl_opts$choices, team = game_state$current_team, dvw = rdata$dvw)
                    pass_pl_opts$choices <- c(pass_pl_opts$choices, Unknown = "Unknown")

                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    chc <- rdata$options$skill_tempo_map %>% dplyr::filter(.data$skill == "Serve") %>% mutate(tempo = sub(" serve", "", .data$tempo))
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
                    ## we pre-select either the passer, or the error type, depending on whether we thought it was an error or not
                    serve_outcome_initial_buttons <- make_fat_radio_buttons(
                        choices = c("Serve error" = "=", "Reception error (serve ace)" = "S#", "Reception in play" = "R~"),
                        input_var = "serve_initial_outcome",
                        selected = if (!is.na(guess_was_err)) "=" else "R~")
                    serve_error_type_buttons <- make_fat_radio_buttons(
                        choices = c("In net" = "=N", "Foot fault/referee call" = "=Z", "Out long" = "=O", "Out left" = "=L", "Out right" = "=R"),
                        selected = if (!is.na(guess_was_err)) guess_was_err else NA,
                        input_var = "serve_error_type", as_radio = "blankable")
                    passer_buttons <- make_fat_radio_buttons(choices = pass_pl_opts$choices, selected = pass_pl_opts$selected, input_var = "pass_player")
                    accept_fun("do_assign_serve_outcome")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL, width = app_data$styling$scout_modal_width, modal_halign = "left",
                                                   tags$p(tags$strong("Serve type:")),
                                                   do.call(fixedRow, lapply(serve_type_buttons, function(but) column(if (mcols() / length(serve_type_buttons) >= 2) 2 else 1, but))),
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
                    sxy <- resolve_courtxy()
                    game_state$start_x <- sxy$x[1]
                    game_state$start_y <- sxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    game_state$startxy_valid <- sxy$valid[1]
                    sxy$start_end <- "start"
                    overlay_points(sxy)
                    ## popup
                    ## TODO maybe also setter call here
                    ## allow user to override auto-assigned reception quality
                    passq <- guess_pass_quality(game_state, dvw = rdata$dvw)
                    c2_pq_buttons <- make_fat_radio_buttons(choices = c(Overpass = "/", Poor = "-", OK = "!", Good = "+", Perfect = "#"), selected = passq, input_var = "c2_pq")
                    c2_buttons <- make_fat_radio_buttons(
                        choices = c(Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball<br />attack" = "P2", "Freeball over" = "F", "Reception error<br />(serve ace)" = "R=", ## rcv team actions
                                    "Opp. dig" = "aF", "Opp. dig error" = "aF=", "Opp. overpass attack" = "aPR"), ## opp actions
                        selected = "E", input_var = "c2")
                    if (app_data$is_beach) {
                        stop("setter for beach")
                        ## choose the player who didn't pass
                    }
                    ##guessed_setter <- guess_setter_options(pass_quality = passq, game_state = game_state, dvw = rdata$dvw)
                    guessed_setter <- get_setter(game_state) ## setter on court
                    sp <- c(sort(get_players(game_state, dvw = rdata$dvw)), sort(get_liberos(game_state, dvw = rdata$dvw)))
                    names(sp) <- player_nums_to(sp, team = game_state$current_team, dvw = rdata$dvw)
                    sp <- c(sp, Unknown = "Unknown")
                    setter_buttons <- make_fat_radio_buttons(choices = sp, selected = guessed_setter, input_var = "c2_player")
                    opp <- c(sort(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw)), sort(get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw)))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_buttons <- make_fat_radio_buttons(choices = opp, selected = NA, input_var = "c2_opp_player")
                    if (was_shift_click) {
                        ## accept set by setter on court, with no popup
                        esz <- as.character(dv_xy2subzone(game_state$start_x, game_state$start_y))
                        passq <- guess_pass_quality(game_state, dvw = rdata$dvw)
                        rc <- rally_codes()
                        rc$eval[rc$skill %eq% "R"] <- passq
                        ## find corresponding serve evaluation code
                        seval <- rdata$options$compound_table %>% dplyr::filter(.data$skill == "S", .data$compound_skill == "R", .data$compound_code == passq) %>% pull(.data$code)
                        if (length(seval) != 1 || nchar(seval) != 1) seval <- "~"
                        rc$eval[rc$skill %eq% "S"] <- seval
                        start_t <- retrieve_video_time(game_state$start_t, video_times = video_times)
                        ## note that the position gets assigned to the start coordinates, but end zone/subzone
                        rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = guessed_setter, skill = "E", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, endxy_valid = game_state$startxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                        rally_state("click third contact")
                        do_video("play")
                    } else {
                        ## current phase
                        ph <- if (nrow(rally_codes()) > 0) tail(make_plays2(rally_codes(), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)$phase, 1) else NA_character_
                        accept_fun("do_assign_c2")
                        show_scout_modal(vwModalDialog(title = "Details", footer = NULL, width = app_data$styling$scout_modal_width, modal_halign = "left",
                                                       do.call(fixedRow, c(list(column(2, tags$strong(paste0(if (ph %eq% "Transition") "Dig" else "Reception", " quality")))), lapply(c2_pq_buttons, function(but) column(1, but)))),
                                                       tags$br(), tags$hr(),
                                                       tags$p(tags$strong("Second contact:")),
                                                       do.call(fixedRow, lapply(c2_buttons[1:6], function(but) column(if (mcols() < 12) 1 else 2, but))),
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
                    sxy <- resolve_courtxy()
                    game_state$start_x <- sxy$x[1]
                    game_state$start_y <- sxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    game_state$startxy_valid <- sxy$valid[1]
                    sxy$start_end <- "start"
                    overlay_points(sxy)
                    ## popup
                    ## current phase
                    ph <- if (nrow(rally_codes()) > 0) tail(make_plays2(rally_codes(), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)$phase, 1) else NA_character_
                    if (rdata$options$attacks_by %eq% "codes") {
                        atk <- guess_attack(game_state, dvw = rdata$dvw, opts = rdata$options, system = rdata$options$team_system)
                        ac <- atk$code
                        ## label with code and description TODO make this a preference, perhaps?
                        if (TRUE) {
                            temp <- left_join(tibble(code = ac), rdata$dvw$meta$attacks, by = "code") %>%
                                mutate(lbl = case_when(!is.na(.data$description) ~ paste0(.data$code, "<br />(", .data$description, ")"), TRUE ~ .data$code))
                            ac <- setNames(temp$code, temp$lbl)
                        } else {
                            ## label by code
                            ac <- setNames(ac, ac)
                        }
                        if (!isTRUE(rdata$options$transition_sets) && ph %eq% "Transition") {
                            ac <- head(ac, if (mcols() < 12) 5 else 9) ## wow, we don't have a lot here if we need to leave room for the three below plus space for the attack review pane
                            ## if we aren't scouting transition sets, then this "third" contact could be a setter dump or second-ball attack
                            ac <- c(ac, c("Setter dump" = rdata$options$setter_dump_code, "Second-ball<br />attack" = rdata$options$second_ball_attack_code))
                        } else {
                            ac <- head(ac, if (mcols() < 12) 7 else 11)
                        }
                        ac_others <- c("Choose other", setdiff(rdata$dvw$meta$attacks$code, ac), "Other attack")
                        attack_other_opts(ac_others)
                        if (TRUE) {
                            ## label with description
                            temp <- left_join(tibble(code = ac_others), rdata$dvw$meta$attacks, by = "code") %>%
                                mutate(lbl = case_when(!is.na(.data$description) ~ paste0(.data$code, " (", .data$description, ")"), TRUE ~ .data$code))
                            ac_others <- setNames(temp$code, temp$lbl)
                        }
                        attack_pl_opts <- atk$player
                    } else {
                        ac <- c("High ball" = "H", "Medium/fast<br />attack" = "M", "Quick attack" = "Q")##, "Other attack" = "O")
                        if (!isTRUE(rdata$options$transition_sets) && ph %eq% "Transition") {
                            ## if we aren't scouting transition sets, then this "third" contact could be a setter dump or second-ball attack
                            ac <- c(ac, c("Setter dump" = rdata$options$setter_dump_code, "Second-ball<br />attack" = rdata$options$second_ball_attack_code))
                        }
                        attack_pl_opts <- guess_attack_player_options(game_state, dvw = rdata$dvw, system = rdata$options$team_system)
                    }
                    n_ac <- length(ac)
                    ## always offer set error option
                    n_ac2 <- 2L
                    ac <- c(ac, "Freeball over" = "F", "Set error" = "E=")
                    if (nrow(rally_codes()) > 0 && tail(rally_codes()$skill, 1) %eq% "E") {
                        ac <- c(ac, "Change prev. set<br />to setter dump" = "PP")
                        n_ac2 <- n_ac2 + 1L
                    }
                    c3_buttons <- make_fat_radio_buttons(choices = c(ac, c("Opp. dig" = "aF", "Opp. dig error" = "aF=", "Opp. overpass attack" = "aPR")), input_var = "c3")
                    fatradio_class_uuids$c3 <- attr(c3_buttons, "class")
                    hit_type_buttons <- make_fat_radio_buttons(
                        choices = if (app_data$is_beach) c(Power = "H", Poke = "T", Shot = "P") else c(Hit = "H", Tip = "T", "Soft/Roll" = "P"),
                        input_var = "hit_type")
                    fatradio_class_uuids$hit_type <- attr(hit_type_buttons, "class")
                    ap <- sort(attack_pl_opts$choices)
                    names(ap) <- player_nums_to(ap, team = game_state$current_team, dvw = rdata$dvw)
                    ap <- c(ap, Unknown = "Unknown")
                    ## since we have a freeball over option here, it could be done by a libero
                    libs <- sort(get_liberos(game_state, team = game_state$current_team, dvw = rdata$dvw))
                    ap <- c(ap, setNames(libs, player_nums_to(libs, team = game_state$current_team, dvw = rdata$dvw)))
                    attacker_buttons <- make_fat_radio_buttons(choices = ap, selected = attack_pl_opts$selected, input_var = "c3_player")
                    ## do we want to support "hole" block?
                    if (isTRUE(rdata$options$nblockers)) {
                        nblocker_buttons <- make_fat_radio_buttons(
                            choices = c("No block" = 0, "Single block" = 1, "Double block" = 2, "Triple block" = 3, "Hole block" = 4),
                            selected = if (!is.null(rdata$options$default_nblockers) && !is.na(rdata$options$default_nblockers)) rdata$options$default_nblockers,
                            input_var = "nblockers")
                    }
                    ## attack error, blocked, replay will be scouted on next entry
                    ## TODO other special codes ?
                    opp <- c(sort(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw)), sort(get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw)))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_player_buttons <- make_fat_radio_buttons(choices = opp, selected = NA, input_var = "c3_opp_player")
                    accept_fun("do_assign_c3")
                    show_scout_modal(vwModalDialog(title = "Details: attack or freeball over", footer = NULL, width = app_data$styling$scout_modal_width, modal_halign = "left",
                                            do.call(fixedRow, c(lapply(c3_buttons[seq_len(n_ac)], function(but) column(1, but)),
                                                                if (rdata$options$attacks_by %eq% "codes") list(column(1, tags$div(id = "c3_other_outer", pickerInput("c3_other_attack", label = NULL, choices = ac_others, selected = "Choose other", width = "100%")))))),
                                            tags$br(),
                                            ## hit type and then the freeball over, set error, setter dump buttons, shift them to the right
                                            fixedRow(column(1, hit_type_buttons[1]), column(1, hit_type_buttons[2]), column(1, hit_type_buttons[3]),
                                                     column(2, offset = 2, c3_buttons[n_ac + 1L]), column(2, c3_buttons[n_ac + 2L]), if (n_ac2 > 2) column(2, c3_buttons[n_ac + 3L])),
                                            tags$br(),
                                            tags$div(id = "c3_pl_ui", tags$p("by player"), tags$br(),
                                                     do.call(fixedRow, lapply(attacker_buttons, function(but) column(1, but)))
                                                     ),
                                            tags$br(),
                                            tags$div(id = "c3_bl_ui",
                                                     if (isTRUE(rdata$options$nblockers)) tags$div("with", tags$br()),
                                                     if (isTRUE(rdata$options$nblockers)) do.call(fixedRow, lapply(nblocker_buttons, function(but) column(2, but))),
                                                     tags$br()),
                                            tags$hr(), tags$div("OR"), tags$br(),
                                            do.call(fixedRow, lapply(tail(c3_buttons, 3), function(but) column(2, but))),
                                            tags$div(id = "c3_opp_pl_ui", style = "display:none;", tags$br(), tags$p("by player"), tags$br(),
                                                     do.call(fixedRow, lapply(opp_player_buttons, function(but) column(1, but)))
                                                     ),
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
                    sxy <- resolve_courtxy()
                    game_state$end_x <- sxy$x[1]
                    game_state$end_y <- sxy$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    game_state$endxy_valid <- sxy$valid[1]
                    sxy$start_end <- "end"
                    overlay_points(rbind(overlay_points(), sxy)) ## show start and end
                    ## popup
                    ## note that we can't currently cater for a block kill with cover-dig error (just scout as block kill without the dig error)
                    blocked_for_reattack <- (game_state$start_y < 3.5) %eq% (game_state$end_y < 3.5)
                    c1_buttons <- make_fat_radio_buttons(
                        choices = c("Attack kill (without dig error)" = "A#", "Attack error" = "A=", "Blocked for reattack (play continues)" = "A!", "Dig" = "D", "Dig error (attack kill)" = "D=", "Block kill" = "B#", "Block fault" = "B/"),
                        ## default blocked for reattack if the ball did not cross the net, otherwise dig
                        selected = if (blocked_for_reattack) "A!" else "D",
                        input_var = "c1")
                    ## allow the possibility to change the hit type
                    htype <- check_hit_type(input$hit_type) ## the currently-selected hit type, default to "H" if not assigned
                    hit_type_buttons <- make_fat_radio_buttons(
                        choices = if (app_data$is_beach) c(Power = "H", Poke = "T", Shot = "P") else c(Hit = "H", Tip = "T", "Soft/Roll" = "P"),
                        selected = htype, input_var = "hit_type")
                    fatradio_class_uuids$hit_type <- attr(hit_type_buttons, "class")
                    ae_buttons <- make_fat_radio_buttons(
                        choices = c("Out long" = "O", "Out side" = "S", "In net" = "N", "Net contact" = "I", Antenna = "A", "Other/referee call" = "Z"),
                        selected = NA, input_var = "attack_error_type")
                    ## blocking players
                    blockp <- get_players(game_state, team = game_state$current_team, dvw = rdata$dvw)
                    ## note that block fault could be a backrow player, so need to show all 6 for this
                    blockp_all <- blockp
                    if (length(blockp) == 6) blockp <- blockp[2:4] ## front-row only
                    blockp <- sort(blockp)
                    names(blockp) <- player_nums_to(blockp, team = game_state$current_team, dvw = rdata$dvw)
                    blockp <- c(blockp, Unknown = "Unknown")
                    block_player_buttons <- make_fat_radio_buttons(choices = blockp, selected = NA, input_var = "c1_block_touch_player") ## use for block touch or kill
                    ## and another set of buttons for block fault, because this could be a backrow player
                    blockp_all <- sort(blockp_all)
                    names(blockp_all) <- player_nums_to(blockp_all, team = game_state$current_team, dvw = rdata$dvw)
                    blockp_all <- c(blockp_all, Unknown = "Unknown")
                    block_fault_player_buttons <- make_fat_radio_buttons(choices = blockp_all, selected = NA, input_var = "c1_block_fault_player")
                    ## identify defending players
                    dig_pl_opts <- guess_dig_player_options(game_state, dvw = rdata$dvw, system = rdata$options$team_system)
                    digp <- dig_pl_opts$choices
                    names(digp) <- player_nums_to(digp, team = game_state$current_team, dvw = rdata$dvw)
                    digp <- c(digp, Unknown = "Unknown")
                    dig_player_buttons <- make_fat_radio_buttons(choices = digp, selected = dig_pl_opts$selected, input_var = "c1_def_player")
                    ## covering players (attacking team)
                    cover_pl_opts <- guess_cover_player_options(game_state, dvw = rdata$dvw, system = rdata$options$team_system)
                    coverp <- cover_pl_opts$choices
                    names(coverp) <- player_nums_to(coverp, team = other(game_state$current_team), dvw = rdata$dvw)
                    coverp <- c(coverp, Unknown = "Unknown", "No cover dig" = "No cover dig")
                    cover_player_buttons <- make_fat_radio_buttons(choices = coverp, selected = cover_pl_opts$selected, input_var = "c1_cover_player")
                    if (was_shift_click) {
                        ## attack in play (i.e. was dug), but we are not stopping to enter details
                        ## we can either - insert a dig with an unknown dig player
                        ##               - insert a dig with the suggested dig player (though this might be incorrect)
                        ##               - do not insert a dig, just record the attack end location
                        ## note that we can't record a block touch if we aren't stopping to enter details
                        insert_auto_dig <- FALSE ## change to TRUE if we want a dig skill inserted. If FALSE, we simply modify the attack details (end location and evaluation)
                        digp <- 0L ## or use suggested defensive player ## if (!is.null(input$c1_def_player)) input$c1_def_player else 0L
                        sz <- "~"; szv <- FALSE ## default to unknown start zone
                        esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
                        rc <- rally_codes()
                        end_t <- retrieve_video_time(game_state$end_t, video_times = video_times)
                        ## was the previous skill an attack, or one previous to that an attack with a block in between
                        ## the latter should not be possible, but keep the code in anyway
                        Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                        if (!is.na(Aidx)) {
                            sz <- rc$sz[Aidx] ## existing start zone
                            if (isTRUE(game_state$startxy_valid)) {
                                szv <- TRUE
                                ## update start pos, it may have been edited by the user
                                if (!rdata$options$attacks_by %eq% "codes") {
                                    ## don't change start zone if using attack combo codes
                                    sz <- as.character(adjusted_backrow_pos(game_state = game_state)$zone) ## preceding skill was attack, so use adjusted zone
                                }
                                rc$sz[Aidx] <- sz
                                rc$start_x[Aidx] <- game_state$start_x
                                rc$start_y[Aidx] <- game_state$start_y
                            } else {
                                szv <- rdata$options$attacks_by %eq% "codes" ## start zone still valid if from attack combo code
                            }
                            if (isTRUE(game_state$endxy_valid)) {
                                rc$ez[Aidx] <- esz[1]
                                rc$esz[Aidx] <- esz[2]
                                rc$end_x[Aidx] <- game_state$end_x
                                rc$end_y[Aidx] <- game_state$end_y
                            }
                            tempo <- rc$tempo[Aidx]
                        } else {
                            tempo <- "~"
                        }
                        if (!is.na(Aidx)) rc$eval[Aidx] <- "-" ## poor attack
                        eval <- "+" ## good dig
                        ## dig location: start is as for attack start, end is the dig location. No block touch if shift-clicking
                        if (insert_auto_dig) {
                            rc <- bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "D", eval = eval, tempo = tempo, sz = sz, ez = esz[1], esz = esz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, start_zone_valid = szv, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table))
                        }
                        rally_codes(rc)
                        rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
                        do_video("play")
                    }  else {
                        accept_fun("do_assign_c1")
                        show_scout_modal(vwModalDialog(title = "Details", footer = NULL, width = app_data$styling$scout_modal_width, modal_halign = "left",
                                                       tags$p(tags$strong("Attack outcome and hit type:")),
                                                       fixedRow(column(2, c1_buttons[1]), column(2, c1_buttons[2]), column(3, c1_buttons[3]),
                                                                column(1, hit_type_buttons[1]), column(1, hit_type_buttons[2]), column(1, hit_type_buttons[3])),
                                                       tags$br(), tags$div(id = "ae_ui", style = "display:none;", do.call(fixedRow, lapply(ae_buttons, function(but) column(1, but)))),
                                                       tags$div("OR", tags$strong("Defence outcome:")),
                                                       do.call(fixedRow, lapply(c1_buttons[4:7], function(but) column(2, but))),
                                                       tags$br(), tags$hr(),
                                                       ## either dig players (defending team)
                                                       tags$div(id = "c1_digp_ui", style = if (blocked_for_reattack) "display:none;" else NULL, tags$p(tags$strong("Dig player")),
                                                                do.call(fixedRow, lapply(dig_player_buttons, function(but) column(1, but)))),
                                                       ## or cover players (attacking team)
                                                       tags$div(id = "c1_coverp_ui", style = if (blocked_for_reattack) NULL else "display:none;", tags$p(tags$strong("Cover dig player")),
                                                                do.call(fixedRow, lapply(cover_player_buttons, function(but) column(1, but)))),
                                                       ## or block players
                                                       tags$div(id = "c1_blockp_ui", style = "display:none;", tags$p(tags$strong("Block kill"), "by player"),
                                                                do.call(fixedRow, lapply(block_player_buttons, function(but) column(2, but)))),
                                                       tags$div(id = "c1_blockfaultp_ui", style = "display:none;", tags$p(tags$strong("Block fault"), "by player"),
                                                                do.call(fixedRow, lapply(block_fault_player_buttons, function(but) column(1, but)))),
                                                       tags$br(),
                                                       tags$div(id = "c1_touchp_ui", tags$hr(), tags$div("WITH", tags$strong("Block touch"), "by player"), tags$br(),
                                                                do.call(fixedRow, lapply(block_player_buttons, function(but) column(2, but)))),
                                                       tags$hr(),
                                                       fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", class = "cancel fatradio")),
                                                                column(2, offset = 8, actionButton("assign_c1", "Continue", class = "continue fatradio")))
                                                       ))
                    }
                } else if (rally_state() == "click freeball end point") {
                    ## freeball dig, freeball dig error, freeball error (in theory could be blocked, blocked for replay, block touch (freeball kill))
                    do_video("pause")
                    ## click was the dig or freeball end
                    sxy <- resolve_courtxy()
                    game_state$end_x <- sxy$x[1]
                    game_state$end_y <- sxy$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    game_state$endxy_valid <- sxy$valid[1]
                    sxy$start_end <- "end"
                    overlay_points(rbind(overlay_points(), sxy)) ## show start and end
                    ## popup
                    ## note that we can't currently cater for a block kill with cover-dig error (just scout as block kill without the dig error)
                    f1_buttons <- make_fat_radio_buttons(
                        choices = c("Freeball over error" = "F=", "Freeball dig" = "FD", "Freeball dig error" = "FD=", "Opp. overpass attack" = "aPR"),
                        selected = "FD", input_var = "f1")
                    ## Identify defending players
                    dig_pl_opts <- guess_freeball_dig_player_options(game_state, dvw = rdata$dvw, system = rdata$options$team_system)
                    digp <- dig_pl_opts$choices
                    names(digp) <- player_nums_to(digp, team = game_state$current_team, dvw = rdata$dvw)
                    digp <- c(digp, Unknown = "Unknown")
                    dig_player_buttons <- make_fat_radio_buttons(choices = digp, selected = dig_pl_opts$selected, input_var = "f1_def_player")
                    accept_fun("do_assign_f1")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL, width = app_data$styling$scout_modal_width, modal_halign = "left",
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
                if (debug > 0) {
                    if (nrow(rally_codes()) > 0) {
                        cat("rally codes:\n")
                        print_rally_codes(rally_codes())
                        ##cat(str(rally_codes()))
                    }
                    cat("rally state: ", rally_state(), "\n")
                }
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
            ## c1 can be A#, A=, A!, D, D=, B#, B/
            ## for A!, show cover players and block touch
            ## D, D=, show def players and block touch
            ## A# show block touch
            ## A= hide all player selectors
            ## B# show block touch, B/ show block fault
            ## cover player buttons (c1_coverp_ui), block player buttons (c1_blockp_ui), and dig player buttons (c1_digp_ui) are alternatives, only one of these should be shown at any one time
            if (!is.null(input$c1)) {
                if (input$c1 %eq% "A!") {
                    js_show2("c1_coverp_ui")
                    js_hide2("c1_blockp_ui")
                    js_hide2("c1_blockfaultp_ui")
                    js_hide2("c1_digp_ui")
                    js_show2("c1_touchp_ui")
                } else if (input$c1 %in% c("D", "D=")) {
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_blockp_ui")
                    js_hide2("c1_blockfaultp_ui")
                    js_show2("c1_digp_ui")
                    js_show2("c1_touchp_ui")
                } else if (input$c1 %in% c("B#")) {
                    js_show2("c1_blockp_ui")
                    js_hide2("c1_blockfaultp_ui")
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                    js_hide2("c1_touchp_ui")
                } else if (input$c1 %in% c("B/")) {
                    js_show2("c1_blockfaultp_ui")
                    js_hide2("c1_blockp_ui")
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                    js_hide2("c1_touchp_ui")
                } else if (input$c1 %in% c("A#")) {
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                    js_hide2("c1_blockp_ui")
                    js_hide2("c1_blockfaultp_ui")
                    js_show2("c1_touchp_ui")
                } else {
                    ## A=
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                    js_hide2("c1_blockp_ui")
                    js_hide2("c1_blockfaultp_ui")
                    js_hide2("c1_touchp_ui")
                    if (!is.na(game_state$end_x) && !is.na(game_state$end_y)) {
                        guess_att_err <- NA_character_
                        if (game_state$end_x < 0.5 || game_state$end_x > 3.5) {
                            ## out side
                            guess_att_err <- "S"
                        } else if (game_state$end_y < 0.5 || game_state$end_y > 6.5) {
                            ## out long
                            guess_att_err <- "O"
                        } else if (game_state$end_y > 3.3 && game_state$end_y < 3.7) {
                            ## ball end point is near the net
                            guess_att_err <- "N"
                        }
                        if (!is.na(guess_att_err)) dojs(paste0("$('#",  digest::digest(paste0("but-attack_error_type-", guess_att_err)), "').click();"))
                    }
                }
                if (input$c1 %eq% "A=") js_show2("ae_ui") else js_hide2("ae_ui")
            }
        })

        observe({
            if (!is.null(input$f1)) {
                if (input$f1 %eq% "F=") {
                    js_hide2("f1_digp_ui")
                } else {
                    js_show2("f1_digp_ui")
                }
            }
        })

        observeEvent(input$c3_other_attack, {
            if (!is.null(input$c3_other_attack) && !input$c3_other_attack %eq% "Choose other") {
                dojs(paste0("Shiny.setInputValue('c3', '", input$c3_other_attack, "');"))
                ## set the other c3 button styles as unselected, and this as selected
                if (!is.null(fatradio_class_uuids$c3)) dojs(paste0("$('.", fatradio_class_uuids$c3, "').removeClass('active');"))
                dojs("$('#c3_other_outer').addClass('active');")
            }
        })
        observeEvent(input$c3, {
            if (!is.null(input$c3)) {
                ## if an 'other' attack isn't selected, set its style as unselected
                if (!input$c3 %in% attack_other_opts()) dojs("$('#c3_other_outer').removeClass('active');")
                if (input$c3 %eq% "E=") {
                    ## hide the blockers and opposition players selections
                    js_hide2("c3_bl_ui"); js_hide2("c3_opp_pl_ui")
                    ## if we've already scouted the set (i.e. in reception phase, or we are scouting transition sets) then also hide the player selector because it won't be used
                    if (length(rally_codes()$skill) > 0 && tail(rally_codes()$skill, 1) %eq% "E") js_hide2("c3_pl_ui") else js_show2("c3_pl_ui")
                } else {
                    js_show2("c3_bl_ui");
                    js_show2("c3_pl_ui")
                    if (input$c3 %in% c("aF", "aF=", "aPR")) {
                        ## show the opp players
                        js_show2("c3_opp_pl_ui")
                    } else {
                        ## hide the opp players
                        js_hide2("c3_opp_pl_ui")
                    }
                }
            }
        })

        ## for the playslist table, convert the rally codes into plays2 rows, and build plays from plays2
        observe({
            temp_rally_plays2 <- if (nrow(rally_codes()) > 0) make_plays2(rally_codes(), game_state = game_state, dvw = rdata$dvw) else NULL
            ##print(dplyr::glimpse(temp_rally_plays2))
            ##print(temp_rally_plays2$phase)
            ##cat(str(rdata$dvw$plays2))
            rdata$dvw$plays <- plays2_to_plays(rp2(bind_rows(rdata$dvw$plays2, temp_rally_plays2)), dvw = rdata$dvw, evaluation_decoder = app_data$evaluation_decoder)
        })

        observeEvent(input$cancelrew, {
            do_cancel_rew()
        })
        do_cancel_rew <- function() {
            do_video("rew", 3)
            do_video("play")
            if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) overlay_points(head(overlay_points(), -1))
            remove_scout_modal()
        }
        observeEvent(input$cancel, {
            do_video("play")
            remove_scout_modal()
        })

        observeEvent(input$end_of_set_confirm, {
            game_state$set_number <- game_state$set_number + 1L ## should be incremented in this plays2 line
            rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(paste0("**", game_state$set_number - 1L, "set"), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
            game_state$home_score_start_of_point <- game_state$visiting_score_start_of_point <- 0L
            game_state$home_team_end <- other_end(game_state$home_team_end) ## for 5th set, the user can change this if needed via the gui
            if ((!app_data$is_beach && game_state$set_number < 5) || (app_data$is_beach && game_state$set_number < 3)) {
                ## serving team is the one that did not serve first in the previous set
                temp <- rdata$dvw$plays2[rdata$dvw$plays2$set_number %eq% (game_state$set_number - 1L), ]
                temp <- temp$serving[!grepl("^\\*\\*[[:digit:]]set", temp$code) & !grepl(">LUp", temp$code, ignore.case = TRUE) & !is.na(temp$serving)]
                if (length(temp) > 0) game_state$serving <- other(head(temp, 1))
            }
            ## update match metadata
            rdata$dvw <- update_meta(rp2(rdata$dvw))
            remove_scout_modal()
            rally_state("click serve start")
            have_asked_end_of_set(FALSE) ## reset
        })
        observeEvent(input$end_of_set_cancel, {
            do_video("play")
            remove_scout_modal()
            rally_state("click serve start")
        })

        observeEvent(input$assign_serve_outcome, do_assign_serve_outcome())

        do_assign_serve_outcome <- function() {
            if (is.null(input$serve_initial_outcome)) {
                ## wtf? should not happen
            } else {
                sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
                rc <- rally_codes()
                ## serving player
                sp <- if (!is.null(input$serve_preselect_player)) input$serve_preselect_player else if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                ## serve type (tempo)
                st <- if (!is.null(input$serve_type)) input$serve_type else default_skill_tempo("S")
                start_t <- retrieve_video_time(game_state$start_t, video_times = video_times)
                end_t <- retrieve_video_time(game_state$end_t, video_times = video_times)
                Sidx <- which(rc$skill == "S")
                if (input$serve_initial_outcome %eq% "=") {
                    ## serve error
                    serve_err_type <- if (!is.null(input$serve_error_type)) input$serve_error_type else "="
                    remove_scout_modal()
                    special_code <- substr(serve_err_type, 2, 2)
                    if (special_code %eq% "N" && rdata$options$end_convention %eq% "actual") {
                        game_state$end_y <- 3.5 ## exactly on net
                        ## make sure that the zone/subzone are as for the receiving side of the court
                        esz <- as.character(dv_xy2subzone(game_state$end_x, if (isTRUE((game_state$serving == "*" && game_state$home_team_end == "upper") || (game_state$serving == "a" && game_state$home_team_end == "lower"))) 3.45 else 3.55))
                    }
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = ldz(sp), tempo = st, eval = "=", sz = sz, ez = esz[1], esz = esz[2], special = if (nzchar(special_code)) special_code else "~", t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, game_state = game_state)
                    }
                    rally_codes(rc)
                    game_state$point_won_by <- other(game_state$serving)
                    rally_ended()
                } else if (input$serve_initial_outcome %eq% "S#") {
                    ## serve ace
                    pp <- if (!is.null(input$pass_player)) input$pass_player else 0L
                    remove_scout_modal()
                    if (length(Sidx) == 1) {
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = ldz(sp), tempo = st, eval = "#", sz = sz, ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, game_state = game_state)
                    }
                    rally_codes(bind_rows(rc, code_trow(team = other(game_state$serving), pnum = pp, skill = "R", eval = "=", tempo = st, sz = sz, ez = esz[1], esz = esz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))

                    game_state$current_team <- game_state$serving
                    game_state$point_won_by <- game_state$serving
                    rally_ended()
                } else {
                    ## reception in play
                    pp <- if (!is.null(input$pass_player)) input$pass_player else 0L
                    remove_scout_modal()
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = ldz(sp), tempo = st, ez = esz[1], esz = esz[2], t = start_t, end_x = game_state$end_x, end_y = game_state$end_y, game_state = game_state)
                    }
                    rally_codes(bind_rows(rc, code_trow(team = other(game_state$serving), pnum = pp, skill = "R", tempo = st, sz = sz, ez = esz[1], esz = esz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                    rally_state("click second contact")
                    do_video("rew", app_data$play_overlap)
                }
                if (rally_state() != "confirm end of set") do_video("play")
            }
        }

        observeEvent(input$assign_c1, do_assign_c1())
        do_assign_c1 <- function() {
            ## possible values for input$c1 are currently: A#, A=, A!, D, D=, B/, B#
            ## A#, A=, D, D= can be (but not necessarily) accompanied by a block touch (but not B#, B/). A= is unlikely but theoretically possible. A! always gets a block action
            mid_xy <- c(NA_real_, NA_real_)
            game_state$midxy_valid <- FALSE
            if (!is.null(input$c1_block_touch_player)) {
                if (input$c1 %in% c("A#", "A=", "D", "D=", "A!")) {
                    mid_xy <- infer_mid_coords(game_state = game_state) ## will be NAs if start or end are invalid
                    ## NOTE that if we are scouting with `rdata$options$end_convention %eq% "intended")` then the mid_xy should generally be OK in these situations; if we are scouting with end_convention "actual" then it is less accurate. But good enough?
                    game_state$midxy_valid <- !any(is.na(mid_xy))
                    if (input$c1 %in% c("A#", "A=", "D", "D=")) {
                        ## insert the block touch now. For A! it happens below
                        beval <- if (input$c1 %eq% "A#") "=" else default_skill_eval("B")
                        rc <- rally_codes()
                        Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                        rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = input$c1_block_touch_player, skill = "B", eval = beval, tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", ez = if (!is.na(Aidx)) block_zone(rc$sz[Aidx]) else "~", start_x = mid_xy[1], start_y = mid_xy[2], t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                    }
                } else if (input$c1 %in% c("B#")) {
                    ## for block kill and "actual" end convention, the end point should be the net (block) location or the ball landing location. In either of these cases we'll use mid_xy as the block location
                    if (rdata$options$end_convention %eq% "actual") mid_xy <- infer_mid_coords(game_state = game_state)
                }
                ## for block fault B/ it isn't clear what the scout will have entered for the attack end point, so leave mid_xy as NA
            }
            esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
            rc <- rally_codes()
            if (input$c1 %in% c("A#", "A=")) {
                ##end_t <- retrieve_video_time(game_state$end_t, video_times = video_times)
                eval <- substr(input$c1, 2, 2)
                ## find the attack, should be either the previous skill, or one previous to that with a block in between
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                if (!is.na(Aidx)) {
                    if (!is.null(input$attack_error_type) && input$attack_error_type %eq% "N") {
                        game_state$end_y <- 3.5 ## exactly on net
                        ## make sure that the zone/subzone are as for the defending side of the court
                        ## game_state$current_team is the defending team
                        esz <- as.character(dv_xy2subzone(game_state$end_x, if (isTRUE((game_state$current_team == "*" && game_state$home_team_end == "upper") || (game_state$current_team == "a" && game_state$home_team_end == "lower"))) 3.55 else 3.45))
                    }
                    rc$x_type[Aidx] <- check_hit_type(input$hit_type) ## update hit type if needed
                    if (FALSE) {
                        sz <- rc$sz[Aidx] ## default to existing start zone
                        if (isTRUE(game_state$startxy_valid)) {
                            ## update start pos, it may have been edited by the user
                            if (!rdata$options$attacks_by %eq% "codes") {
                                ## don't change start zone if using attack combo codes
                                sz <- as.character(adjusted_backrow_pos(game_state = game_state)$zone) ## preceding skill was attack, so use adjusted zone
                            }
                            rc$sz[Aidx] <- sz
                            rc$start_x[Aidx] <- game_state$start_x
                            rc$start_y[Aidx] <- game_state$start_y
                        }
                        if (isTRUE(game_state$endxy_valid)) {
                            rc$ez[Aidx] <- esz[1]
                            rc$esz[Aidx] <- esz[2]
                            rc$end_x[Aidx] <- game_state$end_x
                            rc$end_y[Aidx] <- game_state$end_y
                        }
                        rc$mid_x[Aidx] <- mid_xy[1]
                        rc$mid_y[Aidx] <- mid_xy[2]
                        rc$eval[Aidx] <- eval
                        if (!is.null(input$attack_error_type)) rc$special[Aidx] <- input$attack_error_type
                        ##cat("revised A row is: \n"); print(dplyr::glimpse(rc[Aidx, ]))
                    } else {
                        ## that can be replaced by
                        ## update start pos, it may have been edited by the user, but don't change start zone if using attack combo codes
                        sz <- if (isTRUE(game_state$startxy_valid) && !rdata$options$attacks_by %eq% "codes") {
                                  as.character(adjusted_backrow_pos(game_state = game_state)$zone) ## preceding skill was attack, so use adjusted zone
                              } else {
                                  rc$sz[Aidx] ## keep same
                              }
                        temp <- update_code_trow(rc[Aidx, ], eval = eval, sz = sz, ez = esz[1], esz = esz[2], special = if (!is.null(input$attack_error_type)) input$attack_error_type else "~", start_x = game_state$start_x, start_y = game_state$start_y, mid_x = game_state$mid_x, mid_y = game_state$mid_y, end_x = game_state$end_x, end_y = game_state$end_y, start_zone_valid = TRUE, game_state = game_state)
                        rc[Aidx, ] <- temp
                        ##cat("but maybe could be: \n"); print(dplyr::glimpse(temp))
                        ##cat("all equal?\n"); print(all.equal(rc[Aidx, ], temp))
                    }
                    rally_codes(rc)
                }
                ## "current" team here is the digging team
                game_state$point_won_by <- if (eval == "#") other(game_state$current_team) else game_state$current_team
                rally_ended()
            } else if (input$c1 %eq% "A!") {
                ## blocked for reattack
                end_t <- retrieve_video_time(game_state$end_t, video_times = video_times)
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else NA_integer_
                if (!is.na(Aidx)) rc$x_type[Aidx] <- check_hit_type(input$hit_type) ## update hit type if needed
                ## TODO if we already have a block skill here, don't add a new one, just update the existing one ... though there should never already be block skill here
                bp <- if (!is_nnn(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
                if (!is.na(Aidx)) {
                    ## adjust the attack row
                    sz <- rc$sz[Aidx] ## default to existing start zone
                    if (isTRUE(game_state$startxy_valid)) {
                        ## update start pos, it may have been edited by the user
                        if (!rdata$options$attacks_by %eq% "codes") {
                            ## don't change start zone if using attack combo codes
                            sz <- as.character(adjusted_backrow_pos(game_state = game_state)$zone) ## preceding skill was attack, so use adjusted zone
                        }
                        rc$sz[Aidx] <- sz
                        rc$start_x[Aidx] <- game_state$start_x
                        rc$start_y[Aidx] <- game_state$start_y
                    }
                    rc$eval[Aidx] <- "!"
                    if (isTRUE(game_state$endxy_valid)) {
                        rc$ez[Aidx] <- esz[1]
                        rc$esz[Aidx] <- esz[2]
                        rc$end_x[Aidx] <- game_state$end_x
                        rc$end_y[Aidx] <- game_state$end_y
                    }
                    rc$mid_x[Aidx] <- mid_xy[1]
                    rc$mid_y[Aidx] <- mid_xy[2]
                } else {
                    sz <- NA_integer_
                }
                ## "current" team here is the digging/blocking team
                rally_codes(bind_rows(rc,
                                      ## the block. end zone of block is the mirrored start zone of the attack but start xy is mid_xy of the attack
                                      code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "!", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", ez = block_zone(sz), start_x = mid_xy[1], start_y = mid_xy[2], t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table),
                                      ## and the dig cover. Prior to 2025-04-27 the cover dig start location was recorded as the end location of the attack, but this is inconsistent with normal defensive digs. Changed to be the same as defensive digs, so that the cover dig start and end are identical to the attack start and end
                                      if (!input$c1_cover_player %eq% "No cover dig") code_trow(team = other(game_state$current_team), pnum = if (!is_nnn(input$c1_cover_player)) input$c1_cover_player else 0L, skill = "D", eval = default_skill_eval("D"), sz = sz, ez = esz[1], esz = esz[2], x_type = "C", t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, mid_x = mid_xy[1], mid_y = mid_xy[2], end_x = game_state$end_x, end_y = game_state$end_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table) ## startxy_valid = game_state$startxy_valid, ## TODO check if we need to set *_valid here
                                      ))
                game_state$current_team <- other(game_state$current_team) ## attacking team now playing
                rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
            } else if (input$c1 %eq% "B/") {
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc)  else NA_integer_
                bp <- if (!is_nnn(input$c1_block_fault_player)) input$c1_block_fault_player else 0L
                if (!is.na(Aidx)) {
                    ## adjust the attack row
                    rc$x_type[Aidx] <- check_hit_type(input$hit_type) ## update hit type if needed
                    sz <- rc$sz[Aidx] ## default to existing start zone
                    if (isTRUE(game_state$startxy_valid)) {
                        ## update start pos, it may have been edited by the user
                        if (!rdata$options$attacks_by %eq% "codes") {
                            ## don't change start zone if using attack combo codes
                            sz <- as.character(adjusted_backrow_pos(game_state = game_state)$zone) ## preceding skill was attack, so use adjusted zone
                        }
                        rc$sz[Aidx] <- sz
                        rc$start_x[Aidx] <- game_state$start_x
                        rc$start_y[Aidx] <- game_state$start_y
                    }
                    if (isTRUE(game_state$endxy_valid)) {
                        rc$ez[Aidx] <- esz[1]
                        rc$esz[Aidx] <- esz[2]
                        rc$end_x[Aidx] <- game_state$end_x
                        rc$end_y[Aidx] <- game_state$end_y
                    }
                    ## if the attack row has an incompatible evaluation, change it. It can't be /, and arguably can't be # or ! either but we'll leave these for the time being
                    if (rc$eval[Aidx] %in% c("/")) rc$eval[Aidx] <- default_skill_eval("A")
                } else {
                    sz <- NA_integer_
                }
                ## TODO if we already have a block skill here, don't add a new one, just update the existing one ... though there should never already be block skill here
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "/", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", ez = block_zone(sz), t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table))) ## start_x,y can't really be inferred here
                game_state$point_won_by <- other(game_state$current_team) ## "current" team here is the digging team
                rally_ended()
            } else if (input$c1 %eq% "B#") {
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else NA_integer_
                bp <- if (!is_nnn(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
                if (!is.na(Aidx)) {
                    ## adjust the attack row
                    rc$x_type[Aidx] <- check_hit_type(input$hit_type) ## update hit type if needed
                    sz <- rc$sz[Aidx] ## default to existing start zone
                    if (isTRUE(game_state$startxy_valid)) {
                        ## update start pos, it may have been edited by the user
                        if (!rdata$options$attacks_by %eq% "codes") {
                            ## don't change start zone if using attack combo codes
                            sz <- as.character(adjusted_backrow_pos(game_state = game_state)$zone) ## preceding skill was attack, so use adjusted zone
                        }
                        rc$sz[Aidx] <- sz
                        rc$start_x[Aidx] <- game_state$start_x
                        rc$start_y[Aidx] <- game_state$start_y
                    }
                    if (isTRUE(game_state$endxy_valid)) {
                        rc$ez[Aidx] <- esz[1]
                        rc$esz[Aidx] <- esz[2]
                        rc$end_x[Aidx] <- game_state$end_x
                        rc$end_y[Aidx] <- game_state$end_y
                    }
                    rc$mid_x[Aidx] <- mid_xy[1]
                    rc$mid_y[Aidx] <- mid_xy[2]
                    rc$eval[Aidx] <- "/"
                } else {
                    sz <- NA_integer_
                }
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "#", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", ez = block_zone(sz), start_x = mid_xy[1], start_y = mid_xy[2], t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                game_state$point_won_by <- game_state$current_team ## "current" team here is the digging/blocking team
                rally_ended()
            } else {
                ## D or D=
                digp <- if (!is.null(input$c1_def_player)) input$c1_def_player else 0L
                end_t <- retrieve_video_time(game_state$end_t, video_times = video_times)
                sz <- "~"; szv <- FALSE ## default to unknown start zone
                ## was the previous skill an attack, or one previous to that an attack with a block in between
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                if (!is.na(Aidx)) {
                    rc$x_type[Aidx] <- check_hit_type(input$hit_type) ## update hit type if needed
                    sz <- rc$sz[Aidx] ## existing start zone
                    if (isTRUE(game_state$startxy_valid)) {
                        szv <- TRUE
                        ## update start pos, it may have been edited by the user
                        if (!rdata$options$attacks_by %eq% "codes") {
                            ## don't change start zone if using attack combo codes
                            sz <- as.character(adjusted_backrow_pos(game_state = game_state)$zone) ## preceding skill was attack, so use adjusted zone
                        }
                        rc$sz[Aidx] <- sz
                        rc$start_x[Aidx] <- game_state$start_x
                        rc$start_y[Aidx] <- game_state$start_y
                    } else {
                        szv <- rdata$options$attacks_by %eq% "codes" ## start zone still valid if from attack combo code
                    }
                    if (isTRUE(game_state$endxy_valid)) {
                        rc$ez[Aidx] <- esz[1]
                        rc$esz[Aidx] <- esz[2]
                        rc$end_x[Aidx] <- game_state$end_x
                        rc$end_y[Aidx] <- game_state$end_y
                    }
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
                ## dig location: start is as for attack start, end is the dig location
                dx <- game_state$end_x
                dy <- game_state$end_y
                dsz <- esz
                if (!is.null(input$c1_block_touch_player) && rdata$options$end_convention %eq% "intended") {
                    ## if we are scouting intended attack directions, and there was a block touch, then don't use a dig location
                    dx <- dy <- NA_real_
                    dsz <- c("~", "~")
                }
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "D", eval = eval, tempo = tempo, sz = sz, ez = dsz[1], esz = dsz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = dx, end_y = dy, start_zone_valid = szv, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                if (input$c1 == "D=") {
                    game_state$point_won_by <- other(game_state$current_team)
                    rally_ended()
                } else {
                    rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
                }
            }
            if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
            if (rally_state() != "confirm end of set") {
                remove_scout_modal()
                do_video("play")
            }
        }

        observeEvent(input$assign_f1, do_assign_f1())
        do_assign_f1 <- function() {
            ## possible values for input$f1 are currently: F=, FD, FD=, aPR
            esz <- if (isTRUE(game_state$endxy_valid)) as.character(dv_xy2subzone(game_state$end_x, game_state$end_y)) else c("~", "~")
            rc <- rally_codes()
            ## find the freeball, should be the previous skill
            Fidx <- if (rc$skill[nrow(rc)] == "F") nrow(rc) else NA_integer_
            if (!is.na(Fidx)) {
                if (isTRUE(game_state$startxy_valid)) {
                    rc$sz[Fidx] <- dv_xy2zone(game_state$start_x, game_state$start_y)
                    rc$start_x[Fidx] <- game_state$start_x
                    rc$start_y[Fidx] <- game_state$start_y
                }
                if (isTRUE(game_state$endxy_valid)) {
                    rc$ez[Fidx] <- esz[1]
                    rc$esz[Fidx] <- esz[2]
                    rc$end_x[Fidx] <- game_state$end_x
                    rc$end_y[Fidx] <- game_state$end_y
                }
            }
            if (input$f1 %eq% "F=") {
                if (!is.na(Fidx)) {
                    rc$eval[Fidx] <- "="
                    rally_codes(rc)
                }
                ## "current" team here is the digging team
                game_state$point_won_by <- game_state$current_team
                rally_ended()
            } else {
                ## FD, FD=, or aPR
                digp <- if (!is.null(input$f1_def_player)) input$f1_def_player else 0L
                was_f <- grepl("^F", input$f1) ## was a freeball dig or freeball dig error
                end_t <- retrieve_video_time(game_state$end_t, video_times = video_times)
                if (!is.na(Fidx)) {
                    rc$eval[Fidx] <- if (input$f1 %eq% "FD=") "+" else "-"
                }
                ## game_state$current_team is digging team
                if (was_f) {
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "F", eval = if (input$f1 %eq% "FD=") "=" else default_skill_eval("F"), sz = esz[1], t = end_t, start_x = game_state$end_x, start_y = game_state$end_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, startxy_valid = game_state$endxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                    if (input$f1 == "FD=") {
                        game_state$point_won_by <- other(game_state$current_team)
                        rally_ended()
                    } else {
                        ## FD
                        rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
                    }
                } else {
                    ## aPR
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "A", tempo = "O", combo = rdata$options$overpass_attack_code, sz = esz[1], t = end_t, start_x = game_state$end_x, start_y = game_state$end_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, startxy_valid = game_state$endxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                    rally_state("click attack end point")
                }
            }
            if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
            if (rally_state() != "confirm end of set") {
                remove_scout_modal()
                do_video("play")
            }
        }

        observeEvent(input$assign_c2, do_assign_c2())
        do_assign_c2 <- function() {
            ## set uses end position for zone/subzone
            esz <- if (isTRUE(game_state$startxy_valid)) as.character(dv_xy2subzone(game_state$start_x, game_state$start_y)) else c("~", "~")
            passq <- if (!is.null(input$c2_pq) && nchar(input$c2_pq) == 1) input$c2_pq else guess_pass_quality(game_state, dvw = rdata$dvw)
            rc <- rally_codes()
            ph <- if (nrow(rally_codes()) > 0) tail(make_plays2(rally_codes(), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)$phase, 1) else NA_character_
            ## if we are scouting transition sets this c2 could be in transition
            if (ph %eq% "Transition") {
                ## look for preceding dig, and its matching attack. Adjust evaluation codes
                if (isTRUE(rdata$options$transition_sets)) {
                    if (isTRUE(tail(rc$skill, 1) %in% c("D", "F"))) { ## can be a dig or freeball-dig
                        tempeval <- rc$eval
                        tempeval[length(tempeval)] <- passq ## dig qual
                        aeval <- rdata$options$compound_table %>% dplyr::filter(.data$skill == "A", .data$compound_skill == "D", .data$compound_code == passq) %>% pull(.data$code)
                        if (length(aeval) == 1 && nchar(aeval) == 1) {
                            if (length(tempeval) > 1 && isTRUE(rc$skill[nrow(rc) - 1] == "A")) {
                                ## dig direct off attack
                                tempeval[length(tempeval) - 1] <- aeval
                            } else if (length(tempeval) > 2 && isTRUE(rc$skill[nrow(rc) - 2] == "A") && isTRUE(rc$skill[nrow(rc) - 1] == "B") && isTRUE(rc$team[nrow(rc) - 2] != rc$team[nrow(rc)])) {
                                ## dig off block touch, ensuring that digging team was not attacking team, though that should not happen anyway
                                tempeval[length(tempeval) - 2] <- aeval
                            }
                        } else {
                            warning("could not adjust attack evaluation for dig with evaluation: ", cstr(passq))
                        }
                        rc$eval <- tempeval
                    }
                } else {
                    ## this should not happen
                    stop("not expecting second contact in transition because transition_sets scouting option is FALSE")
                }
            } else if (ph %eq% "Reception") {
                rc$eval[rc$skill %eq% "R"] <- passq
                ## find corresponding serve evaluation code
                seval <- rdata$options$compound_table %>% dplyr::filter(.data$skill == "S", .data$compound_skill == "R", .data$compound_code == passq) %>% pull(.data$code)
                if (length(seval) != 1 || nchar(seval) != 1) seval <- "~"
                rc$eval[rc$skill %eq% "S"] <- seval
            } else {
                warning("unknown play phase for second contact, the scouted code will probably be wrong at this point")
            }
            start_t <- retrieve_video_time(game_state$start_t, video_times = video_times)
            ## possible values for input$c2 are: Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball attack" = "P2", "Freeball over" = "F", R= rec error
            ##                                   "Opp. dig" = "aF", error "aF=", "Opp. overpass attack" = "aPR"
            if (input$c2 %in% c("E", "E=", "PP", "P2", "F", "R=")) {
                sp <- input$c2_player
                if (input$c2 == "E") {
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "E", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, endxy_valid = game_state$startxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                    rally_state("click third contact")
                } else if (input$c2 == "E=") {
                    ## set error
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "E", eval = "=", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, endxy_valid = game_state$startxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                    game_state$point_won_by <- other(game_state$current_team)
                    rally_ended()
                } else if (input$c2 %in% c("PP", "P2")) {
                    ## setter dump or second ball attack
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                    ## although we use PP and P2 in the R code here, we can use different combo codes in the dvw, following rdata$options$setter_dump_code and rdata$options$second_ball_attack_code
                    trg <- if (input$c2 == "PP") "S" else "~"
                    ## update target in the preceding set row, if there was one
                    ##if (tail(rc$skill, 1) == "E") rc$target[nrow(rc)] <- trg
                    ## these only seem to be populated when setter calls are used TODO
                    cmb <- if (input$c2 == "PP") rdata$options$setter_dump_code else rdata$options$second_ball_attack_code
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "A", tempo = "O", combo = cmb, sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                    rally_state("click attack end point")
                    game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                } else if (input$c2 == "F") {
                    ## freeball over
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "F", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                    ## TODO add end pos to this on next contact
                    game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                    rally_state("click freeball end point")
                } else if (input$c2 == "R=") {
                    ## delayed reception error (e.g. shanked pass)
                    ## just adjust the S & R evaluations and end the point
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) rc[Sidx, ] <- update_code_trow(rc[Sidx, ], eval = "#", game_state = game_state)
                    Ridx <- which(rc$skill == "R")
                    if (length(Ridx) == 1) rc[Ridx, ] <- update_code_trow(rc[Ridx, ], eval = "=", game_state = game_state)
                    rally_codes(rc)
                    game_state$current_team <- game_state$serving
                    game_state$point_won_by <- game_state$serving
                    rally_ended()
                }
            } else if (input$c2 %eq% "aPR") {
                ## opposition overpass attack
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/", if it was a set then "-"
                if (tail(rc$skill, 1) %in% c("R", "D", "E", "F") && tail(rc$team, 1) %eq% game_state$current_team) {
                    new_eval <- if (tail(rc$skill, 1) %eq% "E") "-" else "/"
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = new_eval, game_state = game_state)
                }
                ## and if prev was reception and prev to that was serve, adjust that too
                if (tail(rc$skill, 1) %eq% "R" && tail(rc$skill, 2)[1] %eq% "S" && tail(rc$team, 1) %eq% game_state$current_team) {
                    rc$eval[nrow(rc) - 1L] <- "/"
                }
                op <- if (!is.null(input$c2_opp_player)) input$c2_opp_player else 0L
                ## esz here actually came from start_x and start_y above
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "A", tempo = "O", combo = rdata$options$overpass_attack_code, sz = esz[1], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                rally_state("click attack end point")
            } else if (input$c2 %in% c("aF", "aF=")) {
                ## opposition dig on overpass
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/"
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/", if it was a set then "-"
                if (tail(rc$skill, 1) %in% c("R", "D", "E", "F") && tail(rc$team, 1) %eq% game_state$current_team) {
                    new_eval <- if (tail(rc$skill, 1) %in% c("R", "D")) "/" else "-"
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = new_eval, game_state = game_state)
                }
                op <- if (!is.null(input$c2_opp_player)) input$c2_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "F", eval = if (input$c2 %eq% "aF=") "=" else "~", sz = esz[1], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                if (input$c2 %eq% "aF=") {
                    game_state$point_won_by <- game_state$current_team
                    rally_ended()
                } else {
                    game_state$current_team <- other(game_state$current_team)
                    rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
                }
            }
            if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
            if (rally_state() != "confirm end of set") {
                remove_scout_modal()
                do_video("play")
            }
        }

        observeEvent(input$assign_c3, do_assign_c3())
        do_assign_c3 <- function() {
            ## possible values for input$c3 are: an attack code, Other attack, "F" Freeball, "E=" set error (if not scouting transition sets), or in some cases "PP"
            ##    "Opp. dig" = "aF", "Opp. overpass attack" = "aPR"
            start_t <- retrieve_video_time(game_state$start_t, video_times = video_times)
            sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
            rc <- rally_codes()
            if (input$c3 %in% c("aPR", "aF", "aF=")) {
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/", otherwise "-"
                ## but we can only do this in transition if we are scouting transition sets, otherwise we can't be sure if it was e.g. D/ or a set over (E-)
                ph <- if (nrow(rally_codes()) > 0) tail(make_plays2(rally_codes(), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)$phase, 1) else NA_character_
                if ((!ph %eq% "Transition" || isTRUE(rdata$options$transition_sets)) && tail(rc$skill, 1) %in% c("R", "D", "E", "F") && tail(rc$team, 1) %eq% game_state$current_team) {
                    ## is the R needed here, surely we can never see a preceding R on 3rd contact?
                    new_eval <- if (tail(rc$skill, 1) %in% c("R", "D")) "/" else "-"
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = new_eval, game_state = game_state)
                }
            }
            if (input$c3 %eq% "aPR") {
                ## opposition overpass attack
                op <- if (!is.null(input$c3_opp_player)) input$c3_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "A", tempo = "O", combo = rdata$options$overpass_attack_code, sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                rally_state("click attack end point")
            } else if (input$c3 %in% c("aF", "aF=")) {
                ## opposition dig on overpass
                op <- if (!is.null(input$c3_opp_player)) input$c3_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "F", eval = if (input$c3 %eq% "aF=") "=" else "~", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                if (input$c3 %eq% "aF=") {
                    game_state$point_won_by <- game_state$current_team
                    rally_ended()
                } else {
                    game_state$current_team <- other(game_state$current_team)
                    rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
                }
            } else if (input$c3 == "F") {
                ## freeball over
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = if (!is.null(input$c3_player)) input$c3_player else 0L, skill = "F", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                ## TODO add end pos to this on next contact
                game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                rally_state("click freeball end point")
            } else if (input$c3 == "PP") {
                ## setter dump
                ## if we are in reception phase, or we are scouting transition sets, then we are changing the previously-scouted set to a setter dump
                ## otherwise (transition phase and not scouting transition sets) it's a new touch
                ph <- if (nrow(rally_codes()) > 0) tail(make_plays2(rally_codes(), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)$phase, 1) else NA_character_
                replace_set_with_dump <- isTRUE(rdata$options$transition_sets) || !ph %eq% "Transition"
                if (replace_set_with_dump) {
                    if (nrow(rc) > 0 && tail(rc$skill, 1) %eq% "E") {
                        sp <- tail(rc$pnum, 1)
                        ## set position was recorded in start xy but end zone, need to move the end zone to start attack zone
                        asz <- tail(rc$ez, 1)
                        rc <- head(rc, -1) ## remove the preceding set row
                    } else {
                        sp <- NA_integer_
                        asz <- NA_integer_
                    }
                    ## also modify the rally state that we think was in operation for the rc entry we are about to add, because a PP should have happened on second contact (but currently we think it's third). If we don't do this, the undo sequence will be out of whack
                    rs <- "click second contact"
                } else {
                    sp <- if (!is.null(input$c3_player)) input$c3_player else 0L
                    rs <- rally_state()
                    asz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                }
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "A", tempo = "O", combo = rdata$options$setter_dump_code, sz = asz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rs, game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                ## if we are replacing a set with dump, this was clicked in "third contact" context, but the clicked location is actually now the first contact of other team after setter dump
                rally_state("click attack end point")
                game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                if (replace_set_with_dump) return(process_action()) ## jump straight to the c1 modal
                rally_state("click attack end point")
            } else if (input$c3 == "E=") {
                ## set error
                ## this is the third contact, so we should modify the existing set if we have scouted a set
                if (tail(rc$skill, 1) %eq% "E") {
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = "=", game_state = game_state)
                    rally_codes(rc)
                } else {
                    esz <- as.character(dv_xy2subzone(game_state$start_x, game_state$start_y))
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = if (!is.null(input$c3_player)) input$c3_player else 0L, skill = "E", eval = "=", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, endxy_valid = game_state$startxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                }
                game_state$point_won_by <- other(game_state$current_team)
                rally_ended()
            } else {
                ## attack
                ap <- if (!is.null(input$c3_player)) input$c3_player else 0L
                ac <- input$c3
                if (is.null(input$c3)) ac <- ""
                ## if the attack was by code, then the start zone is valid even if the coords are not, because zone is inferred from code not coords
                szvalid <- game_state$startxy_valid
                if (rdata$options$attacks_by %eq% "codes") {
                    if (length(ac) == 1 && nchar(ac) == 2) {
                        tempo <- tryCatch(rdata$dvw$meta$attacks$type[rdata$dvw$meta$attacks$code %eq% ac], error = function(e) "~")
                        targ <- tryCatch(rdata$dvw$meta$attacks$set_type[rdata$dvw$meta$attacks$code %eq% ac], error = function(e) "~")
                        ## use the start zone from the attacks combo table
                        newsz <- tryCatch(rdata$dvw$meta$attacks$attacker_position[rdata$dvw$meta$attacks$code %eq% ac], error = function(e) NA_integer_)
                        if (newsz %in% 1:9) {
                            sz <- newsz
                            szvalid <- TRUE ## inferred from combo code
                        }
                    } else {
                        ## other attack
                        ac <- "~~" ## no combo code
                        tempo <- targ <- "" ## filled below
                        sz <- adjusted_backrow_pos(game_state = game_state)$zone
                    }
                } else {
                    sz <- adjusted_backrow_pos(game_state = game_state)$zone
                    if (ac %in% c("PP", "P2")) {
                        tempo <- tryCatch(rdata$dvw$meta$attacks$type[rdata$dvw$meta$attacks$code %eq% ac], error = function(e) "~")
                        targ <- tryCatch(rdata$dvw$meta$attacks$set_type[rdata$dvw$meta$attacks$code %eq% ac], error = function(e) "~")
                    } else {
                        tempo <- if (ac %in% c("H", "M", "Q", "O")) ac else ""
                        ac <- "~~"
                        targ <- ""
                    }
                }
                if (nchar(tempo) != 1) tempo <- default_skill_tempo("A") ## fallback
                if (nchar(tempo) != 1) tempo <- "~" ## double-fallback
                if (nchar(targ) != 1 || targ %eq% "-") targ <- "~"
                ## update target in the preceding set row, if there was one
                ##if (tail(rc$skill, 1) == "E") rc$target[nrow(rc)] <- targ
                ## these only seem to be populated when setter calls are used TODO
                ## update set tempo
                if (tail(rc$skill, 1) == "E" && tempo != "~") rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], tempo = tempo, game_state = game_state)
                nb <- input$nblockers
                if (is.null(nb) || !nb %in% 0:3) nb <- "~"
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = ap, skill = "A", tempo = tempo, combo = ac, sz = sz, x_type = if (!is.null(input$hit_type) && input$hit_type %in% c("H", "P", "T")) input$hit_type else "~", num_p = nb, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, time = time_but_utc(), rally_state = rally_state(), game_state = game_state, start_zone_valid = szvalid, default_scouting_table = rdata$options$default_scouting_table)))
                rally_state("click attack end point")
                game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
            }
            if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
            if (rally_state() != "confirm end of set") {
                remove_scout_modal()
                do_video("play")
            }
        }

        output$rally_state <- renderUI({
            if (app_data$scout_mode != "type") {
                tags$div(id = "rallystate", tags$strong("Rally state: "), rally_state())
            } else {
                NULL
            }
        })

        ## handle the pre-selection of serve player and type
        observeEvent(list(rally_state(), game_state$serving, game_state$home_p1, game_state$visiting_p1), {
            if (rally_state() == "click serve start") {
                ## show the serve player and tempo pre-select buttons
                do_serve_preselect()
            } else {
                output$serve_preselect <- NULL
            }
        })

        do_serve_preselect <- function() {
            if (app_data$scout_mode != "type") { ## don't show if using typing input
                sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                ## sp should be the serving player
                ## other players that could be serving, if the rotation is somehow wrong
                other_sp <- get_players(game_state, team = game_state$serving, dvw = rdata$dvw) ## includes sp in here too
                names(other_sp) <- player_nums_to(other_sp, team = game_state$serving, dvw = rdata$dvw)
                serve_player_buttons <- make_fat_radio_buttons(choices = sort(other_sp), selected = sp, input_var = "serve_preselect_player")
                ## default serve type is either the most common serve type by this player, or the default serve type
                st_default <- get_player_serve_type(px = rdata$dvw$plays, serving_player_num = sp, game_state = game_state, opts = rdata$options)
                if (is.na(st_default)) st_default <- default_skill_tempo("S")
                chc <- rdata$options$skill_tempo_map %>% dplyr::filter(.data$skill == "Serve") %>% mutate(tempo = sub(" serve", "", .data$tempo))
                chc <- setNames(chc$tempo_code, chc$tempo)
                serve_type_buttons <- make_fat_radio_buttons(choices = chc, selected = st_default, input_var = "serve_preselect_type")
                output$serve_preselect <- renderUI(
                    tags$div(tags$strong("Serve type:"), do.call(fixedRow, lapply(serve_type_buttons, function(but) column(2, but))),
                             tags$strong("Serve player:"), do.call(fixedRow, lapply(serve_player_buttons, function(but) column(2, but))))
                )
            }
        }

        observeEvent(input$admin_dismiss, dismiss_admin_modal(editing = editing, scout_mode = app_data$scout_mode))

        observeEvent(input$change_setter, {
            if (!is.null(input$change_setter)) {
                show_change_setter_modal(code = input$change_setter, game_state = game_state, dvw = rdata$dvw)
            }
        })

        observeEvent(input$substitution, {
            if (!is.null(input$substitution)) show_substitution_pane()
        })
        show_substitution_pane <- function(sub_code) {
            if (missing(sub_code)) sub_code <- isolate(input$substitution)
            show_substitution_modal(sub_code = sub_code, game_state = game_state, dvw = rdata$dvw)
        }

        review_pane_active <- reactiveVal(FALSE)
        observeEvent(list(input$rv_height, input$rv_width), {
            if ((length(input$rv_height) < 1 || is.na(input$rv_height) || input$rv_height <= 0 || length(input$rv_width) < 1 || is.na(input$rv_width) || input$rv_width <= 0) && review_pane_active()) {
                dojs("Shiny.setInputValue('rv_height', $('#review_player').innerHeight()); Shiny.setInputValue('rv_width', $('#review_player').innerWidth());")
            }
        })
        ## for the review overlay, we always need the plot shown (so it can capture click/drags). But if we are using canvas, then only plot once (blank plot) and don't update it
        observe({
            ## canvas overlay plot
            if (!isTRUE(input$review_overlay_nocanvas > 0)) {
                req(input$rv_width, input$rv_height)
                if (input$rv_width > 0 && input$rv_height > 0) {
                    ## draw directly with canvas
                    w <- input$rv_width
                    h <- input$rv_height
                    cc <- canvas_drawing$new(id = "review_overlay_canvas", width = w, height = h, on_fail = "Shiny.setInputValue('review_overlay_nocanvas', 1);")
                    ## if context fails, fall back to base plotting
                    if (isTRUE(prefs$show_courtref) && !is.null(overlay_court_lines())) {
                        oxy <- overlay_court_lines()
                        ## don't need to account for aspect ratios, because the review pane will not be letterboxed
                        cc$lines(x0 = oxy$image_x, y0 = oxy$image_y, x1 = oxy$xend, y1 = oxy$yend, col = app_data$styling$court_lines_colour, unit = "npc")
                    }
                    if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                        ixy <- setNames(crt_to_vid(overlay_points(), arfix = FALSE, detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data), c("x", "y"))
                        if (any(overlay_points()$valid)) {
                            cc$circles(x = ixy$x[overlay_points()$valid], y = ixy$y[overlay_points()$valid], r = 0.02, col = "white", fill_col = "dodgerblue", unit = "npc")
                        }
                        if (!all(overlay_points()$valid)) {
                            cc$circles(x = ixy$x[!overlay_points()$valid], y = ixy$y[!overlay_points()$valid], r = 0.02, col = "white", fill_col = "firebrick", unit = "npc")
                        }
                    }
                    cc$draw()
                }
            }
        })
        observe({
            ## review overlay blank plot
            req(input$rv_width, input$rv_height)
            if (!isTRUE(input$review_overlay_nocanvas > 0) && input$rv_width > 0 && input$rv_height > 0) {
                ## using canvas, so make a blank plot and react only to size
                output$review_overlay <- renderPlot({
                    opar <- par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
                    plot(c(0, 1), c(0, 1), xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = NA, ylab = NA, axes = FALSE, xaxs = "i", yaxs = "i")
                    par(opar)
                }, bg = "transparent", height = input$rv_height)
            }
        })
        observe({
            ## review overlay full plot
            req(input$rv_width, input$rv_height)
            if (isTRUE(input$review_overlay_nocanvas > 0) && input$rv_width > 0 && input$rv_height > 0) {
                ## do the overlay by base plotting, but this is slow
                output$review_overlay <- renderPlot({
                    opar <- par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
                    plot(c(0, 1), c(0, 1), xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = NA, ylab = NA, axes = FALSE, xaxs = "i", yaxs = "i")
                    if (isTRUE(prefs$show_courtref) && !is.null(overlay_court_lines())) {
                        oxy <- overlay_court_lines()
                        ## don't need to account for aspect ratios, because the review pane will not be letterboxed
                        segments(x0 = oxy$image_x, y0 = oxy$image_y, x1 = oxy$xend, y1 = oxy$yend, col = app_data$styling$court_lines_colour)
                    }
                    if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                        ixy <- setNames(crt_to_vid(overlay_points(), arfix = FALSE, detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data), c("x", "y"))
                        ## points as blue, invalid points as red
                        points(ixy$x[overlay_points()$valid], ixy$y[overlay_points()$valid], bg = "dodgerblue", pch = 21, col = "white", cex = 2.5)
                        points(ixy$x[!overlay_points()$valid], ixy$y[!overlay_points()$valid], bg = "firebrick", pch = 21, col = "white", cex = 2.5)
                    }
                    par(opar)
                }, bg = "transparent", height = input$rv_height)
            }
        })

        rv_clickdrag <- reactiveValues(mousedown = NULL, mousedown_time = NULL, closest_down = NULL, mouseup = NULL)
        last_rv_mouse_pos <- reactiveVal(NULL)
        observeEvent(input$did_rv_mousedown, {
            ##cat("rv mouse down\n")
            if (!is.null(detection_ref()$court_ref)) {
                closest <- NULL
                if (!is.null(input$rv_hover)) {
                    px <- list(x = input$rv_hover$x, y = input$rv_hover$y)
                    ## find the closest point, using court space for the distance
                    cpx <- vid_to_crt(px, arfix = FALSE, detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data)
                    if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                        closest <- which.min((overlay_points()$x - cpx$x[1])^2 + (overlay_points()$y - cpx$y[1])^2)
                        if (length(closest) < 1) closest <- NA_integer_
                    } else {
                        closest <- NA_integer_
                    }
                } else {
                    px <- NULL
                }
            } else {
                ##cat("no court ref\n")
                px <- NULL
                closest <- NA_integer_
            }
            rv_clickdrag$mousedown <- px
            rv_clickdrag$mousedown_time <- R.utils::System$currentTimeMillis()
            rv_clickdrag$closest_down <- closest
        })

        observeEvent(input$did_rv_mouseup, {
            ## stop dragging
            rv_clickdrag$mousedown <- NULL
            rv_clickdrag$mousedown_time <- NULL
            rv_clickdrag$closest_down <- NULL
        })

        observeEvent(input$rv_hover, {
            ##cat("rv plot hover\n")
            ## triggered when mouse moved over the plot. Use this to track drag position
            if (!is.null(input$rv_hover)) last_rv_mouse_pos(list(x = input$rv_hover$x, y = input$rv_hover$y))
        })
        last_rv_refresh_time <- NA_real_
        observe({
            ## here we handle either a click (find the nearest point, and move it to the click location)
            ##  or a drag (move the point that was closest to the drag start point, to the click location)
            px <- last_rv_mouse_pos()
            if (!is.null(px)) {
                ##cat("was rv drag or click\n")
                ## if a click, use the click position, else use the last_rv_mouse_pos (but this might lag the actual click pos, because of the hover lag)
                closest <- NA_integer_
                if (!was_mouse_drag(rv_clickdrag) && !is.null(input$rv_click)) {
                    ##cat("was click\n")
                    px <- input$rv_click
                    ## find the closest point, using court space for the distance
                    cpx <- vid_to_crt(px, arfix = FALSE, detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data)
                    if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                        closest <- which.min((overlay_points()$x - cpx$x[1])^2 + (overlay_points()$y - cpx$y[1])^2)
                        if (length(closest) < 1) closest <- NA_integer_
                    } else {
                        closest <- NA_integer_
                    }
                } else {
                    ##cat("was drag\n")
                    closest <- rv_clickdrag$closest_down
                }
                now_time <- R.utils::System$currentTimeMillis() ## use this to reduce redraw rate when dragging
                if (!was_mouse_drag(rv_clickdrag) || (is.na(last_rv_refresh_time) || (now_time - last_rv_refresh_time) > 100)) {
                    last_rv_refresh_time <<- now_time
                    if (!is.null(overlay_points()) && nrow(overlay_points()) > 0 && length(closest) && !is.na(closest)) {
                        ## convert px from image to court space
                        px <- vid_to_crt(px, arfix = FALSE, detection_ref = detection_ref, input = input, current_video_src = current_video_src, app_data = app_data)
                        op <- isolate(overlay_points())
                        op$x[closest] <- px$x
                        op$y[closest] <- px$y
                        op$valid[closest] <- TRUE
                        overlay_points(op)
                        ## but also game_state
##                        cat("op is: ", cstr(op), ", updating row ", closest, "\n")
                        if (op$start_end[closest] %eq% "start") {
                            game_state$start_x <- px$x
                            game_state$start_y <- px$y
                            game_state$startxy_valid <- TRUE
                        } else if (op$start_end[closest] %eq% "end") {
                            game_state$end_x <- px$x
                            game_state$end_y <- px$y
                            game_state$endxy_valid <- TRUE
                        }
                    }
                } else {
                    ## was drag but redraw delay time hasn't passed yet
                    shiny::invalidateLater(50)
                }
            }
        })

        ## TODO now if courtxy is updated here, it needs to be propagated into game_state start/mid/end pos

        observeEvent(input$undo, {
            do_undo()
            dismiss_admin_modal(editing = editing, scout_mode = app_data$scout_mode)
        })

        do_undo <- function() {
            rc <- rally_codes()
            if (!is.null(rc) && nrow(rc) > 0) {
                restore_rally_state <- tail(rc$rally_state, 1)
                restore_current_team <- tail(rc$current_team, 1)
                restore_t <- head(rc$t, 1)
                rc <- head(rc, -1)
                rally_codes(rc)
                ## reset the rally_state back to what it was for the last-existing code
                rally_state(restore_rally_state)
                ## and the current team
                game_state$current_team <- restore_current_team
                if (nrow(rc) < 1) game_state$rally_started <- FALSE ## we've undone all actions, so the rally is back to it's pre-start state. This allows us to e.g. display the serve-switch and rotate buttons in the court diagram module
                ## rewind the video to the time of the last action, or if we've removed all actions then the prior time of the first one
                if (nrow(rc) > 0) do_video("set_time", tail(rc$t, 1)) else if (length(restore_t) == 1 && !is.na(restore_t)) do_video("set_time", restore_t) else do_video("rew", 3)
            } else {
                ## undo the last code in plays2, this is trickier
                ## we need the game_state and rally_state to reset to, these are in plays2
                ## also need to reconstruct the rally_codes to this point in the rally
                ## game_state is a list, and will be NULL or NA for non-skill rows (?)
                ## so when undoing, remove the last row in plays2 AND all preceding rows with NULL game_state??
                p2 <- rdata$dvw$plays2
                if (!is.null(p2) && nrow(p2) > 0) {
                    p2keep <- rep(TRUE, nrow(p2))
                    temp <- p2$rally_codes
                    ## strip the trailing rows that have NULL or NA rally_code entries, these will be non-skill rows
                    rcnull <- vapply(temp, function(z) is.null(z) || all(is.na(z)), FUN.VALUE = TRUE)
                    ## but note that if we've started from a partially-scouted dvw file, we won't have rally_codes saved in plays2
                    ## so if we have a skill code, treat this as not-undoable
                    rcnull <- rcnull & !substr(p2$code, 4, 4) %in% c("S", "R", "A", "B", "D", "E", "F")
                    todrop <- rep(FALSE, length(rcnull))
                    for (i in rev(seq_along(rcnull))) { if (rcnull[i]) todrop[i] <- TRUE else break }
                    temp <- temp[!todrop]
                    p2keep[todrop] <- FALSE
                    rcnull <- rcnull[!todrop]
                    ## AND strip the last non-NULL/NA rally_code row, this is the one we are undoing
                    ## but save its rally_state and current_team first
                    restore_rally_state <- temp[[length(temp)]]$rally_state
                    restore_current_team <- temp[[length(temp)]]$current_team
                    p2keep[length(temp)] <- FALSE
                    temp <- temp[-length(temp)]
                    rcnull <- rcnull[-length(rcnull)]
                    ## then everything back to the next NULL goes into rally_codes
                    totake <- rep(FALSE, length(rcnull))
                    for (i in rev(seq_along(rcnull))) { if (!rcnull[i]) totake[i] <- TRUE else break }
                    new_rc <- bind_rows(temp[totake])
                    p2keep[totake] <- FALSE
                    ## set plays2
                    rdata$dvw$plays2 <- p2[p2keep, ]
                    ## set rally state
                    rally_state(restore_rally_state)##tail(new_rc$rally_state, 1))
                    ## set game state
                    gs <- new_rc$game_state[[nrow(new_rc)]]
                    for (nm in names(gs)) game_state[[nm]] <- gs[[nm]]
                    game_state$current_team <- restore_current_team
                    ## reset rally codes
                    rally_codes(new_rc)
                    do_video("set_time", tail(new_rc$t, 1))
                }
            }
            if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) overlay_points(head(overlay_points(), -1))
            if (app_data$with_video && !is.null(rally_codes()) && nrow(rally_codes()) > 0 && !all(is.na(rally_codes()$t))) {
                ## set time to last action minus play_overlap
                new_vt <- max(rally_codes()$t, na.rm = TRUE) - app_data$play_overlap
                do_video("set_time", new_vt)
            }
        }

        ## for manual/direct code entry, this is triggered by the button on the admin modal
        observeEvent(input$enter_code, {
           insert_data_row("below")
        })

        insert_data_row <- function(where) {
            if (missing(where)) where <- "above"
            where <- tolower(where)
            if (!where %in% c("above", "below")) where <- "above" ## default
            ridx <- playslist_mod$current_row()
            if (!is.null(ridx) && !is.na(ridx)) {
                ##if (where == "above" && ridx > 1) ridx <- ridx-1L ## we are inserting above the selected row, so use the previous row to populate this one
                ## otherwise (if inserting below) use the current row (ridx) as the template
                editing$active <- paste0("insert ", where)
                show_manual_code_modal(editing$active, dvw = rdata$dvw)
            }
        }

        delete_data_row <- function() {
            editing$active <- "delete"
            show_delete_code_modal()
        }

        show_delete_code_modal <- function() {
            ridx <- playslist_mod$current_row()
            if (!is.null(ridx) && !is.na(ridx)) {
                thiscode <- tryCatch(codes_from_rc_rows(get_current_rally_code(playslist_mod = playslist_mod, rdata = rdata, rally_codes = rally_codes)), error = function(e) NULL)
                showModal(modalDialog(title = "Delete current row", size = "l", footer = tags$div(actionButton("delete_commit", label = "Delete code (or press Enter)"), actionButton("cancel_back_to_playslist", label = "Cancel (or press Esc)")),
                                      if (!is.null(thiscode)) tags$p(tags$strong("To delete:"), thiscode)
                                      ))
                focus_to_modal_element("delete_commit")
            }
        }
        observeEvent(input$cancel_back_to_playslist, {
            editing$active <- NULL
            removeModal()
            if (app_data$scout_mode == "type") focus_to_playslist() ## focus here so that focus returns to the playslist after it is re-rendered
        })

        observeEvent(input$delete_commit, do_delete_code())
        do_delete_code <- function(ridx) {
            if (!is.null(editing$active)) {
                if (missing(ridx)) ridx <- playslist_mod$current_row()
                if (!is.null(ridx) && !is.na(ridx)) {
                    if (app_data$scout_mode == "type") focus_to_playslist() ## focus here so that focus returns to the playslist after it is re-rendered
                    if (ridx <= nrow(rdata$dvw$plays2)) {
                        rdata$dvw$plays2 <- rdata$dvw$plays2[-ridx, ]
                    } else if ((ridx - nrow(rdata$dvw$plays2)) <= nrow(rally_codes())) {
                        rc <- rally_codes()
                        rc <- rc[setdiff(seq_len(nrow(rc)), ridx - nrow(rdata$dvw$plays2)), ]
                        rally_codes(rc)
                    }
                }
                editing$active <- NULL
                removeModal()
                if (app_data$scout_mode == "type") focus_to_playslist()
            }
        }

        edit_data_row <- function() {
            editing$active <- "edit"
            show_manual_code_modal(editing$active, ridx = playslist_mod$current_row(), dvw = rdata$dvw)
        }

        ## the helpers that are defined as functions in code_bits_tbl are dynamic, they depend on skill/evaluation
        ## add UI outputs corresponding to the helper functions here as needed
        output$code_entry_helper_team_ui <- renderUI({
            HTML(team_helper(rdata$dvw))
        })
        output$code_entry_helper_number_ui <- renderUI({
            HTML(number_helper(input$code_entry_team, rdata$dvw))
        })
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

        observeEvent(input$manual_code, {
            res <- handle_non_skill_code(input$manual_code)
            if (res$ok) {
                editing$active <- NULL
                if (!res$end_of_set) {
                    removeModal()
                    do_video("play")
                    if (app_data$scout_mode == "type") focus_to_scout_bar()
                }
            }
        })

        ## deals with non-skill codes entered manually, or the same codes sent via buttons
        handle_non_skill_code <- function(code, process_rally_end = TRUE) {
            ok <- TRUE
            end_of_set <- FALSE
            if (!is.null(code)) {
                if (grepl("^>", code) || code %in% c("*T", "aT")) {
                    ## comment, or perhaps >LUp, or timeout
                    rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(code, game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
                } else if (code %in% c("*p", "ap")) {
                    game_state$point_won_by <- substr(code, 1, 1)
                    end_of_set <- if (process_rally_end) rally_ended() else NA
                } else if (grepl("^[a\\*][cC]", code)) {
                    ## substitution
                    if (code %in% c("*c", "*C")) {
                        p_out <- as.numeric(input$ht_sub_out)
                        p_in <- as.numeric(input$ht_sub_in)
                    } else if (code %in% c("ac", "aC")) {
                        p_out <- as.numeric(input$vt_sub_out)
                        p_in <- as.numeric(input$vt_sub_in)
                    } else {
                        ## get numbers from code
                        p_out <- as.numeric(sub("[:\\.].*", "", substr(code, 3, 99)))
                        p_in <- as.numeric(sub(".*[:\\.]", "", substr(code, 3, 99)))
                    }
                    if (length(p_out) == 1 && length(p_in) == 1 && !is.na(p_out) && !is.na(p_in)) {
                        tm <- substr(code, 1, 1)
                        current_setter <- get_setter(game_state, team = tm)
                        game_state <- game_state_make_substitution(game_state, team = tm, player_out = p_out, player_in = p_in, dvw = rdata$dvw)
                        rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(paste0(substr(code, 1, 1), "c", ldz(p_out), ":", ldz(p_in)), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
                        ## if we just substituted the player about to serve, we need to update the serve preselect buttons
                        do_serve_preselect()
                        ## did we substitute the setter?
                        if (p_out %eq% current_setter && current_setter > 0) {
                            ## yes we did
                            show_change_setter_modal(code = paste0(tm, "P"), game_state = game_state, dvw = rdata$dvw)
                            ok <- FALSE ## keep this modal showing for setter input
                        }
                    } else {
                        ## players in/out not selected, ignore
                        ok <- FALSE
                    }
                } else if (code %in% c("*P", "aP")) {
                    ## setter on court
                    ## input$new_setter will be in format player_number@position on court
                    new_setr <- as.numeric(sub("@.*", "", input$new_setter))
                    new_pos <- as.numeric(sub(".*@", "", input$new_setter))
                    if (!is.na(new_setr) && !is.na(new_pos)) {
                        if (substr(code, 1, 1) == "*") {
                            game_state$home_setter_position <- new_pos
                        } else {
                            game_state$visiting_setter_position <- new_pos
                        }
                        ## and add the aPXX, azYY codes
                        rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(c(paste0(substr(code, 1, 1), "P", ldz(new_setr)), paste0(substr(code, 1, 1), "z", new_pos)), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
                    } else {
                        ok <- FALSE
                    }
                }
            }
            list(ok = ok, end_of_set = end_of_set)
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

        sc_to_edit <- reactiveVal(NULL)
        sc_newvalue <- reactiveVal(NULL)
        observeEvent(input$scedit, {
            editing$active <- "edit_shortcuts"
            ## TODO validate the shortcut?
            sc_to_edit(input$scedit)
            ## playslist or general shortcut?
            sctype <- substr(input$scedit, 1, 2) ##if (grepl("^PL", input$scedit)) "PL" else ""
            if (!sctype %in% c("PL", "TY", "CL")) {
                warning("unrecognized shortcut type, ignoring: ", input$scedit)
            } else {
                this_sc <- sub("^(PL|TY|CL)", "", input$scedit)
                showModal(vwModalDialog(title = "Modify Keyboard shortcut", easyClose = FALSE, width = "50%", class = "scedit-modal", footer = NULL,
                                        tags$h4("Shortcut for:", this_sc, if (sctype == "PL") " (in plays list)" else if (sctype == "TY") " (in 'type' mode)" else " in 'click' mode"),
                                        uiOutput("scedit_out"),
                                        tags$hr(),
                                        fixedRow(column(2, actionButton("scedit_cancel", "Cancel", class = "cancel fatradio")),
                                                 column(2, offset = 8, actionButton("scedit_save", "Apply and save", class = "continue fatradio")))
                                        ))
                output$scedit_out <- renderUI({
                    this_sc <- sub("^(PL|TY|CL)", "", input$scedit)
                    sctype <- substr(input$scedit, 1, 2) ##if (grepl("^PL", input$scedit)) "PL" else ""
                    cat("out this_sc is:", cstr(this_sc), "\n")
                    tags$code(app_data[[if (sctype == "PL") "playstable_shortcuts" else if (sctype == "TY") "type_shortcuts" else "click_shortcuts"]][[this_sc]])
                })
            }
        })
        observeEvent(input$scedit_cancel, {
            sc_to_edit(NULL)
            sc_newvalue(NULL)
            show_shortcuts(app_data)
        })
        observeEvent(input$scedit_save, {
            if (!is.null(sc_to_edit()) && !is.null(sc_newvalue())) {
                this_sc <- sub("^(PL|TY|CL)", "", sc_to_edit())
                this_sclist <- if (grepl("^PL", sc_to_edit())) "playstable_shortcuts" else if (grepl("^TY", sc_to_edit())) "type_shortcuts" else "click_shortcuts"
                cat("setting app_data$", this_sclist, "$", this_sc, " to: ", sc_newvalue(), "\n", sep = "")
                app_data[[this_sclist]][[this_sc]] <<- sc_newvalue()
            }
            ## TODO transfer these to the active app_data$shortcuts object
            ## TODO save to prefs file
            sc_to_edit(NULL)
            sc_newvalue(NULL)
            show_shortcuts(app_data)
        })

        observeEvent(input$general_help, introjs(session, options = list("nextLabel" = "Next", "prevLabel" = "Previous", "skipLabel" = "Skip")))
        observeEvent(input$show_shortcuts, show_shortcuts(app_data))

        ## TODO @param key_remapping list: a named list of key remappings, with entries as per [ov_default_key_remapping()]

        ## functions for saving files
        save_file_basename <- reactive({
            if (!is.null(rdata$dvw$meta$filename) && !is.na(rdata$dvw$meta$filename) && nchar(rdata$dvw$meta$filename)) {
                fs::path_ext_remove(basename(rdata$dvw$meta$filename))
            } else if (app_data$with_video && !is.null(app_data$video_src) && nchar(app_data$video_src) && !is_url(app_data$video_src)) {
                basename(fs::path_ext_remove(app_data$video_src))
            } else {
                "myfile"
            }
        })

        output$save_dvw_button <- downloadHandler(
            filename = function() paste0(save_file_basename(), ".dvw"),
            content = function(file) {
                tryCatch({
                    dv_write2(update_meta(rp2(rdata$dvw)), file = file, convert_cones = isTRUE(input$dvw_save_with_cones))
                    removeModal()
                }, error = function(e) {
                    temp <- save_to_ovs(rdata = rdata, app_data = app_data, courtref1 = detection_ref1(), courtref2 = detection_ref2())
                    show_save_error_modal(msg = temp$error_message, ovs_ok = temp$ok, tempfile_name = temp$filename)
                    NULL
                })
            }
        )
        ## ask about exporting with cones
        observeEvent(input$ask_save_dvw_button, show_save_dvw_modal())

        output$save_rds_button <- downloadHandler(
            filename = function() paste0(save_file_basename(), ".ovs"),
            content = function(file) {
                tryCatch({
                    ## TODO flush any rally codes to plays2 - but note that then we won't have the right rally_state when we restart
                    ## so might not be able to do this
                    out <- update_meta(rp2(rdata$dvw))
                    out$plays <- NULL ## don't save this
                    out$game_state <- isolate(reactiveValuesToList(game_state))
                    out$scouting_options <- isolate(rdata$options)
                    ## save court refs
                    dr <- list()
                    isolate({
                        if (!is.null(detection_ref1()$court_ref)) dr[[app_data$video_src]] <- detection_ref1()
                        if (!is.null(detection_ref2()$court_ref) && "video_src2" %in% names(app_data) && !is.na(app_data$video_src2) && nzchar(app_data$video_src2)) dr[[app_data$video_src2]] <- detection_ref2()
                    })
                    out$detection_refs <- dr
                    saveRDS(out, file)
                }, error = function(e) {
                    temp <- save_to_ovs(rdata = rdata, app_data = app_data, courtref1 = detection_ref1(), courtref2 = detection_ref2())
                    show_save_error_modal(msg = temp$error_message, ovs_ok = temp$ok, tempfile_name = temp$filename)
                    NULL
                })
            }
        )

        ## disaster recovery
        shiny::onSessionEnded(function() {
            try({
                save_to_ovs(rdata = rdata, app_data = app_data, courtref1 = detection_ref1(), courtref2 = detection_ref2(), game_state = game_state, was_session_end = TRUE)
            })
        })
        ## seek to video time on startup
        if ("video_time" %in% names(app_data$dvw$plays2) && nrow(app_data$dvw$plays2) > 0) {
            temp_vt <- na.omit(app_data$dvw$plays2$video_time)
            if (length(temp_vt) > 0) do_video("set_time", rebase_time(max(temp_vt), time_from = 1, rdata = rdata)) ## rebase here should not be necessary, unless somehow we've started on video 2
        }

        ## reports
        output$reports_ui <- renderUI({
            if (ov_pandoc_ok() && nrow(rdata$dvw$plays2) > 0) {
                shinyWidgets::dropdown(inputId = "reports", label = "Reports", actionButton("mr_generate", "Match report", class = "leftbut"))
            } else {
                NULL
            }
        })
        observeEvent(input$mr_generate, {
            editing$active <- "match report"
            ov2_generate_match_report(dvw = rdata$dvw, app_data = app_data)
        })

        ## other auxiliary functions
        do_rotate <- function(tm = "home") {
            tm <- match.arg(tm, c("home", "visiting"))
            hv <- if (tm == "home") "*" else "a"
            game_state[[paste0(tm, "_setter_position")]] <- rotpos(game_state[[paste0(tm, "_setter_position")]], n = length(pseq))
            temp <- rotvec(as.numeric(reactiveValuesToList(game_state)[paste0(tm, "_p", pseq)]))
            for (i in pseq) game_state[[paste0(tm, "_p", i)]] <- temp[i]
            poscode <- paste0(hv, "z", game_state[[paste0(tm, "_setter_position")]])
            rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(poscode, game_state = game_state, dvw = rdata$dvw)))
        }
    }
}
