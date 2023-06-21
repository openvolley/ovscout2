ov_scouter_server <- function(app_data) {
    function(input, output, session) {
        debug <- 0L
        cstr <- function(z) capture.output(str(z))
        have_warned_auto_save <- FALSE
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
        rdata <- reactiveValues(dvw = app_data$dvw, options = app_data$options)
        prefs <- reactiveValues(scout_name = app_data$scout_name, show_courtref = app_data$show_courtref, scoreboard = app_data$scoreboard, ball_path = app_data$ball_path, playlist_display_option = app_data$playlist_display_option, review_pane = app_data$review_pane)

        ## function to reference a video time measured on the time scale of video "from", to its equivalent time relative to video "to"
        rebase_time <- function(t, time_to = 1, time_from) {
            if (missing(time_from)) time_from <- current_video_src()
            if (!time_from %in% c(1, 2)) time_from <- 1
            if (!time_to %in% c(1, 2)) time_to <- 1
            if (time_from > 1) t <- t - rdata$dvw$video2_offset
            ## t is now relative to 1
            if (time_to > 1) t <- t + rdata$dvw$video2_offset
            t
        }

        pseq <- if (app_data$is_beach) 1:2 else 1:6

        have_second_video <- !is.null(app_data$video_src2)
        current_video_src <- reactiveVal(1L) ## start with video 1
        preview_video_src <- reactiveVal(1L)
        observe({
            chk <- is.null(input$video_width) || is.na(input$video_width) || is.null(input$video_height) || is.na(input$video_height) ||
                ## zero width or height is also invalid, except if it's a YT video
                (current_video_src() == 1L && !is_youtube_url(app_data$video_src) && input$video_width < 1) || (current_video_src() == 2L && !is_youtube_url(app_data$video_src2) && input$video_height < 1)
            if (chk) {
                dojs("Shiny.setInputValue('video_width', vidplayer.videoWidth()); Shiny.setInputValue('video_height', vidplayer.videoHeight());")
                shiny::invalidateLater(200)
            }
        })
        get_src_type <- function(src) {
            type <- "local"
            if (is_youtube_url(src)) {
                type <- "youtube"
            } else if (!is_url(src)) {
                src <- file.path(app_data$video_server_base_url, basename(src))
            }
            list(src = src, type = type)
        }
        observeEvent(input$switch_video, {
            do_switch_video()
        })
        do_switch_video <- function() {
            if (have_second_video) {
                current_video_src(3L - current_video_src())
                if (current_video_src() == 1L) {
                    new_src <- app_data$video_src
                    offs <- -rdata$dvw$video2_offset
                } else {
                    new_src <- app_data$video_src2
                    offs <- rdata$dvw$video2_offset
                }
                new_src <- get_src_type(new_src)
                myjs <- paste0("var ct=vidplayer.currentTime(); ct=ct", if (offs >= 0) "+", offs, "; if (ct >= 0) { vidplayer.src(", if (new_src$type == "youtube") paste0("{ \"type\": \"video/youtube\", \"src\": \"", new_src$src, "\"}") else paste0("\"", new_src$src, "\""), "); vidplayer.currentTime(ct); vidplayer.play(); Shiny.setInputValue('video_width', vidplayer.videoWidth()); Shiny.setInputValue('video_height', vidplayer.videoHeight()); }")
                dojs(myjs)
            }
        }
        ## video 2 offset tweak
        ## TODO, this would be better with side-by-side videos, but couldn't get that working reliably
        ## maybe side-by-side still frames would be even better (though difficult to do with remote videos)
        observeEvent(input$v2_offset, {
            prevsrc <- get_src_type(if (preview_video_src() == 1L) app_data$video_src else app_data$video_src2)
            editing$active <- "video offset"
            showModal(vwModalDialog(title = "Video setup", footer = NULL, width = 100,
                                    uiOutput("preview_header"),
                                    HTML(paste0("<video id=\"video_preview\" style=\"width:100%; height:50vh;\" class=\"video-js\" data-setup='{ ", if (prevsrc$type == "youtube") "\"techOrder\": [\"youtube\"], ", "\"controls\": true, \"autoplay\": true, \"preload\": \"auto\", \"liveui\": true, \"muted\": true, \"sources\": ", if (prevsrc$type == "youtube") paste0("[{ \"type\": \"video/youtube\", \"src\": \"", prevsrc$src, "\"}]") else paste0("[{ \"src\": \"", prevsrc$src, "\"}]"), " }'>\n", "<p class=\"vjs-no-js\">This app cannot be used without a web browser that <a href=\"https://videojs.com/html5-video-support/\" target=\"_blank\">supports HTML5 video</a></p></video>")),
                                    fluidRow(column(4, offset = 2, numericInput("v2_offset_value", "Video 2 offset (s):", value = rdata$dvw$video2_offset)),
                                             column(4, actionButton("switch_preview", "Switch video"))),
                                    tags$br(),
                                    tags$hr(),
                                    fixedRow(column(2, offset = 10, actionButton("preview_dismiss", "Return to scouting", class = "continue fatradio")))
                                    ))
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

        observeEvent(input$switch_preview, {
            preview_video_src(3L - preview_video_src())
            if (preview_video_src() == 1L) {
                new_src <- app_data$video_src
                offs <- -rdata$dvw$video2_offset
            } else {
                new_src <- app_data$video_src2
                offs <- rdata$dvw$video2_offset
            }
            new_src <- get_src_type(new_src)
            myjs <- paste0("var ct=videojs('video_preview').currentTime(); ct=ct", if (offs >= 0) "+", offs, "; if (ct >= 0) { videojs('video_preview').src(", if (new_src$type == "youtube") paste0("{ \"type\": \"video/youtube\", \"src\": \"", new_src$src, "\"}") else paste0("\"", new_src$src, "\""), "); videojs('video_preview').currentTime(ct); videojs('video_preview').play(); }")
            dojs(myjs)
        })

        observeEvent(input$v2_offset_value, {
            ## TODO need this to react straight away, not after the value has been entered (the input box has been exited)
            if (!is.null(input$v2_offset_value) && !is.na(input$v2_offset_value)) rdata$dvw$video2_offset <<- input$v2_offset_value
        })

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
        temp$start_x <- temp$start_y <- temp$mid_x <- temp$mid_y <- temp$end_x <- temp$end_y <- NA_real_
        temp$startxy_valid <- temp$midxy_valid <- temp$endxy_valid <- FALSE
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
        court_inset <- callModule(mod_courtrot2, id = "courtrot", rdata = rdata, game_state = game_state, rally_codes = rally_codes, rally_state = rally_state, current_video_src = current_video_src, styling = app_data$styling, with_ball_path = reactive(prefs$ball_path))
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
        detection_ref1 <- reactiveVal({ if (!is.null(app_data$court_ref)) app_data$court_ref else NULL })
        ## for the court reference modules, pass the video file as well as the URL. Video will be shown, but the metadata can be stored/retrieved from the file
        courtref1 <- callModule(mod_courtref, id = "courtref1", video_file = if (!is_url(app_data$video_src)) app_data$video_src else NULL, video_url = if (is_url(app_data$video_src)) app_data$video_src else file.path(app_data$video_server_base_url, basename(app_data$video_src)), detection_ref = detection_ref1, main_video_time_js = "vidplayer.currentTime()", styling = app_data$styling)
        detection_ref2 <- reactiveVal({ if (!is.null(app_data$court_ref2)) app_data$court_ref2 else NULL })
        courtref2 <- if (have_second_video) callModule(mod_courtref, id = "courtref2", video_file = if (!is_url(app_data$video_src2)) app_data$video_src2 else NULL, video_url = if (is_url(app_data$video_src2)) app_data$video_src2 else file.path(app_data$video_server_base_url, basename(app_data$video_src2)), detection_ref = detection_ref2, main_video_time_js = "vidplayer.currentTime()", styling = app_data$styling) else NULL
        detection_ref <- reactive(if (current_video_src() < 2) detection_ref1() else detection_ref2()) ## whichever is associated with the current view
        courtref <- reactiveValues(active = FALSE)
        observe({
            if (current_video_src() < 2) {
                courtref$active <- courtref1$active
            } else {
                courtref$active <- courtref2$active
            }
        })
        tsc_mod <- callModule(mod_teamscores, id = "tsc", game_state = game_state, rdata = rdata, styling = app_data$styling, visible = reactive(prefs$scoreboard))

        playslist_mod <- callModule(mod_playslist, id = "playslist", rdata = rdata, plays_cols_to_show = plays_cols_to_show,
                                    plays_cols_renames = plays_cols_renames, display_option = reactive(prefs$playlist_display_option))

        video_state <- reactiveValues(paused = TRUE, muted = TRUE) ## starts paused and muted
        editing <- reactiveValues(active = NULL)

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
        team_select_mod <- callModule(mod_team_select, id = "team_selector", rdata = rdata, editing = editing, app_data = app_data)
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

        get_current_rally_code <- function() {
            tryCatch({
                ridx <- playslist_mod$current_row()
                ## if ridx is greater than the length of plays2 rows, then take it from rally_codes()
                if (!is.null(ridx) && !is.na(ridx)) {
                    if (ridx <= nrow(rdata$dvw$plays2)) rdata$dvw$plays2$rally_codes[[ridx]] else if ((ridx - nrow(rdata$dvw$plays2)) <= nrow(rally_codes())) rally_codes()[ridx - nrow(rdata$dvw$plays2), ] else NULL
                } else {
                    NULL
                }
            }, error = function(e) NULL)
        }

        do_edit_commit <- function() {
            if (!is.null(editing$active)) {
                if (editing$active %in% c("edit", "insert above", "insert below")) {
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
                    if (editing$active %eq% "edit") {
                        crc <- get_current_rally_code() ## this is from rally_codes BUT this won't have the actual scout code unless it was a non-skill code, hrumph
                        old_code <- if (is.null(crc$code) || is.na(crc$code)) rdata$dvw$plays$code[playslist_mod$current_row()] else crc$code
                        ridx <- playslist_mod$current_row()
                        if (!is.null(crc) && !is.null(ridx) && !is.na(ridx)) {
                            ## user has changed EITHER input$code_entry or used the code_entry_guide
                            changed1 <- (!newcode1 %eq% old_code) && nzchar(newcode1)
                            changed2 <- (!newcode2 %eq% old_code) && nzchar(newcode2)
                            if (!changed1 && changed2) {
                                newcode <- newcode2
                                ## if we entered via the text box, then run this through the code parser
                                newcode <- sub("~+$", "", ov_code_interpret(newcode))
                                ## TODO deal with a compound code that returns multiple codes here
                            } else if (!changed1 && !changed2) {
                                ## neither changed, nothing to do
                                newcode <- NULL
                            } else {
                                newcode <- newcode1
                            }
                            if (!is.null(newcode)) {
                                crc$code <- newcode
                                ## if ridx is greater than the length of plays2 rows, then put this in rally_codes()
                                if (ridx <= nrow(rdata$dvw$plays2)) {
                                    newrc <- make_plays2(crc, game_state = crc$game_state[[1]], rally_ended = FALSE, dvw = rdata$dvw)
                                    rdata$dvw$plays2[ridx, ] <- newrc
                                } else if ((ridx - nrow(rdata$dvw$plays2)) <= nrow(rally_codes())) {
                                    rc <- rally_codes()
                                    rc[ridx - nrow(rdata$dvw$plays2), ] <- crc
                                    rally_codes(rc)
                                    ## TODO check that this works, because the code in rally_codes should be used but just need to check
                                }
                            }
                        }
                    } else {
                        newcode <- if (nzchar(newcode1)) newcode1 else newcode2
                        if (grepl("^[a\\*]?[[:digit:]][[:digit:]][SREABDF]", newcode)) {
                            ## this is a skill code
                            ## we are ignoring these, at least for now
                            ##newcode <- sub("~+$", "", ov_code_interpret(input$code_entry))
                            warning("ignoring skill code: ", newcode)
                            newcode <- NULL
                        }
                        if (editing$active %eq% "insert below" && !is.null(newcode)) {
                            handle_manual_code(newcode)
                        } else if (editing$active %in% c("insert above")) {
                            ## not handled yet
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
                removeModal()
                ##deal_with_pause() ## no need to unpause after any of these actions?
            }
        }

        meta_is_valid <- reactiveVal(TRUE)
        observe({
            notnn <- function(z) !is.null(z) && !is.na(z)
            ## check teams
            teams_ok <- !is.null(rdata$dvw$meta$teams) && !any(tolower(rdata$dvw$meta$teams$team) %in% c("home team", "visiting team"))
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
                                      if (!isTRUE(rosters_ok)) tags$li(if (is.character(rosters_ok)) paste0(rosters_ok, " "), "Use the 'Edit teams' button to enter or adjust the team rosters."),
                                      if (!isTRUE(lineups_ok)) tags$li(if (is.character(lineups_ok)) paste0(lineups_ok, " "), paste0("Use the 'Edit lineups' to enter or adjust the starting lineups", if (!is.null(game_state$set_number) && !is.na(game_state$set_number)) paste0(" for set ", game_state$set_number), ".")),
                                      if (!video_media_ok) tags$li("Wait for the video media information to be loaded.")
                                  ),
                             tags$hr(),
                             tags$p("Scouting cannot start until this information has been entered.")
                             )
                } else {
                    if (rally_state() == "fix required information before scouting can begin") rally_state(if (video_state$paused) "click or unpause the video to start" else "click serve start")
                    NULL
                }
            })
        })

        ## exteral video control buttons
        observeEvent(input$video_pause, deal_with_pause())
        observeEvent(input$video_rew_10, do_video("rew", 10))
        observeEvent(input$video_rew_2, do_video("rew", 2))
        observeEvent(input$video_ff_2, do_video("ff", 2))
        observeEvent(input$video_ff_10, do_video("ff", 10))
        observeEvent(input$video_volume, if (!is.null(input$video_volume)) do_video("set_volume", input$video_volume))
        observeEvent(input$video_toggle_mute, do_video("toggle_mute"))

        deal_with_pause <- function(show_modal = TRUE) {
            ## don't allow unpause if we have a scouting modal shown
            if (isTRUE(scout_modal_active())) {
                ## but do allow pause, if somehow it isn't already
                do_video("pause")
            } else {##if (meta_is_valid()) {
                ## don't allow unpause if the lineups are not valid, else it'll crash
                if (video_state$paused) {
                    ## we are paused
                    if (is.null(editing$active)) {
                        ## just unpause
                        do_video("play")
                    } else if (editing$active %eq% "admin") {
                        ## otherwise, and only if we have the admin modal showing, dismiss it and unpause
                        dismiss_admin_modal()
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
                if (ky %in% app_data$shortcuts$hide_popup) {
                    ## temporarily hide the modal, so the video can be seen
                    ## but only for the admin, lineup modal or the ones that pop up during the rally, not the editing modals for teams or rosters
                    if (is.null(editing$active) || editing$active %in% c("admin", "change starting lineup")) hide_popup()
                } else if (ky %in% c(app_data$shortcuts$pause, app_data$shortcuts$pause_no_popup)) {
                    ## only accept this if we are not editing, or it's the admin modal being shown
                    if (is.null(editing$active) || editing$active %eq% "admin") {
                        ## video pause/unpause
                        ## Q (uppercase) does just pause, with no admin modal
                        deal_with_pause(show_modal = !ky %in% app_data$shortcuts$pause_no_popup)
                    }
                } else if (is.null(editing$active) && !courtref$active()) {
                    ## none of these should be allowed to happen if we are e.g. editing lineups or teams or doing the court ref
                    if (ky %in% app_data$shortcuts$go_to_time) {
                        ## video go to currently-selected event
                        ridx <- playslist_mod$current_row()
                        vt <- if (!is.na(ridx) && !is.na(ridx)) rdata$dvw$plays$video_time[ridx] else NA
                        if (!is.null(vt) && !is.na(vt)) {
                            if (debug > 1) cat("jumping to video time: ", vt, "\n")
                            do_video("set_time", rebase_time(vt, time_to = current_video_src()))
                        }
                    } else if (ky %in% app_data$shortcuts$undo) {
                        ## undo
                        do_undo()
                    } else if (ky %in% app_data$shortcuts$switch_video) {
                        ## switch video
                        do_switch_video()
                    } else if (ky %in% app_data$shortcuts$contact) {
                        ## player contact with the ball
                        do_contact()
                    } else if (ky %in% app_data$shortcuts$edit_code) {
                        ## edit_data_row()
                        ## not enabled yet
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
                                do_cancel_rew()
                            } else if (courtref$active()) {
                                ## do nothing
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
                            ## but not for team editing, because pressing enter in the DT fires this too
                            if (!is.null(editing$active) && !editing$active %eq% "teams") {
                                do_edit_commit()
                            } else if (isTRUE(scout_modal_active())) {
                                ## if we have a scouting modal showing, and a valid accept_fun entry, run that function
                                if (!is.null(accept_fun())) try(get(accept_fun(), mode = "function")())
                            }
                            ## need to stop this propagating to the browser, else it risks e.g. re-firing the most recently used button - done in UI code
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
                        if (ky %in% utf8ToInt(paste0(app_data$shortcuts$hide_popup, collapse = ""))) {
                            ## z
                            ## re-show the modal after temporarily hiding
                            unhide_popup()
                        }
                    }
                }
            }
        })

        hide_popup <- function() {
            dojs("$('#shiny-modal-wrapper').hide(); $('.modal-backdrop').hide();") ## popup
            if (review_pane_active()) js_hide2("review_pane") ## review pane
        }
        unhide_popup <- function() {
            dojs("$('#shiny-modal-wrapper').show(); $('.modal-backdrop').show();")
            if (review_pane_active()) js_show2("review_pane")
        }

        ## options
        observeEvent(input$preferences, {
            editing$active <- "preferences"
            showModal(vwModalDialog(title = "Preferences", footer = NULL, width = 100,
                                    tabsetPanel(id = "prefs_tabs",
                                                tabPanel("App preferences",
                                                         tags$hr(), tags$br(),
                                                         fluidRow(column(3, checkboxInput("prefs_show_courtref", "Show court reference?", value = prefs$show_courtref)),
                                                                  column(3, textInput("prefs_scout", label = "Default scout name:", placeholder = "Your name", value = prefs$scout_name)),
                                                                  column(3, checkboxInput("prefs_scoreboard", "Show scoreboard in the top-right of the video pane?", value = prefs$scoreboard))),
                                                         tags$br(),
                                                         fluidRow(column(3, checkboxInput("prefs_ball_path", "Show the ball path on the court inset diagram?", value = prefs$ball_path)),
                                                                  column(3, selectInput("prefs_playlist_display_option", "Plays table style", choices = c("Scouted codes" = "dv_codes", "Commentary style" = "commentary"), selected = prefs$playlist_display_option)),
                                                                  column(3, checkboxInput("prefs_review_pane", "Show review pane (video loop) in popups?", value = prefs$review_pane)))
                                                         ),
                                                tabPanel("Scouting options",
                                                         tags$hr(), tags$br(), tags$p("Warning: changing scouting options once a match is already partially-scouted could lead to inconsistent files."),
                                                         tags$hr(),
                                                         fluidRow(column(3, selectInput("scopts_end_convention", tags$span(title = HTML("Is the end coordinate of an attack or serve the actual end location (where the ball was played or contacted the floor), or the intended one. The actual might differ from the intended if there is a block touch or the ball hit the net. If 'intended', and a block touch is recorded, then the end location of the attack will not be used for the dig location (the dig location will be missing)."), "End convention:", icon("question-circle")), choices = c(Intended = "intended", Actual = "actual"), selected = rdata$options$end_convention)),
                                                                  column(3, checkboxInput("scopts_nblockers", tags$span(title = HTML("Record the number of blockers on each attack?"), "Record the number of blockers?", icon("question-circle")), value = rdata$options$nblockers)),
                                                                  column(3, selectInput("scopts_default_nblockers", tags$span(title = HTML("If we are scouting the number of blockers, what number should we default to?"), "Default number of blockers:", icon("question-circle")), choices = c("No default" = NA, "No block" = 0, "Single block" = 1, "Double block" = 2, "Triple block" = 3, "Hole block" = 4), selected = rdata$options$default_nblockers))),
                                                         tags$br(),
                                                         fluidRow(column(3, checkboxInput("scopts_transition_sets", tags$span(title = HTML("If transition sets are not recorded, then just the endpoint of each attack (i.e. the dig) and the subsequent counter-attack are scouted."), "Record sets in transition?", icon("question-circle")), value = rdata$options$transition_sets)),
                                                                  column(3, selectInput("scopts_attacks_by", tags$span(title = HTML("Classify attacks by 'Code' (X5, V5, etc) or just 'Tempo' (high, medium, quick)."), "Attacks by:", icon("question-circle")), choices = c(Codes = "codes", Tempo = "tempo"), selected = rdata$options$attacks_by))##,
                                                                  ##column(3, selectInput("scopts_team_system", tags$span(title = HTML("the assumed system that teams are using to assign e.g. passing and hitting responsibilities. 'SHM3' is a setter-hitter-middle rotation, with 3 passers (the libero and two outside hitters)."), "Team system:", icon("question-circle")), choices = c("SHM3" = "SHM3"), selected = rdata$options$team_system))
                                                                  ),
                                                         tags$br(),
                                                         fluidRow(column(3, textInput("scopts_setter_dump_code", tags$span(title = HTML("The attack combination code for a setter dump"), "Setter tip attack code:", icon("question-circle")), placeholder = "PP", value = rdata$options$setter_dump_code)), ## string: the attack combination code for a setter dump
                                                                  column(3, textInput("scopts_second_ball_attack_code", tags$span(title = HTML("The attack combination code for a second-ball attack"), "Second-ball attack code:", icon("question-circle")), placeholder = "P2", value = rdata$options$second_ball_attack_code)), ## string: the attack combination code for a second-ball attack
                                                                  column(3, textInput("scopts_overpass_attack_code", tags$span(title = HTML("The attack combination code for an attack on an overpass"), "Overpass attack code:", icon("question-circle")), placeholder = "PR", value = rdata$options$overpass_attack_code))) ## string: the attack combination code for an attack on an overpass
## TODO @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
## TODO @param compound_table tibble: the table of compound codes
                                                         )
                                                ),
                                    tags$br(),
                                    tags$hr(),
                                    fixedRow(column(2, actionButton("just_cancel", "Cancel", class = "cancel fatradio")),
                                             column(2, offset = 8, actionButton("prefs_save", "Apply and save", class = "continue fatradio")))
                                    ))
        })
        observeEvent(input$just_cancel, {
            editing$active <- NULL
            removeModal()
        })
        observeEvent(input$prefs_save, {
            thisprefs <- list(scout_name = if (is.null(input$prefs_scout) || is.na(input$prefs_scout)) "" else input$prefs_scout, show_courtref = isTRUE(input$prefs_show_courtref), scoreboard = isTRUE(input$prefs_scoreboard), ball_path = isTRUE(input$prefs_ball_path), playlist_display_option = input$prefs_playlist_display_option, review_pane = input$prefs_review_pane)
            this_opts <- list(end_convention = input$scopts_end_convention, nblockers = input$scopts_nblockers, default_nblockers = as.numeric(input$scopts_default_nblockers), transition_sets = input$scopts_transition_sets, attacks_by = input$scopts_attacks_by, team_system = input$scopts_team_system, setter_dump_code = if (nzchar(input$scopts_setter_dump_code)) input$scopts_setter_dump_code else ov_scouting_options()$setter_dump_code, second_ball_attack_code = if (nzchar(input$scopts_second_ball_attack_code)) input$scopts_second_ball_attack_code else ov_scouting_options()$second_ball_attack_code, overpass_attack_code = if (nzchar(input$scopts_overpass_attack_code)) input$scopts_overpass_attack_code else ov_scouting_options()$overpass_attack_code)

            ## save prefs
            tryCatch(saveRDS(thisprefs, app_data$options_file), error = function(e) warning("could not save preferences to file"))
            ## transfer to active prefs object
            for (nm in names(thisprefs)) prefs[[nm]] <- thisprefs[[nm]]
            ## apply any that require immediate action
            if (is.null(rdata$dvw$meta$more$scout) || is.na(rdata$dvw$meta$more$scout) || !nzchar(rdata$dvw$meta$more$scout)) rdata$dvw$meta$more$scout <- prefs$scout_name

            ## apply scouting opts
            for (nm in names(this_opts)) rdata$options[[nm]] <- this_opts[[nm]]

            editing$active <- NULL
            removeModal()
        })

        ## account for aspect ratios
        ## the video overlay image (which receives the mouse clicks) will fill the whole video div element
        ## but the actual video content can have a different aspect ratio, which means it will be letterboxed
        ## direction "to_image" means we've taken unit coords in the outer (video div) space and are converting to inner video content space
        ## direction "to_court" means we've taken unit coords in the inner video content space and are converting to outer (video div) space
        ar_fix_x <- function(x, direction = "to_image") {
            eAR <- input$dv_width / input$dv_height
            mAR <- if ((current_video_src() == 1L && is_youtube_url(app_data$video_src)) || (current_video_src() == 2L && is_youtube_url(app_data$video_src2))) {
                       16 / 9
                   } else {
                       input$video_width / input$video_height
                   }
            if (length(eAR) && length(mAR) && isTRUE(eAR > mAR)) {
                ## element is wider than the actual media, we have letterboxing on the sides
                visW <- mAR * input$dv_height
                lw <- (input$dv_width - visW) / input$dv_width / 2 ## letterboxing each side as proportion of element (visible) width
                if (direction == "to_image") lw + x * (1 - 2 * lw) else (x - lw) / (1 - 2 * lw) ## adjust x
            } else {
                x
            }
        }
        ar_fix_y <- function(y, direction = "to_image") {
            eAR <- input$dv_width / input$dv_height
            mAR <- if ((current_video_src() == 1L && is_youtube_url(app_data$video_src)) || (current_video_src() == 2L && is_youtube_url(app_data$video_src2))) {
                       16 / 9
                   } else {
                       input$video_width / input$video_height
                   }
            if (length(eAR) && length(mAR) && isTRUE(mAR > eAR)) {
                ## media is wider than the element, we have letterboxing on the top/bottom
                visH <- input$dv_width / mAR
                lh <- (input$dv_height - visH) / input$dv_height / 2 ## letterboxing top/bottom as proportion of element (visible) height
                if (direction == "to_image") lh + y * (1 - 2 * lh) else (y - lh) / (1 - 2 * lh) ## adjust y
            } else {
                y
            }
        }
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
                cc <- canvas_drawing$new(id = "video_overlay_canvas", width = w, height = h, on_fail = "Shiny.setInputValue('overlay_nocanvas', 1);")
                ## if context fails, fall back to base plotting
                if (isTRUE(prefs$show_courtref) && !is.null(overlay_court_lines())) {
                    oxy <- overlay_court_lines()
                    ## account for aspect ratios
                    oxy$image_x <- ar_fix_x(oxy$image_x)
                    oxy$xend <- ar_fix_x(oxy$xend)
                    oxy$image_y <- ar_fix_y(oxy$image_y)
                    oxy$yend <- ar_fix_y(oxy$yend)
                    cc$lines(x0 = oxy$image_x, y0 = oxy$image_y, x1 = oxy$xend, y1 = oxy$yend, col = app_data$styling$court_lines_colour, unit = "npc")
                }
                if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                    ixy <- setNames(crt_to_vid(overlay_points()), c("x", "y"))
                    if (any(overlay_points()$valid)) {
                        cc$circles(x = ixy$x[overlay_points()$valid], y = ixy$y[overlay_points()$valid], r = 0.01, col = "white", fill_col = "dodgerblue", unit = "npc")
                    }
                    if (!all(overlay_points()$valid)) {
                        cc$circles(x = ixy$x[!overlay_points()$valid], y = ixy$y[!overlay_points()$valid], r = 0.01, col = "white", fill_col = "firebrick", unit = "npc")
                    }
                }
                cc$draw()
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
                        oxy$image_x <- ar_fix_x(oxy$image_x)
                        oxy$xend <- ar_fix_x(oxy$xend)
                        oxy$image_y <- ar_fix_y(oxy$image_y)
                        oxy$yend <- ar_fix_y(oxy$yend)
                        segments(x0 = oxy$image_x, y0 = oxy$image_y, x1 = oxy$xend, y1 = oxy$yend, col = app_data$styling$court_lines_colour)
                    }
                    if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                        ixy <- setNames(crt_to_vid(overlay_points()), c("x", "y"))
                        points(ixy$x[overlay_points()$valid], ixy$y[overlay_points()$valid], bg = "dodgerblue", pch = 21, col = "white", cex = 2.5)
                        points(ixy$x[!overlay_points()$valid], ixy$y[!overlay_points()$valid], bg = "firebrick", pch = 21, col = "white", cex = 2.5)
                    }
                    par(opar)
                }, bg = "transparent", width = as.numeric(input$dv_width), height = as.numeric(input$dv_height))
            }
        })
        vid_to_crt <- function(obj, arfix = TRUE) {
            courtxy <- data.frame(x = rep(NA_real_, length(obj$x)), y = rep(NA_real_, length(obj$x)))
            if (!is.null(detection_ref()$court_ref) && length(obj$x) > 0) {
                thisx <- obj$x; thisy <- obj$y
                if (arfix) {
                    ## account for aspect ratios
                    thisx <- ar_fix_x(obj$x, direction = "to_court")
                    thisy <- ar_fix_y(obj$y, direction = "to_court")
                }
                courtxy <- ovideo::ov_transform_points(thisx, thisy, ref = detection_ref()$court_ref, direction = "to_court")
            }
            courtxy
        }
        crt_to_vid <- function(obj, arfix = TRUE) {
            imagexy <- data.frame(image_x = rep(NA_real_, length(obj$x)), image_y = rep(NA_real_, length(obj$x)))
            if (!is.null(detection_ref()$court_ref) && length(obj$x) > 0) {
                imagexy <- setNames(ovideo::ov_transform_points(obj$x, obj$y, ref = detection_ref()$court_ref, direction = "to_image"), c("image_x", "image_y"))
                if (arfix) {
                    ## account for aspect ratios
                    imagexy$image_x <- ar_fix_x(imagexy$image_x)
                    imagexy$image_y <- ar_fix_y(imagexy$image_y)
                }
            }
            imagexy
        }

        courtxy <- reactiveVal(list(x = NA_real_, y = NA_real_)) ## keeps track of click locations (in court x, y space)
        loop_trigger <- reactiveVal(0L)
        observeEvent(input$video_click, priority = 99, {
            if (debug) dojs("var thisct = new Date().getTime(); var thiscd = thisct - clktm; console.log('click processing time: ' + thiscd + ' (' + thisct + ')')")
            ## when video clicked, get the corresponding video time and trigger the loop
            flash_screen() ## visual indicator that click has registered
            ## calculate the normalized x,y coords
            this_click <- if (length(input$video_click) > 4) list(x = input$video_click[1] / input$video_click[3], y = 1 - input$video_click[2] / input$video_click[4])
            ## and video time
            time_uuid <- uuid()
            game_state$current_time_uuid <- time_uuid
            click_time <- as.numeric(input$video_click[5])
            if (!is.na(click_time) && click_time >= 0) {
                ## got the video time as part of the click packet, stash it
                this_timebase <- current_video_src()
                if (length(this_timebase) != 1 || !this_timebase %in% c(1, 2)) this_timebase <- 1
                video_times[[time_uuid]] <<- round(rebase_time(click_time, time_from = this_timebase), 2) ## video times to 2 dec places
            } else {
                ## invalid time received, ask again
                warning("invalid click time\n")
                do_video("get_time_fid", paste0(time_uuid, "@", current_video_src())) ## make asynchronous request, noting which video is currently being shown (@)
            }
            if (rally_state() != "click or unpause the video to start") courtxy(vid_to_crt(this_click))
            loop_trigger(loop_trigger() + 1L)
            process_action()
            ## TODO MAYBE also propagate the click to elements below the overlay?
        })
        observeEvent(court_inset$click(), {
            ## when video clicked, get the corresponding video time and trigger the loop
            flash_screen() ## visual indicator that click has registered
            time_uuid <- uuid()
            game_state$current_time_uuid <- time_uuid
            do_video("get_time_fid", paste0(time_uuid, "@", current_video_src())) ## make asynchronous request, noting which video is currently being shown (@1 or @2)
            courtxy(court_inset$click())
            loop_trigger(loop_trigger() + 1L)
            process_action()
        })
        do_contact <- function() {
            ## keyboard entry indicating a contact at this time
            ## ask the browser for the current video time
            dojs("Shiny.setInputValue('contact', [vidplayer.currentTime(), new Date().getTime()])")
        }
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
                click_time <- round(rebase_time(click_time, time_from = this_timebase), 2) ## video times to 2 dec places
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
            if (nzchar(this_uuid)) video_times[[this_uuid]] <<- round(rebase_time(as.numeric(sub("&.+", "", temp)), time_from = this_timebase), 2) ## video times to 2 dec places
        })
        retrieve_video_time <- function(id) {
            id <- sub("@.*", "", id)
            if (is_uuid(id)) {
                if (nzchar(id) && id %in% names(video_times)) video_times[[id]] else NA_real_
            } else {
                id
            }
        }

        ## rally_codes is a reactive that returns a tibble with columns team, pnum, skill, tempo, eval, combo, target, sz, ez, esz, x_type, num_p, special, custom, t, start_x, start_y, end_x, end_y
        ## rally_codes are the actions in the current rally

        ## modal things
        ## styling
        scout_modal_width <- 100 - app_data$styling$review_pane_width
        ## keep track of whether we have a modal up or not, so that pause behaviour can be modified
        scout_modal_active <- reactiveVal(FALSE)
        show_scout_modal <- function(mui, with_review_pane = TRUE) {
            scout_modal_active(TRUE)
            showModal(mui)
            if (with_review_pane && isTRUE(prefs$review_pane)) show_review_pane()
        }
        remove_scout_modal <- function() {
            scout_modal_active(FALSE)
            accept_fun(NULL)
            removeModal()
            if (isTRUE(prefs$review_pane)) hide_review_pane()
        }
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

        accept_fun <- reactiveVal(NULL) ## use this to determine what function should be run when the "Continue" button on a modal is clicked, or the enter key is used to shortcut it
        ## single click the video to register a tag location, or starting ball coordinates
        process_action <- function() {
            if (loop_trigger() > 0 && rally_state() != "fix required information before scouting can begin") {
                if (rally_state() == "click or unpause the video to start") {
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
                    ## time might not have resolved yet if it is coming from the asynchronous handler, so add it after next click
                    rally_codes(bind_rows(rally_codes(), code_trow(team = game_state$serving, pnum = sp, skill = "S", tempo = st, sz = sz, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                    game_state$current_team <- other(game_state$serving)
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
                    ## TODO possibly also guess foot fault, although that will be confusing because the (legal) serve contact might be inside the baseline
                    ## we pre-select either the passer, or the error type, depending on whether we thought it was an error or not
                    serve_outcome_initial_buttons <- make_fat_radio_buttons(choices = c("Serve error" = "=", "Reception error (serve ace)" = "S#", "Reception in play" = "R~"), input_var = "serve_initial_outcome", selected = if (!is.na(guess_was_err)) "=" else "R~")
                    serve_error_type_buttons <- make_fat_radio_buttons(choices = c("In net" = "=N", "Foot fault/referee call" = "=Z", "Out long" = "=O", "Out left" = "=L", "Out right" = "=R"), selected = if (!is.na(guess_was_err)) guess_was_err else NA, input_var = "serve_error_type", as_radio = "blankable")
                    passer_buttons <- make_fat_radio_buttons(choices = pass_pl_opts$choices, selected = pass_pl_opts$selected, input_var = "pass_player")
                    accept_fun("do_assign_serve_outcome")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL, width = scout_modal_width, modal_halign = "left",
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
                    if (isTRUE(input$shiftkey)) {
                        ## accept set by setter on court, with no popup
                        esz <- as.character(dv_xy2subzone(game_state$start_x, game_state$start_y))
                        passq <- guess_pass_quality(game_state, dvw = rdata$dvw)
                        rc <- rally_codes()
                        rc$eval[rc$skill %eq% "R"] <- passq
                        ## find corresponding serve evaluation code
                        seval <- rdata$options$compound_table$code[rdata$options$compound_table$skill %eq% "S" & rdata$options$compound_table$compound_skill %eq% "R" & rdata$options$compound_table$compound_code %eq% passq]
                        if (nchar(seval) != 1) seval <- "~"
                        rc$eval[rc$skill %eq% "S"] <- seval
                        start_t <- retrieve_video_time(game_state$start_t)
                        ## note that the position gets assigned to the start coordinates, but end zone/subzone
                        rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = guessed_setter, skill = "E", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, endxy_valid = game_state$startxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                        rally_state("click third contact")
                        do_video("play")
                    } else {
                        accept_fun("do_assign_c2")
                        show_scout_modal(vwModalDialog(title = "Details", footer = NULL, width = scout_modal_width, modal_halign = "left",
                                                       do.call(fixedRow, c(list(column(2, tags$strong("Reception quality"))), lapply(c2_pq_buttons, function(but) column(1, but)))),
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
                    ## figure current phase
                    if (nrow(rally_codes()) > 0) {
                        temp <- make_plays2(rally_codes(), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)
                        ph <- tail(temp$phase, 1)
                    } else {
                        ph <- NA_character_
                    }
                    if (rdata$options$attacks_by %eq% "codes") {
                        ac <- guess_attack_code(game_state, dvw = rdata$dvw, opts = rdata$options)
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
                    } else {
                        ac <- c("High ball" = "H", "Medium/fast<br />attack" = "M", "Quick attack" = "Q")##, "Other attack" = "O")
                        if (!isTRUE(rdata$options$transition_sets) && ph %eq% "Transition") {
                            ## if we aren't scouting transition sets, then this "third" contact could be a setter dump or second-ball attack
                            ac <- c(ac, c("Setter dump" = rdata$options$setter_dump_code, "Second-ball<br />attack" = rdata$options$second_ball_attack_code))
                        }
                    }
                    n_ac <- length(ac)
                    ## always offer set error option
                    n_ac2 <- 2L
                    ac <- c(ac, "Freeball over" = "F", "Set error" = "E=")
                    c3_buttons <- make_fat_radio_buttons(choices = c(ac, c("Opp. dig" = "aF", "Opp. dig error" = "aF=", "Opp. overpass attack" = "aPR")), input_var = "c3")
                    fatradio_class_uuids$c3 <- attr(c3_buttons, "class")
                    hit_type_buttons <- make_fat_radio_buttons(choices = if (app_data$is_beach) c(Power = "H", Poke = "T", Shot = "P") else c(Hit = "H", Tip = "T", "Soft/Roll" = "P"), input_var = "hit_type")
                    fatradio_class_uuids$hit_type <- attr(hit_type_buttons, "class")
                    attack_pl_opts <- guess_attack_player_options(game_state, dvw = rdata$dvw, system = rdata$options$team_system)
                    ap <- sort(attack_pl_opts$choices)
                    names(ap) <- player_nums_to(ap, team = game_state$current_team, dvw = rdata$dvw)
                    ap <- c(ap, Unknown = "Unknown")
                    ## since we have a freeball over option here, it could be done by a libero
                    libs <- sort(get_liberos(game_state, team = game_state$current_team, dvw = rdata$dvw))
                    ap <- c(ap, setNames(libs, player_nums_to(libs, team = game_state$current_team, dvw = rdata$dvw)))
                    attacker_buttons <- make_fat_radio_buttons(choices = ap, selected = attack_pl_opts$selected, input_var = "c3_player")
                    ## do we want to support "hole" block?
                    if (isTRUE(rdata$options$nblockers)) nblocker_buttons <- make_fat_radio_buttons(choices = c("No block" = 0, "Single block" = 1, "Double block" = 2, "Triple block" = 3, "Hole block" = 4), selected = if (!is.null(rdata$options$default_nblockers) && !is.na(rdata$options$default_nblockers)) rdata$options$default_nblockers, input_var = "nblockers")
                    ## attack error, blocked, replay will be scouted on next entry
                    ## TODO other special codes ?
                    opp <- c(sort(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw)), sort(get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw)))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_player_buttons <- make_fat_radio_buttons(choices = opp, selected = NA, input_var = "c3_opp_player")
                    accept_fun("do_assign_c3")
                    show_scout_modal(vwModalDialog(title = "Details: attack or freeball over", footer = NULL, width = scout_modal_width, modal_halign = "left",
                                            do.call(fixedRow, c(lapply(c3_buttons[seq_len(n_ac)], function(but) column(1, but)),
                                                                if (rdata$options$attacks_by %eq% "codes") list(column(1, tags$div(id = "c3_other_outer", selectInput("c3_other_attack", label = NULL, choices = ac_others, selected = "Choose other", width = "100%")))))),
                                            tags$br(),
                                            ## hit type and then the freeball over and set error buttons, shift them to the right
                                            fixedRow(column(1, hit_type_buttons[1]), column(1, hit_type_buttons[2]), column(1, hit_type_buttons[3]),
                                                     column(2, offset = 2, c3_buttons[n_ac + 1L]), column(2, c3_buttons[n_ac + 2L])),
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
                    c1_buttons <- make_fat_radio_buttons(choices = c("Attack kill (without dig error)" = "A#", "Attack error" = "A=", "Blocked for reattack (play continues)" = "A!", "Dig" = "D", "Dig error (attack kill)" = "D=", "Block kill" = "B#", "Block fault" = "B/"), selected = "D", input_var = "c1") ## defaults to dig
                    ## allow the possibility to change the hit type
                    htype <- check_hit_type(input$hit_type) ## the currently-selected hit type, default to "H" if not assigned
                    hit_type_buttons <- make_fat_radio_buttons(choices = if (app_data$is_beach) c(Power = "H", Poke = "T", Shot = "P") else c(Hit = "H", Tip = "T", "Soft/Roll" = "P"), selected = htype, input_var = "hit_type")
                    fatradio_class_uuids$hit_type <- attr(hit_type_buttons, "class")
                    ae_buttons <- make_fat_radio_buttons(choices = c("Out long" = "O", "Out side" = "S", "In net" = "N", "Net contact" = "I", Antenna = "A", "Other/referee call" = "Z"), selected = NA, input_var = "attack_error_type")
                    ## blocking players
                    blockp <- get_players(game_state, team = game_state$current_team, dvw = rdata$dvw)
                    if (length(blockp) == 6) blockp <- blockp[2:4] ## front-row only
                    blockp <- sort(blockp)
                    names(blockp) <- player_nums_to(blockp, team = game_state$current_team, dvw = rdata$dvw)
                    blockp <- c(blockp, Unknown = "Unknown")
                    block_player_buttons <- make_fat_radio_buttons(choices = blockp, selected = NA, input_var = "c1_block_touch_player")
                    ## also blocking players, to show for block fault/kill in different part of form
                    ## define this here because selected is NA, whereas we make a default selection for c1_def_player below
                    block1_player_buttons <- make_fat_radio_buttons(choices = blockp, selected = NA, input_var = "c1_def_player")
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
                    if (isTRUE(input$shiftkey)) {
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
                        end_t <- retrieve_video_time(game_state$end_t)
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
                            rc <- bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "D", eval = eval, tempo = tempo, sz = sz, ez = esz[1], esz = esz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, start_zone_valid = szv, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table))
                        }
                        rally_codes(rc)
                        rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
                        do_video("play")
                    }  else {
                        accept_fun("do_assign_c1")
                        show_scout_modal(vwModalDialog(title = "Details", footer = NULL, width = scout_modal_width, modal_halign = "left",
                                                       tags$p(tags$strong("Attack outcome and hit type:")),
                                                       fixedRow(column(2, c1_buttons[1]), column(2, c1_buttons[2]), column(3, c1_buttons[3]),
                                                                column(1, hit_type_buttons[1]), column(1, hit_type_buttons[2]), column(1, hit_type_buttons[3])),
                                                       tags$br(), tags$div(id = "ae_ui", style = "display:none;", do.call(fixedRow, lapply(ae_buttons, function(but) column(1, but)))),
                                                       tags$div("OR", tags$strong("Defence outcome:")),
                                                       do.call(fixedRow, lapply(c1_buttons[4:7], function(but) column(2, but))),
                                                       tags$br(), tags$hr(),
                                                       ## either dig players (defending team)
                                                       tags$div(id = "c1_digp_ui", tags$p(tags$strong("Dig player")),
                                                                do.call(fixedRow, lapply(dig_player_buttons, function(but) column(1, but)))),
                                                       ## or cover players (attacking team)
                                                       tags$div(id = "c1_coverp_ui", style = "display:none;", tags$p(tags$strong("Cover dig player")),
                                                                do.call(fixedRow, lapply(cover_player_buttons, function(but) column(1, but)))),
                                                       ## or block players
                                                       tags$div(id = "c1_blockp_ui", style = "display:none;", tags$p(tags$strong("Block player")),
                                                                do.call(fixedRow, lapply(block1_player_buttons, function(but) column(2, but)))),
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
                    f1_buttons <- make_fat_radio_buttons(choices = c("Freeball over error" = "F=", "Freeball dig" = "FD", "Freeball dig error" = "FD=", "Opp. overpass attack" = "aPR"), selected = "FD", input_var = "f1")
                    ## Identify defending players
                    dig_pl_opts <- guess_freeball_dig_player_options(game_state, dvw = rdata$dvw, system = rdata$options$team_system)
                    digp <- dig_pl_opts$choices
                    names(digp) <- player_nums_to(digp, team = game_state$current_team, dvw = rdata$dvw)
                    digp <- c(digp, Unknown = "Unknown")
                    dig_player_buttons <- make_fat_radio_buttons(choices = digp, selected = dig_pl_opts$selected, input_var = "f1_def_player")
                    accept_fun("do_assign_f1")
                    show_scout_modal(vwModalDialog(title = "Details", footer = NULL, width = scout_modal_width, modal_halign = "left",
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
                    vwModalDialog(title = "End of set", footer = NULL, width = scout_modal_width, modal_halign = "left",
                                  paste0("Confirm end of set ", game_state$set_number, "?"),
                                  tags$hr(),
                                  fixedRow(column(2, actionButton("end_of_set_cancel", "Cancel", class = "cancel fatradio")),
                                           column(2, offset = 8, actionButton("end_of_set_confirm", "Confirm", class = "continue fatradio")))
                                  ), with_review_pane = FALSE)
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
            ## c1 can be A#, A=, A!, D, D=, B#, B/
            ## for A!, show cover players and block touch
            ## D, D=, show def players and block touch
            ## A# show block touch
            ## A= hide all player selectors
            ## B#, B/ show block1
            ## cover player buttons (c1_coverp_ui), block player buttons (c1_blockp_ui), and dig player buttons (c1_digp_ui) are alternatives, only one of these should be shown at any one time
            if (!is.null(input$c1)) {
                if (input$c1 %eq% "A!") {
                    js_show2("c1_coverp_ui")
                    js_hide2("c1_blockp_ui")
                    js_hide2("c1_digp_ui")
                    js_show2("c1_touchp_ui")
                } else if (input$c1 %in% c("D", "D=")) {
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_blockp_ui")
                    js_show2("c1_digp_ui")
                    js_show2("c1_touchp_ui")
                } else if (input$c1 %in% c("B#", "B/")) {
                    js_show2("c1_blockp_ui")
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                    js_hide2("c1_touchp_ui")
                } else if (input$c1 %in% c("A#")) {
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                    js_hide2("c1_blockp_ui")
                    js_show2("c1_touchp_ui")
                } else {
                    ## A=
                    js_hide2("c1_coverp_ui")
                    js_hide2("c1_digp_ui")
                    js_hide2("c1_blockp_ui")
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
            if (!is.null(app_data$auto_save_dir)) {
                if (!dir.exists(app_data$auto_save_dir) && !have_warned_auto_save) {
                    have_warned_auto_save <<- TRUE
                    warning("auto-save dir does not exist, ignoring")
                } else {
                    tryCatch({
                        temp_dvw_file <- file.path(app_data$auto_save_dir, paste0(save_file_basename(), "-live.dvw"))
                        if (file.exists(temp_dvw_file)) unlink(temp_dvw_file)
                        dv_write2(update_meta(rp2(rdata$dvw)), file = temp_dvw_file)
                    }, error = function(e) warning("could not auto-save file"))
                }
            }
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
            game_state$home_team_end <- other_end(game_state$home_team_end) ## TODO deal with 5th set
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
                start_t <- retrieve_video_time(game_state$start_t)
                end_t <- retrieve_video_time(game_state$end_t)
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
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, eval = "=", sz = sz, ez = esz[1], esz = esz[2], special = if (nzchar(special_code)) special_code else "~", t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, game_state = game_state)
                    }
                    rally_codes(rc)
                    game_state$point_won_by <- other(game_state$serving)
                    rally_ended()
                } else if (input$serve_initial_outcome %eq% "S#") {
                    ## serve ace
                    pp <- if (!is.null(input$pass_player)) input$pass_player else 0L
                    remove_scout_modal()
                    if (length(Sidx) == 1) {
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, eval = "#", sz = sz, ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, game_state = game_state)
                    }
                    rally_codes(bind_rows(rc, code_trow(team = other(game_state$serving), pnum = pp, skill = "R", eval = "=", tempo = st, sz = sz, ez = esz[1], esz = esz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))

                    game_state$current_team <- game_state$serving
                    game_state$point_won_by <- game_state$serving
                    rally_ended()
                } else {
                    ## reception in play
                    pp <- if (!is.null(input$pass_player)) input$pass_player else 0L
                    remove_scout_modal()
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- update_code_trow(rc[Sidx, ], pnum = zpn(sp), tempo = st, ez = esz[1], esz = esz[2], t = start_t, end_x = game_state$end_x, end_y = game_state$end_y, game_state = game_state)
                    }
                    rally_codes(bind_rows(rc, code_trow(team = other(game_state$serving), pnum = pp, skill = "R", tempo = st, sz = sz, ez = esz[1], esz = esz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                    rally_state("click second contact")
                    do_video("rew", app_data$play_overlap)
                }
                if (rally_state() != "confirm end of set") do_video("play")
            }
        }

        observeEvent(input$assign_c1, do_assign_c1())
        do_assign_c1 <- function() {
            ## possible values for input$c1 are currently: A#, A=, A!, D, D=, B/, B#
            ## A#, A=, D, D= can be preceded by a block touch (but not B#, B/ and A!). A= is unlikely but theoretically possible
            mid_xy <- c(NA_real_, NA_real_)
            game_state$midxy_valid <- FALSE
            if (!is.null(input$c1_block_touch_player)) {
                if (input$c1 %in% c("A#", "A=", "D", "D=")) {
                    beval <- if (input$c1 %eq% "A#") "=" else default_skill_eval("B")
                    rc <- rally_codes()
                    Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                    mid_xy <- infer_mid_coords(game_state = game_state) ## will be NAs if start or end are invalid
                    game_state$midxy_valid <- !any(is.na(mid_xy))
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = input$c1_block_touch_player, skill = "B", eval = beval, tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table))) ## TODO x,y?
                }
            }
            esz <- as.character(dv_xy2subzone(game_state$end_x, game_state$end_y))
            rc <- rally_codes()
            if (input$c1 %in% c("A#", "A=")) {
                ##end_t <- retrieve_video_time(game_state$end_t)
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
                end_t <- retrieve_video_time(game_state$end_t)
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else NA_integer_
                if (!is.na(Aidx)) rc$x_type[Aidx] <- check_hit_type(input$hit_type) ## update hit type if needed
                ## TODO if we already have a block skill here, don't add a new one, just update the existing one ... though there should never already be block skill here
                ## block fault player should be in input$c1_def_player, but we'll take input$b1_block_touch_player otherwise
                bp <- if (!is.na(input$c1_def_player)) input$c1_def_player else if (!is.na(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
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
                }
                ## "current" team here is the digging/blocking team
                rally_codes(bind_rows(rc,
                                      ## the block
                                      code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "!", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table),
                                      ## and the dig cover
                                      if (!input$c1_cover_player %eq% "No cover dig") code_trow(team = other(game_state$current_team), pnum = if (!is.na(input$c1_cover_player)) input$c1_cover_player else 0L, skill = "D", eval = default_skill_eval("D"), sz = esz[1], t = end_t, start_x = game_state$end_x, start_y = game_state$end_y, rally_state = rally_state(), game_state = game_state, startxy_valid = game_state$endxy_valid, default_scouting_table = rdata$options$default_scouting_table)
                                      ))
                game_state$current_team <- other(game_state$current_team) ## attacking team now playing
                rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
            } else if (input$c1 %eq% "B/") {
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc)  else NA_integer_
                ## block fault player should be in input$c1_def_player, but we'll take input$b1_block_touch_player otherwise
                bp <- if (!is.na(input$c1_def_player)) input$c1_def_player else if (!is.na(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
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
                }
                ## TODO if we already have a block skill here, don't add a new one, just update the existing one ... though there should never already be block skill here
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "/", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table))) ## TODO x,y?
                game_state$point_won_by <- other(game_state$current_team) ## "current" team here is the digging team
                rally_ended()
            } else if (input$c1 %eq% "B#") {
                if (rc$skill[nrow(rc)] == "B") {
                    ## already a block skill (though this should not ever happen!). Delete it, to be replaced
                    rc <- head(rc, -1)
                }
                Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else NA_integer_
                ## block player should be in input$c1_def_player, but we'll take input$b1_block_touch_player otherwise
                bp <- if (!is.na(input$c1_def_player)) input$c1_def_player else if (!is.na(input$c1_block_touch_player)) input$c1_block_touch_player else 0L
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
                }
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = bp, skill = "B", eval = "#", tempo = if (!is.na(Aidx)) rc$tempo[Aidx] else "~", t = if (!is.na(Aidx)) rc$t[Aidx] else NA_real_, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table))) ## TODO x,y?
                game_state$point_won_by <- game_state$current_team ## "current" team here is the digging/blocking team
                rally_ended()
            } else {
                ## D or D=
                digp <- if (!is.null(input$c1_def_player)) input$c1_def_player else 0L
                end_t <- retrieve_video_time(game_state$end_t)
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
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "D", eval = eval, tempo = tempo, sz = sz, ez = dsz[1], esz = dsz[2], t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = dx, end_y = dy, start_zone_valid = szv, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
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
                end_t <- retrieve_video_time(game_state$end_t)
                if (!is.na(Fidx)) {
                    rc$eval[Fidx] <- if (input$f1 %eq% "FD=") "+" else "-"
                }
                if (was_f) {
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "F", eval = if (input$f1 %eq% "FD=") "=" else default_skill_eval("F"), sz = esz[1], t = end_t, start_x = game_state$end_x, start_y = game_state$end_y, rally_state = rally_state(), game_state = game_state, startxy_valid = game_state$endxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                    if (input$f1 == "FD=") {
                        game_state$point_won_by <- other(game_state$current_team)
                        rally_ended()
                    } else {
                        ## FD
                        rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
                    }
                } else {
                    ## aPR
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "A", tempo = "O", combo = rdata$options$overpass_attack_code, sz = esz[1], t = end_t, start_x = game_state$end_x, start_y = game_state$end_y, rally_state = rally_state(), game_state = game_state, startxy_valid = game_state$endxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
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
            passq <- if (!is.null(input$c2_pq)) input$c2_pq else guess_pass_quality(game_state, dvw = rdata$dvw)
            rc <- rally_codes()
            rc$eval[rc$skill %eq% "R"] <- passq
            ## find corresponding serve evaluation code
            seval <- rdata$options$compound_table$code[rdata$options$compound_table$skill %eq% "S" & rdata$options$compound_table$compound_skill %eq% "R" & rdata$options$compound_table$compound_code %eq% passq]
            if (nchar(seval) != 1) seval <- "~"
            rc$eval[rc$skill %eq% "S"] <- seval
            start_t <- retrieve_video_time(game_state$start_t)
            ## possible values for input$c2 are: Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball attack" = "P2", "Freeball over" = "F", R= rec error
            ##                                   "Opp. dig" = "aF", error "aF=", "Opp. overpass attack" = "aPR"
            if (input$c2 %in% c("E", "E=", "PP", "P2", "F", "R=")) {
                sp <- input$c2_player
                if (input$c2 == "E") {
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "E", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, endxy_valid = game_state$startxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
                    rally_state("click third contact")
                } else if (input$c2 == "E=") {
                    ## set error
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "E", eval = "=", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, endxy_valid = game_state$startxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
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
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "A", tempo = "O", combo = cmb, sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                    rally_state("click attack end point")
                    game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                } else if (input$c2 == "F") {
                    ## freeball over
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = sp, skill = "F", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
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
                op <- if (!is.null(input$c2_opp_player)) input$c2_opp_player else 0L
                ## esz here actually came from start_x and start_y above
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "A", tempo = "O", combo = rdata$options$overpass_attack_code, sz = esz[1], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
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
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "F", eval = if (input$c2 %eq% "aF=") "=" else "~", sz = esz[1], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
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
            ## possible values for input$c3 are: an attack code, Other attack, "F" Freeball or "E=" set error (if not scouting transition sets)
            ##    "Opp. dig" = "aF", "Opp. overpass attack" = "aPR"
            start_t <- retrieve_video_time(game_state$start_t)
            sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
            rc <- rally_codes()
            if (input$c3 %in% c("aPR", "aF", "aF=")) {
                ## adjust the prior skill, if it was a dig or reception then evaluation is "/", otherwise "-"
                ## but we can only do this if we are scouting transition sets, otherwise we can't be sure if it was e.g. D/ or a set over (E-)
                if (isTRUE(rdata$options$transition_sets) && tail(rc$skill, 1) %in% c("R", "D", "E", "F") && tail(rc$team, 1) %eq% game_state$current_team) {
                    new_eval <- if (tail(rc$skill, 1) %in% c("R", "D")) "/" else "-"
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = new_eval, game_state = game_state)
                }
            }
            if (input$c3 %eq% "aPR") {
                ## opposition overpass attack
                op <- if (!is.null(input$c3_opp_player)) input$c3_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "A", tempo = "O", combo = rdata$options$overpass_attack_code, sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                rally_state("click attack end point")
            } else if (input$c3 %in% c("aF", "aF=")) {
                ## opposition dig on overpass
                op <- if (!is.null(input$c3_opp_player)) input$c3_opp_player else 0L
                rally_codes(bind_rows(rc, code_trow(team = other(game_state$current_team), pnum = op, skill = "F", eval = if (input$c3 %eq% "aF=") "=" else "~", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                if (input$c3 %eq% "aF=") {
                    game_state$point_won_by <- game_state$current_team
                    rally_ended()
                } else {
                    game_state$current_team <- other(game_state$current_team)
                    rally_state(if (isTRUE(rdata$options$transition_sets)) "click second contact" else "click third contact")
                }
            } else if (input$c3 == "F") {
                ## freeball over
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = if (!is.null(input$c3_player)) input$c3_player else 0L, skill = "F", sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, default_scouting_table = rdata$options$default_scouting_table)))
                ## TODO add end pos to this on next contact
                game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                rally_state("click freeball end point")
            } else if (input$c3 == "E=") {
                ## set error
                ## this is the third contact, so we should modify the existing set if we have scouted a set
                if (tail(rc$skill, 1) %eq% "E") {
                    rc[nrow(rc), ] <- update_code_trow(rc[nrow(rc), ], eval = "=", game_state = game_state)
                    rally_codes(rc)
                } else {
                    esz <- as.character(dv_xy2subzone(game_state$start_x, game_state$start_y))
                    rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = if (!is.null(input$c3_player)) input$c3_player else 0L, skill = "E", eval = "=", ez = esz[1], esz = esz[2], t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, endxy_valid = game_state$startxy_valid, default_scouting_table = rdata$options$default_scouting_table)))
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
                    if (nchar(ac) == 2) {
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
                rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = ap, skill = "A", tempo = tempo, combo = ac, sz = sz, x_type = if (!is.null(input$hit_type) && input$hit_type %in% c("H", "P", "T")) input$hit_type else "~", num_p = nb, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y, rally_state = rally_state(), game_state = game_state, start_zone_valid = szvalid, default_scouting_table = rdata$options$default_scouting_table)))
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
            tags$div(id = "rallystate", tags$strong("Rally state: "), rally_state())
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

        show_admin_modal <- function() {
            ## can home team make a sub?
            ht_on <- sort(get_players(game_state, team = "*", dvw = rdata$dvw))
            ht_other <- setdiff(na.omit(rdata$dvw$meta$players_h$number), ht_on)
            ht_other <- setdiff(ht_other, get_liberos(game_state, team = "*", dvw = rdata$dvw))
            ht_can_sub <- length(ht_other) > 0
            ## and visiting team?
            vt_on <- sort(get_players(game_state, team = "a", dvw = rdata$dvw))
            vt_other <- setdiff(na.omit(rdata$dvw$meta$players_v$number), vt_on)
            vt_other <- setdiff(vt_other, get_liberos(game_state, team = "a", dvw = rdata$dvw))
            vt_can_sub <- length(vt_other) > 0

            showModal(vwModalDialog(title = "Miscellaneous", footer = NULL, width = 100,
                                    tags$p(tags$strong("Match actions")),
                                    fluidRow(column(2, actionButton("undo", "Undo last rally action", class = "undo fatradio")),
                                             column(2, actionButton("enter_code", "Enter scout code", class = "fatradio"), tags$span(style = "font-size:small;", "Only non-skill codes are supported")),
                                             column(2, actionButton("end_of_set_confirm", "End of set", class = "fatradio"))),
                                    tags$br(), tags$br(),
                                    ## TODO consider if all of these buttons should be available mid-rally or not (e.g. timeouts)
                                    fluidRow(column(6, tags$strong(datavolley::home_team(rdata$dvw), "(home)")),
                                             column(6, tags$strong(datavolley::visiting_team(rdata$dvw), "(visiting)"))),
                                    fluidRow(column(2, make_fat_buttons(choices = c("Won current rally" = "*p"), input_var = "manual_code")),
                                             column(2, make_fat_buttons(choices = c(Timeout = "*T"), input_var = "manual_code")),
                                             column(2, if (ht_can_sub) make_fat_buttons(choices = c(Substitution = "*c"), input_var = "substitution")),
                                             column(2, make_fat_buttons(choices = c("Won current rally" = "ap"), input_var = "manual_code")),
                                             column(2, make_fat_buttons(choices = c(Timeout = "aT"), input_var = "manual_code")),
                                             column(2, if (vt_can_sub) make_fat_buttons(choices = c(Substitution = "ac"), input_var = "substitution"))),
                                    tags$br(),
                                    fluidRow(column(2, make_fat_buttons(choices = c("Change setter" = "*P"), input_var = "change_setter")),
                                             column(2, offset = 4, make_fat_buttons(choices = c("Change setter" = "aP"), input_var = "change_setter"))),
                                    tags$br(),
                                    tags$hr(),
                                    fixedRow(column(2, offset = 10, actionButton("admin_dismiss", "Return to scouting", class = "continue fatradio")))
                                    ))
        }
        dismiss_admin_modal <- function() {
            ## dismiss the admin modal and unpause the video
            editing$active <- NULL
            removeModal()
            do_video("play")
        }
        observeEvent(input$admin_dismiss, dismiss_admin_modal())

        observeEvent(input$change_setter, {
            if (!is.null(input$change_setter)) {
                ht <- vt <- FALSE
                if (input$change_setter %eq% "*P") {
                    ## home players on court
                    ht_on <- get_players(game_state, team = "*", dvw = rdata$dvw)
                    ord <- order(ht_on)
                    chc <- setNames(paste0(ht_on, "@", seq_along(ht_on)), player_nums_to(ht_on, team = "*", dvw = rdata$dvw))[ord]
                    ht <- TRUE
                    buts <- make_fat_radio_buttons(choices = chc, selected = NA, input_var = "new_setter")
                } else {
                    ## visiting players on court
                    vt_on <- get_players(game_state, team = "a", dvw = rdata$dvw)
                    ord <- order(vt_on)
                    chc <- setNames(paste0(vt_on, "@", seq_along(vt_on)), player_nums_to(vt_on, team = "a", dvw = rdata$dvw))[ord]
                    vt <- TRUE
                    buts <- make_fat_radio_buttons(choices = chc, selected = NA, input_var = "new_setter")
                }
                showModal(vwModalDialog(title = paste0("On-court setter: ", if (ht) paste0(datavolley::home_team(rdata$dvw), " (home)") else paste0(datavolley::visiting_team(rdata$dvw), " (visiting)")), footer = NULL, width = 100,
                                        tags$div(tags$p(tags$strong("New setter")), do.call(fixedRow, lapply(buts, function(but) column(2, but)))),
                                        tags$hr(),
                                        fixedRow(column(2, offset = 8, make_fat_buttons(choices = c("Assign setter" = if (ht) "*P" else "aP"), input_var = "manual_code", class = "continue")),
                                                 column(2, actionButton("admin_dismiss", "Cancel", class = "cancel fatradio")))
                                        ))
            }
        })

        observeEvent(input$substitution, {
            if (!is.null(input$substitution)) {
                ht_sub <- vt_sub <- FALSE
                if (input$substitution %eq% "*c") {
                    ## home player sub buttons
                    ht_on <- sort(get_players(game_state, team = "*", dvw = rdata$dvw))
                    names(ht_on) <- player_nums_to(ht_on, team = "*", dvw = rdata$dvw)
                    ht_other <- setdiff(na.omit(rdata$dvw$meta$players_h$number), ht_on)
                    ht_other <- setdiff(ht_other, get_liberos(game_state, team = "*", dvw = rdata$dvw))
                    names(ht_other) <- player_nums_to(ht_other, team = "*", dvw = rdata$dvw)
                    ht_sub_out <- make_fat_radio_buttons(choices = ht_on, selected = NA, input_var = "ht_sub_out")
                    ht_sub_in <- make_fat_radio_buttons(choices = ht_other, selected = NA, input_var = "ht_sub_in")
                    ht_sub <- TRUE
                } else {
                    ## visiting player sub buttons
                    vt_on <- sort(get_players(game_state, team = "a", dvw = rdata$dvw))
                    names(vt_on) <- player_nums_to(vt_on, team = "a", dvw = rdata$dvw)
                    vt_other <- setdiff(na.omit(rdata$dvw$meta$players_v$number), vt_on)
                    vt_other <- setdiff(vt_other, get_liberos(game_state, team = "a", dvw = rdata$dvw))
                    names(vt_other) <- player_nums_to(vt_other, team = "a", dvw = rdata$dvw)
                    vt_sub_out <- make_fat_radio_buttons(choices = vt_on, selected = NA, input_var = "vt_sub_out")
                    vt_sub_in <- make_fat_radio_buttons(choices = vt_other, selected = NA, input_var = "vt_sub_in")
                    vt_sub <- TRUE
                }
                showModal(vwModalDialog(title = paste0("Substitution: ", if (ht_sub) paste0(datavolley::home_team(rdata$dvw), " (home)") else paste0(datavolley::visiting_team(rdata$dvw), " (visiting)")), footer = NULL, width = 100,
                                        if (ht_sub) tags$div(tags$p(tags$strong("Player out")),
                                                             do.call(fixedRow, lapply(ht_sub_out, function(but) column(2, but))),
                                                             tags$p(tags$strong("Player in")),
                                                             do.call(fixedRow, lapply(ht_sub_in, function(but) column(if (length(ht_other) <= 6) 2 else 1, but)))),
                                        if (vt_sub) tags$div(tags$p(tags$strong("Player out")),
                                                             do.call(fixedRow, lapply(vt_sub_out, function(but) column(2, but))),
                                                             tags$p(tags$strong("Player in")),
                                                             do.call(fixedRow, lapply(vt_sub_in, function(but) column(if (length(vt_other) <= 6) 2 else 1, but)))),
                                        tags$hr(),
                                        fixedRow(column(2, offset = 8, make_fat_buttons(choices = c("Make substitution" = if (ht_sub) "*c" else "ac"), input_var = "manual_code", class = "continue")),
                                                 column(2, actionButton("admin_dismiss", "Cancel", class = "cancel fatradio")))
                                        ))
            }
        })

        review_pane_active <- reactiveVal(FALSE)
        show_review_pane <- function() {
            ## use the current video time from the main video
            ## construct the playlist js by hand, because we need to inject the current video time
            revsrc <- get_src_type(if (current_video_src() == 1L) app_data$video_src else app_data$video_src2)
            pbrate <- if (!is.null(input$playback_rate) && input$playback_rate > 0) input$playback_rate * 1.4 else 1.4
            dojs(paste0("var start_t=vidplayer.currentTime()-2; revpl.set_playlist_and_play([{'video_src':'", revsrc$src, "','start_time':start_t,'duration':4,'type':'", revsrc$type, "'}], 'review_player', '", revsrc$type, "', true); revpl.set_playback_rate(", pbrate, ");"))
            js_show2("review_pane")
            dojs("Shiny.setInputValue('rv_height', $('#review_player').innerHeight()); Shiny.setInputValue('rv_width', $('#review_player').innerWidth());")
            review_pane_active(TRUE)
        }
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
                        ixy <- setNames(crt_to_vid(overlay_points(), arfix = FALSE), c("x", "y"))
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
                        ixy <- setNames(crt_to_vid(overlay_points(), arfix = FALSE), c("x", "y"))
                        ## points as blue, invalid points as red
                        points(ixy$x[overlay_points()$valid], ixy$y[overlay_points()$valid], bg = "dodgerblue", pch = 21, col = "white", cex = 2.5)
                        points(ixy$x[!overlay_points()$valid], ixy$y[!overlay_points()$valid], bg = "firebrick", pch = 21, col = "white", cex = 2.5)
                    }
                    par(opar)
                }, bg = "transparent", height = input$rv_height)
            }
        })
        hide_review_pane <- function() {
            js_hide2("review_pane")
            dojs("revpl.video_stop();")
            review_pane_active(FALSE)
        }

        rv_clickdrag <- reactiveValues(mousedown = NULL, mousedown_time = NULL, closest_down = NULL, mouseup = NULL)
        last_rv_mouse_pos <- reactiveVal(NULL)
        observeEvent(input$did_rv_mousedown, {
            ##cat("rv mouse down\n")
            if (!is.null(detection_ref()$court_ref)) {
                closest <- NULL
                if (!is.null(input$rv_hover)) {
                    px <- list(x = input$rv_hover$x, y = input$rv_hover$y)
                    ## find the closest point, using court space for the distance
                    cpx <- vid_to_crt(px, arfix = FALSE)
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
                    cpx <- vid_to_crt(px, arfix = FALSE)
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
                        px <- vid_to_crt(px, arfix = FALSE)
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
            dismiss_admin_modal()
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
                if (nrow(rc) > 0) do_video("set_time", tail(rc$t, 1)) else if (length(restore_t) == 1 && !is.na(restore_t)) do_video("set_time", restore_t) else do_video("rew", 3)
            } else {
                ## undo the last code in plays2, this is trickier
                ## we need the game_state and rally_state to reset to, these are in plays2
                ## also need to reconstruct the rally_codes to this point in the rally
                ## game_state is a list, and will be NULL or NA for non-skill rows (?)
                ## so when undoing, remove the last row in plays2 AND all preceding rows with NULL game_state??
                p2 <- rdata$dvw$plays2
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
            if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) overlay_points(head(overlay_points(), -1))
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
            if (!is.null(ridx) && !is.na(ridx)) {
                ##if (where == "above" && ridx > 1) ridx <- ridx-1L ## we are inserting above the selected row, so use the previous row to populate this one
                ## otherwise (if inserting below) use the current row (ridx) as the template
                editing$active <- paste0("insert ", where)
                show_manual_code_modal(editing$active)
            }
        }

        edit_data_row <- function() {
            editing$active <- "edit"
            show_manual_code_modal(editing$active)
        }

        ## op "edit" will also need the row number passed to build_code_entry, not yet implemented
        show_manual_code_modal <- function(op, entry_guide = FALSE) {
            op <- match.arg(op, c("insert below", "insert above", "edit"))
            if (op == "edit") {
                ridx <- playslist_mod$current_row()
                entry_guide <- TRUE
            } else {
                ridx <- 1L
            }
            if (!is.null(ridx) && !is.na(ridx)) {
                showModal(modalDialog(title = if (grepl("insert", op)) paste0("Insert new code ", sub("insert ", "", op), " current row") else "Edit code", size = "l", footer = tags$div(actionButton("edit_commit", label = paste0(if (grepl("insert", op)) "Insert" else "Update", " code (or press Enter)")), actionButton("edit_cancel", label = "Cancel (or press Esc)")),
                                      if (entry_guide) paste0(if (op == "edit") "Edit" else "Enter", " the code either in the top text box or in the individual boxes (but not both)"),
                                      textInput("code_entry", label = "Code:", value = if (op == "edit") rdata$dvw$plays$code[ridx] else ""), if (entry_guide) "or", if (entry_guide) build_code_entry_guide(sub(" .*", "", op), thisrow = rdata$dvw$plays[ridx, ])
                                      ))
                focus_to_modal_element("code_entry")
            }
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
            } else if (!is.null(app_data$video_src) && nchar(app_data$video_src) && !is_url(app_data$video_src)) {
                basename(fs::path_ext_remove(app_data$video_src))
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
                    if (app_data$run_env %eq% "shiny_local") {
                        ## this only makes sense if running locally, not deployed on a server
                        tf <- tempfile(tmpdir = file.path(app_data$user_dir, "autosave"), pattern = "ovscout2-", fileext = ".rds")
                        try({
                            temp <- rdata$dvw
                            temp$scouting_options <- isolate(rdata$options)
                            ## save court refs
                            dr <- list()
                            isolate({
                                if (!is.null(detection_ref1()$court_ref)) dr[[app_data$video_src]] <- detection_ref1()
                                if (!is.null(detection_ref2()$court_ref) && "video_src2" %in% names(app_data) && !is.na(app_data$video_src2) && nzchar(app_data$video_src2)) dr[[app_data$video_src2]] <- detection_ref2()
                            })
                            temp$detection_refs <- dr
                            saveRDS(temp, file = tf)
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
                    rds_ok <- FALSE
                    if (app_data$run_env %eq% "shiny_local") {
                        ## this only makes sense if running locally, not deployed on a server
                        tf <- tempfile(tmpdir = file.path(app_data$user_dir, "autosave"), pattern = "ovscout2-", fileext = ".rds")
                        try({
                            out <- rdata$dvw
                            out$scouting_options <- isolate(rdata$options)
                            ## save court refs
                            dr <- list()
                            isolate({
                                if (!is.null(detection_ref1()$court_ref)) dr[[app_data$video_src]] <- detection_ref1()
                                if (!is.null(detection_ref2()$court_ref) && "video_src2" %in% names(app_data) && !is.na(app_data$video_src2) && nzchar(app_data$video_src2)) dr[[app_data$video_src2]] <- detection_ref2()
                            })
                            out$detection_refs <- dr
                            saveRDS(out, file = tf)
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
                    rally_ended()
                } else if (code %in% c("*c", "ac")) {
                    ## substitution
                    if (code %eq% "*c") {
                        p_out <- as.numeric(input$ht_sub_out)
                        p_in <- as.numeric(input$ht_sub_in)
                    } else {
                        p_out <- as.numeric(input$vt_sub_out)
                        p_in <- as.numeric(input$vt_sub_in)
                    }
                    if (length(p_out) == 1 && length(p_in) == 1) {
                        tm <- substr(code, 1, 1)
                        current_setter <- get_setter(game_state, team = tm)
                        game_state <- game_state_make_substitution(game_state, team = tm, player_out = p_out, player_in = p_in, dvw = rdata$dvw)
                        rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(paste0(substr(code, 1, 1), "c", ldz(p_out), ":", ldz(p_in)), game_state = game_state, rally_ended = FALSE, dvw = rdata$dvw)))
                        ## if we just substituted the player about to serve, we need to update the serve preselect buttons
                        do_serve_preselect()
                        ## did we substitute the setter?
                        if (p_out %eq% current_setter && current_setter > 0) {
                            ## yes we did
                            ht <- vt <- FALSE
                            if (tm %eq% "*") {
                                ## home players on court
                                ht_on <- get_players(game_state, team = "*", dvw = rdata$dvw)
                                ord <- order(ht_on)
                                chc <- setNames(paste0(ht_on, "@", seq_along(ht_on)), player_nums_to(ht_on, team = "*", dvw = rdata$dvw))[ord]
                                ht <- TRUE
                                buts <- make_fat_radio_buttons(choices = chc, selected = NA, input_var = "new_setter")
                            } else {
                                ## visiting players on court
                                vt_on <- get_players(game_state, team = "a", dvw = rdata$dvw)
                                ord <- order(vt_on)
                                chc <- setNames(paste0(vt_on, "@", seq_along(vt_on)), player_nums_to(vt_on, team = "a", dvw = rdata$dvw))[ord]
                                vt <- TRUE
                                buts <- make_fat_radio_buttons(choices = chc, selected = NA, input_var = "new_setter")
                            }
                            showModal(vwModalDialog(title = paste0("On-court setter: ", if (ht) paste0(datavolley::home_team(rdata$dvw), " (home)") else paste0(datavolley::visiting_team(rdata$dvw), " (visiting)")), footer = NULL, width = 100,
                                                    tags$div(tags$p(tags$strong("New setter")), do.call(fixedRow, lapply(buts, function(but) column(2, but)))),
                                                    tags$hr(),
                                                    fixedRow(column(2, offset = 8, make_fat_buttons(choices = c("Assign setter" = if (ht) "*P" else "aP"), input_var = "manual_code", class = "continue")),
                                                             column(2, actionButton("admin_dismiss", "Cancel", class = "cancel fatradio")))
                                                    ))
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

        observeEvent(input$general_help, introjs(session, options = list("nextLabel" = "Next", "prevLabel" = "Previous", "skipLabel" = "Skip")))
        observeEvent(input$show_shortcuts, {
            c_or <- function(...) paste0(..., collapse = " or ")
            content <- list(tags$li(paste0("[", c_or(app_data$shortcuts$pause), "] pause")),
                            tags$li(paste0("[", c_or(app_data$shortcuts$pause_no_popup), "] pause (without the admin popup)")),
                            tags$li(paste0("[", c_or(app_data$shortcuts$go_to_time), "] jump the video to the time of the currently-selected event in the plays table")),
                            tags$li(paste0("[", c_or(app_data$shortcuts$undo), "] undo last rally action")))
            if (have_second_video) content <- c(content, list(tags$li(paste0("[", c_or(app_data$shortcuts$switch_video), "] switch video source"))))
            content <- list(tags$p(tags$strong("General controls")), do.call(tags$ul, content))
            if (app_data$with_video) {
                content <- c(content,
                             list(tags$p(tags$strong("Video controls")),
                                  tags$ul(
                                           tags$li(paste0("[", c_or(app_data$shortcuts$video_forward_2), "] forward 2s, [", c_or(app_data$shortcuts$video_forward_10), "] forward 10s, [", c_or(app_data$shortcuts$video_forward_0.1), "] forwards 0.1s, [", c_or(app_data$shortcuts$video_forward_1_30), "] forwards 1 frame")),
                                           tags$li(paste0("[", c_or(app_data$shortcuts$video_rewind_2), "] backward 2s, [", c_or(app_data$shortcuts$video_rewind_10), "] backward 10s, [", c_or(app_data$shortcuts$video_rewind_0.1), "] backwards 0.1s, [", c_or(app_data$shortcuts$video_rewind_1_30), "] backwards 1 frame")),
                                           tags$li(paste0("[", c_or(app_data$shortcuts$pause), "] pause video")),
                                           tags$li(paste0("[", c_or(app_data$shortcuts$go_to_time), "] go to currently-selected event")),
                                           tags$li(paste0("[", c_or(app_data$shortcuts$switch_video), "] switch videos (if two available)"))
                                       )))
            }
            showModal(
                modalDialog(title = "Keyboard shortcuts", easyClose = TRUE, size = "l",
                            do.call(tagList, content)
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
                dvw$plays <- NULL ## don't save this
                dvw$game_state <- isolate(reactiveValuesToList(game_state))
                dvw$scouting_options <- isolate(rdata$options)
                tf <- tempfile(tmpdir = file.path(app_data$user_dir, "autosave"), pattern = "ovscout2-", fileext = ".ovs")
                ## save court refs
                dr <- list()
                isolate({
                    if (!is.null(detection_ref1()$court_ref)) dr[[app_data$video_src]] <- detection_ref1()
                    if (!is.null(detection_ref2()$court_ref) && "video_src2" %in% names(app_data) && !is.na(app_data$video_src2) && nzchar(app_data$video_src2)) dr[[app_data$video_src2]] <- detection_ref2()
                })
                dvw$detection_refs <- dr
                saveRDS(dvw, tf)
                message("working file has been saved to: ", tf)
            }, error = function(e) {
                message("could not save working file on exit (error message was: ", conditionMessage(e))
            })
        })
        ## seek to video time on startup
        if ("video_time" %in% names(app_data$dvw$plays2) && nrow(app_data$dvw$plays2) > 0) {
            temp_vt <- na.omit(app_data$dvw$plays2$video_time)
            if (length(temp_vt) > 0) do_video("set_time", rebase_time(max(temp_vt), time_from = 1)) ## rebase here should not be necessary, unless somehow we've started on video 2
        }

        ## reports
        output$reports_ui <- renderUI({
            if (ov_pandoc_ok() && nrow(rdata$dvw$plays2) > 0) {
                shinyWidgets::dropdown(inputId = "reports", label = "Reports", actionButton("mr_generate", "Match report"))
            } else {
                NULL
            }
        })
        observeEvent(input$mr_generate, {
            temp_dvw_file <- tempfile(fileext = ".dvw")
            dv_write2(update_meta(rp2(rdata$dvw)), file = temp_dvw_file)
            servable_url <- NULL
            tryCatch({
                withProgress(message = "Generating match report", value = 0, {
                    rargs <- list(x = temp_dvw_file, format = "paged_pdf", style = "ov1", vote = FALSE, shiny_progress = TRUE, chrome_print_extra_args = if (app_data$run_env %eq% "shiny_local") NULL else c("--no-sandbox", "--disable-gpu"))
                    if ("icon" %in% names(app_data) && file.exists(app_data$icon)) rargs$icon <- app_data$icon
                    ##header_extra_pre = "<div style=\"position:absolute; bottom:-7mm; right:2mm; font-size:9px;\">\nReport via <https://openvolley.org/ovscout2>\n</div>\n"
                    rcss <- volleyreport::vr_css()
                    if ("report_css" %in% names(app_data) && is.list(app_data$report_css) && length(app_data$report_css) > 0) rcss <- modifyList(rcss, app_data$report_css)
                    rargs$css <- rcss
                    rfile <- do.call(volleyreport::vr_match_summary, rargs)
                    unlink(temp_dvw_file)
                    servable_file_abs_path <- file.path(app_data$reports_dir, basename(rfile))
                    servable_url <- paste0("/reports/", basename(rfile))
                    file.copy(rfile, servable_file_abs_path)
                    ##onStop(function() try({ unlink(servable_file_abs_path); unlink(rfile) }, silent = TRUE))
                    ##onSessionEnded(function() try({ unlink(servable_file_abs_path); unlink(rfile) }, silent = TRUE))
                    editing$active <- "match report"
                    showModal(vwModalDialog(title = "Match report", footer = NULL, width = 100,
                                            tags$iframe(style = "width:80%; height:100vh;", src = servable_url),
                                            tags$br(),
                                            tags$hr(),
                                            fixedRow(column(2, offset = 10, actionButton("just_cancel", "Return to scouting", class = "continue fatradio")))
                                            ))
                })
            }, error = function(e) {
                showModal(vwModalDialog(title = "Match report", footer = NULL, width = 100,
                                        tags$p("Sorry, something went wrong generating the PDF. (The error message was: ", conditionMessage(e), ")"),
                                        tags$br(),
                                        tags$hr(),
                                        fixedRow(column(2, offset = 10, actionButton("just_cancel", "Return to scouting", class = "continue fatradio")))
                                        ))
            })
        })
    }
}
