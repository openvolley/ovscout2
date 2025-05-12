show_admin_modal <- function(game_state, dvw) {
    ## can home team make a sub?
    ht_on <- sort(get_players(game_state, team = "*", dvw = dvw))
    ht_other <- setdiff(na.omit(dvw$meta$players_h$number), ht_on)
    ht_other <- setdiff(ht_other, get_liberos(game_state, team = "*", dvw = dvw))
    ht_can_sub <- length(ht_other) > 0
    ## and visiting team?
    vt_on <- sort(get_players(game_state, team = "a", dvw = dvw))
    vt_other <- setdiff(na.omit(dvw$meta$players_v$number), vt_on)
    vt_other <- setdiff(vt_other, get_liberos(game_state, team = "a", dvw = dvw))
    vt_can_sub <- length(vt_other) > 0
    rip <- isTRUE(game_state$rally_started) ## rip is "rally in progress"
    showModal(vwModalDialog(title = "Miscellaneous", footer = NULL, width = 100,
                            tags$p(tags$strong("Match actions")),
                            fluidRow(column(2, actionButton("undo", "Undo last rally action", class = "undo fatradio")),
                                     column(2, actionButton("enter_code", "Enter scout code", class = "fatradio"), tags$span(style = "font-size:small;", "Only non-skill codes are supported")),
                                     column(2, actionButton("end_of_set_confirm", "End of set", class = "fatradio"))),
                            tags$br(), tags$br(),
                            ## NB if (!rip) - applied to buttons that should not be available mid-rally
                            fluidRow(column(6, tags$strong(datavolley::home_team(dvw), "(home)")),
                                     column(6, tags$strong(datavolley::visiting_team(dvw), "(visiting)"))),
                            fluidRow(column(2, make_fat_buttons(choices = c("Won current rally" = "*p"), input_var = "manual_code")),
                                     column(2, if (!rip) make_fat_buttons(choices = c(Timeout = "*T"), input_var = "manual_code")),
                                     column(2, if (ht_can_sub && !rip) make_fat_buttons(choices = c(Substitution = "*c"), input_var = "substitution")),
                                     column(2, make_fat_buttons(choices = c("Won current rally" = "ap"), input_var = "manual_code")),
                                     column(2, if (!rip) make_fat_buttons(choices = c(Timeout = "aT"), input_var = "manual_code")),
                                     column(2, if (vt_can_sub && !rip) make_fat_buttons(choices = c(Substitution = "ac"), input_var = "substitution"))),
                            if (!rip) tags$br(),
                            if (!rip) fluidRow(column(2, make_fat_buttons(choices = c("Change setter" = "*P"), input_var = "change_setter")),
                                               column(2, offset = 4, make_fat_buttons(choices = c("Change setter" = "aP"), input_var = "change_setter"))),
                            tags$br(),
                            tags$hr(),
                            fixedRow(column(2, offset = 10, actionButton("admin_dismiss", "Return to scouting", class = "continue fatradio")))
                            ))
}

dismiss_admin_modal <- function(editing) {
    ## dismiss the admin modal and unpause the video
    editing$active <- NULL
    removeModal()
    do_video("play")
    focus_to_scout_bar()
}

show_save_error_modal <- function(msg, ovs_ok, tempfile_name) {
    showModal(modalDialog(title = "Save error",
                          tags$div(class = "alert alert-danger", "Sorry, the save failed. The error message was:", tags$br(), tags$pre(msg), tags$br(), if (ovs_ok) paste0("The edited datavolley object has been saved to ", tempfile_name, "."))))
}

show_change_setter_modal <- function(code, game_state, dvw) {
    ht <- vt <- FALSE
    if (code %eq% "*P") {
        ## home players on court
        ht_on <- get_players(game_state, team = "*", dvw = dvw)
        ord <- order(ht_on)
        chc <- setNames(paste0(ht_on, "@", seq_along(ht_on)), player_nums_to(ht_on, team = "*", dvw = dvw, game_state = game_state))[ord]
        ht <- TRUE
        buts <- make_fat_radio_buttons(choices = chc, selected = NA, input_var = "new_setter")
    } else {
        ## visiting players on court
        vt_on <- get_players(game_state, team = "a", dvw = dvw)
        ord <- order(vt_on)
        chc <- setNames(paste0(vt_on, "@", seq_along(vt_on)), player_nums_to(vt_on, team = "a", dvw = dvw, game_state = game_state))[ord]
        vt <- TRUE
        buts <- make_fat_radio_buttons(choices = chc, selected = NA, input_var = "new_setter")
    }
    showModal(vwModalDialog(title = paste0("On-court setter: ", if (ht) paste0(datavolley::home_team(dvw), " (home)") else paste0(datavolley::visiting_team(dvw), " (visiting)")), footer = NULL, width = 100,
                            tags$div(tags$p(tags$strong("New setter")), do.call(fixedRow, lapply(buts, function(but) column(2, but)))),
                            tags$hr(),
                            fixedRow(column(2, offset = 8, make_fat_buttons(choices = c("Assign setter" = if (ht) "*P" else "aP"), input_var = "manual_code", class = "continue")),
                                     column(2, actionButton("admin_dismiss", "Cancel", class = "cancel fatradio")))
                            ))
}

show_substitution_modal <- function(sub_code, game_state, dvw) {
    ht_sub <- vt_sub <- FALSE
    if (sub_code %eq% "*c") {
        ## home player sub buttons
        ht_on <- sort(get_players(game_state, team = "*", dvw = dvw))
        names(ht_on) <- player_nums_to(ht_on, team = "*", dvw = dvw, game_state = game_state)
        ht_other <- setdiff(na.omit(dvw$meta$players_h$number), ht_on)
        ht_other <- sort(setdiff(ht_other, get_liberos(game_state, team = "*", dvw = dvw)))
        names(ht_other) <- player_nums_to(ht_other, team = "*", dvw = dvw, game_state = game_state)
        ht_sub_out <- make_fat_radio_buttons(choices = ht_on, selected = NA, input_var = "ht_sub_out")
        ht_sub_in <- make_fat_radio_buttons(choices = ht_other, selected = NA, input_var = "ht_sub_in")
        ht_sub <- TRUE
    } else {
        ## visiting player sub buttons
        vt_on <- sort(get_players(game_state, team = "a", dvw = dvw))
        names(vt_on) <- player_nums_to(vt_on, team = "a", dvw = dvw, game_state = game_state)
        vt_other <- setdiff(na.omit(dvw$meta$players_v$number), vt_on)
        vt_other <- sort(setdiff(vt_other, get_liberos(game_state, team = "a", dvw = dvw)))
        names(vt_other) <- player_nums_to(vt_other, team = "a", dvw = dvw, game_state = game_state)
        vt_sub_out <- make_fat_radio_buttons(choices = vt_on, selected = NA, input_var = "vt_sub_out")
        vt_sub_in <- make_fat_radio_buttons(choices = vt_other, selected = NA, input_var = "vt_sub_in")
        vt_sub <- TRUE
    }
    showModal(vwModalDialog(title = paste0("Substitution: ", if (ht_sub) paste0(datavolley::home_team(dvw), " (home)") else paste0(datavolley::visiting_team(dvw), " (visiting)")), footer = NULL, width = 100,
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

show_video_setup_modal <- function(prevsrc, dvw) {
    showModal(vwModalDialog(title = "Video setup", footer = NULL, width = 100,
       uiOutput("preview_header"),
       HTML(paste0("<video id=\"video_preview\" style=\"width:100%; height:50vh;\" class=\"video-js\" data-setup='{ ", if (prevsrc$type == "youtube") "\"techOrder\": [\"youtube\"], ", "\"controls\": true, \"autoplay\": true, \"preload\": \"auto\", \"liveui\": true, \"muted\": true, \"sources\": ", if (prevsrc$type == "youtube") paste0("[{ \"type\": \"video/youtube\", \"src\": \"", prevsrc$src, "\"}]") else paste0("[{ \"src\": \"", prevsrc$src, "\"}]"), " }'>\n", "<p class=\"vjs-no-js\">This app cannot be used without a web browser that <a href=\"https://videojs.com/html5-video-support/\" target=\"_blank\">supports HTML5 video</a></p></video>")),
       fluidRow(style = "margin-top:40px;", column(4, offset = 2, numericInput("v2_offset_value", "Video 2 offset (s):", value = dvw$video2_offset)),
                column(4, actionButton("switch_preview", "Switch video"))),
       tags$br(),
       tags$hr(),
       fixedRow(column(2, offset = 10, actionButton("preview_dismiss", "Return to scouting", class = "continue fatradio")))
       ))
}

show_save_dvw_modal <- function() {
    showModal(modalDialog(title = "Export dvw file", easyClose = TRUE, size = "l", footer = NULL,
                          checkboxInput("dvw_save_with_cones", "Save attack directions as cones"),
                          tags$br(),
                          tags$hr(),
                          fixedRow(column(2, actionButton("just_cancel", "Cancel", class = "cancel fatradio")),
                                   column(2, offset = 8, downloadButton("save_dvw_button", "Export dvw", class = "continue fatradio", style = "padding-top:24px;")))
                          ))
}

show_prefcuts_modal <- function(prefs, opts) {
    ## opts comes from rdata$options
    showModal(vwModalDialog(title = "Preferences", footer = NULL, width = 100,
       tabsetPanel(id = "prefs_tabs",
                   tabPanel(tags$strong("App preferences"),
                            tags$hr(), tags$br(),
                            fluidRow(column(3, selectInput("prefs_scout_mode", "Scouting mode", choices = c("Click mode" = "click", "Typing mode" = "type"), selected = if (is.null(prefs$scout_mode) || !prefs$scout_mode %in% c("click", "type")) "click" else prefs$scout_mode),
                                            tags$p("Note that changing the scout mode preference here will", tags$strong("not"), "affect the current match, it sets your preference for future matches. If you want to change the current scout mode, do this via the 'Change scout mode' button in the main interface.")),
                                     column(3, checkboxInput("prefs_show_courtref", "Show court reference?", value = prefs$show_courtref)),
                                     column(3, textInput("prefs_scout", label = "Default scout name:", placeholder = "Your name", value = prefs$scout_name)),
                                     column(3, checkboxInput("prefs_scoreboard", "Show scoreboard in the top-right of the video pane?", value = prefs$scoreboard))),
                            tags$br(),
                            fluidRow(column(3, checkboxInput("prefs_ball_path", "Show the ball path on the court inset diagram?", value = prefs$ball_path)),
                                     column(3, selectInput("prefs_playlist_display_option", "Plays list style", choices = c("Scouted codes" = "dv_codes", "Commentary style" = "commentary"), selected = prefs$playlist_display_option)),
                                     column(3, checkboxInput("prefs_review_pane", "Show review pane (video loop) in popups?", value = prefs$review_pane)),
                                     column(3, numericInput("prefs_pause_on_type", tags$span(title = HTML("Pauses the video for this many milliseconds after each keypress in the scouting bar (typing mode only). Set to zero for no pause after keypress."), "Pause video after each keypress (milliseconds):", icon("question-circle")), value = prefs$pause_on_type, min = 0, step = 100)))
                            ),
                   tabPanel(tags$strong("'Click' scouting conventions"),
                            tags$hr(), tags$br(), tags$p("These conventions apply when scouting in \"click\" mode (not typing mode).", tags$span(style = "color:red;", "Warning: changing scouting conventions once a match is already partially-scouted could lead to inconsistent files.")),
                            tags$hr(),
                            fluidRow(column(3, selectInput("scopts_end_convention", tags$span(title = HTML("Is the end coordinate of an attack or serve the actual end location (where the ball was played or contacted the floor), or the intended one. The actual might differ from the intended if there is a block touch or the ball hit the net. If 'intended', and a block touch is recorded, then the end location of the attack will not be used for the dig location (the dig location will be missing)."), "End convention:", icon("question-circle")), choices = c(Intended = "intended", Actual = "actual"), selected = opts$end_convention)),
                                     column(3, checkboxInput("scopts_nblockers", tags$span(title = HTML("Record the number of blockers on each attack?"), "Record the number of blockers?", icon("question-circle")), value = opts$nblockers)),
                                     column(3, selectInput("scopts_default_nblockers", tags$span(title = HTML("If we are scouting the number of blockers, what number should we default to?"), "Default number of blockers:", icon("question-circle")), choices = c("No default" = NA, "No block" = 0, "Single block" = 1, "Double block" = 2, "Triple block" = 3, "Hole block" = 4), selected = opts$default_nblockers))),
                            tags$br(),
                            fluidRow(column(3, checkboxInput("scopts_transition_sets", tags$span(title = HTML("If transition sets are not recorded, then just the endpoint of each attack (i.e. the dig) and the subsequent counter-attack are scouted."), "Record sets in transition?", icon("question-circle")), value = opts$transition_sets)),
                                     column(3, checkboxInput("scopts_set_quality", tags$span(title = HTML("Set quality allows each setting action to be assessed as 'Excellent', 'Good', 'Poor', etc."), "Record set quality?", icon("question-circle")), value = opts$set_quality)),
                                     column(3, selectInput("scopts_setter_calls", tags$span(title = HTML("The 'setter call' is where the setter instructed their middle hitter to run. For example, a setter call of 'K2' means that the middle hitter ran behind the setter (as if to hit a quick ball behind), even if the setter actually set someone else."), "Record setter calls?", icon("question-circle")), choices = c(None = "none", "In reception phase only" = "reception", "In both transition and reception" = "both"), selected = opts$setter_calls))
                                     ##column(3, selectInput("scopts_team_system", tags$span(title = HTML("the assumed system that teams are using to assign e.g. passing and hitting responsibilities. 'SHM3' is a setter-hitter-middle rotation, with 3 passers (the libero and two outside hitters)."), "Team system:", icon("question-circle")), choices = c("SHM3" = "SHM3"), selected = opts$team_system))
                                     ),
                            tags$br(),
                            fluidRow(column(3, selectInput("scopts_attacks_by", tags$span(title = HTML("Classify attacks by 'Code' (X5, V5, etc) or just 'Tempo' (high, medium, quick)."), "Attacks by:", icon("question-circle")), choices = c(Codes = "codes", Tempo = "tempo"), selected = opts$attacks_by)),
                                     column(3, textInput("scopts_setter_dump_code", tags$span(title = HTML("The attack combination code for a setter dump"), "Setter tip attack code:", icon("question-circle")), placeholder = "PP", value = opts$setter_dump_code)), ## string: the attack combination code for a setter dump
                                     column(3, textInput("scopts_second_ball_attack_code", tags$span(title = HTML("The attack combination code for a second-ball attack"), "Second-ball attack code:", icon("question-circle")), placeholder = "P2", value = opts$second_ball_attack_code)), ## string: the attack combination code for a second-ball attack
                                     column(3, textInput("scopts_overpass_attack_code", tags$span(title = HTML("The attack combination code for an attack on an overpass"), "Overpass attack code:", icon("question-circle")), placeholder = "PR", value = opts$overpass_attack_code))) ## string: the attack combination code for an attack on an overpass
                            ## TODO @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
                            ## TODO @param compound_table tibble: the table of compound codes
                            ),
                   tabPanel(tags$strong("'Type' scouting conventions"),
                            tags$hr(), tags$br(), tags$p("These conventions apply when scouting in \"type\" mode."##, tags$span(style = "color:red;", "Warning: changing scouting conventions once a match is already partially-scouted could lead to inconsistent files.")
                                                         ), tags$hr(),
                            fluidRow(column(3, selectInput("scopts_zones_cones", tags$span(title = HTML("Are attack directions being recorded as zones or cones?"), "Attack directions by zones or cones:", icon("question-circle")), choices = c(Cones = "C", Zones = "Z"), selected = opts$zones_cones),
                                            tags$p("Note that changing the zones/cones preference here will", tags$strong("not"), "affect the current match. If you want to change the current match settings, you will need to do this via the 'Edit match data' button in the main interface.")))
                            )
                   ),
       tags$br(),
       tags$hr(),
       fixedRow(column(2, actionButton("just_cancel", "Cancel", class = "cancel fatradio")),
                column(2, offset = 8, actionButton("prefs_save", "Apply and save", class = "continue fatradio")))
       ))
}

show_shortcuts <- function(app_data, editing) {
    show_sc <- function(sc, txt, sctype = "CL") {
        ## sctype is CL for click mode, TY for typing mode, PL for playslist shortcuts
        bb <- paste0(sctype, sc) ## the prefix is a horrible hack so that we can distinguish playslist shortcuts from general ones in the edit handler
        sc_list <- if (sctype == "PL") app_data$playstable_shortcuts else if (sctype == "TY") app_data$type_shortcuts else app_data$click_shortcuts
        ## if we have multiple shortcuts, number them by "@n" suffix
        if (length(sc_list[[sc]]) > 1) bb <- paste0(rep(bb, length(sc_list[[sc]])), "@", seq_len(length(sc_list[[sc]])))
        names(bb) <- if (is.null(sc_list[[sc]])) "no shortcut" else sc_list[[sc]]
        tags$span(txt, make_fat_buttons(choices = bb, input_var = "scedit", class = "scedit notuc"))
    }
    editing$active <- .C_editing_shortcuts
    showModal(
        vwModalDialog(title = "Keyboard shortcuts", easyClose = FALSE, footer = NULL,
            fluidRow(column(4, ## click_shortcuts
                            tags$p(tags$strong("'Click' mode shortcuts"), tags$br(), "These apply when using 'click' scout mode."),
                            tags$ul(tags$li(show_sc("pause", "pause")),
                                    tags$li(show_sc("pause_no_popup", "pause (without the admin popup)")),
                                    tags$li(show_sc("hide_popup", "hide admin popup while key held down")),
                                    tags$li(show_sc("undo", "undo last rally action"))),
                            tags$p("Video controls in 'click' mode"),
                            tags$ul(tags$li(show_sc("video_forward_2", "forward 2s"), show_sc("video_forward_10", "forward 10s"), tags$br(),
                                            show_sc("video_forward_0.1", "forwards 0.1s"), show_sc("video_forward_1_30", "forwards 1 frame")),
                                    tags$li(show_sc("video_rewind_2", "backward 2s"), show_sc("video_rewind_10", "backward 10s"), tags$br(),
                                            show_sc("video_rewind_0.1", "backwards 0.1s"), show_sc("video_rewind_1_30", "backwards 1 frame")),
                                    tags$li(show_sc("pause", "pause video")),
                                    tags$li(show_sc("video_faster", "increase video playback speed")),
                                    tags$li(show_sc("video_slower", "decrease video playback speed")),
                                    tags$li(show_sc("switch_video", "switch video source (if using two videos)")))),
                     column(4, ## type_shortcuts
                            tags$p(tags$strong("'Type' mode shortcuts"), tags$br(), "These apply when the cursor is in the scouting input bar when using typing scout mode. Don't use ordinary keys as shortcuts here, because they will typically be used during normal typing. Use modified keys (e.g. Ctrl-something, Alt-something)."),
                            tags$ul(tags$li(show_sc("pause", "pause", sctype = "TY")),
                                    tags$li(show_sc("pause_no_popup", "pause (without the admin popup)", sctype = "TY")),
                                    tags$li(show_sc("undo", "undo last rally action", sctype = "TY")),
                                    tags$li(show_sc("switch_windows", "switch to plays list", sctype = "TY"))),
                            tags$p("Video controls in 'type' mode"),
                            tags$ul(tags$li(show_sc("video_forward_2", "forward 2s", sctype = "TY"),
                                            show_sc("video_forward_10", "forward 10s", sctype = "TY"), tags$br(),
                                            show_sc("video_forward_0.1", "forwards 0.1s", sctype = "TY"),
                                            show_sc("video_forward_1_30", "forwards 1 frame", sctype = "TY")),
                                    tags$li(show_sc("video_rewind_2", "backward 2s", sctype = "TY"),
                                            show_sc("video_rewind_10", "backward 10s", sctype = "TY"), tags$br(),
                                            show_sc("video_rewind_0.1", "backwards 0.1s", sctype = "TY"),
                                            show_sc("video_rewind_1_30", "backwards 1 frame", sctype = "TY")),
                                    tags$li(show_sc("pause", "pause video", sctype = "TY")),
                                    tags$li(show_sc("video_faster", "increase video playback speed", sctype = "TY")),
                                    tags$li(show_sc("video_slower", "decrease video playback speed", sctype = "TY")),
                                    tags$li(show_sc("switch_video", "switch video source (if using two videos)", sctype = "TY")))),
                     column(4, ## plays list shortcuts, applies to both scouting modes
                            tags$p(tags$strong("Plays list controls"), tags$br("These apply when in the plays list, regardless of scouting mode (click or type).")),
                            tags$ul(tags$li(show_sc("up", "move to previous skill row", sctype = "PL")),
                                    tags$li(show_sc("down", "move to next skill row", sctype = "PL")),
                                    tags$li(show_sc("edit_code", "edit selected code", sctype = "PL")),
                                    tags$li(show_sc("delete_code", "delete selected code", sctype = "PL")),
                                    tags$li(show_sc("insert_code", "insert new code above selected row", sctype = "PL")),
                                    tags$li(show_sc("go_to_time", "seek video to the time of the selected row", sctype = "PL")),
                                    tags$li(show_sc("switch_windows", "switch to scouting bar input (typing mode only)", sctype = "PL"))
                                    ))),
            ## none of these are relevant yet
            ##fluidRow(column(6, tags$strong("Keyboard controls"),
            ##         tags$ul(tags$li("[r or 5] sync selected event video time"),
            ##                 tags$li("[F1] home team rotate +1"),
            ##                 tags$li("[F2] insert setting codes before every attack"),
            ##                 tags$li("[F4] delete all setting codes (except errors)"),
            ##                 tags$li("[F6] insert digging codes after every attack"),
            ##                 tags$li("[F8] delete all digging codes"),
            ##                 tags$li("[F10] visiting team rotate +1"),
            ##                 )),
            ##                tags$strong("Ball coordinates"), tags$ul(tags$li("[left-click the court inset] register the start/mid/end ball positions"),
            ##                                                         tags$li("[accept ball coordinates] to add coordinates to the currently selected item"))))
            tags$br(),
            tags$hr(),
            fixedRow(column(2, actionButton("just_cancel", "Dismiss", class = "cancel fatradio")))
            ))
}

## the modal popup used to review the rally codes before accepting them
review_rally_modal <- function(rcodes) {
    rctxt <- codes_from_rc_rows(rcodes)
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

show_scout_modal <- function(mui, with_review_pane = TRUE) {
    scout_modal_active <- getsv("scout_modal_active")
    prefs <- getsv("prefs")
    scout_modal_active(TRUE)
    showModal(mui)
    if (with_review_pane && isTRUE(prefs$review_pane)) show_review_pane()
}

remove_scout_modal <- function() {
    scout_modal_active <- getsv("scout_modal_active")
    accept_fun <- getsv("accept_fun")
    scout_modal_active(FALSE)
    prefs <- getsv("prefs")
    accept_fun(NULL)
    removeModal()
    if (isTRUE(prefs$review_pane)) hide_review_pane()
}

show_review_pane <- function() {
    current_video_src <- getsv("current_video_src")
    app_data <- getsv("app_data")
    input <- getsv("input")
    review_pane_active <- getsv("review_pane_active")
    ## use the current video time from the main video
    ## construct the playlist js by hand, because we need to inject the current video time
    revsrc <- get_video_source_type(if (current_video_src() == 1L) app_data$video_src else app_data$video_src2, base_url = app_data$video_server_base_url)
    pbrate <- if (!is.null(input$playback_rate) && input$playback_rate > 0) input$playback_rate * 1.4 else 1.4
    dojs(paste0("var start_t=videojs.players.main_video.currentTime()-2; revpl.set_playlist_and_play([{'video_src':'", revsrc$src, "','start_time':start_t,'duration':4,'type':'", revsrc$type, "'}], 'review_player', '", revsrc$type, "', true); revpl.set_playback_rate(", pbrate, ");"))
    js_show2("review_pane")
    dojs("Shiny.setInputValue('rv_height', $('#review_player').innerHeight()); Shiny.setInputValue('rv_width', $('#review_player').innerWidth());")
    review_pane_active(TRUE)
}

hide_review_pane <- function() {
    review_pane_active <- getsv("review_pane_active")
    js_hide2("review_pane")
    dojs("revpl.video_stop();")
    review_pane_active(FALSE)
}

code_edit_dialog_content <- function(which) {
    if (is.null(which)) which <- "clear"
    which <- match.arg(tolower(which), c("coord_click_start", "coord_click_mid", "coord_click_end", "clear"))
    if (which == "coord_click_start") {
        tags$div(class = "alert alert-danger", "Click start location or", actionButton("edit_coord_clear", "No location"), "or", actionButton("edit_coord_cancel", "Cancel"))
    } else if (which == "coord_click_mid") {
        tags$div(class = "alert alert-danger", "Click mid location or", actionButton("edit_coord_clear", "No location"), "or", actionButton("edit_coord_cancel", "Cancel"))
    } else if (which == "coord_click_end") {
        tags$div(class = "alert alert-danger", "Click end location or", actionButton("edit_coord_clear", "No location"), "or", actionButton("edit_coord_cancel", "Cancel"))
    } else if (which == "clear") {
        NULL
    } else {
        warning("unexpected set_code_edit_dialog value: ", which)
        NULL
    }
}
