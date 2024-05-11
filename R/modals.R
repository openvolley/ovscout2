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

show_change_setter_modal <- function(code, game_state, dvw) {
    ht <- vt <- FALSE
    if (code %eq% "*P") {
        ## home players on court
        ht_on <- get_players(game_state, team = "*", dvw = dvw)
        ord <- order(ht_on)
        chc <- setNames(paste0(ht_on, "@", seq_along(ht_on)), player_nums_to(ht_on, team = "*", dvw = dvw))[ord]
        ht <- TRUE
        buts <- make_fat_radio_buttons(choices = chc, selected = NA, input_var = "new_setter")
    } else {
        ## visiting players on court
        vt_on <- get_players(game_state, team = "a", dvw = dvw)
        ord <- order(vt_on)
        chc <- setNames(paste0(vt_on, "@", seq_along(vt_on)), player_nums_to(vt_on, team = "a", dvw = dvw))[ord]
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
        names(ht_on) <- player_nums_to(ht_on, team = "*", dvw = dvw)
        ht_other <- setdiff(na.omit(dvw$meta$players_h$number), ht_on)
        ht_other <- sort(setdiff(ht_other, get_liberos(game_state, team = "*", dvw = dvw)))
        names(ht_other) <- player_nums_to(ht_other, team = "*", dvw = dvw)
        ht_sub_out <- make_fat_radio_buttons(choices = ht_on, selected = NA, input_var = "ht_sub_out")
        ht_sub_in <- make_fat_radio_buttons(choices = ht_other, selected = NA, input_var = "ht_sub_in")
        ht_sub <- TRUE
    } else {
        ## visiting player sub buttons
        vt_on <- sort(get_players(game_state, team = "a", dvw = dvw))
        names(vt_on) <- player_nums_to(vt_on, team = "a", dvw = dvw)
        vt_other <- setdiff(na.omit(dvw$meta$players_v$number), vt_on)
        vt_other <- sort(setdiff(vt_other, get_liberos(game_state, team = "a", dvw = dvw)))
        names(vt_other) <- player_nums_to(vt_other, team = "a", dvw = dvw)
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

show_prefcuts_modal <- function(prefs, opts) {
    ## opts comes from rdata$options
    showModal(vwModalDialog(title = "Preferences", footer = NULL, width = 100,
       tabsetPanel(id = "prefs_tabs",
                   tabPanel(tags$strong("App preferences"),
                            tags$hr(), tags$br(),
                            fluidRow(column(3, checkboxInput("prefs_show_courtref", "Show court reference?", value = prefs$show_courtref)),
                                     column(3, textInput("prefs_scout", label = "Default scout name:", placeholder = "Your name", value = prefs$scout_name)),
                                     column(3, checkboxInput("prefs_scoreboard", "Show scoreboard in the top-right of the video pane?", value = prefs$scoreboard)),
                                     column(3, numericInput("prefs_pause_on_type", tags$span(title = HTML("Pauses the video for this many milliseconds after each keypress in the scouting bar (typing mode only). Set to zero for no pause after keypress."), "Pause video after each keypress (milliseconds):", icon("question-circle")), value = prefs$pause_on_type, min = 0, step = 100))),
                            tags$br(),
                            fluidRow(column(3, checkboxInput("prefs_ball_path", "Show the ball path on the court inset diagram?", value = prefs$ball_path)),
                                     column(3, selectInput("prefs_playlist_display_option", "Plays list style", choices = c("Scouted codes" = "dv_codes", "Commentary style" = "commentary"), selected = prefs$playlist_display_option)),
                                     column(3, checkboxInput("prefs_review_pane", "Show review pane (video loop) in popups?", value = prefs$review_pane)))
                            ),
                   tabPanel(tags$strong("'Click' scouting conventions"),
                            tags$hr(), tags$br(), tags$p("These conventions apply when scouting in \"click\" mode (not typing mode). Warning: changing scouting conventions once a match is already partially-scouted could lead to inconsistent files."),
                            tags$hr(),
                            fluidRow(column(3, selectInput("scopts_end_convention", tags$span(title = HTML("Is the end coordinate of an attack or serve the actual end location (where the ball was played or contacted the floor), or the intended one. The actual might differ from the intended if there is a block touch or the ball hit the net. If 'intended', and a block touch is recorded, then the end location of the attack will not be used for the dig location (the dig location will be missing)."), "End convention:", icon("question-circle")), choices = c(Intended = "intended", Actual = "actual"), selected = opts$end_convention)),
                                     column(3, checkboxInput("scopts_nblockers", tags$span(title = HTML("Record the number of blockers on each attack?"), "Record the number of blockers?", icon("question-circle")), value = opts$nblockers)),
                                     column(3, selectInput("scopts_default_nblockers", tags$span(title = HTML("If we are scouting the number of blockers, what number should we default to?"), "Default number of blockers:", icon("question-circle")), choices = c("No default" = NA, "No block" = 0, "Single block" = 1, "Double block" = 2, "Triple block" = 3, "Hole block" = 4), selected = opts$default_nblockers))),
                            tags$br(),
                            fluidRow(column(3, checkboxInput("scopts_transition_sets", tags$span(title = HTML("If transition sets are not recorded, then just the endpoint of each attack (i.e. the dig) and the subsequent counter-attack are scouted."), "Record sets in transition?", icon("question-circle")), value = opts$transition_sets)),
                                     column(3, selectInput("scopts_attacks_by", tags$span(title = HTML("Classify attacks by 'Code' (X5, V5, etc) or just 'Tempo' (high, medium, quick)."), "Attacks by:", icon("question-circle")), choices = c(Codes = "codes", Tempo = "tempo"), selected = opts$attacks_by))##,
                                     ##column(3, selectInput("scopts_team_system", tags$span(title = HTML("the assumed system that teams are using to assign e.g. passing and hitting responsibilities. 'SHM3' is a setter-hitter-middle rotation, with 3 passers (the libero and two outside hitters)."), "Team system:", icon("question-circle")), choices = c("SHM3" = "SHM3"), selected = opts$team_system))
                                     ),
                            tags$br(),
                            fluidRow(column(3, textInput("scopts_setter_dump_code", tags$span(title = HTML("The attack combination code for a setter dump"), "Setter tip attack code:", icon("question-circle")), placeholder = "PP", value = opts$setter_dump_code)), ## string: the attack combination code for a setter dump
                                     column(3, textInput("scopts_second_ball_attack_code", tags$span(title = HTML("The attack combination code for a second-ball attack"), "Second-ball attack code:", icon("question-circle")), placeholder = "P2", value = opts$second_ball_attack_code)), ## string: the attack combination code for a second-ball attack
                                     column(3, textInput("scopts_overpass_attack_code", tags$span(title = HTML("The attack combination code for an attack on an overpass"), "Overpass attack code:", icon("question-circle")), placeholder = "PR", value = opts$overpass_attack_code))) ## string: the attack combination code for an attack on an overpass
                            ## TODO @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
                            ## TODO @param compound_table tibble: the table of compound codes
                            )
                   ),
       tags$br(),
       tags$hr(),
       fixedRow(column(2, actionButton("just_cancel", "Cancel", class = "cancel fatradio")),
                column(2, offset = 8, actionButton("prefs_save", "Apply and save", class = "continue fatradio")))
       ))
}
