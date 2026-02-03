## match data editing
code_make_change <- function(editing_active, game_state, dvw, input, htdata_edit = NULL, vtdata_edit = NULL, htdata_select = NULL, vtdata_select = NULL, type_mode_preferred_zones_or_cones) {
    removeModal()
    do_reparse <- FALSE
    if (is.null(editing_active)) {
        ## not triggered from current editing activity, huh?
        warning("code_make_change entered but editing not active")
    } else if (editing_active %eq% .C_teams) {
        if (!"setter_system" %in% names(dvw$meta$teams)) dvw$meta$teams$setter_system <- "Not specified"
        ## update from all the input$ht_edit_name/id/coach/assistant inputs
        te_ns <- function(id) paste0("team_editor-", id) ## to reference the UI elements in the team_editor module. Note the hard-coding of the 'team_editor' id
        htidx <- which(dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
        dvw$meta$teams$team[htidx] <- input[[te_ns("ht_edit_name")]]
        dvw$meta$teams$team_id[htidx] <- input[[te_ns("ht_edit_id")]]
        dvw$meta$teams$coach[htidx] <- input[[te_ns("ht_edit_coach")]]
        dvw$meta$teams$assistant[htidx] <- input[[te_ns("ht_edit_assistant")]]
        dvw$meta$teams$setter_system[htidx] <- input[[te_ns("ht_setter_system")]]
        if (!is.null(htdata_edit)) dvw$meta$players_h <- make_players(htdata_edit)
        ## and visiting team
        vtidx <- which(dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
        dvw$meta$teams$team[vtidx] <- input[[te_ns("vt_edit_name")]]
        dvw$meta$teams$team_id[vtidx] <- input[[te_ns("vt_edit_id")]]
        dvw$meta$teams$coach[vtidx] <- input[[te_ns("vt_edit_coach")]]
        dvw$meta$teams$assistant[vtidx] <- input[[te_ns("vt_edit_assistant")]]
        dvw$meta$teams$setter_system[vtidx] <- input[[te_ns("vt_setter_system")]]
        if (!is.null(vtdata_edit)) dvw$meta$players_v <- make_players(vtdata_edit)
        do_reparse <- TRUE
    } else if (editing_active %eq% .C_select_teams) {
        if (!"setter_system" %in% names(dvw$meta$teams)) dvw$meta$teams$setter_system <- "Not specified"
        ts_ns <- function(id) paste0("team_selector-", id) ## to reference the UI elements in the team_selector module. Note the hard-coding of the 'team_selector' id
        ## Home team
        htidx <- which(dvw$meta$teams$home_away_team %eq% "*")
        dvw$meta$teams$team[htidx] <- input[[ts_ns("ht_select_name")]]
        dvw$meta$teams$team_id[htidx] <- input[[ts_ns("ht_select_id")]]
        dvw$meta$teams$coach[htidx] <- input[[ts_ns("ht_select_coach")]]
        dvw$meta$teams$assistant[htidx] <- input[[ts_ns("ht_select_assistant")]]
        dvw$meta$teams$setter_system[htidx] <- input[[ts_ns("ht_select_setter_system")]]
        if (!is.null(htdata_select)) dvw$meta$players_h <- make_players(htdata_select)
        ## and visiting team
        vtidx <- which(dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
        dvw$meta$teams$team[vtidx] <- input[[ts_ns("vt_select_name")]]
        dvw$meta$teams$team_id[vtidx] <- input[[ts_ns("vt_select_id")]]
        dvw$meta$teams$coach[vtidx] <- input[[ts_ns("vt_select_coach")]]
        dvw$meta$teams$assistant[vtidx] <- input[[ts_ns("vt_select_assistant")]]
        dvw$meta$teams$setter_system[vtidx] <- input[[ts_ns("vt_select_setter_system")]]
        if (!is.null(vtdata_select)) dvw$meta$players_v <- make_players(vtdata_select) %>% mutate(X3 = .data$X3 + nrow(dvw$meta$players_h))
        do_reparse <- TRUE
    } else if (editing_active %eq% .C_match_data) {
        md_ns <- function(id) paste0("match_data_editor-", id) ## to reference the UI elements in the match_data_editor module. Note the hard-coding of the 'match_data_editor' id
        dvw$meta$match$date <- input[[md_ns("match_edit_date")]]
        dvw$meta$match$time <- tryCatch(lubridate::hms(input[[md_ns("match_edit_time")]]), error = function(e) lubridate::as.period(NA))
        dvw$meta$match$season <- input[[md_ns("match_edit_season")]]
        dvw$meta$match$league <- input[[md_ns("match_edit_league")]]
        dvw$meta$match$phase <- input[[md_ns("match_edit_phase")]]
        dvw$meta$match$home_away <- input[[md_ns("match_edit_home_away")]]
        dvw$meta$match$day_number <- input[[md_ns("match_edit_day_number")]]
        dvw$meta$match$match_number <- input[[md_ns("match_edit_match_number")]]
        ## currently disabled dvw$meta$match$regulation <- input[[md_ns("match_edit_regulation")]]
        if (!is.null(input[[md_ns("match_edit_zones_or_cones")]]) && input[[md_ns("match_edit_zones_or_cones")]] %in% c("Z", "C")) {
            dvw$meta$match$zones_or_cones <- input[[md_ns("match_edit_zones_or_cones")]]
            app_data <- getsv("app_data")
            if (isolate(app_data$scout_mode_r()) == "type") {
                ## user has (possibly) modified the zones/cones for this match, so keep track of their preferred setting
                type_mode_preferred_zones_or_cones <- input[[md_ns("match_edit_zones_or_cones")]]
            }
        }
        dvw$meta$more$scout <- input[[md_ns("more_edit_scout")]]
        ## comments
        null2na <- function(z) if (length(z) < 1) NA_character_ else z
        dvw$meta$comments <- tibble(comment_1 = null2na(input[[md_ns("edit_comments1")]]), comment_2 = null2na(input[[md_ns("edit_comments2")]]), comment_3 = null2na(input[[md_ns("edit_comments3")]]), comment_4 = null2na(input[[md_ns("edit_comments4")]]))
        do_reparse <- TRUE
    } else if (editing_active %eq% .C_change_starting_lineup) {
        le_ns <- function(id) paste0("lineup_editor-", id) ## to reference the UI elements in the lineup_editor module. Note the hard-coding of the 'lineup_editor' id
        beach <- is_beach(dvw)
        pseq <- if (beach) 1:2 else 1:6
        htok <- nzchar(input[[le_ns("ht_P1")]]) && nzchar(input[[le_ns("ht_P2")]])
        if (!beach) htok <- htok && nzchar(input[[le_ns("ht_P3")]]) && nzchar(input[[le_ns("ht_P4")]]) && nzchar(input[[le_ns("ht_P5")]]) && nzchar(input[[le_ns("ht_P6")]]) && nzchar(input[[le_ns("ht_setter")]])
        if (htok) {
            ht <- list(lineup = as.numeric(c(input[[le_ns("ht_P1")]], input[[le_ns("ht_P2")]], if (!beach) c(input[[le_ns("ht_P3")]], input[[le_ns("ht_P4")]], input[[le_ns("ht_P5")]], input[[le_ns("ht_P6")]]))), setter = NA_integer_)
            if (!beach) {
                ht$setter <- as.numeric(input[[le_ns("ht_setter")]])
                ht$liberos <- c(if (!nzchar(input[[le_ns("ht_libero1")]])) -1L else as.integer(input[[le_ns("ht_libero1")]]),
                                if (!nzchar(input[[le_ns("ht_libero2")]])) -1L else as.integer(input[[le_ns("ht_libero2")]]))
            }
        } else {
            ## missing or incomplete home team lineup
            ht <- list(lineup = as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)]), setter = NA_integer_)
            if (!beach) {
                ht$setter <- as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)])[game_state$home_setter_position]
                ht$liberos <- c(NA_integer_, NA_integer_)
            }
        }
        vtok <- nzchar(input[[le_ns("vt_P1")]]) && nzchar(input[[le_ns("vt_P2")]])
        if (!beach) vtok <- vtok && nzchar(input[[le_ns("vt_P3")]]) && nzchar(input[[le_ns("vt_P4")]]) && nzchar(input[[le_ns("vt_P5")]]) && nzchar(input[[le_ns("vt_P6")]]) && nzchar(input[[le_ns("vt_setter")]])
        if (vtok) {
            vt <- list(lineup = as.numeric(c(input[[le_ns("vt_P1")]], input[[le_ns("vt_P2")]], if (!beach) c(input[[le_ns("vt_P3")]], input[[le_ns("vt_P4")]], input[[le_ns("vt_P5")]], input[[le_ns("vt_P6")]]))), setter = NA_integer_)
            if (!beach) {
                vt$setter <- as.numeric(input[[le_ns("vt_setter")]])
                vt$liberos <- c(if (!nzchar(input[[le_ns("vt_libero1")]])) -1L else as.integer(input[[le_ns("vt_libero1")]]),
                                if (!nzchar(input[[le_ns("vt_libero2")]])) -1L else as.integer(input[[le_ns("vt_libero2")]]))
            }
        } else {
            ## missing or incomplete home team lineup
            vt <- list(lineup = as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)]), setter = NA_integer_)
            if (!beach) {
                vt$setter <- as.numeric(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)])[game_state$visiting_setter_position]
                vt$liberos <- c(NA_integer_, NA_integer_)
            }
        }
        ## the liberos go into game_state
        game_state$ht_lib1 <- if (length(ht$liberos) > 0) ht$liberos[1] else NA_integer_
        game_state$ht_lib2 <- if (length(ht$liberos) > 1) ht$liberos[2] else NA_integer_
        game_state$vt_lib1 <- if (length(vt$liberos) > 0) vt$liberos[1] else NA_integer_
        game_state$vt_lib2 <- if (length(vt$liberos) > 1) vt$liberos[2] else NA_integer_
        setnum <- if (is.null(game_state$set_number) || is.na(game_state$set_number)) {
                      ## assume is set 1, probably needs something better
                      1L
                  } else {
                      game_state$set_number
                  }
        if (!isTRUE(game_state$set_started)) {
            ## update the metadata and also plays2
            tryCatch({
                dvw <- dv_set_lineups(dvw, set_number = setnum, lineups = list(c(ht$lineup, na.omit(ht$liberos)), c(vt$lineup, na.omit(vt$liberos))), setters = c(ht$setter, vt$setter))
            }, error = function(e) warning(conditionMessage(e)))
            ## TODO, show some useful message to the user that the lineup operation failed
            do_reparse <- TRUE
        }
        ## NOTE that if the set was in progress (set_started = TRUE) then only the game_state$Xt_libY values will be updated and the rest will be silently ignored TODO warn the user if they are editing lineups after the set has started?
    } else {
        warning("I don't know what to do with editing_active: ", editing_active)
    }
    list(dvw = dvw, type_mode_preferred_zones_or_cones = type_mode_preferred_zones_or_cones, do_reparse = do_reparse)
}

