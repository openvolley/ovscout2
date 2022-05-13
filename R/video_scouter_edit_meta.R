## match data editing
code_make_change <- function(editing_active, game_state, dvw, input, htdata_edit = NULL, vtdata_edit = NULL) {
    removeModal()
    do_reparse <- FALSE
    if (is.null(editing_active)) {
        ## not triggered from current editing activity, huh?
        warning("code_make_change entered but editing not active")
    } else if (editing_active %eq% "teams") {
        ## update from all the input$ht_edit_name/id/coach/assistant inputs
        te_ns <- function(id) paste0("team_editor-", id) ## to reference the UI elements in the team_editor module. Note the hard-coding of the 'team_editor' id
        htidx <- which(dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
        dvw$meta$teams$team[htidx] <- input[[te_ns("ht_edit_name")]]
        dvw$meta$teams$team_id[htidx] <- input[[te_ns("ht_edit_id")]]
        dvw$meta$teams$coach[htidx] <- input[[te_ns("ht_edit_coach")]]
        dvw$meta$teams$assistant[htidx] <- input[[te_ns("ht_edit_assistant")]]
        if (!is.null(htdata_edit)) {
            dvw$meta$players_h <- htdata_edit
            dvw$meta$players_h$name <- paste(dvw$meta$players_h$firstname, dvw$meta$players_h$lastname)
        }
        ## and visiting team
        vtidx <- which(dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
        dvw$meta$teams$team[vtidx] <- input[[te_ns("vt_edit_name")]]
        dvw$meta$teams$team_id[vtidx] <- input[[te_ns("vt_edit_id")]]
        dvw$meta$teams$coach[vtidx] <- input[[te_ns("vt_edit_coach")]]
        dvw$meta$teams$assistant[vtidx] <- input[[te_ns("vt_edit_assistant")]]
        if (!is.null(vtdata_edit)) {
            dvw$meta$players_v <- vtdata_edit
            dvw$meta$players_v$name <- paste(dvw$meta$players_v$firstname, dvw$meta$players_v$lastname)
        }
        do_reparse <- TRUE
    } else if (editing_active %eq% "match_data") {
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
        dvw$meta$match$zones_or_cones <- input[[md_ns("match_edit_zones_or_cones")]]
        do_reparse <- TRUE
    } else if (editing_active %eq% "change starting lineup") {
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
        tryCatch({
            dvw <- dv_set_lineups(dvw, set_number = setnum, lineups = list(ht$lineup, vt$lineup), setters = c(ht$setter, vt$setter))
        }, error = function(e) warning(conditionMessage(e)))
        ## TODO, show some useful message to the user that the lineup operation failed
        do_reparse <- TRUE
    } else {
        warning("I don't know what to do with editing_active: ", editing_active)
    }
    ##            if (do_reparse) {
    ##                ## reparse the dvw
    ##                dvw <- reparse_dvw(dvw, dv_read_args = dv_read_args)
    ##                playslist_needs_scroll(TRUE)
    ##            }
    list(dvw = dvw, do_reparse = do_reparse)
}
