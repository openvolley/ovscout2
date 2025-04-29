## Some functions used in the main server code. Defined here to reduce clutter in that file

## getsv() will `get` a server variable
## can be used so that variables defined in the server scope can be used in these functions, which are defined outside of that scope, without having to pass them as function parms. Use sparingly, it's really only to reduce the length of function call code in the server
getsv <- function (varname, fail = TRUE) {
    for (pf in seq(1L, sys.nframe() - 1L, by = 1L)) {
        ## if (exists(varname, envir = parent.frame(n = pf))) return(get(varname, envir = parent.frame(n = pf))) ## this will potentially find the named variable in an environment that isn't the server environment
        if (exists(".am_in_server", envir = parent.frame(n = pf))) return(get(varname, envir = parent.frame(n = pf)))
    }
    if (isTRUE(fail)) stop("variable ", varname, " could not be found")
    NULL
}

## function to populate the team character and player number of the serving player in the scout typing entry box
populate_server <- function(game_state) {
    isolate({
        ## cstr(reactiveValuesToList(game_state))
        srv_code <- if (game_state$serving %eq% "*" && !is.null(game_state$home_p1) && !is.na(game_state$home_p1)) {
                        paste0("*", ldz2(game_state$home_p1))
                    } else if (game_state$serving %eq% "a" && !is.null(game_state$visiting_p1) && !is.na(game_state$visiting_p1)) {
                        paste0("a", ldz2(game_state$visiting_p1))
                    } else {
                        ""
                    }
    })
    focus_to_scout_bar(srv_code)
}

## ---
## rally code functions

get_current_rally_code <- function(playslist_mod, rdata, rally_codes) {
    ## NOTE this won't have the actual scout code unless it was a non-skill code
    ## if you need the code, use codes_from_rc_rows(this)
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

review_rally <- function(editing, app_data, rally_codes) {
    ## codes can be reviewed and edited at the end of the rally
    editing$active <- "rally_review"
    if (app_data$with_video) do_video("pause")
    review_rally_modal(rally_codes())
}

do_cancel_rally_review <- function(editing, app_data) {
    editing$active <- NULL
    removeModal()
    if (app_data$with_video) do_video("play")
    focus_to_scout_bar()
}

apply_rally_review <- function(editing, rally_codes, game_state, input, rdata, app_data) {
    editing$active <- NULL
    removeModal()
    rctxt0 <- codes_from_rc_rows(rally_codes()) ## the codes before review
    ## run the reviewed codes through the interpreter
    home_setter_num <- game_state[[paste0("home_p", game_state$home_setter_position)]]
    visiting_setter_num <- game_state[[paste0("visiting_p", game_state$visiting_setter_position)]]
    rctxt <- lapply(seq_along(rctxt0), function(i) ov_code_interpret(input[[paste0("rcedit_", i)]], attack_table = rdata$options$attack_table, compound_table = rdata$options$compound_table, default_scouting_table = rdata$options$default_scouting_table, home_setter_num = home_setter_num, visiting_setter_num = visiting_setter_num))
    ##cat("edited codes:\n")
    ##cat(str(rctxt))
    ## may now have more (or less) codes than we started with
    smth <- bind_rows(lapply(seq_along(rctxt), function(i) {
        newcode <- rctxt[[i]] ## one or more codes, but some can be empty strings if the review text box was empty
        newcode <- newcode[nzchar(newcode)]
        if (length(newcode) < 1) return(NULL)
        crc <- rally_codes()[i, ][rep(1, length(newcode)), ]
        temp <- bind_rows(parse_code_minimal(newcode)) ## this might fail?
        crc <- bind_cols(crc[, setdiff(names(crc), names(temp))], temp)[, names(crc)]
        crc
    }))
    ## make sure details now match from one skill to the next
    ## no, can't do this sequentially, because if we update e.g. an attack tempo during rally review, that will get overridden by the set tempo that precedes it
    ## would need to do this as codes are edited
    ## smth <- bind_rows(lapply(seq_len(nrow(smth)), function(i) {
    ##     if (i < 2) smth[1, ] else transfer_scout_row_details(from = smth[i - 1, ], to = smth[i, ])
    ## }))
    ## cat(str(smth, max.level = 2))
    rally_codes(smth) ## update
    end_of_set <- rally_ended() ## process
    ## end of point, pre-populate the scout box with the server team and number
    if (!end_of_set) {
        populate_server(game_state)
        if (app_data$with_video) do_video("play")
    }
}

rally_ended <- function() {
    ## this function gets called a lot, so we'll retrieve the parameters using `getsv` rather than having to pass them in every call
    rdata <- getsv("rdata")
    rally_codes <- getsv("rally_codes")
    game_state <- getsv("game_state")
    app_data <- getsv("app_data")
    have_asked_end_of_set <- getsv("have_asked_end_of_set")
    rally_state <- getsv("rally_state")
    ## add rally codes to scout object now
    rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(rally_codes(), game_state = game_state, rally_ended = TRUE, dvw = rdata$dvw)))
    do_rally_end_things(game_state = game_state, app_data = app_data, rdata = rdata, rally_codes = rally_codes, rally_state = rally_state)
    ## check for end of set
    scores <- c(game_state$home_score_start_of_point, game_state$visiting_score_start_of_point)
    end_of_set <- if (app_data$is_beach) {
                      max(scores) >= 21 && abs(diff(scores)) >= 2
                  } else {
                      ((max(scores) >= 25 && game_state$set_number < 5) || (max(scores) >= 15 && game_state$set_number == 5)) && abs(diff(scores)) >= 2
                  }
    if (end_of_set && !have_asked_end_of_set()) {
        show_scout_modal(
               vwModalDialog(title = "End of set", footer = NULL, width = app_data$styling$scout_modal_width, modal_halign = "left",
                             paste0("Confirm end of set ", game_state$set_number, "?"),
                             tags$hr(),
                             fixedRow(column(2, actionButton("end_of_set_cancel", "Cancel", class = "cancel fatradio")),
                                      column(2, offset = 8, actionButton("end_of_set_confirm", "Confirm", class = "continue fatradio")))
                             ), with_review_pane = FALSE)
        do_video("pause")
        rally_state("confirm end of set")
        have_asked_end_of_set(TRUE)
    }
    end_of_set
}

do_rally_end_things <- function(game_state, app_data, rdata, rally_codes, rally_state) {
    pseq <- if (app_data$is_beach) 1:2 else 1:6
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
    game_state$rally_started <- FALSE
    rally_codes(empty_rally_codes)
    game_state$start_x <- game_state$start_y <- game_state$mid_x <- game_state$mid_y <- game_state$end_x <- game_state$end_y <- NA_real_
    game_state$startxy_valid <- game_state$midxy_valid <- game_state$endxy_valid <- FALSE
    game_state$current_time_uuid <- ""
    game_state$point_won_by <- NA_character_
    if (!is.null(app_data$auto_save_dir)) {
        if (!dir.exists(app_data$auto_save_dir) && !app_data$have_warned_auto_save) {
            app_data$have_warned_auto_save <- TRUE
            warning("auto-save dir does not exist, ignoring")
        } else {
            tryCatch({
                save_file_basename <- getsv("save_file_basename")
                temp_dvw_file <- file.path(app_data$auto_save_dir, paste0(save_file_basename(), "-live.dvw"))
                if (file.exists(temp_dvw_file)) unlink(temp_dvw_file)
                dv_write2(update_meta(rp2(rdata$dvw)), file = temp_dvw_file) ## TODO something about convert_cones here
            }, error = function(e) warning("could not auto-save file"))
        }
    }
    rally_state("click serve start")
}


assign_lineup_from_manual <- function(code, rdata, game_state, app_data) {
    lup <- lineup_preprocess(code, beach = is_beach(rdata$dvw)) ## list(home = L, visiting = L) where L is a list with elements lineup, liberos, setter
    ## apply
    setnum <- if (is.null(game_state$set_number) || is.na(game_state$set_number)) {
                  ## assume is set 1, probably needs something better
                  1L
              } else {
                  game_state$set_number
              }
    for (this in c("home", "visiting")) {
        this_lup <- lup[[this]]
        if (!is.null(this_lup)) {
            hv <- if (this == "home") "*" else "a"
            ## TODO if libero is not specified, default to lineup libero? Unless that person is on court?
            for (i in seq_along(this_lup$lineup)) game_state[[paste0(this, "_p", i)]] <- this_lup$lineup[i]
            if (!app_data$is_beach) { ## if beach, not setter or liberos
                temp_sp <- which(this_lup$lineup == this_lup$setter) ## setter position
                ## the liberos go into game_state
                ## if the lineup did not specify liberos, but we have them on the roster, use them
                if (length(this_lup$liberos) < 1) {
                    temp_libs <- get_liberos(game_state = game_state, team = hv, dvw = rdata$dvw) ## liberos from roster
                    this_lup$liberos <- head(sort(setdiff(temp_libs, this_lup$lineup)), 2)
                }
                game_state[[paste0(substr(this, 1, 1), "t_lib1")]] <- if (length(this_lup$liberos) > 0) this_lup$liberos[1] else NA_integer_
                game_state[[paste0(substr(this, 1, 1), "t_lib2")]] <- if (length(this_lup$liberos) > 1) this_lup$liberos[2] else NA_integer_
                game_state[[paste0(this, "_setter_position")]] <- temp_sp
            }
            rdata$dvw <- set_lineup(rdata$dvw, set_number = setnum, team = hv, lineup = c(this_lup$lineup, na.omit(this_lup$liberos))) ## allocate the starting positions for set setnum in meta$players_h or meta$players_v
            if (!app_data$is_beach) {
                lineup_codes <- c(paste0(hv, "P", ldz2(this_lup$setter), ">LUp"), paste0(hv, "z", temp_sp, ">LUp"))
                print(lineup_codes)
                rdata$dvw$plays2 <- rp2(bind_rows(rdata$dvw$plays2, make_plays2(rally_codes = lineup_codes, game_state = game_state, dvw = rdata$dvw)))
            }
        }
    }
}

## ---
## video-related functions

## video function shortcut
do_video <- function(...) {
    do_video_inner(..., video_state = getsv("video_state"), rally_state = getsv("rally_state"), app_data = getsv("app_data"), session = getsv("session"))
}

deal_with_pause <- function(scout_modal_active, video_state, editing, game_state, rdata, app_data, show_modal = TRUE) {
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
                dismiss_admin_modal(editing = editing, scout_mode = app_data$scout_mode)
            }
        } else {
            ## not paused, so pause and show admin modal
            do_video("pause")
            if (show_modal) {
                editing$active <- "admin"
                show_admin_modal(game_state = game_state, dvw = rdata$dvw)
            }
        }
    }
}

## ---
## functions related to dual-video mode

## function to reference a video time measured on the time scale of video "from", to its equivalent time relative to video "to"
rebase_time <- function(t, time_to = 1, time_from, rdata) {
    if (!time_from %in% c(1, 2)) time_from <- 1
    if (!time_to %in% c(1, 2)) time_to <- 1
    if (time_from > 1) t <- t - rdata$dvw$video2_offset
    ## t is now relative to 1
    if (time_to > 1) t <- t + rdata$dvw$video2_offset
    t
}

do_switch_video <- function(have_second_video, current_video_src, rdata, app_data, video_state) {
    if (have_second_video) {
        current_video_src(3L - current_video_src())
        if (current_video_src() == 1L) {
            new_src <- app_data$video_src
            offs <- -rdata$dvw$video2_offset
        } else {
            new_src <- app_data$video_src2
            offs <- rdata$dvw$video2_offset
        }
        new_src <- get_video_source_type(new_src, base_url = app_data$video_server_base_url)
        myjs <- paste0("var ct=vidplayer.currentTime(); ct=ct", if (offs >= 0) "+", offs, "; if (ct >= 0) { vidplayer.src(", if (new_src$type == "youtube") paste0("{ \"type\": \"video/youtube\", \"src\": \"", new_src$src, "\"}") else paste0("\"", new_src$src, "\""), "); vidplayer.currentTime(ct);", if (!video_state$paused) "vidplayer.play(); pause_on_type = ", app_data$pause_on_type, "; Shiny.setInputValue('video_width', vidplayer.videoWidth()); Shiny.setInputValue('video_height', vidplayer.videoHeight()); }")
        dojs(myjs)
    }
}

do_switch_preview <- function(preview_video_src, app_data, rdata) {
    preview_video_src(3L - preview_video_src())
    if (preview_video_src() == 1L) {
        new_src <- app_data$video_src
        offs <- -rdata$dvw$video2_offset
    } else {
        new_src <- app_data$video_src2
        offs <- rdata$dvw$video2_offset
    }
    new_src <- get_video_source_type(new_src, base_url = app_data$video_server_base_url)
    myjs <- paste0("var ct=videojs('video_preview').currentTime(); ct=ct", if (offs >= 0) "+", offs, "; if (ct >= 0) { videojs('video_preview').src(", if (new_src$type == "youtube") paste0("{ \"type\": \"video/youtube\", \"src\": \"", new_src$src, "\"}") else paste0("\"", new_src$src, "\""), "); videojs('video_preview').currentTime(ct); videojs('video_preview').play(); }")
    dojs(myjs)
}
