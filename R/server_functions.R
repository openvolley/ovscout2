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
