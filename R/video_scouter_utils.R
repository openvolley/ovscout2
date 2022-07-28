## a player's most common serve type
## px is a plays object
get_player_serve_type <- function(px, serving_player_num, game_state, opts) {
    if (is.null(px)) return(NA_character_)
    out <- dplyr::select(dplyr::filter(px, .data$skill %eq% "Serve" & .data$team == game_state$serving & .data$player_number %eq% serving_player_num), .data$skill_type)
    ## reverse-map serve description to code, e.g. Jump serve back to "Q"
    chc <- dplyr::filter(opts$skill_tempo_map, .data$skill == "Serve")
    chc <- setNames(chc$tempo_code, chc$tempo)
    out$stype <- do.call(dplyr::recode, c(list(out$skill_type), as.list(chc)))
    ## was previously hard-coded
    ##out <- mutate(out, stype = case_when(.data$skill_type %eq% "Float serve" ~ "H",
    ##                                     .data$skill_type %eq% "Topspin serve" ~ "T",
    ##                                     .data$skill_type %eq% "Jump-float serve" ~ "M",
    ##                                     .data$skill_type %eq% "Jump serve" ~ "Q"))
    out <- dplyr::arrange(dplyr::count(out, .data$stype), desc(.data$n))
    if (nrow(out) > 0) out$stype[1] else NA_character_
}

make_plays2 <- function(rally_codes, game_state, rally_ended = FALSE, dvw) {
    pseq <- seq_len(if (dv_is_beach(dvw)) 2L else 6L)
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (is.data.frame(rally_codes)) {
        if (nrow(rally_codes) > 0) {
            codes <- codes_from_rc_rows(rally_codes)
            start_coord <- dv_xy2index(as.numeric(rally_codes$start_x), as.numeric(rally_codes$start_y))
            mid_coord <- dv_xy2index(as.numeric(rally_codes$mid_x), as.numeric(rally_codes$mid_y))
            end_coord <- dv_xy2index(as.numeric(rally_codes$end_x), as.numeric(rally_codes$end_y))
            vt <- rally_codes$t
            phase <- rep(NA_character_, length(codes))
            rec_tm <- ""
            for (i in seq_along(codes)) {
                if (grepl("^[a\\*][[:digit:]][[:digit:]]S", codes[i])) {
                    phase[i] <- "Serve"
                } else if (grepl("^[a\\*][[:digit:]][[:digit:]]R", codes[i])) {
                    phase[i] <- "Reception"
                    rec_tm <- substr(codes[i], 1, 1)
                } else if (i > 1 && phase[i - 1] %eq% "Reception" && (substr(codes[i], 1, 1) %eq% rec_tm || grepl("^[a\\*][[:digit:]][[:digit:]]B", codes[i]))) {
                    phase[i] <- "Reception"
                } else if (i > 1 && !is.na(phase[i - 1])) {
                    phase[i] <- "Transition"
                }
            }
            rcv <- split(rally_codes, seq_len(nrow(rally_codes)))
        } else {
            codes <- character()
            start_coord <- mid_coord <- end_coord <- vt <- numeric()
            phase <- character()
            rcv <- list()
        }
    } else {
        ## rally_codes are just char, which means that these aren't skill codes, they are auto codes (position codes or similar)
        codes <- rally_codes
        start_coord <- mid_coord <- end_coord <- NA_integer_
        vt <- NA_real_
        phase <- NA_character_
        rcv <- vector("list", length(rally_codes))
    }
    ## nb rcv is the vector of rally_code (tibble rows), one per row
    if (rally_ended) {
        assert_that(game_state$point_won_by %in% c("*", "a"))
        ## add green codes if needed
        ## add the [*a]pXX:YY code at the end of the rally
        scores_end_of_point <- c(game_state$home_score_start_of_point, game_state$visiting_score_start_of_point) + as.integer(c(game_state$point_won_by == "*", game_state$point_won_by == "a"))
        pcode <- paste0(game_state$point_won_by, "p", sprintf("%02d:%02d", scores_end_of_point[1], scores_end_of_point[2]))
        codes <- make_auto_codes(c(codes, pcode), dvw)
        if (length(start_coord) > 1 || !is.na(start_coord)) {
            ## we've added entries to code, so we need to add dummy (NA) coord and video time values as well
            n_extra <- length(codes) - nrow(rally_codes)
            start_coord <- c(start_coord, rep(NA_integer_, n_extra))
            mid_coord <- c(mid_coord, rep(NA_integer_, n_extra))
            end_coord <- c(end_coord, rep(NA_integer_, n_extra))
            vt <- c(vt, rep(NA_real_, n_extra))
            phase <- c(phase, rep(NA_character_, n_extra))
            rcv <- c(rcv, vector("list", n_extra))
        }
    }
    out <- tibble(code = codes, ## col 1
                  point_phase = NA_character_, ## col 2
                  attack_phase = NA_character_, ## col 3
##                  X4 = NA, ## col 4
                  start_coordinate = start_coord, mid_coordinate = mid_coord, end_coordinate = end_coord, ## cols 5-7
                  time = time_but_utc(), ## col 8
                  set_number = game_state$set_number, home_setter_position = game_state$home_setter_position, visiting_setter_position = game_state$visiting_setter_position, ## cols 9-11, NB these 3 not used directly in dv_read
                  video_file_number = NA, video_time = vt, ## cols 12-13
  ##                X14 = NA, ## col 14
                  home_score_start_of_point = game_state$home_score_start_of_point,
                  visiting_score_start_of_point = game_state$visiting_score_start_of_point,
                  serving = game_state$serving)
    out <- bind_cols(out, as.data.frame(game_state[paste0("home_p", pseq)]))
    out <- bind_cols(out, as.data.frame(game_state[paste0("visiting_p", pseq)]))
    for (lib in c(paste0("ht_lib", 1:2), paste0("vt_lib", 1:2))) out[[lib]] <- if (!lib %in% names(game_state)) NA_integer_ else game_state[[lib]]
    out$phase <- phase
    out$rally_codes <- rcv
    out
}

## rationalize plays2 rows
rp2 <- function(p2) {
    if (is.null(p2)) return(p2)
    ## input can either be a whole dvw object, or just the plays2 component thereof
    was_dvw <- is.list(p2) && "plays2" %in% names(p2)
    if (was_dvw) {
        dvw <- p2
        p2 <- dvw$plays2
    }
    if (nrow(p2) < 1) return(if (was_dvw) dvw else p2)
    ## strip out redundant codes
    ## if we have multiple az or *z codes (setter locations) without other intervening codes (other than *P or aP, which designate the setter on court) then just take the last of each
    ok <- rep(TRUE, nrow(p2))
    home_sl <- FALSE; visiting_sl <- FALSE; home_snum <- FALSE; visiting_snum <- FALSE
    is_home_sl <- grepl("^\\*z", p2$code)
    is_visiting_sl <- grepl("^az", p2$code)
    is_home_snum <- grepl("^\\*P", p2$code)
    is_visiting_snum <- grepl("^aP", p2$code)
    ## go backwards through codes. For each block of these codes, keep the last of each that we see (which will be the first that we encounter going backwards)
    for (i in rev(seq_len(nrow(p2)))) {
        if (!is_visiting_sl[i] && !is_home_sl[i] && !is_home_snum[i] && !is_visiting_snum[i]) {
            home_sl <- visiting_sl <- home_snum <- visiting_snum <- FALSE
        } else if (is_home_sl[i]) {
            if (!home_sl) home_sl <- TRUE else ok[i] <- FALSE
        } else if (is_visiting_sl[i]) {
            if (!visiting_sl) visiting_sl <- TRUE else ok[i] <- FALSE
        } else if (is_home_snum[i]) {
            if (!home_snum) home_snum <- TRUE else ok[i] <- FALSE
        } else if (is_visiting_snum[i]) {
            if (!visiting_snum) visiting_snum <- TRUE else ok[i] <- FALSE
        }
    }
    ## similarly, can't have repeated **1set etc codes
    notok <- grepl("^\\*\\*[[:digit:]]set$", p2$code) & duplicated(p2$code, fromLast = TRUE) ## fromLast => keep the last of each
    p2 <- p2[ok & !notok, ]
    if (was_dvw) {
        dvw$plays2 <- p2
        dvw
    } else {
        p2
    }
}

game_state_make_substitution <- function(game_state, team, player_out, player_in, dvw) {
    team <- match.arg(team, c("*", "a"))
    pseq <- seq_len(if (dv_is_beach(dvw)) 2L else 6L)
    lup_cols <- if (team == "*") paste0("home_p", pseq) else paste0("visiting_p", pseq)
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    this_lup <- as.numeric(game_state[lup_cols])
    available_players <- if (team == "*") dvw$meta$players_h$number else dvw$meta$players_v$number
    available_players <- na.omit(available_players)
    if (!player_out %in% this_lup) {
        message("player being subbed out is not on court, ignoring sub")
    } else if (player_in %in% this_lup) {
        message("player being subbed in is already on court, ignoring sub")
    } else if (!player_in %in% available_players) {
        message("player being subbed in is not in player list, ignoring sub")
    } else {
        this_lup[this_lup == player_out] <- player_in
        for (i in seq_along(lup_cols)) game_state[[lup_cols[i]]] <- this_lup[i]
        message(if (team == "*") "home" else "visiting", " team player ", player_in, " in for player ", player_out)
    }
    game_state
}

plays2_to_plays <- function(plays2, dvw, evaluation_decoder) {
    pseq <- seq_len(if (is_beach(dvw)) 2L else 6L)
    if (is.null(plays2) || nrow(plays2) < 1) {
        out <- tibble(code = character(), skill = character(), home_setter_position = integer(), visiting_setter_position = integer(), home_team_score = integer(), visiting_team_score = integer(), phase = character(), set_number = integer(), video_time = numeric(), error_icon = character(), start_coordinate_x = numeric(), start_coordinate_y = numeric(), mid_coordinate_x = numeric(), mid_coordinate_y = numeric(), end_coordinate_x = numeric(), end_coordinate_y = numeric())
        for (pn in pseq) {
            out[[paste0("home_p", pn)]] <- integer()
            out[[paste0("visiting_p", pn)]] <- integer()
        }
        return(out)
    }
    out <- datavolley:::parse_code(plays2$code, meta = dvw$meta, evaluation_decoder = evaluation_decoder, file_type = if (is_beach(dvw)) "beach" else "indoor")$plays
    out$home_setter_position <- plays2$home_setter_position
    out$visiting_setter_position <- plays2$visiting_setter_position
    out$home_team_score <- plays2$home_score_start_of_point ## TODO FIX
    out$visiting_team_score <- plays2$visiting_score_start_of_point ## TODO FIX
  ##code team player_number player_name player_id skill skill_type evaluation_code evaluation attack_code
  ##attack_description set_code set_description set_type start_zone end_zone end_subzone end_cone skill_subtype num_players
  ##num_players_numeric special_code timeout end_of_set substitution point home_team_score visiting_team_score
  ##home_setter_position visiting_setter_position custom_code file_line_number
    out <- bind_cols(out, setNames(dv_index2xy(plays2$start_coordinate), c("start_coordinate_x", "start_coordinate_y")))
    out <- bind_cols(out, setNames(dv_index2xy(plays2$mid_coordinate), c("mid_coordinate_x", "mid_coordinate_y")))
    out <- bind_cols(out, setNames(dv_index2xy(plays2$end_coordinate), c("end_coordinate_x", "end_coordinate_y")))
    out$phase <- plays2$phase ##datavolley::play_phase(out)
    out$set_number <- plays2$set_number
    out$video_time <- plays2$video_time
    ## add point_id
    pid <- 0
    temp_point_id <- rep(NA, nrow(out))
    temp_point_id[1] <- pid
    temp_timeout <- rep(FALSE, nrow(out))##out$timeout
    if (nrow(out) > 1) {
        for (k in 2:nrow(out)) {
            if (isTRUE(out$point[k-1]) || temp_timeout[k] || temp_timeout[k-1]) pid <- pid+1
            temp_point_id[k] <- pid
        }
    }
    out$point_id <- temp_point_id
    ## serving team
    who_served <- dplyr::distinct(dplyr::filter(out, .data$skill == "Serve"), .data$point_id, .data$team) %>%
        dplyr::rename(serving_team = "team") %>% dplyr::filter(!duplicated(.data$point_id))
    out <- left_join(out, who_served, by = c("point_id"))
    out$serving_team <- as.character(out$serving_team) ## to be sure is not factor
    out$error_icon <- ""##ifelse(is.na(x$plays$error_message), "", HTML(as.character(shiny::icon("exclamation-triangle"))))
    bind_cols(out, plays2[, c(paste0("home_p", pseq), paste0("visiting_p", pseq))])
}

is_skill <- function(z) !is.na(z) & (!z %in% c("Timeout", "Technical timeout", "Substitution"))
is_beach <- function(dvw) isTRUE(grepl("beach", dvw$meta$match$regulation))
zpn <- function(n) sprintf("%02d", as.numeric(n))
other <- function(tm) { oth <- rep(NA_character_, length(tm)); oth[tm %eq% "*"] <- "a"; oth[tm %eq% "a"] <- "*"; oth }
##other <- function(tm) c("a", "*")[as.numeric(factor(tm, levels = c("*", "a")))]

empty_rally_codes <- tibble(team = character(), pnum = character(), skill = character(), tempo = character(), eval = character(), combo = character(), target = character(), sz = character(), ez = character(), esz = character(), x_type = character(), num_p = character(), special = character(), custom = character(), code = character(), t = numeric(), start_x = numeric(), start_y = numeric(), mid_x = numeric(), mid_y = numeric(), end_x = numeric(), end_y = numeric(), rally_state = character(), game_state = list(), current_team = character())

print_rally_codes <- function(rc) {
    tr <- function(z) tryCatch(round(z, 1), error = function(e) z)
    rc$code <- codes_from_rc_rows(rc)
    do.call(cat, c(lapply(seq_len(nrow(rc)), function(i) paste0(rc$code[i], ", t=", tr(rc$t[i]), ", start x=", tr(rc$start_x[i]), ", y=", tr(rc$start_y[i]), ", mid x=", tr(rc$mid_x[i]), ", y=", tr(rc$mid_y[i]), ", end x=", tr(rc$end_x[i]), ", y=", tr(rc$end_y[i]))), list(sep = "\n")))
}
codes_from_rc_rows <- function(rc) {
    if (!"code" %in% names(rc)) rc$code <- NA_character_
    out <- rc$code
    idx <- is.na(rc$code) | !nzchar(rc$code) ## not-already-provided codes
    if (any(idx)) {
        rc <- rc[idx, ]
        out[idx] <- sub("~+$", "", paste0(rc$team, rc$pnum, rc$skill, rc$tempo, rc$eval, rc$combo, rc$target, rc$sz, rc$ez, rc$esz, rc$x_type, rc$num_p, rc$special, rc$custom))
    }
    out
}

## the code parm here can be used to provide a direct scout code, useful if e.g. the tibble row isn't a scouted skill
code_trow <- function(team, pnum = 0L, skill, tempo, eval, combo = "~~", target = "~", sz = "~", ez = "~", esz = "~", x_type = "~", num_p = "~", special = "~", custom = "", t = NA_real_, start_x = NA_real_, start_y = NA_real_, mid_x = NA_real_, mid_y = NA_real_, end_x = NA_real_, end_y = NA_real_, code = NA_character_, rally_state, startxy_valid, midxy_valid, endxy_valid, game_state, default_scouting_table) {
    ## abbreviated parameter names here to make code more concise: pnum = player number, eval = evaluation code, sz = start zone, ez = end zone, esz = end subzone, x_type = extended skill type code, num_p = extended num players code, special = extended special code
    ## providing 'code' is a special case
    na2t <- function(z, width = 1) if (is.na(z)) { if (width == 1) "~" else paste0(rep("~", width), collapse = "") } else z
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (missing(startxy_valid)) startxy_valid <- game_state$startxy_valid
    if (missing(midxy_valid)) midxy_valid <- game_state$midxy_valid
    if (missing(endxy_valid)) endxy_valid <- game_state$endxy_valid
    if (!isTRUE(startxy_valid)) {
        start_x <- start_y <- NA_real_
        sz <- "~"
    }
    if (!isTRUE(midxy_valid)) {
        mid_x <- mid_y <- NA_real_
    }
    if (!isTRUE(endxy_valid)) {
        end_x <- end_y <- NA_real_
        ez <- esz <- "~"
    }
    if (!is.na(code)) {
        NAc <- NA_character_
        tibble(team = NAc, pnum = NAc, skill = NAc, tempo = NAc, eval = NAc, combo = NAc, target = NAc, sz = NAc, ez = NAc, esz = NAc, x_type = NAc, num_p = NAc, special = NAc, custom = NAc, code = code, t = t, start_x = start_x, start_y = start_y, mid_x = mid_x, mid_y = mid_y, end_x = end_x, end_y = end_y, rally_state = rally_state, game_state = list(game_state), current_team = game_state$current_team)
    } else {
        if (missing(tempo) || is.null(tempo) || tempo %eq% "~" || is.na(tempo)) tempo <- tryCatch(default_scouting_table$tempo[default_scouting_table$skill == skill], error = function(e) "~")
       if (missing(eval) || is.null(eval) || eval %eq% "~" || is.na(eval)) eval <- tryCatch(default_scouting_table$evaluation_code[default_scouting_table$skill == skill], error = function(e) "~")
        if ((missing(x_type) || is.null(x_type) ||x_type %eq% "~" || is.na(x_type)) && skill %eq% "A") x_type <- "H" ## default to hard hit
        if (is.null(pnum) || is.null(pnum) || is.na(pnum) || pnum %eq% "Unknown") pnum <- 0L
        as_tibble(c(lapply(list(team = team, pnum = zpn(pnum), skill = skill, tempo = tempo, eval = eval, combo = na2t(combo, 2), target = na2t(target), sz = na2t(sz), ez = na2t(ez), esz = na2t(esz), x_type = na2t(x_type), num_p = na2t(num_p), special = na2t(special), custom = if (is.na(custom)) "" else custom), as.character), list(code = code, t = t, start_x = start_x, start_y = start_y, mid_x = mid_x, mid_y = mid_y, end_x = end_x, end_y = end_y, rally_state = rally_state, game_state = list(game_state), current_team = game_state$current_team)))
    }
}

update_code_trow <- function(trow, team, pnum, skill, tempo, eval, combo, target, sz, ez, esz, x_type, num_p, special, custom, code, t, start_x, start_y, mid_x, mid_y, end_x, end_y, game_state) {
    ## the only things we take from the input game_state parm are the *xy_valid entries
    ## start/end positions passed in here will be ignored if the corresponding *xy_valid entry is FALSE
    new_ez <- if (!missing(ez) && isTRUE(game_state$endxy_valid)) ez else trow$ez
    new_esz <- trow$esz
    if (!missing(esz) && isTRUE(game_state$endxy_valid)) {
        if (nchar(esz) == 2) {
            new_ez <- substr(esz, 1, 1)
            new_esz <- substr(esz, 2, 2)
        } else {
            new_esz <- if (nchar(esz) > 0) substr(esz, 1, 1) else "~" ## first char only
        }
    }
    ## the existing game_state gets re-used, but update the *xy_valid entries
    gs <- trow$game_state[[1]]
    if (!missing(sz) || !missing(start_x) || !missing(start_y)) gs$startxy_valid <- game_state$startxy_valid
    if (!missing(mid_x) || !missing(mid_y)) gs$midxy_valid <- game_state$midxy_valid
    if (!missing(ez) || !missing(esz) || !missing(end_x) || !missing(end_y)) gs$endxy_valid <- game_state$endxy_valid
    code_trow(team = if (!missing(team)) team else trow$team,
              pnum = if (!missing(pnum)) pnum else trow$pnum,
              skill = if (!missing(skill)) skill else trow$skill,
              tempo = if (!missing(tempo)) tempo else trow$tempo,
              eval = if (!missing(eval)) eval else trow$eval,
              combo = if (!missing(combo)) combo else trow$combo,
              target = if (!missing(target)) target else trow$target,
              sz = if (!missing(sz) && isTRUE(game_state$startxy_valid)) sz else trow$sz,
              ez = new_ez,
              esz = new_esz,
              x_type = if (!missing(x_type)) x_type else trow$x_type,
              num_p = if (!missing(num_p)) num_p else trow$num_p,
              special = if (!missing(special)) special else trow$special,
              custom = if (!missing(custom)) custom else trow$custom,
              code = if (!missing(code)) code else trow$code,
              t = if (!missing(t)) t else trow$t,
              start_x = if (!missing(start_x) && isTRUE(game_state$startxy_valid)) start_x else trow$start_x,
              start_y = if (!missing(start_y) && isTRUE(game_state$startxy_valid)) start_y else trow$start_y,
              mid_x = if (!missing(mid_x) && isTRUE(game_state$midxy_valid)) mid_x else trow$mid_x,
              mid_y = if (!missing(mid_y) && isTRUE(game_state$midxy_valid)) mid_y else trow$mid_y,
              end_x = if (!missing(end_x) && isTRUE(game_state$endxy_valid)) end_x else trow$end_x,
              end_y = if (!missing(end_y) && isTRUE(game_state$endxy_valid)) end_y else trow$end_y,
              rally_state = trow$rally_state,
              game_state = gs)
}

get_setter_pos <- function(game_state, team) {
    if (missing(team)) team <- game_state$current_team
    if (team == "*") game_state$home_setter_position else if (team == "a") game_state$visiting_setter_position else NA_integer_
}

get_setter <- function(game_state, team) {
    pseq <- seq_len(6L) ## indoor only
    if (missing(team)) team <- game_state$current_team
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (team == "*") {
        tryCatch(as.numeric(game_state[paste0("home_p", pseq)])[game_state$home_setter_position], error = function(e) 0L)
    } else if (team == "a") {
        tryCatch(as.numeric(game_state[paste0("visiting_p", pseq)])[game_state$visiting_setter_position], error = function(e) 0L)
    } else {
        0L
    }
}

get_liberos <- function(game_state, team, dvw) {
    if (missing(team)) team <- game_state$current_team
    if (is_beach(dvw)) {
        c()
    } else {
        ## liberos can be specified in game_state, which should be the actual liberos (possibly changing from set to set)
        ## if those aren't specified, we take the liberos from the player lists
        if (team == "*") {
            if (!all(paste0("ht_lib", 1:2) %in% names(game_state)) || (is.na(game_state$ht_lib1) && is.na(game_state$ht_lib2))) {
                na.omit(dvw$meta$players_h$number[dvw$meta$players_h$special_role %eq% "L"])
            } else {
                out <- c(game_state$ht_lib1, game_state$ht_lib2)
                ## note that -1 means no libero used
                out[!is.na(out) & out >= 0]
            }
        } else if (team == "a") {
            if (!all(paste0("vt_lib", 1:2) %in% names(game_state)) || (is.na(game_state$vt_lib1) && is.na(game_state$vt_lib2))) {
                na.omit(dvw$meta$players_v$number[dvw$meta$players_v$special_role %eq% "L"])
            } else {
                out <- c(game_state$vt_lib1, game_state$vt_lib2)
                out[!is.na(out) & out >= 0]
            }
        } else {
            c()
        }
    }
}

get_players <- function(game_state, team, dvw) {
    pseq <- seq_len(if (is_beach(dvw)) 2L else 6L)
    if (missing(team)) team <- game_state$current_team
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (team == "*") {
        tryCatch(as.numeric(game_state[paste0("home_p", pseq)]), error = function(e) rep(NA_integer_, length(pseq)))
    } else if (team == "a") {
        tryCatch(as.numeric(game_state[paste0("visiting_p", pseq)]), error = function(e) rep(NA_integer_, length(pseq)))
    } else {
        rep(NA_integer_, length(pseq))
    }
}

player_nums_to <- function(nums, team, dvw, to = "number lastname") {
    to <- match.arg(to, c("name", "number lastname"))
    temp_players <- if (team == "*") dvw$meta$players_h else if (team == "a") dvw$meta$players_v else stop("team should be '*' or 'a'")
    temp_players <- temp_players %>% dplyr::filter(!is.na(.data$number))
    temp <- left_join(tibble(number = nums), temp_players[, c("number", if (to == "name") "name", if (to == "number lastname") "lastname", "special_role")], by = "number")
    if (to == "name") {
        temp$name[is.na(temp$name)] <- ""
        temp$name
    } else if (to == "number lastname") {
        temp$lastname[is.na(temp$lastname)] <- ""
        temp$lastname[temp$special_role %eq% "L"] <- paste0(temp$lastname[temp$special_role %eq% "L"], " (L)")
        paste(temp$number, temp$lastname, sep = "<br />")
    } else {
        character()
    }
}

## return a list of the possible passing players, and our guess of the most likely passer
## First, we identify the rotation, and check if passing has occured in the past from a serve at that location.
## If yes, the passer (currently on court) with the most receptions will be proposed in priority.
## If no, we assume S-H-M rotation, and articulate the 3-player passing line with each taking left-middle-right channel.
guess_pass_player_options <- function(game_state, dvw, system) {
    beach <- is_beach(dvw)
    pseq <- seq_len(if (beach) 2L else 6L)
    if (!game_state$serving %in% c("*", "a")) return(list(choices = numeric(), selected = c()))

    passing_team <- other(game_state$serving)
    home_visiting <- if (game_state$serving %eq% "*") "visiting" else "home"
    passing_rot <- game_state[[paste0(home_visiting, "_setter_position")]]
    passing_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
    libs <- get_liberos(game_state, team = passing_team, dvw = dvw)

    ## Define the prior probability of passing given rotation, passing zone, etc... Defined as a simple mean of beta().
    passing_responsibility <- player_responsibility_fn(system = system, skill = "Reception", setter_position = passing_rot, zone = passing_zone, libs = libs, home_visiting = home_visiting)

    passing_responsibility_prior <- setNames(rep(0, length(pseq) + 1L), c(paste0(home_visiting, "_p", pseq), "libero"))
    if (!is.na(passing_responsibility)) passing_responsibility_prior[passing_responsibility] <- 1

    ## Update the probability with the history of the game
    passing_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Reception",
                                     .data[[paste0(home_visiting, "_setter_position")]] %eq% as.character(passing_rot),
                                     .data$end_zone %eq% passing_zone,
                                     .data$team %eq% passing_team)

    passing_responsibility_posterior <- passing_responsibility_prior
    if (nrow(passing_history) > 0) {
        passing_history <- dplyr::select(passing_history, "team", "player_number", paste0(home_visiting, "_p", pseq)) %>%
            tidyr::pivot_longer(cols = paste0(home_visiting, "_p", pseq)) %>%
            dplyr::filter(.data$value %eq% .data$player_number) %>%
            dplyr::group_by(.data$name) %>%
            dplyr::summarise(n_reception = dplyr::n()) %>% dplyr::ungroup()
        passing_responsibility_posterior[passing_history$name] <- passing_responsibility_prior[passing_history$name] + passing_history$n_reception
        passing_responsibility_posterior <-  passing_responsibility_posterior / sum(passing_responsibility_posterior)
    }
    plsel_tmp <- names(sort(passing_responsibility_posterior, decreasing = TRUE))
    poc <- paste0(home_visiting, "_p", pseq) ## players on court

    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    pp <- c(sort(as.numeric(game_state[poc])), sort(libs))
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(game_state[plsel_tmp[1]])
    list(choices = pp, selected = plsel)
}

guess_pass_quality <- function(game_state, dvw) {
    if (is_beach(dvw)) {
        ## TODO
        message("beach, defaulting to '+' pass quality")
        return("+")
    }
    ## reference clicks to lower court
    do_flip_click <- (game_state$current_team == "*" && game_state$home_team_end == "upper") || (game_state$current_team == "a" && game_state$home_team_end == "lower")
    thisxy <- if (do_flip_click) as.numeric(dv_flip_xy(game_state$start_x, game_state$start_y)) else c(game_state$start_x, game_state$start_y)
    ## sets use "start" coordinates but "end" zone/subzone
    esz <- paste(dv_xy2subzone(game_state$start_x, game_state$start_y), collapse = "")
    ## TODO account for time since previous contact, and better boundaries here
    out <- if (thisxy[2] > 3.5) {
        "/" ## overpass
    } else if (thisxy[2] >= 3 & thisxy[1] >= 2  & thisxy[1] <= 3) {
        "#"
    } else if (thisxy[2] >= 2.75 & thisxy[1] >= 1.75  & thisxy[1] <= 3.1) {
        "+"
    } else if (thisxy[2] > 2.25 & thisxy[1] >= 1 & thisxy[1] <= 3.5) {
        "!"
    } else {
        "-"
    }
    ##cat("guessed pass quality: ", out, "\n")
    out
}

## return a list of the possible attacking players, and our guess of the most likely attacker
## First, we identify the rotation, and check if attacking has occurred in the past from that location / rotation.
## If yes, the attacker (currently on court) with the most attacks will be proposed in priority.
## If no, we assume S-H-M rotation, and articulate the 4-player attacking system (front-left, front-middle, front-or-back-right, back-middle).
guess_attack_player_options <- function(game_state, dvw, system) {
    beach <- is_beach(dvw)
    pseq <- seq_len(if (beach) 2L else 6L)
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (beach) {
        warning("guess_attack_player_options for beach not yet coded")
        ## placeholder code
        if (game_state$current_team %eq% "*") {
            poc <- paste0("home_p", pseq)
        } else {
            poc <- paste0("visiting_p", pseq)
        }
        pp <- as.numeric(game_state[poc])
        plsel <- as.numeric(game_state[poc[1]])
        return(list(choices = pp, selected = plsel))
    }
    ## else for indoor
    if (!game_state$current_team %in% c("*", "a")) return(list(choices = numeric(), selected = c()))
    attacking_team <- game_state$current_team
    home_visiting <- if (game_state$current_team %eq% "*") "home" else "visiting"
    setter_rot <- game_state[[paste0(home_visiting, "_setter_position")]]
    this_y <- game_state$start_y
    ## adjust the y-coordinate: back row attacks are likely to be clicked just in front of the 3m line, and dv_xy2zone will assign them a front-row zone
    ##cat("attack y was: ", this_y)
    y_margin <- 0.33 ## in court units, so 0.33 * 3 = about 1m in real court coordinates
    if (this_y < (2.5 + y_margin)) this_y <- this_y - y_margin else if (this_y > (4.5 - y_margin)) this_y <- this_y + y_margin
    ##cat(", now:", this_y, "\n")
    attacking_zone <- dv_xy2zone(game_state$start_x, this_y)
    ##cat("attack zone: ", attacking_zone, "\n")
    ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
    attacking_responsibility <- player_responsibility_fn(system = system, skill = "Attack", setter_position = setter_rot, zone = attacking_zone, libs = NULL, home_visiting = home_visiting, serving = game_state$serving %eq% attacking_team)

    attacking_responsibility_prior <- setNames(rep(0, length(pseq)), c(paste0(home_visiting, "_p", pseq)))
    if (!is.na(attacking_responsibility)) attacking_responsibility_prior[attacking_responsibility] <- 1

    ## Update the probability with the history of the game
    ##  this also needs to account for the attacking team being in sideout/breakpoint
    attacking_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Attack" & .data[[paste0(home_visiting, "_setter_position")]] %eq% as.character(setter_rot) &
                                                  .data$start_zone %eq% attacking_zone & .data$team %eq% attacking_team & .data$serving_team == game_state$serving)

    attacking_responsibility_posterior <- attacking_responsibility_prior
    if (nrow(attacking_history) > 0) {
        attacking_history <- dplyr::select(attacking_history, "team", "player_number", paste0(home_visiting, "_p", pseq)) %>% tidyr::pivot_longer(cols = paste0(home_visiting, "_p", pseq)) %>%
            dplyr::filter(.data$value %eq% .data$player_number) %>%
            dplyr::group_by(.data$name) %>% dplyr::summarize(n_attacks = dplyr::n()) %>% dplyr::ungroup()
        attacking_responsibility_posterior[attacking_history$name] <- attacking_responsibility_prior[attacking_history$name] + attacking_history$n_attacks
        attacking_responsibility_posterior <-  attacking_responsibility_posterior / sum(attacking_responsibility_posterior)
    }
    poc <- names(sort(attacking_responsibility_posterior, decreasing = TRUE))
    pp <- sort(as.numeric(game_state[poc]))
    plsel <- as.numeric(game_state[poc[1]])
    list(choices = pp, selected = plsel)
}

guess_attack_code <- function(game_state, dvw, opts) {
    exclude_codes <- if (!missing(opts) && !is.null(opts$setter_dump_code)) opts$setter_dump_code else "PP"
    exclude_codes <- c(exclude_codes, if (!missing(opts) && !is.null(opts$second_ball_attack_code)) opts$second_ball_attack_code else "P2")
    exclude_codes <- c(exclude_codes, if (!missing(opts) && !is.null(opts$overpass_attack_code)) opts$overpass_attack_code else "PR")
    atbl <- dvw$meta$attacks %>% dplyr::filter(!.data$code %in% exclude_codes)
    do_flip_click <- (game_state$current_team == "*" && game_state$home_team_end == "upper") || (game_state$current_team == "a" && game_state$home_team_end == "lower")
    thisxy <- if (do_flip_click) as.numeric(dv_flip_xy(game_state$start_x, game_state$start_y)) else c(game_state$start_x, game_state$start_y)
    ## the start location in the attack table is a bit in front of the 3m line for back-row attacks
    ## shift our y-location forwards a bit to reduce risk of our front-row click looking like it's nearest to a back-row location
    ## TODO, better solution than this
    thisxy[2] <- thisxy[2] + 0.33 ## 0.33 = about 1m in real court space. See same adjustment in guess_attack_player_options
    d <- sqrt((atbl$start_x - thisxy[1])^2 + (atbl$start_y - thisxy[2])^2)
    ## if setter is back row, slides are unlikely
    ## TODO what happens with beach?
    if (get_setter_pos(game_state) %in% c(5, 6, 1)) {
        d[grepl("Slide ", atbl$description)] <- d[grepl("Slide ", atbl$description)] + 0.5 ## penalty. Hard-coding "Slide" is not great, could also look for type = "N" but some scouts use this for non-slides. TODO FIX
        ## also back-row right side is less likely than front-row right side if setter is back row
        d[atbl$attacker_position %eq% 9] <- d[atbl$attacker_position %eq% 9] + 0.5
    }
    ## TODO also incorporate set -> attack contact time and distance, to infer tempo
    d[is.na(d)] <- Inf
    ac <- head(atbl$code[order(d)], 5)
    for (i in head(seq_along(ac), -1)) {
        ## swap e.g. V5 X5 so that X5 is first
        if (substr(ac[i], 2, 2) == substr(ac[i+1], 2, 2) && grepl("^V", ac[i]) && grepl("^X", ac[i+1])) ac[c(i, i+1)] <- ac[c(i+1, i)]
    }
    ac
}



## return a list of the possible digging players, and our guess of the most likely defender
## First, we identify the rotation, and check if defense has occurred in the past from that location / rotation & opp attack
## If yes, the defender (currently on court) with the most digs will be proposed in priority.
## If no, we assume S-H-M rotation, and articulate the perimeter defense system.
guess_dig_player_options <- function(game_state, dvw, system) {
    beach <- is_beach(dvw)
    pseq <- seq_len(if (beach) 2L else 6L)
    if (beach) {
        warning("guess_dig_player_options for beach not yet tested")
    }
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (!game_state$current_team %in% c("*", "a")) return(list(choices = numeric(), selected = c()))
    defending_team <- game_state$current_team
    home_visiting <- if (game_state$current_team %eq% "a") "visiting" else "home"
    setter_rot <- game_state[[paste0(home_visiting, "_setter_position")]]
    attacking_zone <- dv_xy2zone(game_state$start_x, game_state$start_y)
    defending_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
    libs <- get_liberos(game_state, team = defending_team, dvw = dvw)
    if (game_state$serving %eq% defending_team) libs <- rev(libs) ## rev here so that if we have two liberos, the second is preferred in breakpoint phase

    ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
    dig_responsibility <- player_responsibility_fn(system = system, skill = "Dig", setter_position = setter_rot, zone = defending_zone, libs = libs,
                                                   home_visiting = home_visiting, opp_attack_start_zone = attacking_zone, serving = game_state$serving %eq% defending_team)

    dig_responsibility_prior <- setNames(rep(0, length(pseq)), c(paste0(home_visiting, "_p", pseq)))
    if (!is.na(dig_responsibility)) dig_responsibility_prior[dig_responsibility] <- 1

    ## Update the probability with the history of the game
    digging_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Dig",
                                     .data[[paste0(home_visiting, "_setter_position")]] %eq% as.character(setter_rot),
                                     .data$start_zone %eq% attacking_zone,
                                     .data$end_zone %eq% defending_zone,
                                     .data$team %eq% defending_team)

    dig_responsibility_posterior <- dig_responsibility_prior
    if (nrow(digging_history) > 0) {
        digging_history <- dplyr::select(digging_history, "team", "player_number", paste0(home_visiting, "_p", pseq)) %>%
            tidyr::pivot_longer(cols = paste0(home_visiting, "_p", pseq)) %>%
            dplyr::filter(.data$value %eq% .data$player_number) %>%
            dplyr::group_by(.data$name) %>%
            dplyr::summarise(n_digs = dplyr::n()) %>% dplyr::ungroup()
        dig_responsibility_posterior[digging_history$name] <- dig_responsibility_prior[digging_history$name] + digging_history$n_digs
        dig_responsibility_posterior <-  dig_responsibility_posterior / sum(dig_responsibility_posterior)
    }
    plsel_tmp <- names(sort(dig_responsibility_posterior, decreasing = TRUE))
    poc <- paste0(home_visiting, "_p", pseq)
    pp <- c(sort(as.numeric(game_state[poc])), sort(libs))
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(game_state[plsel_tmp[1]])
    list(choices = pp, selected = plsel)
}

guess_cover_player_options <- function(game_state, dvw, system) {
    beach <- is_beach(dvw)
    pseq <- seq_len(if (beach) 2L else 6L)
    if (beach) {
        warning("guess_cover_player_options for beach not yet tested")
    }
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (!game_state$current_team %in% c("*", "a")) return(list(choices = numeric(), selected = c()))
    attacking_team <- other(game_state$current_team)
    ## e.g. if current (defending) team is "*", attacking team is "a"
    home_visiting <- if (game_state$current_team %eq% "*") "visiting" else "home"
    setter_rot <- game_state[[paste0(home_visiting, "_setter_position")]]
    attacking_zone <- dv_xy2zone(game_state$start_x, game_state$start_y)
    defending_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
    libs <- get_liberos(game_state, team = attacking_team, dvw = dvw)
    if (game_state$serving %eq% attacking_team) libs <- rev(libs) ## rev here so that if we have two liberos, the second is preferred in breakpoint phase

    ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
    dig_responsibility <- player_responsibility_fn(system = system, skill = "Cover", setter_position = setter_rot, zone = defending_zone, libs = libs,
                                                   home_visiting = home_visiting, opp_attack_start_zone = attacking_zone, serving = game_state$serving %eq% attacking_team)

    dig_responsibility_prior <- setNames(rep(0, length(pseq)), c(paste0(home_visiting, "_p", pseq)))
    if (!is.na(dig_responsibility)) dig_responsibility_prior[dig_responsibility] <- 1

    ## Update the probability with the history of the game
    digging_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Dig" & lag(.data$skill) %eq% "Block" & !.data$team %eq% lag(.data$team), ## just cover digs
                                     .data[[paste0(home_visiting, "_setter_position")]] %eq% as.character(setter_rot),
                                     .data$start_zone %eq% attacking_zone,
                                     .data$end_zone %eq% defending_zone,
                                     .data$team %eq% attacking_team)

    dig_responsibility_posterior <- dig_responsibility_prior
    if (nrow(digging_history) > 0) {
        digging_history <- dplyr::select(digging_history, "team", "player_number", paste0(home_visiting, "_p", pseq)) %>%
            tidyr::pivot_longer(cols = paste0(home_visiting, "_p", pseq)) %>%
            dplyr::filter(.data$value %eq% .data$player_number) %>%
            dplyr::group_by(.data$name) %>%
            dplyr::summarise(n_digs = dplyr::n()) %>% dplyr::ungroup()
        dig_responsibility_posterior[digging_history$name] <- dig_responsibility_prior[digging_history$name] + digging_history$n_digs
        dig_responsibility_posterior <-  dig_responsibility_posterior / sum(dig_responsibility_posterior)
    }
    plsel_tmp <- names(sort(dig_responsibility_posterior, decreasing = TRUE))
    poc <- paste0(home_visiting, "_p", pseq)
    pp <- c(sort(as.numeric(game_state[poc])), sort(libs))
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(game_state[plsel_tmp[1]])
    list(choices = pp, selected = plsel)
}

## returns a list of button tags
## radio buttons are slightly non-standard, in that it is possible to start with none selected (use selected = NA). Also selecting the already-selected button will cause none to be selected. But you can't have more than one selected at a time
make_fat_radio_buttons <- function(..., as_radio = "radio") make_fat_buttons(..., as_radio = as_radio) ## "blankable", can de-select the selected button, "radio" must always have one selected
make_fat_buttons <- function(choices, selected, input_var, extra_class = c(), as_radio = "", ...) {
    if (length(choices) < 1) return(NULL)
    if (length(names(choices)) < 1) names(choices) <- choices
    cls <- uuid()
    ids <- uuid(n = length(choices))
    ## the actual buttons
    if (missing(selected)) selected <- 1L
    if (is.null(selected)) selected <- NA
    if (!is.na(selected)) selected <- if (selected %in% choices) which(choices == selected) else if (selected %in% names(choices)) which(names(choices) == selected) else if (!is.na(selected)) 1L
    if (length(selected) > 1) selected <- selected[1]
    clickfun <- if (nzchar(as_radio)) paste0(if (as_radio == "blankable") "var wa=$(this).hasClass('active');" else "var wa=false;", " $('.", cls, "').removeClass('active'); if (!wa) { $(this).addClass('active'); };") else "var wa=false;"
    buts <- lapply(seq_along(choices), function(i) tags$button(id = digest::digest(paste0("but-", input_var, "-", choices[[i]])), class = paste(c("btn", "btn-default", "fatradio", cls, extra_class, if (grepl("(L)", names(choices)[i], fixed = TRUE)) "libero", if ((i %eq% selected) && nzchar(as_radio)) "active"), collapse = " "), HTML(names(choices)[i]), onclick = paste0(clickfun, " if (!wa) { Shiny.setInputValue('", input_var, "', '", choices[[i]], "', {priority: 'event'}) } else { Shiny.setInputValue('", input_var, "', null, {priority: 'event'}) }"), ...))
    ## set the initial value of the associated input variable, or clear it
    dojs(paste0("Shiny.setInputValue(\"", input_var, "\", ", if (!is.na(selected) && nzchar(as_radio)) paste0("\"", choices[[selected]], "\"") else "null", ")"))
    attr(buts, "class") <- cls
    buts
}

plotOutputWithAttribs <- function(outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE, ...) {
    out <- shiny::plotOutput(outputId = outputId, width = width, height = height, click = click, dblclick = dblclick, hover = hover, brush = brush, inline = inline)
    rgs <- list(...)
    ## add extra attributes
    for (i in seq_along(rgs)) out$attribs[[names(rgs)[i]]] <- rgs[[i]]
    out
}

flash_screen <- function() dojs("$('#video_overlay_img').css('background-color', '#FFFF0080'); setTimeout(function() { $('#video_overlay_img').css('background-color', ''); }, 50);")

gg_tight <- list(theme(legend.position = "none", panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0, "null"), plot.margin = rep(unit(0, "null"), 4), axis.ticks = element_blank(), axis.ticks.length = unit(0, "null"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()), scale_x_continuous(limits = c(0, 1), expand = c(0, 0)), scale_y_continuous(limits = c(0, 1), expand = c(0, 0)))

infer_mid_coords <- function(game_state, start_x, start_y, end_x, end_y) {
    if (!missing(game_state)) {
        if (missing(start_x)) start_x <- game_state$start_x
        if (missing(start_y)) start_y <- game_state$start_y
        if (missing(end_x)) end_x <- game_state$end_x
        if (missing(end_y)) end_y <- game_state$end_y
    }
    ## assume that ball has either bounced off the block as if it were a perpendicular surface (for A!), or not deviated from the block (for block touches, not used for this yet)
    mid_x <- start_x + (end_x - start_x) * abs(start_y - 3.5) / (abs(start_y - 3.5) + abs(end_y - 3.5))
    c(mid_x, 3.5)
}

get_teams_from_dvw_dir <- function(season) {
    if (is.null(season) || is.na(season) || !dir.exists(season)) return(tibble())
    myfiles <- dir(season, pattern = "\\.(dvw|psvb|ovs)$", ignore.case = TRUE, full.names = TRUE)
    if (length(myfiles) < 1) return(tibble())
    ## remove duplicate files (might be saved in both dvw and ovs, for example)
    ok <- rep(TRUE, length(myfiles))
    is_dup <- fs::path_ext_remove(myfiles) %in% fs::path_ext_remove(myfiles)[duplicated(fs::path_ext_remove(myfiles))]
    ## remove psvb files that also exist as dvw or ovs
    ok <- ok & !(grepl("\\.psvb$", myfiles, ignore.case = TRUE) & is_dup)
    ## and dvw files that exist as ovs
    as_ovs <- paste0(fs::path_ext_remove(myfiles), ".ovs") %in% myfiles
    ok <- ok & !(grepl("\\.dvw$", myfiles, ignore.case = TRUE) & is_dup & as_ovs)
    myfiles <- myfiles[ok]
    out <- lapply(myfiles, function(z) if (grepl("psvb$", z, ignore.case = TRUE)) peranavolley::pv_read(z)$meta else if (grepl("ovs$", z, ignore.case = TRUE)) readRDS(z)$meta else dv_read(z, metadata_only = TRUE)$meta)
    team_list <- dplyr::select(do.call(bind_rows, lapply(out, function(z) z$teams)), "team_id", "team", "coach", "assistant", "shirt_colour") %>%
        mutate(team_id = stringr::str_to_upper(.data$team_id),
               team = stringr::str_to_title(.data$team),
               coach = stringr::str_to_title(.data$coach),
               assistant = stringr::str_to_title(.data$assistant),
               shirt_colour = stringr::str_to_lower(.data$shirt_colour))

    csu <- function(z) paste(unique(na.omit(z)), collapse = ", ") ## comma-separated unique values
    team_list <- team_list %>% group_by(.data$team_id) %>% dplyr::summarize(team = csu(.data$team), coach = csu(.data$coach), assistant = csu(.data$assistant), shirt_colour = most_common_value(.data$shirt_colour, na.rm = TRUE)) %>% ungroup
    ##  team_list <- left_join(left_join(left_join(aggregate(team ~ team_id, data = team_list, FUN = paste, collapse = ", "),
    ##                                             aggregate(coach ~ team_id, data = team_list, FUN = paste, collapse = ", "), by = "team_id"),
    ##                                   aggregate(assistant ~ team_id, data = team_list, FUN = paste, collapse = ", "), by = "team_id"),
    ##                         aggregate(shirt_colour ~ team_id, data = team_list, FUN = paste, collapse = ", "), by = "team_id")

    team_list <- as_tibble(team_list)
    team_list$player_table <- list(NULL)
    tid_list <- team_list$team_id

    for (t_id in tid_list) {
        player_table_tid <- do.call(bind_rows, lapply(out, function(x) {
            if (t_id %in% stringr::str_to_upper(x$teams$team_id)) {
                if(x$teams$home_away_team[which(t_id == stringr::str_to_upper(x$teams$team_id))] == "*"){
                    x$players_h[, c("player_id","number", "lastname", "firstname", "role")]
                } else if(x$teams$home_away_team[which(t_id == stringr::str_to_upper(x$teams$team_id))] == "a"){
                    x$players_v[, c("player_id","number", "lastname", "firstname", "role")]}
            } else {
                tibble(player_id = character(), number = integer(), lastname = character(), firstname = character(), role = character())
            }
        })) %>% mutate(player_id = stringr::str_to_upper(.data$player_id),
                       lastname = stringr::str_to_title(.data$lastname),
                       firstname = stringr::str_to_title(.data$firstname),
                       role = stringr::str_to_lower(.data$role)) %>% dplyr::distinct()

        player_table_tid <- player_table_tid %>% group_by(.data$player_id, .data$lastname, .data$firstname, .data$number) %>% mutate(role = case_when(is.na(.data$role) ~ "", TRUE ~ .data$role)) %>% dplyr::summarize(role = paste(.data$role)) %>% ungroup
        team_list$player_table[team_list$team_id == t_id] <- list(player_table_tid)

    }
    team_list
}

## directory chooser function, picking the best one depending on the platform
dchoose <- function(caption, path) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && tryCatch({ rstudioapi::versionInfo(); TRUE }, error = function(e) FALSE)) {
        dchoosefun <- function(caption, path) rstudioapi::selectDirectory(caption = caption, path = if (missing(path)) getwd() else path)
    } else {
        if (.Platform$OS.type == "windows") {
            dchoosefun <- function(caption, path) utils::choose.dir(caption = caption, default = if (missing(path)) "" else path)
        } else {
            ## file.choose won't work non-interactively (e.g. started via Rscript)
            if (!requireNamespace("tcltk", quietly = TRUE)) {
                stop("the tcltk package is required")
            }
            dchoosefun <- function(caption, path) tcltk::tk_choose.dir(caption = caption, default = if (missing(path)) "" else path)
        }
    }
    dchoosefun(caption = caption, path = path)
}

## and similarly for file chooser
fchoose <- function(caption, path) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && tryCatch({ rstudioapi::versionInfo(); TRUE }, error = function(e) FALSE)) {
        fchoosefun <- function(caption, path) rstudioapi::selectFile(caption = caption, path = if (missing(path)) getwd() else path)
    } else {
        if (.Platform$OS.type == "windows") {
            fchoosefun <- function(caption, path) utils::choose.files(caption = caption, multi = FALSE, default = if (missing(path)) "" else file.path(path, "*"))
        } else {
            if (!interactive()) {
                ## file.choose won't work non-interactively (e.g. started via Rscript)
                if (!requireNamespace("tcltk", quietly = TRUE)) {
                    stop("the tcltk package is required")
                }
                fchoosefun <- function(caption, path) tcltk::tk_choose.files(caption = caption, multi = FALSE, default = if (missing(path)) "" else file.path(path, "*"))
            } else {
                cat(caption, "\n"); flush.console()
                fchoosefun <- function(caption, path) file.choose()
            }
        }
    }
    fchoosefun(caption = caption, path = path)
}

was_mouse_drag <- function(start, dragtime = 500) {
    ## start should be a clickdrag object e.g. from the court ref module
    ## dragtime is the time that must have passed since the initial mouse click, in msx
    if (is.null(start) || is.null(start$mousedown)) {
        FALSE
    } else {
        (R.utils::System$currentTimeMillis() - start$mousedown_time) > dragtime
    }
}
