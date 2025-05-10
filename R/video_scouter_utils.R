## a player's most common serve type
## px2 is the dvw$plays2 object, or px is a plays object (but if we can, work from plays2, we don't need to build plays from it first)
get_player_serve_type <- function(px = NULL, px2 = NULL, serving_player_num, gs, opts) {
    if (is.null(px) && (is.null(px2) || nrow(px2) < 1)) return(NA_character_)
    if (!missing(px2)) {
        ## out <- bind_rows(px2$rally_codes) %>% dplyr::filter(.data$skill == "S", .data$team == gs$serving, .data$pnum == serving_player_num) %>% dplyr::rename(stype = "tempo") %>% dplyr::count(.data$stype) %>% dplyr::arrange(desc(.data$n))
        ## extracting just the vectors we need is faster than bind_rows'ing everything
        skl <- unlist(sapply(px2$rally_codes, function(z) z$skill))
        tm <- unlist(sapply(px2$rally_codes, function(z) z$team))
        pnm <- unlist(sapply(px2$rally_codes, function(z) z$pnum))
        tmpo <- unlist(sapply(px2$rally_codes, function(z) z$tempo))
        out <- tibble(stype = tmpo[which(skl == "S" & tm == gs$serving & pnm == serving_player_num)])
        if (nrow(out) > 0) out <- out %>% dplyr::count(.data$stype) %>% dplyr::arrange(desc(.data$n))
    } else {
        out <- dplyr::filter(px, .data$skill == "Serve", .data$team == gs$serving, .data$player_number == serving_player_num) %>% dplyr::count(.data$skill_type) %>% dplyr::arrange(desc(.data$n))
        ## reverse-map serve description to code, e.g. Jump serve back to "Q"
        chc <- dplyr::filter(opts$skill_tempo_map, .data$skill == "Serve")
        chc <- setNames(chc$tempo_code, chc$tempo)
        out$stype <- do.call(dplyr::recode, c(list(out$skill_type), as.list(chc)))
    }
    if (nrow(out) > 0) out$stype[1] else NA_character_
}

make_plays2 <- function(rally_codes, game_state, rally_ended = FALSE, dvw) {
    pseq <- seq_len(if (dv_is_beach(dvw)) 2L else 6L)
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    clock_time <- time_but_utc()
    if (is.data.frame(rally_codes)) {
        if (nrow(rally_codes) > 0) {
            codes <- codes_from_rc_rows(rally_codes) ## the actual scout code (char)
            start_coord <- dv_xy2index(as.numeric(rally_codes$start_x), as.numeric(rally_codes$start_y))
            mid_coord <- dv_xy2index(as.numeric(rally_codes$mid_x), as.numeric(rally_codes$mid_y))
            end_coord <- dv_xy2index(as.numeric(rally_codes$end_x), as.numeric(rally_codes$end_y))
            if ("time" %in% names(rally_codes) && !all(is.na(rally_codes$time))) clock_time <- rally_codes$time
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
        ## if we've added entries to code, add dummy (NA) coord and video times
        if ((length(codes) - length(start_coord)) > 0) start_coord <- c(start_coord, rep(NA_integer_, length(codes) - length(start_coord)))
        if ((length(codes) - length(mid_coord)) > 0) mid_coord <- c(mid_coord, rep(NA_integer_, length(codes) - length(mid_coord)))
        if ((length(codes) - length(end_coord)) > 0) end_coord <- c(end_coord, rep(NA_integer_, length(codes) - length(end_coord)))
        if ((length(codes) - length(vt)) > 0) vt <- c(vt, rep(if (length(vt) > 0) tail(vt, 1) else NA_integer_, length(codes) - length(vt)))
        if ((length(codes) - length(clock_time)) > 0) clock_time <- c(clock_time, rep(if (length(clock_time) > 0) tail(clock_time, 1) else as.POSIXct(NA), length(codes) - length(clock_time)))
        if ((length(codes) - length(phase)) > 0) phase <- c(phase, rep(NA_character_, length(codes) - length(phase)))
        if ((length(codes) - length(rcv)) > 0) rcv <- c(rcv, vector("list", length(codes) - length(rcv)))
    }
    out <- tibble(code = codes, ## col 1
                  point_phase = NA_character_, ## col 2
                  attack_phase = NA_character_, ## col 3
##                  X4 = NA, ## col 4
                  start_coordinate = start_coord, mid_coordinate = mid_coord, end_coordinate = end_coord, ## cols 5-7
                  time = clock_time, ## col 8
                  set_number = game_state$set_number, home_setter_position = game_state$home_setter_position, visiting_setter_position = game_state$visiting_setter_position, ## cols 9-11, NB these 3 not used directly in dv_read
                  video_file_number = NA, video_time = vt, ## cols 12-13
  ##                X14 = NA, ## col 14
                  home_score_start_of_point = game_state$home_score_start_of_point,
                  visiting_score_start_of_point = game_state$visiting_score_start_of_point,
                  serving = game_state$serving)
    out <- bind_cols(out, as.data.frame(game_state[paste0("home_p", pseq)]))
    out <- bind_cols(out, as.data.frame(game_state[paste0("visiting_p", pseq)]))
    for (lib in c(paste0("ht_lib", 1:2), paste0("vt_lib", 1:2))) out[[lib]] <- if (!lib %in% names(game_state)) NA_integer_ else as.integer(game_state[[lib]])
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

sanitize_game_state <- function(gs) {
    if (is.null(gs)) return(gs)
    ## if we are inserting a new row and using a game_state vector from another row, strip out values that are not transferable
    ## (but assume that the fundamentals haven't changed: team lineups, scores, and similar
    gs$start_x <- gs$start_y <- gs$mid_x <- gs$mid_y <- gs$end_x <- gs$end_y <- NA_real_
    gs$startxy_valid <- gs$midxy_valid <- gs$endxy_valid <- FALSE
    gs$current_time_uuid <- ""
    gs$end_t <- gs$start_t <- NA_real_
    gs$code <- character()
    ## not sure yet about
    ##gs$current_team
    ##gs$serving
    ##gs$rally_started
    #gs$point_won_by <- NA_character_
    gs
}

game_state_make_substitution <- function(game_state, team, player_out, player_in, dvw) {
    team <- match.arg(team, c("*", "a"))
    pseq <- seq_len(if (dv_is_beach(dvw)) 2L else 6L)
    lup_cols <- if (team == "*") paste0("home_p", pseq) else paste0("visiting_p", pseq)
    this_lup <- as.numeric(reactiveValuesToList(game_state)[lup_cols])
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

plays2_to_plays <- function(plays2, dvw, evaluation_decoder, starting_point_id = 0L) {
    pseq <- seq_len(if (is_beach(dvw)) 2L else 6L)
    if (is.null(plays2) || nrow(plays2) < 1) {
        out <- tibble(code = character(), skill = character(), home_setter_position = integer(), visiting_setter_position = integer(), home_team_score = integer(), visiting_team_score = integer(), phase = character(), set_number = integer(), video_time = numeric(), error_icon = character(), start_coordinate_x = numeric(), start_coordinate_y = numeric(), mid_coordinate_x = numeric(), mid_coordinate_y = numeric(), end_coordinate_x = numeric(), end_coordinate_y = numeric())
        for (pn in pseq) {
            out[[paste0("home_p", pn)]] <- integer()
            out[[paste0("visiting_p", pn)]] <- integer()
        }
        return(out)
    }
    out <- datavolley:::parse_code(plays2$code, meta = dvw$meta, evaluation_decoder = evaluation_decoder, file_type = if (is_beach(dvw)) "beach" else "indoor")$plays %>%
        mutate(home_setter_position = plays2$home_setter_position,
               visiting_setter_position = plays2$visiting_setter_position,
               home_team_score = plays2$home_score_start_of_point, ## TODO FIX
               visiting_team_score = plays2$visiting_score_start_of_point, ## TODO FIX
               phase = if ("phase" %in% names(plays2)) plays2$phase else NA_character_,
               set_number = plays2$set_number,
               video_time = plays2$video_time) %>%
        bind_cols(setNames(dv_index2xy(plays2$start_coordinate), c("start_coordinate_x", "start_coordinate_y")),
                  setNames(dv_index2xy(plays2$mid_coordinate), c("mid_coordinate_x", "mid_coordinate_y")),
                  setNames(dv_index2xy(plays2$end_coordinate), c("end_coordinate_x", "end_coordinate_y")))
    ##code team player_number player_name player_id skill skill_type evaluation_code evaluation attack_code
    ##attack_description set_code set_description set_type start_zone end_zone end_subzone end_cone skill_subtype num_players
    ##num_players_numeric special_code timeout end_of_set substitution point home_team_score visiting_team_score
    ##home_setter_position visiting_setter_position custom_code file_line_number
    ## add point_id
    pid <- if (!is.null(starting_point_id) && !is.na(starting_point_id)) starting_point_id else 0L
    temp_point_id <- rep(NA_integer_, nrow(out))
    temp_point_id[1] <- pid
    temp_timeout <- rep(FALSE, nrow(out))
    if (nrow(out) > 1) {
        for (k in 2:nrow(out)) {
            if (isTRUE(out$point[k - 1]) || temp_timeout[k] || temp_timeout[k-1]) pid <- pid + 1L
            temp_point_id[k] <- pid
        }
    }
    out$point_id <- temp_point_id
    ## serving team
    who_served <- dplyr::distinct(dplyr::filter(out, .data$skill == "Serve"), .data$point_id, .data$team) %>%
        dplyr::rename(serving_team = "team") %>% dplyr::filter(!duplicated(.data$point_id))
    out <- left_join(out, who_served, by = c("point_id"))
    out$serving_team <- as.character(out$serving_team) ## to be sure is not factor
    out$error_icon <- ""##ifelse(is.na(x$plays$error_message), "", HTML(as.character(icon("exclamation-triangle"))))
    bind_cols(out, plays2[, c(paste0("home_p", pseq), paste0("visiting_p", pseq))])
}

is_skill <- function(z) !is.na(z) & (!z %in% c("Timeout", "Technical timeout", "Substitution"))
is_beach <- function(dvw) isTRUE(grepl("beach", dvw$meta$match$regulation))
other <- function(tm) { oth <- rep(NA_character_, length(tm)); oth[tm %eq% "*"] <- "a"; oth[tm %eq% "a"] <- "*"; oth }
##other <- function(tm) c("a", "*")[as.numeric(factor(tm, levels = c("*", "a")))]

empty_rally_codes <- tibble(team = character(), pnum = character(), skill = character(), tempo = character(), eval = character(), combo = character(), target = character(), sz = character(), ez = character(), esz = character(), x_type = character(), num_p = character(), special = character(), custom = character(), code = character(), t = numeric(), start_x = numeric(), start_y = numeric(), mid_x = numeric(), mid_y = numeric(), end_x = numeric(), end_y = numeric(), time = as.POSIXct(numeric()), rally_state = character(), game_state = list(), current_team = character())

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

## the code parm here can be used to provide a direct scout code, useful if e.g. the tibble row isn't a scouted skill, but also if code is provided along with skill elements (team, etc) it will be used
## note that startxy_valid, midxy_valid, endxy_valid are taken from game_state (unless provided), and if FALSE the corresponding position will not be used in the code row
## start_zone is treated separately, because sometimes we know the start zone (e.g. from an attack combo code) even if we didn't get clicked coordinates
code_trow <- function(team, pnum = 0L, skill, tempo, eval, combo = "~~", target = "~", sz = "~", ez = "~", esz = "~", x_type = "~", num_p = "~", special = "~", custom = "", t = NA_real_, start_x = NA_real_, start_y = NA_real_, mid_x = NA_real_, mid_y = NA_real_, end_x = NA_real_, end_y = NA_real_, code = NA_character_, time = as.POSIXct(NA), rally_state, startxy_valid, start_zone_valid, midxy_valid, endxy_valid, game_state, default_scouting_table) {
    ## abbreviated parameter names here to make code more concise: pnum = player number, eval = evaluation code, sz = start zone, ez = end zone, esz = end subzone, x_type = extended skill type code, num_p = extended num players code, special = extended special code
    ## providing 'code' but not the other skill-related params (team, pnum, etc) is a special case
    na2t <- function(z, width = 1) if (is.na(z)) { if (width == 1) "~" else paste0(rep("~", width), collapse = "") } else z
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (missing(startxy_valid)) startxy_valid <- game_state$startxy_valid
    if (missing(start_zone_valid)) start_zone_valid <- game_state$startxy_valid
    if (missing(midxy_valid)) midxy_valid <- game_state$midxy_valid
    if (missing(endxy_valid)) endxy_valid <- game_state$endxy_valid
    if (!isTRUE(startxy_valid)) {
        start_x <- start_y <- NA_real_
    }
    ## treat start zone validity separately from startxy (coords), because the zone might be valid (e.g. inferred from combo code) even if the coords are not valid
    if (!isTRUE(start_zone_valid)) sz <- "~"
    if (!isTRUE(midxy_valid)) {
        mid_x <- mid_y <- NA_real_
    }
    if (!isTRUE(endxy_valid)) {
        end_x <- end_y <- NA_real_
        ez <- esz <- "~"
    }
    if (!is.na(code) && missing(team) && missing(pnum)) {
        ## only the code has been provided
        NAc <- NA_character_
        tibble(team = NAc, pnum = NAc, skill = NAc, tempo = NAc, eval = NAc, combo = NAc, target = NAc, sz = NAc, ez = NAc, esz = NAc, x_type = NAc, num_p = NAc, special = NAc, custom = NAc, code = code, t = t, start_x = start_x, start_y = start_y, mid_x = mid_x, mid_y = mid_y, end_x = end_x, end_y = end_y, time = time, rally_state = rally_state, game_state = list(game_state), current_team = game_state$current_team)
    } else {
        if (missing(tempo) || is.null(tempo) || tempo %eq% "~" || is.na(tempo)) tempo <- tryCatch(default_scouting_table$tempo[default_scouting_table$skill == skill], error = function(e) "~")
       if (missing(eval) || is.null(eval) || eval %eq% "~" || is.na(eval)) eval <- tryCatch(default_scouting_table$evaluation_code[default_scouting_table$skill == skill], error = function(e) "~")
        if ((missing(x_type) || is.null(x_type) ||x_type %eq% "~" || is.na(x_type)) && skill %eq% "A") x_type <- "H" ## default to hard hit
        if (is.null(pnum) || is.null(pnum) || is.na(pnum) || pnum %eq% "Unknown") pnum <- 0L
        as_tibble(c(lapply(list(team = team, pnum = ldz(pnum), skill = skill, tempo = tempo, eval = eval, combo = na2t(combo, 2), target = na2t(target), sz = na2t(sz), ez = na2t(ez), esz = na2t(esz), x_type = na2t(x_type), num_p = na2t(num_p), special = na2t(special), custom = if (is.na(custom)) "" else custom), as.character), list(code = code, t = t, start_x = start_x, start_y = start_y, mid_x = mid_x, mid_y = mid_y, end_x = end_x, end_y = end_y, time = time, rally_state = rally_state, game_state = list(game_state), current_team = game_state$current_team)))
    }
}

update_code_trow <- function(trow, team, pnum, skill, tempo, eval, combo, target, sz, ez, esz, x_type, num_p, special, custom, code, t, start_x, start_y, mid_x, mid_y, end_x, end_y, start_zone_valid, time, game_state) {
    if (missing(start_zone_valid)) start_zone_valid <- game_state$startxy_valid
    ## the only things we take from the input game_state parm are the *xy_valid entries
    ## start/end positions passed in here will be ignored if the corresponding *xy_valid entry is FALSE
    new_ez <- if (!missing(ez) && !is.null(ez) && isTRUE(game_state$endxy_valid)) ez else trow$ez
    new_esz <- trow$esz
    if (!missing(esz) && !is.null(esz) && isTRUE(game_state$endxy_valid)) {
        if (nchar(esz) == 2) {
            new_ez <- substr(esz, 1, 1)
            new_esz <- substr(esz, 2, 2)
        } else {
            new_esz <- if (nchar(esz) > 0) substr(esz, 1, 1) else "~" ## first char only
        }
    }
    ## the existing game_state gets re-used, but update the *xy_valid entries
    gs <- trow$game_state[[1]]
    if (##(!missing(sz) && !is.null(sz)) || ## sz might be provided (from e.g. attack combo code) even though coords are invalid
        (!missing(start_x) && !is.null(start_x)) || (!missing(start_y) && !is.null(start_y))) gs$startxy_valid <- game_state$startxy_valid
    if ((!missing(mid_x) && !is.null(mid_x)) || (!missing(mid_y) && !is.null(mid_y))) gs$midxy_valid <- game_state$midxy_valid
    if ((!missing(ez) && !is.null(ez)) || (!missing(esz) && !is.null(esz)) || (!missing(end_x) && !is.null(end_x)) || (!missing(end_y) && !is.null(end_y))) gs$endxy_valid <- game_state$endxy_valid
    code_trow(team = if (!missing(team) && !is.null(team)) team else trow$team,
              pnum = if (!missing(pnum) && !is.null(pnum)) pnum else trow$pnum,
              skill = if (!missing(skill) && !is.null(skill)) skill else trow$skill,
              tempo = if (!missing(tempo) && !is.null(tempo)) tempo else trow$tempo,
              eval = if (!missing(eval) && !is.null(eval)) eval else trow$eval,
              combo = if (!missing(combo) && !is.null(combo)) combo else trow$combo,
              target = if (!missing(target) && !is.null(target)) target else trow$target,
              sz = if (!missing(sz) && !is.null(sz) && isTRUE(start_zone_valid)) sz else trow$sz,
              ez = new_ez,
              esz = new_esz,
              x_type = if (!missing(x_type) && !is.null(x_type)) x_type else trow$x_type,
              num_p = if (!missing(num_p) && !is.null(num_p)) num_p else trow$num_p,
              special = if (!missing(special) && !is.null(special)) special else trow$special,
              custom = if (!missing(custom) && !is.null(custom)) custom else trow$custom,
              code = if (!missing(code) && !is.null(code)) code else trow$code,
              t = if (!missing(t) && !is.null(t)) t else trow$t,
              start_x = if (!missing(start_x) && !is.null(start_x) && isTRUE(game_state$startxy_valid)) start_x else trow$start_x,
              start_y = if (!missing(start_y) && !is.null(start_y) && isTRUE(game_state$startxy_valid)) start_y else trow$start_y,
              mid_x = if (!missing(mid_x) && !is.null(mid_x) && isTRUE(game_state$midxy_valid)) mid_x else trow$mid_x,
              mid_y = if (!missing(mid_y) && !is.null(mid_y) && isTRUE(game_state$midxy_valid)) mid_y else trow$mid_y,
              end_x = if (!missing(end_x) && !is.null(end_x) && isTRUE(game_state$endxy_valid)) end_x else trow$end_x,
              end_y = if (!missing(end_y) && !is.null(end_y) && isTRUE(game_state$endxy_valid)) end_y else trow$end_y,
              time = if (!missing(time) && !is.null(time)) time else trow$time,
              rally_state = trow$rally_state,
              game_state = gs)
}

is_green_code <- function(codes) grepl("^[a\\*]\\$\\$&H", codes)

## transfer selected skill details from the `from` row to the `to` row
## each of `from` and `to` should be a one-row tibble, as from rally_codes(), or from plays2
## The details transferred are somewhat conservative. We could in principle adjust skill evaluations according to the
#    compound skills table (so if e.g. the scout started with A+ D- and changed the D to D+, we could automatically adjust the A to A-)
## But it seems like this is a risk that the auto-correction will override something that the scout is trying to do (that does not
##   match the compound skills table). So left for now as something to think about TODO
transfer_scout_row_details <- function(from, to, tempo = TRUE, num_p = TRUE, pos = TRUE, eval = FALSE, dvw) {
    if ("rally_codes" %in% names(from) && "rally_codes" %in% names(to)) {
        ## we have been given a row from the plays2 data.frame
        ## call this function again with the rally_codes from those rows
        temp <- transfer_scout_row_details(from = from$rally_codes[[1]], to = to$rally_codes[[1]], tempo = tempo, num_p = num_p)
        ## if we tried an impossible transfer (e.g. from an attack kill to its following row, which will be a green code) then we can't run make_plays2 etc
        ## TODO make this check more robust: can it be something other than NULL?
        if (!is.null(temp)) {
            ## now rebuild code etc
            out <- make_plays2(temp, game_state = to$rally_codes[[1]]$game_state[[1]], dvw = dvw)
            out[, names(to)]
        } else {
            to
        }
    } else if (!"game_state" %in% names(from) || !"game_state" %in% names(to)) {
        warning("cannot transfer scout details: unexpected from/to format")
        to
    } else {
        ## we've been given a row from rally_codes
        if (tempo) {
            ## make e.g. set tempo match attack tempo, dig tempo match attack tempo
            ## not yet allowing dig tempo to change the preceding attack or set tempo, that will need to disambiguate the
            ## set-attack-(block)-dig from the same dig in a subsequent dig-set-attack sequence
            if (((from$skill %eq% "A" && to$skill %eq% "E" || (from$skill %eq% "E" && to$skill %eq% "A")) && from$team %eq% to$team) ||
                 (from$skill %eq% "S" && to$skill %eq% "R") || (from$skill %eq% "R" && to$skill %eq% "S") ||
                 (from$skill %eq% "A" && to$skill %in% c("B", "D") && from$team %neq% to$team) ||
                 (from$skill %eq% "B" && to$skill %in% c("A", "D") && from$team %eq% to$team)) {
                to$tempo <- from$tempo
            }
        }
        if (num_p) {
            if (((from$skill %eq% "A" && to$skill %eq% "B") || (from$skill %eq% "B" && to$skill %eq% "A")) && from$team %neq% to$team) {
                to$num_p <- from$num_p
            }
        }
        if (pos) {
            if ((from$skill %eq% "S" && to$skill %eq% "R") || (from$skill %eq% "R" && to$skill %eq% "S")) {
                to$sz <- from$sz
                to$ez <- from$ez
                to$esz <- from$esz
            }
            ## dig location could in principle come from attack end location but that depends on whether the file is using zones or cones
            ##  and also whether the scout is scouting attacks with intended direction (end zone) or actual (after deflection off block)
            ##  so leave for now
        }
        if (eval) {
            ## match e.g. serve quality to reception
            ## but see note above
            warning("evaluation transfer not yet enabled")
        }
        to
    }
}

## given a new row `from_row` and a data frame `to_df`, replace the row at `row_idx` with `from_row` and transfer selected skill details from that
##  row to the surrounding rows in `to_df`
## `row_idx` gives the row number of `from_row` in `to_df`. `row_idx` can be (nrow(to_df) + 1) if it's being appended but note that in this case `from_row` does NOT get appended by this function
## the `which` parameter is a vector of row offsets to modify (-1 means immediately preceding row, +1 = immediately following row, +2 = the row after that, etc)
transfer_scout_details <- function(from_row, to_df, row_idx, tempo = TRUE, num_p = TRUE, which = c(-1L, 1L), dvw) {
    if (nrow(to_df) < 1) return(to_df)
    if (is.logical(row_idx)) row_idx <- which(row_idx)
    if (length(row_idx) != 1 || is.na(row_idx)) {
        warning("cannot update scout details: invalid row_idx")
        return(to_df)
    }
    if (!setequal(names(from_row), names(to_df))) {
        warning("cannot update scout details: names differ")
        return(to_df)
    }
    from_row <- from_row[, names(to_df)] ## ensure column ordering
    if (is.null(from_row$rally_codes[[1]]) && !is.null(from_row$code) && is_green_code(from_row$code)) {
        ## from_row is a green code, nothing to transfer
        return(to_df)
    }
    ## potentially need to transfer some details from the new row to the preceding or following rows
    insert_row_idx <- row_idx ## indices of rows in to_df that will be replaced
    new_df_chunk <- from_row ## replace with this
    if ((-1L %in% which) && row_idx > 1 && row_idx <= (nrow(to_df) + 1)) {
        ## new details to the immediately preceding row, but not if it's a green code
        to_row <- to_df[row_idx - 1L, ]
        if (!(is.null(to_row$rally_codes[[1]]) && !is.null(to_row$code) && is_green_code(to_row$code))) {
            temp <- transfer_scout_row_details(from = from_row, to = to_row, tempo = tempo, num_p = num_p, dvw = dvw)
            new_df_chunk <- bind_rows(temp, new_df_chunk)
            insert_row_idx <- c(row_idx - 1L, insert_row_idx)
        }
    }
    if ((1L %in% which) && row_idx < nrow(to_df)) {
        ## new details to the immediately following row but not if it's a green code
        to_row <- to_df[row_idx + 1L, ]
        if (!(is.null(to_row$rally_codes[[1]]) && !is.null(to_row$code) && is_green_code(to_row$code))) {
            temp <- transfer_scout_row_details(from = from_row, to = to_row, tempo = tempo, num_p = num_p, dvw = dvw)
            new_df_chunk <- bind_rows(new_df_chunk, temp)
            insert_row_idx <- c(insert_row_idx, row_idx + 1L)
        }
    }
    ## NOTE and TODO FIX this is incomplete! If we edit the tempo of a dig row, then we need to change the preceding block, attack, and set tempos. Not just the one row prior and after the new row
    to_df[insert_row_idx, ] <- new_df_chunk
    to_df
}

## take a vector of scout codes (character) and return a list. List elements will be NULL for non-skill codes (timeouts, subs, end-of-set, etc) or a one-row x 14 column data frame otherwise, with the 14 code components in columns
## roughly similar to datavolley:::parse_code but does not return interpreted columns, nor add player names, etc
parse_code_minimal <- function(codes) {
    out <- vector("list", length(codes))
    if (length(codes) < 1) return(out)
    ## for timeouts, subs, green codes, point assignments, setter pos, end-of-set markers return NULL
    idx <- which(!codes %in% c("T", "*T", "aT") | ## timeouts
        grepl("^[a\\*]?[zcCpP]", codes) | ## subs, setter pos, point assignments
        grepl("^\\*\\*[[:digit:]]set", codes, ignore.case = TRUE) | ## end of set
        is_green_code(codes) | ## green codes
        grepl(">LUp", codes, ignore.case = TRUE)) ## lineup codes
    codes <- codes[idx]
    codes <- str_pad(codes, 20, side = "right", pad = "~")

    ## split codes into components
    ss <- setNames(matrix(c(1, 1, ## 1 = team
                            2, 3, ## 2 = player number
                            4, 4, ## 3 = skill
                            5, 5, ## 4 = tempo
                            6, 6, ## 5 = eval
                            7, 8, ## 6 = combo
                            9, 9, ## 7 = target
                            10, 10, ## 8 = start zone
                            11, 11, ## 9 = end zone
                            12, 12, ## 10 = end subzone
                            13, 13, ## 11 = skill subtype
                            14, 14, ## 12 = num players
                            15, 15, ## 13 = special
                            16, 20), ## 14 = custom
                          ncol = 2, byrow = TRUE), c("start", "end"))
    csplit <- do.call(rbind, stringr::str_sub_all(codes, start = ss))
    flt <- function(z, allowed, repl = "~") {
        z[!z %in% allowed] <- repl
        z
    }
    custom <- sub("~+$", "", csplit[, 14])
    out[idx] <- split(tibble(team = flt(csplit[, 1], c("*", "a"), repl = NA_character_),
                             pnum = ldz2(csplit[, 2]),
                             skill = flt(csplit[, 3], c("S", "R", "A", "B", "D", "E", "F")),
                             tempo = flt(csplit[, 4], c("H", "M", "Q", "T", "U", "N", "O")),
                             eval = flt(csplit[, 5], c("#", "+", "!", "-", "/", "=")),
                             combo = csplit[, 6],
                             target = flt(csplit[, 7], c("F", "C", "B", "P", "S")),
                             sz = flt(csplit[, 8], 1:9),
                             ez = flt(csplit[, 9], 1:9),
                             esz = flt(csplit[, 10], c("A", "B", "C", "D")),
                             x_type = csplit[, 11],
                             num_p = flt(csplit[, 12], 0:9),
                             special = csplit[, 13],
                             custom = custom), seq_along(idx))
    out
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
                na.omit(dvw$meta$players_h$number[grepl("L", dvw$meta$players_h$special_role)])
            } else {
                out <- c(game_state$ht_lib1, game_state$ht_lib2)
                ## note that -1 means no libero used
                out[!is.na(out) & out >= 0]
            }
        } else if (team == "a") {
            if (!all(paste0("vt_lib", 1:2) %in% names(game_state)) || (is.na(game_state$vt_lib1) && is.na(game_state$vt_lib2))) {
                na.omit(dvw$meta$players_v$number[grepl("L", dvw$meta$players_v$special_role)])
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
    temp <- left_join(tibble(number = nums), temp_players[, c("number", if (to == "name") "name", if (to == "number lastname") c("lastname", "firstname"), "special_role")], by = "number")
    if (to == "name") {
        temp$name[is.na(temp$name)] <- ""
        temp$name
    } else if (to == "number lastname") {
        temp$lastname[is.na(temp$lastname)] <- ""
        ## also include first initial if surnames are duplicated
        if (any(duplicated(temp$lastname))) {
            for (ninit in 1:4) {
                didx <- temp$lastname %in% temp$lastname[duplicated(temp$lastname)]
                chk <- temp$lastname
                chk[didx] <- paste(temp$lastname[didx], substr(temp$firstname[didx], 1, ninit))
                if (!any(duplicated(chk))) break
                chk <- temp$lastname ## so that if the last loop fails to give unique names, we fall back to just surnames
            }
            temp$lastname <- chk
        }
        temp$lastname[grepl("L", temp$special_role)] <- paste0(temp$lastname[grepl("L", temp$special_role)], " (L)")
        paste(temp$number, temp$lastname, sep = "<br />")
    } else {
        character()
    }
}

## return a list of the possible passing players, and our guess of the most likely passer
## First, we identify the rotation, and check if passing has occured in the past from a serve at that location.
## If yes, the passer (currently on court) with the most receptions will be proposed in priority.
## If no, we assume S-H-M rotation, and articulate the 3-player passing line with each taking left-middle-right channel.
## if weighted = TRUE, we use all passes for a given rotation, and weight them according to their distance from the current pass location
## if weighted = FALSE, we only consider passes in the end zone of the current pass
guess_pass_player_options <- function(game_state, dvw, system, weighted = TRUE) {
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
    if (!weighted) {
        passing_history <- dplyr::filter(dvw$plays, .data$skill == "Reception", .data[[paste0(home_visiting, "_setter_position")]] == passing_rot,
                                         .data$end_zone == passing_zone, .data$team == passing_team)
    } else {
        passing_history <- dplyr::filter(dvw$plays, .data$skill == "Reception", .data[[paste0(home_visiting, "_setter_position")]] == passing_rot,
                                         .data$team == passing_team)
    }
    passing_responsibility_posterior <- do_responsibility_posterior(history = passing_history, game_state = game_state, prior = passing_responsibility_prior, home_visiting = home_visiting, weighted = weighted, current_team = passing_team, start_end = "end", pseq = pseq)
    ##cat("passing responsibility posterior\n")
    ##print(passing_responsibility_posterior)

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
    thisxy <- if (isTRUE(do_flip_click)) as.numeric(dv_flip_xy(game_state$start_x, game_state$start_y)) else c(game_state$start_x, game_state$start_y)
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
    ## adjust the y-coordinate: back row attacks are likely to be clicked just in front of the 3m line, and dv_xy2zone will assign them a front-row zone
    this_y <- adjusted_backrow_pos(game_state = game_state)$y
    ##cat(", now:", this_y, "\n")
    attacking_zone <- dv_xy2zone(game_state$start_x, this_y)
    ##cat("attack zone: ", attacking_zone, "\n")
    ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
    attacking_responsibility <- player_responsibility_fn(system = system, skill = "Attack", setter_position = setter_rot, zone = attacking_zone, libs = NULL, home_visiting = home_visiting, serving = game_state$serving %eq% attacking_team)

    attacking_responsibility_prior <- setNames(rep(0, length(pseq)), c(paste0(home_visiting, "_p", pseq)))
    if (!is.na(attacking_responsibility)) attacking_responsibility_prior[attacking_responsibility] <- 1

    ## Update the probability with the history of the game
    ##  this also needs to account for the attacking team being in sideout/breakpoint
    attacking_history <- dplyr::filter(dvw$plays, .data$skill == "Attack", .data[[paste0(home_visiting, "_setter_position")]] == setter_rot,
                                       .data$start_zone == attacking_zone, .data$team == attacking_team, .data$serving_team == game_state$serving)

    attacking_responsibility_posterior <- attacking_responsibility_prior
    if (nrow(attacking_history) > 0) {
        attacking_history <- dplyr::select(attacking_history, "team", "player_number", paste0(home_visiting, "_p", pseq)) %>% tidyr::pivot_longer(cols = paste0(home_visiting, "_p", pseq)) %>%
            dplyr::filter(.data$value == .data$player_number) %>%
            dplyr::group_by(.data$name) %>% dplyr::summarize(n_attacks = dplyr::n()) %>% dplyr::ungroup()
        attacking_responsibility_posterior[attacking_history$name] <- attacking_responsibility_prior[attacking_history$name] + attacking_history$n_attacks
        attacking_responsibility_posterior <-  rescale_posterior(attacking_responsibility_posterior)
    }
    poc <- names(sort(attacking_responsibility_posterior, decreasing = TRUE))
    pp <- sort(as.numeric(game_state[poc]))
    plsel <- as.numeric(game_state[poc[1]])
    list(choices = pp, selected = plsel)
}

## combined guess of attack code and player
guess_attack <- function(game_state, dvw, opts, system, weight = 2.5) {
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    prior <- guess_attack_code_prior(game_state = game_state, dvw = dvw, opts = opts)
    if (!game_state$current_team %in% c("*", "a")) {
        ## can't refine code guess from data
        return(list(code = head(prior$code, 5), player = guess_attack_player_options(game_state = game_state, dvw = dvw, system = system)))
    }
    beach <- is_beach(dvw)
    pseq <- seq_len(if (beach) 2L else 6L)
    home_visiting <- if (game_state$current_team %eq% "a") "visiting" else "home"
    setter_position <- game_state[[paste0(home_visiting, "_setter_position")]]
    exclude_codes <- c(if (!missing(opts) && !is.null(opts$setter_dump_code)) opts$setter_dump_code else "PP",
                       if (!missing(opts) && !is.null(opts$second_ball_attack_code)) opts$second_ball_attack_code else "P2",
                       if (!missing(opts) && !is.null(opts$overpass_attack_code)) opts$overpass_attack_code else "PR",
                       NA_character_)
    history <- dplyr::filter(dvw$plays, .data$skill == "Attack", .data[[paste0(home_visiting, "_setter_position")]] == setter_position, .data$team == game_state$current_team, !.data$attack_code %in% exclude_codes)
    serving <- isTRUE(game_state$serving == game_state$current_team)
    history <- if (serving) dplyr::filter(history, .data$serving_team == .data$team) else dplyr::filter(history, .data$serving_team != .data$team)
    ## TODO perhaps - filter history by pass quality as well (at least into poor/OK/good)
    if (nrow(history) > 0) {
        histxy <- history[, c("start_coordinate_x", "start_coordinate_y")]
        thisxy <- c(game_state$start_x, game_state$start_y)
        flipidx <- history$start_coordinate_y > 3.5 ## flip by start loc
        ## use flipidx to refer all locations to the lower end of court
        histxy[flipidx, ] <- dv_flip_xy(histxy[flipidx, ])
        ## and flip the current action if needed
        if (isTRUE(game_state$current_team == "*" && game_state$home_team_end == "upper") || (game_state$current_team == "a" && game_state$home_team_end == "lower")) {
            thisxy <- as.numeric(dv_flip_xy(thisxy[1], thisxy[2]))
        }
        history$dist <- sqrt((thisxy[1] - histxy[, 1])^2 + (thisxy[2] - histxy[, 2])^2)
        history$w <- exp(-weight*history$dist)
        history <- mutate(history, skill_id = dplyr::row_number()) %>% dplyr::select("skill_id", "team", "attack_code", "player_number", paste0(home_visiting, "_p", pseq), "w") %>%
                tidyr::pivot_longer(cols = paste0(home_visiting, "_p", pseq)) %>%
            ## libero doesn't appear in _p* cols
            group_by(.data$skill_id) %>%
            mutate(lib = sum(.data$value %eq% .data$player_number) < 1)
        history <- bind_rows(history  %>% ungroup %>% dplyr::filter(!.data$lib) %>% dplyr::filter(.data$value == .data$player_number),
                             history %>% dplyr::filter(.data$lib) %>% dplyr::slice(1L) %>% mutate(name = "libero"))
        history <- history %>% dplyr::group_by(.data$name, .data$attack_code) %>% dplyr::summarise(n_times = mean(.data$w) * nrow(history)) %>% dplyr::ungroup() ## take mean of w, but weight the whole lot by the number of attempts so that as the match progresses, this outweighs the prior
        prior <- prior %>% mutate(name = attack_player_prior_by_code(system = system, setter_position = setter_position, set_type = .data$set_type, attacker_position = .data$attacker_position, home_visiting = home_visiting, serving = serving),
                                  n_times_prior = exp(-weight * .data$d))
        history <- dplyr::full_join(history, prior %>% dplyr::select("name", attack_code = "code", "n_times_prior"), by = c("attack_code", "name"))
        history <- history %>% mutate(n_times = if_else(is.na(.data$n_times), 0.0, .data$n_times), n_times_prior = if_else(is.na(.data$n_times_prior), 0.0, .data$n_times_prior),
                                      n_times = .data$n_times + .data$n_times_prior) %>%
            group_by(.data$attack_code) %>% dplyr::summarize(n_times = sum(.data$n_times), name = case_when(all(is.na(.data$name)) ~ NA_character_, TRUE ~ most_common_value(na.omit(.data$name)))) %>% ungroup %>%
            dplyr::arrange(desc(.data$n_times))
        ## if the pass was poor, favour Vx over Xx
        ## TODO this should go by attack code tempo, not "V"
        if (identical(tail(dvw$plays$skill, 2), c("Reception", "Set")) && isTRUE(game_state$current_team %eq% tail(dvw$plays$team, 2)[1]) && isTRUE(grepl("^Negative", tail(dvw$plays$evaluation, 2)[1]))) {
            idx <- grepl("^V", history$attack_code)
            history$n_times[idx] <- history$n_times[idx] * 2
            history <- history %>% dplyr::arrange(desc(.data$n_times))
        }
        ## swap e.g. V5 X5 so that X5 is first, if the probabilities are the same
        ## TODO this should go by attack code tempo, not "V" and "X"
        for (i in head(seq_len(nrow(history)), -1)) {
            if (substr(history$attack_code[i], 2, 2) == substr(history$attack_code[i+1], 2, 2) && grepl("^V", history$attack_code[i]) && grepl("^X", history$attack_code[i+1]) &&
                abs(history$n_times[i] - history$n_times[i+1]) < 0.01) {
                history[c(i, i+1), ] <- history[c(i+1, i), ]
            }
        }
        ## now we can take the most likely player along with the most likely code
        pp <- tryCatch({
            poc <- na.omit(as.numeric(sub(".*_p", "", unique(na.omit(history$name))))) ## player pos in most likely order
            if (length(poc) < 1) {
                ## got nothing, fall back to guessing player independent of attack code
                NULL
            } else {
                poc <- paste0(home_visiting, "_p", c(poc, setdiff(pseq, poc))) ## any missing ones added
                pp <- sort(as.numeric(game_state[poc])) ## as jersey numbers, sorted
                plsel <- as.numeric(game_state[poc[1]]) ## most likely
                list(choices = pp, selected = plsel)
            }
        }, error = function(e) NULL)
        if (is.null(pp)) pp <- guess_attack_player_options(game_state = game_state, dvw = dvw, system = system)
        list(code = head(history$attack_code, 5), player = pp)
    } else {
        list(code = head(prior$code, 5), player = guess_attack_player_options(game_state = game_state, dvw = dvw, system = system))
    }
}

guess_attack_code_prior <- function(game_state, dvw, opts) {
    exclude_codes <- c(if (!missing(opts) && !is.null(opts$setter_dump_code)) opts$setter_dump_code else "PP",
                       exclude_codes <- if (!missing(opts) && !is.null(opts$second_ball_attack_code)) opts$second_ball_attack_code else "P2",
                       exclude_codes <- if (!missing(opts) && !is.null(opts$overpass_attack_code)) opts$overpass_attack_code else "PR",
                       NA_character_)
    atbl <- dvw$meta$attacks %>% dplyr::filter(!.data$code %in% exclude_codes)
    do_flip_click <- (game_state$current_team == "*" && game_state$home_team_end == "upper") || (game_state$current_team == "a" && game_state$home_team_end == "lower")
    thisxy <- if (isTRUE(do_flip_click)) as.numeric(dv_flip_xy(game_state$start_x, game_state$start_y)) else c(game_state$start_x, game_state$start_y)
    ## the start location in the attack table is a bit in front of the 3m line for back-row attacks
    ## shift our y-location forwards a bit to reduce risk of our front-row click looking like it's nearest to a back-row location
    ## TODO, better solution than this
    thisxy[2] <- thisxy[2] + backrow_y_margin ## 0.33 = about 1m in real court space. See same adjustment in guess_attack_player_options
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
    atbl$d <- d
    ac <- atbl[order(d), ]
    for (i in head(seq_len(nrow(ac)), -1)) {
        ## swap e.g. V5 X5 so that X5 is first
        if (substr(ac$code[i], 2, 2) == substr(ac$code[i+1], 2, 2) && grepl("^V", ac$code[i]) && grepl("^X", ac$code[i+1])) ac[c(i, i+1), ] <- ac[c(i+1, i), ]
    }
    ac
}



## return a list of the possible digging players, and our guess of the most likely defender
## First, we identify the rotation, and check if defense has occurred in the past from that location / rotation & opp attack
## If yes, the defender (currently on court) with the most digs will be proposed in priority.
## If no, we assume S-H-M rotation, and articulate the perimeter defense system.
## if weighted = TRUE, we use all digs for a given rotation, and weight them according to their distance from the current pass location
## if weighted = FALSE, we only consider digs in the end zone of the current dig
guess_dig_player_options <- function(game_state, dvw, system, weighted = TRUE) {
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

    dig_responsibility_prior <- setNames(rep(0, length(pseq) + 1L), c(paste0(home_visiting, "_p", pseq), "libero"))
    if (!is.na(dig_responsibility)) dig_responsibility_prior[dig_responsibility] <- 1

    ## Update the probability with the history of the game
    if (!weighted) {
        digging_history <- dplyr::filter(dvw$plays, .data$skill == "Dig", .data[[paste0(home_visiting, "_setter_position")]] == setter_rot,
                                         .data$start_zone == attacking_zone, .data$end_zone == defending_zone, .data$team == defending_team)
    } else {
        digging_history <- dplyr::filter(dvw$plays, .data$skill == "Dig", .data[[paste0(home_visiting, "_setter_position")]] == setter_rot,
                                         .data$start_zone == attacking_zone, .data$team == defending_team)
        ##cat("ndigs by team: ", nrow(dplyr::filter(dvw$plays, .data$skill %eq% "Dig", .data$team %eq% defending_team)), "\n")
        ##cat("ndigs by team+rot: ", nrow(dplyr::filter(dvw$plays, .data$skill %eq% "Dig", .data$team %eq% defending_team, .data[[paste0(home_visiting, "_setter_position")]] %eq% as.character(setter_rot))), "\n")
        ##cat("ndigs by team+rot+startzone: ", nrow(dplyr::filter(dvw$plays, .data$skill %eq% "Dig", .data$team %eq% defending_team, .data[[paste0(home_visiting, "_setter_position")]] %eq% as.character(setter_rot), .data$start_zone %eq% attacking_zone)), "\n")
    }
    dig_responsibility_posterior <- do_responsibility_posterior(history = digging_history, game_state = game_state, prior = dig_responsibility_prior, home_visiting = home_visiting, weighted = weighted, start_end = "start", pseq = pseq)
    ##cat("dig responsibility posterior\n")
    ##print(dig_responsibility_posterior)

    plsel_tmp <- names(sort(dig_responsibility_posterior, decreasing = TRUE))
    poc <- paste0(home_visiting, "_p", pseq)
    pp <- c(sort(as.numeric(game_state[poc])), sort(libs))
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(game_state[plsel_tmp[1]])
    list(choices = pp, selected = plsel)
}

guess_freeball_dig_player_options <- function(game_state, dvw, system, weighted = TRUE) {
    beach <- is_beach(dvw)
    pseq <- seq_len(if (beach) 2L else 6L)
    if (beach) {
        warning("guess_freeball_dig_player_options for beach not yet tested")
    }
    if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
    if (!game_state$current_team %in% c("*", "a")) return(list(choices = numeric(), selected = c()))
    defending_team <- game_state$current_team
    home_visiting <- if (game_state$current_team %eq% "a") "visiting" else "home"
    setter_rot <- game_state[[paste0(home_visiting, "_setter_position")]]
    defending_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
    libs <- get_liberos(game_state, team = defending_team, dvw = dvw)
    if (game_state$serving %eq% defending_team) libs <- rev(libs) ## rev here so that if we have two liberos, the second is preferred in breakpoint phase

    ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
    dig_responsibility <- player_responsibility_fn(system = system, skill = "Freeball dig", setter_position = setter_rot, zone = defending_zone, libs = libs,
                                                   home_visiting = home_visiting, serving = game_state$serving %eq% defending_team)

    dig_responsibility_prior <- setNames(rep(0, length(pseq) + 1L), c(paste0(home_visiting, "_p", pseq), "libero"))
    if (!is.na(dig_responsibility)) dig_responsibility_prior[dig_responsibility] <- 1

    ## update the probability with the history of the game
    px <- dv_add_freeball_over(dvw$plays)
    if (!weighted) {
        digging_history <- dplyr::filter(px, .data$skill == "Freeball" & !.data$freeball_over, .data[[paste0(home_visiting, "_setter_position")]] == setter_rot,
                                         .data$end_zone == defending_zone, .data$team == defending_team)
    } else {
        digging_history <- dplyr::filter(px, .data$skill == "Freeball" & !.data$freeball_over, .data[[paste0(home_visiting, "_setter_position")]] == setter_rot,
                                         .data$team == defending_team)
    }
    dig_responsibility_posterior <- do_responsibility_posterior(history = digging_history, game_state = game_state, prior = dig_responsibility_prior, home_visiting = home_visiting, weighted = weighted, start_end = "start", pseq = pseq)
    ##cat("freeball dig responsibility posterior\n")
    ##print(dig_responsibility_posterior)
    plsel_tmp <- names(sort(dig_responsibility_posterior, decreasing = TRUE))
    poc <- paste0(home_visiting, "_p", pseq)
    pp <- c(sort(as.numeric(game_state[poc])), sort(libs))
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(game_state[plsel_tmp[1]])
    list(choices = pp, selected = plsel)
}

do_responsibility_posterior <- function(history, game_state, prior, home_visiting, weighted, current_team, start_end, pseq, weight = 2.5, liberos = TRUE) {
    if (!liberos) stop("do_responsibility_posterior does not yet work without liberos")
    if (missing(current_team)) current_team <- game_state$current_team
    posterior <- prior
##    cat("history passed to do_responsibility_posterior:\n"); print(dplyr::glimpse(history))
##    cat("prior passed to do_responsibility_posterior:\n"); print(prior)
    if (nrow(history) > 0) {
        if (!weighted) {
            history <- mutate(history, skill_id = dplyr::row_number()) %>% dplyr::select("skill_id", "team", "player_number", paste0(home_visiting, "_p", pseq)) %>%
                tidyr::pivot_longer(cols = paste0(home_visiting, "_p", pseq)) %>%
                ## libero doesn't appear in _p* cols
                group_by(.data$skill_id) %>%
                ## if no players match, assume the libero did it
                mutate(lib = sum(.data$value %eq% .data$player_number) < 1)
            history <- bind_rows(history %>% ungroup %>% dplyr::filter(!.data$lib) %>% dplyr::filter(.data$value == .data$player_number),
                                 history %>% dplyr::filter(.data$lib) %>% dplyr::slice(1L) %>% mutate(name = "libero"))
            history <- history %>% dplyr::group_by(.data$name) %>% dplyr::summarise(n_times = dplyr::n()) %>% dplyr::ungroup()
            posterior[history$name] <- posterior[history$name] + history$n_times
        } else {
        ## use all skills but weight by distance
            ## distance of each previous skill to current skill location
            ## reception, use end loc (reception), but flip according to start (serve loc) - the rec location could be on net or other weird places, though this should not matter for inference here
            ## dig use start (dig loc) - when it's a block back to the attacking team (cover dig) our inference here (for a normal defensive dig) will be wrong, but it doesn't matter because that won't be used
            ## cover dig use start (dig loc)
            ## freeball dig use start (freeball dig loc). Note that for the freeball over, it has start and end locs, but the freeball dig has only start (which is the end of the freeball over)
            if (start_end == "start") {
                histxy <- history[, c("start_coordinate_x", "start_coordinate_y")]
                thisxy <- c(game_state$start_x, game_state$start_y)
                flipidx <- history$start_coordinate_y > 3.5 ## flip by start loc
            } else {
                histxy <- history[, c("end_coordinate_x", "end_coordinate_y")]
                thisxy <- c(game_state$end_x, game_state$end_y)
                flipidx <- history$start_coordinate_y < 3.5 ## flip by start loc, noting that we want the END loc to be in the lower court (so `<` here)
            }
            ## use flipidx to refer all locations to the lower end of court
            histxy[flipidx, ] <- dv_flip_xy(histxy[flipidx, ])
            ## and flip the current action if needed
            if (isTRUE(current_team == "*" && game_state$home_team_end == "upper") || (current_team == "a" && game_state$home_team_end == "lower")) {
                thisxy <- as.numeric(dv_flip_xy(thisxy[1], thisxy[2]))
            }
            history$w <- exp(-weight*sqrt((thisxy[1] - histxy[, 1])^2 + (thisxy[2] - histxy[, 2])^2)) ## semi-arbitrary weighting
            history <- mutate(history, skill_id = dplyr::row_number()) %>% dplyr::select("skill_id", "team", "player_number", paste0(home_visiting, "_p", pseq), "w") %>%
                tidyr::pivot_longer(cols = paste0(home_visiting, "_p", pseq)) %>%
                ## libero doesn't appear in _p* cols
                group_by(.data$skill_id) %>%
                mutate(lib = sum(.data$value %eq% .data$player_number) < 1)
            history <- bind_rows(history  %>% ungroup %>% dplyr::filter(!.data$lib) %>% dplyr::filter(.data$value == .data$player_number),
                                 history %>% dplyr::filter(.data$lib) %>% dplyr::slice(1L) %>% mutate(name = "libero"))
##            history <- history %>% dplyr::group_by(.data$name) %>% dplyr::summarise(n_times = sum(.data$w)) %>% dplyr::ungroup()
            ## don't use sum(w), because a player that passes (or whatever skill) a lot, but nowhere near this particular point on the court, will get upweighted too much
            history <- history %>% dplyr::group_by(.data$name) %>% dplyr::summarise(n_times = mean(.data$w) * nrow(history)) %>% dplyr::ungroup() ## take mean of w, but weight the whole lot by the number of attempts so that as the match progresses, this outweighs the prior
            posterior[history$name] <- posterior[history$name] + history$n_times
        }
    }
    rescale_posterior(posterior)
}

rescale_posterior <- function(pst) {
    pst[is.na(pst) | is.infinite(pst)] <- 0
    if (sum(pst) < 1e-08) {
        setNames(rep(0, length(pst)), names(pst))
    } else {
        pst / sum(pst)
    }
}

guess_cover_player_options <- function(game_state, dvw, system, weighted = TRUE) {
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

    dig_responsibility_prior <- setNames(rep(0, length(pseq) + 1L), c(paste0(home_visiting, "_p", pseq), "libero"))
    if (!is.na(dig_responsibility)) dig_responsibility_prior[dig_responsibility] <- 1

    ## Update the probability with the history of the game
    if (!weighted) {
        digging_history <- dplyr::filter(dvw$plays, .data$skill == "Dig", .data[[paste0(home_visiting, "_setter_position")]] == setter_rot,
                                         .data$start_zone == attacking_zone, .data$end_zone == defending_zone, .data$team == attacking_team,
                                         lag(.data$skill) == "Block", !.data$team == lag(.data$team)) ## just cover digs
    } else {
        digging_history <- dplyr::filter(dvw$plays, .data$skill == "Dig", .data[[paste0(home_visiting, "_setter_position")]] == setter_rot,
                                         .data$start_zone == attacking_zone, .data$team == attacking_team,
                                         lag(.data$skill) == "Block", !.data$team == lag(.data$team)) ## just cover digs
    }
    dig_responsibility_posterior <- do_responsibility_posterior(history = digging_history, game_state = game_state, prior = dig_responsibility_prior, home_visiting = home_visiting, weighted = weighted, current_team = attacking_team, start_end = "start", pseq = pseq)
    ##cat("cover dig responsibility posterior\n")
    ##print(dig_responsibility_posterior)

    plsel_tmp <- names(sort(dig_responsibility_posterior, decreasing = TRUE))
    poc <- paste0(home_visiting, "_p", pseq)
    pp <- c(sort(as.numeric(game_state[poc])), sort(libs))
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(game_state[plsel_tmp[1]])
    list(choices = pp, selected = plsel)
}

## guess_setter_options <- function(pass_quality, game_state, dvw) {
##     if (shiny::is.reactivevalues(game_state)) game_state <- reactiveValuesToList(game_state)
##     poc <- get_players(game_state = game_state, dvw = dvw)
##     if (is_beach(dvw)) return(list(choices = poc, selected = NA_integer_)) ## TODO, whichever player didn't pass/dig
##     pseq <- seq_len(6L) ## indoor only
##     psel <- if (pass_quality %in% c("#", "+", "!")) {
##                 ## assume that setter on court is setting
##                 get_setter(game_state)
##             } else if (pass_quality %in% c("/")) {
##                 ## overpass
##                 NA_integer_
##             } else {
##                 ## poor pass
##                 home_visiting <- if (game_state$current_team %eq% "a") "visiting" else "home"
##                 setter_position <- game_state[[paste0(home_visiting, "_setter_position")]]
##                 serving <- isTRUE(game_state$serving == game_state$current_team)
##                 libs <- get_liberos(game_state, dvw = dvw)
##                 if (serving) libs <- rev(libs) ## rev here so that if we have two liberos, the second is preferred in breakpoint phase
##                 ##        ## need to know if the libero is on court or not, assume not in breakpoint P2, P5
##                 ##        if (length(libs) > 0 && isTRUE(game_state$serving == game_state$current_team) && setter_position %in% c(2, 5)) {
##                 ##            ## the middle is on court (serving) so we have no libero
##                 ##            libs <- c()
##                 ##        }
##                 ##        loc <- length(libs) > 0 ## libero on court
##                 history <- dvw$plays %>% ##mutate(serving = .data$team == .data$serving_team) %>%
##                     dplyr::filter(.data$skill == "Set", lag(.data$evaluation_code) == "-", .data[[paste0(home_visiting, "_setter_position")]] == setter_position,
##                                   .data$team == game_state$current_team)
##                 prior <- setNames(rep(0, length(pseq) + 1L), c(paste0(home_visiting, "_p", pseq), "libero"))
##                 ## simple prior, if ball in front of the 6m line then setter, otherwise libero if on court else player in 5 (back-row middle)
##                 if (isTRUE(game_state$start_y < 1.5 || game_state$start_y > 5.5)) {
##                     prior[[paste0(home_visiting, "_p", setter_position)]] <- 1
##                 } else if (length(libs) > 0) {
##                     prior$libero <- 1
##                 } else {
##                     prior[[paste0(home_visiting, "_p", if (setter_position %in% c(1, 4)) 6 else if (setter_position %in% c(2, 5)) 1 else 5)]] <- 1
##                 }
##                 posterior <- do_responsibility_posterior(history = history, game_state = game_state, prior = prior, home_visiting = home_visiting, weighted = TRUE, start_end = "start", pseq = pseq)
##                 cat("setter responsibility posterior\n")
##                 print(posterior)
##         plsel_tmp <- names(sort(dig_responsibility_posterior, decreasing = TRUE))
##         poc <- paste0(home_visiting, "_p", pseq)
##         pp <- c(sort(as.numeric(game_state[poc])), sort(libs))
##         plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(game_state[plsel_tmp[1]])
##     }
## }

## returns a list of button tags
## radio buttons are slightly non-standard, in that it is possible to start with none selected (use selected = NA). Also selecting the already-selected button will cause none to be selected. But you can't have more than one selected at a time
make_fat_radio_buttons <- function(..., as_radio = "radio") make_fat_buttons(..., as_radio = as_radio) ## "blankable", can de-select the selected button, "radio" must always have one selected
make_fat_buttons <- function(choices, selected, input_var, as_radio = "", ...) {
    ## any extra classes can be passed as class = "whatever" in the ... arguments, this will be added to the classes
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
    buts <- lapply(seq_along(choices), function(i) tags$button(id = digest::digest(paste0("but-", input_var, "-", choices[[i]])), class = paste(c("btn", "btn-default", "fatradio", cls, if (grepl("(L)", names(choices)[i], fixed = TRUE)) "libero", if ((i %eq% selected) && nzchar(as_radio)) "active"), collapse = " "), HTML(names(choices)[i]), onclick = paste0(clickfun, " if (!wa) { Shiny.setInputValue('", input_var, "', '", choices[[i]], "', {priority: 'event'}) } else { Shiny.setInputValue('", input_var, "', null, {priority: 'event'}) }"), ...))
    ## set the initial value of the associated input variable, or clear it
    dojs(paste0("Shiny.setInputValue(\"", input_var, "\", ", if (!is.na(selected) && nzchar(as_radio)) paste0("\"", choices[[selected]], "\"") else "null", ")"))
    attr(buts, "class") <- cls
    buts
}
## helper function to clear the visual selection from a fatradio group
## NOTE that this only changes the visual style, you'll still need to issue a Shiny.setInputValue(...) to change the actual input
deselect_fatradio_buttons <- function(jquery_selector, hide = FALSE) {
    dojs(paste0("$('", jquery_selector, "').removeClass('active');", if (hide) paste0("$('", jquery_selector, "').hide();")))
}

plotOutputWithAttribs <- function(outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE, ...) {
    out <- shiny::plotOutput(outputId = outputId, width = width, height = height, click = click, dblclick = dblclick, hover = hover, brush = brush, inline = inline)
    rgs <- list(...)
    ## add extra attributes
    for (i in seq_along(rgs)) out$attribs[[names(rgs)[i]]] <- rgs[[i]]
    out
}

do_contact <- function() {
    ## keyboard entry indicating a contact at this time
    ## ask the browser for the current video time
    dojs("Shiny.setInputValue('contact', [vidplayer.currentTime(), new Date().getTime()])")
}

flash_screen <- function() dojs("flash_screen();")

gg_tight <- list(theme(legend.position = "none", panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0, "null"), plot.margin = rep(unit(0, "null"), 4), axis.ticks = element_blank(), axis.ticks.length = unit(0, "null"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()), scale_x_continuous(limits = c(0, 1), expand = c(0, 0)), scale_y_continuous(limits = c(0, 1), expand = c(0, 0)))

infer_mid_coords <- function(game_state, start_x, start_y, end_x, end_y) {
    if (missing(start_x)) start_x <- game_state$start_x
    if (missing(start_y)) start_y <- game_state$start_y
    if (missing(end_x)) end_x <- game_state$end_x
    if (missing(end_y)) end_y <- game_state$end_y
    if (!isTRUE(game_state$startxy_valid)) { start_x <- start_y <- NA_real_ }
    if (!isTRUE(game_state$endxy_valid)) { end_x <- end_y <- NA_real_ }
    ## assume that ball has either bounced off the block as if it were a perpendicular surface (for A!), or not deviated from the block (for block touches)
    mid_x <- start_x + (end_x - start_x) * abs(start_y - 3.5) / (abs(start_y - 3.5) + abs(end_y - 3.5))
    c(mid_x, if (!is.na(mid_x)) 3.5 else NA_real_)
}

## clicking a back-row attack will often have the click in front of the 3m line, but we want the start zone of these attacks to be considered as the backrow zone
backrow_y_margin <- 0.33 ## how much in front of the line do we allow? 0.33 in court units is about 1m in real court space
adjusted_backrow_pos <- function(x, y, game_state) {
    adjy <- if (missing(y)) game_state$start_y else y
    if (adjy < (2.5 + backrow_y_margin)) adjy <- adjy - backrow_y_margin else if (adjy > (4.5 - backrow_y_margin)) adjy <- adjy + backrow_y_margin
    list(zone = dv_xy2zone(if (missing(x)) game_state$start_x else x, adjy), y = adjy)
}

get_teams_from_dvw_dir <- function(season) {
    if (is.null(season) || is.na(season) || !dir.exists(season)) return(tibble())
    myfiles <- dir(season, pattern = "\\.(dvw|psvb|ovs|sq)$", ignore.case = TRUE, full.names = TRUE)
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
    ## helper function to read .sq file
    sq2 <- function(fl) {
        tryCatch({
            this <- datavolley::dv_read_sq(fl)
            names(this)[names(this) %eq% "team"] <- "teams"
            this$teams$shirt_colour <- "#000000"
            this
        }, error = function(e) NULL)
    }
    out <- lapply(seq_along(myfiles), function(i) {
        try(setProgress(value = i / length(myfiles)), silent = TRUE)
        z <- myfiles[i]
        if (grepl("psvb$", z, ignore.case = TRUE)) tryCatch(peranavolley::pv_read(z)$meta, error = function(e) NULL) else if (grepl("ovs$", z, ignore.case = TRUE)) tryCatch(readRDS(z)$meta, error = function(e) NULL) else if (grepl("sq$", z, ignore.case = TRUE)) sq2(z) else tryCatch(dv_read(z, metadata_only = TRUE)$meta, error = function(e) NULL)
    })
    out <- Filter(Negate(is.null), out)
    if (length(out) < 1) return(tibble())
    team_list <- dplyr::select(do.call(bind_rows, lapply(out, function(z) z$teams)), "team_id", "team", "coach", "assistant", "shirt_colour") %>%
        mutate(team_id = stringr::str_to_upper(.data$team_id),
               team = .data$team, ##stringr::str_to_title(.data$team),
               coach = .data$coach, ##stringr::str_to_title(.data$coach),
               assistant = .data$assistant, ##stringr::str_to_title(.data$assistant),
               shirt_colour = stringr::str_to_lower(.data$shirt_colour))

    csu <- function(z) paste(unique(na.omit(z)), collapse = ", ") ## comma-separated unique values
    team_list <- team_list %>% group_by(.data$team_id) %>% dplyr::summarize(team = csu(.data$team), coach = csu(.data$coach), assistant = csu(.data$assistant), shirt_colour = most_common_value(.data$shirt_colour, na.rm = TRUE)) %>% ungroup

    team_list <- as_tibble(team_list)
    team_list$player_table <- list(NULL)
    tid_list <- team_list$team_id

    for (t_id in tid_list) {
        player_table_tid <- do.call(bind_rows, lapply(out, function(x) {
            if (t_id %in% stringr::str_to_upper(x$teams$team_id)) {
                if ("players" %in% names(x)) {
                    ## from sq file
                    x$players[, c("player_id","number", "lastname", "firstname", "role"), drop = FALSE]
                } else {
                    if(x$teams$home_away_team[which(t_id == stringr::str_to_upper(x$teams$team_id))] == "*"){
                        x$players_h[, c("player_id", "number", "lastname", "firstname", "role"), drop = FALSE]
                    } else if(x$teams$home_away_team[which(t_id == stringr::str_to_upper(x$teams$team_id))] == "a"){
                        x$players_v[, c("player_id", "number", "lastname", "firstname", "role"), drop = FALSE]}
                }
            } else {
                tibble(player_id = character(), number = integer(), lastname = character(), firstname = character(), role = character())
            }
        })) %>% mutate(player_id = stringr::str_to_upper(.data$player_id),
                       lastname = .data$lastname, ##stringr::str_to_title(.data$lastname),
                       firstname = .data$firstname, ##stringr::str_to_title(.data$firstname),
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
    if (is.null(start) || is.null(start$mousedown_time)) {
        FALSE
    } else {
        isTRUE((R.utils::System$currentTimeMillis() - start$mousedown_time) > dragtime)
    }
}

check_hit_type <- function(htype) {
    if (length(htype) != 1 || !htype %in% c("H", "T", "P")) "H" else htype
}

hide_popup <- function(is_review_pane_active) {
    dojs("$('#shiny-modal-wrapper').hide(); $('.modal-backdrop').hide();") ## popup
    if (is_review_pane_active) js_hide2("review_pane") ## review pane
}
unhide_popup <- function(is_review_pane_active) {
    dojs("$('#shiny-modal-wrapper').show(); $('.modal-backdrop').show();")
    if (is_review_pane_active) js_show2("review_pane")
}

get_video_source_type <- function(src, base_url) {
    type <- "local"
    if (is_youtube_url(src)) {
        type <- "youtube"
    } else if (!is_url(src)) {
        src <- file.path(base_url, basename(src))
    }
    list(src = src, type = type)
}

## video functions
## return value can take various forms (all returned invisibly):
## - if we are running without video: NULL
## - for "pause", "play", or "toggle_pause": the pause state after executing the command (logical)
## - for "mute", "unmute": the mute state (logical)
## - for everything else: NULL
do_video_inner <- function(what, ..., video_state, rally_state, app_data, session, id = "main_video") {
    if (!app_data$with_video) return(invisible(NULL))
    getel <- "vidplayer"
    myargs <- list(...)
    invisible({
        if (what == "pause") {
            dojs(paste0(getel, ".pause(); pause_on_type = 0;")) ## disable, otherwise it will unpause the video after a keypress
            video_state$paused <- TRUE
            if (rally_state() == .C_click_serve_start) set_rally_state(app_data$click_to_start_msg)
            TRUE ## paused state
        } else if (what == "play") {
            dojs(paste0(
                if (isTRUE(myargs$with_rewind)) paste0(getel, ".currentTime(", getel, ".currentTime() - ", app_data$play_overlap, ");"),
                getel, ".play();",
                "pause_on_type = ", app_data$pause_on_type, ";")) ## re-enable, it would have been disabled while the modal was showing
            video_state$paused <- FALSE
            if (rally_state() == app_data$click_to_start_msg) set_rally_state(.C_click_serve_start)
            FALSE ## not paused state
        } else if (what == "toggle_pause") {
            ## careful using this, because there are situations where we don't want to allow unpausing - see deal_with_pause()
            if (video_state$paused) {
                dojs(paste0(getel, ".play(); pause_on_type = 0;"))
                video_state$paused <- FALSE
                if (rally_state() == app_data$click_to_start_msg) set_rally_state(.C_click_serve_start)
                FALSE
            } else {
                dojs(paste0(getel, ".pause(); pause_on_type = ", app_data$pause_on_type, ";"))
                video_state$paused <- TRUE
                TRUE
            }
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
        ##    video_state$muted <- TRUE
            TRUE ## mute state
        } else if (what == "unmute") {
            dojs(paste0(getel, ".muted(false);"))
        ##    video_state$muted <- FALSE
            FALSE ## mute state
        ## this no longer works because the button has been removed from the UI, just use the player controls
        ## } else if (what == "muted") {
        ##    video_state$muted
        ## } else if (what == "toggle_mute") {
        ##     if (video_state$muted) {
        ##         shiny::updateActionButton(session, "video_toggle_mute", label = "Mute")
        ##         ##do_video("unmute")
        ##         dojs(paste0(getel, ".muted(false);"))
        ##         video_state$muted <- FALSE
        ##         FALSE ## mute state
        ##     } else {
        ##         shiny::updateActionButton(session, "video_toggle_mute", label = "Unmute")
        ##         ##do_video("mute")
        ##         dojs(paste0(getel, ".muted(true);"))
        ##         video_state$muted <- TRUE
        ##         TRUE ## mute state
        ##     }
        } else {
            NULL
        }
    })
}

## save to ovs file. game_state only needed if was_session_end is TRUE
save_to_ovs <- function(rdata, app_data, courtref1, courtref2, game_state, was_session_end = FALSE) {
    msg <- NULL
    ok <- FALSE
    tf <- character()
    isolate({
        if (app_data$run_env %eq% "shiny_local") {
            ## this only makes sense if running locally, not deployed on a server
            tf <- tempfile(tmpdir = file.path(app_data$user_dir, "autosave"), pattern = "ovscout2-", fileext = ".ovs")
            msg <- tryCatch({
                msgi <- NULL
                temp <- rdata$dvw
                temp$scouting_options <- rdata$options
                if (was_session_end) {
                    temp$plays <- NULL ## don't save this
                    temp$game_state <- reactiveValuesToList(game_state)
                }
                ## save court refs
                dr <- list()
                if (!is.null(courtref1$court_ref)) dr[[app_data$video_src]] <- courtref1
                if (!is.null(courtref2$court_ref) && "video_src2" %in% names(app_data) && !is.na(app_data$video_src2) && nzchar(app_data$video_src2)) dr[[app_data$video_src2]] <- courtref2
                temp$detection_refs <- dr
                saveRDS(temp, file = tf)
                if (!(file.exists(tf) && file.size(tf) > 0)) msgi <- "could not save file"
                if (was_session_end) {
                    if (is.null(msgi)) {
                        message("working file has been saved to: ", tf)
                    } else {
                        message("could not save working file on exit")
                    }
                }
                msgi
            }, error = function(e) {
                if (was_session_end) message("could not save working file on exit (error message was: ", conditionMessage(e))
                conditionMessage(e)
            })
        }
    })
    list(ok = is.null(msg), filename = tf, error_message = msg)
}
