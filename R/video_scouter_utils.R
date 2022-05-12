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
    if (is.data.frame(rally_codes)) {
        if (nrow(rally_codes) > 0) {
            codes <- codes_from_rc_rows(rally_codes)
            start_coord <- dv_xy2index(as.numeric(rally_codes$start_x), as.numeric(rally_codes$start_y))
            end_coord <- dv_xy2index(as.numeric(rally_codes$end_x), as.numeric(rally_codes$end_y))
            vt <- rally_codes$t
        } else {
            codes <- character()
            start_coord <- end_coord <- vt <- numeric()
        }
    } else {
        ## rally_codes are just char, which means that these aren't skill codes, they are auto codes (position codes or similar)
        codes <- rally_codes
        start_coord <- end_coord <- NA_integer_
        vt <- NA_real_
    }
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
            end_coord <- c(end_coord, rep(NA_integer_, n_extra))
            vt <- c(vt, rep(NA_real_, n_extra))
        }
    }
    ##cat("codes: "); print(codes)

    out <- tibble(code = codes, ## col 1
                  point_phase = NA_character_, ## col 2
                  attack_phase = NA_character_, ## col 3
##                  X4 = NA, ## col 4
                  start_coordinate = start_coord, mid_coordinate = NA_integer_, end_coordinate = end_coord, ## cols 5-7
                  time = time_but_utc(), ## col 8
                  set_number = game_state$set_number, home_setter_position = game_state$home_setter_position, visiting_setter_position = game_state$visiting_setter_position, ## cols 9-11, NB these 3 not used directly in dv_read
                  video_file_number = NA, video_time = vt, ## cols 12-13
  ##                X14 = NA, ## col 14
                  home_score_start_of_point = game_state$home_score_start_of_point,
                  visiting_score_start_of_point = game_state$visiting_score_start_of_point,
                  serving = game_state$serving)
    out <- bind_cols(out, as.data.frame(reactiveValuesToList(game_state)[paste0("home_p", pseq)]))
    bind_cols(out, as.data.frame(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)]))
}

## rationalize plays2 rows
rp2 <- function(p2) {
    ## strip out redundant codes
    ## if we have multiple az or *z codes (setter locations) without other intervening codes then just take the last of each
    ok <- rep(TRUE, nrow(p2))
    home_sl <- FALSE; visiting_sl <- FALSE
    is_home_sl <- grepl("^\\*z", p2$code)
    is_visiting_sl <- grepl("^az", p2$code)
    for (i in rev(seq_len(nrow(p2)))) {
        if (!is_visiting_sl[i] && !is_home_sl[i]) {
            home_sl <- visiting_sl <- FALSE
        } else if (is_home_sl[i]) {
            if (!home_sl) home_sl <- TRUE else ok[i] <- FALSE
        } else if (is_visiting_sl[i]) {
            if (!visiting_sl) visiting_sl <- TRUE else ok[i] <- FALSE
        }
    }
    p2[ok, ]
}

game_state_make_substitution <- function(game_state, team, player_out, player_in, dvw) {
    team <- match.arg(team, c("*", "a"))
    pseq <- seq_len(if (dv_is_beach(dvw)) 2L else 6L)
    lup_cols <- if (team == "*") paste0("home_p", pseq) else paste0("visiting_p", pseq)
    this_lup <- as.numeric(reactiveValuesToList(game_state)[lup_cols])
    available_players <- if (team == "*") dvw$meta$players_h$number else dvw$meta$players_v$number
    if (!player_out %in% this_lup) {
        message("player being subbed out is not on court, ignoring sub")
    } else if (player_in %in% this_lup) {
        message("player being subbed in is already on court, ignoring sub")
    ##} else if (!player_in %in% available_players) {
    ##    message("player being subbed in is not in player list, ignoring sub")
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
    out$phase <- datavolley::play_phase(out)
    out$set_number <- plays2$set_number
    out$video_time <- plays2$video_time
    out$error_icon <- ""##ifelse(is.na(x$plays$error_message), "", HTML(as.character(shiny::icon("exclamation-triangle"))))
    bind_cols(out, plays2[, c(paste0("home_p", pseq), paste0("visiting_p", pseq))])
}

is_skill <- function(z) !is.na(z) & (!z %in% c("Timeout", "Technical timeout", "Substitution"))
is_beach <- function(dvw) isTRUE(grepl("beach", dvw$meta$match$regulation))
zpn <- function(n) sprintf("%02d", as.numeric(n))
other <- function(tm) { oth <- rep(NA_character_, length(tm)); oth[tm %eq% "*"] <- "a"; oth[tm %eq% "a"] <- "*"; oth }
##other <- function(tm) c("a", "*")[as.numeric(factor(tm, levels = c("*", "a")))]

empty_rally_codes <- tibble(code = character(), t = numeric(), start_x = numeric(), start_y = numeric(), end_x = numeric(), end_y = numeric())
print_rally_codes <- function(rc) {
    tr <- function(z) tryCatch(round(z, 1), error = function(e) z)
    rc$code <- codes_from_rc_rows(rc)
    do.call(cat, c(lapply(seq_len(nrow(rc)), function(i) paste0(rc$code[i], ", t=", tr(rc$t[i]), ", start x=", tr(rc$start_x[i]), ", y=", tr(rc$start_y[i]), ", end x=", tr(rc$end_x[i]), ", y=", tr(rc$end_y[i]))), list(sep = "\n")))
}
codes_from_rc_rows <- function(rc) sub("~+$", "", paste0(rc$team, rc$pnum, rc$skill, rc$tempo, rc$eval, rc$combo, rc$target, rc$sz, rc$ez, rc$esz, rc$x_type, rc$num_p, rc$special, rc$custom))

code_trow <- function(team, pnum = 0L, skill, tempo, eval, combo = "~~", target = "~", sz = "~", ez = "~", esz = "~", x_type = "~", num_p = "~", special = "~", custom = "", t = NA_real_, start_x = NA_real_, start_y = NA_real_, end_x = NA_real_, end_y = NA_real_, rally_state, default_scouting_table) {
    ## abbreviated parameter names here to make code more concise: pnum = player number, eval = evaluation code, sz = start zone, ez = end zone, esz = end subzone, x_type = extended skill type code, num_p = extended num players code, special = extended special code
    if (missing(tempo) || tempo %eq% "~" || is.na(tempo)) tempo <- tryCatch(default_scouting_table$tempo[default_scouting_table$skill == skill], error = function(e) "~")
    if (missing(eval) || eval %eq% "~" || is.na(eval)) eval <- tryCatch(default_scouting_table$evaluation_code[default_scouting_table$skill == skill], error = function(e) "~")
    if ((missing(x_type) || x_type %eq% "~" || is.na(x_type)) && skill %eq% "A") x_type <- "H" ## default to hard hit
    if (is.null(pnum) || is.na(pnum) || pnum %eq% "Unknown") pnum <- 0L
    as_tibble(c(lapply(list(team = team, pnum = zpn(pnum), skill = skill, tempo = tempo, eval = eval, combo = combo, target = target, sz = sz, ez = ez, esz = esz, x_type = x_type, num_p = num_p, special = special, custom = custom), as.character), list(t = t, start_x = start_x, start_y = start_y, end_x = end_x, end_y = end_y, rally_state = rally_state)))
}

update_code_trow <- function(trow, team, pnum, skill, tempo, eval, combo, target, sz, ez, esz, x_type, num_p, special, custom, t, start_x, start_y, end_x, end_y) {
    new_ez <- if (!missing(ez)) ez else trow$ez
    new_esz <- trow$esz
    if (!missing(esz)) {
        if (nchar(esz) == 2) {
            new_ez <- substr(esz, 1, 1)
            new_esz <- substr(esz, 2, 2)
        } else {
            new_esz <- if (nchar(esz) > 0) substr(esz, 1, 1) else "~" ## first char only
        }
    }
    code_trow(team = if (!missing(team)) team else trow$team,
              pnum = if (!missing(pnum)) pnum else trow$pnum,
              skill = if (!missing(skill)) skill else trow$skill,
              tempo = if (!missing(tempo)) tempo else trow$tempo,
              eval = if (!missing(eval)) eval else trow$eval,
              combo = if (!missing(combo)) combo else trow$combo,
              target = if (!missing(target)) target else trow$target,
              sz = if (!missing(sz)) sz else trow$sz,
              ez = new_ez,
              esz = new_esz,
              x_type = if (!missing(x_type)) x_type else trow$x_type,
              num_p = if (!missing(num_p)) num_p else trow$num_p,
              special = if (!missing(special)) special else trow$special,
              custom = if (!missing(custom)) custom else trow$custom,
              t = if (!missing(t)) t else trow$t,
              start_x = if (!missing(start_x)) start_x else trow$start_x,
              start_y = if (!missing(start_y)) start_y else trow$start_y,
              end_x = if (!missing(end_x)) end_x else trow$end_x,
              end_y = if (!missing(end_y)) end_y else trow$end_y,
              rally_state = trow$rally_state)
}

get_setter_pos <- function(game_state, team) {
    if (missing(team)) team <- game_state$current_team
    if (team == "*") game_state$home_setter_position else if (team == "a") game_state$visiting_setter_position else NA_integer_
}

get_setter <- function(game_state, team) {
    pseq <- seq_len(6L) ## indoor only
    if (missing(team)) team <- game_state$current_team
    if (team == "*") {
        tryCatch(as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)])[game_state$home_setter_position], error = function(e) 0L)
    } else if (team == "a") {
        tryCatch(as.numeric(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)])[game_state$visiting_setter_position], error = function(e) 0L)
    } else {
        0L
    }
}

get_liberos <- function(game_state, team, dvw) {
    if (missing(team)) team <- game_state$current_team
    if (is_beach(dvw)) c() else if (team == "*") dvw$meta$players_h$number[dvw$meta$players_h$special_role %eq% "L"] else if (team == "a") dvw$meta$players_v$number[dvw$meta$players_v$special_role %eq% "L"] else c()
}

get_players <- function(game_state, team, dvw) {
    pseq <- seq_len(if (is_beach(dvw)) 2L else 6L)
    if (missing(team)) team <- game_state$current_team
    if (team == "*") {
        tryCatch(as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)]), error = function(e) rep(NA_integer_, length(pseq)))
    } else if (team == "a") {
        tryCatch(as.numeric(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)]), error = function(e) rep(NA_integer_, length(pseq)))
    } else {
        rep(NA_integer_, length(pseq))
    }
}

player_nums_to <- function(nums, team, dvw, to = "number lastname") {
    to <- match.arg(to, c("name", "number lastname"))
    temp_players <- if (team == "*") dvw$meta$players_h else if (team == "a") dvw$meta$players_v else stop("team should be '*' or 'a'")
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
    if (game_state$serving %eq% "*") {
        passing_team <- "a"
        passing_rot <- game_state$visiting_setter_position
        passing_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
        libs <- if (beach) c() else dvw$meta$players_v$number[dvw$meta$players_v$special_role %eq% "L"]

        ## Define the prior probability of passing given rotation, passing zone, etc... Defined as a simple mean of beta().
        passing_responsibility <- player_responsibility_fn(system = system, skill = "Reception",
                                                           setter_position = passing_rot,
                                                           zone = passing_zone, libs = libs, home_visiting = "visiting")

        passing_responsibility_prior <- setNames(rep(0, length(pseq) + 1L), c(paste0("visiting_p", pseq), "libero"))
        if (!is.na(passing_responsibility)) passing_responsibility_prior[passing_responsibility] <- 1

        ## Update the probability with the history of the game
        passing_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Reception",
                                         .data$visiting_setter_position %eq% as.character(passing_rot),
                                         .data$end_zone %eq% passing_zone,
                                         .data$team %eq% "a")

        passing_responsibility_posterior <- passing_responsibility_prior
        if(nrow(passing_history)>0){
          passing_history <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(dplyr::filter(tidyr::pivot_longer(dplyr::select(passing_history, "team", "player_number", paste0("visiting_p", pseq)), cols = paste0("visiting_p", pseq)), .data$value %eq% .data$player_number), .data$name), n_reception = dplyr::n()))
          passing_responsibility_posterior[passing_history$name] <- passing_responsibility_prior[passing_history$name] + passing_history$n_reception
          passing_responsibility_posterior <-  passing_responsibility_posterior / sum(passing_responsibility_posterior)
        }
        plsel_tmp <- names(sort(passing_responsibility_posterior, decreasing = TRUE))

        poc <- paste0("visiting_p", pseq) ## players on court
    } else if (game_state$serving %eq% "a") {
        passing_team <- "*"
        passing_rot <- game_state$home_setter_position
        passing_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
        libs <- if (beach) c() else dvw$meta$players_h$number[dvw$meta$players_h$special_role %eq% "L"]

        ## Define the prior probability of passing given rotation, passing zone, etc... Defined as a simple mean of beta().
        passing_responsibility <- player_responsibility_fn(system = system, skill = "Reception",
                                                           setter_position = passing_rot,
                                                           zone = passing_zone, libs = libs, home_visiting = "home")


        passing_responsibility_prior <- setNames(rep(0, length(pseq) + 1L), c(paste0("home_p", pseq),"libero"))
        if (!is.na(passing_responsibility)) passing_responsibility_prior[passing_responsibility] <- 1

        ## Update the probability with the history of the game
        passing_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Reception",
                                         .data$home_setter_position %eq% as.character(passing_rot),
                                         .data$end_zone %eq% passing_zone,
                                         .data$team %eq% "*")

        passing_responsibility_posterior <- passing_responsibility_prior
        if(nrow(passing_history)>0){
          passing_history <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(dplyr::filter(tidyr::pivot_longer(dplyr::select(passing_history, "team", "player_number",
                                                                                                                             paste0("home_p", pseq)), cols = paste0("home_p", pseq)),
                                                                                           .data$value %eq% .data$player_number), .data$name), n_reception = dplyr::n()))
          passing_responsibility_posterior[passing_history$name] <- passing_responsibility_prior[passing_history$name] + passing_history$n_reception
          passing_responsibility_posterior <-  passing_responsibility_posterior / sum(passing_responsibility_posterior)
        }
        plsel_tmp <- names(sort(passing_responsibility_posterior, decreasing = TRUE))

        poc <- paste0("home_p", pseq) ## players on court
    } else {
        return(list(choices = numeric(), selected = c()))
    }
    pp <- c(as.numeric(reactiveValuesToList(game_state)[poc]), libs)
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(reactiveValuesToList(game_state)[plsel_tmp[1]])
    ## passing player guess based on reception xy c(game_state$end_x, game_state$end_y), rotation, and assumed passing system
    ## for now, either the first libero or last player in pp
    list(choices = pp, selected = plsel)
}

guess_pass_quality <- function(game_state, dvw, home_end) {
    if (is_beach(dvw)) {
        ## TODO
        cat("beach, defaulting to '+' pass quality\n")
        return("+")
    }
    ## reference clicks to lower court
    do_flip_click <- (game_state$current_team == "*" && home_end == "upper") || (game_state$current_team == "a" && home_end == "lower")
    thisxy <- if (do_flip_click) as.numeric(dv_flip_xy(game_state$start_x, game_state$start_y)) else c(game_state$start_x, game_state$start_y)
    ## sets use "start" coordinates but "end" zone/subzone
    esz <- paste(dv_xy2subzone(game_state$start_x, game_state$start_y), collapse = "")
    ## TODO account for time since previous contact, and better boundaries here
    out <- if (thisxy[2] > 3.5) {
        "/" ## overpass
    } else if (thisxy[2] >= 3 & thisxy[1] >= 2  & thisxy[1] <= 3) {
        "#"
    } else if (thisxy[2] >= 3.25 & thisxy[1] >= 1.5  & thisxy[1] <= 3) {
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
    if (beach) {
        warning("guess_attack_player_options for beach not yet coded")
        ## placeholder code
        if (game_state$current_team %eq% "*") {
            poc <- paste0("home_p", pseq)
        } else {
            poc <- paste0("visiting_p", pseq)
        }
        pp <- as.numeric(reactiveValuesToList(game_state)[poc])
        plsel <- as.numeric(reactiveValuesToList(game_state)[poc[1]])
        return(list(choices = pp, selected = plsel))
    }
    ## else for indoor
    if (game_state$current_team %eq% "*") {
        attacking_team <- game_state$current_team
        setter_rot <- game_state$home_setter_position
        attacking_zone <- dv_xy2zone(game_state$start_x, game_state$start_y)

        ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
        attacking_responsibility <- player_responsibility_fn(system = system, skill = "Attack",
                                                             setter_position = setter_rot,
                                                             zone = attacking_zone, libs = NULL, home_visiting = "home", serving = game_state$serving %eq% "*")

        attacking_responsibility <- NA
        attacking_responsibility_prior <- setNames(rep(0, length(pseq)), c(paste0("home_p", pseq)))
        if (!is.na(attacking_responsibility)) attacking_responsibility_prior[attacking_responsibility] <- 1

        ## Update the probability with the history of the game
        attacking_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Attack",
                                           .data$home_setter_position %eq% as.character(setter_rot),
                                           .data$start_zone %eq% attacking_zone,
                                           .data$team %eq% "*")

        attacking_responsibility_posterior <- attacking_responsibility_prior
        if (nrow(attacking_history) > 0) {
            attacking_history <- dplyr::select(attacking_history, "team", "player_number", paste0("home_p", pseq)) %>% tidyr::pivot_longer(cols = paste0("home_p", pseq)) %>%
                dplyr::filter(.data$value %eq% .data$player_number) %>%
                dplyr::group_by(.data$name) %>% dplyr::summarize(n_attacks = dplyr::n()) %>% dplyr::ungroup()
            attacking_responsibility_posterior[attacking_history$name] <- attacking_responsibility_prior[attacking_history$name] + attacking_history$n_attacks
            attacking_responsibility_posterior <-  attacking_responsibility_posterior / sum(attacking_responsibility_posterior)
        }
        cat(str(attacking_responsibility_posterior))
        poc <- names(sort(attacking_responsibility_posterior, decreasing = TRUE))
    } else if (game_state$current_team %eq% "a") {
        attacking_team <- game_state$current_team
        setter_rot <- game_state$visiting_setter_position
        attacking_zone <- dv_xy2zone(game_state$start_x, game_state$start_y)

        ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
        attacking_responsibility <- player_responsibility_fn(system = system, skill = "Attack",
                                                             setter_position = setter_rot,
                                                             zone = attacking_zone, libs = NULL, home_visiting = "visiting", serving = game_state$serving %eq% "a")


        attacking_responsibility_prior <- setNames(rep(0, length(pseq)), c(paste0("visiting_p", pseq)))
        if (!is.na(attacking_responsibility)) attacking_responsibility_prior[attacking_responsibility] <- 1

        ## Update the probability with the history of the game
        attacking_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Attack",
                                           .data$visiting_setter_position %eq% as.character(setter_rot),
                                           .data$start_zone %eq% attacking_zone,
                                           .data$team %eq% "a")

        attacking_responsibility_posterior <- attacking_responsibility_prior
        if (nrow(attacking_history) > 0) {
            attacking_history <- dplyr::select(attacking_history, "team", "player_number", paste0("visiting_p", pseq)) %>%
                tidyr::pivot_longer(cols = paste0("visiting_p", pseq)) %>%
                dplyr::filter(.data$value %eq% .data$player_number) %>%
                dplyr::group_by(.data$name) %>% dplyr::summarise(n_attacks = dplyr::n()) %>% dplyr::ungroup()
            attacking_responsibility_posterior[attacking_history$name] <- attacking_responsibility_prior[attacking_history$name] + attacking_history$n_attacks
            attacking_responsibility_posterior <-  attacking_responsibility_posterior / sum(attacking_responsibility_posterior)
        }
        poc <- names(sort(attacking_responsibility_posterior, decreasing = TRUE))
    } else {
        return(list(choices = numeric(), selected = c()))
    }
    cat(str(poc))
    pp <- as.numeric(reactiveValuesToList(game_state)[poc])
    cat(str(pp))
    plsel <- as.numeric(reactiveValuesToList(game_state)[poc[1]])
    cat(str(plsel))
    list(choices = pp, selected = plsel)
}

guess_attack_code <- function(game_state, dvw, home_end, opts) {
    exclude_codes <- if (!missing(opts) && !is.null(opts$setter_dump_code)) opts$setter_dump_code else "PP"
    exclude_codes <- c(exclude_codes, if (!missing(opts) && !is.null(opts$second_ball_attack_code)) opts$second_ball_attack_code else "P2")
    exclude_codes <- c(exclude_codes, if (!missing(opts) && !is.null(opts$overpass_attack_code)) opts$overpass_attack_code else "PR")
    atbl <- dvw$meta$attacks %>% dplyr::filter(!.data$code %in% exclude_codes)
    do_flip_click <- (game_state$current_team == "*" && home_end == "upper") || (game_state$current_team == "a" && home_end == "lower")
    thisxy <- if (do_flip_click) as.numeric(dv_flip_xy(game_state$start_x, game_state$start_y)) else c(game_state$start_x, game_state$start_y)
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
    if (game_state$current_team %eq% "a") {
        defending_team <- game_state$current_team
        setter_rot <- game_state$visiting_setter_position
        attacking_zone <- dv_xy2zone(game_state$start_x, game_state$start_y)
        defending_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
        libs <- if (beach) c() else dvw$meta$players_v$number[dvw$meta$players_v$special_role %eq% "L"]

        ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
        dig_responsibility <- player_responsibility_fn(system = system, skill = "Dig",
                                                       setter_position = setter_rot,
                                                       zone = defending_zone, libs = libs, home_visiting = "visiting", opp_attack_start_zone = attacking_zone, serving = game_state$serving %eq% "a")


        dig_responsibility_prior <- setNames(rep(0, length(pseq)), c(paste0("visiting_p", pseq)))
        if (!is.na(dig_responsibility)) dig_responsibility_prior[dig_responsibility] <- 1

        ## Update the probability with the history of the game
        digging_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Dig",
                                         .data$visiting_setter_position %eq% as.character(setter_rot),
                                         .data$start_zone %eq% attacking_zone,
                                         .data$end_zone %eq% defending_zone,
                                         .data$team %eq% "a")

        dig_responsibility_posterior <- dig_responsibility_prior
        if(nrow(digging_history)>0){
            digging_history <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(dplyr::filter(tidyr::pivot_longer(dplyr::select(digging_history, "team", "player_number", paste0("visiting_p", pseq)), cols = paste0("visiting_p", pseq)), .data$value %eq% .data$player_number), .data$name), n_digs = dplyr::n()))
            dig_responsibility_posterior[digging_history$name] <- dig_responsibility_prior[digging_history$name] + digging_history$n_digs
            dig_responsibility_posterior <-  dig_responsibility_posterior / sum(dig_responsibility_posterior)
        }
        plsel_tmp <- names(sort(dig_responsibility_posterior, decreasing = TRUE))
        poc <- paste0("visiting_p", pseq)
    } else if (game_state$current_team %eq% "*") {
        defending_team <- game_state$current_team
        setter_rot <- game_state$home_setter_position
        attacking_zone <- dv_xy2zone(game_state$start_x, game_state$start_y)
        defending_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
        libs <- if (beach) c() else dvw$meta$players_h$number[dvw$meta$players_h$special_role %eq% "L"]

        ## Define the prior probability of attacking given rotation, attacking zone, etc... Defined as a simple mean of beta().
        dig_responsibility <- player_responsibility_fn(system = system, skill = "Dig",
                                                       setter_position = setter_rot,
                                                       zone = defending_zone, libs = libs, home_visiting = "home", opp_attack_start_zone = attacking_zone, serving = game_state$serving %eq% "*")


        dig_responsibility_prior <- setNames(rep(0, length(pseq)), c(paste0("visiting_p", pseq)))
        if (!is.na(dig_responsibility)) dig_responsibility_prior[dig_responsibility] <- 1

        ## Update the probability with the history of the game
        digging_history <- dplyr::filter(dvw$plays, .data$skill %eq% "Dig",
                                         .data$visiting_setter_position %eq% as.character(setter_rot),
                                         .data$start_zone %eq% attacking_zone,
                                         .data$end_zone %eq% defending_zone,
                                         .data$team %eq% "*")

        dig_responsibility_posterior <- dig_responsibility_prior
        if(nrow(digging_history)>0){
            digging_history <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(dplyr::filter(tidyr::pivot_longer(dplyr::select(digging_history, "team", "player_number", paste0("home_p", pseq)), cols = paste0("home_p", pseq)), .data$value %eq% .data$player_number), .data$name), n_digs = dplyr::n()))
            dig_responsibility_posterior[digging_history$name] <- dig_responsibility_prior[digging_history$name] + digging_history$n_digs
            dig_responsibility_posterior <-  dig_responsibility_posterior / sum(dig_responsibility_posterior)
        }
        plsel_tmp <- names(sort(dig_responsibility_posterior, decreasing = TRUE))
        poc <- paste0("home_p", pseq)
    } else {
        return(list(choices = numeric(), selected = c()))
    }
    pp <- c(as.numeric(reactiveValuesToList(game_state)[poc]), libs)
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(reactiveValuesToList(game_state)[plsel_tmp[1]])
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
    if (!is.na(selected)) selected <- if (selected %in% choices) which(choices == selected) else if (selected %in% names(choices)) which(names(choices) == selected) else if (!is.na(selected)) 1L
    clickfun <- if (nzchar(as_radio)) paste0(if (as_radio == "blankable") "var wa=$(this).hasClass('active');" else "var wa=false;", " $('.", cls, "').removeClass('active'); if (!wa) { $(this).addClass('active'); };") else "var wa=false;"
    buts <- lapply(seq_along(choices), function(i) tags$button(class = paste(c("btn", "btn-default", "fatradio", cls, extra_class, if (grepl("(L)", names(choices)[i], fixed = TRUE)) "libero", if (i %eq% selected && nzchar(as_radio)) "active"), collapse = " "), id = ids[i], HTML(names(choices)[i]), onclick = paste0(clickfun, " if (!wa) { Shiny.setInputValue('", input_var, "', '", choices[[i]], "', {priority: 'event'}) } else { Shiny.setInputValue('", input_var, "', null, {priority: 'event'}) }"), ...))
    ## set the initial value of the associated input variable, or clear it
    dojs(paste0("Shiny.setInputValue(\"", input_var, "\", ", if (!is.na(selected) && nzchar(as_radio)) paste0("\"", choices[[selected]], "\"") else "null", ")"))
    buts
}

plotOutputWithAttribs <- function(outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE, ...) {
    out <- shiny::plotOutput(outputId = outputId, width = width, height = height, click = click, dblclick = dblclick, hover = hover, brush = brush, inline = inline)
    rgs <- list(...)
    ## add extra attributes
    for (i in seq_along(rgs)) out$attribs[[names(rgs)[i]]] <- rgs[[i]]
    out
}
