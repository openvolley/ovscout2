#' Create a new datavolley match object
#'
#' @param match list or single-row data.frame: (optional) with components `date` (defaults to current date), `time` (defaults to current time), `season`, `league`, `phase`, `home_away`, `day_number`, `match_number`, `regulation`, `zones_or_cones`. `zones_or_cones` can also be provided directly
#' @param more list or single-row data.frame: (optional) with components `referees`, `spectators`, `receipts`, `city`, `arena`, `scout`
#' @param teams data.frame: a 2-row data frame, with required columns `team_id`, `team` and optional columns `coach`, `assistant`, `shirt_colour`
#' @param players_h,players_v data.frame: with required columns `number`, `firstname`, `lastname`, and optional columns `player_id`, `role` (character vector with "outside", "opposite", "middle", "libero", "setter"), `nickname`, `special_role` (character vector with "L", "C", or NA), `foreign` (logical, defaults to `FALSE`)
#' @param video_file string: (optional) path to video file
#' @param attacks data.frame: as returned by [ov_simplified_attack_table()] or [ov_default_attack_table()]
#' @param setter_calls data.frame: as returned by [ov_default_setter_calls_table()]
#' @param winning_symbols data.frame: as returned by [ov_default_winning_symbols()]
#' @param zones_or_cones string: "Z" or "C". Will be ignored if `zones_or_cones` is provided in the `match` parameter
#' @param regulation string: "indoor rally point", "beach rally point", or "indoor sideout". Will be ignored if `regulation` is provided in the `match` parameter
#' @param comments character: optional vector of length up to 5, of comments
#'
#' @return A datavolley object
#'
#' @examples
#' x <- dv_create(teams = data.frame(team_id = c("TM1", "TM2"), team = c("Team 1", "Team 2")),
#'                comments = "Test file",
#'                players_h = data.frame(firstname = toupper(letters[1:7]), lastname = "Player",
#'                                       number = 1:7),
#'                players_v = data.frame(firstname = letters[10:15], lastname = "VisPlayer",
#'                                       number = 10:15))
#'
#' ## enter the team lineups for set 1
#' x <- dv_set_lineups(x, set_number = 1, lineups = list(6:1, 15:10), setter_positions = c(2, 1))
#'
#' @export
dv_create <- function(match, more, teams, players_h, players_v, video_file, attacks = ov_simplified_attack_table(), setter_calls = ov_default_setter_calls_table(), winning_symbols = ov_default_winning_symbols(), zones_or_cones = "Z", regulation = "indoor rally point", comments) {
    reg <- if (!missing(match) && "regulation" %in% names(match)) match$regulation else regulation
    out <- list(##raw,
        messages = tibble(file_line_number = integer(), video_time = numeric(), message = character(), file_line = character()),
        file_meta = create_file_meta(file_type = if (grepl("beach", reg)) "beach" else "indoor"),
        meta = create_meta(match = match, more = more, teams = teams, players_h = players_h, players_v = players_v, video_file = video_file, attacks = attacks, setter_calls = setter_calls, winning_symbols = winning_symbols, zones_or_cones = zones_or_cones, regulation = regulation, comments = comments),
        plays = tibble(), plays2 = tibble(), game_state = NULL)
    ## plays is the full plays dataframe, plays2 is just the columns that appear in the dvw file directly
    structure(out, class = c("datavolley", "list"))
}

#' Enter the team lineups at the start of a set
#'
#' @param x datavolley: a datavolley object
#' @param set_number integer: set number, 1--3 for beach or 1--5 for indoor
#' @param lineups list: two-element list with numeric vectors of player numbers. Each lineup is
#' * for indoor, of length 6, 7, or 8 (first 6 are player jersey numbers in positions 1--6, elements 7 and 8 are optionally the libero jersey numbers)
#' * for beach, of length 2
#' @param setter_positions integer: two-element integer vector giving the position on court of the two setters. At least one of `setter_positions` or `setters` must be provided for indoor. Ignored for beach
#' @param setters integer: two-element integer vector giving the jersey numbers of the two setters. At least one of `setter_positions` or `setters` must be provided for indoor. Ignored for beach
#'
#' @return A modified version of `x`
#'
#' @export
dv_set_lineups <- function(x, set_number, lineups, setter_positions, setters) {
    assert_that(inherits(x, "datavolley"))
    assert_that(is.list(lineups), length(lineups) == 2)
    is_beach <- dv_is_beach(x)
    max_n_sets <- if (is_beach) 3 else 5
    if (set_number > max_n_sets) stop("set_number ", set_number, " is greater than the maximum expected")
    if (is_beach) {
        setter_positions <- setters <- c(NA_integer_, NA_integer_)
    } else {
        if (missing(setter_positions)) {
            if (!missing(setters)) {
                assert_that(length(setters) == 2)
                setter_positions <- c(NA_integer_, NA_integer_)
                if (setters[1] %in% lineups[[1]][1:6]) {
                    setter_positions[1] <- which(lineups[[1]] == setters[1])
                } else {
                    stop("home setter ", setters[1], " is not in lineup")
                }
                if (setters[2] %in% lineups[[2]][1:6]) {
                    setter_positions[2] <- which(lineups[[2]] == setters[2])
                } else {
                    stop("visiting setter ", setters[2], " is not in lineup")
                }
            } else {
                stop("specify either setters or setter_positions")
            }
        } else if (missing(setters)) {
            assert_that(length(setter_positions) == 2, all(setter_positions >= 1), all(setter_positions <= 6))
            setters <- c(lineups[[1]][setter_positions[1]], lineups[[2]][setter_positions[2]])
        }
    }
    ## update the starting positions of all players
    x <- set_lineup(x, set_number = set_number, team = "*", lineup = lineups[[1]])
    x <- set_lineup(x, set_number = set_number, team = "a", lineup = lineups[[2]])
    ## insert the start-of-set codes and lineups in the plays data
    ## e.g. "*P04>LUp"          "*z3>LUp"
    lineup_codes <- c(sprintf("*P%02d>Lup", setters[1]), paste0("*z", setter_positions[1], ">Lup"), sprintf("aP%02d>Lup", setters[2]), paste0("az", setter_positions[2], ">Lup"))
    add_to_plays2(x, codes = lineup_codes, set_number = set_number, home_setter_position = setter_positions[1], visiting_setter_position = setter_positions[2], home_lineup = lineups[[1]], visiting_lineup = lineups[[2]], scores = c(0L, 0L), serving = NA_character_)
}

#' Enter scout codes from the console
#'
#' Probably only useful for testing.
#'
#' @param x datavolley: a datavolley object as returned by [dv_create()]
#' @param prompt string: the prompt to show
#' @param compound_table tibble: the table of default compound codes
#' @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
#'
#' @return A modified version of `x`, with rows added to the plays2 component
#'
#' @export
dv_scout_from_console <- function(x, prompt = "SCOUT> ", compound_table = ov_default_compound_table(), default_scouting_table = ov_default_scouting_table()) {
    if (is.null(x$plays2) || nrow(x$plays2) < 1) {
        if (!is.null(x$plays) && nrow(x$plays) > 0) x$plays2 <- plays_to_plays2(x$plays)
        ## note that this might get the last element of the serving column wrong, to fix
    }
    if (nrow(x$plays2) < 1) stop("no plays2 data, do you need to run dv_set_lineup?")
    message("enter 'p' to assign the point to the home team, 'ap' to the visiting team, 'END' to exit before set has finished")
    message("  for testing: pzz, apzz to assign point and end the set")
    if (is.null(x$game_state)) {
        x$game_state <- tail(x$plays2, 1)
        if (!"serving" %in% names(x$game_state)) x$game_state$serving <- NA ## adjust this if starting partway through set TODO
    }
    while (TRUE) {
        rally_going <- TRUE
        rally_codes <- c()
        while (rally_going) {
            code <- readline(prompt)
            if (code == "END") break
            if (grepl("^undo", tolower(code))) {
                n_to_undo <- as.numeric(sub("undo[[:space:]]*", "", code, ignore.case = TRUE))
                if (is.na(n_to_undo)) n_to_undo <- 1L
                x <- undo(x, n = n_to_undo)
                if (length(rally_codes) >= n_to_undo) rally_codes <- head(rally_codes, -n_to_undo)
            } else if (code %in% c("T", "*T", "aT")) {
                ## timeout
                if (code == "T") code <- "*T"
                x <- add_timeout(x = x, team = substr(code, 1, 1))
            } else if (grepl("^[a\\*]?S$", code)) {
                x$game_state$serving <- if (grepl("^[S\\*]", code)) "*" else "a"
            } else if (grepl("^[a\\*]?C", code)) {
                ## substitution Cout.in or aCout.in
                if (!grepl("^[a\\*]?C[[:digit:]]+[\\.:][[:digit:]]+", code)) {
                    message("illegal sub code, ignoring")
                } else {
                    if (!grepl("^[a\\*]", code)) code <- paste0("*", code)
                    temp <- stringr::str_match(code, "C([[:digit:]]+)[\\.:]([[:digit:]]+)")
                    sub_out <- as.numeric(temp[1, 2])
                    sub_in <- as.numeric(temp[1, 3])
                    sub_team <- if (grepl("^[C\\*]", code)) "*" else "a"
                    x <- add_substitution(x = x, team = sub_team, player_out = sub_out, player_in = sub_in)
                }
            } else if (grepl("^[a\\*]?P", code)) {
                ## change of setter
                if (!grepl("^[a\\*]?P[[:digit:]]+$", code)) {
                    message("illegal setter code, ignoring")
                } else {
                    new_setter <- as.numeric(sub("^.*P", "", code))
                    team <- if (grepl("^[P\\*]", code)) "*" else "a"
                    x <- change_setter(x, team = team, new_setter = new_setter)
                }
            } else if (code %in% c("p", "ap", "pzz", "apzz")) {
                set_ended <- code %in% c("pzz", "apzz")
                code <- sub("zz", "", code)
                point_won_by <- if (code == "p") "*" else "a"
                rally_codes <- unlist(lapply(rally_codes, ov_code_interpret, attack_table = x$meta$attacks, compound_table = compound_table, default_scouting_table = default_scouting_table))
                x <- add_rally(x, codes = rally_codes, point_won_by = point_won_by)
                rally_going <- FALSE
                if (is_end_of_set(x)) {
                    x <- update_meta(x, set_ended = TRUE) ## update the meta$result section, etc
                    break
                }
            } else {
                rally_codes <- c(rally_codes, code)
            }
        }
        if (code == "END" || grepl("^\\*\\*[[:digit:]]set", code)) break
    }
    ## update match metadata
    x <- update_meta(x)
    ## reparse to populate full plays data frame
    dv_reparse(x)
}

## functions that manipulate the scouted match data. Currently called by dv_scout_from_console, but intended for use with other (graphical) scouting interfaces as well
undo <- function(x, n = 1L) {
    x$plays2 <- head(x$plays2, -n)
    x$game_state <- tail(x$plays2, 1)
    x
}

add_timeout <- function(x, team) {
    add_non_rally(x, codes = paste0(team, "T"))
}

add_substitution <- function(x, team, player_out, player_in) {
    team <- match.arg(team, c("*", "a"))
    pseq <- seq_len(if (dv_is_beach(x)) 2L else 6L)
    lup_cols <- if (team == "*") paste0("home_p", pseq) else paste0("visiting_p", pseq)
    this_lup <- as.numeric(x$game_state[, lup_cols])
    available_players <- if (team == "*") x$meta$players_h$number else x$meta$players_v$number
    if (!player_out %in% this_lup) {
        message("player being subbed out is not on court, ignoring sub")
    } else if (player_in %in% this_lup) {
        message("player being subbed in is already on court, ignoring sub")
    ##} else if (!player_in %in% available_players) {
    ##    message("player being subbed in is not in player list, ignoring sub")
    } else {
        this_lup[this_lup == player_out] <- player_in
        x$game_state[, lup_cols] <- as.list(this_lup)
        message(if (team == "*") "home" else "visiting", " team player ", player_in, " in for player ", player_out)
        x <- add_non_rally(x, codes = paste0(team, "C", player_out, ".", player_in))
    }
    x
}

change_setter <- function(x, team, new_setter) {
    team <- match.arg(team, c("*", "a"))
    pseq <- seq_len(if (dv_is_beach(x)) 2L else 6L)
    lup_cols <- if (team == "*") paste0("home_p", pseq) else paste0("visiting_p", pseq)
    this_lup <- as.numeric(x$game_state[, lup_cols])
    if (!new_setter %in% this_lup) {
        message("new setter ", new_setter, " is not on court, ignoring new setter code")
    } else {
        setter_pos <- which(this_lup == new_setter)
        if (team == "*") x$game_state$home_setter_position <- setter_pos else x$game_state$visiting_setter_position <- setter_pos
        message(if (team == "*") "home" else "visiting", " team player ", new_setter, " is now the setter on court")
        x <- add_non_rally(x, codes = paste0(team, "P", new_setter))
    }
    x
}

add_non_rally <- function(x, codes) {
    add_to_plays2(x, codes = codes)
}

add_rally <- function(x, codes, point_won_by) {
    is_beach <- dv_is_beach(x)
    pseq <- seq_len(if (is_beach) 2L else 6L)
    temp <- na.omit(stringr::str_match(codes, "^([a\\*]?)[[:digit:]]+S")[, 2])
    if (length(temp) == 1) {
        if (nzchar(temp)) temp <- "*" ## was NNS, missing the leading *
        this_serving <- temp
        if (!is.na(x$game_state$serving) && this_serving != x$game_state$serving) {
            message("team ", this_serving, " has been scouted as serving, but I think team ", x$game_state$serving, " is serving")
        }
    } else {
        ## no serve in this rally
        if (!is.na(x$game_state$serving)) {
            this_serving <- x$game_state$serving
        } else {
            stop("don't know who is serving") ## could happen on first point of set and rotation error scouted without serve, to deal with
        }
    }
    ## add the [*a]pXX:YY code at the end of the rally
    scores_end_of_point <- as.numeric(x$game_state[, c("home_score_start_of_point", "visiting_score_start_of_point")]) + as.integer(c(point_won_by == "*", point_won_by == "a"))
    pcode <- paste0(point_won_by, "p", sprintf("%02d:%02d", scores_end_of_point[1], scores_end_of_point[2]))
    ## add green codes if needed
    codes <- make_auto_codes(c(codes, pcode), x)
    x <- add_to_plays2(x, codes = codes, serving = this_serving)
    ## update game_state
    do_rot <- point_won_by != this_serving
    x$game_state$serving <- point_won_by
    if (point_won_by == "*") {
        x$game_state$home_score_start_of_point <- x$game_state$home_score_start_of_point + 1L
    } else {
        x$game_state$visiting_score_start_of_point <- x$game_state$visiting_score_start_of_point + 1L
    }
    if (do_rot) {
        if (point_won_by == "*") {
            x$game_state$home_setter_position <- rotpos(x$game_state$home_setter_position, n = length(pseq))
            x$game_state[, paste0("home_p", pseq)] <- as.list(rotvec(as.numeric(x$game_state[, paste0("home_p", pseq)])))
            poscode <- paste0("*z", x$game_state$home_setter_position)
        } else {
            x$game_state$visiting_setter_position <- rotpos(x$game_state$visiting_setter_position, n = length(pseq))
            x$game_state[, paste0("visiting_p", pseq)] <- as.list(rotvec(as.numeric(x$game_state[, paste0("visiting_p", pseq)])))
            poscode <- paste0("az", x$game_state$visiting_setter_position)
        }
        x <- add_to_plays2(x, codes = poscode)
    }
    x
}

## update the match metadata:
## - played, partial set scores, score, duration, score_home_team, score_visiting_team in meta$result
## - sets_won, won_match in meta$teams
## - starting positions and substitutions in meta$players_h and meta$players_v
update_meta <- function(x, set_ended = FALSE) {
    is_beach <- any(grepl("beach", x$meta$match$regulation))
    pseq <- seq_len(if (is_beach) 2L else 6L)
    if (set_ended) {
        ## only call this as the set ends, i.e. the last row in x$plays2 is the last action of the set
        message("set ", x$game_state$set_number, " ended")
        x <- add_non_rally(x, codes = paste0("**", x$game_state$set_number, "set"))
        ## add row(s) to x$meta$result if needed
        if (nrow(x$meta$result) < x$game_state$set_number) {
            x$meta$result <- bind_rows(x$meta$result, x$meta$result[0, ][rep(1, x$game_state$set_number - nrow(x$meta$result)), ]) ## add all-NA row(s)
        }
        set_start_rows <- which(grepl(">Lup", x$plays2$code) & !grepl(">Lup", dplyr::lag(x$plays2$code)))
        if (length(set_start_rows) == x$game_state$set_number) {
            ## duration
            ## only do this for the set just finished, because this could be edited (or clock times edited) later
            set_start_end_time <- range(x$plays2$time[seq(tail(set_start_rows, 1), nrow(x$plays2))], na.rm = TRUE)
            x$meta$result$duration[x$game_state$set_number] <- as.numeric(difftime(set_start_end_time[2], set_start_end_time[1], units = "min"))
        }
    }
    ## update all set results, excluding durations
    set_start_rows <- which(grepl(">Lup", x$plays2$code) & !grepl(">Lup", dplyr::lag(x$plays2$code)))
    x$meta$result$played[seq_along(set_start_rows)] <- TRUE
    set_end_rows <- grep("^\\*\\*[[:digit:]]set", x$plays2$code)
    if (length(set_start_rows) == (length(set_end_rows) + 1) && length(set_start_rows) > 1 && all(set_end_rows > head(set_start_rows, -1))) {
        ## we have an uncompleted set, that isn't the first set
        ## so we can still update the meta for the earlier sets
        set_start_rows <- head(set_start_rows, -1)
    }
    if (length(set_start_rows) == length(set_end_rows) && all(set_end_rows > set_start_rows)) {
        sets_won <- c(0L, 0L) ## sets won by home, visiting teams
        for (si in seq_along(set_start_rows)) {
            message("updating scores for set ", si)
            set_plays2 <- x$plays2[seq(set_start_rows[si], set_end_rows[si]), ]
            ## scores
            scores <- c(max(set_plays2$home_score_start_of_point, na.rm = TRUE), max(set_plays2$visiting_score_start_of_point, na.rm = TRUE))
            x$meta$result$score[si] <- paste0(scores[1], "-", scores[2])
            x$meta$result$score_home_team[si] <- scores[1]
            x$meta$result$score_visiting_team[si] <- scores[2]
            ## sets won
            if (is_beach) {
                if (max(scores) >= 21 && abs(diff(scores)) >= 2) {
                    sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                }
            } else {
                if ((si < 5 && max(scores) >= 25 && abs(diff(scores)) >= 2) || max(scores) >= 15 && abs(diff(scores)) >= 2) {
                    sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                }
            }
            iss <- if (is_beach) c(5, 10, 15) else if (si == 5) c(5, 10, 12) else c(8, 16, 21) ## intermediate score levels
            for (issi in seq_along(iss)) {
                idx <- which(set_plays2$home_score_start_of_point == iss[issi] | set_plays2$visiting_score_start_of_point == iss[issi])
                if (length(idx) > 0) {
                    idx <- min(idx)
                    if (!is.na(set_plays2$home_score_start_of_point[idx]) && !is.na(set_plays2$visiting_score_start_of_point[idx])) {
                        x$meta$result[[paste0("score_intermediate", issi)]][si] <- paste0(set_plays2$home_score_start_of_point[idx], "-", set_plays2$visiting_score_start_of_point[idx])
                    }
                }
            }
        }
        x$meta$teams$sets_won <- sets_won
        if ((is_beach && max(sets_won) > 1) || max(sets_won) > 2) x$meta$teams$won_match <- (sets_won == max(sets_won))
    }
    ## starting lineups and subs
    ## this can be done even for sets that haven't been completed
    for (si in seq_len(max(x$plays2$set_number, na.rm = TRUE))) {
        ## use the final >Lup row for starting lineup
        final_lup_row <- which(x$plays2$set_number == si & grepl(">Lup", x$plays2$code))
        if (length(final_lup_row) > 0) final_lup_row <- max(final_lup_row)
        if (length(final_lup_row) == 1) {
            home_starting_lineup <- as.numeric(x$plays2[final_lup_row, paste0("home_p", pseq)])
            for (j in seq_along(home_starting_lineup)) {
                pl_row <- which(x$meta$players_h$number == home_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_h[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
            ## subs
            all_home_pl <- unique(na.omit(as.numeric(unlist(x$plays2[which(x$plays2$set_number == si & !grepl(">Lup", x$plays2$code)), paste0("home_p", pseq)]))))
            home_subs <- setdiff(all_home_pl, home_starting_lineup)
            x$meta$players_h[[paste0("starting_position_set", si)]][x$meta$players_h$number %in% home_subs] <- "*"
            ## visiting team
            visiting_starting_lineup <- as.numeric(x$plays2[final_lup_row, paste0("visiting_p", pseq)])
            for (j in seq_along(visiting_starting_lineup)) {
                pl_row <- which(x$meta$players_v$number == visiting_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_v[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
            ## subs
            all_visiting_pl <- unique(na.omit(as.numeric(unlist(x$plays2[which(x$plays2$set_number == si & !grepl(">Lup", x$plays2$code)), paste0("visiting_p", pseq)]))))
            visiting_subs <- setdiff(all_visiting_pl, visiting_starting_lineup)
            x$meta$players_v[[paste0("starting_position_set", si)]][x$meta$players_v$number %in% visiting_subs] <- "*"
        }
    }
    x
}

is_end_of_set <- function(x) {
    scores <- c(x$game_state$home_score_start_of_point, x$game_state$visiting_score_start_of_point)
    if (dv_is_beach(x)) {
        if (max(scores) >= 21 && abs(diff(scores)) >= 2) return(TRUE)
    } else {
        if (((max(scores) >= 25 && x$game_state$set_number < 5) || (max(scores) >= 15 && x$game_state$set_number == 5)) && abs(diff(scores)) >= 2) return(TRUE)
    }
    FALSE
}

## write the scouted match to a file, using the plays2 data instead of the plays
## then it is possible to read that file back in, which will populate the full plays data.frame without having to duplicate the code needed to do this
dv_write2 <- function(x, file, text_encoding = "UTF-8") {
    plays_saved <- x$plays
    x$plays <- tibble()
    ## write without the plays part
    dv_write(x, file = file, text_encoding = text_encoding)
    ## now the plays but from plays2
    x$plays2$time <- format(x$plays2$time, "%H.%M.%S")

    ## TODO, modify other columns
    ##this <- xp$point_phase
    ##this[this %eq% "Breakpoint"] <- "p"
    ##this[this %eq% "Sideout"] <- "s"
    ##xp$point_phase <- this
    ##this <- xp$attack_phase
    ##this[this %eq% "Transition breakpoint"] <- "p"
    ##this[this %eq% "Transition sideout"] <- "s"
    ##this[this %eq% "Reception"] <- "r"
    ##xp$attack_phase <- this

    ## setter position uses 0 or 5 when unknown
    x$plays2$home_setter_position[is.na(x$plays2$home_setter_position)] <- 5
    x$plays2$visiting_setter_position[is.na(x$plays2$visiting_setter_position)] <- 5

    ##x$plays2 <- x$plays2[, setdiff(names(x$plays2), c("home_score_start_of_point", "visiting_score_start_of_point", "serving"))]
    x$plays2$na_col <- NA
    ## make sure we have the right columns, including the all-NA ones
    nms <- c("code", "point_phase", "attack_phase", "na_col", ## cols 1-4
             "start_coordinate", "mid_coordinate", "end_coordinate", ## cols 5-7
             "time", ## col 8, HH.MM.SS format
             "set_number", ## col 9
             "home_setter_position", "visiting_setter_position", # 10-11
             "video_file_number", "video_time", ## 12-13
             "na_col", ## 14 need to check this one
             if ("home_p3" %in% names(x$plays2)) paste0("home_p", 1:6) else c(paste0("home_p", 1:2), rep("na_col", 4)), ## 15-20, home team, court positons 1-6, entries are player numbers
             if ("visiting_p3" %in% names(x$plays2)) paste0("visiting_p", 1:6) else c(paste0("visiting_p", 1:2), rep("na_col", 4)), ## 21-26, same for visiting team
             "na_col" ## always a trailing ;
             )
    x$plays2 <- x$plays2[, nms]
    out <- df2txt(x$plays2)
    outf <- file(file, "ab")
    writeLines(out, outf, sep = "\n")
    close(outf)
    invisible(file)
}

dv_reparse <- function(x, text_encoding = "UTF-8") {
    ## write and re-read, which will populate the full x$plays data.frame
    tf <- tempfile(fileext = ".dvw")
    on.exit(unlink(tf))
    out <- dv_read(dv_write2(x, tf), encoding = "UTF-8")
    ## repopulate these
    out$game_state <- x$game_state
    out$plays2 <- x$plays2
    out
}


## ---
## support functions, probably not necessary to call these directly

set_lineup <- function(x, set_number, team, lineup) {
    ##assert_that(is.string(team))
    ##if (tolower(team) == "home" || team %eq% datavolley::home_team(x) || team %eq% datavolley::home_team_id(x)) team <- "*"
    ##if (tolower(team) == "visiting" || team %eq% datavolley::visiting_team(x) || team %eq% datavolley::visiting_team_id(x)) team <- "a"
    if (!team %in% c("*", "a")) stop("team ", team, " is not recognized")
    assert_that(is.numeric(lineup))
    if (dv_is_beach(x)) {
        assert_that(length(lineup) == 2)
    } else {
        assert_that(length(lineup) %in% c(6, 7, 8))
    }
    players <- if (team == "*") x$meta$players_h else x$meta$players_v
    ## update the starting positions of these players
    sp <- rep(NA_character_, nrow(players))
    for (i in seq_along(lineup)) sp[which(players$number == lineup[i])] <- as.character(i)
    for (i in intersect(seq_along(lineup), 7:8)) sp[which(players$number == lineup[i])] <- "*" ## liberos
    players[[paste0("starting_position_set", set_number)]] <- sp
    if (team == "*") x$meta$players_h <- players else x$meta$players_v <- players
    x
}

create_file_meta <- function(file_type = "indoor", date_format = "%Y/%m/%d") {
    tdnow <- time_but_utc()
    tdformat <- function(z) {
        if (is.character(z)) {
            z ## as-is
        } else {
            tryCatch(format(z, paste0(date_format, " %H.%M.%S")), error = function(e) NULL)
        }
    }
    tibble(fileformat = "2.0",
           generator_day = tdformat(tdnow),
           generator_idp = "ovscout", generator_prg = "ovscout-R", generator_rel = packageVersion("ovscout"), generator_ver = "", generator_nam = "",
           lastchange_day = tdformat(tdnow),
           lastchange_idp = "ovscout", lastchange_prg = "ovscout-R", lastchange_rel = packageVersion("ovscout"), lastchange_ver = "", lastchange_nam = "",
           preferred_date_format = tolower(gsub("[%/]", "", date_format)), file_type = file_type)
}

create_meta <- function(match, more, teams, players_h, players_v, video_file, attacks, setter_calls, winning_symbols, zones_or_cones, regulation, comments) {
    meta <- list()
    ## match
    if (missing(match)) match <- list()
    assert_that(is.list(match))
    if (is.data.frame(match)) assert_that(nrow(match) == 1)
    meta$match <- tibble(date = nn_or(match$date, Sys.Date()),
                         time = nn_or(match$time, time_but_utc()),
                         season = nn_or(match$season),
                         league = nn_or(match$league),
                         phase = nn_or(match$phase),
                         home_away = nn_or(match$home_away, NA),
                         day_number = nn_or(match$day_number, NA_integer_),
                         match_number = nn_or(match$match_number, NA_integer_),
                         text_encoding = "UTF-8",
                         regulation = nn_or(match$regulation, regulation),
                         zones_or_cones = nn_or(match$zones_or_cones, zones_or_cones),
                         X12 = NA)
    if (inherits(meta$match$time, "POSIXt")) meta$match$time <- format(meta$match$time, "%HH %MM %SS")
    meta$match$time <- tryCatch(lubridate::period(meta$match$time), error = function(e) NA)
    if (!meta$match$regulation %in% c("indoor sideout", "indoor rally point", "beach rally point")) stop("regulation: '", meta$match$regulation, "' is unrecognized")

    ## more
    if (missing(more)) more <- list()
    assert_that(is.list(more))
    if (is.data.frame(more)) assert_that(nrow(more) == 1)
    meta$more <- tibble(referees = nn_or(more$referees), spectators = nn_or(more$spectators), receipts = nn_or(more$receipts), city = nn_or(more$city), arena = nn_or(more$arena), scout = nn_or(more$scout), X7 = NA)

    ## comments
    if (missing(comments)) comments <- character()
    assert_that(is.character(comments))
    comments <- c(comments, rep(NA_character_, 5 - length(comments)))
    meta$comments <- setNames(as.data.frame(as.list(comments)), paste0("comment_", 1:5))

    ## result
    meta$result <- tibble(played = logical(), score_intermediate1 = character(), score_intermediate2 = character(), score_intermediate3 = character(), score = character(), duration = numeric(), X7 = logical(), score_home_team = numeric(), score_visiting_team = numeric())

    ## teams
    assert_that(is.data.frame(teams), nrow(teams) == 2)
    meta$teams <- tibble(team_id = teams$team_id, team = teams$team,
                         sets_won = c(0L, 0L),
                         coach = nn_or(teams$coach, c("", "")),
                         assistant = nn_or(teams$assistant, c("", "")),
                         shirt_colour = nn_or(teams$shirt_colour, c("#FF0000", "#0000FF")),
                         X7 = NA, X8 = NA, X9 = NA, X10 = NA,
                         home_away_team = c("*", "a"),
                         won_match = c(NA, NA))
    ## players
    meta$players_h <- make_players(players_h, "home")
    meta$players_v <- make_players(players_v, "visiting")
    meta$players_v$X3 <- meta$players_v$X3 + nrow(players_h)
    pids <- c(meta$players_h$player_id, meta$players_v$player_id)
    if (any(duplicated(pids))) {
        dup <- pids[duplicated(pids)]
        stop("duplicated player_id(s): ", paste(dup, collapse = ", "), ". If these have been automatically generated you might need to explicitly provide them in the players_h and players_v data.frames")
    }
    roles <- c("libero", "outside", "opposite", "middle", "setter", "unknown")
    if (!all(meta$players_h$role %in% roles)) {
        warning("unknown role(s) in home players list: ", paste(setdiff(meta$players_h$role, roles), collapse = ", "), ". Replacing with 'unknown'")
        meta$players_h$role[!meta$players_h$role %in% roles] <- "unknown"
    }
    if (!all(meta$players_v$role %in% roles)) {
        warning("unknown role(s) in visiting players list: ", paste(setdiff(meta$players_v$role, roles), collapse = ", "), ". Replacing with 'unknown'")
        meta$players_v$role[!meta$players_v$role %in% roles] <- "unknown"
    }
    min_n_players <- if (grepl("beach", meta$match$regulation)) 2L else 6L
    if (nrow(meta$players_h) < min_n_players) stop("less than ", min_n_players, " players in home player list")
    if (nrow(meta$players_v) < min_n_players) stop("less than ", min_n_players, " players in visiting player list")

    meta$attacks <- attacks
    meta$sets <- setter_calls
    meta$winning_symbols <- winning_symbols
    ## match_id
    meta$video <- if (!missing(video_file)) tibble(camera = "Camera0", file = video_file) else tibble(camera = character(), file = character())
    meta$filename <- ""

    meta
}

make_players <- function(players, which = "home") {
    assert_that(is.data.frame(players))
    req <- c("lastname", "firstname", "number")
    if (!all(req %in% names(players))) {
        stop(which, " players data.frame is missing columns: ", paste(setdiff(req, names(players)), collapse = ", "))
    }
    if (!"player_id" %in% names(players)) players$player_id <- make_player_id(players$lastname, players$firstname)
    if (!"role" %in% names(players)) players$role <- "unknown"
    temp <- tibble(X1 = 0,
                   number = players$number,
                   X3 = seq_len(nrow(players)),
                   starting_position_set1 = NA_character_,
                   starting_position_set2 = NA_character_,
                   starting_position_set3 = NA_character_,
                   starting_position_set4 = NA_character_,
                   starting_position_set5 = NA_character_,
                   player_id = players$player_id,
                   lastname = players$lastname,
                   firstname = players$firstname,
                   nickname = nn_or(players$nickname, ""),
                   special_role = toupper(nn_or(players$special_role, NA_character_)),
                   role = tolower(players$role),
                   foreign = nn_or(players$foreign, FALSE))
    temp$X16 <- temp$X17 <- temp$X18 <- temp$X19 <- temp$X20 <- temp$X21 <- temp$X22 <- temp$X23 <- NA
    temp$name <- paste(temp$firstname, temp$lastname)
    temp
}
make_player_id <- function(lastname, firstname) toupper(paste0(substr(lastname, 1, 3), "-", substr(firstname, 1, 3)))

## add rows to x$plays2
## if any of the set_number, home_setter_position, or following params are provided, they override what is provided in x$game_state. If all are provided, don't need to provide x
add_to_plays2 <- function(x, codes, video_time, set_number, home_setter_position, visiting_setter_position, home_lineup, visiting_lineup, scores = c(0L, 0L), serving = NA_character_) {
    pseq <- seq_len(if (dv_is_beach(x)) 2L else 6L)
    if (missing(set_number)) set_number <- x$game_state$set_number
    if (missing(home_setter_position)) home_setter_position <- x$game_state$home_setter_position
    if (missing(visiting_setter_position)) visiting_setter_position <- x$game_state$visiting_setter_position
    if (missing(home_lineup)) home_lineup <- as.numeric(x$game_state[, paste0("home_p", pseq)])
    if (missing(visiting_lineup)) visiting_lineup <- as.numeric(x$game_state[, paste0("visiting_p", pseq)])
    if (missing(scores)) scores <- as.numeric(x$game_state[, c("home_score_start_of_point", "visiting_score_start_of_point")])
    if (missing(serving)) serving <- x$game_state$serving

    vt <- if (!missing(video_time)) video_time else NA_integer_
    out <- tibble(code = as.character(codes), ## col 1
                  point_phase = NA_character_, ## col 2
                  attack_phase = NA_character_, ## col 3
##                  X4 = NA, ## col 4
                  start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_, ## cols 5-7
                  time = time_but_utc(), ## col 8
                  set_number = set_number, home_setter_position = home_setter_position, visiting_setter_position = visiting_setter_position, ## cols 9-11, NB these 3 not used directly in dv_read
                  video_file_number = NA, video_time = vt, ## cols 12-13
  ##                X14 = NA, ## col 14
                  home_score_start_of_point = scores[1],
                  visiting_score_start_of_point = scores[2],
                  serving = serving)
    assert_that(length(home_lineup) == 6)
    assert_that(length(visiting_lineup) == 6)
    out <- bind_cols(out, setNames(as.data.frame(as.list(home_lineup)), paste0("home_p", 1:6))) ## cols 15-20
    out <- bind_cols(out, setNames(as.data.frame(as.list(visiting_lineup)), paste0("visiting_p", 1:6))) ## cols 21-26
    x$plays2 <- bind_rows(x$plays2, out)
    x
}

## generate the auto-codes necessary to make a complete rally
## @param code character: vector of scouted codes, only those relating to skill executions along with the "*p" or "ap" code at the end
make_auto_codes <- function(code, x) {
    if (!grepl("^[a\\*]p[[:digit:]]+:[[:digit:]]+", tail(code, 1))) stop("the final element of the code vector should be the '*p' or 'ap' code")
    won_by <- substr(tail(code, 1), 1, 1)
    if (!won_by %in% c("*", "a")) stop("won_by must be '*' or 'a'")
    team_char <- substr(code, 1, 1)
    team_char[!team_char %in% c("*", "a")] <- NA_character_ ## "*" or "a"
    skill <- substr(code, 4, 4)
    skill[!skill %in% c("S", "R", "A", "B", "D", "E", "F")] <- NA_character_
    evaluation_code <- substr(code, 6, 6)
    evaluation_code[!evaluation_code %in% c("#", "+", "!", "-", "/", "=")] <- NA_character_
    wswin <- x$meta$winning_symbols$win_lose == "W"
    wsidx <- skill %in% x$meta$winning_symbols$skill[wswin] & evaluation_code %in% x$meta$winning_symbols$code[wswin]
    lsidx <- skill %in% x$meta$winning_symbols$skill[!wswin] & evaluation_code %in% x$meta$winning_symbols$code[!wswin]
    green_codes <- c()
    lost_by <- setdiff(c("*", "a"), won_by)
    if (sum(team_char %eq% won_by & wsidx) < 1) {
        green_codes <- paste0(won_by, "$$&H#")
    } else if (sum(team_char %eq% lost_by & wsidx) > 0) {
        warning("winning code for wrong team?")
    }
    if (sum(team_char %eq% lost_by & lsidx) < 1) {
        green_codes <- c(green_codes, paste0(lost_by, "$$&H="))
    } else if (sum(team_char %eq% won_by & lsidx) > 0) {
        warning("losing code for wrong team?")
    }
    c(head(code, -1), green_codes, tail(code, 1))
}

df2txt <- function(z) {
    ## change any logical cols to char "True" "False"
    logical2char <- function(w) {
        out <- as.character(w)
        out[which(w)] <- "True"
        out[which(!w)] <- "False"
        out
    }
    findlogicalcols <- function(w) {
        vapply(seq_len(ncol(w)), function(ci) is.logical(w[[ci]]) & !all(is.na(w[[ci]])), FUN.VALUE = TRUE)
    }
    for (lc in which(findlogicalcols(z))) z[[lc]] <- logical2char(z[[lc]])
    ## convert period cols to text
    findperiodcols <- function(w) vapply(seq_len(ncol(w)), function(ci) lubridate::is.period(w[[ci]]), FUN.VALUE = TRUE)
    ldz <- function(nn, width = 2) formatC(nn, flag = "0", width = width) ## leading zeros
    for (pc in which(findperiodcols(z))) {
        nnaidx <- which(!is.na(z[[pc]]))
        temp <- rep("", nrow(z))
        temp[nnaidx] <- paste0(ldz(lubridate::hour(z[[pc]][nnaidx])), ".", ldz(lubridate::minute(z[[pc]][nnaidx])), ".", ldz(lubridate::second(z[[pc]][nnaidx])))
        z[[pc]] <- temp
    }
    capture.output(data.table::fwrite(z, sep = ";", col.names = FALSE, row.names = FALSE, quote = FALSE, na = ""))
}

rotpos <- function(p, n = 6L) (p - 2) %% n + 1
rotvec <- function(z) c(z[-1], z[1])
dv_is_beach <- function(z) any(grepl("beach", z$meta$match$regulation))
time_but_utc <- function() {
    nw <- Sys.time()
    lubridate::tz(nw) <- "UTC"
    round(nw)
}

plays_to_plays2 <- function(p) {
    p2 <- mutate(p, serving = case_when(.data$serving_team == .data$home_team ~ "*", .data$serving_team == .data$visiting_team ~ "a"))##, X4 = NA, X14 = NA)
    as_tibble(dplyr::select(p2, "code", "point_phase", "attack_phase", ##"X4",
                            "start_coordinate", "mid_coordinate", "end_coordinate", "time", "set_number", "home_setter_position", "visiting_setter_position", "video_file_number", "video_time", ##"X14",
                            "home_score_start_of_point", "visiting_score_start_of_point", "serving", "home_p1", "home_p2", "home_p3", "home_p4", "home_p5", "home_p6", "visiting_p1", "visiting_p2", "visiting_p3", "visiting_p4", "visiting_p5", "visiting_p6"))
}

## parse just the skill rows, giving the team, player number, skill and type, and evaluation code
minimal_parse_code <- function(codes) {
    code_split <- stringr::str_match_all(codes, "^([a\\*])([[:digit:]][[:digit:]])([SRABDEF])([HMQTUNO])([#\\+!/=\\-])")
    na_row <- as.list(rep(NA, 5))
    parsed <- setNames(as.data.frame(cbind(codes, do.call(rbind, lapply(code_split, function(this) if (nrow(this) < 1) na_row else as.list(this[1, -1]))))), c("code", "team_code", "player_number", "skill_code", "skill_type_code", "evaluation_code"))
    parsed
}
