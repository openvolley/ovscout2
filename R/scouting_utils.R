#' Create a new datavolley match object
#'
#' @param match list or single-row data.frame: (optional) with components `date` (defaults to current date), `time` (defaults to current time), `season`, `league`, `phase`, `home_away`, `day_number`, `match_number`, `regulation`, `zones_or_cones`. `zones_or_cones` can also be provided directly
#' @param more list or single-row data.frame: (optional) with components `referees`, `spectators`, `receipts`, `city`, `arena`, `scout`
#' @param teams data.frame: a 2-row data frame describing the home and visiting teams, with required columns `team_id`, `team` and optional columns `coach`, `assistant`, `shirt_colour`. The home team must be in the first row of this data frame
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
        plays = tibble(), plays2 = tibble(code = character(), set_number = integer()), game_state = NULL)
    ## plays is the full plays dataframe, plays2 is just the columns that appear in the dvw file directly. Minimal set of plays2 columns initialized here, just to keep downstream code quiet (no warnings about missing columns)
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
    ht_libs <- c(tail(lineups[[1]], -6), rep(NA_integer_, 8 - length(lineups[[1]])))
    vt_libs <- c(tail(lineups[[2]], -6), rep(NA_integer_, 8 - length(lineups[[2]])))
    ## insert the start-of-set codes and lineups in the plays data
    ## e.g. "*P04>LUp"          "*z3>LUp"
    lineup_codes <- c(sprintf("*P%02d>LUp", setters[1]), paste0("*z", setter_positions[1], ">LUp"), sprintf("aP%02d>LUp", setters[2]), paste0("az", setter_positions[2], ">LUp"))
    ## add rows to x$plays2
    pseq <- seq_len(if (is_beach) 2L else 6L)
    home_lineup <- lineups[[1]][pseq]
    visiting_lineup <- lineups[[2]][pseq]
    p2x <- tibble(code = as.character(lineup_codes), ## col 1
                  point_phase = NA_character_, ## col 2
                  attack_phase = NA_character_, ## col 3
                  ##                  X4 = NA, ## col 4
                  start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_, ## cols 5-7
                  time = time_but_utc(), ## col 8
                  set_number = set_number, home_setter_position = setter_positions[1], visiting_setter_position = setter_positions[2], ## cols 9-11, NB these 3 not used directly in dv_read
                  video_file_number = NA, video_time = NA_integer_, ## cols 12-13
                  ##                X14 = NA, ## col 14
                  home_score_start_of_point = 0L,
                  visiting_score_start_of_point = 0L,
                  serving = NA_character_)
    p2x <- bind_cols(p2x, setNames(as.data.frame(as.list(home_lineup)), paste0("home_p", 1:6))) ## cols 15-20
    p2x <- bind_cols(p2x, setNames(as.data.frame(as.list(visiting_lineup)), paste0("visiting_p", 1:6))) ## cols 21-26
    p2x$ht_lib1 <- ht_libs[1]
    p2x$ht_lib2 <- ht_libs[2]
    p2x$vt_lib1 <- vt_libs[1]
    p2x$vt_lib2 <- vt_libs[2]
    x$plays2 <- bind_rows(x$plays2, p2x)
    x
}

## update the match metadata:
## - played, partial set scores, score, duration, score_home_team, score_visiting_team in meta$result
## - sets_won, won_match in meta$teams
## - starting positions and substitutions in meta$players_h and meta$players_v
update_meta <- function(x) {## used to have this but was only used in console test scouting ## , set_ended = FALSE) {
    is_beach <- any(grepl("beach", x$meta$match$regulation))
    pseq <- seq_len(if (is_beach) 2L else 6L)
    if (is.null(x$plays2) || nrow(x$plays2) < 1) return(x)
    ## update all set results, including durations
    set_start_rows <- which(grepl(">LUp", x$plays2$code, ignore.case = TRUE) & (!grepl(">LUp", dplyr::lag(x$plays2$code), ignore.case = TRUE) | seq_along(x$plays2$code) == 1))
    if (nrow(x$meta$result) < length(set_start_rows)) {
        temp <- length(set_start_rows) - nrow(x$meta$result)
        x$meta$result <- bind_rows(x$meta$result, tibble(played = rep(NA, temp), score_intermediate1 = rep(NA_character_, temp), score_intermediate2 = rep(NA_character_, temp), score_intermediate3 = rep(NA_character_, temp), score = rep(NA_character_, temp), duration = rep(NA_real_, temp), X7 = rep(NA, temp), score_home_team = rep(NA_real_, temp), score_visiting_team = rep(NA_real_, temp)))
    }
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
            ##message("updating scores for set ", si)
            set_plays2 <- x$plays2[seq(set_start_rows[si], set_end_rows[si]), ]
            ## scores
            scores <- c(max(set_plays2$home_score_start_of_point, na.rm = TRUE), max(set_plays2$visiting_score_start_of_point, na.rm = TRUE))
            x$meta$result$score[si] <- paste0(scores[1], "-", scores[2])
            x$meta$result$score_home_team[si] <- scores[1]
            x$meta$result$score_visiting_team[si] <- scores[2]
            ## duration
            set_start_end_time <- range(set_plays2$video_time, na.rm = TRUE)
            x$meta$result$duration[si] <- if (any(is.infinite(set_start_end_time))) NA_real_ else round(diff(set_start_end_time) / 60)
            ## sets won
            ## need scores at end of points
            temp <- do.call(rbind, stringr::str_match_all(set_plays2$code, "^[a\\*]p([[:digit:]]+):([[:digit:]]+)"))
            scores <- c(max(as.numeric(temp[, 2]), na.rm = TRUE), max(as.numeric(temp[, 3]), na.rm = TRUE))
            set_was_completed <- FALSE
            if (isTRUE(any(grepl("^\\*\\*[[:digit:]]set", set_plays2$code, ignore.case = TRUE), na.rm = TRUE))) {
                ## we have an end-of-set marker, so take the team that had the highest score as the winner
                sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                set_was_completed <- TRUE
            } else {
                ## no end-of-set marker, but we can't be sure if the set is incomplete or the marker is just missing. If scores are such that the set would be complete, assume it was
                if (test_end_of_set(scores, set_number = si, beach = is_beach)) {
                    sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                    set_was_completed <- TRUE
                }
            }
            ## intermediate score levels
            ## these won't always be correct if sets are played to non-standard scores (e.g. Aus AVSL plays all sets to 18 points) but it's not immediately obvious how to allow for that
            iss <- if (is_beach) {
                       c(5, 10, 15)
                   } else {
                       ## indoor
                       if (si >= 5) {
                           ## 5th set
                           if (max(scores) < 15 && set_was_completed) {
                               ## possibly non-standard regs, scores to less than 15
                               round(max(scores) * c(0.33, 0.66, 0.83))
                           } else {
                               c(5, 10, 12) ## default, standard regs
                           }
                       } else {
                           if (max(scores) < 25 && set_was_completed) {
                               ## possibly non-standard regs, scores to less than 25
                               round(max(scores) * c(0.33, 0.66, 0.83))
                           } else {
                               c(8, 16, 21) ## default, standard regs
                           }
                       }
                   }
            for (issi in seq_along(iss)) {
                idx <- which(set_plays2$home_score_start_of_point >= iss[issi] | set_plays2$visiting_score_start_of_point >= iss[issi])
                if (length(idx) > 0) {
                    idx <- min(idx)
                    if (!is.na(set_plays2$home_score_start_of_point[idx]) && !is.na(set_plays2$visiting_score_start_of_point[idx])) {
                        x$meta$result[[paste0("score_intermediate", issi)]][si] <- paste0(set_plays2$home_score_start_of_point[idx], "-", set_plays2$visiting_score_start_of_point[idx])
                    } else {
                        x$meta$result[[paste0("score_intermediate", issi)]][si] <- NA_character_
                    }
                } else {
                    x$meta$result[[paste0("score_intermediate", issi)]][si] <- NA_character_
                }
            }
        }
        x$meta$teams$sets_won <- sets_won
        if ((is_beach && max(sets_won) > 1) || max(sets_won) > 2) x$meta$teams$won_match <- (sets_won == max(sets_won))
    }
    ## starting lineups and subs
    ## this can be done even for sets that haven't been completed
    for (si in seq_len(max(x$plays2$set_number, na.rm = TRUE))) {
        ## use the final >LUp row for starting lineup
        final_lup_row <- which(x$plays2$set_number == si & grepl(">LUp", x$plays2$code, ignore.case = TRUE))
        if (length(final_lup_row) > 0) final_lup_row <- max(final_lup_row)
        if (length(final_lup_row) == 1) {
            home_starting_lineup <- as.numeric(x$plays2[final_lup_row, paste0("home_p", pseq)])
            x$meta$players_h[[paste0("starting_position_set", si)]] <- NA_character_
            for (j in seq_along(home_starting_lineup)) {
                pl_row <- which(x$meta$players_h$number == home_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_h[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
            ## subs
            all_home_pl <- unique(na.omit(as.numeric(unlist(x$plays2[which(x$plays2$set_number == si & !grepl("^(>LUp|\\*\\*[[:digit:]]set)", x$plays2$code, ignore.case = TRUE)), paste0("home_p", pseq)]))))
            ## also any players recorded making a play, because liberos won't appear in the home_pX lineup columns
            ## reconstruct home player number from the code column because there isn't a player_number column in the plays2 dataframe, grr
            temp_hpn <- unique(na.omit(as.numeric(stringr::str_match(x$plays2$code[which(x$plays2$set_number == si)], "^\\*([[:digit:]]+)[SREABDF]")[, 2])))
            all_home_pl <- unique(c(all_home_pl, temp_hpn))
            home_subs <- na.omit(setdiff(all_home_pl, home_starting_lineup))
            x$meta$players_h[[paste0("starting_position_set", si)]][x$meta$players_h$number %in% home_subs] <- "*"
            ## visiting team
            visiting_starting_lineup <- as.numeric(x$plays2[final_lup_row, paste0("visiting_p", pseq)])
            x$meta$players_v[[paste0("starting_position_set", si)]] <- NA_character_
            for (j in seq_along(visiting_starting_lineup)) {
                pl_row <- which(x$meta$players_v$number == visiting_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_v[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
            ## subs
            all_visiting_pl <- unique(na.omit(as.numeric(unlist(x$plays2[which(x$plays2$set_number == si & !grepl("^(>LUp|\\*\\*[[:digit:]]set)", x$plays2$code, ignore.case = TRUE)), paste0("visiting_p", pseq)]))))
            temp_vpn <- unique(na.omit(as.numeric(stringr::str_match(x$plays2$code[which(x$plays2$set_number == si)], "^a([[:digit:]]+)[SREABDF]")[, 2])))
            all_home_pl <- unique(c(all_home_pl, temp_vpn))
            visiting_subs <- na.omit(setdiff(all_visiting_pl, visiting_starting_lineup))
            x$meta$players_v[[paste0("starting_position_set", si)]][x$meta$players_v$number %in% visiting_subs] <- "*"
        }
    }
    x
}

remove_players_not_played <- function(roster, plays, home_visiting, faststart_only = TRUE) {
    if (tolower(home_visiting) %eq% "h") home_visiting <- "home" else if (tolower(home_visiting) %eq% "v") home_visiting <- "visiting"
    home_visiting <- match.arg(home_visiting, c("home", "visiting"))
    np <- roster
    if (faststart_only) np <- np %>% dplyr::filter(tolower(.data$firstname) == "player" & grepl(paste0("^", home_visiting, "[[:digit:]]+$"), .data$lastname, ignore.case = TRUE))
    np <- np %>% dplyr::filter(!.data$number %in% unique(na.omit(unlist(plays[, intersect(names(plays), paste0(home_visiting, "_p", 1:6))]))))
    if (nrow(np) > 0) {
        tryCatch(dplyr::anti_join(roster, np, by = names(roster)), error = function(e) roster) ## fallback to unmodified roster
    } else {
        ## no players to remove, return the unmodified roster
        roster
    }
}

## write the scouted match to a file, using the plays2 data instead of the plays
## then it is possible to read that file back in, which will populate the full plays data.frame without having to duplicate the code needed to do this
dv_write2 <- function(x, file, text_encoding = "UTF-8", convert_cones = TRUE) {
    ## convert_cones = TRUE will convert zones to cones prior to saving (necessary if using click-scouting but the user wants cones in their dvw file. Internally all attacks are recorded with end_zone when click scouting, so we can convert them on export if needed)
    ## Note that we don't want to do that with type-scouting, because the user will have actually scouted cones in that case
    ## (if x$meta$match$zones_or_cones is already "C", we don't want to try and convert them. That should never be "C" with click-scouted files)
    ## attacks with end positions
    convert_cones <- isTRUE(convert_cones) && x$meta$match$zones_or_cones %eq% "Z"
    if (convert_cones) x$meta$match$zones_or_cones <- "C" ## change the indicator in the metadata
    x[["plays"]] <- tibble()
    ## ensure players are OK
    x$meta$players_h <- make_players(remove_players_not_played(roster = x$meta$players_h, plays = x$plays2, home_visiting = "h", faststart_only = TRUE))
    x$meta$players_v <- make_players(remove_players_not_played(roster = x$meta$players_v, plays = x$plays2, home_visiting = "v", faststart_only = TRUE))
    ## write without the plays part
    dv_write(x, file = file, text_encoding = text_encoding)
    if (!is.null(x$plays2) && nrow(x$plays2) > 0) {
        ## some columns
        for (cl in c("time", "video_time")) if (!cl %in% names(x$plays2)) x$plays2[[cl]] <- rep(NA_real_, nrow(x$plays2))
        for (cl in c("home_setter_position", "visiting_setter_position")) if (!cl %in% names(x$plays2)) x$plays2[[cl]] <- rep(NA_integer_, nrow(x$plays2))
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

        ## when click-mode scouting, ovs files use coordinates as their primary location source, and these are converted to zones/subzones
        ## but we can convert to cones before saving to dvw if desired
        ## but don't do this if scouting in type-mode
        if (convert_cones) {
            aidx <- which(nchar(x$plays2$code) > 10 & substr(x$plays2$code, 4, 4) %in% "A" & !is.na(x$plays2$start_coordinate) & !is.na(x$plays2$end_coordinate))
            if (length(aidx) > 0) {
                sz <- dv_xy2zone(x$plays2$start_coordinate[aidx]) ## start zone for each attack
                ec <- as.character(dv_xy2cone(x$plays2$end_coordinate[aidx], start_zones = sz)) ## end cone for each attack, from coord and start zone
                ec[!ec %in% 1:8] <- "~"
                if (length(ec) != length(aidx)) ec <- "~" ## just in case we are out of whack
                ## rebuild code
                x$plays2$code[aidx] <- sub("~+$", "", paste0(substr(x$plays2$code[aidx], 1, 10), ec, "~", substr(x$plays2$code[aidx], 13, 99)))
            }
        }
        ## setter position uses 0 or 5 when unknown
        x$plays2$home_setter_position[is.na(x$plays2$home_setter_position)] <- 5
        x$plays2$visiting_setter_position[is.na(x$plays2$visiting_setter_position)] <- 5

        ##x$plays2 <- x$plays2[, setdiff(names(x$plays2), c("home_score_start_of_point", "visiting_score_start_of_point", "serving"))]
        x$plays2$na_col <- NA
        x$plays2$video_time <- round(x$plays2$video_time)
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
    }
    invisible(file)
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
        ## make sure that players in the first six aren't listed as liberos
        lineup <- c(lineup[1:6], na.omit(setdiff(lineup[7:8], lineup[1:6])))
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
           generator_idp = "ovscout2", generator_prg = "ovscout2-R", generator_rel = packageVersion("ovscout2"), generator_ver = "", generator_nam = "",
           lastchange_day = tdformat(tdnow),
           lastchange_idp = "ovscout2", lastchange_prg = "ovscout2-R", lastchange_rel = packageVersion("ovscout2"), lastchange_ver = "", lastchange_nam = "",
           preferred_date_format = tolower(gsub("[%/]", "", date_format)), file_type = file_type)
}

create_meta <- function(match, more, teams, players_h, players_v, video_file, attacks, setter_calls, winning_symbols, zones_or_cones, regulation, comments) {
    meta <- list()
    ## match
    if (missing(match)) match <- list()
    assert_that(is.list(match))
    if (is.data.frame(match)) assert_that(nrow(match) == 1)
    meta$match <- tibble(date = nn_or(match$date, as.Date(NA)),
                         time = nn_or(match$time, NA),
                         season = nn_or(match$season, NA_character_),
                         league = nn_or(match$league, NA_character_),
                         phase = nn_or(match$phase, NA_character_),
                         home_away = nn_or(match$home_away, NA),
                         day_number = nn_or(match$day_number, NA_integer_),
                         match_number = nn_or(match$match_number, NA_integer_),
                         text_encoding = "UTF-8",
                         regulation = nn_or(match$regulation, regulation),
                         zones_or_cones = nn_or(match$zones_or_cones, zones_or_cones),
                         X12 = NA)
    if (inherits(meta$match$time, "POSIXt")) meta$match$time <- format(meta$match$time, "%HH %MM %SS")
    meta$match$time <- tryCatch(lubridate::period(meta$match$time), error = function(e) lubridate::as.period(NA))
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
    if (is.character(teams)) {
        assert_that(length(teams) == 2)
        teams <- tibble(team_id = substr(teams, 1, 4), team = teams)
    }
    assert_that(is.data.frame(teams), nrow(teams) == 2)
    if (any(duplicated(teams$team_id))) stop("team_ids must be unique")
    if (!"coach" %in% names(teams)) teams$coach <- ""
    if (!"assistant" %in% names(teams)) teams$assistant <- ""
    if (!"shirt_colour" %in% names(teams)) teams$shirt_colour <- c("#FF0000", "#0000FF")
    if ("home_away_team" %in% names(teams) && identical(teams$home_away_team, c("a", "*"))) teams <- teams[2:1, ]
    meta$teams <- tibble(team_id = teams$team_id, team = teams$team,
                         sets_won = c(0L, 0L),
                         coach = teams$coach,
                         assistant = teams$assistant,
                         shirt_colour = teams$shirt_colour,
                         X7 = NA, X8 = NA, X9 = NA, X10 = NA,
                         home_away_team = c("*", "a"),
                         won_match = c(NA, NA))
    ## players
    if (missing(players_h)) players_h <- tibble(number = integer(), firstname = character(), lastname = character())
    if (missing(players_v)) players_v <- tibble(number = integer(), firstname = character(), lastname = character())
    meta$players_h <- make_players(players_h, "home")
    meta$players_v <- make_players(players_v, "visiting")
    meta$players_v$X3 <- meta$players_v$X3 + nrow(players_h)
    ## check for duplicate player IDs (on same team)
    pids <- c(meta$players_h$player_id, meta$players_v$player_id)
    duph <- meta$players_h$player_id[duplicated(meta$players_h$player_id)]
    dupv <- meta$players_v$player_id[duplicated(meta$players_v$player_id)]
    if (length(duph) > 0 || length(dupv) > 0) {
        stop("duplicated player_id(s): ",
             if (length(duph) > 0) paste0("home team ", paste(duph, collapse = ", "), if (length(dupv) > 0) ", "),
             if (length(dupv) > 0) paste0("visiting team ", paste(dupv, collapse = ", ")),
             ". If these have been automatically generated you might need to explicitly provide them in the players_h and players_v data.frames")
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
    if (nrow(meta$players_h) < min_n_players) warning("less than ", min_n_players, " players in home player list")
    if (nrow(meta$players_v) < min_n_players) warning("less than ", min_n_players, " players in visiting player list")

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
    req <- c("lastname", "firstname", "number") ## absolutely required
    if (!all(req %in% names(players))) stop(which, " players data.frame is missing columns: ", paste(setdiff(req, names(players)), collapse = ", "))
    if (!"player_id" %in% names(players)) players$player_id <- make_player_id(players$lastname, players$firstname)
    if (!"role" %in% names(players)) players$role <- "unknown"
    if (!"nickname" %in% names(players)) players$nickname <- ""
    if (!"special_role" %in% names(players)) players$special_role <- ""
    ## reset liberos and reassign (but keep e.g. "C" captain)
    players$special_role <- gsub("L", "", players$special_role)
    players$special_role[tolower(players$role) %eq% "libero"] <- paste0(players$special_role[tolower(players$role) %eq% "libero"], "L")
    if (!"foreign" %in% names(players)) players$foreign <- FALSE
    for (nm in paste0("starting_position_set", 1:5)) if (!nm %in% names(players)) players[[nm]] <- NA_character_
    temp <- tibble(X1 = 0,
                   number = players$number,
                   X3 = seq_len(nrow(players)),
                   starting_position_set1 = players$starting_position_set1,
                   starting_position_set2 = players$starting_position_set2,
                   starting_position_set3 = players$starting_position_set3,
                   starting_position_set4 = players$starting_position_set4,
                   starting_position_set5 = players$starting_position_set5,
                   player_id = players$player_id,
                   lastname = players$lastname,
                   firstname = players$firstname,
                   nickname = players$nickname,
                   special_role = toupper(players$special_role),
                   role = tolower(players$role),
                   foreign = players$foreign)
    temp$X16 <- temp$X17 <- temp$X18 <- temp$X19 <- temp$X20 <- temp$X21 <- temp$X22 <- temp$X23 <- NA
    temp$name <- paste(temp$firstname, temp$lastname)
    temp
}
make_player_id <- function(lastname, firstname) toupper(paste0(substr(lastname, 1, 3), "-", substr(firstname, 1, 3)))

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
    ## for each skill/eval pair in code, is it a W or L or neither?
    my_wl <- vapply(seq_along(skill), function(i) {
        out <- x$meta$winning_symbols[x$meta$winning_symbols$skill %eq% skill[i] & x$meta$winning_symbols$code %eq% evaluation_code[i], ]
        if (nrow(out) == 1) out$win_lose else NA_character_
    }, FUN.VALUE = "A", USE.NAMES = FALSE)
    wsidx <- my_wl %eq% "W" ## skill %in% x$meta$winning_symbols$skill[wswin] & evaluation_code %in% x$meta$winning_symbols$code[wswin]
    lsidx <- my_wl %eq% "L" ## skill %in% x$meta$winning_symbols$skill[!wswin] & evaluation_code %in% x$meta$winning_symbols$code[!wswin]
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

## the end zone of a block is typically recorded as the start zone of the attack
block_zone <- function(attack_zone) {
    out <- rep(NA_integer_, length(attack_zone))
    out[attack_zone %in% c(2, 9, 1)] <- 4L
    out[attack_zone %in% c(3, 8, 6)] <- 3L
    out[attack_zone %in% c(4, 7, 5)] <- 2L
    out
}

plays_to_plays2 <- function(p) {
    p2 <- mutate(p, serving = case_when(.data$serving_team == .data$home_team ~ "*", .data$serving_team == .data$visiting_team ~ "a"))##, X4 = NA, X14 = NA)
    p2 <- as_tibble(dplyr::select(p2, "code", "point_phase", "attack_phase", ##"X4",
                                  "start_coordinate", "mid_coordinate", "end_coordinate", "time", "set_number", "home_setter_position", "visiting_setter_position", "video_file_number", "video_time", ##"X14",
                                  home_score_start_of_point = "home_team_score", visiting_score_start_of_point = "visiting_team_score", "serving", "home_p1", "home_p2", "home_p3", "home_p4", "home_p5", "home_p6", "visiting_p1", "visiting_p2", "visiting_p3", "visiting_p4", "visiting_p5", "visiting_p6", "phase")) %>%
        mutate(ht_lib1 = NA_integer_, ht_lib2 = NA_integer_, vt_lib1 = NA_integer_, vt_lib2 = NA_integer_)
    p2$rally_codes <- vector("list", nrow(p2))
    p2
}

## empty (zero-row) plays2 data frame, for initialization
empty_plays2 <- function() {
    chr <- character(); num <- numeric(); int <- integer()
    tibble(code = chr, point_phase = chr, attack_phase = chr,
           start_coordinate = num, mid_coordinate = num, end_coordinate = num,
           time = num, set_number = int, home_setter_position = int, visiting_setter_position = int, video_file_number = int, video_time = num,
           home_score_start_of_point = int, visiting_score_start_of_point = int, serving = chr,
           home_p1 = int, home_p2 = int, home_p3 = int, home_p4 = int, home_p5 = int, home_p6 = int,
           visiting_p1 = int, visiting_p2 = int, visiting_p3 = int, visiting_p4 = int, visiting_p5 = int, visiting_p6 = int,
           ht_lib1 = int, ht_lib2 = int, vt_lib1 = int, vt_lib2 = int,
           phase = chr, rally_codes = list())
}

## parse just the skill rows, giving the team, player number, skill and type, and evaluation code
minimal_parse_code <- function(codes) {
    code_split <- stringr::str_match_all(codes, "^([a\\*])([[:digit:]][[:digit:]])([SRABDEF])([HMQTUNO])([#\\+!/=\\-])")
    na_row <- as.list(rep(NA, 5))
    parsed <- setNames(as.data.frame(cbind(codes, do.call(rbind, lapply(code_split, function(this) if (nrow(this) < 1) na_row else as.list(this[1, -1]))))), c("code", "team_code", "player_number", "skill_code", "skill_type_code", "evaluation_code"))
    parsed
}

disambig_names <- function(last, first) {
    firstinit <- substr(first, 1, 1)
    didx <- which(last %in% last[duplicated(last)])
    last[didx] <- paste(firstinit[didx], last[didx])
    last
}

names2roster <- function(pm, join = TRUE) {
    pm <- dplyr::arrange(pm, .data$number)
    pm$lastname <- disambig_names(pm$lastname, pm$firstname)
    lc <- paste(ifelse(grepl("L", pm$special_role), "L", ""), ifelse(grepl("C", pm$special_role), "C", ""), sep = ",")
    lc <- sub("^,", "", sub(",$", "", lc))
    lc[nzchar(lc)] <- paste0(" (", lc[nzchar(lc)], ")")
    pm$lastname <- paste0(pm$lastname, lc)
    if (join) str_trim(paste(pm$number, pm$lastname)) else pm[, c("number", "lastname")]
}


calc_sets_won <- function(plays2) {
    sets_won <- c(0L, 0L) ## sets won by home, visiting teams
    if (nrow(plays2) < 1 || !"code" %in% names(plays2)) return(sets_won)
    set_nums <- suppressWarnings(as.numeric(na.omit(str_match(plays2$code, "^\\*\\*([[:digit:]])set")[, 2])))
    for (si in set_nums) {
        temp <- do.call(rbind, stringr::str_match_all(plays2 %>% dplyr::filter(.data$set_number == si) %>% pull(.data$code), "^[a\\*]p([[:digit:]]+):([[:digit:]]+)"))
        scores <- c(max(as.numeric(temp[, 2]), na.rm = TRUE), max(as.numeric(temp[, 3]), na.rm = TRUE))
        ## the set has been marked as complete, so don't check score thresholds, just take the team with the higher score as the winner of the set
        sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
    }
    sets_won
}

test_end_of_set <- function(scores, set_number, beach = FALSE) {
    if (isTRUE(beach)) {
        isTRUE(((set_number < 3 && max(scores) >= 21) || (set_number > 2 && max(scores) >= 15)) && abs(diff(scores)) >= 2)
    } else {
        isTRUE(((set_number < 5 && max(scores) >= 25) || (set_number > 4 && max(scores) >= 15)) && abs(diff(scores)) >= 2)
    }
}
