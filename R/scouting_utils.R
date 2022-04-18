## generate the auto-codes necessary to make a complete rally
## @param code character: vector of scouted codes, only those relating to skill executions along with the "*p" or "ap" code at the end
## @param home_rot, visiting_rot integer: home and visiting setter positions (1 to 6)
## @param home_setter, visiting_setter integer: jersey number of home and visiting setter
## @param rotated string: "a" or "*" if that team just sided out and rotated (requires the corresponding team's rot and setter values)
## @param winning_symbols data.frame: as returned by datavolley::winning_symbols_df
make_auto_codes <- function(code, home_rot, visiting_rot, home_setter, visiting_setter, rotated, winning_symbols) {
    if (!grepl("^[a\\*]p[[:digit:]]+:[[:digit:]]+", tail(code, 1))) stop("the final element of the code vector should be the '*p' or 'ap' code")
    won_by <- substr(tail(code, 1), 1, 1)
    if (!won_by %in% c("*", "a")) stop("won_by must be '*' or 'a'")
    team_char <- substr(code, 1, 1)
    team_char[!team_char %in% c("*", "a")] <- NA_character_ ## "*" or "a"
    skill <- substr(code, 4, 4)
    skill[!skill %in% c("S", "R", "A", "B", "D", "E", "F")] <- NA_character_
    evaluation_code <- substr(code, 6, 6)
    evaluation_code[!evaluation_code %in% c("#", "+", "!", "-", "/", "=")] <- NA_character_
    position_codes <- c()
    if (!missing(rotated)) {
        if (!rotated %in% c("*", "a")) stop("rotated should be '*' or 'a'")
        position_codes <- paste0(rotated, "z", if (rotated == "*") home_rot else visiting_rot)
    }
    wswin <- winning_symbols$win_lose == "W"
    wsidx <- skill %in% winning_symbols$skill[wswin] & evaluation_code %in% winning_symbols$code[wswin]
    lsidx <- skill %in% winning_symbols$skill[!wswin] & evaluation_code %in% winning_symbols$code[!wswin]
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
    c(position_codes, code[-length(code)], green_codes, tail(code, 1))
}

create_file_meta <- function(file_type = "indoor", date_format = "%Y/%m/%d") {
    tdnow <- Sys.time()
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
                         time = nn_or(match$time, Sys.time()),
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

#' Create a datavolley match object
#'
#' @param match list or single-row data.frame: (optional) with components `date` (defaults to current date), `time` (defaults to current time), `season`, `league`, `phase`, `home_away`, `day_number`, `match_number`, `regulation`, `zones_or_cones`. `zones_or_cones` can also be provided directly
#' @param more list or single-row data.frame: (optional) with components `referees`, `spectators`, `receipts`, `city`, `arena`, `scout`
#' @param teams data.frame: a 2-row data frame, with required columns `team_id`, `team` and optional columns `coach`, `assistant`, `shirt_colour`
#' @param players_h,players_v data.frame: with required columns `number`, `firstname`, `lastname`, and optional columns `player_id`, `role` (character vector with "outside", "opposite", "middle", "libero", "setter"), `nickname`, `special_role` (character vector with "L", "C", or NA), `foreign` (logical, defaults to `FALSE`)
#' @param video_file string: (optional) path to video file
#' @param attacks data.frame: as returned by [ov_default_attack_table()]
#' @param setter_calls data.frame: as returned by [ov_default_setter_calls_table()]
#' @param winning_symbols data.frame: as returned by [ov_default_winning_symbols()]
#' @param zones_or_cones string: "Z" or "C". Will be ignored if `zones_or_cones` is provided in the `match` parameter
#' @param regulation string: "indoor rally point", "beach rally point", or "indoor sideout". Will be ignored if `regulation` is provided in the `match` parameter
#' @param comments character: optional vector of length up to 5, of comments
#'
#' @return A datavolley object
#'
#' @examples
#' x <- dv_create(teams = data.frame(team_id = c("TM1", "TM2"), team = c("Team 1", "Team 2")), comments = "Test file",
#'                players_h = data.frame(firstname = toupper(letters[1:6]), lastname = "Player", number = 1:6),
#'                players_v = data.frame(firstname = letters[10:15], lastname = "VisPlayer", number = 10:15))
#' x <- dv_set_lineups(x, set_number = 1, lineups = list(6:1, 15:10), setter_positions = c(2, 1))
#'
#' @export
dv_create <- function(match, more, teams, players_h, players_v, video_file, attacks = ov_default_attack_table(), setter_calls = ov_default_setter_calls_table(), winning_symbols = ov_default_winning_symbols(), zones_or_cones = "Z", regulation = "indoor rally point", comments) {
    reg <- if (!missing(match) && "regulation" %in% names(match)) match$regulation else regulation
    out <- list(##raw,
        messages = tibble(file_line_number = integer(), video_time = numeric(), message = character(), file_line = character()),
        file_meta = create_file_meta(file_type = if (grepl("beach", reg)) "beach" else "indoor"),
        meta = create_meta(match = match, more = more, teams = teams, players_h = players_h, players_v = players_v, video_file = video_file, attacks = attacks, setter_calls = setter_calls, winning_symbols = winning_symbols, zones_or_cones = zones_or_cones, regulation = regulation, comments = comments),
        plays = tibble(), plays2 = tibble())
    ## plays is the full plays dataframe, plays2 is just the columns that appear in the dvw file directly
    structure(out, class = c("datavolley", "list"))
}

#' Enter the team lineups at the start of the set
#'
#' @param x datavolley: a datavolley object
#' @param set_number integer: set number, 1--3 for beach or 1--5 for indoor
#' @param lineups list: two-element list with numeric vectors of player numbers. Each lineup is
#' * for indoor, of length 6--8 (first 6 are player jersey numbers in positions 1--6, elements 7 and 8 are optionally the libero jersey numbers)
#' * for beach, if length 2
#' @param setter_positions integer: two-element integer vector giving the position on court of the two setters. At least one of `setter_positions` or `setters` must be provided. Ignored for beach
#' @param setters integer: two-element integer vector giving the jersey numbers of the two setters. At least one of `setter_positions` or `setters` must be provided. Ignored for beach
#'
#' @return A modified version of `x`
#'
#' @export
dv_set_lineups <- function(x, set_number, lineups, setter_positions, setters) {
    assert_that(inherits(x, "datavolley"))
    assert_that(is.list(lineups), length(lineups) == 2)
    is_beach <- grepl("beach", x$meta$match$regulation)
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
    x <- set_lineup(x, set_number = set_number, team = "*", lineup = lineups[[1]], is_beach = is_beach)
    x <- set_lineup(x, set_number = set_number, team = "a", lineup = lineups[[2]], is_beach = is_beach)
    ## insert the start-of-set codes and lineups in the plays data
    ## e.g. "*P04>LUp"          "*z3>LUp"
    lineup_codes <- c(sprintf("*P%02d>Lup", setters[1]), paste0("*z", setter_positions[1], ">Lup"), sprintf("aP%02d>Lup", setters[2]), paste0("az", setter_positions[2], ">Lup"))
    newplays <- make_narrow_plays(lineup_codes, set_number = set_number, home_setter_position = setter_positions[1], visiting_setter_position = setter_positions[2], home_lineup = lineups[[1]], visiting_lineup = lineups[[2]])
    x$plays2 <- bind_rows(x$plays2, newplays)
    x
}

make_narrow_plays <- function(codes, set_number, home_setter_position, visiting_setter_position, home_lineup, visiting_lineup) {
    out <- tibble(code = as.character(codes), ## col 1
                       point_phase = NA_character_, ## col 2
                       attack_phase = NA_character_, ## col 3
                       X4 = NA, ## col 4
                       start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_, ## cols 5-7
                       time = NA_character_, ## col 8
                       set = set_number, home_rot = home_setter_position, visiting_rot = visiting_setter_position, ## cols 9-11, NB these 3 not used directly in dv_read
                       video_file_number = NA, video_time = NA_integer_, ## cols 12-13
                       X14 = NA ## col 14
                       )
    assert_that(length(home_lineup) == 6)
    assert_that(length(visiting_lineup) == 6)
    out <- bind_cols(out, setNames(as.data.frame(as.list(home_lineup)), paste0("home_p", 1:6))) ## cols 15-20
    bind_cols(out, setNames(as.data.frame(as.list(visiting_lineup)), paste0("visiting_p", 1:6))) ## cols 21-26
}

set_lineup <- function(x, set_number, team, lineup, is_beach = FALSE) {
    ##assert_that(is.string(team))
    ##if (tolower(team) == "home" || team %eq% datavolley::home_team(x) || team %eq% datavolley::home_team_id(x)) team <- "*"
    ##if (tolower(team) == "visiting" || team %eq% datavolley::visiting_team(x) || team %eq% datavolley::visiting_team_id(x)) team <- "a"
    if (!team %in% c("*", "a")) stop("team ", team, " is not recognized")
    assert_that(is.numeric(lineup))
    if (is_beach) {
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


## update the metadata:
## - played, partial set scores, score, duration, score_home_team, score_visiting_team in meta$result
## - sets_won, won_match in meta$teams
## - starting positions including substitutions in meta$players_h and meta$players_v
dv_update_meta <- function(x) {
}

