preprocess_dvw <- function(x) {
    x$plays <- mutate(x$plays, clock_time = format(.data$time, "%H:%M:%S"))
    msgs <- dplyr::select(dplyr::filter(x$messages, !is.na(.data$file_line_number)), .data$file_line_number, .data$message)
    ## check for negative steps in video_time
    chk <- mutate(na.omit(dplyr::select(x$plays, .data$skill, .data$file_line_number, .data$video_time)), vdiff = .data$video_time - lag(.data$video_time))
    if (any(chk$vdiff < 0, na.rm = TRUE)) {
        msgs <- bind_rows(msgs, dplyr::select(mutate(dplyr::filter(chk, .data$vdiff < 0), message = "Video time went backwards"), .data$file_line_number, .data$message))
    }
    msgs <- dplyr::summarize(group_by_at(msgs, "file_line_number"), error_message = paste0(.data$message, collapse = "<br />"))
    if ("error_message" %in% names(x$plays)) x$plays <- dplyr::select(x$plays, -"error_message")
    x$plays <- left_join(x$plays, msgs, by = "file_line_number")
    x$plays$error_icon <- ifelse(is.na(x$plays$error_message), "", HTML(as.character(icon("exclamation-triangle"))))
    x
}

## TODO replace these with vscoututils version

## find attack rows for which we could insert sets
## dv_insert_sets_check <- function(dvw, no_set_attacks) {
##     ridx_set <- mutate(dvw$plays, rowN = row_number(),
##                        add_set_before = case_when(.data$skill %eq% "Attack" & !(str_sub(.data$code, 7, 8) %in% no_set_attacks) & !(lag(.data$skill) %eq% "Set") ~ TRUE,
##                                                   TRUE ~ FALSE))
##     ridx_set$rowN[which(ridx_set$add_set_before)]
## }

## insert the set rows
## if ridx not supplied, recalculate it

# Test:
# dvw <- datavolley::read_dv("/home/ick003/Documents/Donnees/VolleyBall/GameDatasets/Southern League 2020 Mens - Spring (Datavolley)/&fir01 vikings me-phoenix men_with_vt.dvw")
# ridx <- 11
# no_set_attacks <- c("PR", "PP", "P2")
# tt <- dv_insert_sets(dvw=dvw, ridx = ridx, no_set_attacks=no_set_attacks)
# dv_create_substitution(dvw, team, ridx, in_player, out_player, new_setter)
##dv_insert_sets <- function(dvw, no_set_attacks, default_set_evaluation = "+", ridx = NULL) {
##    if (is.null(ridx)) ridx <- dv_insert_sets_check(dvw = dvw, no_set_attacks = no_set_attacks)
##    if (length(ridx) > 0) {
##        set_code <- mutate(dvw$plays, passQ = case_when(lag(.data$skill) %in% c("Reception") ~ lag(.data$evaluation)),
##                           digQ = case_when(lag(.data$skill) %in% c("Dig") ~ lag(.data$evaluation)))
##        set_code <- mutate(dplyr::filter(set_code, row_number() %in% ridx),
##                           team_oncourt_setter_number = case_when(.data$team %eq% .data$home_team ~ case_when(.data$home_setter_position == 1 ~ .data$home_p1,
##                                                                                                              .data$home_setter_position == 2 ~ .data$home_p2,
##                                                                                                              .data$home_setter_position == 3 ~ .data$home_p3,
##                                                                                                              .data$home_setter_position == 4 ~ .data$home_p4,
##                                                                                                              .data$home_setter_position == 5 ~ .data$home_p5,
##                                                                                                              .data$home_setter_position == 6 ~ .data$home_p6),
##                                                                  .data$team %eq% .data$visiting_team ~ dplyr::case_when(.data$visiting_setter_position == 1 ~ .data$visiting_p1,
##                                                                                                                         .data$visiting_setter_position == 2 ~ .data$visiting_p2,
##                                                                                                                         .data$visiting_setter_position == 3 ~ .data$visiting_p3,
##                                                                                                                         .data$visiting_setter_position == 4 ~ .data$visiting_p4,
##                                                                                                                         .data$visiting_setter_position == 5 ~ .data$visiting_p5,
##                                                                                                                         .data$visiting_setter_position == 6 ~ .data$visiting_p6)),
##                           team_oncourt_setter_number = sprintf("%02d", .data$team_oncourt_setter_number),
##                           s_code = str_sub(.data$code, 1, 1),
##                           s_skill = "E",
##                           s_hittingtempo = str_sub(.data$code, 5, 5),
##                           s_eval = case_when(.data$num_players_numeric %in% c(0, 1) ~ "#",
##                                     is.na(.data$num_players_numeric) ~ "+",
##                                     TRUE ~ default_set_evaluation),
##                           set_code = paste0(.data$s_code, ## Team
##                                             .data$team_oncourt_setter_number, ## setter player_number
##                                             .data$s_skill, # set skill
##                                             .data$s_hittingtempo, # hitting tempo
##                                             .data$s_eval), #(FIVB recommendations)
##                           TEMP_attack_code = str_sub(.data$code, 7, 8),
##                           setter_call = case_when(.data$TEMP_attack_code %eq% "X1" ~ "K1",
##                                                   .data$TEMP_attack_code %eq% "X2" ~ "K2",
##                                                   .data$TEMP_attack_code %eq% "X7" ~ "K7",
##                                                   grepl("^(Negative)", .data$passQ) ~ "~~",
##                                                   grepl("^(Perfect|Positive|OK)", .data$passQ) ~ "KK",
##                                                   is.na(.data$passQ) ~ "~~",
##                                                   TRUE ~ "~~"),
##                           set_code = paste0(.data$set_code, .data$setter_call))
##        ## TODO, these could be taken from the dvw$meta$attacks table, and then they would keep up with user configuration
##        set_code <- mutate(set_code, set_code = dplyr::case_when(.data$TEMP_attack_code %eq% "X1" ~ paste0(.data$set_code, "C3"),
##                                                                 .data$TEMP_attack_code %eq% "X2" ~ paste0(.data$set_code, "C3"),
##                                                                 .data$TEMP_attack_code %eq% "X7" ~ paste0(.data$set_code, "C3"),
##                                                                 .data$TEMP_attack_code %eq% "X5" ~ paste0(.data$set_code, "F4"),
##                                                                 .data$TEMP_attack_code %eq% "V5" ~ paste0(.data$set_code, "F4"),
##                                                                 .data$TEMP_attack_code %eq% "X6" ~ paste0(.data$set_code, "B2"),
##                                                                 .data$TEMP_attack_code %eq% "V6" ~ paste0(.data$set_code, "B2"),
##                                                                 .data$TEMP_attack_code %eq% "X8" ~ paste0(.data$set_code, "B9"),
##                                                                 .data$TEMP_attack_code %eq% "V8" ~ paste0(.data$set_code, "B9"),
##                                                                 .data$TEMP_attack_code %eq% "XP" ~ paste0(.data$set_code, "P8"),
##                                                                 .data$TEMP_attack_code %eq% "VP" ~ paste0(.data$set_code, "P8"),
##                                                                 TRUE ~ .data$set_code))$set_code
##        dvw$plays <- mutate(dvw$plays, tmp_row_number = row_number())
##        newline <- dvw$plays[ridx, ]
##        newline$code <- set_code
##        newline$video_time <- newline$video_time - 2
##        newline$tmp_row_number <- newline$tmp_row_number - 0.5
##        dvw$plays <- dplyr::arrange(bind_rows(dvw$plays, newline), .data$tmp_row_number)##.data$set_number, .data$point_id, .data$team_touch_id, .data$tmp_row_number)
##        dvw$plays <- dplyr::select(dvw$plays, -"tmp_row_number")
##    }
##    dvw
##}

##dv_insert_digs_check <- function(dvw) {
##    ridx_dig <- mutate(dvw$plays, rowN = row_number(),
##                       attack_in_play = .data$skill %eq% "Attack" & grepl("^(Winning|Positive|Poor|Blocked for reattack|Spike in play)", .data$evaluation, ignore.case = TRUE),
##                       add_dig_after = case_when(.data$attack_in_play & !lead(.data$skill) %in% c("Dig", "Block") ~ TRUE, ## attack in play, not followed by dig or block
##                                                 lag(.data$attack_in_play) & .data$skill %eq% "Block" & !.data$evaluation %in% c("Error", "Invasion", "Winning block") & !lead(.data$skill) %eq% "Dig" ~ TRUE, ## block touch after attack in play, not followed by dig
##                                                 TRUE ~ FALSE))
##    ridx_dig$rowN[which(ridx_dig$add_dig_after)]
##}

##dv_insert_digs <- function(dvw, ridx = NULL) {
##    if (is.null(ridx)) ridx <- dv_insert_digs_check(dvw)
##    if (length(ridx) > 0) {
##        dig_code <- mutate(dplyr::filter(dvw$plays, row_number() %in% ridx),
##                           dig_team = case_when(.data$evaluation %eq% "Blocked for reattack" & .data$team == .data$home_team ~ "*", # Coding for cover
##                                                .data$evaluation %eq% "Blocked for reattack" & .data$team == .data$visiting_team ~ "a", # Coding for cover
##                                                grepl("^(Winning|Positive|Poor|Spike in play)", .data$evaluation) & .data$team == .data$home_team ~ "a", # Coding for dig
##                                                grepl("^(Winning|Positive|Poor|Spike in play)", .data$evaluation) & .data$team == .data$visiting_team ~ "*"), # Coding for dig
##                           dig_eval_code = case_when(
##                               .data$skill %eq% "Attack" & grepl("^Winning", .data$evaluation) ~ "=", ## error dig on a winning attack
##                               .data$skill %eq% "Attack" & grepl("^Positive", .data$evaluation) ~ "-", ## negative dig on positive attack
##                                                     TRUE ~ "#"), ## Excellent dig otherwise (FIVB recommendations)
##                           dig_code = paste0(.data$dig_team, ## Team
##                                             "99", ## dig player_number
##                                             "D", ## dig skill
##                                             str_sub(.data$code, 5, 5), # hitting tempo
##                                             .data$dig_eval_code))
##        dig_code <- dig_code$dig_code
##        dvw$plays <- mutate(dvw$plays, tmp_row_number = row_number())
##        newline <- dvw$plays[ridx, ]
##        newline$code <- dig_code
##        newline$video_time <- newline$video_time + 1
##        newline$tmp_row_number <- newline$tmp_row_number + 0.5
##        dvw$plays <- dplyr::arrange(bind_rows(dvw$plays, newline), .data$tmp_row_number) ##.data$set_number, .data$point_id, .data$team_touch_id, .data$tmp_row_number)
##        dvw$plays <- dplyr::select(dvw$plays, -"tmp_row_number")
##    }
##    dvw
##}
