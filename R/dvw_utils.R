reparse_dvw <- function(x, dv_read_args = list()) {
    tf <- tempfile()
    on.exit(unlink(tf))
    ## dvw contents are UTF-8-converted during dv_read
    ## make sure it's written as that
    dv_write(x, tf, text_encoding = "UTF-8")
    dv_read_args$filename <- tf
    dv_read_args$encoding <- "UTF-8" ## and now we don't need to guess encoding when re-reading
    out <- do.call(read_dv, dv_read_args)
    out$meta$filename <- x$meta$filename ## preserve this
    preprocess_dvw(out)
}

preprocess_dvw <- function(x) {
    x$plays <- mutate(x$plays, clock_time = format(.data$time, "%H:%M:%S"))
    msgs <- dplyr::select(dplyr::filter(x$messages, !is.na(.data$file_line_number)), .data$file_line_number, .data$message)
    ## check for negative steps in video_time
    chk <- mutate(na.omit(dplyr::select(x$plays, .data$skill, .data$file_line_number, .data$video_time)), vdiff = .data$video_time - lag(.data$video_time))
    if (any(chk$vdiff < 0, na.rm = TRUE)) {
        msgs <- bind_rows(msgs, dplyr::select(mutate(dplyr::filter(chk, .data$vdiff < 0), message = "Video time went backwards"), .data$file_line_number, .data$message))
    }
    msgs <- dplyr::summarize(group_by_at(msgs, "file_line_number"), error_message = paste0(.data$message, collapse = "<br />"))
    x$plays <- left_join(x$plays, msgs, by = "file_line_number")
    x$plays$error_icon <- ifelse(is.na(x$plays$error_message), "", HTML(as.character(shiny::icon("exclamation-triangle"))))
    x
}

## find attack rows for which we could insert sets
dv_insert_sets_check <- function(dvw, no_set_attacks) {
    ridx_set <- mutate(dvw$plays, rowN = row_number(),
                       add_set_before = case_when(.data$skill %eq% "Attack" & !(str_sub(.data$code, 7, 8) %in% no_set_attacks) & !(lag(.data$skill) %eq% "Set") ~ TRUE,
                                                  TRUE ~ FALSE))
    ridx_set$rowN[which(ridx_set$add_set_before)]
}

## insert the set rows
## if ridx not supplied, recalculate it

# Test:
# dvw <- datavolley::read_dv("/home/ick003/Documents/Donnees/VolleyBall/GameDatasets/Southern League 2020 Mens - Spring (Datavolley)/&fir01 vikings me-phoenix men_with_vt.dvw")
# ridx <- 11
# no_set_attacks <- c("PR", "PP", "P2")
# tt <- dv_insert_sets(dvw=dvw, ridx = ridx, no_set_attacks=no_set_attacks)
# dv_create_substitution(dvw, team, ridx, in_player, out_player, new_setter)
dv_insert_sets <- function(dvw, no_set_attacks, default_set_evaluation = "+", ridx = NULL) {
    if (is.null(ridx)) ridx <- dv_insert_sets_check(dvw = dvw, no_set_attacks = no_set_attacks)
    if (length(ridx) > 0) {
        set_code <- mutate(dvw$plays, passQ = case_when(lag(.data$skill) %in% c("Reception", "Dig") ~ lag(.data$evaluation)))
        set_code <- mutate(dplyr::filter(set_code, row_number() %in% ridx),
                           team_oncourt_setter_number = case_when(.data$team %eq% .data$home_team ~ case_when(.data$home_setter_position == 1 ~ .data$home_p1,
                                                                                                              .data$home_setter_position == 2 ~ .data$home_p2,
                                                                                                              .data$home_setter_position == 3 ~ .data$home_p3,
                                                                                                              .data$home_setter_position == 4 ~ .data$home_p4,
                                                                                                              .data$home_setter_position == 5 ~ .data$home_p5,
                                                                                                              .data$home_setter_position == 6 ~ .data$home_p6),
                                                                  .data$team %eq% .data$visiting_team ~ dplyr::case_when(.data$visiting_setter_position == 1 ~ .data$visiting_p1,
                                                                                                                         .data$visiting_setter_position == 2 ~ .data$visiting_p2,
                                                                                                                         .data$visiting_setter_position == 3 ~ .data$visiting_p3,
                                                                                                                         .data$visiting_setter_position == 4 ~ .data$visiting_p4,
                                                                                                                         .data$visiting_setter_position == 5 ~ .data$visiting_p5,
                                                                                                                         .data$visiting_setter_position == 6 ~ .data$visiting_p6)),
                           team_oncourt_setter_number = sprintf("%02d", .data$team_oncourt_setter_number),
                           s_code = str_sub(.data$code, 1, 1),
                           s_skill = "E",
                           s_hittingtempo = str_sub(.data$code, 5, 5),
                           s_eval = case_when(.data$num_players_numeric %in% c(0, 1) ~ "#",
                                     is.na(.data$num_players_numeric) ~ "+",
                                     TRUE ~ default_set_evaluation),
                           set_code = paste0(.data$s_code, ## Team
                                             .data$team_oncourt_setter_number, ## setter player_number
                                             .data$s_skill, # set skill
                                             .data$s_hittingtempo, # hitting tempo
                                             .data$s_eval), #(FIVB recommendations)
                           TEMP_attack_code = str_sub(.data$code, 7, 8),
                           setter_call = case_when(.data$TEMP_attack_code %eq% "X1" ~ "K1",
                                                   .data$TEMP_attack_code %eq% "X2" ~ "K2",
                                                   .data$TEMP_attack_code %eq% "X7" ~ "K7",
                                                   grepl("^(OK|Negative)", .data$passQ) ~ "KE",
                                                   grepl("^(Perfect|Positive)", .data$passQ) ~ "KK",
                                                   is.na(.data$passQ) ~ "KK",
                                                   TRUE ~ "~~"),
                           set_code = paste0(.data$set_code, .data$setter_call))
        ## TODO, these could be taken from the dvw$meta$attacks table, and then they would keep up with user configuration
        set_code <- mutate(set_code, set_code = dplyr::case_when(.data$TEMP_attack_code %eq% "X1" ~ paste0(.data$set_code, "C3"),
                                                                 .data$TEMP_attack_code %eq% "X2" ~ paste0(.data$set_code, "C3"),
                                                                 .data$TEMP_attack_code %eq% "X7" ~ paste0(.data$set_code, "C3"),
                                                                 .data$TEMP_attack_code %eq% "X5" ~ paste0(.data$set_code, "F4"),
                                                                 .data$TEMP_attack_code %eq% "V5" ~ paste0(.data$set_code, "F4"),
                                                                 .data$TEMP_attack_code %eq% "X6" ~ paste0(.data$set_code, "B2"),
                                                                 .data$TEMP_attack_code %eq% "V6" ~ paste0(.data$set_code, "B2"),
                                                                 .data$TEMP_attack_code %eq% "X8" ~ paste0(.data$set_code, "B9"),
                                                                 .data$TEMP_attack_code %eq% "V8" ~ paste0(.data$set_code, "B9"),
                                                                 .data$TEMP_attack_code %eq% "XP" ~ paste0(.data$set_code, "P8"),
                                                                 .data$TEMP_attack_code %eq% "VP" ~ paste0(.data$set_code, "P8"),
                                                                 TRUE ~ .data$set_code))$set_code
        dvw$plays <- mutate(dvw$plays, tmp_row_number = row_number())
        newline <- dvw$plays[ridx, ]
        newline$code <- set_code
        newline$video_time <- newline$video_time - 2
        newline$tmp_row_number <- newline$tmp_row_number - 0.5
        dvw$plays <- dplyr::arrange(bind_rows(dvw$plays, newline), .data$tmp_row_number)##.data$set_number, .data$point_id, .data$team_touch_id, .data$tmp_row_number)
        dvw$plays <- dplyr::select(dvw$plays, -"tmp_row_number")
    }
    dvw
}

dv_insert_digs_check <- function(dvw) {
    ridx_dig <- mutate(dvw$plays, rowN = row_number(),
                       attack_in_play = .data$skill %eq% "Attack" & grepl("^(Winning|Positive|Poor|Blocked for reattack|Spike in play)", .data$evaluation, ignore.case = TRUE),
                       add_dig_after = case_when(.data$attack_in_play & !lead(.data$skill) %in% c("Dig", "Block") ~ TRUE, ## attack in play, not followed by dig or block
                                                 lag(.data$attack_in_play) & .data$skill %eq% "Block" & !.data$evaluation %in% c("Error", "Invasion", "Winning block") & !lead(.data$skill) %eq% "Dig" ~ TRUE, ## block touch after attack in play, not followed by dig
                                                 TRUE ~ FALSE))
    ridx_dig$rowN[which(ridx_dig$add_dig_after)]
}

dv_insert_digs <- function(dvw, ridx = NULL) {
    if (is.null(ridx)) ridx <- dv_insert_digs_check(dvw)
    if (length(ridx) > 0) {
        dig_code <- mutate(dplyr::filter(dvw$plays, row_number() %in% ridx),
                           dig_team = case_when(.data$evaluation %eq% "Blocked for reattack" & .data$team == .data$home_team ~ "*", # Coding for cover
                                                .data$evaluation %eq% "Blocked for reattack" & .data$team == .data$visiting_team ~ "a", # Coding for cover
                                                grepl("^(Winning|Positive|Poor|Spike in play)", .data$evaluation) & .data$team == .data$home_team ~ "a", # Coding for dig
                                                grepl("^(Winning|Positive|Poor|Spike in play)", .data$evaluation) & .data$team == .data$visiting_team ~ "*"), # Coding for dig
                           dig_eval_code = case_when(
                               .data$skill %eq% "Attack" & grepl("^Winning", .data$evaluation) ~ "=", ## error dig on a winning attack
                               .data$skill %eq% "Attack" & grepl("^Positive", .data$evaluation) ~ "-", ## negative dig on positive attack
                                                     TRUE ~ "#"), ## Excellent dig otherwise (FIVB recommendations)
                           dig_code = paste0(.data$dig_team, ## Team
                                             "99", ## dig player_number
                                             "D", ## dig skill
                                             str_sub(.data$code, 5, 5), # hitting tempo
                                             .data$dig_eval_code))
        dig_code <- dig_code$dig_code
        dvw$plays <- mutate(dvw$plays, tmp_row_number = row_number())
        newline <- dvw$plays[ridx, ]
        newline$code <- dig_code
        newline$video_time <- newline$video_time + 1
        newline$tmp_row_number <- newline$tmp_row_number + 0.5
        dvw$plays <- dplyr::arrange(bind_rows(dvw$plays, newline), .data$tmp_row_number) ##.data$set_number, .data$point_id, .data$team_touch_id, .data$tmp_row_number)
        dvw$plays <- dplyr::select(dvw$plays, -"tmp_row_number")
    }
    dvw
}


# Test:
# dvw <- datavolley::read_dv("/home/ick003/Documents/Donnees/VolleyBall/GameDatasets/Southern League 2020 Womens (Datavolley)/& State U18 Women-Zuno Women.dvw")
# ridx <- 11
# team = datavolley::home_team(dvw)
# in_player = 7
# out_player = 16
# new_setter = 7
# dv_create_substitution(dvw, team, ridx, in_player, out_player, new_setter)

dv_create_substitution <- function(dvw, team = NULL, ridx = NULL, in_player = NULL, out_player = NULL, new_setter = NULL){
    current_point_id <- dvw$plays$point_id[ridx]
    current_set_number <- dvw$plays$set_number[ridx]
    teamSelect <- team
    
    teamCode = dplyr::case_when(teamSelect %eq% home_team(dvw) ~ "*", 
                                TRUE ~ "a")
    
    current_home_rotation <- c(dvw$plays$home_p1[ridx], dvw$plays$home_p2[ridx],
                               dvw$plays$home_p3[ridx], dvw$plays$home_p4[ridx],
                               dvw$plays$home_p5[ridx], dvw$plays$home_p6[ridx])
    
    current_visiting_rotation <- c(dvw$plays$visiting_p1[ridx], dvw$plays$visiting_p2[ridx],
                                   dvw$plays$visiting_p3[ridx], dvw$plays$visiting_p4[ridx],
                                   dvw$plays$visiting_p5[ridx], dvw$plays$visiting_p6[ridx])
    
    current_rotation = dplyr::case_when(teamSelect %eq% home_team(dvw) ~ current_home_rotation, 
                                        teamSelect %eq% visiting_team(dvw) ~ current_visiting_rotation)
    
    new_rotation <- current_rotation
    if(any(current_rotation %eq% out_player)){new_rotation[current_rotation == out_player] <- in_player}
    
    changedRows <- rotations(dvw, team = teamSelect, start_point_id = current_point_id, 
                             set_number = current_set_number, new_rotation = new_rotation)
    
    toreplace <- dvw$plays[dvw$plays$point_id %in% changedRows$new_rotation$point_id, colnames(changedRows$new_rotation)]
    new_xx <- dplyr::left_join(dplyr::select(toreplace, 'point_id'), changedRows$new_rotation, by = "point_id")
    
    # THe first row of toreplace, which is the rotation line, is kept
    
    #new_xx[1,]<- toreplace[1,]
    
    dvw$plays[dvw$plays$point_id %in% changedRows$new_rotation$point_id, colnames(changedRows$new_rotation)] <- new_xx
    
    ## Add one row for substitution
    
    if (!grepl("^[[:digit:]][[:digit:]]", in_player)) in_player <- stringr::str_c("0", in_player)
    if (!grepl("^[[:digit:]][[:digit:]]", out_player)) out_player <- stringr::str_c("0", out_player)
    
    new_row = dvw$plays[dvw$plays$point_id %eq% current_point_id,][1,]
    new_row[new_row$point_id %in% changedRows$new_rotation$point_id, colnames(changedRows$new_rotation)] <- new_xx[2,]
    new_row$code <- paste0(teamCode,"c", out_player,":", in_player)

    dvw$plays$file_line_number[which(dvw$plays$file_line_number > new_row$file_line_number)] <-  dvw$plays$file_line_number[which(dvw$plays$file_line_number > new_row$file_line_number)] + 1
    
    new_row$file_line_number = new_row$file_line_number + 1
    
    dvw$plays =  dplyr::arrange(dplyr::bind_rows(dvw$plays, new_row), .data$file_line_number)
    
    # If the substitution leads to a change of setter:
    if(new_setter != ""){
        row2change = which(dvw$plays$point_id %in% changedRows$new_rotation$point_id)
        if(teamSelect %eq% datavolley::home_team(dvw)){
            dvw$plays$home_setter_position[row2change] <- dplyr::case_when(dvw$plays$home_p1[row2change] == new_setter ~ 1,
                                                                           dvw$plays$home_p2[row2change] == new_setter ~ 2,
                                                                           dvw$plays$home_p3[row2change] == new_setter ~ 3,
                                                                           dvw$plays$home_p4[row2change] == new_setter ~ 4,
                                                                           dvw$plays$home_p5[row2change] == new_setter ~ 5,
                                                                           dvw$plays$home_p6[row2change] == new_setter ~ 6)
        }
        if(teamSelect %eq% datavolley::visiting_team(dvw)){
            dvw$plays$visiting_setter_position[row2change] <- dplyr::case_when(dvw$plays$visiting_p1[row2change] == new_setter ~ 1,
                                                                               dvw$plays$visiting_p2[row2change] == new_setter ~ 2,
                                                                               dvw$plays$visiting_p3[row2change] == new_setter ~ 3,
                                                                               dvw$plays$visiting_p4[row2change] == new_setter ~ 4,
                                                                               dvw$plays$visiting_p5[row2change] == new_setter ~ 5,
                                                                               dvw$plays$visiting_p6[row2change] == new_setter ~ 6)
        }
        
        # Setter rotation
        
        idxvz = grepl("az[[:digit:]]", dvw$plays$code[row2change])
        new_code_v = stringr::str_replace(dvw$plays$code[row2change][idxvz], "[[:digit:]]", as.character(dvw$plays$visiting_setter_position[row2change][idxvz]))

        idxhz = grepl("\\*z[[:digit:]]", dvw$plays$code[row2change])
        new_code_h = stringr::str_replace(dvw$plays$code[row2change][idxhz], "[[:digit:]]", as.character(dvw$plays$home_setter_position[row2change][idxhz]))

        if(teamSelect %eq% datavolley::visiting_team(dvw)){ dvw$plays$code[row2change][idxvz] <- new_code_v }
        if(teamSelect %eq% datavolley::home_team(dvw)){ dvw$plays$code[row2change][idxhz] <- new_code_h }
        
        # Setter declaration
        
        if (!grepl("^[[:digit:]][[:digit:]]", new_setter)) new_setter <- stringr::str_c("0", new_setter)
        
        new_row = dvw$plays[row2change,][2,]
        new_row$code <- paste0(teamCode,"P", new_setter)
        
        dvw$plays$file_line_number[which(dvw$plays$file_line_number > new_row$file_line_number)] <-  dvw$plays$file_line_number[which(dvw$plays$file_line_number > new_row$file_line_number)] + 1
        
        new_row$file_line_number = new_row$file_line_number + 1
        
        dvw$plays =  dplyr::arrange(dplyr::bind_rows(dvw$plays, new_row), .data$file_line_number)
        
    }
    
    return(dvw)
}

# Test:
# dvw <- datavolley::read_dv("/home/ick003/Documents/Donnees/VolleyBall/GameDatasets/AOVC 2019 Womens (Datavolley)/&qua02 tasmanian-queensland p.dvw")
# setnumber <- 4
# team = datavolley::visiting_team(dvw)
# new_rotation = c(28,25,29,32,24,33)
# new_setter = 28
# dv_change_startinglineup(dvw, team, setnumber, new_rotation, new_setter)
dv_change_startinglineup <- function(dvw, team, setnumber, new_rotation = NULL, new_setter = NULL){
    selectTeam = team
    changedRows <- rotations(dvw, team = selectTeam, set_number = setnumber, new_rotation = new_rotation)
    toreplace <- dvw$plays[dvw$plays$point_id %in% changedRows$new_rotation$point_id, colnames(changedRows$new_rotation)]
    new_xx <- dplyr::left_join(dplyr::select(toreplace, 'point_id'), changedRows$new_rotation, by = "point_id")
    dvw$plays[dvw$plays$point_id %in% changedRows$new_rotation$point_id, colnames(changedRows$new_rotation)] <- new_xx
    
    # Need to update the setter position as well: change the column home/visiting_setter_position and the codes *z1 etc...
    row2change = which(dvw$plays$point_id %in% changedRows$new_rotation$point_id)
    if(selectTeam %eq% datavolley::home_team(dvw)){
        dvw$plays$home_setter_position[row2change] <- dplyr::case_when(dvw$plays$home_p1[row2change] == new_setter ~ 1,
                                                                       dvw$plays$home_p2[row2change] == new_setter ~ 2,
                                                                       dvw$plays$home_p3[row2change] == new_setter ~ 3,
                                                                       dvw$plays$home_p4[row2change] == new_setter ~ 4,
                                                                       dvw$plays$home_p5[row2change] == new_setter ~ 5,
                                                                       dvw$plays$home_p6[row2change] == new_setter ~ 6)
    }
    if(selectTeam %eq% datavolley::visiting_team(dvw)){
        dvw$plays$visiting_setter_position[row2change] <- dplyr::case_when(dvw$plays$visiting_p1[row2change] == new_setter ~ 1,
                                                                           dvw$plays$visiting_p2[row2change] == new_setter ~ 2,
                                                                           dvw$plays$visiting_p3[row2change] == new_setter ~ 3,
                                                                           dvw$plays$visiting_p4[row2change] == new_setter ~ 4,
                                                                           dvw$plays$visiting_p5[row2change] == new_setter ~ 5,
                                                                           dvw$plays$visiting_p6[row2change] == new_setter ~ 6)
    }
    
    # Setter rotation
    
    idxvz = grepl("az[[:digit:]]", dvw$plays$code[row2change])
    new_code_v = stringr::str_replace(dvw$plays$code[row2change][idxvz], "[[:digit:]]", as.character(dvw$plays$visiting_setter_position[row2change][idxvz]))
    
    idxhz = grepl("\\*z[[:digit:]]", dvw$plays$code[row2change])
    new_code_h = stringr::str_replace(dvw$plays$code[row2change][idxhz], "[[:digit:]]", as.character(dvw$plays$home_setter_position[row2change][idxhz]))
    
    dvw$plays$code[row2change][idxvz] <- new_code_v
    dvw$plays$code[row2change][idxhz] <- new_code_h
    
    # Setter declaration
    
    if (!grepl("^[[:digit:]][[:digit:]]", new_setter)) new_setter <- stringr::str_c("0", new_setter)
    
    if(selectTeam %eq% datavolley::visiting_team(dvw)){
        idxvP = grepl("aP[[:digit:]][[:digit:]]", dvw$plays$code[row2change])
        new_code_vP = stringr::str_replace(dvw$plays$code[row2change][idxvP], "[[:digit:]][[:digit:]]", new_setter)
        dvw$plays$code[row2change][idxvP] <- new_code_vP
    }
    if(selectTeam %eq% datavolley::home_team(dvw)){
        idxhP = grepl("\\*P[[:digit:]][[:digit:]]", dvw$plays$code[row2change])
        new_code_hP = stringr::str_replace(dvw$plays$code[row2change][idxhP], "[[:digit:]][[:digit:]]", new_setter)
        dvw$plays$code[row2change][idxhP] <- new_code_hP
    }
    
    return(dvw)
}



# Test:
# dvw <- datavolley::read_dv("/home/ick003/Documents/Donnees/VolleyBall/GameDatasets/Southern League 2020 Mens - Spring (Datavolley)/&fir01 boss men-van diemens_with_vt.dvw")
# team = datavolley::visiting_team(dvw)
# ridx = 101
# tt <- dv_force_rotation(dvw, team, ridx, direction = 1)
dv_force_rotation <- function(dvw, team, ridx, direction){
    selectTeam = team
    current_point_id <- dvw$plays$point_id[ridx]
    current_set_number <- dvw$plays$set_number[ridx]
    new_setter = dplyr::case_when(selectTeam %eq% datavolley::home_team(dvw) ~ 
                                      dplyr::case_when(dvw$plays$home_setter_position[ridx] %eq% 1 ~ dvw$plays$home_p1[ridx],
                                                dvw$plays$home_setter_position[ridx] %eq% 2 ~ dvw$plays$home_p2[ridx], 
                                                dvw$plays$home_setter_position[ridx] %eq% 3 ~ dvw$plays$home_p3[ridx],
                                                dvw$plays$home_setter_position[ridx] %eq% 4 ~ dvw$plays$home_p4[ridx], 
                                                dvw$plays$home_setter_position[ridx] %eq% 5 ~ dvw$plays$home_p5[ridx],
                                                dvw$plays$home_setter_position[ridx] %eq% 6 ~ dvw$plays$home_p6[ridx]),
                                  selectTeam %eq% datavolley::visiting_team(dvw) ~ 
                                      dplyr::case_when(dvw$plays$visiting_setter_position[ridx] %eq% 1 ~ dvw$plays$visiting_p1[ridx],
                                                dvw$plays$visiting_setter_position[ridx] %eq% 2 ~ dvw$plays$visiting_p2[ridx], 
                                                dvw$plays$visiting_setter_position[ridx] %eq% 3 ~ dvw$plays$visiting_p3[ridx],
                                                dvw$plays$visiting_setter_position[ridx] %eq% 4 ~ dvw$plays$visiting_p4[ridx], 
                                                dvw$plays$visiting_setter_position[ridx] %eq% 5 ~ dvw$plays$visiting_p5[ridx],
                                                dvw$plays$visiting_setter_position[ridx] %eq% 6 ~ dvw$plays$visiting_p6[ridx]))
    new_setter = as.character(new_setter)
    rows2change = rotations(dvw,  team = selectTeam, start_point_id = current_point_id, set_number = current_set_number)
    rotation2change = unlist(rows2change$current_rotation[1,8:13])
    names_r2c <- names(rotation2change)
    if(direction == 1){
        new_rotation = rotation2change[c(2:6, 1)]
        names(new_rotation) <- names_r2c
    }
    if(direction == -1){
        new_rotation = rotation2change[c(6,1:5)]
        names(new_rotation) <- names_r2c
    }
    changedRows <- rotations(dvw, team = selectTeam, set_number = current_set_number, start_point_id = current_point_id, new_rotation = new_rotation)
    toreplace <- dvw$plays[dvw$plays$point_id %in% changedRows$new_rotation$point_id, colnames(changedRows$new_rotation)]
    new_xx <- dplyr::left_join(dplyr::select(toreplace, 'point_id'), changedRows$new_rotation, by = "point_id")
    dvw$plays[dvw$plays$point_id %in% changedRows$new_rotation$point_id, colnames(changedRows$new_rotation)] <- new_xx
    
    # Need to update the setter position as well: change the column home/visiting_setter_position and the codes *z1 etc...
    row2change = which(dvw$plays$point_id %in% changedRows$new_rotation$point_id)
    if(selectTeam %eq% datavolley::home_team(dvw)){
        dvw$plays$home_setter_position[row2change] <- dplyr::case_when(dvw$plays$home_p1[row2change] == new_setter ~ 1,
                                                                       dvw$plays$home_p2[row2change] == new_setter ~ 2,
                                                                       dvw$plays$home_p3[row2change] == new_setter ~ 3,
                                                                       dvw$plays$home_p4[row2change] == new_setter ~ 4,
                                                                       dvw$plays$home_p5[row2change] == new_setter ~ 5,
                                                                       dvw$plays$home_p6[row2change] == new_setter ~ 6)
    }
    if(selectTeam %eq% datavolley::visiting_team(dvw)){
        dvw$plays$visiting_setter_position[row2change] <- dplyr::case_when(dvw$plays$visiting_p1[row2change] == new_setter ~ 1,
                                                                           dvw$plays$visiting_p2[row2change] == new_setter ~ 2,
                                                                           dvw$plays$visiting_p3[row2change] == new_setter ~ 3,
                                                                           dvw$plays$visiting_p4[row2change] == new_setter ~ 4,
                                                                           dvw$plays$visiting_p5[row2change] == new_setter ~ 5,
                                                                           dvw$plays$visiting_p6[row2change] == new_setter ~ 6)
    }
    
    # Setter rotation
    
    idxvz = grepl("az[[:digit:]]", dvw$plays$code[row2change])
    new_code_v = stringr::str_replace(dvw$plays$code[row2change][idxvz], "[[:digit:]]", as.character(dvw$plays$visiting_setter_position[row2change][idxvz]))
    
    idxhz = grepl("\\*z[[:digit:]]", dvw$plays$code[row2change])
    new_code_h = stringr::str_replace(dvw$plays$code[row2change][idxhz], "[[:digit:]]", as.character(dvw$plays$visiting_setter_position[row2change][idxhz]))
    
    dvw$plays$code[row2change][idxvz] <- new_code_v
    dvw$plays$code[row2change][idxhz] <- new_code_h
    
    # Setter declaration
    
    if (!grepl("^[[:digit:]][[:digit:]]", new_setter)) new_setter <- stringr::str_c("0", new_setter)
    
    if(selectTeam %eq% datavolley::visiting_team(dvw)){
        idxvP = grepl("aP[[:digit:]][[:digit:]]", dvw$plays$code[row2change])
        new_code_vP = stringr::str_replace(dvw$plays$code[row2change][idxvP], "[[:digit:]][[:digit:]]", new_setter)
        dvw$plays$code[row2change][idxvP] <- new_code_vP
    }
    if(selectTeam %eq% datavolley::home_team(dvw)){
        idxhP = grepl("\\*P[[:digit:]][[:digit:]]", dvw$plays$code[row2change])
        new_code_hP = stringr::str_replace(dvw$plays$code[row2change][idxhP], "[[:digit:]][[:digit:]]", new_setter)
        dvw$plays$code[row2change][idxhP] <- new_code_hP
    }
    
    return(dvw)
}

