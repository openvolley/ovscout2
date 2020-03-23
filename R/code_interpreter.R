ov_code_interpret <- function(c, attack_table, compound_table, default_scouting_table) {
    if (missing(default_scouting_table) || is.null(default_scouting_table)) {
        default_scouting_table <- dplyr::tribble(~skill, ~default_skill, ~tempo, ~evaluation_code,
                                                 "S", FALSE, "H", "+",
                                                 "R", FALSE, "H", "+",
                                                 "A", FALSE, "H", "+",
                                                 "B", FALSE, "H", "+",
                                                 "D", TRUE, "H", "+",
                                                 "E", FALSE, "H", "+",
                                                 "F", FALSE, "H", "+")
    }
    if (missing(compound_table) || is.null(compound_table)) {
        compound_table <- dplyr::tribble(~skill, ~compound_skill, ~code, ~compound_code,
                                         "S", "R", "/", "/",
                                         "S", "R", "-", "#",
                                         "S", "R", "!", "+",
                                         "S", "R", "+", "-",
                                         "S", "R", "#", "=",
                                         "A", "B", "/", "#",
                                         "A", "B", "-", "+",
                                         "A", "B", "!", "!",
                                         "A", "B", "+", "-",
                                         "A", "B", "#", "=")
    }
    if (missing(attack_table) || is.null(attack_table)) {
        attack_table <- dplyr::tribble(~code, ~attacker_position, ~side, ~type, ~description, ~set_type,
                                       "X1", 3, "C", "Q", "Quick", "C",
                                       "X2", 2, "C", "Q", "Quick set behind", "C",
                                       "X3", 3, "C", "M", "Meter ball", "F",
                                       "X5", 4, "L", "T", "Shoot in 4", "F",
                                       "X6", 2, "R", "T", "Shoot in 2", "F",
                                       "X7", 4, "L", "Q", "Double push", "C",
                                       "X8", 9, "R", "T", "Shoot in 1", "B",
                                       "XP", 8, "C", "T", "Pipe", "P",

                                       "CB", 2, "L", "N", "Slide next to setter", "C",
                                       "CD", 2, "L", "N", "Slide away from setter", "C",
                                       "CF", 2, "L", "N", "Slide close to setter", "C",

                                       "V3", 3, "C", "H", "High ball in 3", "F",
                                       "V5", 4, "L", "H", "High ball in 4", "F",
                                       "V6", 2, "R", "H", "High ball in 2", "F",
                                       "V8", 9, "R", "H", "High ball in 1", "B",
                                       "VP", 8, "C", "H", "Release pipe", "P",

                                       "PP", 3, "L", "O", "Setter tip", "S",
                                       "PR", 3, "C", "O", "Attack on opponent freeball", "-",
                                       "P2", 3, "C", "O", "Second hit to opponent court", "-"
                                       )
    }

    syntax_table <- dplyr::tribble(~code_type, ~name, ~stx_id,  ~range, ~value_list,
                                   "Main code", "team", "mc_te", 1, c("\\*", "a"),                                    # 1
                                   "Main code", "player number", "mc_pn", 2:3, as.character(c(00:99)),               # 2
                                   "Main code", "Skill", "mc_sk", 4, c("S", "R", "A", "B", "D", "E", "F"),           # 3
                                   "Main code", "Type", "mc_ty", 5, c("H", "M", "Q", "T", "U", "N", "O"),             # 4
                                   "Main code", "Evaluation", "mc_ev", 6, c("\\#", "\\+", "\\!", "\\/", "-", "\\="), # 5
                                   "Advanced code", "Cmb/Call", "ac_cc", 7:8, c(paste0("K",0:9), "KM", "KP", attack_table$code), # 6
                                   "Advanced code", "Target attack", "ac_ta", 9, c("F", "C", "B", "P", "S"),         # 7
                                   "Advanced code", "Start zone", "ac_sz", 10, as.character(c(1:9)),                 # 8
                                   "Advanced code", "End zone / Cone", "ac_ez", 11, as.character(c(1:9)),            # 9
                                   "Advanced code", "End subzone", "ac_es", 12, c("A", "B", "C", "D"),                  # 10
                                   "Extended code", "Skill type", "ec_st", 13, c("H", "P", "T", "A", "L", "R", "W", "O", "M", as.character(c(1:5)), "S", "C", "B", "E"),
                                   "Extended code", "Players", "ec_pl", 14, as.character(c(1:9)),                    # 12
                                   "Extended code", "Special", "ec_sp", 15, c("A", "C", "E", "F", "I", "L", "N", "O", "P", "R", "S", "T", "U", "X", "Z"),
                                   "Custom code", "Custom", "cc_cu", 16:20, c("Free characters")                     # 14
                                   )
    blank_code <- str_c(rep("~", 20))
    if (!str_detect(c, "\\.")) {
        ## no compound code
        new_code <- blank_code
        ## Reading from left to right
        cc_tmp <- c
        cmb <- FALSE
        for (i in 1:14) {
            if (i %in% c(6, 7, 8) && cmb) next
            if (i > 1) cc_tmp <- str_to_upper(cc_tmp)
            if (i == 5) {
                ## Eval code can be located pretty much anywhere, cause its syntax is so specific, and people, you know...
                tmp <- str_match(c, syntax_table$value_list[[i]])
                tmp <- tail(tmp[!is.na(tmp)], 1)
            } else {
                c_tmp <- str_sub(cc_tmp, 1, length(syntax_table$range[[i]]))
                tmp <- str_match(c_tmp, syntax_table$value_list[[i]])
                tmp <- tail(tmp[!is.na(tmp)], 1)
            }

            if (length(tmp) > 0) {
                cc_tmp <- str_remove(cc_tmp, tail(syntax_table$value_list[[i]][str_detect(tmp, syntax_table$value_list[[i]])], 1))
            }

            if (i == 1 && length(tmp) == 0) tmp <- "*"

            ## Special case of CMB
            if (i == 6 && length(tmp) > 0) {
                if (tmp %in% attack_table$code) {
                    cmb <- TRUE
                    new_code[4] <- "A"
                    new_code[5] <- attack_table$type[attack_table$code == tmp]
                    att_eval <- na.omit(str_match(c, syntax_table$value_list[[5]]))
                    new_code[6] <- if (length(att_eval) == 1) att_eval else default_scouting_table$evaluation_code[default_scouting_table$skill == "A"] ## default
                    cc_tmp <- str_remove(cc_tmp, syntax_table$value_list[[5]][str_detect(c, syntax_table$value_list[[5]])])
                    new_code[9] <- attack_table$set_type[attack_table$code == tmp]
                    new_code[10] <- attack_table$attacker_position[attack_table$code == tmp]
                }
                if (tmp %in% paste0("K", 0:9)) new_code[4] <- "E"
            }

            ## If no match is found
            if (length(tmp) == 0) {
                ## Check for existing default scouting convention
                if (i == 3) {
                    tmp <- default_scouting_table$skill[default_scouting_table$default_skill == TRUE]
                } else if (i == 4) {
                    tmp <- default_scouting_table$tempo[default_scouting_table$skill == new_code[4]]
                } else if (i == 5) {
                    tmp <- default_scouting_table$evaluation_code[default_scouting_table$skill == new_code[4]]
                } else {
                    tmp <- "~"
                }
            }
            if (tmp == "~") next
            if (str_count(tmp) == length(syntax_table$range[[i]])) {
                ttmp <- unlist(str_split(tmp, ""))
                new_code[syntax_table$range[[i]]] <- ttmp
            }
            if (str_count(tmp) < length(syntax_table$range[[i]])) {
                tmp <- c("0", unlist(str_split(tmp, "")))
                new_code[syntax_table$range[[i]]] <- tmp
            }
        }
        paste0(new_code, collapse = "")
    } else {
        ## Compound code, separate in two codes, before and after the dot.
        new_code_1 <- blank_code
        new_code_2 <- blank_code

        csp <- unlist(str_split(c, "\\."))

        cc_tmp1 <- csp[1]
        cc_tmp2 <- csp[2]

        cmb <- FALSE
        for (i in 1:14) {
            if (i %in% c(6, 7, 8) && cmb) next

            if (i > 1) {
                cc_tmp1 <- str_to_upper(cc_tmp1)
                cc_tmp2 <- str_to_upper(cc_tmp2)
            }

            if (i == 5) {
                ## Eval code can be located pretty much anywhere, cause its syntax is so specific
                tmp1 <- str_match(csp[1], syntax_table$value_list[[i]])
                tmp1 <- tail(tmp1[!is.na(tmp1)], 1)
                tmp2 <- str_match(csp[2], syntax_table$value_list[[i]])
                tmp2 <- tail(tmp2[!is.na(tmp2)], 1)
            } else {
                c_tmp1 <- str_sub(cc_tmp1, 1,length(syntax_table$range[[i]]))
                tmp1 <- str_match(c_tmp1, syntax_table$value_list[[i]])
                tmp1 <- tail(tmp1[!is.na(tmp1)], 1)
                c_tmp2 <- str_sub(cc_tmp2, 1,length(syntax_table$range[[i]]))
                tmp2 <- str_match(c_tmp2, syntax_table$value_list[[i]])
                tmp2 <- tail(tmp2[!is.na(tmp2)], 1)
            }

            if (length(tmp1) > 0) {
                cc_tmp1 <- str_remove(cc_tmp1, tail(syntax_table$value_list[[i]][str_detect(tmp1, syntax_table$value_list[[i]])], 1))
            }
            if (length(tmp2) > 0) {
                cc_tmp2 <- str_remove(cc_tmp2, tail(syntax_table$value_list[[i]][str_detect(tmp2, syntax_table$value_list[[i]])], 1))
            }
            if (i == 1) {
                if (length(tmp1)==0) tmp1 <- "*"
                tmp2 <- dplyr::case_when(tmp1 == "a" ~ "*", TRUE ~ "a")
            }

            if (i == 3 && length(tmp1) > 0) {
                tmp2 <- unique(compound_table$compound_skill[compound_table$skill == tmp1])
            }
            if (i == 4) tmp2 <- tmp1
            if (i == 5) {
                if (length(tmp2) > 0 && new_code_1[4] != "~" && length(tmp1) == 0) {tmp1 <- compound_table$code[compound_table$compound_code == tmp2 & compound_table$skill == new_code_1[4]]}
                if (length(tmp1) > 0 && new_code_2[4] != "~" && length(tmp2) == 0) {tmp2 <- compound_table$code[compound_table$compound_code == tmp1 & compound_table$skill == new_code_2[4]]}
            }

            ## Special case of CMB
            if (i == 6 && length(tmp1) > 0) {
                if (tmp1 %in% attack_table$code) {
                    cmb <- TRUE
                    new_code_1[4] <- "A"
                    new_code_2[4] <- "B"
                    if (new_code_2[6] != "~") new_code_1[6]  <- compound_table$code[compound_table$compound_code == new_code_2[6] & compound_table$skill == new_code_1[4]] 
                    if (new_code_1[6] != "~") new_code_2[6]  <- compound_table$compound_code[compound_table$code == new_code_1[6] & compound_table$compound_skill == new_code_2[4]]
                    new_code_1[5] <- attack_table$type[attack_table$code == tmp1]
                    new_code_2[5] <- attack_table$type[attack_table$code == tmp1]
                                        #cc_tmp1 <- str_remove(cc_tmp1, syntax_table$value_list[[5]][str_detect(csp[1], syntax_table$value_list[[5]])])
                    new_code_1[9] <- attack_table$set_type[attack_table$code == tmp1]
                    new_code_1[10] <- attack_table$attacker_position[attack_table$code == tmp1]
                }
            }
            ## Compound codes have shared sz, ez. So they can be split on both sides, however the general order needs to remain sz, ez
            if (i %in% c(8:9)) {
                if (length(tmp1) == 0) tmp1 <- tmp2
                if (length(tmp2) == 0) tmp2 <- tmp1
                if (length(tmp1) > 0 && length(tmp2) > 0 && i == 8) {
                    new_code_1[syntax_table$range[[i+1]]] <- tmp2
                    new_code_2[syntax_table$range[[i+1]]] <- tmp2
                    tmp2 <- tmp1
                }
            }
            if (i %in% c(11)) {
                tmp1 <- tmp2
                tmp2 <- "~"
            }
            if (i %in% c(10, 12)) {
                if (length(tmp1) == 0) tmp1 <- tmp2
                if (length(tmp2) == 0) tmp2 <- tmp1
            }
            ## If no match is found
            if (length(tmp1) == 0 && length(tmp2) == 0) {
                ## Check for existing default scouting convention
                if (i == 3) {
                    tmp1 <- "~"
                    tmp2 <- "~"
                } else if (i == 5) {
                    tmp2 <- default_scouting_table$evaluation_code[default_scouting_table$skill == new_code_2[4]]
                    tmp1 <- compound_table$code[compound_table$compound_code == tmp2 & compound_table$skill == new_code_1[4]]
                }
            }

            if (length(tmp1) == 0 && length(tmp2) > 0) {
                tmp1 <- str_c(rep("~", length(syntax_table$range[[i]])), collapse = "")
            }
            if (length(tmp2) == 0 && length(tmp1) > 0) {
                tmp2 <- str_c(rep("~", length(syntax_table$range[[i]])), collapse = "")
            }
            if (length(tmp2) == 0 && length(tmp1) == 0) next

            if (str_count(tmp1) == length(syntax_table$range[[i]])) {
                ttmp1 <- unlist(str_split(tmp1, ""))
                new_code_1[syntax_table$range[[i]]] <- ttmp1
            }
            if (str_count(tmp1) < length(syntax_table$range[[i]])) {
                ttmp1 <- c("0", unlist(str_split(tmp1, "")))
                new_code_1[syntax_table$range[[i]]] <- ttmp1
            }
            if (str_count(tmp2) == length(syntax_table$range[[i]])) {
                ttmp2 <- unlist(str_split(tmp2, ""))
                new_code_2[syntax_table$range[[i]]] <- ttmp2
            }
            if (str_count(tmp2) < length(syntax_table$range[[i]])) {
                ttmp2 <- c("0", unlist(str_split(tmp2, "")))
                new_code_2[syntax_table$range[[i]]] <- ttmp2
            }
        }
        c(paste0(new_code_1, collapse = ""), paste0(new_code_2, collapse = ""))
    }
}
