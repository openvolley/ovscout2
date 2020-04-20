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
        compound_table <- dplyr::tribble(~skill, ~compound_skill, ~code, ~compound_code,~default_compound_skills,
                                         "S", "R", "/", "/", TRUE,
                                         "S", "R", "-", "#", TRUE,
                                         "S", "R", "-", "+", TRUE,
                                         "S", "R", "!", "!", TRUE,
                                         "S", "R", "+", "-", TRUE,
                                         "S", "R", "#", "=", TRUE,
                                         "A", "B", "/", "#", TRUE,
                                         "A", "B", "-", "+", TRUE,
                                         "A", "B", "!", "!", TRUE,
                                         "A", "B", "+", "-", TRUE,
                                         "A", "B", "#", "=", TRUE,
                                         "A", "D", "-", "#", FALSE,
                                         "A", "D", "+", "-", FALSE,
                                         "A", "D", "#", "=", FALSE)
    }
    if (missing(attack_table) || is.null(attack_table)) {
        attack_table = dplyr::tribble(~code, ~attacker_position, ~ side, ~type, ~description,~set_type, 
                         "CB",2,"L","N","Slide next to setter","C",
                         "CD",2,"L","N","Slide away from setter","C",
                         "CF",2,"L","N","Slide close to setter","C",
                         "C0",7,"C","U","Medium Backrow D","F",
                         "C5",4,"R","U","11","F",
                         "C6",2,"L","U","Medium Red","B",
                         "C8",9,"C","U","Medium Backrow A","B",
                         "II",4,"R","M","Attack 4 AW","F",
                         "JJ",2,"L","M","Back AW","B",
                         "PP",3,"L","O","Setter Dump","S",
                         "PR",3,"C","O","Overpass hit","-",
                         "P2",3,"C","O","Attack on 2nd contact","-",
                         "VB",8,"C","H","High Ball B Pipe","P",
                         "VI",2,"L","H","High banc AW","B",
                         "VJ",4,"R","H","High 4 AW","F",
                         "VO",9,"C","H","High Pipe 6-1 - oppo","B",
                         "VP",8,"C","H","Release Pipe","P",
                         "VR",8,"C","H","High Ball C Pipe","P",
                         "VV",7,"R","H","Emerg 4 high","F",
                         "V0",7,"C","H","High Ball Backrow D","F",
                         "V3",3,"C","O","High Ball Pos 3","-",
                         "V4",2,"L","H","High back short","B",
                         "V5",4,"R","H","High Ball Pos 4","F",
                         "V6",2,"L","H","High Ball Pos 2","B",
                         "V8",9,"C","H","High Ball Backrow A","B",
                         "XB",8,"C","M","B Pipe","P",
                         "XC",3,"R","Q","E Quick","C",
                         "XD",3,"R","Q","Floating B Quick","C",
                         "XF",2,"L","N","Slide by Opposite","B",
                         "XG",3,"R","Q","Soup ball","C",
                         "XL",2,"C","Q","A Quick in Pos 2","C",
                         "XM",3,"C","Q","B Quick in Pos 3","C",
                         "XO",2,"L","Q","C Quick by Opposite","B",
                         "XP",8,"C","M","Pipe","P",
                         "XQ",2,"L","M","Mezza Dietro C.D.","B",
                         "XR",8,"C","M","C Pipe","P",
                         "XS",2,"L","Q","D Quick","C",
                         "XT",3,"R","M","3 by Pos 4 Attacker","F",
                         "XZ",4,"R","Q","Short B Quick","C",
                         "X0",7,"C","T","Backrow D","F",
                         "X1",3,"R","Q","A Quick","C",
                         "X2",2,"L","Q","C Quick","C",
                         "X3",3,"L","M","3 by Pos 2 Attacker","B",
                         "X4",2,"L","M","4 (Inside 2)","B",
                         "X5",4,"R","T","Black","F",
                         "X6",2,"L","T","D","B",
                         "X7",4,"R","Q","B Quick","C",
                         "X8",9,"C","T","Backrow A","B",
                         "X9",4,"R","M","2 (Inside 4)","F",
                         "ZP",8,"C","M","Medium Pipe","P",
                         "Z5",4,"R","M","11","F",
                         "Z6",2,"L","M","Medium red","B",
                         "Z8",9,"C","M","Medium Backrow A","B"
        )
    }

    syntax_table <- dplyr::tribble(~code_type, ~name, ~stx_id,  ~range, ~value_list,
                                   "Main code", "team", "mc_te", 1, c("\\*", "a"),                                   # 1
                                   "Main code", "player number", "mc_pn", 2:3, formatC(0:99, width = 2, flag = "0"), # 2
                                   "Main code", "Skill", "mc_sk", 4, c("S", "R", "A", "B", "D", "E", "F"),           # 3
                                   "Main code", "Type", "mc_ty", 5, c("H", "M", "Q", "T", "U", "N", "O"),            # 4
                                   "Main code", "Evaluation", "mc_ev", 6, c("\\#", "\\+", "\\!", "\\/", "-", "\\="), # 5
                                   ##"Advanced code", "Cmb/Call", "ac_cc", 7:8, c(paste0("K",0:9), "KM", "KP", attack_table$code), # 6
                                   ## use all possible valid setter calls and attack combos
                                   "Advanced code", "Cmb/Call", "ac_cc", 7:8, c(paste0("K", c(0:9, toupper(letters))),
                                                                                attack_table$code,
                                                                                unlist(lapply(c("C", "I", "J", "L", "P", "V", "W", "X", "Y", "Z"), paste0, c(0:9)))), # 6
                                   "Advanced code", "Target attack", "ac_ta", 9, c("F", "C", "B", "P", "S"),         # 7
                                   "Advanced code", "Start zone", "ac_sz", 10, as.character(c(1:9)),                 # 8
                                   "Advanced code", "End zone / Cone", "ac_ez", 11, as.character(c(1:9)),            # 9
                                   "Advanced code", "End subzone", "ac_es", 12, c("A", "B", "C", "D"),               # 10
                                   "Extended code", "Skill type", "ec_st", 13, list(c("H", "P", "T"),  # Attack
                                                                                    c("A","T"), # Block
                                                                                    c("L", "R", "W", "O", "M"), # Reception
                                                                                    as.character(c(1:5)), # Set
                                                                                    c("S", "C", "B", "E")), # Dig
                                   "Extended code", "Players", "ec_pl", 14, list(as.character(c(0:4)),as.character(c(1:9))),                  # 12
                                   "Extended code", "Special", "ec_sp", 15, list(c("S", "O", "F", "X", "N", "C","I", "A", "Z"), # Attack
                                                                                 c("S", "O", "F", "X", "N", "C","I", "A", "P", "T","Z"), # Block
                                                                                 c("U", "X", "P", "E","Z"), # Reception
                                                                                 c("O", "L", "R", "N", "Z"), #Serve
                                                                                 c("U", "I", "Z"), # Set
                                                                                 c("U", "X", "P", "Z", "F", "O", "E"), # Dig
                                                                                 c("U", "X", "P", "Z")),
                                   "Custom code", "Custom", "cc_cu", 16:20, c(LETTERS, as.character(c(1:9)))                     # 14
                                   )
    blank_code <- str_c(rep("~", 20))
    if(str_detect(c, "~")){
        paste0(c, paste0(rep("~", 20 - nchar(c)), collapse=""), collapse = "")
    } else if (!str_detect(c, "\\.")) {
        ## no compound code
        new_code <- blank_code
        ## Reading from left to right
        cc_tmp <- c
        cmb <- FALSE
        for (i in 1:14) {
            if (i %in% c(6, 7, 8) && cmb) next
            if (i > 1) cc_tmp <- str_to_upper(cc_tmp)
            if (i == 2) {
                ## if the code has been entered with single-digit player numbers that aren't zero-padded, need to zero pad
                if (grepl("^[[:digit:]][^[:digit:]]", cc_tmp)) cc_tmp <- str_c("0", cc_tmp)
            }
            if (i == 5) {
                ## Eval code can be located pretty much anywhere, cause its syntax is so specific, and people, you know...
                value_list = syntax_table$value_list[[i]]
                tmp <- str_match(c, value_list)
                tmp <- tail(tmp[!is.na(tmp)], 1)
            } else if (i == 11) {
                value_list <- dplyr::case_when(new_code[4] == "A" ~syntax_table$value_list[[i]][1],
                                              new_code[4] == "B" ~syntax_table$value_list[[i]][2],
                                              new_code[4] == "R" ~syntax_table$value_list[[i]][3],
                                              new_code[4] == "S" ~syntax_table$value_list[[i]][4],
                                              new_code[4] == "D" ~syntax_table$value_list[[i]][5], 
                                              TRUE ~ syntax_table$value_list[[i]][5])[[1]]
                c_tmp <- str_sub(cc_tmp, 1, length(syntax_table$range[[i]]))
                tmp <- str_match(c_tmp, value_list)
                tmp <- tail(tmp[!is.na(tmp)], 1)
            } else if (i == 12) {
                value_list = dplyr::case_when(new_code[4] %in% c("A","B") ~ syntax_table$value_list[[i]][1],
                                              new_code[4] == "R" ~ syntax_table$value_list[[i]][2],
                                              TRUE ~ syntax_table$value_list[[i]][1])[[1]]
                c_tmp <- str_sub(cc_tmp, 1, length(syntax_table$range[[i]]))
                tmp <- str_match(c_tmp, value_list)
                tmp <- tail(tmp[!is.na(tmp)], 1)
            } else if (i == 13) {
                value_list = dplyr::case_when(new_code[4] == "A" ~syntax_table$value_list[[i]][1],
                                              new_code[4] == "B" ~syntax_table$value_list[[i]][2],
                                              new_code[4] == "R" ~syntax_table$value_list[[i]][3],
                                              new_code[4] == "S" ~syntax_table$value_list[[i]][4],
                                              new_code[4] == "D" ~syntax_table$value_list[[i]][5], 
                                              new_code[4] == "F" ~syntax_table$value_list[[i]][6], 
                                              TRUE ~ syntax_table$value_list[[i]][6])[[1]]
                c_tmp <- str_sub(cc_tmp, 1, length(syntax_table$range[[i]]))
                tmp <- str_match(c_tmp, value_list)
                tmp <- tail(tmp[!is.na(tmp)], 1)
            } else {
                value_list = syntax_table$value_list[[i]]
                c_tmp <- str_sub(cc_tmp, 1, length(syntax_table$range[[i]]))
                tmp <- str_match(c_tmp, syntax_table$value_list[[i]])
                tmp <- tail(tmp[!is.na(tmp)], 1)
            }
            if (length(tmp) > 0) {
                cc_tmp <- str_remove(cc_tmp, tail(value_list[str_detect(tmp, value_list)], 1))
            } else if (grepl("^~", cc_tmp)) {
                cc_tmp <- substr(cc_tmp, 2, nchar(cc_tmp)) ## strip leading ~
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
                    if(sum(str_detect(c, syntax_table$value_list[[5]])) > 0) cc_tmp <- str_remove(cc_tmp, syntax_table$value_list[[5]][str_detect(c, syntax_table$value_list[[5]])])
                    new_code[9] <- "~"## should not have target in attack codes, only set codes (?) ## attack_table$set_type[attack_table$code == tmp]
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
                } else if (i == 11) {
                    tmp = "~"
                    if (new_code[4] == "A") tmp <- "H" # Default to hard hit when the skill is attack
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
                ttmp <- c("0", unlist(str_split(tmp, "")))
                new_code[syntax_table$range[[i]]] <- ttmp
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
        reverse <- FALSE
        for (i in 1:14) {
            if (i %in% c(6, 7, 8) && cmb) next

            if (i > 1) {
                cc_tmp1 <- str_to_upper(cc_tmp1)
                cc_tmp2 <- str_to_upper(cc_tmp2)
            }
            if (i == 2) {
                ## if the code has been entered with single-digit player numbers that aren't zero-padded, need to zero pad
                if (grepl("^[[:digit:]][^[:digit:]]", cc_tmp1)) cc_tmp1 <- str_c("0", cc_tmp1)
                if (grepl("^[[:digit:]][^[:digit:]]", cc_tmp2)) cc_tmp2 <- str_c("0", cc_tmp2)
            }
            if (i == 5) {
                value_list1 = syntax_table$value_list[[i]]
                value_list2 = syntax_table$value_list[[i]]
                ## Eval code can be located pretty much anywhere, cause its syntax is so specific
                tmp1 <- str_match(csp[1], value_list1)
                tmp1 <- tail(tmp1[!is.na(tmp1)], 1)
                tmp2 <- str_match(csp[2], value_list2)
                tmp2 <- tail(tmp2[!is.na(tmp2)], 1)
            } else if (i == 11) {
                value_list1 = dplyr::case_when(new_code_1[4] == "A" ~syntax_table$value_list[[i]][1],
                                               new_code_1[4] == "B" ~syntax_table$value_list[[i]][2],
                                               new_code_1[4] == "R" ~syntax_table$value_list[[i]][3],
                                               new_code_1[4] == "S" ~syntax_table$value_list[[i]][4],
                                               new_code_1[4] == "D" ~syntax_table$value_list[[i]][5], 
                                               TRUE ~ syntax_table$value_list[[i]][5])[[1]]
                value_list2 = dplyr::case_when(new_code_2[4] == "A" ~syntax_table$value_list[[i]][1],
                                               new_code_2[4] == "B" ~syntax_table$value_list[[i]][2],
                                               new_code_2[4] == "R" ~syntax_table$value_list[[i]][3],
                                               new_code_2[4] == "S" ~syntax_table$value_list[[i]][4],
                                               new_code_2[4] == "D" ~syntax_table$value_list[[i]][5], 
                                               TRUE ~ syntax_table$value_list[[i]][5])[[1]]
                c_tmp1 <- str_sub(cc_tmp1, 1, length(syntax_table$range[[i]]))
                tmp1 <- str_match(c_tmp1, value_list1)
                tmp1 <- tail(tmp1[!is.na(tmp1)], 1)
                c_tmp2 <- str_sub(cc_tmp2, 1, length(syntax_table$range[[i]]))
                tmp2 <- str_match(c_tmp2, value_list2)
                tmp2 <- tail(tmp2[!is.na(tmp2)], 1)
                # Check if maybe the syntax if reversed
                if(length(tmp1) == 0 & length(tmp2) == 0){
                    tmp1 <- str_match(c_tmp2, value_list1)
                    tmp1 <- tail(tmp1[!is.na(tmp1)], 1)
                    reverse = TRUE
                }
            } else if (i == 12) {
                value_list1 = dplyr::case_when(new_code_1[4] %in% c("A","B") ~ syntax_table$value_list[[i]][1],
                                              new_code_1[4] == "R" ~ syntax_table$value_list[[i]][2],
                                              TRUE ~ syntax_table$value_list[[i]][1])[[1]]
                value_list2 = dplyr::case_when(new_code_2[4] %in% c("A","B") ~ syntax_table$value_list[[i]][1],
                                               new_code_2[4] == "R" ~ syntax_table$value_list[[i]][2],
                                               TRUE ~ syntax_table$value_list[[i]][1])[[1]]
                c_tmp1 <- str_sub(cc_tmp1, 1, length(syntax_table$range[[i]]))
                tmp1 <- str_match(c_tmp1, value_list1)
                tmp1 <- tail(tmp1[!is.na(tmp1)], 1)
                c_tmp2 <- str_sub(cc_tmp2, 1, length(syntax_table$range[[i]]))
                tmp2 <- str_match(c_tmp2, value_list2)
                tmp2 <- tail(tmp2[!is.na(tmp2)], 1)
            }else if (i == 13) {
                value_list1 = dplyr::case_when(new_code_1[4] == "A" ~syntax_table$value_list[[i]][1],
                                               new_code_1[4] == "B" ~syntax_table$value_list[[i]][2],
                                               new_code_1[4] == "R" ~syntax_table$value_list[[i]][3],
                                               new_code_1[4] == "S" ~syntax_table$value_list[[i]][4],
                                               new_code_1[4] == "D" ~syntax_table$value_list[[i]][5], 
                                               new_code_1[4] == "F" ~syntax_table$value_list[[i]][6], 
                                               TRUE ~ syntax_table$value_list[[i]][5])[[1]]
                value_list2 = dplyr::case_when(new_code_2[4] == "A" ~syntax_table$value_list[[i]][1],
                                               new_code_2[4] == "B" ~syntax_table$value_list[[i]][2],
                                               new_code_2[4] == "R" ~syntax_table$value_list[[i]][3],
                                               new_code_2[4] == "S" ~syntax_table$value_list[[i]][4],
                                               new_code_2[4] == "D" ~syntax_table$value_list[[i]][5],
                                               new_code_2[4] == "F" ~syntax_table$value_list[[i]][6], 
                                               TRUE ~ syntax_table$value_list[[i]][5])[[1]]
                c_tmp1 <- str_sub(cc_tmp1, 1, length(syntax_table$range[[i]]))
                tmp1 <- str_match(c_tmp1, value_list1)
                tmp1 <- tail(tmp1[!is.na(tmp1)], 1)
                c_tmp2 <- str_sub(cc_tmp2, 1, length(syntax_table$range[[i]]))
                tmp2 <- str_match(c_tmp2, value_list2)
                tmp2 <- tail(tmp2[!is.na(tmp2)], 1)
            } else {
                value_list1 = syntax_table$value_list[[i]]
                value_list2 = syntax_table$value_list[[i]]
                c_tmp1 <- str_sub(cc_tmp1, 1,length(syntax_table$range[[i]]))
                tmp1 <- str_match(c_tmp1, value_list1)
                tmp1 <- tail(tmp1[!is.na(tmp1)], 1)
                c_tmp2 <- str_sub(cc_tmp2, 1,length(syntax_table$range[[i]]))
                tmp2 <- str_match(c_tmp2, value_list2)
                tmp2 <- tail(tmp2[!is.na(tmp2)], 1)
            }

            if (length(tmp1) > 0) {
                cc_tmp1 <- str_remove(cc_tmp1, tail(value_list1[str_detect(tmp1,  value_list1)], 1))
            }
            if (length(tmp2) > 0) {
                cc_tmp2 <- str_remove(cc_tmp2, tail(value_list2[str_detect(tmp2, value_list2)], 1))
            }
            if (reverse) {
                cc_tmp2 <- str_remove(cc_tmp2, tail(value_list1[str_detect(tmp1, value_list1)], 1))
                reverse  = FALSE
            }
            if (i == 1) {
                if (length(tmp1)==0) tmp1 <- "*"
                tmp2 <- dplyr::case_when(tmp1 == "a" ~ "*", TRUE ~ "a")
            }

            if (i == 3 && length(tmp1) > 0) {
                tmp2 <- unique(compound_table$compound_skill[compound_table$skill == tmp1 & compound_table$default_compound_skills == TRUE])
            }
            if (i == 4) tmp2 <- tmp1
            if (i == 5) {
                if (length(tmp2) > 0 && new_code_1[4] != "~" && length(tmp1) == 0) {
                    tmp1 <- compound_table$code[compound_table$compound_code == tmp2 & compound_table$skill == new_code_1[4]]}
                if (length(tmp1) > 0 && new_code_2[4] != "~" && length(tmp2) == 0) {
                    tmp2 <- compound_table$code[compound_table$compound_code == tmp1 & compound_table$skill == new_code_2[4]]}
            }

            ## Special case of CMB
            if (i == 6 && length(tmp1) > 0) {
                if (tmp1 %in% attack_table$code) {
                    cmb <- TRUE
                    new_code_1[4] <- "A"
                    if(new_code_2[4] == "~") new_code_2[4] = unique(compound_table$compound_skill[compound_table$skill == "A" & compound_table$default_compound_skills == TRUE])
                    if (new_code_2[6] != "~") new_code_1[6]  <- compound_table$code[compound_table$compound_code == new_code_2[6] & compound_table$skill == new_code_1[4] & compound_table$compound_skill == new_code_2[4]] 
                    if (new_code_1[6] != "~") new_code_2[6]  <- compound_table$compound_code[compound_table$code == new_code_1[6] & compound_table$compound_skill == new_code_2[4] & compound_table$skill == new_code_1[4]]
                    new_code_1[5] <- attack_table$type[attack_table$code == tmp1]
                    new_code_2[5] <- attack_table$type[attack_table$code == tmp1]
                                        #cc_tmp1 <- str_remove(cc_tmp1, syntax_table$value_list[[5]][str_detect(csp[1], syntax_table$value_list[[5]])])
                    new_code_1[9] <- "~"## should not have target in attack codes, only set codes (?) ## attack_table$set_type[attack_table$code == tmp1]
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
