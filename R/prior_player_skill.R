#Prior player selection

player_responsibility_fn <- function(system = "SHM3", skill, setter_position, zone, libs, home_visiting, opp_attack_start_zone = NA, serving) {
    system <- match.arg(system, c("SHM3"))
    if (system == "SHM3") {
        if (skill == "Reception") {
            loc <- length(libs) > 0 ## libero on court
            ln <- if (loc) "libero" else NA_character_
            ret <- case_when(setter_position %in% c(3,6) ~ case_when(zone %in% c(1,9,2) ~ paste0(home_visiting,"_p1"),
                                                                     zone %in% c(3,6,8) & loc ~ ln,
                                                                     zone %in% c(3,6,8) ~ paste0(home_visiting,"_p5"),
                                                                     zone %in% c(4,7,5) ~ paste0(home_visiting,"_p4")),
                             setter_position %in% c(1) ~ case_when(zone %in% c(1,9,2) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(3,6,8) & loc ~ ln,
                                                                   zone %in% c(3,6,8) ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(4,7,5) ~ paste0(home_visiting,"_p5")),
                             setter_position %in% c(2) ~ case_when(zone %in% c(1,9,2) & loc ~ ln,
                                                                   zone %in% c(1,9,2) ~ paste0(home_visiting,"_p1"),
                                                                   zone %in% c(3,6,8) ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(4,7,5) ~ paste0(home_visiting,"_p3")),
                             setter_position %in% c(4) ~ case_when(zone %in% c(1,9,2) & loc ~ ln,
                                                                   zone %in% c(1,9,2) ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(3,6,8) ~ paste0(home_visiting,"_p5"),
                                                                   zone %in% c(4,7,5) ~ paste0(home_visiting,"_p2")),
                             setter_position %in% c(5) ~ case_when(zone %in% c(1,9,2) & loc  ~ ln,
                                                                   zone %in% c(1,9,2) ~ paste0(home_visiting,"_p1"),
                                                                   zone %in% c(3,6,8) ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(4,7,5) ~ paste0(home_visiting,"_p3")))
        } else if (skill == "Attack") {
            ret <- case_when(setter_position %in% c(1) ~ case_when(isTRUE(serving) ~
                                                                       case_when(zone %in% c(2, 9) ~ paste0(home_visiting,"_p4"),
                                                                                 zone %in% c(3) ~ paste0(home_visiting,"_p3"),
                                                                                 zone %in% c(4, 7) ~ paste0(home_visiting,"_p2"),
                                                                                 zone %in% c(8) ~ paste0(home_visiting,"_p5")),
                                                                   TRUE ~ case_when(zone %in% c(2, 9) ~ paste0(home_visiting,"_p2"),
                                                                                    zone %in% c(3) ~ paste0(home_visiting,"_p3"),
                                                                                    zone %in% c(4, 7) ~ paste0(home_visiting,"_p4"),
                                                                                    zone %in% c(8) ~ paste0(home_visiting,"_p5"))),
                             setter_position %in% c(6) ~ case_when(zone %in% c(2, 9) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(4, 7) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p1")),
                             setter_position %in% c(5) ~ case_when(zone %in% c(2, 9) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(4, 7) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p6")),
                             setter_position %in% c(2) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(4, 7) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(9) ~ paste0(home_visiting,"_p5")),
                             setter_position %in% c(3) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(4, 7) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p1"),
                                                                   zone %in% c(9) ~ paste0(home_visiting,"_p6")),
                             setter_position %in% c(4) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(4, 7) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p5"),
                                                                   zone %in% c(9) ~ paste0(home_visiting,"_p1")),
                             )
        } else if (skill == "Dig") {
            loc <- length(libs) > 0 ## libero on court
            if (isTRUE(serving) && setter_position %in% c(2, 5)) {
                ## the middle is on court (serving) so we have no libero
                loc <- FALSE
            }
            ln <- if (loc) "libero" else NA_character_
            if (opp_attack_start_zone %in% c(4,7,5)) {
                ret <- case_when(setter_position %in% c(3,6) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p1"),
                                                                         zone %in% c(3, 4) ~ paste0(home_visiting,"_p4"),
                                                                         zone %in% c(1,9) ~ paste0(home_visiting,"_p6"),
                                                                         zone %in% c(2) ~ paste0(home_visiting,"_p3"),
                                                                         zone %in% c(5,7,8) & loc ~ ln,
                                                                         zone %in% c(5,7,8) ~ paste0(home_visiting,"_p5")),
                                 setter_position %in% c(1,4) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p5"),
                                                                         zone %in% c(3, 4) ~ paste0(home_visiting,"_p2"),
                                                                         zone %in% c(1,9) ~ paste0(home_visiting,"_p1"),
                                                                         zone %in% c(2) ~ paste0(home_visiting,"_p4"),
                                                                         zone %in% c(5,7,8) & loc ~ ln,
                                                                         zone %in% c(5,7,8) ~ paste0(home_visiting,"_p6")),
                                 setter_position %in% c(2,5) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p6"),
                                                                         zone %in% c(3, 4) ~ paste0(home_visiting,"_p3"),
                                                                         zone %in% c(1,9) ~ paste0(home_visiting,"_p5"),
                                                                         zone %in% c(2) ~ paste0(home_visiting,"_p2"),
                                                                         zone %in% c(5,7,8) & loc ~ ln,
                                                                         zone %in% c(5,7,8) ~ paste0(home_visiting,"_p1")))
            } else if (opp_attack_start_zone %in% c(3,8,6)) {
                ret <- case_when(setter_position %in% c(3,6) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p1"),
                                                                         zone %in% c(3) ~ paste0(home_visiting,"_p2"),
                                                                         zone %in% c(4) ~ paste0(home_visiting,"_p4"),
                                                                         zone %in% c(1,9) ~ paste0(home_visiting,"_p6"),
                                                                         zone %in% c(2) ~ paste0(home_visiting,"_p3"),
                                                                         zone %in% c(5,7,8) & loc ~ ln,
                                                                         zone %in% c(5,7,8) ~ paste0(home_visiting,"_p5")),
                                 setter_position %in% c(1,4) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p5"),
                                                                         zone %in% c(3) ~ paste0(home_visiting,"_p3"),
                                                                         zone %in% c(4) ~ paste0(home_visiting,"_p2"),
                                                                         zone %in% c(1,9) ~ paste0(home_visiting,"_p1"),
                                                                         zone %in% c(2) ~ paste0(home_visiting,"_p4"),
                                                                         zone %in% c(5,7,8) & loc ~ ln,
                                                                         zone %in% c(5,7,8) ~ paste0(home_visiting,"_p6")),
                                 setter_position %in% c(2,5) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p6"),
                                                                         zone %in% c(3) ~ paste0(home_visiting,"_p4"),
                                                                         zone %in% c(4) ~ paste0(home_visiting,"_p3"),
                                                                         zone %in% c(1,9) ~ paste0(home_visiting,"_p5"),
                                                                         zone %in% c(2) ~ paste0(home_visiting,"_p2"),
                                                                         zone %in% c(5,7,8) & loc ~ ln,
                                                                         zone %in% c(5,7,8) ~ paste0(home_visiting,"_p1")))
            } else if (opp_attack_start_zone %in% c(2,9,1)) {
                ret <- case_when(setter_position %in% c(3,6) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p1"),
                                                                         zone %in% c(2, 3) ~ paste0(home_visiting,"_p3"),
                                                                         zone %in% c(1,9,8) ~ paste0(home_visiting,"_p6"),
                                                                         zone %in% c(4) ~ paste0(home_visiting,"_p4"),
                                                                         zone %in% c(5,7) & loc ~ ln,
                                                                         zone %in% c(5,7) ~ paste0(home_visiting,"_p5")),
                                 setter_position %in% c(1,4) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p5"),
                                                                         zone %in% c(2, 3) ~ paste0(home_visiting,"_p4"),
                                                                         zone %in% c(1,9,8) ~ paste0(home_visiting,"_p1"),
                                                                         zone %in% c(4) ~ paste0(home_visiting,"_p2"),
                                                                         zone %in% c(5,7) & loc ~ ln,
                                                                         zone %in% c(5,7) ~ paste0(home_visiting,"_p6")),
                                 setter_position %in% c(2,5) ~ case_when(zone %in% c(6) ~ paste0(home_visiting,"_p6"),
                                                                         zone %in% c(2, 3) ~ paste0(home_visiting,"_p2"),
                                                                         zone %in% c(1,9,8) ~ paste0(home_visiting,"_p5"),
                                                                         zone %in% c(4) ~ paste0(home_visiting,"_p3"),
                                                                         zone %in% c(5,7) & loc ~ ln,
                                                                         zone %in% c(5,7) ~ paste0(home_visiting,"_p1")))
            } else {
              ret <- NA_character_
            }
        } else if (skill == "Cover") {
            warning("cover responsibility not coded yet")
            ret <- NA_character_
        } else if (skill == "Freeball dig") {
            loc <- length(libs) > 0 ## libero on court
            if (isTRUE(serving) && setter_position %in% c(2, 5)) {
                ## the middle is on court (serving) so we have no libero
                loc <- FALSE
            }
            ln <- if (loc) "libero" else NA_character_
            ret <- case_when(setter_position %in% c(1, 4) ~ case_when(zone %in% c(1,9,2) ~ paste0(home_visiting,"_p5"),
                                                                      loc ~ ln,
                                                                      TRUE ~ paste0(home_visiting,"_p6")),
                             setter_position %in% c(2, 5) ~ case_when(zone %in% c(1,9,2) ~ paste0(home_visiting,"_p6"),
                                                                      loc ~ ln,
                                                                      TRUE ~ paste0(home_visiting,"_p1")),
                             setter_position %in% c(3, 6) ~ case_when(zone %in% c(1,9,2) ~ paste0(home_visiting,"_p1"),
                                                                      loc ~ ln,
                                                                      TRUE ~ paste0(home_visiting,"_p5")))
        }
    } else {
        stop("unrecognized system: ", system)
    }
    ret
}

attack_player_prior_by_code <- function(system = "SHM3", setter_position, set_type, attacker_position, home_visiting, serving) {
    system <- match.arg(system, c("SHM3"))
    if (system == "SHM3") {
        ## use set_type (F, C, B, P, S, -) to figure the attack player pos
        ## also using attacker_position to distinguish front/back row combo codes. If the setter is front row (opposite is back row) and the combo code is for a front-row backset, assign that to the front-row middle, and similarly for a back-row backset when setter is back row
        ret <- case_when(set_type == "S" ~ setter_position,
                         setter_position == 1 ~ case_when(isTRUE(serving) ~ case_when(set_type == "B" & attacker_position %in% 2:4 ~ 4L,
                                                                                      set_type == "B" ~ 3L,
                                                                                      set_type == "C" ~ 3L,
                                                                                      set_type == "F" ~ 2L,
                                                                                      set_type == "P" ~ 5L),
                                                          TRUE ~ case_when(set_type == "B" & attacker_position %in% 2:4 ~ 2L,
                                                                           set_type == "B" ~ 3L,
                                                                           set_type == "C" ~ 3L,
                                                                           set_type == "F" ~ 4L,
                                                                           set_type == "P" ~ 5L)),
                         setter_position == 6 ~ case_when(set_type == "B" & attacker_position %in% 2:4 ~ 3L,
                                                          set_type == "B" ~ 2L,
                                                          set_type == "C" ~ 2L,
                                                          set_type == "F" ~ 4L,
                                                          set_type == "P" ~ 1L),
                         setter_position == 5 ~ case_when(set_type == "B" & attacker_position %in% 2:4 ~ 2L,
                                                          set_type == "B" ~ 4L,
                                                          set_type == "C" ~ 4L,
                                                          set_type == "F" ~ 3L,
                                                          set_type == "P" ~ 6L),
                         setter_position == 2 ~ case_when(set_type == "C" ~ 4L,
                                                          set_type == "F" ~ 3L,
                                                          set_type == "P" ~ 6L,
                                                          set_type == "B" & attacker_position %in% 2:4 ~ 4L,
                                                          set_type == "B" ~ 5L),
                         setter_position == 3 ~ case_when(set_type == "C" ~ 2L,
                                                          set_type == "F" ~ 4L,
                                                          set_type == "P" ~ 1L,
                                                          set_type == "B" & attacker_position %in% 2:4 ~ 2L,
                                                          set_type == "B" ~ 6L),
                         setter_position == 4 ~ case_when(set_type == "C" ~ 3L,
                                                          set_type == "F" ~ 2L,
                                                          set_type == "P" ~ 5L,
                                                          set_type == "B" & attacker_position %in% 2:4 ~ 3L,
                                                          set_type == "B" ~ 1L))
        ifelse(!is.na(ret), paste0(home_visiting, "_p", ret), NA_character_)
    } else {
        stop("unrecognized system: ", system)
    }
}
