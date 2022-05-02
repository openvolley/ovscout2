#Prior player selection

player_responsibility_fn <- function(system = "SHM3", skill, setter_position, zone, libs, home_visiting){
    system <- match.arg(system, c("SHM3"))
    if (system == "SHM3") {
        if(skill == "Reception"){
            if(length(libs)> 0) ln = "libero" else ln = NA_character_
            ret <- case_when(setter_position %in% c(3,6) ~ case_when(zone %in% c(1,9,2) ~ paste0(home_visiting,"_p1"),
                                                                     zone %in% c(3,6,8) & length(libs)> 0 ~ ln,
                                                                     zone %in% c(3,6,8) & length(libs)== 0 ~ paste0(home_visiting,"_p5"),
                                                                     zone %in% c(4,7,5) ~ paste0(home_visiting,"_p4")),
                             setter_position %in% c(1) ~ case_when(zone %in% c(1,9,2) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(3,6,8) & length(libs)> 0 ~ ln,
                                                                   zone %in% c(3,6,8) & length(libs)== 0 ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(4,7,5) ~ paste0(home_visiting,"_p5")),
                             setter_position %in% c(2) ~ case_when(zone %in% c(1,9,2) & length(libs)> 0 ~ ln,
                                                                   zone %in% c(1,9,2) & length(libs)== 0 ~ paste0(home_visiting,"_p1"),
                                                                   zone %in% c(3,6,8) ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(4,7,5) ~ paste0(home_visiting,"_p3")),
                             setter_position %in% c(4) ~ case_when(zone %in% c(1,9,2) & length(libs)> 0 ~ ln,
                                                                   zone %in% c(1,9,2) & length(libs)== 0 ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(3,6,8) ~ paste0(home_visiting,"_p5"),
                                                                   zone %in% c(4,7,5) ~ paste0(home_visiting,"_p2")),
                             setter_position %in% c(5) ~ case_when(zone %in% c(1,9,2) & length(libs)> 0  ~ ln,
                                                                   zone %in% c(1,9,2) & length(libs)== 0 ~ paste0(home_visiting,"_p1"),
                                                                   zone %in% c(3,6,8) ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(4,7,5) ~ paste0(home_visiting,"_p3")))
        } else if(skill == "Attack"){
            ret <- case_when(setter_position %in% c(1) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(4) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p5"),
                                                                   TRUE ~ NA_character_),
                             setter_position %in% c(6) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(4) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p1"),
                                                                   TRUE ~ NA_character_),
                             setter_position %in% c(5) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(4) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p6"),
                                                                   TRUE ~ NA_character_),
                             setter_position %in% c(2) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(4) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p6"),
                                                                   zone %in% c(9) ~ paste0(home_visiting,"_p5"),
                                                                   TRUE ~ NA_character_),
                             setter_position %in% c(3) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(4) ~ paste0(home_visiting,"_p4"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p1"),
                                                                   zone %in% c(9) ~ paste0(home_visiting,"_p6"),
                                                                   TRUE ~ NA_character_),
                             setter_position %in% c(4) ~ case_when(zone %in% c(2) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(3) ~ paste0(home_visiting,"_p3"),
                                                                   zone %in% c(4) ~ paste0(home_visiting,"_p2"),
                                                                   zone %in% c(8) ~ paste0(home_visiting,"_p5"),
                                                                   zone %in% c(9) ~ paste0(home_visiting,"_p1"),
                                                                   TRUE ~ NA_character_)
                             )
        }
        ## else if(skill == "Dig"){
        ##     if(length(libs)> 0) ln = "libero" else ln = NA_character_
        ##
        ## }
    } else {
        stop("unrecognized system: ", system)
    }
    ret
}
