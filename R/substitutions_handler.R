# Read the point by point rotation, and update it if necessary with new entries
#' Get team rotations point by point
#'
#' @param x datavolley or data.frame: a datavolley object as returned by \code{read_dv}, or the plays component of that object
#' @param team string: team name
#' @param start_point_id vector: vector of pointids for which to return the rotation
#' @param set_number value: Set number
#' @param new_rotation vector: vector of player number, positionned from 1 to 6.
#'
#' @return list of 2 data.frames. Current data.frame, without changes, and updated data.frame, with new player rotation. 
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'   new_x <- rotations(x, start_point_id = 25, new_rotation = c(9,6,15,4,12,7))
#' }
#' @export
rotations <- function(x, team, start_point_id, set_number, new_rotation = NULL) {
    teamSelect <- if (missing(team)) datavolley::home_team(x) else team
    if (!teamSelect %in% datavolley::teams(x)) stop("team does not appear in the data")
    set_number_select <- if (missing(set_number)) 1 else set_number
    if (missing(start_point_id)) start_point_id <- min(x$plays$point_id[x$plays$set_number == set_number_select])
    
    all_point_ids <- unique(x$plays$point_id[x$plays$set_number == set_number_select])
    
    point_ids <- all_point_ids[which(all_point_ids == start_point_id):length(all_point_ids)]

    is_home <- teamSelect == datavolley::home_team(x)
    player_table <- if (is_home) x$meta$players_h else x$meta$players_v
    player_table <- dplyr::select(player_table, "number", "player_id", "special_role", "role")

    # Point id may not uniquely identify rotation, because substitutions will affect a point id as well. So we need to create our own unique ids. 
    # Say, when the skill is equal to serve, or Timeout
    
    x$plays$skill[grepl("z[[:digit:]]",x$plays$code)] <- "Rotation"
    
    if (is_home) {
        ## old was x_tmp <- dplyr::distinct(dplyr::select(dplyr::filter(dplyr::select(x$plays, 'point_id','skill',tidyselect::starts_with("home_player_id")), .data$point_id %in% point_ids, .data$skill %in% c("Serve", "Timeout", "Rotation")), 'point_id', tidyselect::starts_with("home_player_id")))
        x_tmp <- dplyr::distinct(dplyr::filter(dplyr::select(x$plays, 'point_id','skill',tidyselect::starts_with("home_player_id")), .data$point_id %in% point_ids, .data$skill %in% c("Serve", "Timeout")))
    } else {
        x_tmp <- dplyr::distinct(dplyr::filter(dplyr::select(x$plays, 'point_id', 'skill', tidyselect::starts_with("visiting_player_id")), .data$point_id %in% point_ids, .data$skill %in% c("Serve", "Timeout")))
    }
    if (is_home){
        x_tmp_long <- tidyr::pivot_longer(x_tmp, tidyselect::starts_with("home_player_id"), names_to = "position", values_to = "home_player_id")
        x_tmp_long$position <- stringr::str_remove(x_tmp_long$position, "home_player_id")
    } else {
        x_tmp_long <- tidyr::pivot_longer(x_tmp, tidyselect::starts_with("visiting_player_id"), names_to = "position", values_to = "visiting_player_id")
        x_tmp_long$position <- stringr::str_remove(x_tmp_long$position, "visiting_player_id")
    }
    if (is_home){
        x_tmp_long <- dplyr::left_join(x_tmp_long, dplyr::rename(dplyr::select(player_table, 'number', 'player_id'), "home_p" = "number", "home_player_id" = "player_id"), by = "home_player_id")
        x_tmp_wide <- tidyr::pivot_wider(x_tmp_long, id_cols = "point_id", names_from = "position", values_from = c("home_player_id", "home_p"), names_sep = "")
    } else {
        x_tmp_long <- dplyr::left_join(x_tmp_long, dplyr::rename(dplyr::select(player_table, 'number', 'player_id'), "visiting_p" = "number", "visiting_player_id" = "player_id"), by = "visiting_player_id")
        x_tmp_wide <- tidyr::pivot_wider(x_tmp_long,id_cols = "point_id", names_from = "position", values_from = c("visiting_player_id", "visiting_p"), names_sep = "")
    }
    x_tmp_wide_new <- NULL
    if (!is.null(new_rotation)){
        new_rotation <- as.character(new_rotation)
        if (sum(new_rotation %in% player_table$number) < 6) stop("Not all players are on the team list. Please update.")
        if (is_home){
            starting_rotation <- stringr::str_c("\\b", as.character(x_tmp_long$home_p[1:6]), "\\b", collapse="|")
            replaceRot <- function(rot) new_rotation[which(rot == as.character(x_tmp_long$home_p[1:6]))]
            x_tmp_long$new_p <- stringr::str_replace_all(x_tmp_long$home_p, starting_rotation, replaceRot)
            x_tmp_long$new_p <- as.numeric(x_tmp_long$new_p)
            x_tmp_long_new <- dplyr::rename(dplyr::select(dplyr::left_join(x_tmp_long, dplyr::select(player_table, 'number', 'player_id'), by = c("new_p" = "number")),
                                                          'point_id', 'position', 'new_p', 'player_id'), "home_p" = "new_p", "home_player_id" = "player_id")
            x_tmp_wide_new <- tidyr::pivot_wider(x_tmp_long_new,id_cols = "point_id", names_from = "position", values_from = c("home_player_id","home_p"), names_sep = "")
        } else {
            starting_rotation <- stringr::str_c("\\b", as.character(x_tmp_long$visiting_p[1:6]), "\\b", collapse="|")
            replaceRot <- function(rot) new_rotation[which(rot == as.character(x_tmp_long$visiting_p[1:6]))]
            x_tmp_long$new_p <- stringr::str_replace_all(x_tmp_long$visiting_p, starting_rotation, replaceRot)
            x_tmp_long$new_p <- as.numeric(x_tmp_long$new_p)
            x_tmp_long_new <- dplyr::rename(dplyr::select(dplyr::left_join(x_tmp_long, dplyr::select(player_table,'number', 'player_id'), by = c("new_p" = "number")),
                                                          'point_id', 'position', 'new_p', 'player_id'), "visiting_p" = "new_p", "visiting_player_id" = "player_id")
            x_tmp_wide_new <- tidyr::pivot_wider(x_tmp_long_new,id_cols = "point_id", names_from = "position", values_from = c("visiting_player_id","visiting_p"), names_sep = "")
        }
    }
    return(list(current_rotation = x_tmp_wide, new_rotation = x_tmp_wide_new))
}






