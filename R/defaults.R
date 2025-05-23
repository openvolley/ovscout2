#' Default attack combination code table
#'
#' @return A tibble
#'
#' @export
ov_default_attack_table <- function() {
    out <- dplyr::tribble(~code, ~attacker_position, ~side, ~type, ~description, ~X6, ~colour, ~start_coordinate, ~set_type,
                          "CB", 2, "L", "N", "Slide next to setter", NA, 16711680, 4976, "C",
                          "CD", 2, "L", "N", "Slide away from setter", NA, 16711680, 4970, "C",
                          "CF", 2, "L", "N", "Slide close to setter", NA,16711680, 4986, "C",
                          "C0", 7, "C", "U", "Medium Backrow D", NA, 0, 4114, "F",
                          "C5", 4, "R", "U", "11", NA, 0, 4912, "F",
                          "C6", 2, "L", "U", "Medium Red", NA, 0, 4988, "B",
                          "C8", 9, "C", "U", "Medium Backrow A", NA, 0, 4186, "B",
                          "II", 4, "R", "M", "Attack 4 AW", NA, 0, 4912, "F",
                          "JJ", 2, "L", "M", "Back AW", NA, 0, 4988, "B",
                          "PP", 3, "L", "O", "Setter Dump", NA, 16711680, 4964, "S",
                          "PR", 3, "C", "O", "Overpass hit", NA, 255, 4949, "-",
                          "P2", 3, "C", "O", "Attack on 2nd contact", NA, 255, 4949, "-",
                          "VB", 8, "C", "H", "High B Pipe", NA, 255, 4163, "P",
                          "VI", 2, "L", "H", "High banc AW", NA, 255, 4988, "B",
                          "VJ", 4, "R", "H", "High 4 AW", NA, 255, 4912, "F",
                          "VO", 9, "C", "H", "High Pipe 6-1 - oppo", NA, 255, 4163, "B",
                          "VP", 8, "C", "H", "High Pipe", NA, 255, 4150, "P",
                          "VR", 8, "C", "H", "High C Pipe", NA, 255, 4137, "P",
                          "VV", 7, "R", "H", "Emerg 4 high", NA, 0, 4912, "F",
                          "V0", 7, "C", "H", "High Ball Backrow D", NA, 255, 4114, "F",
                          "V3", 3, "C", "O", "High Ball Pos 3", NA, 255, 4950, "-",
                          "V4", 2, "L", "H", "High back short", NA, 255, 4868, "B",
                          "V5", 4, "R", "H", "High Ball Pos 4", NA, 255, 4912, "F",
                          "V6", 2, "L", "H", "High Ball Pos 2", NA, 255, 4988, "B",
                          "V8", 9, "C", "H", "High Ball Backrow A", NA, 255, 4186, "B",
                          "XB", 8, "C", "M", "B Pipe", NA, 16711680, 4163, "P",
                          "XC", 3, "R", "Q", "E Quick", NA, 65280, 4947, "C",
                          "XD", 3, "R", "Q", "Floating B Quick", NA, 65280, 4941, "C",
                          "XF", 2, "L", "N", "Slide by Opposite", NA, 65280, 4976, "B",
                          "XG", 3, "R", "Q", "Soup ball", NA, 65280, 4946, "C",
                          "XL", 2, "C", "Q", "A Quick in Pos 2", NA, 65280, 4868, "C",
                          "XM", 3, "C", "Q", "B Quick in Pos 3", NA, 65280, 4949, "C",
                          "XO", 2, "L", "Q", "C Quick by Opposite", NA, 65280, 4973, "B",
                          "XP", 8, "C", "M", "Pipe", NA, 16711680, 4150, "P",
                          "XQ", 2, "L", "M", "Mezza Dietro C.D.", NA, 16711680, 4976, "B",
                          "XR", 8, "C", "M", "C Pipe", NA, 16711680, 4138, "P",
                          "XS", 2, "L", "Q", "D Quick", NA, 65280, 4981, "C",
                          "XT", 3, "R", "M", "3 by Pos 4 Attacker", NA, 16711680, 4950, "F",
                          "XZ", 4, "R", "Q", "Short B Quick", NA, 65280, 4941, "C",
                          "X0", 7, "C", "T", "Backrow D", NA, 16711680, 4114, "F",
                          "X1", 3, "R", "Q", "A Quick", NA, 65280, 4956, "C",
                          "X2", 2, "L", "Q", "Quick behind", NA, 65280, 4868, "C",
                          "X3", 3, "L", "M", "3 by Pos 2 Attacker", NA, 16711680, 4950, "B",
                          "X4", 2, "L", "M", "4 (Inside 2)", NA, 16711680, 4976, "B",
                          "X5", 4, "R", "T", "Black", NA, 16711680, 4912, "F",
                          "X6", 2, "L", "T", "Red", NA, 16711680, 4988, "B",
                          "X7", 4, "R", "Q", "B Quick", NA, 65280, 4932, "C",
                          "X8", 9, "C", "T", "Backrow A", NA, 16711680, 4186, "B",
                          "X9", 4, "R", "M", "2 (Inside 4)", NA, 16711680, 4924, "F",
                          "ZP", 8, "C", "M", "Medium Pipe", NA, 0, 4150, "P",
                          "Z5", 4, "R", "M", "11", NA, 0, 4912, "F",
                          "Z6", 2, "L", "M", "Medium Red", NA, 0, 4988, "B",
                          "Z8", 9, "C", "M", "Medium Backrow A", NA, 0, 4186, "B"
                          )
    out$X10 <- out$X11 <- NA ## some other, unpopulated columns
    out
}

#' @export
#' @rdname ov_default_attack_table
ov_simplified_attack_table <- function() {
    dplyr::filter(ov_default_attack_table(),
                  .data$code %in% c("X1", "X2", "X7", "XD", "CF", "CD", "PP", "PR", "P2",
                                    "VP", "V0", "V3", "V4", "V5", "V6", "V8",
                                    "XP", "X0", "X5", "X6", "X8"))
}

#' Default setter calls table (where the setter has instructed the middle hitter to run)
#'
#' @param data_type string: "indoor", "beach". The returned table will be empty for "beach"
#' @return A tibble
#'
#' @export
ov_default_setter_calls_table <- function(data_type = "indoor") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    if (data_type == "beach") {
        tibble(code = character(), X2 = logical(), description = character(), X4 = logical(), colour = character(), start_coordinate = integer(),
               mid_coordinate = integer(), end_coordinate = integer(), path = character(), path_colour = character(), X11 = logical())
    } else {
        tribble(~code, ~X2, ~description, ~X4, ~colour, ~start_coordinate, ~mid_coordinate, ~end_coordinate, ~path, ~path_colour, ~X11,
                "K1", NA, "Front quick", NA, 16711680L, 3949L, 4454L, 4958L, NA_character_, NA_integer_, NA,
                "K2", NA, "Back quick", NA, 16711680L, 3864L, 4278L, 4974L, NA_character_, NA_integer_, NA,
                "K7", NA, "B-quick (shoot)", NA, 16711680L, 3923L, 4426L, 4930L, NA_character_, NA_integer_, NA,
                ## "KC", NA, "Quick in 3", NA, 16711680L, 3849L, 4449L, 5049L, NA_character_, NA_integer_, NA,
                "KE", NA, "No first tempo", NA, 0L, 0L, 0L, 0L, "5858,5826,6426,6458,6458", 12632256L, NA)
    }
}

#' Default winning symbols table
#'
#' @return A tibble
#'
#' @export
ov_default_winning_symbols <- function() {
    tribble(~skill, ~win_lose, ~code,
            "S", "L", "=",
            "S", "W", "#",
            "R", "L", "=",
            "A", "L", "=",
            "A", "L", "/",
            "A", "W", "#",
            "B", "L", "=",
            "B", "L", "/",
            "B", "W", "#",
            "D", "L", "=",
            "E", "L", "=",
            "F", "L", "=")
}

#' Default compound skills table
#'
#' @return A tibble
#'
#' @export
ov_default_compound_table <- function() {
    dplyr::tribble(~skill, ~compound_skill, ~code, ~compound_code,~default_compound_skills,
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
                   "A", "D", "-", "+", FALSE,
                   "A", "D", "-", "!", FALSE,
                   "A", "D", "+", "-", FALSE,
                   "A", "D", "+", "/", FALSE,
                   "A", "D", "#", "=", FALSE,
                   ## add freeball-over to freeball-dig combinations, noting that these are not standard in e.g. DV
                   "F", "F", "+", "-", TRUE,
                   "F", "F", "!", "!", TRUE,
                   "F", "F", "-", "#", TRUE,
                   "F", "F", "-", "+", TRUE)
}


#' Default scouting (type and evaluation for each skill) table
#'
#' @return A tibble
#'
#' @export
ov_default_scouting_table <- function() {
    dplyr::tribble(~skill, ~default_skill, ~tempo, ~evaluation_code,
                   "S", FALSE, "H", "+",
                   "R", FALSE, "H", "+",
                   "A", FALSE, "H", "+",
                   "B", FALSE, "H", "+",
                   "D", TRUE, "H", "+",
                   "E", FALSE, "H", "+",
                   "F", FALSE, "H", "+")
}

#' Default keyboard shortcuts for ov_scouter
#'
#' `ov_default_click_shortcuts` apply when `using scout_mode = "click"`, and `ov_default_type_shortcuts` apply when `using scout_mode = "type"`
#'
#' Shortcuts should be defined in terms of the printable representation of the key (e.g. "a", "$", "H", "Escape", "Enter"). See <https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values> for guidance. Shortcuts can optionally use modifier keys: "Ctrl-x" means pressing the control key and x simultaneously; similarly Alt-x, Meta-x, Shift-x.
#' Be aware that some keys are hard-coded for specific functionality and might cause problems if you use them as shortcuts (e.g. "Enter", "Tab"), and some keys have browser-level or operating-system-level handling that cannot be overridden.
#'
#' @return A named list
#'
#' @export
ov_default_click_shortcuts <- function() ov_default_shortcuts(scout_mode = "click")

#' @export
#' @rdname ov_default_click_shortcuts
ov_default_type_shortcuts <- function() ov_default_shortcuts(scout_mode = "type")

ov_default_shortcuts <- function(scout_mode = "click") {
    scout_mode <- tolower(scout_mode)
    scout_mode <- match.arg(scout_mode, c("click", "type"))
    if (scout_mode == "click") {
        list(hide_popup = c("z", "Z"),
             pause = c("q", "0", "Escape"),
             pause_no_popup = c("Q", "Alt-Escape"),
             undo = c("u", "U", "Ctrl-a"),
             switch_video = c("s"),
             contact = c("w"), ## TODO reconsider this
             video_rewind_1_30 = c("b", "7"),
             video_rewind_0.1 = c("n", "1"),
             video_rewind_2 = c("j", "4", "a"),
             video_rewind_10 = c("h", "$"),
             video_forward_1_30 = c(",", "9"),
             video_forward_0.1 = c("m", "3"),
             video_forward_2 = c("l", "6", "d"),
             video_forward_10 = c(";", "^"),
             video_faster = ">",
             video_slower = "<"
             )
    } else {
        list(hide_popup = c(),
             pause = "Escape",
             pause_no_popup = "Alt-Escape",
             undo = "Ctrl-a",
             switch_video = c(),
             contact = c(), ## not used
             video_rewind_1_30 = c(),
             video_rewind_0.1 = c(),
             video_rewind_2 = "Ctrl-ArrowLeft",
             video_rewind_10 = "Alt-ArrowLeft", ## NB C-A-arrows don't work for some reason, even though at least some C-A-otherkeys do
             video_forward_1_30 = c(),
             video_forward_0.1 = c(),
             video_forward_2 = "Ctrl-ArrowRight",
             video_forward_10 = "Alt-ArrowRight",
             video_faster = c(),
             video_slower = c(),
             assign_point_top = "Ctrl-ArrowUp",
             assign_point_bottom = "Ctrl-ArrowDown",
             switch_windows = "Tab",
             save_file = "Ctrl-s"
             )
    }
}

#' @rdname ov_default_click_shortcuts
#' @export
ov_default_playstable_shortcuts <- function() {
    ## same default shortcuts regardless of scout mode
    ## shortcuts here can use modifier keys: Ctrl-x is ctrl and x, ditto Alt-x, Meta-x, Shift-x
    ## NOTE that the actual key should be lower case here (but not the modifiers Ctrl-, Alt-, Shift-, or Meta-)
    list(edit_code = "Enter",
         delete_code = "Delete",
         insert_code = "Insert",
         up = "ArrowUp",
         down = "ArrowDown",
         switch_windows = "Tab",
         go_to_time = "g"
         )
}

#' Default keyboard remapping for ov_scouter
#'
#' These key remappings only apply to input into the scouting bar (when the scouter has been started with `scout_mode = "type"`).
#'
#' @param scout_mode string: either "click" for the guided point-and-click scouting interface, or "type" for the typing-based interface. Currently remapping has no effect with "click"
#' @return A named list
#'
#' @export
ov_default_key_remapping <- function(scout_mode = "click") {
    scout_mode <- tolower(scout_mode)
    scout_mode <- match.arg(scout_mode, c("click", "type"))
    if (scout_mode == "click") {
        list()
    } else {
        ## remappings can use modifier keys: C-x is ctrl and x, A-x is alt-x, M-x is meta-x, S-x is shift-x
        ## list entries are `to = "from"`
        list(a = ";")
    }
}

shortcut2json <- function(key, to) {
    paste0("'", tolower(grepl("Ctrl-", key, fixed = TRUE)), "|", ## ctrl
           tolower(grepl("Alt-", key, fixed = TRUE)), "|", ## alt
           tolower(grepl("Shift-", key, fixed = TRUE)), "|", ## shift
           tolower(grepl("Meta-", key, fixed = TRUE)), "|", ## meta
           sub("Ctrl-", "", sub("Alt-", "", sub("Shift-", "", sub("Meta-", "", key, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
           "': '", to, "'")
}

make_js_keymap <- function(sc) {
    ## e.g. list(undo = c("Ctrl-a")) to "{'true|false|false|false|a': 'undo'}"
    paste0("{",
           paste(unique(unlist(lapply(seq_along(sc), function(i) {
               if (length(sc[[i]]) > 0) shortcut2json(sc[[i]], to = names(sc)[i])
           }))), collapse = ", "),
           "}")
}
