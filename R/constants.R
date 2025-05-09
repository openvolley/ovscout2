## define some constants here rather than repeating the given strings throughout the code
## these are used to set `rally_state(...)` and `editing$active <- ...`
.C_editing_shortcut <- "shortcut" ## editing a particular shortcut
.C_editing_shortcuts <- "shortcuts" ## showing the overall shortcuts dialog
.C_confirm_end_of_set <- "confirm end of set"
.C_rally_review <- "rally_review"
.C_admin <- "admin"
.C_video_offset <- "video offset"
.C_preferences <- "preferences"
## don't change the "coord_click_" part of these, there's a regexp that relies on it
.C_coord_click_start <- "coord_click_start"
.C_coord_click_mid <- "coord_click_mid"
.C_coord_click_end <- "coord_click_end"

## don't change these, they are kinda-hard-coded:
## editing$active <- paste0("insert ", where)
.C_insert_above <- "insert above"
.C_insert_below <- "insert below"

.C_edit <- "edit"
.C_delete <- "delete"
.C_match_report <- "match report"
.C_match_data <- "match_data"
.C_change_starting_lineup <- "change starting lineup"
.C_select_teams <- "select_teams"
.C_teams <- "teams"
.C_fix_required_info <- "fix required information before scouting can begin"
.C_click_serve_start <- "click serve start"
.C_click_serve_end <- "click serve end"
.C_click_freeball_end <- "click freeball end point"
.C_click_second <- "click second contact"
.C_click_third <- "click third contact"
.C_click_attack_end <- "click attack end point"
.C_rally_ended <- "rally ended"

