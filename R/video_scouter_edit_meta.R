## match data editing
match_data_edit_modal <- function(dvw) {
    match_time <- if (!is.na(dvw$meta$match$time)) {
                      as.POSIXct(dvw$meta$match$time, origin = "1970-01-01")
                  } else {
                      NULL
                  }
    showModal(modalDialog(title = "Edit match data", size = "l", footer = tags$div(actionButton("edit_commit", label = "Update match data (or press Enter)"), actionButton("edit_cancel", label = "Cancel (or press Esc)")),
                          tags$div(
                                   fluidRow(column(4, shiny::dateInput("match_edit_date", label = "Match date:", value = dvw$meta$match$date)),
                                            column(4, textInput("match_edit_time", label = "Start time:", value = match_time, placeholder = "HH:MM:SS")),
                                            column(4, textInput("match_edit_season", label = "Season:", value = dvw$meta$match$season))),
                                   fluidRow(column(4, textInput("match_edit_league", label = "League:", value = dvw$meta$match$league)),
                                            column(4, textInput("match_edit_phase", label = "Phase:", value = dvw$meta$match$phase)),
                                            column(4, shiny::selectInput("match_edit_home_away", label = "Home/away:", choices = c("", "Home", "Away"), selected = dvw$meta$match$home_away))),
                                   fluidRow(column(4, textInput("match_edit_day_number", "Day number:", value = dvw$meta$match$day_number)),
                                            column(4, textInput("match_edit_match_number", "Match number:", value = dvw$meta$match$match_number)),
                                            ##column(2, shiny::selectInput("match_edit_regulation", "Regulation:", choices = c("indoor sideout", "indoor rally point", "beach rally point"), selected = dvw$meta$match$regulation)),
                                            column(4, shiny::selectInput("match_edit_zones_or_cones", "Zones or cones:", choices = c("C", "Z"), selected = dvw$meta$match$zones_or_cones), tags$span(style = "font-size:small", "Note: changing cones/zones here will only change the indicator in the file header, it will not convert a file recorded with zones into one recorded with cones, or vice-versa. Don't change this unless you know what you are doing!")))
                               )
                          ))
}


team_data_edit_modal <- function(dvw) {
    htidx <- which(dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
    vtidx <- which(dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
    showModal(modalDialog(title = "Edit teams", size = "l", footer = tags$div(actionButton("edit_commit", label = "Update teams data"), actionButton("edit_cancel", label = "Cancel")),
                          tabsetPanel(
                              tabPanel("Home team",
                                       fluidRow(column(4, textInput("ht_edit_name", label = "Team name:", value = dvw$meta$teams$team[htidx])),
                                                column(4, textInput("ht_edit_id", label = "Team ID:", value = dvw$meta$teams$team_id[htidx])),
                                                column(4, textInput("ht_edit_coach", label = "Coach:", value = dvw$meta$teams$coach[htidx])),
                                                column(4, textInput("ht_edit_assistant", label = "Assistant:", value = dvw$meta$teams$assistant[htidx]))),
                                       DT::dataTableOutput("ht_edit_team"),
                                       wellPanel(
                                           fluidRow(column(2, textInput("ht_new_id", label = "ID:", placeholder = "ID")),
                                                    column(1, textInput("ht_new_number", label = "Number:", placeholder = "Number")),
                                                    column(3, textInput("ht_new_lastname", label = "Last name:", placeholder = "Last name")),
                                                    column(3, textInput("ht_new_firstname", label = "First name:", placeholder = "First name")),
                                                    column(2, selectInput("ht_new_role", label = "Role", choices = c("", "libero", "outside", "opposite", "middle", "setter", "unknown"))),
                                                    column(1, selectInput("ht_new_special", label = "Special", choices = c("", "L", "C")))),
                                           fluidRow(column(3, offset = 9, actionButton("ht_add_player_button", "Add player")))
                                       ),
                                       uiOutput("ht_delete_player_ui")
                                       ),
                              tabPanel("Visiting team",
                                       fluidRow(column(4, textInput("vt_edit_name", label = "Team name:", value = dvw$meta$teams$team[vtidx])),
                                                column(4, textInput("vt_edit_id", label = "Team ID:", value = dvw$meta$teams$team_id[vtidx])),
                                                column(4, textInput("vt_edit_coach", label = "Coach:", value = dvw$meta$teams$coach[vtidx])),
                                                column(4, textInput("vt_edit_assistant", label = "Assistant:", value = dvw$meta$teams$assistant[vtidx]))),
                                       DT::dataTableOutput("vt_edit_team"),
                                       wellPanel(
                                           fluidRow(column(2, textInput("vt_new_id", label = "ID:", placeholder = "ID")),
                                                    column(1, textInput("vt_new_number", label = "Number:", placeholder = "Number")),
                                                    column(3, textInput("vt_new_lastname", label = "Last name:", placeholder = "Last name")),
                                                    column(3, textInput("vt_new_firstname", label = "First name:", placeholder = "First name")),
                                                    column(2, selectInput("vt_new_role", label = "Role", choices = c("", "libero", "outside", "opposite", "middle", "setter", "unknown"))),
                                                    column(1, selectInput("vt_new_special", label = "Special", choices = c("", "L", "C")))),
                                           fluidRow(column(3, offset = 9, actionButton("vt_add_player_button", "Add player")))
                                       ),
                                       uiOutput("vt_delete_player_ui")
                                       )
                          )
                          ))
}


code_make_change <- function(editing_active, dvw, input, htdata_edit = NULL, vtdata_edit = NULL) {
    removeModal()
    do_reparse <- FALSE
    if (is.null(editing_active)) {
        ## not triggered from current editing activity, huh?
        warning("code_make_change entered but editing not active")
    } else if (editing_active %eq% "teams") {
        ## update from all the input$ht_edit_name/id/coach/assistant inputs
        htidx <- which(dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
        dvw$meta$teams$team[htidx] <- input$ht_edit_name
        dvw$meta$teams$team_id[htidx] <- input$ht_edit_id
        dvw$meta$teams$coach[htidx] <- input$ht_edit_coach
        dvw$meta$teams$assistant[htidx] <- input$ht_edit_assistant
        if (!is.null(htdata_edit)) {
            dvw$meta$players_h <- htdata_edit
            dvw$meta$players_h$name <- paste(dvw$meta$players_h$firstname, dvw$meta$players_h$lastname)
        }
        ## and visiting team
        vtidx <- which(dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
        dvw$meta$teams$team[vtidx] <- input$vt_edit_name
        dvw$meta$teams$team_id[vtidx] <- input$vt_edit_id
        dvw$meta$teams$coach[vtidx] <- input$vt_edit_coach
        dvw$meta$teams$assistant[vtidx] <- input$vt_edit_assistant
        if (!is.null(vtdata_edit)) {
            dvw$meta$players_v <- vtdata_edit
            dvw$meta$players_v$name <- paste(dvw$meta$players_v$firstname, dvw$meta$players_v$lastname)
        }
        do_reparse <- TRUE
    } else if (editing_active %eq% "match_data") {
        dvw$meta$match$date <- input$match_edit_date
        dvw$meta$match$time <- tryCatch(lubridate::hms(input$match_edit_time), error = function(e) lubridate::as.period(NA))
        dvw$meta$match$season <- input$match_edit_season
        dvw$meta$match$league <- input$match_edit_league
        dvw$meta$match$phase <- input$match_edit_phase
        dvw$meta$match$home_away <- input$match_edit_home_away
        dvw$meta$match$day_number <- input$match_edit_day_number
        dvw$meta$match$match_number <- input$match_edit_match_number
        ## currently disabled dvw$meta$match$regulation <- input$match_edit_regulation
        dvw$meta$match$zones_or_cones <- input$match_edit_zones_or_cones
        do_reparse <- TRUE
    } ##else if (editing_active %eq% "change starting lineup") {
    ##                if(input$ht_set_number != "" && input$ht_P1  != ""  && input$ht_P2 != "" &&
    ##                   input$ht_P3 != "" &&  input$ht_P4 != "" && input$ht_P5 != "" &&
    ##                   input$ht_P6 != "" && input$ht_setter != ""){
    ##                    team = datavolley::home_team(dvw)
    ##                    setnumber = input$ht_set_number
    ##                    new_setter = input$ht_setter
    ##                    new_rotation = c(input$ht_P1,input$ht_P2,input$ht_P3,input$ht_P4,input$ht_P5,input$ht_P6)
    ##                    new_rotation_id = dvw$meta$players_h$player_id[match(new_rotation, dvw$meta$players_h$number)]
    ##                    # Change meta data in terms of starting rotation
    ##                    dvw$meta$players_h[,paste0("starting_position_set", setnumber)] <- as.character(match(dvw$meta$players_h$player_id, new_rotation_id))
    ##                    ## Change libero to "*" in meta
    ##                    ## BR not sure if this is needed, it was commented out
    ##                    ##dvw$meta$players_h[dvw$meta$players_h$number %eq% input$ht_libero,paste0("starting_position_set", setnumber)] <- "*"
    ##                    # Change in play rotation 
    ##                    dvw <- dv_change_startinglineup(dvw, team, setnumber, new_rotation, new_rotation_id, new_setter)
    ##                }
    ##                if(input$vt_set_number != "" && input$vt_P1  != ""  && input$vt_P2 != "" &&
    ##                   input$vt_P3 != "" &&  input$vt_P4 != "" && input$vt_P5 != "" && 
    ##                   input$vt_P6 != "" && input$vt_setter != ""){
    ##                    team = datavolley::visiting_team(dvw)
    ##                    setnumber = input$vt_set_number
    ##                    new_setter = input$vt_setter
    ##                    new_rotation = c(input$vt_P1,input$vt_P2,input$vt_P3,input$vt_P4,input$vt_P5,input$vt_P6)
    ##                    new_rotation_id = dvw$meta$players_v$player_id[match(new_rotation, dvw$meta$players_v$number)]
    ##                    # Change meta data in terms of starting rotation
    ##                    dvw$meta$players_v[,paste0("starting_position_set", setnumber)] <- as.character(match(dvw$meta$players_v$player_id, new_rotation_id))
    ##                    ## Change libero to "*" in meta
    ##                    ## BR not sure if this is needed, it was commented out
    ##                    ##dvw$meta$players_v[dvw$meta$players_v$number %eq% input$vt_libero,paste0("starting_position_set", setnumber)] <- "*"
    ##                    # Change in play rotation 
    ##                    dvw <- dv_change_startinglineup(dvw, team, setnumber, new_rotation, new_rotation_id, new_setter)
    ##                }
    ##                do_reparse <- TRUE
    ##            }
    ##            if (do_reparse) {
    ##                ## reparse the dvw
    ##                dvw <- reparse_dvw(dvw, dv_read_args = dv_read_args)
    ##                playslist_needs_scroll(TRUE)
    ##            }
    list(dvw = dvw, do_reparse = do_reparse)
}
