mod_teamslists_ui <- function(id) {
    ns <- NS(id)
    tagList(tags$head(tags$style("#hroster {padding-left: 0px; padding-right: 0px; background-color: #bfefff; padding: 12px;} #vroster {padding-left: 0px; padding-right: 0px; background-color: #bcee68; padding: 12px;}")),
            tags$div(style = "border-radius: 4px; padding: 4px",
                     fluidRow(column(6, id = "hroster", uiOutput(ns("htroster"))),
                              column(6, id = "vroster", uiOutput(ns("vtroster"))))
                     )
            )
}

mod_teamslists <- function(input, output, session, rdata) {
    output$htroster <- renderUI({
        re <- names2roster(rdata$dvw$meta$players_h)
        htn <- rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "*"]
        do.call(tags$div, c(list(tags$strong(htn), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))

    })
    output$vtroster <- renderUI({
        re <- names2roster(rdata$dvw$meta$players_v)
        vtn <- rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "a"]
        do.call(tags$div, c(list(tags$strong(vtn), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
    })
}

mod_courtrot2_ui <- function(id, with_ball_coords = TRUE) {
    ns <- NS(id)
    tags$div(style = "border-radius: 4px; padding: 4px",
             if (with_ball_coords) fluidRow(style = "min-height: 34px;", ## min-height to retain layout when buttons are hidden
                                       column(4, checkboxInput(ns("ballcoordsCI"), label = "Display ball coordinates", value = TRUE)),
                                       column(4, actionButton(ns("cancel_ball_coords"), "Cancel ball coordinates")),
                                       column(4, actionButton(ns("validate_ball_coords"), label = "Accept ball coordinates"))),
             fluidRow(column(12, plotOutput(ns("court_inset"), click = ns("plot_click"), height = "45vh")),),
             fluidRow(column(2, actionButton(ns("rotate_home"), tags$span("Home", icon("redo")))),
                      column(3, offset = 1, uiOutput(ns("switch_serving_ui"), inline = TRUE)),
                      column(2, offset = 1, actionButton(ns("court_inset_swap"), label = "\u21f5", class = "iconbut")),
                      column(2, actionButton(ns("rotate_visiting"), tags$span("Visiting", icon("redo")))))
             )
}

mod_courtrot2 <- function(input, output, session, rdata, game_state, rally_codes, rally_state, styling, with_ball_coords = TRUE) {
    ns <- session$ns
    beach <- is_beach(isolate(rdata$dvw))
    pseq <- if (beach) 1:2 else 1:6

    observeEvent(input$cancel_ball_coords, {
        clear_click_queue()
    })

    output$switch_serving_ui <- renderUI({
        if (rally_state() %in% c("click or unpause the video to start", "click serve start")) {
            actionButton(ns("switch_serving"), "Switch serving team")
        } else {
            ## can't switch serving team once the rally has started
            NULL
        }
    })
    observeEvent(input$switch_serving, {
        game_state$serving <- other(game_state$serving)
        game_state$current_team <- game_state$serving
    })

    rotate_teams <- reactiveValues(home = 0L, visiting = 0L)
    ## we keep a queue of (up to) 3 clicked points
    click_points <- reactiveValues(queue = data.frame(x = numeric(), y = numeric()))
    add_to_click_queue <- function(x, y) {
        if (!is.data.frame(x)) x <- data.frame(x = x, y = y)
        click_points$queue <- if (nrow(click_points$queue) == 3) x else rbind(click_points$queue, x)
        click_points$queue
    }
    clear_click_queue <- function() {
        click_points$queue <- data.frame(x = numeric(), y = numeric())
        click_points$queue
    }
    observeEvent(input$plot_click, {
        req(input$plot_click)
        add_to_click_queue(x = input$plot_click$x, y = input$plot_click$y)
    })

    ## the court plot itself
    court_inset_home_team_end <- reactiveVal("lower")
    ball_coords <- if (with_ball_coords) reactive({input$ballcoordsCI}) else reactive(FALSE)

    ## go to some effort to reduce redraws of the court plot
    plot_data <- reactive({
        htrot <- tibble(number = get_players(game_state, team = "*", dvw = rdata$dvw))
        htrot <- dplyr::left_join(htrot, rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], by = "number")
        vtrot <- tibble(number = get_players(game_state, team = "a", dvw = rdata$dvw))
        vtrot <- dplyr::left_join(vtrot, rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], by = "number")
        ht_setter <- get_setter(game_state, team = "*")
        ht_libxy <- vt_libxy <- NULL
        libs <- get_liberos(game_state, team = "*", dvw = rdata$dvw)
        if (length(libs)) {
            ht_libxy <- tibble(number = libs) %>%
                dplyr::left_join(rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], by = "number")
            ht_libxy$pos <- c(5, 7)[seq_len(nrow(ht_libxy))]
            ht_libxy <- cbind(dv_xy(ht_libxy$pos, end = "lower"), ht_libxy) %>% mutate(x = case_when(court_inset_home_team_end() != "lower" ~ .data$x - 1,
                                                                                                     court_inset_home_team_end() == "lower" ~ .data$x + 3))
        }
        vt_setter <- get_setter(game_state, team = "a")
        libs <- get_liberos(game_state, team = "a", dvw = rdata$dvw)
        if (length(libs)) {
            vt_libxy <- tibble(number = libs) %>%
                dplyr::left_join(rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], by = "number")
            vt_libxy$pos <- c(1, 9)[seq_len(nrow(vt_libxy))]
            vt_libxy <- cbind(dv_xy(vt_libxy$pos, end = "upper"), vt_libxy) %>% mutate(x = case_when(court_inset_home_team_end() != "lower" ~ .data$x - 1,
                                                                                                     court_inset_home_team_end() == "lower" ~ .data$x + 3))
        }
        list(htrot = htrot, vtrot = vtrot, ht_setter = ht_setter, vt_setter = vt_setter, ht_libxy = ht_libxy, vt_libxy = vt_libxy, serving = game_state$serving, home_score_start_of_point = game_state$home_score_start_of_point, visiting_score_start_of_point = game_state$visiting_score_start_of_point)
    })
    ## keep track of the digest of the plot_data() object so that we can trigger downstream actions when it has actually changed
    plot_data_digest <- reactiveVal("")
    observe({
        plot_data_digest(digest::digest(plot_data()))
    })

    ## generate the ggplot object of the court with players, this doesn't change within a rally
    base_plot <- reactive({
        p <- ggplot(data = data.frame(x = c(-0.25, 4.25, 4.25, -0.25), y = c(-0.25, -0.25, 7.25, 7.25)), mapping = aes_string("x", "y")) +
            geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = c(0.5, 0.5, 3.5, 3.5)), fill = styling$h_court_colour) +
            geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = 3 + c(0.5, 0.5, 3.5, 3.5)), fill = styling$v_court_colour) +
            ggcourt(labels = NULL, show_zones = FALSE, show_zone_lines = TRUE, court_colour = "indoor")
        px <- isolate(plot_data()) ## don't redraw on every invalidation of plot_data(), because the actual data might not have changed
        blah <- plot_data_digest() ## trigger on this
        htrot <- px$htrot
        vtrot <- px$vtrot
        plxy <- cbind(dv_xy(pseq, end = "lower"), htrot)
        ## player names and circles
        ## home team
        p <- p + geom_polygon(data = court_circle(cz = pseq, end = "lower"), aes_string(group = "id"), fill = styling$h_court_colour, colour = styling$h_court_highlight)
        if (!beach) {
            ## setter
            ht_setter <- px$ht_setter
            if (!is.null(ht_setter) && sum(ht_setter %eq% plxy$number) == 1) {
                p <- p + geom_polygon(data = court_circle(cz = which(ht_setter %eq% plxy$number), end = "lower"), fill = styling$h_court_highlight, colour = "black")
            }
            ## liberos
            if (!is.null(px$ht_libxy)) {
                p <- p + geom_polygon(data = court_circle(px$ht_libxy[, c("x", "y")], end = "lower"), aes_string(group = "id"), fill = styling$libero, colour = "black") +
                    geom_text(data = px$ht_libxy, aes_string("x", "y", label = "number"), size = 6, fontface = "bold", vjust = 0) +
                    geom_text(data = px$ht_libxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
            }
        }
        p <- p + geom_text(data = plxy, aes_string("x", "y", label = "number"), size = 6, fontface = "bold", vjust = 0) +
            geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
        ## visiting team
        plxy <- cbind(dv_xy(pseq, end = "upper"), vtrot)
        p <- p + geom_polygon(data = court_circle(cz = pseq, end = "upper"), aes_string(group = "id"), fill = styling$v_court_colour, colour = styling$v_court_highlight)
        if (!beach) {
            ## setter
            vt_setter <- px$vt_setter
            if (!is.null(vt_setter) && sum(vt_setter %eq% plxy$number) == 1) {
                p <- p + geom_polygon(data = court_circle(cz = which(vt_setter %eq% plxy$number), end = "upper"), fill = styling$v_court_highlight, colour = "black")
            }
            ## liberos
            if (!is.null(px$vt_libxy)) {
                p <- p + geom_polygon(data = court_circle(px$vt_libxy[, c("x", "y")], end = "lower"), aes_string(group = "id"), fill = styling$libero, colour = "black") +
                    geom_text(data = px$vt_libxy, aes_string("x", "y", label = "number"), size = 6, fontface = "bold", vjust = 0) +
                    geom_text(data = px$vt_libxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
            }
        }
        p <- p + geom_text(data = plxy, aes_string("x", "y", label = "number"), size = 6, fontface = "bold", vjust = 0) +
            geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)

        ## add the serving team indicator
        temp <- court_circle(cz = 1, r = 0.2, end = "upper")
        temp$y <- temp$y + 0.75 ## shift to behind baseline
        p <- p + geom_polygon(data = temp, fill = if (px$serving %eq% "a") "white" else NA, colour = "black")
        temp <- court_circle(cz = 1, r = 0.2, end = "lower")
        temp$y <- temp$y - 0.75 ## shift to behind baseline
        p <- p + geom_polygon(data = temp, fill = if (px$serving %eq% "*") "white" else NA, colour = "black")

        ## add the score on the right side
        if (!is.null(px$home_score_start_of_point) && !is.null(px$visiting_score_start_of_point)) {
            scxy <- tibble(score = c(px$home_score_start_of_point, px$visiting_score_start_of_point),
                           x = c(-0.5, -0.5),
                           y = c(3, 4)
                           )
            scxy <- scxy %>% mutate(x = case_when(court_inset_home_team_end() != "lower" ~ .data$x,
                                                  court_inset_home_team_end() == "lower" ~ .data$x + 5))
            p <- p + geom_text(data = scxy, aes_string("x", "y", label = "score"), size = 6, fontface = "bold", vjust = 0)
        }
        if (court_inset_home_team_end() != "lower") p <- p + scale_x_reverse() + scale_y_reverse()
        p
    })

    ## keep track of the digest of the rally_codes() object so that we can trigger the plot update when it has actually changed AND only if we are showing ball coords
    rally_codes_digest <- reactiveVal("")
    observe({
        if (with_ball_coords) rally_codes_digest(digest::digest(rally_codes()))
    })
    output$court_inset <- renderPlot({
        p <- base_plot()
        blah <- rally_codes_digest()
        isolate({
            if (nrow(rally_codes()) > 0 && with_ball_coords) {
                ## plot the current rally actions
                temp_rally_plays2 <- make_plays2(rally_codes(), game_state = game_state, dvw = rdata$dvw)
                temp_rally_plays <- plays2_to_plays(temp_rally_plays2, dvw = rdata$dvw, evaluation_decoder = skill_evaluation_decoder()) ## this is the default evaluation decoder, but it doesn't matter here unless we start e.g. colouring things by evaluation
                temp_rally_plays <- mutate(temp_rally_plays, rn = dplyr::row_number())
                segxy <- bind_rows(temp_rally_plays %>% dplyr::filter(.data$skill == "Serve") %>% dplyr::select(x = "start_coordinate_x", y = "start_coordinate_y", rn = "rn"),
                                   temp_rally_plays %>% dplyr::filter(.data$skill == "Serve") %>% dplyr::select(x = "end_coordinate_x", y = "end_coordinate_y", rn = "rn"),
                                   temp_rally_plays %>% dplyr::filter(!.data$skill %in% c("Serve", "Reception")) %>% dplyr::select(x = "start_coordinate_x", y = "start_coordinate_y", rn = "rn"),
                                   temp_rally_plays %>% dplyr::filter(!.data$skill %in% c("Serve", "Reception") & !is.na(.data$mid_coordinate_x)) %>% dplyr::select(x = "mid_coordinate_x", y = "mid_coordinate_y", rn = "rn") %>% mutate(rn = .data$rn + 0.5)) %>%
                    na.omit() %>% dplyr::arrange(.data$rn)
                if (nrow(segxy) > 0) {
                    ## court module is always plotted assuming that the home team is at the lower end
                    ## but the coordinates will be oriented to the actual video orientation, so flip if needed
                    if (court_inset_home_team_end() != "lower") segxy <- dv_flip_xy(segxy)
                    p <- p + geom_path(data = segxy)
                }
            }
            ##            if (nrow(click_points$queue) > 0) {
            ##                p <- p + geom_point(data = click_points$queue, shape = 16) +
            ##                    geom_path(data = click_points$queue, linetype = "dashed", colour = "black", arrow = arrow(length = unit(0.05, "npc"), ends = "last"))
            ##            }
        })
        p
    })

    observeEvent(input$rotate_home, {
        rotate_teams$home <- 1L
    })
    observeEvent(input$rotate_visiting, {
        rotate_teams$visiting <- 1L
    })

    observeEvent(input$court_inset_swap, {
        court_inset_home_team_end(other_end(court_inset_home_team_end()))
        dojs("document.getElementById('court_inset_swap').blur();") ## un-focus from button
    })
    accept_ball_coords <- reactiveVal(0L)
    observeEvent(input$validate_ball_coords, {
        accept_ball_coords(isolate(accept_ball_coords()) + 1L)
    })
    return(list(rt = rotate_teams, ball_coords_checkbox = ball_coords, accept_ball_coords = accept_ball_coords, click_points = click_points, add_to_click_queue = add_to_click_queue, clear_click_queue = clear_click_queue, home_team_end = court_inset_home_team_end))
}

mod_match_data_edit_ui <- function(id) {
    ns <- NS(id)
    actionButton(ns("edit_match_data_button"), "Edit match data", icon = icon("volleyball-ball"))
}

mod_match_data_edit <- function(input, output, session, rdata, editing, styling) {
    ns <- session$ns
    observeEvent(input$edit_match_data_button, {
        editing$active <- "match_data"
        match_time <- if (!is.na(rdata$dvw$meta$match$time)) {
                          as.POSIXct(rdata$dvw$meta$match$time, origin = "1970-01-01")
                      } else {
                          NULL
                      }
        ## NB the edit and cancel buttons are global, not namespaced by ns()
        showModal(
            vwModalDialog(
                title = "Edit match data", footer = tags$div(actionButton("edit_commit", label = "Update match data (or press Enter)", class = "continue"), actionButton("edit_cancel", label = "Cancel (or press Esc)", class = "cancel")),
                tags$div(
                         fluidRow(column(4, shiny::dateInput(ns("match_edit_date"), label = "Match date:", value = rdata$dvw$meta$match$date)),
                                  column(4, textInput(ns("match_edit_time"), label = "Start time:", value = match_time, placeholder = "HH:MM:SS")),
                                  column(4, textInput(ns("match_edit_season"), label = "Season:", value = rdata$dvw$meta$match$season))),
                         fluidRow(column(4, textInput(ns("match_edit_league"), label = "League:", value = rdata$dvw$meta$match$league)),
                                  column(4, textInput(ns("match_edit_phase"), label = "Phase:", value = rdata$dvw$meta$match$phase)),
                                  column(4, shiny::selectInput(ns("match_edit_home_away"), label = "Home/away:", choices = c("", "Home", "Away"), selected = rdata$dvw$meta$match$home_away))),
                         fluidRow(column(4, textInput(ns("match_edit_day_number"), "Day number:", value = rdata$dvw$meta$match$day_number)),
                                  column(4, textInput(ns("match_edit_match_number"), "Match number:", value = rdata$dvw$meta$match$match_number)),
                                  ##column(2, shiny::selectInput("match_edit_regulation", "Regulation:", choices = c("indoor sideout", "indoor rally point", "beach rally point"), selected = rdata$dvw$meta$match$regulation)),
                                  column(4, shiny::selectInput(ns("match_edit_zones_or_cones"), "Zones or cones:", choices = c("C", "Z"), selected = rdata$dvw$meta$match$zones_or_cones), tags$span(style = "font-size:small", "Note: changing cones/zones here will only change the indicator in the file header, it will not convert a file recorded with zones into one recorded with cones, or vice-versa. Don't change this unless you know what you are doing!")))
                     )
            ))
    })
}

mod_lineup_edit_ui <- function(id) {
    ns <- NS(id)
    actionButton(ns("edit_lineup_button"), "Edit lineups", icon = icon("arrows-alt-h"))
}

mod_lineup_edit <- function(input, output, session, rdata, game_state, editing, video_state, styling) {
    ns <- session$ns
    beach <- is_beach(isolate(rdata$dvw))
    observeEvent(input$edit_lineup_button, {
        editing$active <- "change starting lineup"
        ## pause video
        dojs("vidplayer.pause();") ##dojs("document.getElementById('main_video').pause();")
        video_state$paused <- TRUE
        htidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
        vtidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
        ht_setter <- get_setter(game_state, team = "*")
        if (is.null(ht_setter) || is.na(ht_setter) || ht_setter %eq% 0L) ht_setter <- ""
        vt_setter <- get_setter(game_state, team = "a")
        if (is.null(vt_setter) || is.na(vt_setter) || vt_setter %eq% 0L) vt_setter <- ""
        ht_libs <- get_liberos(game_state = game_state, team = "*", dvw = rdata$dvw)
        vt_libs <- get_liberos(game_state = game_state, team = "a", dvw = rdata$dvw)
        showModal(
            vwModalDialog(
                title = "Edit starting line up", size = "l", footer = tags$div(uiOutput(ns("edit_lineup_commit_ui"), inline = TRUE), actionButton("edit_cancel", label = "Cancel", class = "cancel")),
                tabsetPanel(
                    tabPanel("Home team",
                             tags$style(paste0("#ht_display_team {border: 2px solid ", styling$h_court_colour, ";}")),
                             DT::dataTableOutput(ns("ht_display_team")),
                             wellPanel(
                                 fluidRow(
                                     ##column(1, textInput(ns("ht_set_number"), label = "Set", placeholder = "Set number")),
                                     column(1, textInput(ns("ht_P1"), label = "P1", value = if (!is.null(game_state$home_p1) && !is.na(game_state$home_p1)) game_state$home_p1 else "", placeholder = "P1")),
                                     column(1, textInput(ns("ht_P2"), label = "P2", value = if (!is.null(game_state$home_p2) && !is.na(game_state$home_p2)) game_state$home_p2 else "", placeholder = "P2")),
                                     column(1, textInput(ns("ht_P3"), label = "P3", value = if (!is.null(game_state$home_p3) && !is.na(game_state$home_p3)) game_state$home_p3 else "", placeholder = "P3")),
                                     column(1, textInput(ns("ht_P4"), label = "P4", value = if (!is.null(game_state$home_p4) && !is.na(game_state$home_p4)) game_state$home_p4 else "", placeholder = "P4")),
                                     column(1, textInput(ns("ht_P5"), label = "P5", value = if (!is.null(game_state$home_p5) && !is.na(game_state$home_p5)) game_state$home_p5 else "", placeholder = "P5")),
                                     column(1, textInput(ns("ht_P6"), label = "P6", value = if (!is.null(game_state$home_p6) && !is.na(game_state$home_p6)) game_state$home_p6 else "", placeholder = "P6"))),
                                 fluidRow(
                                     column(1, textInput(ns("ht_setter"), label = "Setter", value = ht_setter, placeholder = "Setter")),
                                     column(1, textInput(ns("ht_libero1"), label = "Libero 1", value = if (length(ht_libs) > 0) ht_libs[1], placeholder = "Libero 1")),
                                     column(1, textInput(ns("ht_libero2"), label = "Libero 2", value = if (length(ht_libs) > 1) ht_libs[2], placeholder = "Libero 2"))
                                 ),
                                 style = paste0("background: ", styling$h_court_colour)
                             )),
                        tabPanel("Visiting team",
                                 tags$style(paste0("#vt_display_team {border: 2px solid ", styling$v_court_colour, ";}")),
                                 DT::dataTableOutput(ns("vt_display_team")),
                                 wellPanel(
                                     fluidRow(
                                         ##column(1, textInput(ns("vt_set_number"), label = "Set", placeholder = "Set number")),
                                         column(1, textInput(ns("vt_P1"), label = "P1", value = if (!is.null(game_state$visiting_p1) && !is.na(game_state$visiting_p1)) game_state$visiting_p1 else "", placeholder = "P1")),
                                         column(1, textInput(ns("vt_P2"), label = "P2", value = if (!is.null(game_state$visiting_p2) && !is.na(game_state$visiting_p2)) game_state$visiting_p2 else "", placeholder = "P2")),
                                         column(1, textInput(ns("vt_P3"), label = "P3", value = if (!is.null(game_state$visiting_p3) && !is.na(game_state$visiting_p3)) game_state$visiting_p3 else "", placeholder = "P3")),
                                         column(1, textInput(ns("vt_P4"), label = "P4", value = if (!is.null(game_state$visiting_p4) && !is.na(game_state$visiting_p4)) game_state$visiting_p4 else "", placeholder = "P4")),
                                         column(1, textInput(ns("vt_P5"), label = "P5", value = if (!is.null(game_state$visiting_p5) && !is.na(game_state$visiting_p5)) game_state$visiting_p5 else "", placeholder = "P5")),
                                         column(1, textInput(ns("vt_P6"), label = "P6", value = if (!is.null(game_state$visiting_p6) && !is.na(game_state$visiting_p6)) game_state$visiting_p6 else "", placeholder = "P6"))),
                                     fluidRow(
                                         column(1, textInput(ns("vt_setter"), label = "Setter", value = vt_setter, placeholder = "Setter")),
                                         column(1, textInput(ns("vt_libero1"), label = "Libero 1", value = if (length(vt_libs) > 0) vt_libs[1], placeholder = "Libero 1")),
                                         column(1, textInput(ns("vt_libero2"), label = "Libero 2", value = if (length(vt_libs) > 1) vt_libs[2], placeholder = "Libero 2"))
                                     ),
                                     style = paste0("background: ", styling$v_court_colour)
                                 ))
                    )
                ))
        })

    output$edit_lineup_commit_ui <- renderUI({
        htok <- nzchar(input$ht_P1) && nzchar(input$ht_P2)
        if (!beach) htok <- htok && nzchar(input$ht_P3) && nzchar(input$ht_P4) && nzchar(input$ht_P5) && nzchar(input$ht_P6) && nzchar(input$ht_setter)
        vtok <- nzchar(input$vt_P1) && nzchar(input$vt_P2)
        if (!beach) vtok <- vtok && nzchar(input$vt_P3) && nzchar(input$vt_P4) && nzchar(input$vt_P5) && nzchar(input$vt_P6) && nzchar(input$vt_setter)
        if (htok && vtok) actionButton("edit_commit", label = "Update teams lineups", class = "continue") else NULL
    })

    output$ht_display_team <- DT::renderDataTable({
        this <- rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "role", "special_role")]
        if (!is.null(this)) {
            DT::datatable(names_first_to_capital(this), rownames = FALSE, selection = "single", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE))
        } else {
            NULL
        }
    })

    output$vt_display_team <- DT::renderDataTable({
        this <- rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "role", "special_role")]
        if (!is.null(this)) {
            DT::datatable(names_first_to_capital(this), rownames = FALSE, selection = "single", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE))
        } else {
            NULL
        }
    })
}

mod_team_edit_ui <- function(id) {
    ns <- NS(id)
    actionButton(ns("edit_teams_button"), "Edit teams", icon = icon("users"))
}

mod_team_edit <- function(input, output, session, rdata, editing, styling) {
    ns <- session$ns
    htdata_edit <- reactiveVal(NULL)
    vtdata_edit <- reactiveVal(NULL)

    observeEvent(input$edit_teams_button, {
        editing$active <- "teams"
        htidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
        vtidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
        ## NB the edit and cancel buttons are global, not namespaced by ns()
        showModal(modalDialog(title = "Edit teams", size = "l", footer = tags$div(actionButton("edit_commit", label = "Update teams data", class = "continue"), actionButton("edit_cancel", label = "Cancel", class = "cancel")),
                              tabsetPanel(
                                  tabPanel("Home team",
                                           fluidRow(column(4, textInput(ns("ht_edit_name"), label = "Team name:", value = rdata$dvw$meta$teams$team[htidx])),
                                                    column(4, textInput(ns("ht_edit_id"), label = "Team ID:", value = rdata$dvw$meta$teams$team_id[htidx])),
                                                    column(4, textInput(ns("ht_edit_coach"), label = "Coach:", value = rdata$dvw$meta$teams$coach[htidx])),
                                                    column(4, textInput(ns("ht_edit_assistant"), label = "Assistant:", value = rdata$dvw$meta$teams$assistant[htidx]))),
                                           DT::dataTableOutput(ns("ht_edit_team")),
                                           wellPanel(
                                               fluidRow(column(2, textInput(ns("ht_new_id"), label = "ID:", placeholder = "ID")),
                                                        column(1, textInput(ns("ht_new_number"), label = "Number:", placeholder = "Number")),
                                                        column(3, textInput(ns("ht_new_lastname"), label = "Last name:", placeholder = "Last name")),
                                                        column(3, textInput(ns("ht_new_firstname"), label = "First name:", placeholder = "First name")),
                                                        column(2, selectInput(ns("ht_new_role"), label = "Role", choices = c("", "libero", "outside", "opposite", "middle", "setter", "unknown"))),
                                                        column(1, selectInput(ns("ht_new_special"), label = "Special", choices = c("", "L", "C")))),
                                               fluidRow(column(3, offset = 9, actionButton(ns("ht_add_player_button"), "Add player")))
                                           ),
                                           uiOutput(ns("ht_delete_player_ui"))
                                           ),
                                  tabPanel("Visiting team",
                                           fluidRow(column(4, textInput(ns("vt_edit_name"), label = "Team name:", value = rdata$dvw$meta$teams$team[vtidx])),
                                                    column(4, textInput(ns("vt_edit_id"), label = "Team ID:", value = rdata$dvw$meta$teams$team_id[vtidx])),
                                                    column(4, textInput(ns("vt_edit_coach"), label = "Coach:", value = rdata$dvw$meta$teams$coach[vtidx])),
                                                    column(4, textInput(ns("vt_edit_assistant"), label = "Assistant:", value = rdata$dvw$meta$teams$assistant[vtidx]))),
                                           DT::dataTableOutput(ns("vt_edit_team")),
                                           wellPanel(
                                               fluidRow(column(2, textInput(ns("vt_new_id"), label = "ID:", placeholder = "ID")),
                                                        column(1, textInput(ns("vt_new_number"), label = "Number:", placeholder = "Number")),
                                                        column(3, textInput(ns("vt_new_lastname"), label = "Last name:", placeholder = "Last name")),
                                                        column(3, textInput(ns("vt_new_firstname"), label = "First name:", placeholder = "First name")),
                                                        column(2, selectInput(ns("vt_new_role"), label = "Role", choices = c("", "libero", "outside", "opposite", "middle", "setter", "unknown"))),
                                                        column(1, selectInput(ns("vt_new_special"), label = "Special", choices = c("", "L", "C")))),
                                               fluidRow(column(3, offset = 9, actionButton(ns("vt_add_player_button"), "Add player")))
                                           ),
                                           uiOutput(ns("vt_delete_player_ui"))
                                           )
                              )
                              ))
    })

    output$ht_edit_team <- DT::renderDataTable({
        if (is.null(htdata_edit())) htdata_edit(rdata$dvw$meta$players_h)
        if (!is.null(htdata_edit())) {
            cols_to_hide <- which(!names(htdata_edit()) %in% c("player_id", "number", "lastname", "firstname", "role", "special_role"))-1L ## 0-based because no row names
            cnames <- names(names_first_to_capital(htdata_edit()))
            DT::datatable(htdata_edit(), rownames = FALSE, colnames = cnames, selection = "single", editable = TRUE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE, columnDefs = list(list(targets = cols_to_hide, visible = FALSE))))
        } else {
            NULL
        }
    }, server = TRUE)

    ht_edit_team_proxy <- DT::dataTableProxy("ht_edit_team")
    observeEvent(input$ht_edit_team_cell_edit, {
        info <- input$ht_edit_team_cell_edit
        isolate(temp <- htdata_edit())
        temp[info$row, info$col+1L] <- DT::coerceValue(info$value, temp[[info$row, info$col+1L]]) ## no row names so +1 on col indices
        DT::replaceData(ht_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
        htdata_edit(temp)
    })
    output$ht_delete_player_ui <- renderUI({
        if (!is.null(input$ht_edit_team_rows_selected)) {
            actionButton(ns("ht_delete_player_button"), "Delete selected player")
        } else {
            NULL
        }
    })
    observeEvent(input$ht_delete_player_button, {
        ridx <- input$ht_edit_team_rows_selected
        if (!is.null(ridx)) {
            temp <- htdata_edit()
            temp <- temp[-ridx, ]
            DT::replaceData(ht_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
            htdata_edit(temp)
        }
    })
    observeEvent(input$ht_add_player_button, {
        chk <- list(input$ht_new_id, input$ht_new_number, input$ht_new_lastname, input$ht_new_firstname)
        if (!any(vapply(chk, is_nnn, FUN.VALUE = TRUE))) {
            try({
                newrow <- tibble(number = as.numeric(input$ht_new_number), player_id = input$ht_new_id, lastname = input$ht_new_lastname, firstname = input$ht_new_firstname, role = if (nzchar(input$ht_new_role)) input$ht_new_role else NA_character_, special_role = if (nzchar(input$ht_new_special)) input$ht_new_special else NA_character_)
                newrow$name <- paste(newrow$firstname, newrow$lastname)
                temp <- bind_rows(htdata_edit(), newrow)
                temp <- dplyr::arrange(temp, .data$number)
                DT::replaceData(ht_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
                htdata_edit(temp)
                ## clear inputs
                updateTextInput(session, "ht_new_number", value = "")
                updateTextInput(session, "ht_new_id", value = "")
                updateTextInput(session, "ht_new_lastname", value = "")
                updateTextInput(session, "ht_new_firstname", value = "")
                updateSelectInput(session, "ht_new_role", selected = "")
                updateSelectInput(session, "ht_new_special", selected = "")
            })
        }
    })
    output$vt_edit_team <- DT::renderDataTable({
        if (is.null(vtdata_edit())) vtdata_edit(rdata$dvw$meta$players_v)
        if (!is.null(vtdata_edit())) {
            cols_to_hide <- which(!names(vtdata_edit()) %in% c("player_id", "number", "lastname", "firstname", "role", "special_role"))-1L ## 0-based because no row names
            cnames <- names(names_first_to_capital(vtdata_edit()))
            DT::datatable(vtdata_edit(), rownames = FALSE, colnames = cnames, selection = "single", editable = TRUE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE, columnDefs = list(list(targets = cols_to_hide, visible = FALSE))))
        } else {
            NULL
        }
    }, server = TRUE)
    vt_edit_team_proxy <- DT::dataTableProxy("vt_edit_team")
    observeEvent(input$vt_edit_team_cell_edit, {
        info <- input$vt_edit_team_cell_edit
        isolate(temp <- vtdata_edit())
        temp[info$row, info$col+1L] <- DT::coerceValue(info$value, temp[[info$row, info$col+1L]]) ## no row names so +1 on col indices
        DT::replaceData(vt_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
        vtdata_edit(temp)
    })
    output$vt_delete_player_ui <- renderUI({
        if (!is.null(input$vt_edit_team_rows_selected)) {
            actionButton(ns("vt_delete_player_button"), "Delete selected player")
        } else {
            NULL
        }
    })
    observeEvent(input$vt_delete_player_button, {
        ridx <- input$vt_edit_team_rows_selected
        if (!is.null(ridx)) {
            temp <- vtdata_edit()
            temp <- temp[-ridx, ]
            DT::replaceData(vt_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
            vtdata_edit(temp)
        }
    })
    observeEvent(input$vt_add_player_button, {
        chk <- list(input$vt_new_id, input$vt_new_number, input$vt_new_lastname, input$vt_new_firstname)
        if (!any(vapply(chk, is_nnn, FUN.VALUE = TRUE))) {
            try({
                newrow <- tibble(number = as.numeric(input$vt_new_number), player_id = input$vt_new_id, lastname = input$vt_new_lastname, firstname = input$vt_new_firstname, role = if (nzchar(input$vt_new_role)) input$vt_new_role else NA_character_, special_role = if (nzchar(input$vt_new_special)) input$vt_new_special else NA_character_)
                newrow$name <- paste(newrow$firstname, newrow$lastname)
                temp <- bind_rows(vtdata_edit(), newrow)
                temp <- dplyr::arrange(temp, .data$number)
                DT::replaceData(vt_edit_team_proxy, temp, resetPaging = FALSE, rownames = FALSE)
                vtdata_edit(temp)
                ## clear inputs
                updateTextInput(session, "vt_new_number", value = "")
                updateTextInput(session, "vt_new_id", value = "")
                updateTextInput(session, "vt_new_lastname", value = "")
                updateTextInput(session, "vt_new_firstname", value = "")
                updateSelectInput(session, "vt_new_role", selected = "")
                updateSelectInput(session, "vt_new_special", selected = "")
            })
        }
    })

    list(htdata_edit = htdata_edit, vtdata_edit = vtdata_edit)

}

mod_teamscores_ui <- function(id, styling) {
    ns <- NS(id)
    tagList(tags$head(tags$style(paste0("@font-face { font-family:'DSEG14'; src: url('css/DSEG14Modern-Regular.woff2') format('woff2'), url('css/DSEG14Modern-Regular.woff') format('woff'); } .scoreboard { background-color:#00000080; border-radius:4px; padding:1px; } .ptscorenum, .setscorenum { padding: 2px; text-align: center; font-family:'DSEG14', sans-serif; } .ptscorenum { font-size:24px; } .setscorenum { font-size:17px; } #hnscore { padding: 2px; text-align: left; font-size:16px;} #vnscore { padding: 2px; text-align: right; font-size:16px;} #tsc_outer {position:absolute; right:14px; width:20vw; -webkit-transform: translateZ(10); z-index:10;}"))),
            fluidRow(class = "scoreboard",
                     column(6, style = paste0("background-color:", styling$h_court_colour),
                            fixedRow(column(9, id = "hnscore", uiOutput(ns("hnaming"))),
                                     column(3, class = "setscorenum", uiOutput(ns("hsetscoring")))),
                            fixedRow(column(3, offset = 9, class = "ptscorenum", uiOutput(ns("hscoring"))))),
                     column(6, style = paste0("background-color:", styling$v_court_colour),
                            fixedRow(column(3, class = "setscorenum", uiOutput(ns("vsetscoring"))),
                                     column(9, id = "vnscore", uiOutput(ns("vnaming")))),
                            fixedRow(column(3, class = "ptscorenum", uiOutput(ns("vscoring"))))
                            )
                     )
            )
}

mod_teamscores <- function(input, output, session, game_state, rdata) {
    ns <- session$ns
    ss <- reactive({
        sets_won <- c(0L, 0L) ## sets won by home, visiting teams
        if (nrow(rdata$dvw$plays2) < 1 || !"code" %in% names(rdata$dvw$plays2)) return(c(0L, 0L))
        set_end_rows <- grep("^\\*\\*[[:digit:]]set", rdata$dvw$plays2$code)
        for (si in seq_along(set_end_rows)) {
            set_plays2 <- rdata$dvw$plays2 %>% dplyr::filter(.data$set_number == si)
            temp <- do.call(rbind, stringr::str_match_all(set_plays2$code, "^[a\\*]p([[:digit:]]+):([[:digit:]]+)"))
            scores <- c(max(as.numeric(temp[, 2]), na.rm = TRUE), max(as.numeric(temp[, 3]), na.rm = TRUE))
            if (is_beach(rdata$dvw)) {
                if (max(scores) >= 21 && abs(diff(scores)) >= 2) {
                    sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                }
            } else {
                if ((si < 5 && max(scores) >= 25 && abs(diff(scores)) >= 2) || max(scores) >= 15 && abs(diff(scores)) >= 2) {
                    sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                }
            }
        }
        sets_won
    })

    output$hnaming <- renderUI({
        tags$strong(rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "*"])
    })
    output$hscoring <- renderUI({
        hs <- game_state$home_score_start_of_point
        if (!is.na(hs)) tags$span(hs) else NULL
    })
    output$hsetscoring <- renderUI({
        if (length(ss()) == 2 && !any(is.na(ss()))) tags$span(ss()[1]) else NULL
    })
    output$vscoring <- renderUI({
        vs <- game_state$visiting_score_start_of_point
        if (!is.na(vs)) tags$span(vs) else NULL
    })
    output$vsetscoring <- renderUI({
        if (length(ss()) == 2 && !any(is.na(ss()))) tags$span(ss()[2]) else NULL
    })
    output$vnaming <- renderUI({
        tags$strong(rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "a"])
    })
}
