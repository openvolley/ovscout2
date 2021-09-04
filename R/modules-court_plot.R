## module for team roster UI
disambig_names <- function(last, first) {
    firstinit <- substr(first, 1, 1)
    didx <- which(last %in% last[duplicated(last)])
    last[didx] <- paste(firstinit[didx], last[didx])
    last
}

names2roster <- function(pm) {
    pm <- dplyr::arrange(pm, .data$number)
    pm$lastname <- disambig_names(pm$lastname, pm$firstname)
    lc <- paste(ifelse(grepl("L", pm$special_role), "L", ""), ifelse(grepl("C", pm$special_role), "C", ""), sep = ",")
    lc <- sub("^,", "", sub(",$", "", lc))
    lc[nzchar(lc)] <- paste0(" (", lc[nzchar(lc)], ")")
    pm$lastname <- paste0(pm$lastname, lc)
    str_trim(paste0(pm$number, " ", pm$lastname))
}

mod_courtrot_ui <- function(id) {
    ns <- NS(id)
    tags$div(style = "border-radius: 4px; padding: 4px",
             fluidRow(style = "min-height: 34px;", ## min-height to retain layout when buttons are hidden
                      column(4, checkboxInput(ns("ballcoordsCI"), label = "Display ball coordinates", value = TRUE)),
                      column(4, actionButton(ns("cancel_ball_coords"), "Cancel ball coordinates")),
                      column(4, actionButton(ns("validate_ball_coords"), label = "Accept ball coordinates"))),
             fluidRow(column(1, offset = 1, actionButton(ns("rotate_home"),icon("undo"))),
                      column(1, offset = 8, actionButton(ns("rotate_visiting"), icon("undo")))),
             fluidRow(column(3, id = "hroster", uiOutput(ns("htroster"))),
                      column(6, plotOutput(ns("court_inset"), click = ns("plot_click"))),
                      column(3, id = "vroster", uiOutput(ns("vtroster")))),
             fluidRow(column(2, offset = 5, actionButton(ns("court_inset_swap"), label = "\u21f5", class = "iconbut")))
             )
}

mod_courtrot <- function(input, output, session, rdata, rowidx, styling) {
    output$htroster <- renderUI({
        re <- names2roster(rdata$dvw$meta$players_h)
        do.call(tags$div, c(list(tags$strong("Home team"), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
    })
    output$vtroster <- renderUI({
        re <- names2roster(rdata$dvw$meta$players_v)
        do.call(tags$div, c(list(tags$strong("Visiting team"), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
    })

    observeEvent(input$cancel_ball_coords, {
        clear_click_queue()
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
    ball_coords <- reactive({input$ballcoordsCI})
    output$court_inset <- renderPlot({
        p <- ggplot(data = data.frame(x = c(-0.25, 4.25, 4.25, -0.25), y = c(-0.25, -0.25, 7.25, 7.25)), mapping = aes_string("x", "y")) +
            geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = c(0.5, 0.5, 3.5, 3.5)), fill = styling$h_court_colour) +
            geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = 3 + c(0.5, 0.5, 3.5, 3.5)), fill = styling$v_court_colour) +
            ggcourt(labels = NULL, show_zones = FALSE, show_zone_lines = TRUE, court_colour = "indoor")
        ridx <- rowidx()
        if (!is.null(ridx)) {
            this_pn <- rdata$dvw$plays$player_number[ridx] ## player in the selected row
            htrot <- tibble(player_id = as.character(rdata$dvw$plays[ridx, paste0("home_player_id", 1:6)]), team_id = rdata$dvw$plays$home_team_id[ridx])
            htrot <- dplyr::left_join(htrot, rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], by = "player_id")
            vtrot <- tibble(player_id = as.character(rdata$dvw$plays[ridx, paste0("visiting_player_id", 1:6)]), team_id = rdata$dvw$plays$visiting_team_id[ridx])
            vtrot <- dplyr::left_join(vtrot, rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], by = "player_id")
            plxy <- cbind(dv_xy(1:6, end = "lower"), htrot)
            plxy$court_num <- unlist(rdata$dvw$plays[ridx, paste0("home_p", 1:6)]) ## the on-court player numbers in the play-by-play data
            ## player names and circles
            ## home team
            p <- p + geom_polygon(data = court_circle(cz = 1:6, end = "lower"), aes_string(group = "id"), fill = styling$h_court_colour, colour = styling$h_court_highlight)
            ## highlighted player
            if (rdata$dvw$plays$team[ridx] %eq% rdata$dvw$plays$home_team[ridx] && sum(this_pn %eq% plxy$court_num) == 1) {
                p <- p + geom_polygon(data = court_circle(cz = which(this_pn %eq% plxy$court_num), end = "lower"), fill = "yellow", colour = "black")
            }
            p <- p + geom_text(data = plxy, aes_string("x", "y", label = "court_num"), size = 6, fontface = "bold", vjust = 0) +
                geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
            ## visiting team
            plxy <- cbind(dv_xy(1:6, end = "upper"), vtrot)
            plxy$court_num <- unlist(rdata$dvw$plays[ridx, paste0("visiting_p", 1:6)]) ## the on-court player numbers in the play-by-play data
            p <- p + geom_polygon(data = court_circle(cz = 1:6, end = "upper"), aes_string(group = "id"), fill = styling$v_court_colour, colour = styling$v_court_highlight)
            if (rdata$dvw$plays$team[ridx] %eq% rdata$dvw$plays$visiting_team[ridx] && sum(this_pn %eq% plxy$court_num) == 1) {
                p <- p + geom_polygon(data = court_circle(cz = which(this_pn %eq% plxy$court_num), end = "upper"), fill = "yellow", colour = "black")
            }
            p <- p + geom_text(data = plxy, aes_string("x", "y", label = "court_num"), size = 6, fontface = "bold", vjust = 0) +
                geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
            if (!is.na(rdata$dvw$plays$start_coordinate_x[ridx]) & !is.na(rdata$dvw$plays$end_coordinate_x[ridx]) && ball_coords()) {
                thisxy <- data.frame(x = as.numeric(rdata$dvw$plays[ridx, c("start_coordinate_x", "mid_coordinate_x", "end_coordinate_x")]),
                                     y = as.numeric(rdata$dvw$plays[ridx, c("start_coordinate_y", "mid_coordinate_y", "end_coordinate_y")]))
                p <- p + geom_point(data = thisxy[1, ], shape = 16, col = "green", size = 5) +
                    geom_point(data = thisxy[3, ], shape = 16, col = "red", size = 5) +
                    geom_path(data = na.omit(thisxy), arrow = arrow(length = unit(0.05, "npc"), ends = "last"))
            }
            if (nrow(click_points$queue) > 0) {
                p <- p + geom_point(data = click_points$queue, shape = 16) +
                    geom_path(data = click_points$queue, linetype = "dashed", colour = "black", arrow = arrow(length = unit(0.05, "npc"), ends = "last"))
            }
        }
        if (court_inset_home_team_end() != "lower") p <- p + scale_x_reverse() + scale_y_reverse()
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
    return(list(rt = rotate_teams, ball_coords_checkbox = ball_coords, accept_ball_coords = accept_ball_coords, click_points = click_points, add_to_click_queue = add_to_click_queue, clear_click_queue = clear_click_queue))
}

