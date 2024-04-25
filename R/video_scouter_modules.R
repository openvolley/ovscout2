## TODO: special_role column has been hidden, todo add a column of checkbox inputs allowing the captain to be specified?
mod_teamslists_ui <- function(id) {
    ns <- NS(id)
    tagList(tags$head(tags$style("#hroster {padding-left: 0px; padding-right: 0px; background-color: #bfefff; padding: 12px; border: 3px solid white; border-radius:6px;}
                                 #vroster {padding-left: 0px; padding-right: 0px; background-color: #bcee68; padding: 12px; border: 3px solid white; border-radius:6px;}")),
            tags$div(style = "border-radius: 15px; padding: 4px",
                     fluidRow(column(6, id = "hroster", uiOutput(ns("htroster"))),
                              column(6, id = "vroster", uiOutput(ns("vtroster"))))
                     )
            )
}

mod_teamslists <- function(input, output, session, rdata, two_cols = TRUE) {
    output$htroster <- renderUI({
        re <- names2roster(rdata$dvw$meta$players_h, join = two_cols)
        htn <- rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "*"]
        if (two_cols) {
            re1 <- re[seq_len(ceiling(length(re) / 2))]
            tags$div(tags$strong(paste("(*)", htn)), fluidRow(do.call(column, c(list(6), lapply(re1, function(z) tagList(tags$span(z), tags$br())))),
                                                do.call(column, c(list(6), lapply(setdiff(re, re1), function(z) tagList(tags$span(z), tags$br()))))))
        } else {
            tags$div(tags$strong(paste("(*)", htn)), tags$hr(), do.call(tags$table, c(list(style="width:100%;"), lapply(seq_len(nrow(re)), function(z) tagList(tags$tr(tags$td(re[z, 1]), tags$td(re[z, 2])))))))
        }
    })
    output$vtroster <- renderUI({
        re <- names2roster(rdata$dvw$meta$players_v, join = two_cols)
        vtn <- rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "a"]
        if (two_cols) {
            re1 <- re[seq_len(ceiling(length(re) / 2))]
            tags$div(tags$strong(paste("(a)", vtn)), fluidRow(do.call(column, c(list(6), lapply(re1, function(z) tagList(tags$span(z), tags$br())))),
                                                do.call(column, c(list(6), lapply(setdiff(re, re1), function(z) tagList(tags$span(z), tags$br()))))))
        } else {
            tags$div(tags$strong(paste("(a)", vtn)), tags$hr(), do.call(tags$table, c(list(style="width:100%;"), lapply(seq_len(nrow(re)), function(z) tagList(tags$tr(tags$td(re[z, 1]), tags$td(re[z, 2])))))))
        }
    })
}

mod_courtrot2_ui <- function(id, styling) {
    ns <- NS(id)
    tagList(tags$head(tags$style(paste0("#", ns("court_inset"), " img {max-width:100%; max-height:100%; object-fit:contain;} .crhbut { background-color:", styling$h_court_colour, "; margin-top:2px; } .crhbut:hover, .crhbut:active { background-color:", styling$h_court_light_colour, "; } .crvbut { background-color:", styling$v_court_colour, "; margin-top:2px; } .crvbut:hover, .crvbut:active { background-color:", styling$v_court_light_colour, "; }"))),
            tags$div(style = "border-radius: 4px; padding: 4px;",
                     fluidRow(
                     column(2,
                            actionButton(ns("rotate_home"), tags$span("Home", tags$br(), icon("redo")), class = "crhbut"),
                            actionButton(ns("p1pt_home"), tags$span("Home", tags$br(), icon("plus")), class = "crhbut"),
                            actionButton(ns("m1pt_home"), tags$span("Home", tags$br(), icon("minus")), class = "crhbut"),
                            actionButton(ns("timeout_home"), tags$span("Home", tags$br(), icon("t")), class = "crhbut"),
                            actionButton(ns("substitution_home"), tags$span("Home", tags$br(), icon("right-left")), class = "crhbut")
                         ),
                     column(8, plotOutputWithAttribs(ns("court_inset"), click = ns("plot_click"), style = "height:46vh; width:36vh;")),
                     column(2,
                            actionButton(ns("rotate_visiting"), tags$span("Visiting", tags$br(), icon("redo")), class = "crvbut"),
                            actionButton(ns("p1pt_visiting"), tags$span("Visiting", tags$br(), icon("plus")), class = "crvbut"),
                            actionButton(ns("m1pt_visiting"), tags$span("Visiting", tags$br(), icon("minus")), class = "crvbut"),
                            actionButton(ns("timeout_visiting"), tags$span("Visiting", tags$br(), icon("t")), class = "crvbut"),
                            actionButton(ns("substitution_visiting"), tags$span("Visiting", tags$br(), icon("right-left")), class = "crvbut")
                     ),
                     ),
                     fluidRow(
                         column(2, offset = 2, actionButton(ns("switch_serving"), HTML("Switch<br />serving team"))),
                         column(1, offset = 4, actionButton(ns("court_inset_swap"), label = tags$span(style = "font-size:150%;", "\u21f5"), class = "iconbut")) ## flip court diagram
                     ),
                     ))
}

mod_courtrot2 <- function(input, output, session, rdata, game_state, rally_codes, rally_state, current_video_src, styling, with_ball_path = function() FALSE) {
    ns <- session$ns
    beach <- is_beach(isolate(rdata$dvw))
    pseq <- if (beach) 1:2 else 1:6

    ## can't switch serving team etc once the rally has started
    observe({
        if (!isTRUE(game_state$rally_started)) {
            for (el in c("court_inset_swap", "switch_serving", "rotate_home", "p1pt_home", "m1pt_home", "timeout_home", "substitution_home", "rotate_visiting", "p1pt_visiting", "m1pt_visiting", "timeout_visiting", "substitution_visiting")) js_show2(ns(el))
        } else {
            for (el in c("court_inset_swap", "switch_serving", "rotate_home", "p1pt_home", "m1pt_home", "timeout_home", "substitution_home", "rotate_visiting", "p1pt_visiting", "m1pt_visiting", "timeout_visiting", "substitution_visiting")) js_hide2(ns(el))
        }
    })

    observeEvent(input$switch_serving, {
        game_state$serving <- other(game_state$serving)
        game_state$current_team <- game_state$serving
    })

    ## do we need to flip the court plot? (noting that we flip it when the video changes, too)
    need_to_flip <- function(vsrc, home_team_end) {
        (vsrc < 2 && home_team_end != "lower") || (vsrc > 1 && home_team_end == "lower")
    }

    clickout <- reactiveVal(list(x = NA_real_, y = NA_real_))
    observeEvent(input$plot_click, {
        req(input$plot_click)
        ## input$plot_click gives the click location, but we want to flip this if the court direction has been reversed
        out <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
        if (need_to_flip(current_video_src(), game_state$home_team_end)) out <- dv_flip_xy(out)
        clickout(out)
    })

    ## the court plot itself
    ## go to some effort to reduce redraws of the court plot
    plot_data <- reactive({
        ## watch these
        blah <- list(game_state$home_team_end, game_state$serving, game_state$home_score_start_of_point, game_state$visiting_score_start_of_point,
                     game_state$home_p1, game_state$home_p2, game_state$visiting_p1, game_state$visiting_p2)
        if (!beach) blah <- c(blah, list(game_state$home_p3, game_state$home_p4, game_state$home_p5, game_state$home_p6, game_state$visiting_p3, game_state$visiting_p4, game_state$visiting_p5, game_state$visiting_p6, game_state$home_setter_position, game_state$visiting_setter_position, game_state$ht_lib1, game_state$ht_lib2, game_state$vt_lib1, game_state$vt_lib2))
        gs <- isolate(reactiveValuesToList(game_state))

        htrot <- tibble(number = get_players(gs, team = "*", dvw = rdata$dvw))
        htrot <- dplyr::left_join(htrot, dplyr::filter(rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
        htrot$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", htrot$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
        vtrot <- tibble(number = get_players(gs, team = "a", dvw = rdata$dvw))
        vtrot <- dplyr::left_join(vtrot, dplyr::filter(rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
        vtrot$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", vtrot$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
        ht_setter <- get_setter(gs, team = "*")
        ht_libxy <- vt_libxy <- NULL
        libs <- get_liberos(gs, team = "*", dvw = rdata$dvw)
        if (length(libs)) {
            ht_libxy <- tibble(number = libs) %>%
                dplyr::left_join(dplyr::filter(rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
            ht_libxy$pos <- c(5, 7)[seq_len(nrow(ht_libxy))]
            ht_libxy$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", ht_libxy$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
            ht_libxy <- cbind(dv_xy(ht_libxy$pos, end = "lower"), ht_libxy) %>% mutate(x = case_when(need_to_flip(current_video_src(), gs$home_team_end) ~ .data$x - 1,
                                                                                                     TRUE ~ .data$x + 3))
        }
        vt_setter <- get_setter(gs, team = "a")
        libs <- get_liberos(gs, team = "a", dvw = rdata$dvw)
        if (length(libs)) {
            vt_libxy <- tibble(number = libs) %>%
                dplyr::left_join(dplyr::filter(rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
            vt_libxy$pos <- c(1, 9)[seq_len(nrow(vt_libxy))]
            vt_libxy$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", vt_libxy$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
            vt_libxy <- cbind(dv_xy(vt_libxy$pos, end = "upper"), vt_libxy) %>% mutate(x = case_when(need_to_flip(current_video_src(), gs$home_team_end) ~ .data$x - 1,
                                                                                                     TRUE ~ .data$x + 3))
        }
        list(htrot = htrot, vtrot = vtrot, ht_setter = ht_setter, vt_setter = vt_setter, ht_libxy = ht_libxy, vt_libxy = vt_libxy, serving = gs$serving, home_score_start_of_point = gs$home_score_start_of_point, visiting_score_start_of_point = gs$visiting_score_start_of_point)
    })
    ## keep track of the digest of the plot_data() object so that we can trigger downstream actions when it has actually changed
    plot_data_digest <- reactiveVal("")
    last_plot_data_digest <- "xx"
    observe({
        dig <- digest::digest(plot_data())
        plot_data_digest(dig)
        ##cat("courtrot2 plot data digest:", isolate(plot_data_digest()), "\n")
        if (dig != last_plot_data_digest) update_base_plot()
        last_plot_data_digest <<- dig
    })

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
    ss_digest <- reactiveVal("")
    last_ss_digest <- "xx"
    observe({
        dig <- digest::digest(list(ss(), game_state$home_team_end))
        ss_digest(dig)
        ##cat("courtrot2 ss digest:", isolate(ss_digest()), "\n")
        if (dig != last_ss_digest) update_base_plot()
        last_ss_digest <<- dig
    })

    ## generate the ggplot object of the court with players, this doesn't change within a rally
    base_plot <- reactiveVal(NULL)
    update_base_plot <- function() {
        p <- ggplot(data = data.frame(x = c(-0.25, 4.25, 4.25, -0.25), y = c(-0.25, -0.25, 7.25, 7.25)), mapping = aes(.data$x, .data$y)) +
            geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = c(0.5, 0.5, 3.5, 3.5)), fill = styling$h_court_colour, na.rm = TRUE) +
            geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = 3 + c(0.5, 0.5, 3.5, 3.5)), fill = styling$v_court_colour, na.rm = TRUE) +
            ggcourt(labels = NULL, show_zones = FALSE, show_zone_lines = TRUE, court_colour = "indoor")
        px <- isolate(plot_data()) ## don't redraw on every invalidation of plot_data(), because the actual data might not have changed
        ssi <- isolate(ss())
        blah <- list(plot_data_digest(), ss_digest()) ## trigger on these
        htrot <- px$htrot
        vtrot <- px$vtrot
        plxy <- cbind(dv_xy(pseq, end = "lower"), htrot)
        ## player names and circles
        ## home team
        p <- p + geom_polygon(data = court_circle(cz = pseq, end = "lower"), aes(group = .data$id), fill = styling$h_court_colour, colour = styling$h_court_highlight_colour, na.rm = TRUE)
        if (!beach) {
            ## setter
            ht_setter <- px$ht_setter
            if (!is.null(ht_setter) && sum(ht_setter %eq% plxy$number) == 1) {
                p <- p + geom_polygon(data = court_circle(cz = which(ht_setter %eq% plxy$number), end = "lower"), fill = styling$h_court_highlight_colour, colour = "black", na.rm = TRUE)
            }
            ## liberos
            if (!is.null(px$ht_libxy)) {
                p <- p + geom_polygon(data = court_circle(px$ht_libxy[, c("x", "y")]), aes(group = .data$id), fill = styling$libero_colour, colour = "black", na.rm = TRUE) +
                    geom_text(data = px$ht_libxy, aes(.data$x, .data$y, label = .data$number), size = 6, fontface = "bold", vjust = 0, na.rm = TRUE) +
                    geom_text(data = px$ht_libxy, aes(.data$x, .data$y + if (need_to_flip(current_video_src(), game_state$home_team_end)) 0.07 else - 0.07, label = .data$lastname_wrapped), size = 3, vjust = 1, lineheight = 1, na.rm = TRUE)
            }
        }
        p <- p + geom_text(data = plxy, aes(.data$x, .data$y, label = .data$number), size = 6, fontface = "bold", vjust = 0, na.rm = TRUE) +
            geom_text(data = plxy, aes(.data$x, .data$y + if (need_to_flip(current_video_src(), game_state$home_team_end)) 0.07 else - 0.07, label = .data$lastname_wrapped), size = 3, vjust = 1, lineheight = 1, na.rm = TRUE)
        p <- p + ggplot2::annotate(geom = "text", x = 2, y = 2, label = datavolley::home_team(rdata$dvw), size = 5, hjust = 0.5)
        ## visiting team
        plxy <- cbind(dv_xy(pseq, end = "upper"), vtrot)
        p <- p + geom_polygon(data = court_circle(cz = pseq, end = "upper"), aes(group = .data$id), fill = styling$v_court_colour, colour = styling$v_court_highlight_colour, na.rm = TRUE)
        if (!beach) {
            ## setter
            vt_setter <- px$vt_setter
            if (!is.null(vt_setter) && sum(vt_setter %eq% plxy$number) == 1) {
                p <- p + geom_polygon(data = court_circle(cz = which(vt_setter %eq% plxy$number), end = "upper"), fill = styling$v_court_highlight_colour, colour = "black", na.rm = TRUE)
            }
            ## liberos
            if (!is.null(px$vt_libxy)) {
                p <- p + geom_polygon(data = court_circle(px$vt_libxy[, c("x", "y")]), aes(group = .data$id), fill = styling$libero_colour, colour = "black", na.rm = TRUE) +
                    geom_text(data = px$vt_libxy, aes(.data$x, .data$y, label = .data$number), size = 6, fontface = "bold", vjust = 0, na.rm = TRUE) +
                    geom_text(data = px$vt_libxy, aes(.data$x, .data$y + if (need_to_flip(current_video_src(), game_state$home_team_end)) 0.07 else - 0.07, label = .data$lastname_wrapped), size = 3, vjust = 1, lineheight = 1, na.rm = TRUE)
            }
        }
        p <- p + geom_text(data = plxy, aes(.data$x, .data$y, label = .data$number), size = 6, fontface = "bold", vjust = 0, na.rm = TRUE) +
            geom_text(data = plxy, aes(.data$x, .data$y + if (need_to_flip(current_video_src(), game_state$home_team_end)) 0.07 else - 0.07, label = .data$lastname_wrapped), size = 3, vjust = 1, lineheight = 1, na.rm = TRUE)
        p <- p + ggplot2::annotate(geom = "text", x = 2, y = 5, label = datavolley::visiting_team(rdata$dvw), size = 5, hjust = 0.5)

        ## add the serving team indicator
        temp <- court_circle(cz = 1, r = 0.2, end = "upper")
        temp$y <- temp$y + 0.75 ## shift to behind baseline
        p <- p + geom_polygon(data = temp, fill = if (px$serving %eq% "a") "white" else NA, colour = "black", na.rm = TRUE)
        temp <- court_circle(cz = 1, r = 0.2, end = "lower")
        temp$y <- temp$y - 0.75 ## shift to behind baseline
        p <- p + geom_polygon(data = temp, fill = if (px$serving %eq% "*") "white" else NA, colour = "black", na.rm = TRUE)

        ## add the point and set scores on the right side
        if (!is.null(px$home_score_start_of_point) && !is.null(px$visiting_score_start_of_point)) {
            ## point scores
            scxy <- tibble(x = c(-0.5, -0.5), y = c(3.15, 3.85), score = c(px$home_score_start_of_point, px$visiting_score_start_of_point))
            if (!need_to_flip(current_video_src(), game_state$home_team_end)) scxy$x <- scxy$x + 5
            p <- p + ggplot2::annotate(x = scxy$x, y = scxy$y, label = scxy$score, geom = "label", size = 9, fontface = "bold", vjust = 0.5, na.rm = TRUE)
            if (length(ssi) == 2 && !any(is.na(ssi))) {
                ## set scores
                ssxy <- tibble(set_score = ssi, x = c(-0.5, -0.5), y = c(2.6, 4.4))
                if (!need_to_flip(current_video_src(), game_state$home_team_end)) ssxy$x <- ssxy$x + 5
                p <- p + ggplot2::annotate(x = ssxy$x, y = ssxy$y, label = ssxy$set_score, geom = "label", size = 6, fontface = "bold", vjust = 0.5, na.rm = TRUE)
            }
        }
        if (need_to_flip(current_video_src(), game_state$home_team_end)) {
            p <- p + scale_x_reverse(limits = c(NA_real_, -1), expand = c(0.01, 0.01)) + scale_y_reverse(expand = c(0.01, 0.01))
        } else {
            p <- p + scale_x_continuous(limits = c(NA_real_, 5), expand = c(0.01, 0.01)) + scale_y_continuous(expand = c(0.01, 0.01))
        }
      base_plot(p)
    }

    ## keep track of the digest of the rally_codes() object so that we can trigger the plot update when it has actually changed AND only if we are showing ball coords
    rally_codes_digest <- reactiveVal("")
    observe({
        if (with_ball_path()) rally_codes_digest(digest::digest(rally_codes()))
    })
    output$court_inset <- renderPlot({
        p <- base_plot()
        blah <- rally_codes_digest()
        isolate({
            if (nrow(rally_codes()) > 0 && with_ball_path()) {
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
                    ## all coords are recorded relative to video1 orientation, so we don't care which video is showing
                    if (game_state$home_team_end != "lower") segxy <- dv_flip_xy(segxy)
                    p <- p + geom_path(data = segxy, na.rm = TRUE)
                }
            }
        })
        p + theme(plot.margin = rep(unit(0, "null"), 4))
    }, height = 950, width = 600, res = 180)

    observeEvent(input$court_inset_swap, game_state$home_team_end <- other_end(game_state$home_team_end))

    return(list(click = clickout,
                rotate_home = reactive(input$rotate_home), p1pt_home = reactive(input$p1pt_home), m1pt_home = reactive(input$m1pt_home),
                timeout_home = reactive(input$timeout_home), substitution_home = reactive(input$substitution_home),
                rotate_visiting = reactive(input$rotate_visiting), p1pt_visiting = reactive(input$p1pt_visiting), m1pt_visiting = reactive(input$m1pt_visiting),
                timeout_visiting = reactive(input$timeout_visiting), substitution_visiting = reactive(input$substitution_visiting)))
}

## base plotting instead of ggplot
mod_courtrot2_base <- function(input, output, session, rdata, game_state, rally_codes, rally_state, current_video_src, styling, with_ball_path = function() FALSE, current_plays_row = function() NULL) {
    ns <- session$ns
    beach <- is_beach(isolate(rdata$dvw))
    pseq <- if (beach) 1:2 else 1:6

    ## can't switch serving team etc once the rally has started
    observe({
        if (!isTRUE(game_state$rally_started)) {
            for (el in c("court_inset_swap", "switch_serving", "rotate_home", "p1pt_home", "m1pt_home", "timeout_home", "substitution_home", "rotate_visiting", "p1pt_visiting", "m1pt_visiting", "timeout_visiting", "substitution_visiting")) js_show2(ns(el))
        } else {
            for (el in c("court_inset_swap", "switch_serving", "rotate_home", "p1pt_home", "m1pt_home", "timeout_home", "substitution_home", "rotate_visiting", "p1pt_visiting", "m1pt_visiting", "timeout_visiting", "substitution_visiting")) js_hide2(ns(el))
        }
    })

    observeEvent(input$switch_serving, {
        game_state$serving <- other(game_state$serving)
        game_state$current_team <- game_state$serving
    })

    ## do we need to flip the court plot? (noting that we flip it when the video changes, too)
    need_to_flip <- function(vsrc, home_team_end) {
        (vsrc < 2 && home_team_end != "lower") || (vsrc > 1 && home_team_end == "lower")
    }

    clickout <- reactiveVal(list(x = NA_real_, y = NA_real_))
    observeEvent(input$plot_click, {
        req(input$plot_click)
        ## input$plot_click gives the click location, but we want to flip this if the court direction has been reversed
        out <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
        if (need_to_flip(current_video_src(), game_state$home_team_end)) out <- dv_flip_xy(out)
        clickout(out)
    })

    ## the court plot itself
    ## go to some effort to reduce redraws of the court plot
    plot_data <- reactive({
        ## watch these
        blah <- list(game_state$home_team_end, game_state$serving, game_state$home_score_start_of_point, game_state$visiting_score_start_of_point,
                     game_state$home_p1, game_state$home_p2, game_state$visiting_p1, game_state$visiting_p2)
        if (!beach) blah <- c(blah, list(game_state$home_p3, game_state$home_p4, game_state$home_p5, game_state$home_p6, game_state$visiting_p3, game_state$visiting_p4, game_state$visiting_p5, game_state$visiting_p6, game_state$home_setter_position, game_state$visiting_setter_position, game_state$ht_lib1, game_state$ht_lib2, game_state$vt_lib1, game_state$vt_lib2))
        gs <- isolate(reactiveValuesToList(game_state))

        htrot <- tibble(number = get_players(gs, team = "*", dvw = rdata$dvw))
        htrot <- dplyr::left_join(htrot, dplyr::filter(rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
        htrot$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", htrot$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
        vtrot <- tibble(number = get_players(gs, team = "a", dvw = rdata$dvw))
        vtrot <- dplyr::left_join(vtrot, dplyr::filter(rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
        vtrot$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", vtrot$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
        ht_setter <- get_setter(gs, team = "*")
        ht_libxy <- vt_libxy <- NULL
        libs <- get_liberos(gs, team = "*", dvw = rdata$dvw)
        if (length(libs)) {
            ht_libxy <- tibble(number = libs) %>%
                dplyr::left_join(dplyr::filter(rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
            ht_libxy$pos <- c(5, 7)[seq_len(nrow(ht_libxy))]
            ht_libxy$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", ht_libxy$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
            ht_libxy <- cbind(dv_xy(ht_libxy$pos, end = "lower"), ht_libxy) %>% mutate(x = case_when(need_to_flip(current_video_src(), gs$home_team_end) ~ .data$x - 1,
                                                                                                     TRUE ~ .data$x + 3))
        }
        vt_setter <- get_setter(gs, team = "a")
        libs <- get_liberos(gs, team = "a", dvw = rdata$dvw)
        if (length(libs)) {
            vt_libxy <- tibble(number = libs) %>%
                dplyr::left_join(dplyr::filter(rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
            vt_libxy$pos <- c(1, 9)[seq_len(nrow(vt_libxy))]
            vt_libxy$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", vt_libxy$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
            vt_libxy <- cbind(dv_xy(vt_libxy$pos, end = "upper"), vt_libxy) %>% mutate(x = case_when(need_to_flip(current_video_src(), gs$home_team_end) ~ .data$x - 1,
                                                                                                     TRUE ~ .data$x + 3))
        }
        list(htrot = htrot, vtrot = vtrot, ht_setter = ht_setter, vt_setter = vt_setter, ht_libxy = ht_libxy, vt_libxy = vt_libxy, serving = gs$serving, home_score_start_of_point = gs$home_score_start_of_point, visiting_score_start_of_point = gs$visiting_score_start_of_point)
    })
    ## keep track of the digest of the plot_data() object so that we can trigger downstream actions when it has actually changed
    plot_data_digest <- reactiveVal("")
    last_plot_data_digest <- "xx"
    observe({
        dig <- digest::digest(plot_data())
        plot_data_digest(dig)
        if (dig != last_plot_data_digest) update_base_plot()
        last_plot_data_digest <<- dig
    })

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
    ss_digest <- reactiveVal("")
    last_ss_digest <- "xx"
    observe({
        dig <- digest::digest(list(ss(), game_state$home_team_end))
        ss_digest(dig)
        if (dig != last_ss_digest) update_base_plot()
        last_ss_digest <<- dig
    })

    ## generate the plot object of the court with players, this doesn't change within a rally
    base_plot <- reactiveVal(0L)
    update_base_plot <- function() { base_plot(isolate(base_plot() + 1L)) }
    do_base_plot <- function() {
        cexsc <- 0.2
        flp <- need_to_flip(current_video_src(), game_state$home_team_end)
        par(bg = "#26A9BD", mai = c(0, 0, 0, 0))
        plot(0, 0, type = "n", xlim = c(0.25, 5), ylim = c(0, 7), axes = FALSE, xlab = "", ylab = "", asp = 1, bg = "#26A9BD")
        ##datavolley::dv_plot_new(x = c(-0.25, 5.5), y = c(-0.25, 7.25), court = "full", margins = c(0, 0, 0, 0))
        rect(0.5, 0.5, 3.5, 6.5, col = "#D98875")
        datavolley::dv_court(labels = NULL, show_zones = FALSE, show_zone_lines = TRUE, grid_colour = "white")

        htend <- if (flp) "upper" else "lower"
        vtend <- if (flp) "lower" else "upper"

        px <- isolate(plot_data()) ## don't redraw on every invalidation of plot_data(), because the actual data might not have changed
        if (!is.null(px$ht_libxy)) px$ht_libxy$x <- 4
        if (!is.null(px$vt_libxy)) px$vt_libxy$x <- 4
        if (flp) {
            if (!is.null(px$ht_libxy)) px$ht_libxy$y <- 7 - px$ht_libxy$y
            if (!is.null(px$vt_libxy)) px$vt_libxy$y <- 7 - px$vt_libxy$y
        }
        ssi <- isolate(ss())
        blah <- list(plot_data_digest(), ss_digest()) ## trigger on these
        htrot <- px$htrot
        vtrot <- px$vtrot
        plxy <- cbind(dv_xy(pseq, end = htend), htrot)
        ## player names and circles
        ## home team
        this <- court_circle(cz = pseq, end = htend)
        for (thisid in pseq) {
            idx <- this$id == thisid
            polygon(this$x[idx], this$y[idx], col = styling$h_court_colour, border = styling$h_court_highlight_colour)
        }
        if (!beach) {
            ## setter
            ht_setter <- px$ht_setter
            if (!is.null(ht_setter) && sum(ht_setter %eq% plxy$number) == 1) {
                this <- court_circle(cz = which(ht_setter %eq% plxy$number), end = htend)
                polygon(this$x, this$y, col = styling$h_court_highlight_colour, border = "black")
            }
            ## liberos
            if (!is.null(px$ht_libxy)) {
                this <- court_circle(px$ht_libxy[, c("x", "y")])
                for (thisid in unique(this$id)) {
                    idx <- this$id == thisid
                    polygon(this$x[idx], this$y[idx], col = styling$libero_colour, border = "black")
                    text(px$ht_libxy$x, px$ht_libxy$y, labels = px$ht_libxy$number, cex = 6 * cexsc, font = 2, adj = c(0.5, 0))
                    text(px$ht_libxy$x, px$ht_libxy$y + if (flp) 0.07 else - 0.07, labels = px$ht_libxy$lastname_wrapped, cex = 3 * cexsc, adj = c(0.5, 1 + flp)) ## lineheight = 1, na.rm = TRUE)
                }
            }
        }
        text(plxy$x, plxy$y, labels = plxy$number, cex = 6 * cexsc, font = 2, adj = c(0.5, 0))
        text(plxy$x, plxy$y + if (flp) 0.07 else - 0.07, labels = plxy$lastname_wrapped, cex = 3 * cexsc, adj = c(0.5, 1 + flp))## lineheight = 1
        text(x = 2, y = 2 + flp * 3, labels = datavolley::home_team(rdata$dvw), cex = 5 * cexsc)

        ## visiting team
        plxy <- cbind(dv_xy(pseq, end = vtend), vtrot)
        this <- court_circle(cz = pseq, end = vtend)
        for (thisid in pseq) {
            idx <- this$id == thisid
            polygon(this$x[idx], this$y[idx], col = styling$v_court_colour, border = styling$v_court_highlight_colour)
        }
        if (!beach) {
            ## setter
            vt_setter <- px$vt_setter
            if (!is.null(vt_setter) && sum(vt_setter %eq% plxy$number) == 1) {
                this <- court_circle(cz = which(vt_setter %eq% plxy$number), end = vtend)
                polygon(this$x, this$y, col = styling$v_court_highlight_colour, border = "black")
            }
            ## liberos
            if (!is.null(px$vt_libxy)) {
                this <- court_circle(px$vt_libxy[, c("x", "y")])
                for (thisid in unique(this$id)) {
                    idx <- this$id == thisid
                    polygon(this$x[idx], this$y[idx], col = styling$libero_colour, border = "black")
                    text(px$vt_libxy$x, px$vt_libxy$y, labels = px$vt_libxy$number, cex = 6 * cexsc, font = 2, adj = c(0.5, 0))
                    text(px$vt_libxy$x, px$vt_libxy$y + if (flp) 0.07 else - 0.07, labels = px$vt_libxy$lastname_wrapped, cex = 3 * cexsc, adj = c(0.5, 1 + flp)) ## lineheight = 1
                }
            }
        }
        text(plxy$x, plxy$y, labels = plxy$number, cex = 6 * cexsc, font = 2, adj = c(0.5, 0))
        text(plxy$x, plxy$y + if (flp) 0.07 else - 0.07, labels = plxy$lastname_wrapped, cex = 3 * cexsc, adj = c(0.5, 1 + flp)) ## lineheight = 1
        text(x = 2, y = 5 - flp * 3, labels = datavolley::visiting_team(rdata$dvw), cex = 5 * cexsc)

        ## add the serving team indicator
        temp <- court_circle(cz = 1, r = 0.2, end = vtend)
        temp$y <- temp$y + if (flp) -0.75 else 0.75 ## shift to behind baseline
        polygon(temp$x, temp$y, col = if (px$serving %eq% "a") "white" else NA, border = "black")
        temp <- court_circle(cz = 1, r = 0.2, end = htend)
        temp$y <- temp$y + if (flp) 0.75 else - 0.75 ## shift to behind baseline
        polygon(temp$x, temp$y, col = if (px$serving %eq% "*") "white" else NA, border = "black")

        ## add the point and set scores on the right side
        if (!is.null(px$home_score_start_of_point) && !is.null(px$visiting_score_start_of_point)) {
            ## point scores
            rect(4.1, 2.3, 4.9, 4.7, col = "white")
            text(x = c(4.5, 4.5), y = if (flp) rev(c(3.15, 3.85)) else c(3.15, 3.85),
                 labels = c(px$home_score_start_of_point, px$visiting_score_start_of_point), cex = 9 * cexsc, font = 2)
            if (length(ssi) == 2 && !any(is.na(ssi))) {
                ## set scores
                text(x = c(4.5, 4.5), y = if (flp) rev(c(2.6, 4.4)) else c(2.6, 4.4), labels = ssi, cex = 6 * cexsc, font = 2)
            }
        }
    }

    ## keep track of the digest of the rally_codes() object so that we can trigger the plot update when it has actually changed AND only if we are showing ball coords
    rally_codes_digest <- reactiveVal("")
    observe({
        if (with_ball_path()) rally_codes_digest(digest::digest(rally_codes()))
    })
    output$court_inset <- renderPlot({
        do_base_plot()
        blah <- list(rally_codes_digest(), current_plays_row()) ## react to
        isolate({
            if (with_ball_path()) {
                temp_rally_plays2 <- if (is.null(current_plays_row()) || current_plays_row() > nrow(rdata$dvw$plays2)) {
                                         make_plays2(rally_codes(), game_state = game_state, dvw = rdata$dvw)
                                     } else {
                                         rdata$dvw$plays2
                                     }
                temp_rally_plays <- plays2_to_plays(temp_rally_plays2, dvw = rdata$dvw, evaluation_decoder = skill_evaluation_decoder()) ## this is the default evaluation decoder, but it doesn't matter here unless we start e.g. colouring things by evaluation
                ## court module is plotted flipped if necessary, and coordinates will be oriented to the actual video orientation, so should be OK to plot without flipping
                ## all coords are recorded relative to video1 orientation, so we don't care which video is showing
                if (!is.null(current_plays_row())) {
                    ## plot the selected action
                    temp_rally_plays <- temp_rally_plays[current_plays_row(), ]
                    segxy <- bind_rows(temp_rally_plays %>% dplyr::select(x = "start_coordinate_x", y = "start_coordinate_y"),
                                       temp_rally_plays %>% dplyr::select(x = "mid_coordinate_x", y = "mid_coordinate_y"),
                                       temp_rally_plays %>% dplyr::select(x = "end_coordinate_x", y = "end_coordinate_y")) %>%
                        na.omit()
                    if (nrow(segxy) > 0) {
                        if (nrow(segxy) == 2) {
                            arrows(segxy$x[1], segxy$y[1], segxy$x[2], segxy$y[2], angle = 15, length = 0.15)
                        } else if (nrow(segxy) == 3) {
                            lines(segxy$x[1], segxy$y[1], segxy$x[2], segxy$y[2])
                            arrows(segxy$x[2], segxy$y[2], segxy$x[3], segxy$y[3], angle = 15, length = 0.15)
                        }
                        points(segxy$x[1], segxy$y[1], pch = 21, bg = "white", cex = 2)
                    }
                } else if (nrow(rally_codes()) > 0) {
                    ## plot the current rally actions
                    temp_rally_plays <- mutate(temp_rally_plays, rn = dplyr::row_number())
                    segxy <- bind_rows(temp_rally_plays %>% dplyr::filter(.data$skill == "Serve") %>% dplyr::select(x = "start_coordinate_x", y = "start_coordinate_y", rn = "rn"),
                                       temp_rally_plays %>% dplyr::filter(.data$skill == "Serve") %>% dplyr::select(x = "end_coordinate_x", y = "end_coordinate_y", rn = "rn"),
                                       temp_rally_plays %>% dplyr::filter(!.data$skill %in% c("Serve", "Reception")) %>% dplyr::select(x = "start_coordinate_x", y = "start_coordinate_y", rn = "rn"),
                                       temp_rally_plays %>% dplyr::filter(!.data$skill %in% c("Serve", "Reception") & !is.na(.data$mid_coordinate_x)) %>% dplyr::select(x = "mid_coordinate_x", y = "mid_coordinate_y", rn = "rn") %>% mutate(rn = .data$rn + 0.5)) %>%
                        na.omit() %>% dplyr::arrange(.data$rn)
                    if (nrow(segxy) > 0) {
                        lines(segxy$x, segxy$y)
                    }
                }
            }
        })
    }, height = 800, width = 600, res = 180)

    observeEvent(input$court_inset_swap, game_state$home_team_end <- other_end(game_state$home_team_end))

    return(list(click = clickout,
                rotate_home = reactive(input$rotate_home), p1pt_home = reactive(input$p1pt_home), m1pt_home = reactive(input$m1pt_home),
                timeout_home = reactive(input$timeout_home), substitution_home = reactive(input$substitution_home),
                rotate_visiting = reactive(input$rotate_visiting), p1pt_visiting = reactive(input$p1pt_visiting), m1pt_visiting = reactive(input$m1pt_visiting),
                timeout_visiting = reactive(input$timeout_visiting), substitution_visiting = reactive(input$substitution_visiting)))
}

mod_match_data_edit_ui <- function(id) {
    ns <- NS(id)
    actionButton(ns("edit_match_data_button"), "Edit match data", icon = icon("volleyball-ball"), class = "leftbut")
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
        na2mt <- function(z) ifelse(is.na(z), "", z)
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
                                  column(4, shiny::selectInput(ns("match_edit_zones_or_cones"), "Zones or cones:", choices = c("C", "Z"), selected = rdata$dvw$meta$match$zones_or_cones))),
                         fluidRow(column(4, textInput(ns("more_edit_scout"), label = "Scout:", value = rdata$dvw$meta$more$scout)),
                                  column(4, textInput(ns("edit_comments1"), label = "Comments:", value = if (ncol(rdata$dvw$meta$comments) > 0) na2mt(rdata$dvw$meta$comments[[1]]) else ""),
                                         textInput(ns("edit_comments2"), label = NULL, value = if (ncol(rdata$dvw$meta$comments) > 1) na2mt(rdata$dvw$meta$comments[[2]]) else ""),
                                         textInput(ns("edit_comments3"), label = NULL, value = if (ncol(rdata$dvw$meta$comments) > 2) na2mt(rdata$dvw$meta$comments[[3]]) else ""),
                                         textInput(ns("edit_comments4"), label = NULL, value = if (ncol(rdata$dvw$meta$comments) > 3) paste(na2mt(unlist(rdata$dvw$meta$comments[4:ncol(rdata$dvw$meta$comments)])), collapse = "\n") else "")
                                         ),
                                column(4, tags$span(style = "font-size:small", "Note: changing cones/zones here will only affect the exported dvw file. ovscout2 always uses coordinates and zones/subzones internally. Changing the setting here will not convert a dvw file recorded with zones into one recorded with cones, or vice-versa. Don't change this unless you know what you are doing!")))
                     )
            ))
    })
}

mod_lineup_edit_ui <- function(id) {
    ns <- NS(id)
    actionButton(ns("edit_lineup_button"), "Edit lineups", icon = icon("arrows-alt-h"), class = "leftbut")
}

mod_lineup_edit <- function(input, output, session, rdata, game_state, editing, video_state, styling) {
    ns <- session$ns
    beach <- is_beach(isolate(rdata$dvw))
    pseq <- if (beach) 1:2 else 1:6
    observeEvent(input$edit_lineup_button, {
        editing$active <- "change starting lineup"
        ## pause video
        dojs("vidplayer.pause();") ##dojs("document.getElementById('main_video').pause();")
        video_state$paused <- TRUE
        htidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
        vtidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
        ht_setter <- get_setter(game_state, team = "*")
        if (length(ht_setter) < 1 || is.null(ht_setter) || is.na(ht_setter) || ht_setter %eq% 0L) ht_setter <- ""
        vt_setter <- get_setter(game_state, team = "a")
        if (length(vt_setter) < 1 || is.null(vt_setter) || is.na(vt_setter) || vt_setter %eq% 0L) vt_setter <- ""
        ht_libs <- get_liberos(game_state = game_state, team = "*", dvw = rdata$dvw)
        vt_libs <- get_liberos(game_state = game_state, team = "a", dvw = rdata$dvw)

        ## suggest lineups, take from (1) this set starting lineup (i.e. we're editing one we've already entered), then (2) previous set starting lineup, then (3) game_state
        ## default to game state
        ht_def_lup <- as.integer(c(if (!is.null(game_state$home_p1)) game_state$home_p1 else NA,
                                   if (!is.null(game_state$home_p2)) game_state$home_p2 else NA,
                                   if (!is.null(game_state$home_p3)) game_state$home_p3 else NA,
                                   if (!is.null(game_state$home_p4)) game_state$home_p4 else NA,
                                   if (!is.null(game_state$home_p5)) game_state$home_p5 else NA,
                                   if (!is.null(game_state$home_p6)) game_state$home_p6 else NA,
                                   if (!is.null(game_state$ht_lib1)) game_state$ht_lib1 else NA,
                                   if (!is.null(game_state$ht_lib2)) game_state$ht_lib2 else NA))
        vt_def_lup <- as.integer(c(if (!is.null(game_state$visiting_p1)) game_state$visiting_p1 else NA,
                                   if (!is.null(game_state$visiting_p2)) game_state$visiting_p2 else NA,
                                   if (!is.null(game_state$visiting_p3)) game_state$visiting_p3 else NA,
                                   if (!is.null(game_state$visiting_p4)) game_state$visiting_p4 else NA,
                                   if (!is.null(game_state$visiting_p5)) game_state$visiting_p5 else NA,
                                   if (!is.null(game_state$visiting_p6)) game_state$visiting_p6 else NA,
                                   if (!is.null(game_state$vt_lib1)) game_state$vt_lib1 else NA,
                                   if (!is.null(game_state$vt_lib2)) game_state$vt_lib2 else NA))
        if ("set_number" %in% names(rdata$dvw$plays2)) {
            temp <- rdata$dvw$plays2 %>% dplyr::filter(.data$set_number == (max(rdata$dvw$plays2$set_number, na.rm = TRUE))) %>% ## this set
                dplyr::filter(grepl(">LUp", .data$code)) %>% dplyr::slice_tail(n = 1)
            if (nrow(temp) == 1) {
                ht_def_lup <- as.integer(temp[, c(paste0("home_p", pseq), "ht_lib1", "ht_lib2")])
                ht_setter <- tryCatch(as.integer(ht_def_lup[[temp$home_setter_position]]), error = function(e) NA_integer_)
                vt_def_lup <- as.integer(temp[, c(paste0("visiting_p", pseq), "vt_lib1", "vt_lib2")])
                vt_setter <- tryCatch(as.integer(vt_def_lup[[temp$visiting_setter_position]]), error = function(e) NA_integer_)
            } else {
                temp <- rdata$dvw$plays2 %>% dplyr::filter(.data$set_number == (max(rdata$dvw$plays2$set_number, na.rm = TRUE) - 1L)) %>% ## previous set
                    dplyr::filter(grepl(">LUp", .data$code)) %>% dplyr::slice_tail(n = 1)
                if (nrow(temp) == 1) {
                    ht_def_lup <- as.integer(temp[, c(paste0("home_p", pseq), "ht_lib1", "ht_lib2")])
                    ht_setter <- tryCatch(as.integer(ht_def_lup[[temp$home_setter_position]]), error = function(e) NA_integer_)
                    vt_def_lup <- as.integer(temp[, c(paste0("visiting_p", pseq), "vt_lib1", "vt_lib2")])
                    vt_setter <- tryCatch(as.integer(vt_def_lup[[temp$visiting_setter_position]]), error = function(e) NA_integer_)
                }
            }
        }
        text_input_with_tabindex <- function (inputId, label, value = "", width = NULL, placeholder = NULL, tabindex = -1) {
            value <- shiny::restoreInput(id = inputId, default = value)
            tags$div(class = "form-group shiny-input-container", style = htmltools::css(width = htmltools::validateCssUnit(width)),
                     shiny:::shinyInputLabel(inputId, label), tags$input(id = inputId, type = "text", class = "form-control", value = value, placeholder = placeholder, tabindex = tabindex))
        }
        ## border helpers, make a kind-of-court diagram with the cell borders
        brd_l <- "border-left:2px solid black;"; brd_r <- "border-right:2px solid black;"; brd_t <- "border-top:2px solid black;"; brd_b <- "border-bottom:8px solid black;";
        showModal(
            vwModalDialog(
                title = "Edit starting line up", size = "l", footer = tags$div(uiOutput(ns("edit_lineup_commit_ui"), inline = TRUE), actionButton("edit_cancel", label = "Cancel", class = "cancel")),
                tabsetPanel(
                    tabPanel(paste0(datavolley::home_team(rdata$dvw), " (home)"),
                             tags$style(paste0("#ht_display_team {border: 2px solid ", styling$h_court_colour, ";}")),
                             DT::dataTableOutput(ns("ht_display_team")),
                             wellPanel(
                                 ## ideally we should not use tabindex > 1, but I can't see a way to generate the UI in a way that renders the positions correctly (i.e. in their proper court positions) while at the same time getting the elements in the correct order in the DOM (in which case the natural tab ordering would work, and tabindex would not be needed) - BR
                                 fluidRow(column(1, style = paste0(brd_t, brd_l), text_input_with_tabindex(ns("ht_P1"), label = "P1", value = if (!is.na(ht_def_lup[1])) ht_def_lup[1] else "", placeholder = "P1", tabindex = 1)),
                                          column(1, style = brd_t, text_input_with_tabindex(ns("ht_P6"), label = "P6", value = if (!is.na(ht_def_lup[6])) ht_def_lup[6] else "", placeholder = "P6", tabindex = 6)),
                                          column(1, style = paste0(brd_t, brd_r), text_input_with_tabindex(ns("ht_P5"), label = "P5", value = if (!is.na(ht_def_lup[5])) ht_def_lup[5] else "", placeholder = "P5", tabindex = 5)),
                                          column(1, offset = 1, text_input_with_tabindex(ns("ht_setter"), label = "Setter", value = ht_setter, placeholder = "Setter", tabindex = 7))),
                                 fluidRow(
                                     column(1, style = paste0(brd_b, brd_l), text_input_with_tabindex(ns("ht_P2"), label = "P2", value = if (!is.na(ht_def_lup[2])) ht_def_lup[2] else "", placeholder = "P2", tabindex = 2)),
                                     column(1, style = brd_b, text_input_with_tabindex(ns("ht_P3"), label = "P3", value = if (!is.na(ht_def_lup[3])) ht_def_lup[3] else "", placeholder = "P3", tabindex = 3)),
                                     column(1, style = paste0(brd_b, brd_r), text_input_with_tabindex(ns("ht_P4"), label = "P4", value = if (!is.na(ht_def_lup[4])) ht_def_lup[4] else "", placeholder = "P4", tabindex = 4)),
                                     column(1, offset = 1, text_input_with_tabindex(ns("ht_libero1"), label = "Libero 1", value = if (is.na(ht_def_lup[7]) && length(ht_libs) > 0) ht_libs[1] else if (!is.na(ht_def_lup[7]) && ht_def_lup[7] == -1) "" else if (!is.na(ht_def_lup[7])) ht_def_lup[7], placeholder = "Libero 1", tabindex = 8)),
                                     column(1, text_input_with_tabindex(ns("ht_libero2"), label = "Libero 2", value = if (is.na(ht_def_lup[8]) && length(ht_libs) > 1) ht_libs[2] else if (!is.na(ht_def_lup[8]) && ht_def_lup[8] == -1) "" else if (!is.na(ht_def_lup[8])) ht_def_lup[8], placeholder = "Libero 2", tabindex = 9)),
                                     column(1, offset = 1, tags$br(), actionButton(ns("rot_home_fwd"), tags$span("Rotate", icon("redo"))), actionButton(ns("rot_home_back"), tags$span("Rotate back", icon("undo"))))),
                                 style = paste0("background: ", styling$h_court_colour)
                             )),
                    tabPanel(paste0(datavolley::visiting_team(rdata$dvw), " (visiting)"), id = ns("vlpan"),
                             tags$style(paste0("#vt_display_team {border: 2px solid ", styling$v_court_colour, ";}")),
                             DT::dataTableOutput(ns("vt_display_team")),
                             wellPanel(
                                 fluidRow(column(1, style = paste0(brd_t, brd_l), text_input_with_tabindex(ns("vt_P1"), label = "P1", value = if (!is.na(vt_def_lup[1])) vt_def_lup[1] else "", placeholder = "P1", tabindex = 10)),
                                          column(1, style = brd_t, text_input_with_tabindex(ns("vt_P6"), label = "P6", value = if (!is.na(vt_def_lup[6])) vt_def_lup[6] else "", placeholder = "P6", tabindex = 15)),
                                          column(1, style = paste0(brd_t, brd_r), text_input_with_tabindex(ns("vt_P5"), label = "P5", value = if (!is.na(vt_def_lup[5])) vt_def_lup[5] else "", placeholder = "P5", tabindex = 14)),
                                          column(1, offset = 1, text_input_with_tabindex(ns("vt_setter"), label = "Setter", value = vt_setter, placeholder = "Setter", tabindex = 16))),
                                 fluidRow(
                                     column(1, style = paste0(brd_b, brd_l), text_input_with_tabindex(ns("vt_P2"), label = "P2", value = if (!is.na(vt_def_lup[2])) vt_def_lup[2] else "", placeholder = "P2", tabindex = 11)),
                                     column(1, style = brd_b, text_input_with_tabindex(ns("vt_P3"), label = "P3", value = if (!is.na(vt_def_lup[3])) vt_def_lup[3] else "", placeholder = "P3", tabindex = 12)),
                                     column(1, style = paste0(brd_b, brd_r), text_input_with_tabindex(ns("vt_P4"), label = "P4", value = if (!is.na(vt_def_lup[4])) vt_def_lup[4] else "", placeholder = "P4", tabindex = 13)),
                                     column(1, offset = 1, text_input_with_tabindex(ns("vt_libero1"), label = "Libero 1", value = if (is.na(vt_def_lup[7]) && length(vt_libs) > 0) vt_libs[1] else if (!is.na(vt_def_lup[7]) && vt_def_lup[7] == -1) "" else if (!is.na(vt_def_lup[7])) vt_def_lup[7], placeholder = "Libero 1", tabindex = 17)),
                                     column(1, text_input_with_tabindex(ns("vt_libero2"), label = "Libero 2", value = if (is.na(vt_def_lup[8]) && length(vt_libs) > 1) vt_libs[2] else if (!is.na(vt_def_lup[8]) && vt_def_lup[8] == -1) "" else if (!is.na(vt_def_lup[8])) vt_def_lup[8], placeholder = "Libero 2", tabindex = 18)),
                                     column(1, offset = 1, tags$br(), actionButton(ns("rot_visiting_fwd"), tags$span("Rotate", icon("redo"))), actionButton(ns("rot_visiting_back"), tags$span("Rotate back", icon("undo"))))),
                                 style = paste0("background: ", styling$v_court_colour)
                             ))
                )
            ))
    })

    observeEvent(input$rot_home_fwd, {
        temp <- input$ht_P1
        updateTextInput(session = session, inputId = "ht_P1", value = input$ht_P2)
        updateTextInput(session = session, inputId = "ht_P2", value = input$ht_P3)
        updateTextInput(session = session, inputId = "ht_P3", value = input$ht_P4)
        updateTextInput(session = session, inputId = "ht_P4", value = input$ht_P5)
        updateTextInput(session = session, inputId = "ht_P5", value = input$ht_P6)
        updateTextInput(session = session, inputId = "ht_P6", value = temp)
    })
    observeEvent(input$rot_home_back, {
        temp <- input$ht_P6
        updateTextInput(session = session, inputId = "ht_P2", value = input$ht_P1)
        updateTextInput(session = session, inputId = "ht_P3", value = input$ht_P2)
        updateTextInput(session = session, inputId = "ht_P4", value = input$ht_P3)
        updateTextInput(session = session, inputId = "ht_P5", value = input$ht_P4)
        updateTextInput(session = session, inputId = "ht_P6", value = input$ht_P5)
        updateTextInput(session = session, inputId = "ht_P1", value = temp)
    })
    observeEvent(input$rot_visiting_fwd, {
        temp <- input$vt_P1
        updateTextInput(session = session, inputId = "vt_P1", value = input$vt_P2)
        updateTextInput(session = session, inputId = "vt_P2", value = input$vt_P3)
        updateTextInput(session = session, inputId = "vt_P3", value = input$vt_P4)
        updateTextInput(session = session, inputId = "vt_P4", value = input$vt_P5)
        updateTextInput(session = session, inputId = "vt_P5", value = input$vt_P6)
        updateTextInput(session = session, inputId = "vt_P6", value = temp)
    })
    observeEvent(input$rot_visiting_back, {
        temp <- input$vt_P6
        updateTextInput(session = session, inputId = "vt_P2", value = input$vt_P1)
        updateTextInput(session = session, inputId = "vt_P3", value = input$vt_P2)
        updateTextInput(session = session, inputId = "vt_P4", value = input$vt_P3)
        updateTextInput(session = session, inputId = "vt_P5", value = input$vt_P4)
        updateTextInput(session = session, inputId = "vt_P6", value = input$vt_P5)
        updateTextInput(session = session, inputId = "vt_P1", value = temp)
    })

    output$edit_lineup_commit_ui <- renderUI({
        htok <- nzchar(input$ht_P1) && nzchar(input$ht_P2) && all(c(input$ht_P1, input$ht_P2) %in% rdata$dvw$meta$players_h$number)
        if (!beach) {
            htok <- htok && nzchar(input$ht_P3) && nzchar(input$ht_P4) && nzchar(input$ht_P5) && nzchar(input$ht_P6) && nzchar(input$ht_setter)
            htok <- htok && length(unique(c(input$ht_P1, input$ht_P2, input$ht_P3, input$ht_P4, input$ht_P5, input$ht_P6))) == 6
            htok <- htok && input$ht_setter %in% c(input$ht_P1, input$ht_P2, input$ht_P3, input$ht_P4, input$ht_P5, input$ht_P6)
            htok <- htok && all(c(input$ht_P3, input$ht_P4, input$ht_P5, input$ht_P6) %in% rdata$dvw$meta$players_h$number)
            if (nzchar(input$ht_libero1)) htok <- htok && !input$ht_libero1 %in% c(input$ht_P1, input$ht_P2, input$ht_P3, input$ht_P4, input$ht_P5, input$ht_P6) && input$ht_libero1 %in% rdata$dvw$meta$players_h$number
            if (nzchar(input$ht_libero2)) htok <- htok && !input$ht_libero2 %in% c(input$ht_P1, input$ht_P2, input$ht_P3, input$ht_P4, input$ht_P5, input$ht_P6) && input$ht_libero2 %in% rdata$dvw$meta$players_h$number
        } else {
            htok <- htok && length(unique(c(input$ht_P1, input$ht_P2))) == 2
        }
        vtok <- nzchar(input$vt_P1) && nzchar(input$vt_P2) && all(c(input$vt_P1, input$vt_P2) %in% rdata$dvw$meta$players_v$number)
        if (!beach) {
            vtok <- vtok && nzchar(input$vt_P3) && nzchar(input$vt_P4) && nzchar(input$vt_P5) && nzchar(input$vt_P6) && nzchar(input$vt_setter)
            vtok <- vtok && length(unique(c(input$vt_P1, input$vt_P2, input$vt_P3, input$vt_P4, input$vt_P5, input$vt_P6))) == 6
            vtok <- vtok && input$vt_setter %in% c(input$vt_P1, input$vt_P2, input$vt_P3, input$vt_P4, input$vt_P5, input$vt_P6)
            vtok <- vtok && all(c(input$vt_P3, input$vt_P4, input$vt_P5, input$vt_P6) %in% rdata$dvw$meta$players_v$number)
            if (nzchar(input$vt_libero1)) vtok <- vtok && !input$vt_libero1 %in% c(input$vt_P1, input$vt_P2, input$vt_P3, input$vt_P4, input$vt_P5, input$vt_P6) && input$vt_libero1 %in% rdata$dvw$meta$players_v$number
            if (nzchar(input$vt_libero2)) vtok <- vtok && !input$vt_libero2 %in% c(input$vt_P1, input$vt_P2, input$vt_P3, input$vt_P4, input$vt_P5, input$vt_P6) && input$vt_libero2 %in% rdata$dvw$meta$players_v$number
        } else {
            vtok <- vtok && length(unique(c(input$vt_P1, input$vt_P2))) == 2
        }
        if (htok && vtok) actionButton("edit_commit", label = "Update teams lineups", class = "continue") else NULL
    })

    output$ht_display_team <- DT::renderDataTable({
        this <- rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "role")]##, "special_role")]
        if (!is.null(this)) {
            DT::datatable(names_first_to_capital(this), rownames = FALSE, selection = "single", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE))
        } else {
            NULL
        }
    })

    output$vt_display_team <- DT::renderDataTable({
        this <- rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "role")]##, "special_role")]
        if (!is.null(this)) {
            DT::datatable(names_first_to_capital(this), rownames = FALSE, selection = "single", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE))
        } else {
            NULL
        }
    })
}

mod_team_select_ui <- function(id){
    ns <- NS(id)
    actionButton(ns("select_teams_button"), "Select teams", icon = icon("users"), class = "leftbut")
}

mod_team_select <- function(input, output, session, rdata, editing, app_data) {
    ns <- session$ns
    htdata_select <- reactiveVal(NULL)
    vtdata_select <- reactiveVal(NULL)
    team_Table <- reactiveVal(NULL)


    observeEvent(input$select_teams_button, {
        editing$active <- "select_teams"
        season_dir <- if (is.null(app_data$season_dir) || !dir.exists(app_data$season_dir)) dchoose(caption = "Choose season directory") else app_data$season_dir
        withProgress({
            team_table <- get_teams_from_dvw_dir(season_dir)
        }, message = "Reading match/team files, please wait")
        if (is.null(team_table) || nrow(team_table) < 1) {
            showModal(modalDialog(title = "Choose teams", size = "m",
                                  tags$p(tags$strong("No teams found")),
                                  tags$p("No teams were found in the season directory. You might need to enter new team information via the 'Edit teams' button"),
                                  footer = tags$div(actionButton("edit_cancel", label = "Cancel", class = "cancel"))))
        } else {
            team_Table(team_table)
            showModal(modalDialog(title = "Choose teams", size = "m",
                                  tabsetPanel(
                                      tabPanel("Home team",
                                               fluidRow(
                                                   column(4, selectInput(ns("home_team_select"), "Select home team", team_table$team_id, multiple=FALSE, selectize=FALSE)),
                                                   column(4, textInput(ns("ht_select_id"), label = "Team ID:", value = "")),
                                                   column(4, textInput(ns("ht_select_name"), label = "Team name:", value = ""))),
                                               fluidRow(
                                                   column(4),
                                                   column(4, textInput(ns("ht_select_coach"), label = "Coach:", value = "")),
                                                   column(4, textInput(ns("ht_select_assistant"), label = "Assistant:", value = "")),
                                                   ),
                                               fluidRow(
                                                   column(12, DT::dataTableOutput(ns("ht_select_team")))
                                               )),
                                      tabPanel("Visiting team",
                                               fluidRow(
                                                   column(4, selectInput(ns("visiting_team_select"), "Select visiting team", team_table$team_id, multiple=FALSE, selectize=FALSE)),
                                                   column(4, textInput(ns("vt_select_id"), label = "Team ID:", value = "")),
                                                   column(4, textInput(ns("vt_select_name"), label = "Team name:", value = ""))),
                                               fluidRow(
                                                   column(4),
                                                   column(4, textInput(ns("vt_select_coach"), label = "Coach:", value = "")),
                                                   column(4, textInput(ns("vt_select_assistant"), label = "Assistant:", value = "")),
                                                   ),
                                               fluidRow(
                                                   column(12, DT::dataTableOutput(ns("vt_select_team")))
                                               ))
                                  ),
                                  footer = tags$div(uiOutput(ns("tsc_ui"), inline = TRUE), actionButton("edit_cancel", label = "Cancel", class = "cancel")))
                      )
        }
    })

    output$tsc_ui <- renderUI({
        if (!is.null(input$ht_select_id) && !is.null(input$vt_select_id) && isTRUE(input$ht_select_id != input$vt_select_id)) {
            actionButton("edit_commit", label = "Select teams", class = "continue")
        } else {
            NULL
        }
    })

    observe({
        updateTextInput(session, "ht_select_id", value = input$home_team_select)
        updateTextInput(session, "ht_select_name", value = team_Table()$team[team_Table()$team_id %eq% input$home_team_select])
        updateTextInput(session, "ht_select_coach", value = team_Table()$coach[team_Table()$team_id %eq% input$home_team_select])
        updateTextInput(session, "ht_select_assistant", value = team_Table()$assistant[team_Table()$team_id %eq% input$home_team_select])

        updateTextInput(session, "vt_select_id", value = input$visiting_team_select)
        updateTextInput(session, "vt_select_name", value = team_Table()$team[team_Table()$team_id %eq% input$visiting_team_select])
        updateTextInput(session, "vt_select_coach", value = team_Table()$coach[team_Table()$team_id %eq% input$visiting_team_select])
        updateTextInput(session, "vt_select_assistant", value = team_Table()$assistant[team_Table()$team_id %eq% input$visiting_team_select])

    })

    output$ht_select_team <- DT::renderDataTable({
        if (is.null(team_Table())) htdata_select(rdata$dvw$meta$players_h)
        if (!is.null(team_Table())) {
            pth <- team_Table()$player_table[team_Table()$team_id %eq% input$home_team_select][[1]]
            htdata_select(pth)
            DT::datatable(pth, rownames = FALSE, colnames = var2fc(colnames(pth)), selection = "none", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE))
        } else {
            NULL
        }
    }, server = TRUE)

    output$vt_select_team <- DT::renderDataTable({
        if (is.null(team_Table())) vtdata_select(rdata$dvw$meta$players_v)
        if (!is.null(team_Table())) {
            ptv <- team_Table()$player_table[team_Table()$team_id %eq% input$visiting_team_select][[1]]
            vtdata_select(ptv)
            DT::datatable(ptv, rownames = FALSE, colnames = var2fc(colnames(ptv)), selection = "none", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE))
        } else {
            NULL
        }
    }, server = TRUE)

    list(htdata_select = htdata_select, vtdata_select = vtdata_select)

}

mod_team_edit_ui <- function(id) {
    ns <- NS(id)
    actionButton(ns("edit_teams_button"), "Edit teams", icon = icon("users"), class = "leftbut")
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
        showModal(modalDialog(title = "Edit teams", size = "l",
                              footer = tags$div(
                                  actionButton("edit_commit", label = "Update teams data", class = "continue"),
                                                actionButton("edit_cancel", label = "Cancel", class = "cancel")),
                              tabsetPanel(
                                  tabPanel("Home team",
                                           fluidRow(column(4, textInput(ns("ht_edit_name"), label = "Team name:", value = rdata$dvw$meta$teams$team[htidx])),
                                                    column(4, textInput(ns("ht_edit_id"), label = "Team ID:", value = rdata$dvw$meta$teams$team_id[htidx])),
                                                    column(4, textInput(ns("ht_edit_coach"), label = "Coach:", value = rdata$dvw$meta$teams$coach[htidx])),
                                                    column(4, textInput(ns("ht_edit_assistant"), label = "Assistant:", value = rdata$dvw$meta$teams$assistant[htidx]))),
                                           DT::dataTableOutput(ns("ht_edit_team")),
                                           wellPanel(
                                               fluidRow(column(1, textInput(ns("ht_new_number"), label = "Number:", placeholder = "Number")),
                                                        column(3, textInput(ns("ht_new_lastname"), label = "Last name:", placeholder = "Last name")),
                                                        column(3, textInput(ns("ht_new_firstname"), label = "First name:", placeholder = "First name")),
                                                        column(2, textInput(ns("ht_new_id"), label = "ID:", placeholder = "ID")),
                                                        column(2, selectInput(ns("ht_new_role"), label = "Role", choices = c("", "libero", "outside", "opposite", "middle", "setter", "unknown"))),
                                                        ##column(1, selectInput(ns("ht_new_special"), label = "Special", choices = c("", "L", "C")))
                                                        ),
                                               fluidRow(column(3, offset = 9, actionButton(ns("ht_add_player_button"), "Add player")))
                                           ),
                                           #actionButton(ns("load_home_team"), label = "Load home team", class = "updating"),
                                           uiOutput(ns("ht_delete_player_ui"))
                                           ),
                                  tabPanel("Visiting team",
                                           fluidRow(column(4, textInput(ns("vt_edit_name"), label = "Team name:", value = rdata$dvw$meta$teams$team[vtidx])),
                                                    column(4, textInput(ns("vt_edit_id"), label = "Team ID:", value = rdata$dvw$meta$teams$team_id[vtidx])),
                                                    column(4, textInput(ns("vt_edit_coach"), label = "Coach:", value = rdata$dvw$meta$teams$coach[vtidx])),
                                                    column(4, textInput(ns("vt_edit_assistant"), label = "Assistant:", value = rdata$dvw$meta$teams$assistant[vtidx]))),
                                           DT::dataTableOutput(ns("vt_edit_team")),
                                           wellPanel(
                                               fluidRow(column(1, textInput(ns("vt_new_number"), label = "Number:", placeholder = "Number")),
                                                        column(3, textInput(ns("vt_new_lastname"), label = "Last name:", placeholder = "Last name")),
                                                        column(3, textInput(ns("vt_new_firstname"), label = "First name:", placeholder = "First name")),
                                                        column(2, textInput(ns("vt_new_id"), label = "ID:", placeholder = "ID")),
                                                        column(2, selectInput(ns("vt_new_role"), label = "Role", choices = c("", "libero", "outside", "opposite", "middle", "setter", "unknown"))),
                                                        ##column(1, selectInput(ns("vt_new_special"), label = "Special", choices = c("", "L", "C")))
                                                        ),
                                               fluidRow(column(3, offset = 9, actionButton(ns("vt_add_player_button"), "Add player")))
                                           ),
                                           #actionButton(ns("load_visiting_team"), label = "Load visiting team", class = "updating"),
                                           uiOutput(ns("vt_delete_player_ui"))
                                           )
                              )
                              ))
    })

    output$ht_edit_team <- DT::renderDataTable({
        if (is.null(htdata_edit())) htdata_edit(rdata$dvw$meta$players_h)
        if (!is.null(htdata_edit())) {
            cols_to_hide <- which(!names(htdata_edit()) %in% c("player_id", "number", "lastname", "firstname", "role"))-1L ## 0-based because no row names ##"special_role"
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
    observe({
        ## fill in player ID automatically
        if (!is.null(input$ht_new_id) && !is.null(input$ht_new_lastname) && !is.null(input$ht_new_firstname) &&
            !nzchar(input$ht_new_id) && nzchar(input$ht_new_lastname) && nzchar(input$ht_new_firstname)) {
            pid <- toupper(paste0(substr(input$ht_new_lastname, 1, 3), "-", substr(input$ht_new_firstname, 1, 3)))
            ## check that this is unique
            isolate({
                if (pid %in% htdata_edit()$player_id) {
                    pid0 <- pid
                    for (ii in 1:9) {
                        pid <- paste0(pid0, ii)
                        if (!pid %in% htdata_edit()$player_id) break
                    }
                    if (pid %in% htdata_edit()$player_id) pid <- "" ## can't figure it out
                }
            })
            updateTextInput(session, "ht_new_id", value = pid)
        }
        if (!is.null(input$vt_new_id) && !is.null(input$vt_new_lastname) && !is.null(input$vt_new_firstname) &&
            !nzchar(input$vt_new_id) && nzchar(input$vt_new_lastname) && nzchar(input$vt_new_firstname)) {
            pid <- toupper(paste0(substr(input$vt_new_lastname, 1, 3), "-", substr(input$vt_new_firstname, 1, 3)))
            ## check that this is unique
            isolate({
                if (pid %in% vtdata_edit()$player_id) {
                    pid0 <- pid
                    for (ii in 1:9) {
                        pid <- paste0(pid0, ii)
                        if (!pid %in% vtdata_edit()$player_id) break
                    }
                    if (pid %in% vtdata_edit()$player_id) pid <- "" ## can't figure it out
                }
            })
            updateTextInput(session, "vt_new_id", value = pid)
        }
    })
    observeEvent(input$ht_add_player_button, {
        chk <- list(input$ht_new_id, input$ht_new_number, input$ht_new_lastname, input$ht_new_firstname)
        if (!any(vapply(chk, is_nnn, FUN.VALUE = TRUE))) {
            try({
                newrow <- tibble(number = as.numeric(input$ht_new_number), player_id = input$ht_new_id, lastname = input$ht_new_lastname, firstname = input$ht_new_firstname, role = if (nzchar(input$ht_new_role)) input$ht_new_role else NA_character_, special_role = if (input$ht_new_role %eq% "libero") "L" else NA_character_) ##(if (nzchar(input$ht_new_special)) input$ht_new_special else NA_character_)
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
                ##updateSelectInput(session, "ht_new_special", selected = "")
                ## focus to number box
                focus_to_element(ns("ht_new_number"))
            })
        }
    })

    output$vt_edit_team <- DT::renderDataTable({
        if (is.null(vtdata_edit())) vtdata_edit(rdata$dvw$meta$players_v)
        if (!is.null(vtdata_edit())) {
            cols_to_hide <- which(!names(vtdata_edit()) %in% c("player_id", "number", "lastname", "firstname", "role"))-1L ## 0-based because no row names ## , "special_role"
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
                newrow <- tibble(number = as.numeric(input$vt_new_number), player_id = input$vt_new_id, lastname = input$vt_new_lastname, firstname = input$vt_new_firstname, role = if (nzchar(input$vt_new_role)) input$vt_new_role else NA_character_, special_role = if (input$vt_new_role %eq% "libero") "L" else NA_character_) ## if (nzchar(input$vt_new_special)) input$vt_new_special else NA_character_)
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
                ##updateSelectInput(session, "vt_new_special", selected = "")
                ## focus to number box
                focus_to_element(ns("vt_new_number"))
            })
        }
    })

    list(htdata_edit = htdata_edit, vtdata_edit = vtdata_edit)

}

mod_teamscores_ui <- function(id) {
    ns <- NS(id)
    tagList(tags$head(tags$style(paste0("@font-face { font-family:'DSEG14'; src: url('css/DSEG14Modern-Regular.woff2') format('woff2'), url('css/DSEG14Modern-Regular.woff') format('woff'); } .scoreboard { background-color:#00000080; border-radius:4px; padding:1px; } .ptscorenum, .setscorenum { padding: 2px; text-align: center; font-family:'DSEG14', sans-serif; } .ptscorenum { font-size:24px; } .setscorenum { font-size:17px; } #hnscore { padding: 2px; text-align: left; font-size:16px;} #vnscore { padding: 2px; text-align: right; font-size:16px;} #tsc_outer {position:absolute; right:14px; width:20vw; -webkit-transform: translateZ(10); z-index:10;}"))),
            uiOutput(ns("scoreboard"))
            )
}

mod_teamscores <- function(input, output, session, game_state, rdata, styling, visible = reactiveVal(TRUE)) {
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


    output$scoreboard <- renderUI({
        hs <- game_state$home_score_start_of_point
        vs <- game_state$visiting_score_start_of_point
        if (isTRUE(visible())) {
            fluidRow(class = "scoreboard",
                     column(6, style = paste0("background-color:", styling$h_court_colour),
                            fixedRow(column(9, id = "hnscore", tags$strong(rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "*"])),
                                     column(3, class = "setscorenum", if (length(ss()) == 2 && !any(is.na(ss()))) tags$span(ss()[1]))),
                            fixedRow(column(3, offset = 9, class = "ptscorenum", if (!is.na(hs)) tags$span(hs)))),
                     column(6, style = paste0("background-color:", styling$v_court_colour),
                            fixedRow(column(3, class = "setscorenum", if (length(ss()) == 2 && !any(is.na(ss()))) tags$span(ss()[2])),
                                     column(9, id = "vnscore", tags$strong(rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "a"]))),
                            fixedRow(column(3, class = "ptscorenum", if (!is.na(vs)) tags$span(vs)))
                            )
                     )
        } else {
            NULL
        }
    })
}
