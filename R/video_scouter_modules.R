## TODO: special_role column has been hidden, todo add a column of checkbox inputs allowing the captain to be specified?
mod_teamslists_ui <- function(id) {
    ns <- NS(id)
    tagList(tags$head(tags$style("#hroster {padding-left: 0px; padding-right: 0px; background-color: #bfefff; padding: 12px; border: 3px solid white; border-radius:6px;}
                                 #vroster {padding-left: 0px; padding-right: 0px; background-color: #bcee68; padding: 12px; border: 3px solid white; border-radius:6px;}")),
            tags$div(style = "border-radius: 15px; padding: 4px", uiOutput(ns("rosters")))
            )
}

mod_teamslists <- function(input, output, session, rdata, scout_mode_r) {

    ## two_cols = TRUE will put the list of players into two columns within the respective roster panels
    ## vertical = TRUE will stack the two roster panels one above the other
    ns <- session$ns
    output$rosters <- renderUI({
        vertical <- scout_mode_r() == "type"
        if (vertical) {
            tags$div(tags$div(id = "hroster", uiOutput(ns("htroster"))), tags$div(id = "vroster", uiOutput(ns("vtroster"))))
        } else {
            fluidRow(column(6, id = "hroster", uiOutput(ns("htroster"))), column(6, id = "vroster", uiOutput(ns("vtroster"))))
        }
    })
    output$htroster <- renderUI({
        two_cols <- scout_mode_r() != "type"
        this <- remove_players_not_played(rdata$dvw$meta$players_h, plays = rdata$dvw$plays2, home_visiting = "h", faststart_only = TRUE)
        re <- names2roster(this, join = two_cols)
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
        two_cols <- scout_mode_r() != "type"
        this <- remove_players_not_played(rdata$dvw$meta$players_v, plays = rdata$dvw$plays2, home_visiting = "v", faststart_only = TRUE)
        re <- names2roster(this, join = two_cols)
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
    jsns <- ns4js(ns)
    tagList(tags$head(tags$style(paste0("#", ns("court_inset"), " img {max-width:100%; max-height:100%; object-fit:contain;} .crbut, .crbut:hover { font-size:11px; padding:4px; } .crhbut { background-color:", styling$h_court_colour, "; margin-top:2px; } .crhbut:hover, .crhbut:active { margin-top:2px; background-color:", styling$h_court_light_colour, "; } .crvbut { background-color:", styling$v_court_colour, "; margin-top:2px; } .crvbut:hover, .crvbut:active { margin-top:2px; background-color:", styling$v_court_light_colour, "; }"))),
            tags$div(id = ns("outer"), style = "border-radius: 4px; padding: 4px;",
                     fluidRow(
                     column(2,
                            actionButton(ns("rotate_home"), tags$span("Home", tags$br(), icon("redo")), class = "crbut crhbut", title = "Rotate"),
                            actionButton(ns("p1pt_home"), tags$span("Home", tags$br(), icon("plus")), class = "crbut crhbut", title = "+1 point"),
                            actionButton(ns("m1pt_home"), tags$span("Home", tags$br(), icon("minus")), class = "crbut crhbut", title = "-1 point"),
                            actionButton(ns("timeout_home"), tags$span("Home", tags$br(), icon("t")), class = "crbut crhbut", title = "Timeout"),
                            actionButton(ns("substitution_home"), tags$span("Home", tags$br(), icon("right-left")), class = "crbut crhbut", title = "Substitution")
                         ),
                     column(7, id = ns("court_inset_holder"), plotOutputWithAttribs(ns("court_inset"), click = ns("plot_click"), style = "width:100%;")), ## previously used aspect-ratio:0.75; here, but it is not well supported on Safari
                     column(2,
                            actionButton(ns("rotate_visiting"), tags$span("Visiting", tags$br(), icon("redo")), class = "crbut crvbut", title = "Rotate"),
                            actionButton(ns("p1pt_visiting"), tags$span("Visiting", tags$br(), icon("plus")), class = "crbut crvbut", title = "+1 point"),
                            actionButton(ns("m1pt_visiting"), tags$span("Visiting", tags$br(), icon("minus")), class = "crbut crvbut", title = "-1 point"),
                            actionButton(ns("timeout_visiting"), tags$span("Visiting", tags$br(), icon("t")), class = "crbut crvbut", title = "Timeout"),
                            actionButton(ns("substitution_visiting"), tags$span("Visiting", tags$br(), icon("right-left")), class = "crbut crvbut", title = "Substitution")
                     ),
                     ),
                     fluidRow(
                         column(2, offset = 2, actionButton(ns("switch_serving"), HTML("Switch<br />serving team"))),
                         column(1, offset = 4, actionButton(ns("court_inset_swap"), label = tags$span(style = "font-size:150%;", "\u21f5"), class = "iconbut", title = "Flip court diagram")) ## flip court diagram
                     )
                     ),
            tags$script(HTML(resize_observer(ns("outer"), fun = paste0("console.log('setting: ", ns("holder_width"), "'); Shiny.setInputValue('", ns("holder_width"), "', $('#", ns("court_inset_holder"), "').innerWidth());"), nsfun = jsns, debounce = 100, as = "string"))))
}

mod_courtrot2 <- function(input, output, session, rdata, game_state, rally_codes, current_video_src, styling, with_ball_path = function() FALSE) {
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
        libs <- setdiff(c(game_state$ht_lib1, game_state$ht_lib2), c(NA, -1))
        if (length(libs) > 0) {
            ht_libxy <- tibble(number = libs) %>%
                dplyr::left_join(dplyr::filter(rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
            ht_libxy$pos <- c(5, 7)[seq_len(nrow(ht_libxy))]
            ht_libxy$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", ht_libxy$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
            ht_libxy <- cbind(dv_xy(ht_libxy$pos, end = "lower"), ht_libxy) %>% mutate(x = case_when(need_to_flip(current_video_src(), gs$home_team_end) ~ .data$x - 1,
                                                                                                     TRUE ~ .data$x + 3))
        }
        vt_setter <- get_setter(gs, team = "a")
        libs <- setdiff(c(game_state$vt_lib1, game_state$vt_lib2), c(NA, -1))
        if (length(libs) > 0) {
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

    ss <- reactive(calc_sets_won(rdata$dvw$plays2))
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

    observeEvent(input$holder_width, {
        ## set the court_inset height to 4/3 the width, which will fit the content (the image)
        dojs(paste0("$('#", ns("court_inset"), "').height(", input$holder_width * 4 / 3, ");"))
    })

    observeEvent(input$court_inset_swap, game_state$home_team_end <- other_end(game_state$home_team_end))

    return(list(click = clickout,
                rotate_home = reactive(input$rotate_home), p1pt_home = reactive(input$p1pt_home), m1pt_home = reactive(input$m1pt_home),
                timeout_home = reactive(input$timeout_home), substitution_home = reactive(input$substitution_home),
                rotate_visiting = reactive(input$rotate_visiting), p1pt_visiting = reactive(input$p1pt_visiting), m1pt_visiting = reactive(input$m1pt_visiting),
                timeout_visiting = reactive(input$timeout_visiting), substitution_visiting = reactive(input$substitution_visiting)))
}

## base plotting instead of ggplot
mod_courtrot2_base <- function(input, output, session, rdata, game_state, rally_codes, current_video_src, styling, with_ball_path = function() FALSE, current_plays_row = function() NULL) {
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
        ## input$plot_click gives the click location
        out <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
        ## unlike the ggplot version (mod_courtrot2) this base plotting does not need to flip the coordinates, because the actual plot data has been transformed, which means that the click location is always oriented the correct way regardless of whether the court direction has been reversed or not
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
        libs <- setdiff(c(game_state$ht_lib1, game_state$ht_lib2), c(NA, -1))
        if (length(libs) > 0) {
            ht_libxy <- tibble(number = libs) %>%
                dplyr::left_join(dplyr::filter(rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], !is.na(.data$number)), by = "number")
            ht_libxy$pos <- c(5, 7)[seq_len(nrow(ht_libxy))]
            ht_libxy$lastname_wrapped <- vapply(strwrap(gsub("-", "- ", ht_libxy$lastname, fixed = TRUE), 10, simplify = FALSE), paste, collapse = "\n", FUN.VALUE = "", USE.NAMES = FALSE)
            ht_libxy <- cbind(dv_xy(ht_libxy$pos, end = "lower"), ht_libxy) %>% mutate(x = case_when(need_to_flip(current_video_src(), gs$home_team_end) ~ .data$x - 1,
                                                                                                     TRUE ~ .data$x + 3))
        }
        vt_setter <- get_setter(gs, team = "a")
        libs <- setdiff(c(game_state$vt_lib1, game_state$vt_lib2), c(NA, -1))
        if (length(libs) > 0) {
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

    ss <- reactive(calc_sets_won(rdata$dvw$plays2))
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
                offs <- 0L ## adjust the current_plays_row() by this when indexing into temp_rally_plays, because if we construct it from rally_codes() then it contains only the current rally, not the preceding rdata$dvw$plays2 rows
                temp_rally_plays2 <- if (is.null(current_plays_row()) || current_plays_row() > nrow(rdata$dvw$plays2)) {
                                         offs <- nrow(rdata$dvw$plays2)
                                         make_plays2(rally_codes(), game_state = game_state, dvw = rdata$dvw)
                                     } else {
                                         rdata$dvw$plays2
                                     }
                ## court module is plotted flipped if necessary, and coordinates will be oriented to the actual video orientation, so should be OK to plot without flipping
                ## all coords are recorded relative to video1 orientation, so we don't care which video is showing
                if (!is.null(current_plays_row())) {
                    ## plot the selected action
                    toplot <- temp_rally_plays2[current_plays_row() - offs, ]$rally_codes
                    if (length(toplot) > 0 && length(toplot[[1]]) > 0) {
                        toplot <- toplot[[1]]
                        segxy <- bind_rows(toplot %>% dplyr::select(x = "start_x", y = "start_y"),
                                           toplot %>% dplyr::select(x = "mid_x", y = "mid_y"),
                                           toplot %>% dplyr::select(x = "end_x", y = "end_y"))
                        if (any(is.na(segxy[2, ]))) segxy <- segxy[-2, ] ## drop mid coord if missing
                        if (nrow(segxy) > 0) {
                            if (nrow(segxy) == 2) {
                                arrows(x0 = segxy$x[1], y0 = segxy$y[1], x1 = segxy$x[2], y1 = segxy$y[2], angle = 15, length = 0.15)
                            } else if (nrow(segxy) == 3) {
                                lines(x = c(segxy$x[1], segxy$x[2]), y = c(segxy$y[1], segxy$y[2]))
                                arrows(x0 = segxy$x[2], y0 = segxy$y[2], x1 = segxy$x[3], y1 = segxy$y[3], angle = 15, length = 0.15)
                            }
                            points(segxy$x[1], segxy$y[1], pch = 21, bg = "white", cex = 2)
                            text(segxy$x[1], segxy$y[1], labels = "S", col = "black", cex = 0.75)
                            points(tail(segxy$x, 1), tail(segxy$y, 1))
                        }
                    }
                } else if (nrow(rally_codes()) > 0) {
                    ## plot all current rally actions
                    toplot <- mutate(rally_codes(), rn = dplyr::row_number())
                    segxy <- bind_rows(toplot %>% dplyr::filter(.data$skill == "S") %>% dplyr::select(x = "start_x", y = "start_y", rn = "rn"),
                                       toplot %>% dplyr::filter(.data$skill == "S") %>% dplyr::select(x = "end_x", y = "end_y", rn = "rn"),
                                       toplot %>% dplyr::filter(!.data$skill %in% c("S", "R")) %>% dplyr::select(x = "start_x", y = "start_y", rn = "rn"),
                                       toplot %>% dplyr::filter(!.data$skill %in% c("S", "R") & !is.na(.data$mid_x)) %>% dplyr::select(x = "mid_x", y = "mid_y", rn = "rn") %>% mutate(rn = .data$rn + 0.5)) %>%
                        na.omit() %>% dplyr::arrange(.data$rn)
                    if (nrow(segxy) > 0) lines(segxy$x, segxy$y)
                }
            }
        })
    }, height = 800, width = 600, res = 180)

    observeEvent(input$holder_width, {
        ## set the court_inset height to 4/3 the width, which will fit the content (the image)
        dojs(paste0("$('#", ns("court_inset"), "').height(", input$holder_width * 4 / 3, ");"))
    })

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

mod_match_data_edit <- function(input, output, session, rdata, editing, app_data) {
    styling <- app_data$styling
    ns <- session$ns
    observeEvent(input$edit_match_data_button, {
        editing$active <- .C_match_data
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
                                  column(4, if (app_data$scout_mode_r() == "click") tags$div(tags$span(tags$strong("Zones or cones:")), tags$br(), tags$span("Scouting in 'click' mode only supports zones.")) else shiny::selectInput(ns("match_edit_zones_or_cones"), "Zones or cones:", choices = c(Cones = "C", Zones = "Z"), selected = rdata$dvw$meta$match$zones_or_cones))),
                         fluidRow(column(4, textInput(ns("more_edit_scout"), label = "Scout:", value = rdata$dvw$meta$more$scout)),
                                  column(4, textInput(ns("edit_comments1"), label = "Comments:", value = if (ncol(rdata$dvw$meta$comments) > 0) na2mt(rdata$dvw$meta$comments[[1]]) else ""),
                                         textInput(ns("edit_comments2"), label = NULL, value = if (ncol(rdata$dvw$meta$comments) > 1) na2mt(rdata$dvw$meta$comments[[2]]) else ""),
                                         textInput(ns("edit_comments3"), label = NULL, value = if (ncol(rdata$dvw$meta$comments) > 2) na2mt(rdata$dvw$meta$comments[[3]]) else ""),
                                         textInput(ns("edit_comments4"), label = NULL, value = if (ncol(rdata$dvw$meta$comments) > 3) paste(na2mt(unlist(rdata$dvw$meta$comments[4:ncol(rdata$dvw$meta$comments)])), collapse = "\n") else "")
                                         ),
                                  column(4, if (app_data$scout_mode_r() == "type") tags$span(style = "font-size:small", "Note: changing cones/zones here will not change any data that has already been scouted for this match. You are advised to change zones/cones before starting scouting. Changing zones/cones for a partially-scouted file is likely to lead to inconsistencies that will affect your analyses.")))
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
        editing$active <- .C_change_starting_lineup
        ## pause video
        dojs("videojs.players.main_video.pause();")
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
        editing$active <- .C_select_teams
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
            choices <- c(list("No team selected" = "..."), as.list(setNames(team_table$team_id, team_table$team)))
            showModal(modalDialog(title = "Choose teams", size = "m",
                                  tabsetPanel(
                                      tabPanel("Home team",
                                               fluidRow(
                                                   column(4, selectInput(ns("home_team_select"), "Select home team", choices = choices, multiple = FALSE, selectize = FALSE)),
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
                                                   column(4, selectInput(ns("visiting_team_select"), "Select visiting team", choices = choices, multiple = FALSE, selectize = FALSE)),
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
        htnull <- is.null(input$home_team_select) || isTRUE(input$home_team_select == "...")
        updateTextInput(session, "ht_select_id", value = input$home_team_select)
        updateTextInput(session, "ht_select_name", value = if (htnull) "" else team_Table()$team[team_Table()$team_id %eq% input$home_team_select])
        updateTextInput(session, "ht_select_coach", value = if (htnull) "" else team_Table()$coach[team_Table()$team_id %eq% input$home_team_select])
        updateTextInput(session, "ht_select_assistant", value = if (htnull) "" else team_Table()$assistant[team_Table()$team_id %eq% input$home_team_select])

        vtnull <- is.null(input$visiting_team_select) || isTRUE(input$visiting_team_select == "...")
        updateTextInput(session, "vt_select_id", value = input$visiting_team_select)
        updateTextInput(session, "vt_select_name", value = if (vtnull) "" else team_Table()$team[team_Table()$team_id %eq% input$visiting_team_select])
        updateTextInput(session, "vt_select_coach", value = if (vtnull) "" else team_Table()$coach[team_Table()$team_id %eq% input$visiting_team_select])
        updateTextInput(session, "vt_select_assistant", value = if (vtnull) "" else team_Table()$assistant[team_Table()$team_id %eq% input$visiting_team_select])

    })

    output$ht_select_team <- DT::renderDataTable({
        if (is.null(team_Table())) htdata_select(rdata$dvw$meta$players_h)
        if (!is.null(team_Table()) && !is.null(input$home_team_select) && !isTRUE(input$home_team_select == "...")) {
            pth <- team_Table()$player_table[team_Table()$team_id %eq% input$home_team_select][[1]]
            htdata_select(pth)
            DT::datatable(pth, rownames = FALSE, colnames = var2fc(colnames(pth)), selection = "none", editable = FALSE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE))
        } else {
            NULL
        }
    }, server = TRUE)

    output$vt_select_team <- DT::renderDataTable({
        if (is.null(team_Table())) vtdata_select(rdata$dvw$meta$players_v)
        if (!is.null(team_Table()) && !is.null(input$visiting_team_select) && !isTRUE(input$visiting_team_select == "...")) {
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

mod_team_edit <- function(input, output, session, rdata, editing, styling, key_in) {
    ns <- session$ns
    htdata_edit <- reactiveVal(NULL)
    vtdata_edit <- reactiveVal(NULL)
    role_choices <- c("outside", "opposite", "middle", "setter", "libero", "unknown")

    observeEvent(input$edit_teams_button, {
        editing$active <- .C_teams
        htidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "*") ## should always be 1
        vtidx <- which(rdata$dvw$meta$teams$home_away_team %eq% "a") ## should always be 2
        ## NB the edit and cancel buttons are global, not namespaced by ns()
        showModal(vwModalDialog(title = "Edit teams",
                              footer = tags$div(
                                  actionButton("edit_commit", label = "Update teams data", class = "continue"),
                                                actionButton("edit_cancel", label = "Cancel", class = "cancel")),
                              tabsetPanel(
                                  tabPanel("Home team",
                                           fluidRow(column(3, textInput(ns("ht_edit_name"), label = "Team name:", value = rdata$dvw$meta$teams$team[htidx])),
                                                    column(3, textInput(ns("ht_edit_id"), label = "Team ID:", value = rdata$dvw$meta$teams$team_id[htidx])),
                                                    column(3, textInput(ns("ht_edit_coach"), label = "Coach:", value = rdata$dvw$meta$teams$coach[htidx])),
                                                    column(3, textInput(ns("ht_edit_assistant"), label = "Assistant:", value = rdata$dvw$meta$teams$assistant[htidx]))),
                                           fluidRow(column(12, tags$div(style = "float:right; font-size:small; margin-left:12px;", "Fast start will auto-populate players 1-99 so you can start scouting and edit the roster later."),
                                                           actionButton(ns("ht_undo_faststart_players"), label = "Undo fast start", class = "leftbut", style = "float:right;"),
                                                           actionButton(ns("ht_faststart_players"), label = tags$span(icon("truck-fast"), "Fast start"), class = "leftbut", style = "float:right;"))),
                                           DT::dataTableOutput(ns("ht_edit_team")),
                                           wellPanel(
                                               fluidRow(column(1, textInput(ns("ht_new_number"), label = "Number:", placeholder = "Number")),
                                                        column(3, textInput(ns("ht_new_lastname"), label = "Last name:", placeholder = "Last name")),
                                                        column(3, textInput(ns("ht_new_firstname"), label = "First name:", placeholder = "First name")),
                                                        column(2, textInput(ns("ht_new_id"), label = "ID:", placeholder = "ID")),
                                                        column(2, selectInput(ns("ht_new_role"), label = "Role", choices = c("", role_choices))),
                                                        ##column(1, selectInput(ns("ht_new_special"), label = "Special", choices = c("", "L", "C")))
                                                        ),
                                               fluidRow(column(2, offset = 10, actionButton_with_enter(ns("ht_add_player_button"), "Add player")))
                                           ),
                                           #actionButton(ns("load_home_team"), label = "Load home team", class = "updating"),
                                           uiOutput(ns("ht_delete_player_ui"))
                                           ),
                                  tabPanel("Visiting team",
                                           fluidRow(column(3, textInput(ns("vt_edit_name"), label = "Team name:", value = rdata$dvw$meta$teams$team[vtidx])),
                                                    column(3, textInput(ns("vt_edit_id"), label = "Team ID:", value = rdata$dvw$meta$teams$team_id[vtidx])),
                                                    column(3, textInput(ns("vt_edit_coach"), label = "Coach:", value = rdata$dvw$meta$teams$coach[vtidx])),
                                                    column(3, textInput(ns("vt_edit_assistant"), label = "Assistant:", value = rdata$dvw$meta$teams$assistant[vtidx]))),
                                           fluidRow(column(12, tags$div(style = "float:right; font-size:small; margin-left:12px;", "Fast start will auto-populate players 1-99 so you can start scouting and edit the roster later."),
                                                           actionButton(ns("vt_undo_faststart_players"), label = "Undo fast start", class = "leftbut", style = "float:right;"),
                                                           actionButton(ns("vt_faststart_players"), label = tags$span(icon("truck-fast"), "Fast start"), class = "leftbut", style = "float:right;"))),
                                           DT::dataTableOutput(ns("vt_edit_team")),
                                           wellPanel(
                                               fluidRow(column(1, textInput(ns("vt_new_number"), label = "Number:", placeholder = "Number")),
                                                        column(3, textInput(ns("vt_new_lastname"), label = "Last name:", placeholder = "Last name")),
                                                        column(3, textInput(ns("vt_new_firstname"), label = "First name:", placeholder = "First name")),
                                                        column(2, textInput(ns("vt_new_id"), label = "ID:", placeholder = "ID")),
                                                        column(2, selectInput(ns("vt_new_role"), label = "Role", choices = c("", role_choices))),
                                                        ##column(1, selectInput(ns("vt_new_special"), label = "Special", choices = c("", "L", "C")))
                                                        ),
                                               fluidRow(column(2, offset = 10, actionButton_with_enter(ns("vt_add_player_button"), "Add player")))
                                           ),
                                           #actionButton(ns("load_visiting_team"), label = "Load visiting team", class = "updating"),
                                           uiOutput(ns("vt_delete_player_ui"))
                                           )
                              )
                              ))
    })

    ## recursive function to inject onchange handler to select widget
    add_onchange_to_select <- function(z, onchange) {
        if (is.list(z) && "name" %in% names(z) && isTRUE(z$name == "select")) {
            z$attribs$onchange <- onchange
        } else {
            ## call recursively on list children
            list_child_idx <- vapply(z, is.list, FUN.VALUE = TRUE)
            if (any(list_child_idx)) z[list_child_idx] <- lapply(z[list_child_idx], add_onchange_to_select, onchange = onchange)
        }
        z
    }

    team_data_add_role_pickers <- function(z, hv) {
        hv <- match.arg(hv, c("h", "v")) ## hv should be "h" or "v"
        z$role_pick <- sapply(seq_len(nrow(z)), function(i) {
            this <- selectInput(inputId = ns(paste0(hv, "_role_", i)), label = NULL, choices = c("", role_choices), selected = if (z$role[i] %in% role_choices) z$role[i] else "unknown")
            this <- as.character(add_onchange_to_select(this, onchange = paste0("Shiny.setInputValue('", ns("rolepicker"), "', {'id': this.id, 'value': this.value }, { priority: 'event' });")))
        })
        z
    }
    observeEvent(input$rolepicker, {
        if (!is.null(input$rolepicker) && all(c("id", "value") %in% names(input$rolepicker)) && editing$active %eq% .C_teams) {
            temp <- stringr::str_match(input$rolepicker$id, ".+\\-([hv])_role_([[:digit:]]+)")
            this_team <- temp[1, 2]
            this_rownum <- suppressWarnings(as.numeric(temp[1, 3]))
            this_data <- if (this_team %eq% "h") htdata_edit() else if (this_team %eq% "v") vtdata_edit() else NULL
            if (!is.null(this_data) && !is.na(this_rownum) && isTRUE(this_rownum > 0 && this_rownum <= nrow(this_data))) {
                this_val <- if (input$rolepicker$value %in% role_choices) input$rolepicker$value else "unknown"
                this_data$role[this_rownum] <- this_val
                this_data$special_role <- if (this_val %eq% "libero") "L" else NA_character_
                if (this_team %eq% "h") {
                    retain_scroll(ns("ht_edit_team"))
                    htdata_edit(this_data)
                } else {
                    retain_scroll(ns("vt_edit_team"))
                    vtdata_edit(this_data)
                }
            }
        }
    })

    output$ht_edit_team <- DT::renderDataTable({
        if (is.null(htdata_edit())) htdata_edit(rdata$dvw$meta$players_h %>% dplyr::arrange(.data$number))
        if (!is.null(htdata_edit())) {
            tbl <- team_data_add_role_pickers(htdata_edit(), hv = "h")
            cols_to_hide <- which(!names(tbl) %in% c("player_id", "number", "lastname", "firstname", "role_pick")) - 1L ## 0-based because no row names ##"special_role"
            ## show the role_pick column not the role, but name it as "Role"
            cnames <- names(names_first_to_capital(tbl))
            cnames[cnames == "Role pick"] <- "Role"
            scrollposvar <- gsub("[\\-]+", "_", ns("ht_edit_team_scrollpos"))
            DT::datatable(tbl, escape = FALSE, rownames = FALSE, colnames = cnames, selection = "single",
                          callback = DT::JS(paste0("if (", scrollposvar, " && ", scrollposvar, " >= 0) { table.one('draw', function () { document.querySelector('#' + '", ns("ht_edit_team"), "' + ' .dataTables_scrollBody').scrollTop = ", scrollposvar, "; }); }")),
                          editable = TRUE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE, columnDefs = list(list(targets = cols_to_hide, visible = FALSE)), scroller = TRUE, scrollY = "35vh"))
        } else {
            NULL
        }
    }, server = TRUE)

    observeEvent(input$ht_edit_team_cell_edit, {
        info <- input$ht_edit_team_cell_edit
        isolate(temp <- htdata_edit())
        temp[info$row, info$col+1L] <- DT::coerceValue(info$value, temp[[info$row, info$col+1L]]) ## no row names so +1 on col indices
        retain_scroll(ns("ht_edit_team"))
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
            retain_scroll(ns("ht_edit_team"))
            htdata_edit(temp)
        }
    })
    auto_player_id <- function(new_id, new_lastname, new_firstname, hv) {
        pid <- new_id
        isolate({
            if (!is.null(new_id) && !is.null(new_lastname) && !is.null(new_firstname) &&
                !nzchar(new_id) && nzchar(new_lastname) && nzchar(new_firstname)) {
                pid <- toupper(paste0(substr(gsub("[[:punct:]]+", "", new_lastname), 1, 3),
                                      "-",
                                      substr(gsub("[[:punct:]]+", "", new_firstname), 1, 3)))
                ## check that this is unique
                curr <- if (hv == "h") htdata_edit() else vtdata_edit()
                if (pid %in% curr$player_id) {
                    pid0 <- pid
                    for (ii in 1:9) {
                        pid <- paste0(pid0, ii)
                        if (!pid %in% htdata_edit()$player_id) break
                    }
                    if (pid %in% htdata_edit()$player_id) pid <- "" ## can't figure it out
                }
            }
        })
        pid
    }

    observeEvent(input$ht_add_player_button, do_add_player(hv = "h"))
    do_add_player <- function(newvals, hv) {
        hv <- match.arg(hv, c("h", "v"))
        if (missing(newvals)) {
            newvals <- if (hv == "h") {
                           list(id = input$ht_new_id, number = input$ht_new_number, lastname = input$ht_new_lastname, firstname = input$ht_new_firstname, role = input$ht_new_role)
                       } else {
                           list(id = input$vt_new_id, number = input$vt_new_number, lastname = input$vt_new_lastname, firstname = input$vt_new_firstname, role = input$vt_new_role)
                       }
        }
        if (!any(vapply(newvals[c("id", "number", "lastname", "firstname")], is_nnn, FUN.VALUE = TRUE)) && !(nzchar(str_trim(newvals$number)) && is.na(suppressWarnings(as.numeric(newvals$number))))) {
            ## don't proceed if the number is non-empty but invalid (not numeric) or any of the id, number, lastname, firstname is NA or NULL (but can be empty)
            try({
                newpid <- auto_player_id(newvals$id, newvals$lastname, newvals$firstname, hv = hv)
                newrow <- tibble(number = as.numeric(newvals$number), player_id = newpid, lastname = newvals$lastname, firstname = newvals$firstname, role = if (nzchar(newvals$role)) newvals$role else NA_character_, special_role = if (newvals$role %eq% "libero") "L" else NA_character_) %>% ##(if (nzchar(newvals$special)) newvals$special else NA_character_)
                    mutate(name = paste(.data$firstname, .data$lastname))
                temp <- bind_rows(if (hv == "h") htdata_edit() else vtdata_edit(), newrow) %>% dplyr::arrange(.data$number)
                if (hv == "h") {
                    retain_scroll(ns("ht_edit_team"))
                    htdata_edit(temp)
                } else {
                    retain_scroll(ns("vt_edit_team"))
                    vtdata_edit(temp)
                }
                ## clear inputs
                updateTextInput(session, paste0(hv, "t_new_number"), value = "")
                updateTextInput(session, paste0(hv, "t_new_id"), value = "")
                updateTextInput(session, paste0(hv, "t_new_lastname"), value = "")
                updateTextInput(session, paste0(hv, "t_new_firstname"), value = "")
                updateSelectInput(session, paste0(hv, "t_new_role"), selected = "")
                ##updateSelectInput(session, paste0(hv, "t_new_special"), selected = "")
                ## focus to number box
                focus_to_element(ns(paste0(hv, "t_new_number")))
            })
        }
    }

    observeEvent(input$add_ht_player_enter, {
        do_add_player(input$add_ht_player_enter, hv = "h")
    })
    observeEvent(input$add_vt_player_enter, {
        ## parse it
        do_add_player(input$add_vt_player_enter, hv = "v")
    })

    observeEvent(key_in(), {
        req(key_in())
        if (grepl("team_editor\\-ht_new_", key_in()$id)) {
            ## in one of the new player entry boxes, treat as accept player
            ## need to force the text in these boxes to be fully propagated first, otherwise the player can be added before the updated inputs have propagated into the Shiny session
            dojs(paste0("Shiny.setInputValue('", ns("add_ht_player_enter"), "', {",
                        "number: $('#", ns("ht_new_number"), "').val(), ",
                        "id: $('#", ns("ht_new_id"), "').val(), ",
                        "lastname: $('#", ns("ht_new_lastname"), "').val(), ",
                        "firstname: $('#", ns("ht_new_firstname"), "').val(), ",
                        "role: $('#", ns("ht_new_role"), "').val() });"))
        } else if (grepl("team_editor\\-vt_new_", key_in()$id)) {
            dojs(paste0("Shiny.setInputValue('", ns("add_vt_player_enter"), "', {",
                        "number: $('#", ns("vt_new_number"), "').val(), ",
                        "id: $('#", ns("vt_new_id"), "').val(), ",
                        "lastname: $('#", ns("vt_new_lastname"), "').val(), ",
                        "firstname: $('#", ns("vt_new_firstname"), "').val(), ",
                        "role: $('#", ns("vt_new_role"), "').val() });"))
        }
    })

    observeEvent(input$ht_faststart_players, {
        num_to_add <- setdiff(1:99, htdata_edit()$number)
        newrows <- tibble(number = num_to_add, player_id = paste0("HOM-P", ldz2(.data$number)), lastname = paste0("Home", ldz2(.data$number)), firstname = "Player", role = NA_character_, special_role = NA_character_) %>%
            mutate(name = paste(.data$firstname, .data$lastname))
        temp <- bind_rows(htdata_edit(), newrows) %>% dplyr::arrange(.data$number)
        retain_scroll(ns("ht_edit_team"))
        htdata_edit(temp)
    })

    observeEvent(input$ht_undo_faststart_players, {
        temp <- remove_players_not_played(roster = htdata_edit(), plays = rdata$dvw$plays2, home_visiting = "h", faststart_only = TRUE) %>% dplyr::arrange(.data$number)
        retain_scroll(ns("ht_edit_team"))
        htdata_edit(temp)
    })

    output$vt_edit_team <- DT::renderDataTable({
        if (is.null(vtdata_edit())) vtdata_edit(rdata$dvw$meta$players_v %>% dplyr::arrange(.data$number))
        if (!is.null(vtdata_edit())) {
            tbl <- team_data_add_role_pickers(vtdata_edit(), hv = "v")
            cols_to_hide <- which(!names(tbl) %in% c("player_id", "number", "lastname", "firstname", "role_pick")) - 1L ## 0-based because no row names ##"special_role"
            ## show the role_pick column not the role, but name it as "Role"
            cnames <- names(names_first_to_capital(tbl))
            cnames[cnames == "Role pick"] <- "Role"
            scrollposvar <- gsub("[\\-]+", "_", ns("vt_edit_team_scrollpos"))
            DT::datatable(tbl, escape = FALSE,
                          callback = DT::JS(paste0("if (", scrollposvar, " && ", scrollposvar, " >= 0) { table.one('draw', function () { document.querySelector('#' + '", ns("vt_edit_team"), "' + ' .dataTables_scrollBody').scrollTop = ", scrollposvar, "; }); }")),
                          rownames = FALSE, colnames = cnames, selection = "single", editable = TRUE, options = list(lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = FALSE, ordering = FALSE, columnDefs = list(list(targets = cols_to_hide, visible = FALSE)), scroller = TRUE, scrollY = "35vh"))
        } else {
            NULL
        }
    }, server = TRUE)
    observeEvent(input$vt_edit_team_cell_edit, {
        info <- input$vt_edit_team_cell_edit
        isolate(temp <- vtdata_edit())
        temp[info$row, info$col+1L] <- DT::coerceValue(info$value, temp[[info$row, info$col+1L]]) ## no row names so +1 on col indices
        retain_scroll(ns("vt_edit_team"))
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
            retain_scroll(ns("vt_edit_team"))
            vtdata_edit(temp)
        }
    })
    observeEvent(input$vt_add_player_button, do_add_player(hv = "v"))
    observeEvent(input$vt_faststart_players, {
        num_to_add <- setdiff(1:99, vtdata_edit()$number)
        newrows <- tibble(number = num_to_add, player_id = paste0("VIS-P", ldz2(.data$number)), lastname = paste0("Visiting", ldz2(.data$number)), firstname = "Player", role = NA_character_, special_role = NA_character_) %>%
            mutate(name = paste(.data$firstname, .data$lastname))
        temp <- bind_rows(vtdata_edit(), newrows) %>% dplyr::arrange(.data$number)
        retain_scroll(ns("vt_edit_team"))
        vtdata_edit(temp)
    })

    observeEvent(input$vt_undo_faststart_players, {
        temp <- remove_players_not_played(roster = vtdata_edit(), plays = rdata$dvw$plays2, home_visiting = "v", faststart_only = TRUE) %>% dplyr::arrange(.data$number)
        retain_scroll(ns("vt_edit_team"))
        vtdata_edit(temp)
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
    ss <- reactive(calc_sets_won(rdata$dvw$plays2))

    output$scoreboard <- renderUI({
        hs <- game_state$home_score_start_of_point
        vs <- game_state$visiting_score_start_of_point
        nhto <- rdata$dvw$plays2 %>% dplyr::filter(.data$code == "*T", .data$set_number == game_state$set_number) %>% nrow
        nvto <- rdata$dvw$plays2 %>% dplyr::filter(.data$code == "aT", .data$set_number == game_state$set_number) %>% nrow
        if (isTRUE(visible())) {
            fluidRow(class = "scoreboard",
                     column(6, style = paste0("background-color:", styling$h_court_colour),
                            fixedRow(column(9, id = "hnscore", tags$strong(rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "*"])),
                                     column(3, class = "setscorenum", if (length(ss()) == 2 && !any(is.na(ss()))) tags$span(ss()[1]))),
                            fixedRow(column(3, class = "setscorenum", tags$span(style = "float:left;", paste0(nhto, "T"))),
                                     column(3, offset = 6, class = "ptscorenum", if (!is.na(hs)) tags$span(hs)))),
                     column(6, style = paste0("background-color:", styling$v_court_colour),
                            fixedRow(column(3, class = "setscorenum", if (length(ss()) == 2 && !any(is.na(ss()))) tags$span(ss()[2])),
                                     column(9, id = "vnscore", tags$strong(rdata$dvw$meta$teams$team[rdata$dvw$meta$teams$home_away_team == "a"]))),
                            fixedRow(column(3, class = "ptscorenum", if (!is.na(vs)) tags$span(vs)),
                                     column(3, offset = 6, class = "setscorenum", tags$span(style = "float:right;", paste0(nvto, "T"))))
                            )
                     )
        } else {
            NULL
        }
    })
}
