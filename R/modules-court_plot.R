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
    fluidRow(column(3, id = "hroster", uiOutput(ns("htroster"))),
             column(6, plotOutput(ns("court_inset")), actionButton(ns("court_inset_swap"), label = "\u21f5", class = "iconbut")),
             column(3, id = "vroster", uiOutput(ns("vtroster"))))
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
    ## the court plot itself
    court_inset_home_team_end <- reactiveVal("lower")
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
        }
        if (court_inset_home_team_end() != "lower") p <- p + scale_x_reverse() + scale_y_reverse()
        p
    })
    observeEvent(input$court_inset_swap, {
        court_inset_home_team_end(other_end(court_inset_home_team_end()))
        dojs("document.getElementById('court_inset_swap').blur();") ## un-focus from button
    })
}

