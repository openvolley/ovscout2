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
    fluidPage(
        fluidRow(column(6,checkboxInput(ns("ballcoordsCI"), label = "Display ball coordinates", FALSE)),
                 column(2,actionButton(ns("court_inset_swap"), label = "\u21f5", class = "iconbut")),
                 column(4,actionButton(ns("cancel_ball_coords"), "Cancel coordinates"))),
        fluidRow(column(3, id = "hroster", uiOutput(ns("htroster"))),
                 column(6,plotOutput(ns("court_inset"),click = ns("plot_click"),dblclick = ns("plot_dblclick"))),
                 column(3, id = "vroster", uiOutput(ns("vtroster")))))
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
    output$coord_clickCI <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
        }
        paste0(
            "Start (sgl-click): x=", round(plot_dataCI$x,2),"y=",round(plot_dataCI$y,2),"\n",
            "End (dbl-click): x=", round(plot_dataCI$xend,2),"y=",round(plot_dataCI$yend,2)
        )
    })
    
    observeEvent(input$cancel_ball_coords, {
        isolate(plot_dataCI$trigger <- 0)
        plot_dataCI$x <- NA
        plot_dataCI$y <- NA
        plot_dataCI$xend <- NA
        plot_dataCI$yend <- NA
    })
    
    plot_dataCI <- reactiveValues(trigger = 0, x = NA, y = NA, xend = NA, yend = NA)
    
    observe({
        req(input$plot_click)
        req(input$plot_dblclick)
        isolate(plot_dataCI$trigger <- plot_dataCI$trigger + 1)
        plot_dataCI$x <- input$plot_click$x
        plot_dataCI$y <- input$plot_click$y
        plot_dataCI$xend <- input$plot_dblclick$x
        plot_dataCI$yend <- input$plot_dblclick$y
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
            
            if(!is.na(rdata$dvw$plays$start_coordinate_x[ridx]) & is.na(rdata$dvw$plays$end_coordinate_x[ridx]) & ball_coords()){
                p = p + geom_point(data = rdata$dvw$plays[ridx,], 
                                   aes_string("start_coordinate_x", "start_coordinate_y"), shape = 16, col = "green")
            }
            if(is.na(rdata$dvw$plays$start_coordinate_x[ridx]) & !is.na(rdata$dvw$plays$end_coordinate_x[ridx]) & ball_coords()){
                p = p + geom_point(data = rdata$dvw$plays[ridx,], 
                                   aes_string("end_coordinate_x", "end_coordinate_y"), shape = 16, col = "red")
            }
            if(!is.na(rdata$dvw$plays$start_coordinate_x[ridx]) & !is.na(rdata$dvw$plays$end_coordinate_x[ridx]) & ball_coords()){
                p = p + geom_point(data = rdata$dvw$plays[ridx,], aes_string("start_coordinate_x", "start_coordinate_y"), 
                                   shape = 16, col = "green", size = 5) + 
                    geom_point(data = rdata$dvw$plays[ridx,], aes_string("end_coordinate_x", "end_coordinate_y"), shape = 16, col = "red", size = 5) + 
                    geom_segment(data = rdata$dvw$plays[ridx,], 
                                 aes_string(x = "start_coordinate_x", y = "start_coordinate_y", xend = "end_coordinate_x", yend = "end_coordinate_y"),
                                 arrow = arrow(length = unit(0.05, "npc")))
            }
            if (plot_dataCI$trigger > 0) {
                p = p +
                    geom_point(aes(plot_dataCI$x, plot_dataCI$y), shape = 16) +
                    geom_point(aes(plot_dataCI$xend, plot_dataCI$yend), shape = 16)+
                    geom_segment(aes(x = plot_dataCI$x, y = plot_dataCI$y, xend = plot_dataCI$xend, yend = plot_dataCI$yend),
                                 arrow = arrow(length = unit(0.05, "npc")), linetype = "dashed")
            }
            
        }
        if (court_inset_home_team_end() != "lower") p <- p + scale_x_reverse() + scale_y_reverse()
        p
    })
    
    observeEvent(input$court_inset_swap, {
        court_inset_home_team_end(other_end(court_inset_home_team_end()))
        dojs("document.getElementById('court_inset_swap').blur();") ## un-focus from button
    })
    return(plot_dataCI)
}

