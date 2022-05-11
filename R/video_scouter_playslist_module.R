mod_playslist_ui <- function(id) {
    ns <- NS(id)
    DT::dataTableOutput(ns("playslist"), width = "98%")
}
mod_playslist <- function(input, output, session, rdata, plays_cols_to_show, plays_cols_renames, height = "40vh") {
    ns <- session$ns

    reactive_scrolling <- FALSE ## testing, not sure it helps. In principle if multiple scroll requests get lined up before the first has actually been initiated, then it'll skip to just the last
    plays_do_rename <- function(z) names_first_to_capital(dplyr::rename(z, plays_cols_renames))
    ## the plays display in the RHS table
    output$playslist <- DT::renderDataTable({
        isolate(mydat <- rdata$dvw$plays) ## render once, then isolate from further renders - will be done by replaceData below
        ##if (!is.null(window_height) && !is.na(window_height)) {
        ##    plh <- window_height*0.4
        ##} else {
        ##    plh <- 200
        ##}
        if (!is.null(mydat)) {
            ## make sure all cols are present, otherwise the DT proxy won't update properly when those columns are added later
            for (cl in setdiff(c("skill", "set_number", "home_team_score", "visiting_team_score", plays_cols_to_show), c("Score", "is_skill"))) {
                if (!cl %in% names(mydat)) mydat[[cl]] <- rep(NA, nrow(mydat))
            }
            isolate({
                sel <- list(mode = "single")
                ##last_skill_row <- which(is_skill(mydat$skill))
                ##if (length(last_skill_row)) last_skill_row <- max(last_skill_row)
                ##if (length(last_skill_row) > 0) {
                ##    sel$target <- "row"
                ##    sel$selected <- last_skill_row
                ##}
                ## select last row on startup, no matter what it is
                if (nrow(mydat) > 0) {
                    sel$target <- "row"
                    sel$selected <- nrow(mydat)
                }
            })
            mydat$is_skill <- is_skill(mydat$skill)
            mydat$set_number <- as.factor(mydat$set_number)
            mydat$Score <- paste(mydat$home_team_score, mydat$visiting_team_score, sep = "-")
            cols_to_hide <- which(plays_cols_to_show %in% c("is_skill")) - 1L ## 0-based because no row names
            cnames <- names(plays_do_rename(mydat[1, plays_cols_to_show, drop = FALSE]))
            cnames[plays_cols_to_show == "error_icon"] <- ""
            out <- DT::datatable(mydat[, plays_cols_to_show, drop = FALSE], rownames = FALSE, colnames = cnames,
                                 extensions = "Scroller",
                                 escape = FALSE, ##filter = "top",
                                 selection = sel, options = list(scroller = TRUE,
                                                                 lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = TRUE, "scrollY" = height,##paste0(plh, "px"),
                                                                 ordering = FALSE, ##autoWidth = TRUE,
                                                                 columnDefs = list(list(targets = cols_to_hide, visible = FALSE)),
                                                                 drawCallback = DT::JS(paste0("function(settings) { Shiny.setInputValue('", ns("playslist_redrawn"), "', new Date().getTime()); }"))
                                                                 ##list(targets = 0, width = "20px")) ## does nothing
                                                                 ))
            out <- DT::formatStyle(out, "is_skill", target = "row", backgroundColor = DT::styleEqual(c(FALSE, TRUE), c("#f0f0e0", "lightgreen"))) ## colour skill rows green
            out <- DT::formatStyle(out, "error_icon", color = "red")
            out
        } else {
            NULL
        }
    }, server = TRUE)

    playslist_proxy <- DT::dataTableProxy("playslist")
    playslist_needs_scroll <- reactiveVal(FALSE)
    playslist_scroll_target <- reactiveVal(-99L)
    observeEvent(input$playslist_redrawn, {
        ## when the table has finished being drawn, scroll it if necessary
        if (playslist_needs_scroll()) {
            playslist_needs_scroll(FALSE)
            if (reactive_scrolling) playslist_scroll_target(playslist_current_row()) else scroll_playslist(playslist_current_row())
        }
        ## and mark current row as selected in the table, but don't re-scroll to it
        playslist_select_row(playslist_current_row(), scroll = FALSE)
    })
    ## keep track of selected playslist row as a reactiveVal
    ##   when updating e.g. video time, set this reactiveVal, then wait for DT to redraw THEN scroll
    playslist_current_row <- reactiveVal(NULL)
    ## the playslist_select_row function just changes the visible selection in the table, and optionally scrolls to it, but does not change playslist_current_row() value
    playslist_select_row <- function(rw, scroll = TRUE) {
        DT::selectRows(playslist_proxy, rw)
        if (isTRUE(scroll)) {
            if (reactive_scrolling) playslist_scroll_target(rw) else scroll_playslist(rw)
        }
    }
    ## when the user changes the selected row, update playslist_current_row
    observeEvent(input$playslist_rows_selected, playslist_current_row(input$playslist_rows_selected))

    observe({
        if (reactive_scrolling && !is.null(playslist_scroll_target()) && !is.na(playslist_scroll_target()) && playslist_scroll_target() > 0) {
            scroll_playslist(playslist_scroll_target())
        }
    })

    scroll_playslist <- function(rw) {
        if (!is.null(rw)) {
            ## scrolling works on the VISIBLE row index, so it depends on any column filters that might have been applied
            visible_rowidx <- which(input$playslist_rows_all == rw)
            scrollto <- max(visible_rowidx-1-5, 0) ## -1 for zero indexing, -5 to keep the selected row 5 from the top
            dojs(paste0("$('#", ns("playslist"), "').find('.dataTable').DataTable().scroller.toPosition(", scrollto, ", false);")) ## no anim, faster
        }
    }

    observe({
        ## replace playslist data when dvw$plays changes
        if (!is.null(rdata$dvw$plays) && nrow(rdata$dvw$plays) > 0) replace_playslist_data()
    })
    replace_playslist_data <- function() {
        mydat <- rdata$dvw$plays
        mydat$is_skill <- is_skill(mydat$skill)
        mydat$set_number <- as.factor(mydat$set_number)
        mydat$Score <- paste(mydat$home_team_score, mydat$visiting_team_score, sep = "-")
        DT::replaceData(playslist_proxy, data = mydat[, plays_cols_to_show, drop = FALSE], rownames = FALSE, clearSelection = "none")
    }

    list(scroll_playslist = scroll_playslist)
}
