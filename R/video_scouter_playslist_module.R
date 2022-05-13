mod_playslist_ui_old <- function(id) {
    ns <- NS(id)
    DT::dataTableOutput(ns("playslist"), width = "98%")
}
mod_playslist_old <- function(input, output, session, rdata, plays_cols_to_show, plays_cols_renames, height = "40vh") {
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
        playslist_current_row(nrow(mydat))
    }

    list(scroll_playslist = scroll_playslist, current_row = playslist_current_row)
}




mod_playslist_ui <- function(id, height = "40vh") {
    ns <- NS(id)
    tagList(
        tags$head(tags$style(paste0(".pl2_fixhdr thead th { position: -webkit-sticky; position: sticky; top: 0; z-index: 2; background-color: #CCC;}",
                           ".pl2-tc {height:", height, "; overflow:hidden} .pl2-tc-inner { overflow-x:hidden; overflow-y:auto; height:100% }",
                           ".", ns("selected"), " {background-color:#FF8000;}"))),
        tags$div(class = "pl2-tc", tags$div(class = "pl2-tc-inner", id = ns("tbl"), uiOutput(ns("pl"))))
    )
}
mod_playslist <- function(input, output, session, rdata, plays_cols_to_show, plays_cols_renames, height = "40vh") {
    ns <- session$ns
    plays_do_rename <- function(z) names_first_to_capital(dplyr::rename(z, plays_cols_renames))
    isolate(mydat <- rdata$dvw$plays)
    my_selected_row <- reactiveVal(NULL)

    update <- function(dat, selected = "keep", scroll = TRUE, initial = FALSE) {
        if (identical(selected, "keep")) {
            selected <- my_selected_row()
        } else if (identical(selected, "last") && !is.null(dat)) {
            selected <- nrow(dat)
        }
        if (!is.numeric(selected)) selected <- NULL
        if (!is.null(dat)) {
            ## make sure all cols are present, otherwise the DT proxy won't update properly when those columns are added later
            for (cl in setdiff(c("skill", "set_number", "home_team_score", "visiting_team_score", plays_cols_to_show), c("Score", "is_skill"))) {
                if (!cl %in% names(dat)) dat[[cl]] <- rep(NA, nrow(dat))
            }
            ##dat$is_skill <- is_skill(dat$skill)
            plays_cols_to_show <- setdiff(plays_cols_to_show, "is_skill")
            dat$set_number <- as.factor(dat$set_number)
            dat$Score <- paste(dat$home_team_score, dat$visiting_team_score, sep = "-")
            ##cols_to_hide <- which(plays_cols_to_show %in% c("is_skill")) - 1L ## 0-based because no row names
            cnames <- names(plays_do_rename(dat[1, plays_cols_to_show, drop = FALSE]))
            cnames[plays_cols_to_show == "error_icon"] <- ""
            dat <- setNames(as.data.frame(dat[, plays_cols_to_show, drop = FALSE]), cnames)

            html <- shiny::renderTable(dat, na = "")()
            ## inject our pl2_fixhdr class name
            html <- sub("(class[[:space:]]*=[[:space:]]*['\"][^'\"]*)(['\"])", "\\1 pl2_fixhdr\\2", html)
            if (!is.null(selected)) {
                my_selected_row(selected)
                ## add the 'selected' class to the appropriate row
                temp <- stringr::str_locate_all(html, "<tr> <td")[[1]]
                if (nrow(temp) >= selected) {
                    html <- paste0(substr(html, 1, temp[selected, 1] + 2), " class=\"", ns("selected"), "\"", substr(html, temp[selected, 1] + 2, nchar(html)))
                }
            } else {
                my_selected_row(NULL)
            }
            output$pl <- renderUI(shiny::HTML(html))
            if (!is.null(selected) && scroll) {
                ## this ain't great, needs work
                dojs(paste0("setTimeout(() => {console.log('initial scroll'); var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); $('#", ns("tbl"), "').scrollTop(rows[", selected - 1L, "].offsetTop - 4 * rows[0].offsetHeight);}, ", if (initial) 1000 else 200, ")"))
            }
        } else {
            output$pl <- renderUI(NULL)
        }
    }
    do_select <- function(i) {
        if (is.numeric(i)) dojs(paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); rows.forEach(row => { row.classList.remove('", ns("selected"), "')}); rows[", i - 1L, "].classList.add('", ns("selected"), "');"))
    }
    select <- function(i, scroll = TRUE) {
        my_selected_row(i)
        do_select(i)
        if (scroll) scroll_to(i)
    }
    scroll_to <- function(i) {
        ## i is zero based in js, but 1-based when passed in
        if (is.numeric(i)) dojs(paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); $('#", ns("tbl"), "').scrollTop(rows[", i - 1L, "].offsetTop - 4 * rows[0].offsetHeight);"))
    }
    ## initialize
    update(mydat, selected = "last", initial = TRUE)
    observe({
        ## replace playslist data when dvw$plays changes
        update(rdata$dvw$plays, selected = "last")
    })

    ## select is a shortcut for select and then scroll to it
    list(scroll_playslist = scroll_to, current_row = my_selected_row, select = select, update = update)
}
