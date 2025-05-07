mod_playslist_ui <- function(id, height = "40vh", styling) {
    ns <- NS(id)
    tagList(
        tags$head(tags$style(paste0(".pl2_fixhdr thead th { position: -webkit-sticky; position: sticky; top: 0; z-index: 2; background-color: #CCC;}",
                           ".pl2-tc {height:", height, "; overflow:hidden} .pl2-tc-inner { overflow-x:hidden; overflow-y:auto; height:100% }",
                           ".", ns("selected"), " {background-color:", if (!missing(styling) && !is.null(styling$playslist_highlight_colour)) styling$playslist_highlight_colour else "orange", ";}"))),
        tags$div(class = "pl2-tc", tags$div(class = "pl2-tc-inner", id = ns("tbl"), uiOutput(ns("pl")))),
        tags$script(HTML(paste0("document.querySelector('#", ns("tbl"), "').addEventListener('click', function(event) { Shiny.setInputValue('", ns("clicked"), "', event.target.parentNode.rowIndex, { priority: 'event' }) });"))) ## click listener
    )
}

mod_playslist <- function(input, output, session, rdata, plays_cols_to_show, plays_cols_renames, display_option = reactiveVal("dv_codes"), height = "40vh") {
    ns <- session$ns
    jsns <- ns4js(ns)
    plays_do_rename <- function(z) names_first_to_capital(dplyr::rename(z, plays_cols_renames))
    selected_row <- reactiveVal(NULL)

    ## we want a copy of rdata$dvw$plays that doesn't get invalidated every time rdata$dvw gets invalidated (this is the actual reactive object)
    plays_hash <- ""
    plays_quiet <- reactiveVal(NULL)
    observe({
        this_hash <- rlang::hash(rdata$dvw$plays)
        if (this_hash != plays_hash) {
            plays_hash <- this_hash
            plays_quiet(rdata$dvw$plays)
        }
    })

    js_with_retry <- function(f, need_n_rows = 1, tries = 10) {
        tries_var <- jsns("tries")
        fn_var <- jsns("retryfn")
        dojs(paste0(tries_var, " = 0; var ", fn_var, "=function(){ var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); if (rows != null && rows.length >", need_n_rows - 1, ") { ", f, " } else { if (", tries_var, " < ", tries, ") { ", tries_var, "++; setTimeout(", fn_var, ", 100) }}}; ", fn_var, "();"))
    }

    dat2html <- function(dat, display_as = "dv_codes") {
        ## make sure all cols are present, otherwise the update won't happen properly when those columns are added later
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
        if (display_as == "dv_codes") {
            dat <- setNames(as.data.frame(dat[, plays_cols_to_show, drop = FALSE]), cnames)
        } else {
            dat$commentary = case_when(!is.na(dat$skill) & dat$skill %in% c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball") ~ paste0(dat$skill, " by ", dat$player_name, " (#", dat$player_number,")"),
                                       !is.na(dat$skill) & dat$skill %in% c("Timeout") ~ paste(dat$skill, dat$team))
            dat$game_time <- ifelse(!is.na(dat$video_time), paste0(floor(dat$video_time / 60), "'", floor(dat$video_time - floor(dat$video_time / 60) * 60)), NA)
            plays_cols_to_show <- c("game_time", "set_number", "Score", "commentary")
            dat <- setNames(as.data.frame(dat[, plays_cols_to_show, drop = FALSE]), c("Game time", "Set", "Score", "Comment"))
        }
        html <- shiny::renderTable(dat, na = "")()
        ## inject our pl2_fixhdr class name and an id for the table element itself. Add tabindex to make it focusable, so that key presses fire the keydown event
        sub("(class[[:space:]]*=[[:space:]]*['\"][^'\"]*)(['\"])", paste0("id=\"", ns("tbl-i"), "\" tabindex=\"0\" \\1 pl2_fixhdr\\2"), html)
    }

    set_data <- function(dat, selected = "last", scroll = TRUE, display_as = "dv_codes") {
        if (identical(selected, "keep")) {
            selected <- selected_row()
        } else if (identical(selected, "last") && !is.null(dat)) {
            selected <- nrow(dat)
        }
        if (!is.numeric(selected)) selected <- NULL
        if (!is.null(dat)) {
            html <- dat2html(dat, display_as = display_as)
            if (!is.null(selected)) {
                selected_row(selected)
                ## add the 'selected' class to the appropriate row
                temp <- stringr::str_locate_all(html, "<tr> <td")[[1]]
                if (nrow(temp) >= selected && nrow(temp) > 0) {
                    html <- paste0(substr(html, 1, temp[selected, 1] + 2), " class=\"", ns("selected"), "\"", substr(html, temp[selected, 1] + 2, nchar(html)))
                }
            } else {
                selected_row(NULL)
            }
            ## if the playstable had focus before we (re-)render it, then we need to assign it focus afterwards
            ##   (when we re-render, we are replacing the DOM element and so the new DOM element will not retain focus, because it's a new element)
            ## check if we have focus
            dojs(paste0(jsns("has_focus"), " = document.activeElement.id === '", ns("tbl-i"), "'"))
            ## then re-assign focus if we did
            html <- paste0(html, paste0("<script>if (", jsns("has_focus"), ") { $('#", ns("tbl-i"), "').focus(); }</script>"))
            output$pl <- renderUI(shiny::HTML(html))
            if (!is.null(selected) && scroll) {
                initfun <- paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); $('#", ns("tbl"), "').scrollTop(rows[", selected - 1L, "].offsetTop - 4 * rows[0].offsetHeight);")
                js_with_retry(initfun, need_n_rows = selected)
                ## might be able to move that to an inline script in the HTML render expression above? TODO check
            }
        } else {
            selected_row(NULL)
            output$pl <- renderUI(NULL)
        }
    }

    was_clicked <- reactiveVal(0L)
    observeEvent(input$clicked, {
        if (!is.null(input$clicked)) {
            if (!is.null(selected_row()) && input$clicked %eq% selected_row()) unselect() else select(input$clicked, scroll = FALSE)
            was_clicked(was_clicked() + 1L)
        }
    })

    ## select a row and then optionally scroll to it
    select <- function(i, scroll = TRUE) {
        if (is.numeric(i) && !is.na(i)) {
            selected_row(i)
            dojs(paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); rows.forEach(row => { row.classList.remove('", ns("selected"), "')}); rows[", i - 1L, "].classList.add('", ns("selected"), "');"))
            if (scroll) scroll_to(i)
        }
    }
    select_last <- function(scroll = TRUE) {
        ## helper function to scroll to last row
        select(nrow(plays_quiet()), scroll = scroll)
    }

    scroll_to <- function(i) {
        ## i is zero based in js, but 1-based when passed in
        if (is.numeric(i)) {
            dojs(paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); if (rows.length >= ", i, ") { $('#", ns("tbl"), "').scrollTop(rows[", i - 1L, "].offsetTop - 4 * rows[0].offsetHeight);}"))
        }
    }

    ## clear selection
    unselect <- function(scroll_to_end = FALSE) {
        selected_row(NULL)
        dojs(paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); rows.forEach(row => { row.classList.remove('", ns("selected"), "')});"))
        if (scroll_to_end) scroll_to(nrow(plays_quiet()))
    }

    redraw_select <- reactiveVal("last")
    observe({
        ## set data initially, and replace it whenever plays_quiet() changes
        set_data(plays_quiet(), selected = isolate(redraw_select()), display_as = display_option())
    })

    list(scroll_playslist = scroll_to, current_row = selected_row, select = select, select_last = select_last, unselect = unselect, redraw_select = redraw_select, clicked = was_clicked)
}
