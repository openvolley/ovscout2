mod_playslist_ui <- function(id, height = "40vh", styling) {
    ns <- NS(id)
    tagList(
        tags$head(tags$style(paste0(".pl2_fixhdr thead th { position: -webkit-sticky; position: sticky; top: 0; z-index: 2; background-color: #CCC;}",
                           ".pl2-tc {height:", height, "; overflow:hidden} .pl2-tc-inner { overflow-x:hidden; overflow-y:auto; height:100% }",
                           ".", ns("selected"), " {background-color:", if (!missing(styling) && !is.null(styling$playslist_highlight_colour)) styling$playslist_highlight_colour else "orange", ";}"))),
        tags$div(class = "pl2-tc", tags$div(class = "pl2-tc-inner", id = ns("tbl"), uiOutput(ns("pl")))),
        tags$script(HTML(paste0("document.querySelector('#", ns("tbl"), "').addEventListener('click', function(event) { Shiny.setInputValue('", ns("clicked"), "', event.target.parentNode.rowIndex) });"))) ## click listener
    )
}

mod_playslist <- function(input, output, session, rdata, plays_cols_to_show, plays_cols_renames, display_option = reactiveVal("dv_codes"), height = "40vh") {
    ns <- session$ns
    jsns <- ns4js(ns)
    plays_do_rename <- function(z) names_first_to_capital(dplyr::rename(z, plays_cols_renames))
    selected_row <- reactiveVal(NULL)

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
        ## inject our pl2_fixhdr class name
        sub("(class[[:space:]]*=[[:space:]]*['\"][^'\"]*)(['\"])", "\\1 pl2_fixhdr\\2", html)
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
            output$pl <- renderUI(shiny::HTML(html))
            if (!is.null(selected) && scroll) {
                initfun <- paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); $('#", ns("tbl"), "').scrollTop(rows[", selected - 1L, "].offsetTop - 4 * rows[0].offsetHeight);")
                js_with_retry(initfun, need_n_rows = selected)
            }
        } else {
            selected_row(NULL)
            output$pl <- renderUI(NULL)
        }
    }

    observeEvent(input$clicked, {
        if (!is.null(input$clicked)) select(input$clicked, scroll = FALSE)
    })

    ## select a row and then optionally scroll to it
    select <- function(i, scroll = TRUE) {
        if (is.numeric(i) && !is.na(i)) {
            selected_row(i)
            dojs(paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); rows.forEach(row => { row.classList.remove('", ns("selected"), "')}); rows[", i - 1L, "].classList.add('", ns("selected"), "');"))
            if (scroll) scroll_to(i)
        }
    }

    scroll_to <- function(i) {
        ## i is zero based in js, but 1-based when passed in
        if (is.numeric(i)) {
            dojs(paste0("var rows=document.querySelectorAll('#", ns("tbl"), " table tbody tr'); if (rows.length >= ", i, ") { $('#", ns("tbl"), "').scrollTop(rows[", i - 1L, "].offsetTop - 4 * rows[0].offsetHeight);}"))
        }
    }

    observe({
        ## set data initially, and replace it whenever dvw$plays changes
        set_data(rdata$dvw$plays, selected = "last", display_as = display_option())
    })

    list(scroll_playslist = scroll_to, current_row = selected_row, select = select)
}
