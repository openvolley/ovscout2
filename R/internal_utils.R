`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)
`%neq%` <- function (x, y) x != y & !is.na(x) & !is.na(y)

is_nnn <- function(z) length(z) < 1 || (length(z) == 1 && (is.na(z) || (z %eq% "none")))
nn_or <- function(z, or = "") if (is.null(z)) or else z

most_common_value <- function(x, na.rm = FALSE) {
    ux <- unique(x)
    if (na.rm) {
        ux <- ux[!is.na(ux)]
        if (length(ux)<1) ux <- methods::as(NA, class(x))
    }
    ux[which.max(tabulate(match(x, ux)))]
}

## leading zeros on numbers, e.g. jersey numbers
ldz <- function(nn, width = 2) formatC(suppressWarnings(as.integer(nn)), flag = "0", width = width)
## same but forcing NAs, negative numbers, and numbers > 99 to "00"
ldz2 <- function(n, width = 2) {
    z <- suppressWarnings(as.integer(n))
    z[is.na(z) | z < 0 | z > 99] <- 0L
    formatC(z, flag = "0", width = width)
}

## plotting
court_circle <- function(cxy, r = 0.45, cz = NULL, end = "lower", nseg = 31) {
    ## cxy must be data.frame with x and y centres
    if (!is.null(cz)) cxy <- datavolley::dv_xy(cz, end = end)
    th <- seq(0, 2*pi, length.out = nseg)
    crx <- r*cos(th)
    cry <- r*sin(th)
    bind_rows(lapply(seq_len(nrow(cxy)), function(z) data.frame(id = z, x = cxy$x[z] + crx, y = cxy$y[z]+cry)))
}

other_end <- function(end) setdiff(c("upper", "lower"), tolower(end))[1]

dojs <- function(jscmd) {
    ##cat("js: ", jscmd, "\n")
    shiny::getDefaultReactiveDomain()$sendCustomMessage("evaljs", jscmd)
}
js_show2 <- function(id) dojs(paste0("var el=$('#", id, "'); el.show();"))
js_hide2 <- function(id) dojs(paste0("var el=$('#", id, "'); el.hide();"))

js_disable <- function(id) dojs(paste0("$('#", id, "').attr('disabled', 'disabled');"))
js_enable <- function(id) dojs(paste0("$('#", id, "').removeAttr('disabled');"))

## call this before issuing a DT::renderDataTable with a vertical scroller, to set the scroll position on first draw. It can be used to retain the scroll position when re-drawing the table
retain_scroll <- function(tbl_id) {
    ## save the current scroll position to a var
    dojs(paste0(gsub("[\\-]+", "_", tbl_id), "_scrollpos = $('#", tbl_id, " .dataTables_scrollBody').scrollTop();"))
}

names_first_to_capital <- function(x, fun) {
    setNames(x, var2fc(if (missing(fun)) names(x) else vapply(names(x), fun, FUN.VALUE = "", USE.NAMES = FALSE)))
}

var2fc <- function(x) {
    vapply(x, function(z) gsub("_", " ", paste0(toupper(substr(z, 1, 1)), substr(z, 2, nchar(z)))), FUN.VALUE = "", USE.NAMES = FALSE)
}

## Variable width modal dialog
##
## @param width numeric: percentage of viewport width
## @param ... : as for [shiny::modalDialog()]
##
## @return As for [shiny::modalDialog()]
##
## @examples
## \dontrun{
##   showModal(vwModalDialog(title = "Wide dialog", "blah", width = 90))
## }
##
## @export
vwModalDialog <- function(..., width = 90, modal_halign = "center", class) {
    rgs <- list(...)
    rgs$size <- "l"
    md <- do.call(shiny::modalDialog, rgs)
    if (!missing(class)) md$attribs <- c(md$attribs, list(class = class))
    ## recursive function to inject style
    rcc <- function(z) {
        if (is.list(z) && "class" %in% names(z)) {
            idx <- which(names(z) %eq% "class")
            if (any(z[idx] %eq% "modal-lg")) z <- c(list(style = paste0("width: ", width, "vw;", if (modal_halign == "left") "margin-left:0;" else if (modal_halign == "right") "margin-right:0;")), z)
        }
        ## call recursively on list children
        list_child_idx <- vapply(z, is.list, FUN.VALUE = TRUE)
        if (any(list_child_idx)) z[list_child_idx] <- lapply(z[list_child_idx], rcc)
        z
    }
    rcc(md)
}


uuid <- function(n = 1L) uuid::UUIDgenerate(n = n)
is_uuid <- function(x) is.character(x) & nchar(x) == 36 & grepl("^[[:digit:]abcdef\\-]+$", x)
##all(is_uuid(uuid(n = 1000)))

is_url <- function(z) {
    if (length(z) < 1) return(FALSE)
    grepl("^https?://", z, ignore.case = TRUE)
}
is_youtube_url <- function(z) grepl("https?://[^/]*youtube\\.com", z, ignore.case = TRUE) || grepl("https?://youtu\\.be/", z, ignore.case = TRUE)
is_youtube_id <- function(z) {
    if (is.null(z)) {
        FALSE
    } else if (!is.character(z)) {
        rep(FALSE, length(z))
    } else {
        !is.na(z) & nchar(z) == 11 & grepl("^[[:alnum:]_\\-]+$", z)
    }
}
youtube_url_to_id <- function(z) {
    if (!is_youtube_id(z) && grepl("^https?://", z, ignore.case = TRUE)) {
        if (grepl("youtu\\.be", z, ignore.case = TRUE)) {
            ## assume https://youtu.be/xyz form
            tryCatch({
                temp <- httr::parse_url(z)
                if (!is.null(temp$path) && length(temp$path) == 1 && is_youtube_id(temp$path)) {
                    temp$path
                } else {
                    z
                }
            }, error = function(e) z)
        } else {
            tryCatch({
                temp <- httr::parse_url(z)
                if (!is.null(temp$query$v) && length(temp$query$v) == 1) {
                    temp$query$v
                } else {
                    z
                }
            }, error = function(e) z)
        }
    } else {
        z
    }
}
## probably misguided attempt to distinguish internal/public IP addresses/hostnames
is_remote_url <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(x) || !is_url(x)) return(FALSE)
    hst <- httr::parse_url(x)$hostname
    !(hst %in% c("localhost") || grepl("^(127|0|192|172\\.16)\\.", hst))
}

ovscout2_app_dir <- function() rappdirs::user_data_dir(appname = "ovscout2")

## adapted from http://conjugateprior.org/2015/06/identifying-the-os-from-r/
get_os <- function() {
    if (.Platform$OS.type == "windows") return("windows")
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf["sysname"]
        if (tolower(os) == "darwin")
            os <- "osx"
    } else {
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os, ignore.case = TRUE))
            os <- "osx"
        if (grepl("linux-gnu", R.version$os, ignore.case = TRUE))
            os <- "linux"
    }
    os <- tolower(os)
    if (!os %in% c("windows", "linux", "unix", "osx"))
        stop("unknown operating system: ", os)
    os
}

## take a namespacing function (e.g. shiny::NS), but make it safe for use with e.g. js variable or function names
ns4js <- function(fun) function(z) gsub("-", "_", fun(z))

## Disambiguate freeball digs from freeballs over
##
## "Freeball" skill can be used both for sending a freeball to the opposition as well as receiving one. This function adds a `freeball_over` column that will be TRUE for freeballs over and FALSE for freeball digs. Mostly. There are some edge cases it can't resolve or which are ambiguous (e.g. overpass -> freeball back to opposition ... it is both a freeball dig and a freeball over)
##
## @param x datavolleyplays: data frame of plays
##
## @return x with column `freeball_over` added
##
## @export
dv_add_freeball_over <- function(x) {
    mutate(x, freeball_over = .data$skill %eq% "Freeball" & lag(.data$point_id) %eq% .data$point_id & ((!is.na(lead(.data$team)) & lead(.data$team) != .data$team) | lag(.data$team) %eq% .data$team))
}

## convenience wrapper around shiny::icon that inserts verify_fa = FALSE to quiet the warnings
icon <- function(...) {
    shiny::icon(..., verify_fa = FALSE)
}

get_port <- function(port = NULL, port_range = c(3000L, 8000L), host = "127.0.0.1") {
    for (i in 1:20) {
        if (is.null(port)) {
            while (TRUE) {
                port <- sample.int(1, n = diff(port_range)) + port_range[1] - 1L
                if (!port %in% c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)) break
            }
        }
        tmp <- try(httpuv::startServer(host, port, list()), silent = TRUE)
        if (!inherits(tmp, "try-error")) {
            httpuv::stopServer(tmp)
            break
        } else {
            port <- NULL
        }
    }
    port
}

focus_to_modal_element <- function(id, highlight_all = TRUE) {
    ## function to set the cursor focus to a particular entry box in a modal popup
    if (!highlight_all) {
        dojs(paste0("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById('", id, "'); el.selectionStart = el.selectionEnd = el.value.length; el.focus(); });"))
    } else {
        dojs(paste0("$(\"#shiny-modal\").on('shown.bs.modal', function (e) { var el = document.getElementById('", id, "'); el.selectionStart = 0; el.selectionEnd = el.value.length; el.focus(); });"))
    }
}
focus_to_element <- function(id, highlight_all = TRUE) {
    ## function to set the cursor focus to a particular entry box
    ## this entry box can be in a modal popup, so long as the popup is already displayed
    if (!highlight_all) {
        dojs(paste0("var el = document.getElementById('", id, "'); el.selectionStart = el.selectionEnd = el.value.length; el.focus();"))
    } else {
        dojs(paste0("var el = document.getElementById('", id, "'); el.selectionStart = 0; el.selectionEnd = el.value.length; el.focus();"))
    }
}

## helper function to evaluate an expression in the server
eval_in_server <- function(expr) {
    for (pf in seq(1L, sys.nframe() - 1L, by = 1L)) {
        if (exists(".am_in_server", envir = parent.frame(n = pf))) return(eval(expr, envir = parent.frame(n = pf)))
    }
    warning("could not evaluate in server:", expr)
}

focus_to_scout_bar <- function(srv_code) {
    dojs(paste0("var el = document.getElementById('scout_in'); if (el) { ", if (!missing(srv_code)) paste0("el.value = '", srv_code, "'; "), "el.selectionStart = el.selectionEnd = el.value.length; el.focus(); }"))
    eval_in_server(expression(active_ui("scout_bar")))
}

focus_to_playslist <- function() {
    dojs("$('#playslist-tbl-i').focus();")
    eval_in_server(expression(active_ui("playslist")))
    eval_in_server(expression(playslist_mod$redraw_select("last")))
}

refocus_to_ui <- function(el) {
    if (el %eq% "scout_bar") {
        focus_to_scout_bar()
    } else if (el %eq% "playslist") {
        focus_to_playslist()
    }
}

decode_keypress <- function(k, debug = 0) {
    temp <- strsplit(k, "@")[[1]]
    ## elements are 1 = modifiers_and_key, 2 = element_class, 3 = element_id, 4 = cursor_position, 5 = field_length, 6 = time
    mycmd <- temp[1]
    myclass <- temp[2]
    myid <- temp[3]
    if (!is.null(myclass) && nzchar(myclass) && myclass %in% c("form-control")) {
        ## don't process these - they are e.g. key events in DT filter boxes
        mycmd <- NULL
    }
    if (!is.null(mycmd)) {
        if (debug > 2) cat("keypress: ", mycmd, " in element #", myid, " of class", myclass, "\n")
        out <- list(ctrl = FALSE, alt = FALSE, shift = FALSE, meta = FALSE, key = "", charcode = 0L, class = myclass, id = myid)
        mycmd <- strsplit(mycmd, "|", fixed = TRUE)[[1]] ## ctrlKey | altKey | shiftKey | metaKey | keyname | charcode
        if (length(mycmd) >= 5) {
            out <- list(ctrl = mycmd[1] %eq% "true", alt = mycmd[2] %eq% "true", shift = mycmd[3] %eq% "true", meta = mycmd[4] %eq% "true", key = mycmd[5], charcode = if (length(mycmd) > 5) mycmd[6] else 0L, class = myclass, id = myid)
        }
        out
    } else {
        NULL
    }
}

## takes a decoded keypress object from the preceding function
## note that prior to 2025-05-04 the key component was converted to lower case, but not so any more
key_as_text <- function(k) paste0(if (k$ctrl) "Ctrl-", if (k$alt) "Alt-", if (k$shift) "Shift-", if (k$meta) "Meta-", k$key)

## takes a decoded keypress object from `decode_keypress` and a shortcut and returns TRUE if it matches any of the entries in sc
is_shortcut <- function(k, sc) {
    as_txt <- key_as_text(k)
    ## we first look for a match on the full key representation with modifiers (e.g. "Alt-ArrowRight")
    ## e.g. if sc is "Alt-ArrowRight" and we press just "ArrowRight", don't want a match
    if (any(as_txt == sc, na.rm = TRUE)) return(TRUE)
    ## if that fails, look for an exact match in the printed representation (ignoring modifiers, e.g. "$") but only if the shortcut has no modifier
    sc <- sc[!grepl("(Ctrl|Alt|Shift|Meta)\\-", sc)]
    any(k$key == sc, na.rm = TRUE)
}

## per the above, but returns the actual shortcut name(s) that match (or NULL)
as_shortcut <- function(k, sc) {
    as_txt <- key_as_text(k)
    ## we first look for a match on the full key representation with modifiers (e.g. "Alt-ArrowRight")
    ## e.g. if sc is "Alt-ArrowRight" and we press just "ArrowRight", don't want a match
    if (any(as_txt == sc, na.rm = TRUE)) return(names(sc[which(as_txt == sc)]))
    ## if that fails, look for an exact match in the printed representation (ignoring modifiers, e.g. "$") but only if the shortcut has no modifier
    sc <- sc[!grepl("(Ctrl|Alt|Shift|Meta)\\-", sc)]
    if (any(k$key == sc, na.rm = TRUE)) return(names(sc[which(k$key == sc)]))
    NULL
}


## action button that will click itself if you press enter on it, saves handling that keypress elsewhere
actionButton_with_enter <- function(...) actionButton(..., onKeyDown = "if (event.keyCode == 13) { this.click(); }")

## get a variable. Use e.g. inside functions defined outside of the server.R code but which need to read e.g. an input$something value
getvar <- function (varname, fail = TRUE) {
    for (pf in seq(1L, sys.nframe() - 1L, by = 1L)) {
        if (exists(varname, envir = parent.frame(n = pf)))
            return(get(varname, envir = parent.frame(n = pf)))
    }
    if (isTRUE(fail)) stop("variable ", varname, " could not be found")
    NULL
}
