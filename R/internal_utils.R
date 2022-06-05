`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

is_nnn <- function(z) is.null(z) || is.na(z) || !nzchar(z)
nn_or <- function(z, or = "") if (is.null(z)) or else z

most_common_value <- function(x, na.rm = FALSE) {
    ux <- unique(x)
    if (na.rm) {
        ux <- ux[!is.na(ux)]
        if (length(ux)<1) ux <- methods::as(NA, class(x))
    }
    ux[which.max(tabulate(match(x, ux)))]
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
vwModalDialog <- function(..., width = 90) {
    rgs <- list(...)
    rgs$size <- "l"
    md <- do.call(shiny::modalDialog, rgs)
    ## recursive function to inject style
    rcc <- function(z) {
        if (is.list(z) && "class" %in% names(z)) {
            idx <- which(names(z) %eq% "class")
            if (any(z[idx] %eq% "modal-lg")) z <- c(list(style = paste0("width: ", width, "vw;")), z)
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
