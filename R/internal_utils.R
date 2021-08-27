`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

is_nnn <- function(z) is.null(z) || is.na(z) || !nzchar(z)

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
