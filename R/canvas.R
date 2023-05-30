## #' Helper code for html canvas drawing
## #' @docType class
## #' @export
html_canvas <- R6::R6Class("canvas",
                           public = list(
                               ## #' @description
                               ## #' Create a new canvas object
                               ## #' @param id string: the id of the associated html element
                               ## #' @param width,height integer: the width and height of the canvas
                               ## #' @param canvas_var string: the canvas variable name to use in the js
                               ## #' @param context_var string: the context variable name to use for the plotting operations
                               ## #' @param on_fail string: js code to run if the canvas cannot be initialized
                               initialize = function(id, width = 300, height = 150, canvas_var = "cvs", context_var = "ctx", on_fail = "") {
                                   private$w <- width
                                   private$h <- height
                                   private$id <- id
                                   private$cvs <- canvas_var
                                   private$ctx <- context_var
                                   private$code <- c(paste0("const ", canvas_var, " = document.getElementById('", id, "')"),
                                                     paste0("if (!", canvas_var, ".getContext) { ", if (!is.null(on_fail) && nzchar(on_fail)) on_fail, " } else { const ", context_var, " = ", canvas_var, ".getContext('2d')"))
                               },
                               ## #' @description
                               ## #' Clear the canvas
                               clear_all = function() {
                                   private$code <- c(private$code, paste0(private$ctx, ".clearRect(0, 0, ", private$w - 1, ", ", private$h - 1, ")"))
                               },
                               ## #' @description
                               ## #' Set stroke style
                               ## #' @param col string: hex colour string
                               stroke_style = function(col) {
                                   rgb <- grDevices::col2rgb(col)
                                   private$code <- c(private$code, paste0(private$ctx, ".strokeStyle = \"rgb(", rgb[1], ", ", rgb[2], ", ", rgb[3], ")\""))
                               },
                               ## #' @description
                               ## #' Set fill style
                               ## #' @param col string: hex colour string
                               fill_style = function(col) {
                                   rgb <- grDevices::col2rgb(col)
                                   private$code <- c(private$code, paste0(private$ctx, ".fillStyle = \"rgb(", rgb[1], ", ", rgb[2], ", ", rgb[3], ")\""))
                               },
                               ## #' @description
                               ## #' Draw one or more lines
                               ## #' @param x0,y0 integer: x, y start coords. Or numeric less than 1 for normalized coords
                               ## #' @param x1,y1 integer: x, y end coords. Or numeric less than 1 for normalized coords
                               ## #' @param col string: hex colour string
                               lines = function(x0, y0, x1, y1, col = "#000000") {
                                   if (!is.null(col) && nzchar(col)) self$stroke_style(col)
                                   for (i in seq_along(x0)) private$code <- c(private$code, paste0(private$ctx, ".beginPath()"), paste0(private$ctx, ".moveTo(", if (x0[i] < 1) x0[i] * private$w else x0[i], ", ", if (y0[i] < 1) (1 - y0[i]) * private$h else private$h - y0[i], ")"), paste0(private$ctx, ".lineTo(", if (x1[i] < 1) x1[i] * private$w else x1[i], ", ", if (y1[i] < 1) (1 - y1[i]) * private$h else private$h - y1[i], ")"), paste0(private$ctx, ".stroke()"))
                               },
                               ## #' @description
                               ## #' Draw one or more circles
                               ## #' @param x,y integer: x, y centre coords. Or numeric less than 1 for normalized coords
                               ## #' @param r integer: radius. Or numeric less than one for a radius expressed as a fraction of min(c(w, h))
                               ## #' @param col string: hex colour string
                               ## #' @param fill_col string: hex colour string
                               circles = function(x, y, r, col = "#000000", fill_col = "") {
                                   if (length(r) == 1 && length(x) > 1) r <- rep(r, length(x))
                                   r[r < 1] <- round(min(c(private$w, private$h)) * r[r < 1])
                                   if (!is.null(col) && nzchar(col)) self$stroke_style(col)
                                   if (!is.null(fill_col) && nzchar(fill_col)) self$fill_style(fill_col)
                                   for (i in seq_along(x)) private$code <- c(private$code, paste0(private$ctx, ".beginPath()"), paste0(private$ctx, ".arc(", if (x[i] < 1) x[i] * private$w else x[i], ", ", if (y[i] < 1) (1 - y[i]) * private$h else private$h - y[i], ", ", r[i], ", 0, 2*Math.PI)"), if (!is.null(fill_col) && nzchar(fill_col)) paste0(private$ctx, ".fill()"), if (!is.null(col) && nzchar(col)) paste0(private$ctx, ".stroke()"))
                               },
                               ## #' @description
                               ## #' Return the js for the current canvas drawing
                               js = function() paste(c(private$code, " }"), collapse = ";")
                           ),
                           private = list(
                               id = "",
                               w = NA_integer_,
                               h = NA_integer_,
                               cvs = "",
                               ctx = "",
                               code = character()
                           )
                           )
