#' Launch a Shiny app for video synchronisation and scout editing
#'
#' @param dvw string or datavolley: either the path to a dvw file (which will be read by \code{\link[datavolley]{read_dv}}) or a datavolley object as returned by that function. Passing the file name (not the datavolley object) is required if any extra arguments are passed via \code{...}
#' @param video_file string: optionally, the path to the video file. If not supplied (or \code{NULL}) the video file specified in the dvw file will be used. Provide `video_file = NA` to run the app without a video file
#' @param launch_browser logical: if \code{TRUE}, launch the app in the system's default web browser (passed to \code{\link[shiny]{runApp}}'s \code{launch.browser} parameter)
#' @param prompt_for_files logical: if \code{dvw} was not specified, prompt the user to select the dvw file
#' @param ... : extra parameters passed to \code{\link[datavolley]{read_dv}} (if \code{dvw} is a provided as a string) and/or to the shiny server and UI functions
#'
#' @seealso \code{\link[datavolley]{read_dv}}
#'
#' @export
ov_shiny_video_sync <- function(dvw, video_file = NULL, launch_browser = TRUE, prompt_for_files = interactive(), ...) {
    assert_that(is.flag(launch_browser), !is.na(launch_browser))
    assert_that(is.flag(prompt_for_files), !is.na(prompt_for_files))
    dots <- list(...)
    dv_read_args <- dots[names(dots) %in% names(formals(datavolley::dv_read))] ## passed to dv_read
    other_args <- dots[!names(dots) %in% names(formals(datavolley::dv_read))] ## passed to the server and UI
    if ((missing(dvw) || is.null(dvw)) && prompt_for_files) {
        if (.Platform$OS.type == "windows") {
            fchoosefun <- utils::choose.files
        } else {
            if (!interactive()) {
                ## file.choose won't work non-interactively (e.g. started via Rscript)
                if (!requireNamespace("tcltk", quietly = TRUE)) {
                    stop("the tcltk package is required")
                }
                fchoosefun <- tcltk::tk_choose.files
            } else {
                cat("Choose dvw file.\n"); flush.console()
                fchoosefun <- function(...) file.choose()
            }
        }
        dvw <- fchoosefun(caption = "Choose dvw file", multi = FALSE, filters = matrix(c("dvw files (*.dvw)", "*.dvw", "All files (*.*)", "*.*"), nrow = 2, byrow = TRUE))
    }
    if (is.string(dvw)) {
        dvw_filename <- dvw
        if (!"skill_evaluation_decode" %in% names(dv_read_args)) dv_read_args$skill_evaluation_decode <- "guess"
        dv_read_args$filename <- dvw
        dvw <- do.call(datavolley::read_dv, dv_read_args)
    } else if (is.null(dvw)) {
        ## dummy, no file. Maybe just tagging a video
        dvw <- structure(list(plays = tibble(skill = character(), video_time = numeric(), set_number = integer(), home_team_score = integer(), visiting_team_score = integer(), code = character(), time = as.POSIXct(numeric(), origin = "1970-01-01"), file_line_number = integer()), meta = list(), messages = tibble(file_line_number = integer(), message = character())))
    } else {
        if (!inherits(dvw, "datavolley")) stop("dvw should be a datavolley object or the path to a .dvw file")
        dvw_filename <- dvw$meta$filename
    }
    ## deal with video_file parm
    if (is.null(dvw$meta$video)) dvw$meta$video <- tibble(camera = character(), file = character())
    if (!is.null(video_file) && !is.na(video_file)) {
        dvw$meta$video <- tibble(camera = "Camera0", file = fs::path_real(video_file))
    }
    if (!is.na(video_file)) {
        if (nrow(dvw$meta$video) > 1) {
            stop("multiple video files have been specified in the dvw file metadata, can't handle this yet")
        } else if (nrow(dvw$meta$video) < 1) {
            stop("no video files specified, either in the dvw file or via the video_file parameter")
        } else {
            if (!file.exists(dvw$meta$video$file)) stop("specified video file (", dvw$meta$video$file, ") does not exist. Perhaps specify the local path via the video_file parameter?")
        }
    }
    ## look for the court ref data, if it hasn't been provided
    if (!"court_ref" %in% names(other_args) && !is.na(video_file)) {
        temp <- NULL
        if (packageVersion("ovideo") >= "0.14.3") temp <- suppressWarnings(ovideo::ov_get_video_data(video_file))
        if (!is.null(temp)) {
            other_args$court_ref <- temp
        } else {
            crfile <- paste0(fs::path_ext_remove(video_file), "_video_info.rds")
            if (file.exists(crfile)) tryCatch(other_args$court_ref <- readRDS(crfile)$court_ref, error = function(e) {
                warning("found video_info.rds file but could not extract court_ref component")
            })
        }
    }
    if (!is.null(other_args$court_ref)) {
        if (is.list(other_args$court_ref) && "court_ref" %in% names(other_args$court_ref)) other_args$court_ref <- other_args$court_ref$court_ref
        if (!is.data.frame(other_args$court_ref) || !all(c("image_x", "image_y", "court_x", "court_y") %in% names(other_args$court_ref))) {
            warning("court_ref is not of the expected format, ignoring")
            other_args$court_ref <- NULL
        }
    }
    ## finally the shiny app
    app_data <- c(list(dvw_filename = dvw_filename, dvw = dvw, dv_read_args = dv_read_args, with_video = !is.na(video_file)), other_args)
    this_app <- list(ui = ov_shiny_video_sync_ui(app_data = app_data), server = ov_shiny_video_sync_server(app_data = app_data))
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser)
}
