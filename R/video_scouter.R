#' Launch a Shiny app for scouting
#'
#' @param dvw string or datavolley: either the path to a dvw file (which will be read by \code{\link[datavolley]{dv_read}}) or a datavolley object (e.g. as returned by [dv_create()]. Passing the file name (not the datavolley object) is required if any extra arguments are passed via \code{...}
#' @param video_file string: optionally, the path to the video file. If not supplied (or \code{NULL}) the video file specified in the dvw file will be used. Provide `video_file = NA` to run the app without a video file
#' @param court_ref data.frame or string: data.frame with the court reference (as returned by [ovideo::ov_shiny_court_ref()]) or the path to the rds file containing the output from this
#' @param scouting_options list: a named list with entries as per [ov_scouter_options()]
#' @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
#' @param compound_code tibble: the table of compound codes
#' @param launch_browser logical: if \code{TRUE}, launch the app in the system's default web browser (passed to \code{\link[shiny]{runApp}}'s \code{launch.browser} parameter)
#' @param prompt_for_files logical: if \code{dvw} was not specified, prompt the user to select the dvw file
#' @param ... : extra parameters passed to \code{\link[datavolley]{dv_read}} (if \code{dvw} is a provided as a string) and/or to the shiny server and UI functions
#'
#' @seealso \code{\link[datavolley]{dv_read}}
#'
#' @export
ov_scouter <- function(dvw, video_file, court_ref, scouting_options = ov_scouter_options(), default_scouting_table = ov_default_scouting_table(), compound_table = ov_default_compound_table(), launch_browser = TRUE, prompt_for_files = interactive(), ...) {
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
        dvw <- do.call(datavolley::dv_read, dv_read_args)
    } else if (is.null(dvw)) {
        ## dummy, no file. Maybe just tagging a video
        stop("dvw is null")
    } else {
        if (!inherits(dvw, "datavolley")) stop("dvw should be a datavolley object or the path to a .dvw file")
        dvw_filename <- dvw$meta$filename
    }
    ## make sure we have an attack table, TODO add parm for the default to use here
    if (is.null(dvw$meta$attacks)) dvw$meta$attacks <- ov_simplified_attack_table()

    ## deal with video_file parm
    if (is.null(dvw$meta$video)) dvw$meta$video <- tibble(camera = character(), file = character())
    if (!is.null(video_file) && !is.na(video_file)) {
        dvw$meta$video <- tibble(camera = "Camera0", file = fs::path_real(video_file))
    }
    ## TODO allow file chooser to find video file
    if (nrow(dvw$meta$video) > 1) {
        stop("multiple video files have been specified in the dvw file metadata, can't handle this yet")
    } else if (nrow(dvw$meta$video) < 1) {
        stop("no video files specified, either in the dvw file or via the video_file parameter")
    } else {
        if (!file.exists(dvw$meta$video$file)) stop("specified video file (", dvw$meta$video$file, ") does not exist. Perhaps specify the local path via the video_file parameter?")
    }
    ## look for the court ref data, if it hasn't been provided
    if (missing(court_ref)) {
        temp <- NULL
        if (packageVersion("ovideo") >= "0.14.3") temp <- suppressWarnings(ovideo::ov_get_video_data(video_file))
        if (!is.null(temp)) {
            court_ref <- temp
        } else {
            crfile <- paste0(fs::path_ext_remove(video_file), "_video_info.rds")
            if (file.exists(crfile)) tryCatch(court_ref <- readRDS(crfile)$court_ref, error = function(e) {
                warning("found video_info.rds file but could not extract court_ref component")
            })
        }
    }
    if (!is.null(court_ref)) {
        if (is.list(court_ref) && "court_ref" %in% names(court_ref)) court_ref <- court_ref$court_ref
        if (!is.data.frame(court_ref) || !all(c("image_x", "image_y", "court_x", "court_y") %in% names(court_ref))) {
            stop("court_ref is not of the expected format")
        }
    }
    opts <- ov_scouter_options()
    for (nm in names(scouting_options)) opts[[nm]] <- scouting_options[[nm]]
    ## finally the shiny app
    app_data <- c(list(dvw_filename = dvw_filename, dvw = dvw, dv_read_args = dv_read_args, with_video = !is.na(video_file), court_ref = court_ref, options = opts, default_scouting_table = default_scouting_table, compound_table = compound_table, ui_header = tags$div()), other_args)
    app_data$serving <- "*" ## HACK for testing
    app_data$play_overlap <- 0.5 ## amount (in seconds) to rewind before restarting the video, after pausing to enter data
    app_data$evaluation_decoder <- skill_evaluation_decoder() ## to expose as a parameter, perhaps
    this_app <- list(ui = ov_scouter_ui(app_data = app_data), server = ov_scouter_server(app_data = app_data))
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser)
}

#' Scouting options
#'
#' @param nblockers logical: scout the number of blockers on each attack?
#' @param default_nblockers integer: if `nblockers` is TRUE, what number of blockers should we default to? If `NA`, no default
#' @param transition_sets logical: scout sets in transition? If `FALSE`, just the endpoint of each attack (i.e. the dig) and the subsequent counter-attack are scouted
#' @param team_system string: the assumed system that teams are using to assign e.g. passing and hitting responsibilities
#' * "SHM3" - a setter-hitter-middle rotation, with 3 passers (the libero and two outside hitters)
#'
#' @return A named list
#'
#' @export
ov_scouter_options <- function(nblockers = TRUE, default_nblockers = NA, transition_sets = FALSE, team_system = "SHM3") {
    skill_tempo_map <- tribble(~skill, ~tempo_code, ~tempo,
                               "Serve", "Q", "Jump serve",
                               "Serve", "M", "Jump-float serve",
                               "Serve", "H", "Float serve",
                               "Serve", "T", "Topspin serve")
    ## or (some) beach conventions are T=jump-float, H=standing; VM use H=float far from the service line and T=float from the service line
    list(nblockers = nblockers, default_nblockers = default_nblockers, transition_sets = transition_sets, team_system = team_system, skill_tempo_map = skill_tempo_map)
}


## TODO
## - if on first serve of the set, the click is at the wrong end according to game_state$home_end and game_state$serving == "*", then either auto-flip the court or pause and check with user
## - outside of video pane, have "Rewind 5s", "Pause", "Enter manual code" buttons
## - propagate clicks to video itself? (controls)
## - have a popup with all buttons required to manually enter a skill code:
##   Team * a
##   Number blah
##   Skill S R etc but not including T C P etc
##   - and text boxes to enter T, C, P etc
## - player selection via the court inset, with players shown in their assumed playing locations ?
## - allow starting libero to be specified in the lineup, and that libero shown first in selections by default

## default/pre-selected choices:
## - serves, guess serve type based on player's previous serves AND/OR time between serve and reception contacts
## - reception evaluation (and serve evaluation) based on set contact xy and time between reception and set contacts
## - passing player based on reception xy, rotation, and assumed passing system
## - attacking player based on start xy, rotation, and assumed offensive system
## - attack tempo based on time and distance between set and attack contacts
## - number of blockers based on attack tempo and location (default to 1 on quick, 2 on medium, 3 on high  - make this configurable)
