#' Launch a Shiny app for scouting
#'
#' @param dvw string or datavolley: either the path to a dvw file (which will be read by \code{\link[datavolley]{dv_read}}) or a datavolley object (e.g. as returned by [dv_create()]. Passing the file name (not the datavolley object) is required if any extra arguments are passed via \code{...}.
#' If `dvw` is "demo", the app will be started with a demonstration data set
#' @param video_file string: optionally, the path to the video file. If not supplied (or \code{NULL}) the video file specified in the dvw file will be used. Provide `video_file = NA` to run the app without a video file
#' @param court_ref data.frame or string: data.frame with the court reference (as returned by [ovideo::ov_shiny_court_ref()]) or the path to the rds file containing the output from this
#' @param scoreboard logical: if `TRUE`, show a scoreboard in the top-right of the video pane
#' @param scouting_options list: a named list with entries as per [ov_scouter_options()]
#' @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
#' @param compound_table tibble: the table of compound codes
#' @param launch_browser logical: if \code{TRUE}, launch the app in the system's default web browser (passed to \code{\link[shiny]{runApp}}'s \code{launch.browser} parameter)
#' @param prompt_for_files logical: if \code{dvw} was not specified, prompt the user to select the dvw file
#' @param ... : extra parameters passed to \code{\link[datavolley]{dv_read}} (if \code{dvw} is a provided as a string) and/or to the shiny server and UI functions
#'
#' @seealso \code{\link[datavolley]{dv_read}}
#' @examples
#' \dontrun{
#'   ov_scouter("demo")
#' }
#'
#' @export
ov_scouter <- function(dvw, video_file, court_ref, scoreboard = TRUE, scouting_options = ov_scouter_options(), default_scouting_table = ov_default_scouting_table(), compound_table = ov_default_compound_table(), launch_browser = TRUE, prompt_for_files = interactive(), ...) {
    if (!missing(dvw) && identical(dvw, "demo")) return(ov_scouter_demo(scoreboard = scoreboard, scouting_options = scouting_options, default_scouting_table = default_scouting_table, compound_table = compound_table, launch_browser = launch_browser, prompt_for_files = prompt_for_files, ...))
    assert_that(is.flag(launch_browser), !is.na(launch_browser))
    assert_that(is.flag(prompt_for_files), !is.na(prompt_for_files))
    dots <- list(...)
    dv_read_args <- dots[names(dots) %in% names(formals(datavolley::dv_read))] ## passed to dv_read
    other_args <- dots[!names(dots) %in% names(formals(datavolley::dv_read))] ## passed to the server and UI
    fchoose <- function(caption) {
        if (requireNamespace("rstudioapi", quietly = TRUE)) {
            fchoosefun <- function(caption) rstudioapi::selectFile(caption = caption)
        } else {
            if (.Platform$OS.type == "windows") {
                fchoosefun <- function(caption) utils::choose.files(caption = caption, multi = FALSE)
            } else {
                if (!interactive()) {
                    ## file.choose won't work non-interactively (e.g. started via Rscript)
                    if (!requireNamespace("tcltk", quietly = TRUE)) {
                        stop("the tcltk package is required")
                    }
                    fchoosefun <- tcltk::tk_choose.files
                } else {
                    cat(caption, "\n"); flush.console()
                    fchoosefun <- function(caption) file.choose()
                }
            }
        }
        fchoosefun(caption = caption)
    }
    if ((missing(dvw) || is.null(dvw))) {
        if (prompt_for_files) {
            dvw <- tryCatch({
                fchoose(caption = "Choose dvw file")##, filters = matrix(c("dvw files (*.dvw)", "*.dvw", "All files (*.*)", "*.*"), nrow = 2, byrow = TRUE))
            }, error = function(e) NULL)
            if (!is.null(dvw) && (is.character(dvw) && all(!nzchar(dvw) | is.na(dvw)))) dvw <- NULL
        } else {
            dvw <- NULL
        }
    }
    if (is.null(dvw)) {
        ## default to an empty one
        dvw <- dv_create(teams = c("Home team", "Visiting team"))
    }
    if (is.string(dvw)) {
        dvw_filename <- dvw
        if (!"skill_evaluation_decode" %in% names(dv_read_args)) dv_read_args$skill_evaluation_decode <- "guess"
        dv_read_args$filename <- dvw
        dvw <- do.call(datavolley::dv_read, dv_read_args)
    } else if (is.null(dvw)) {
        ## dummy, no file. Maybe just tagging a video
        stop("no dvw file")
    } else {
        if (!inherits(dvw, "datavolley")) stop("dvw should be a datavolley object or the path to a .dvw file")
        dvw_filename <- dvw$meta$filename
    }
    ## make sure we have an attack table, TODO add parm for the default to use here
    if (is.null(dvw$meta$attacks)) dvw$meta$attacks <- ov_simplified_attack_table()

    ## deal with video_file parm
    if (is.null(dvw$meta$video)) dvw$meta$video <- tibble(camera = character(), file = character())

    if (!missing(video_file) && !is.null(video_file) && !is.na(video_file) && nchar(video_file)) {
        tryCatch({
            dvw$meta$video <- tibble(camera = "Camera0", file = fs::path_real(video_file))
        }, error = function(e) stop("the provided video_file (", video_file, ") does not exist"))
    }
    if (nrow(dvw$meta$video) > 1) {
        warning("multiple video files have been specified in the dvw file metadata, using only the first one")
        dvw$meta$video <- dvw$meta$video[1, ]
    }
    ## has any of that resulted in a video file?
    if (!(nrow(dvw$meta$video) == 1 && file.exists(dvw$meta$video$file))) {
        if (prompt_for_files) {
            ## allow file chooser to find video file
            video_file <- fchoose(caption = "Choose video file")##, filters = matrix(c("dvw files (*.dvw)", "*.dvw", "All files (*.*)", "*.*"), nrow = 2, byrow = TRUE))
            if (length(video_file) == 1) dvw$meta$video <- tibble(camera = "Camera0", file = video_file)
        }
    }
    if (nrow(dvw$meta$video) < 1) {
        stop("no video files specified, either in the dvw file or via the video_file parameter")
    } else {
        if (!file.exists(dvw$meta$video$file)) stop("specified video file (", dvw$meta$video$file, ") does not exist. Perhaps specify the local path via the video_file parameter?")
    }

    ## look for the court ref data, if it hasn't been provided
    if (missing(court_ref)) {
        court_ref <- NULL
        if (packageVersion("ovideo") >= "0.14.3") temp <- suppressWarnings(ovideo::ov_get_video_data(video_file))
        if (!is.null(temp)) {
            court_ref <- temp
        } else {
            crfile <- paste0(fs::path_ext_remove(video_file), "_video_info.rds")
            if (file.exists(crfile)) tryCatch(court_ref <- readRDS(crfile), error = function(e) {
                warning("found video_info.rds file but could not extract court_ref component")
            })
        }
    }
    if (!is.null(court_ref)) {
        if (is.data.frame(court_ref) && any(c("image_x", "image_y", "court_x", "court_y") %in% names(court_ref))) {
            ## just the court reference has been passed, not the full video info
            court_ref <- list(court_ref = court_ref)
        }
        if (!is.data.frame(court_ref$court_ref) || !all(c("image_x", "image_y", "court_x", "court_y") %in% names(court_ref$court_ref))) {
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
    app_data$scoreboard <- isTRUE(scoreboard)
    if (app_data$with_video) {
        video_src <- app_data$dvw$meta$video$file[1]
        if (!fs::file_exists(as.character(video_src))) {
            ## can't find the file, go looking for it
            chk <- ovideo::ov_find_video_file(dvw_filename = app_data$dvw_filename, video_filename = video_src)
            if (!is.na(chk)) video_src <- chk
        }
        app_data$video_src <- video_src
        have_lighttpd <- FALSE
        video_server_port <- sample.int(4000, 1) + 8000 ## random port from 8001
        tryCatch({
            chk <- sys::exec_internal("lighttpd", "-version")
            have_lighttpd <- TRUE
        }, error = function(e) warning("could not find the lighttpd executable, install it with e.g. 'apt install lighttpd' on Ubuntu/Debian or from http://lighttpd.dtech.hu/ on Windows. Using \"servr\" video option"))
        video_serve_method <- if (have_lighttpd) "lighttpd" else "servr"
        if (video_serve_method == "lighttpd") {
            ## build config file to pass to lighttpd
            lighttpd_conf_file <- tempfile(fileext = ".conf")
            cat("server.document-root = \"", dirname(video_src), "\"\nserver.port = \"", video_server_port, "\"\n", sep = "", file = lighttpd_conf_file, append = FALSE)
            lighttpd_pid <- sys::exec_background("lighttpd", c("-D", "-f", lighttpd_conf_file), std_out = FALSE) ## start lighttpd not in background mode
            lighttpd_cleanup <- function() {
                message("cleaning up lighttpd")
                try(tools::pskill(lighttpd_pid), silent = TRUE)
            }
            onStop(function() try({ lighttpd_cleanup() }, silent = TRUE))
        } else {
            ## start servr instance serving from the video source directory
            blah <- servr::httd(dir = dirname(video_src), port = video_server_port, browser = FALSE, daemon = TRUE)
            onStop(function() {
                message("cleaning up servr")
                servr::daemon_stop()
            })
        }
        app_data$video_server_base_url <- paste0("http://localhost:", video_server_port)
        message(paste0("video server ", video_serve_method, " on port: ", video_server_port))
    }
    ## initialize the plays and plays2 components
    ## if we've started with an empty dvw and done dv_set_lineups, then we'll have an empty tibble for $plays but something in $plays2
    ## but if we are continuing a partially-scouted file, then we'll have something in plays but not plays2
    if ((is.null(app_data$dvw[["plays"]]) || nrow(app_data$dvw[["plays"]]) < 1)) {
        app_data$dvw$plays <- plays2_to_plays(app_data$dvw$plays2, dvw = app_data$dvw, evaluation_decoder = app_data$evaluation_decoder)
    } else if (is.null(app_data$dvw$plays2) || nrow(app_data$dvw$plays2) < 1) {
        app_data$dvw$plays <- app_data$dvw$plays[!app_data$dvw$plays$skill %eq% "Technical timeout", ]
        app_data$dvw <- preprocess_dvw(app_data$dvw)
        app_data$dvw$plays2 <- plays_to_plays2(app_data$dvw[["plays"]])
    } else {
        stop("both the plays and plays2 components of x are non-empty, so I'm not sure which to use")
    }
    ## styling
    ## note that colours here need to be hex strings or names, but names must be recognized both by R and as CSS colour names
    app_data$styling <- list(h_court_colour = "#BFEFFF", ## lightblue1
                             h_court_highlight = "#43AFD3",
                             v_court_colour = "#BCEE68", ## darkolivegreen2
                             v_court_highlight = "#5D8022",
                             continue = "#10C424", continue_light = "#60FC71",
                             cancel = "#D41024", cancel_light = "#DF5463",
                             undo = "#EB6927", undo_light = "#F9AC50",
                             libero = "yellow", libero_light = "#FFFF70", libero_dark = "#FFCD4C",
                             setter = "grey90",
                             playslist_highlight = "orange")

    this_app <- list(ui = ov_scouter_ui(app_data = app_data), server = ov_scouter_server(app_data = app_data))
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser)
}

#' Scouting options
#'
#' @param attack_end string: "actual" or "intended" the end coordinate of an attack is the actual end location, or the intended one. The actual might differ from the intended if there is a block touch. If "actual", and a block touch is recorded, then the end location of the attack will not be used for the dig location (the dig location will be missing)
#' @param nblockers logical: scout the number of blockers on each attack?
#' @param default_nblockers integer: if `nblockers` is TRUE, what number of blockers should we default to? If `NA`, no default
#' @param transition_sets logical: scout sets in transition? If `FALSE`, just the endpoint of each attack (i.e. the dig) and the subsequent counter-attack are scouted
#' @param team_system string: the assumed system that teams are using to assign e.g. passing and hitting responsibilities
#' * "SHM3" - a setter-hitter-middle rotation, with 3 passers (the libero and two outside hitters)
#' @param setter_dump_code string: the attack combination code for a setter dump
#' @param second_ball_attack_code string: the attack combination code for a second-ball attack
#' @param overpass_attack_code string: the attack combination code for an attack on an overpass
#'
#' @return A named list
#'
#' @export
ov_scouter_options <- function(attack_end = "actual", nblockers = TRUE, default_nblockers = NA, transition_sets = FALSE, team_system = "SHM3", setter_dump_code = "PP", second_ball_attack_code = "P2", overpass_attack_code = "PR") {
    attack_end <- match.arg(attack_end, c("actual", "intended"))
    assert_that(is.flag(nblockers), !is.na(nblockers))
    if (!is.na(default_nblockers)) assert_that(default_nblockers %in% 1:3)
    assert_that(is.flag(transition_sets), !is.na(transition_sets))
    team_system <- match.arg(team_system, c("SHM3"))
    assert_that(is.string(setter_dump_code))
    assert_that(is.string(second_ball_attack_code))
    assert_that(is.string(overpass_attack_code))
    skill_tempo_map <- tribble(~skill, ~tempo_code, ~tempo,
                               "Serve", "Q", "Jump serve",
                               "Serve", "M", "Jump-float serve",
                               "Serve", "H", "Float serve",
                               "Serve", "T", "Topspin serve")
    ## or (some) beach conventions are T=jump-float, H=standing; VM use H=float far from the service line and T=float from the service line
    list(attack_end = attack_end, nblockers = nblockers, default_nblockers = default_nblockers, transition_sets = transition_sets, team_system = team_system, skill_tempo_map = skill_tempo_map, setter_dump_code = setter_dump_code, second_ball_attack_code = second_ball_attack_code, overpass_attack_code = overpass_attack_code)
}


ov_scouter_demo <- function(...) {
    video_file <- ovdata::ovdata_example_video("190301_kats_beds")
    x0 <- ovdata::ovdata_example("190301_kats_beds-clip", as = "parsed")

    ## the court reference for the example video, generated via ovideo::ov_shiny_court_ref
    court_ref <- data.frame(image_x = c(0.05397063, 0.95402573, 0.75039756, 0.28921230),
                            image_y = c(0.02129301, 0.02294600, 0.52049712, 0.51884413),
                            court_x = c(0.5, 3.5, 3.5, 0.5),
                            court_y = c(0.5, 0.5, 6.5, 6.5))

    ## use the team list and match info from the already-scouted file
    x <- dv_create(teams = x0$meta$teams, match = x0$meta$match, players_h = x0$meta$players_h, players_v = x0$meta$players_v)
    ## enter the team lineups for set 1, with liberos
    x <- dv_set_lineups(x, set_number = 1, lineups = list(c(as.numeric(x0$plays[4, paste0("home_p", 1:6)]), 14), c(as.numeric(x0$plays[4, paste0("visiting_p", 1:6)]), 13)), setter_positions = c(x0$plays$home_setter_position[4], x0$plays$visiting_setter_position[4]))

    ov_scouter(x, video_file = video_file, court_ref = court_ref, ...)
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
