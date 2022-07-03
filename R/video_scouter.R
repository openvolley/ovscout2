#' Launch a Shiny app for scouting
#'
#' @param dvw string or datavolley: either the path to a dvw or ovs file or a datavolley object (e.g. as returned by [dv_create()]. Passing the file name (not the datavolley object) is required if any extra arguments are passed via `...`. `dvw` can also be an object as saved by `ov_scouter()` in ovs format. If `dvw` is "demo", the app will be started with a demonstration data set
#' @param video_file string: optionally, the path to the video file. If not supplied (or `NULL`) the video file specified in the dvw file will be used. `video_file` can also be a URL (including a YouTube URL or video ID). NOTE that in this case, `court_ref` must be provided, it can't yet be entered interactively in the app
# @param video_file2 string: optionally, the file path or URL to a second video file (e.g. video from the opposite end of the court to `video_file`). If this is a local file, it must be in the same directory as `video_file`
#' @param court_ref data.frame or string: data.frame with the court reference (as returned by [ovideo::ov_shiny_court_ref()]) or the path to the rds file containing the output from this
# @param court_ref2 data.frame or string: data.frame with the court reference for `video_file2` (as returned by [ovideo::ov_shiny_court_ref()]) or the path to the rds file containing the output from this. Note that `court_ref2` must be defined in the same orientation as `court_ref` - for example, the corner of the court considered to be "far left" must be the same physical court corner in both court references
#' @param season_dir string: optional path to a directory with other dvw/ovs files from this season
#' @param auto_save_dir string: optional path to a directory where the dvw will be saved automatically after each rally
#' @param scoreboard logical: if `TRUE`, show a scoreboard in the top-right of the video pane
#' @param ball_path logical: if `TRUE`, show the ball path on the court inset diagram. Note that this will slow the app down slightly
#' @param playlist_display_option string: what to show in the plays table? Either "dv_codes" (scouted codes) or "commentary" (a plain-language interpretation of the touches)
#' @param review_pane logical: if `TRUE`, entry popups will be accompanied by a small video pane that shows a loop of the video of the action in question
#' @param scouting_options list: a named list with entries as per [ov_scouter_options()]
#' @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
#' @param compound_table tibble: the table of compound codes
#' @param shortcuts list: named list of keyboard shortcuts, as returned by [ov_default_shortcuts()]
#' @param launch_browser logical: if `TRUE`, launch the app in the system's default web browser (passed to [shiny::runApp()]'s `launch.browser` parameter)
#' @param prompt_for_files logical: if `dvw` was not specified, prompt the user to select the dvw file
#' @param ... : extra parameters passed to [datavolley::dv_read()] (if `dvw` is a provided as a string) and/or to the shiny server and UI functions
#'
#' @examples
#' \dontrun{
#'   ov_scouter("demo")
#' }
#'
#' @export
ov_scouter <- function(dvw, video_file, court_ref, season_dir, auto_save_dir, scoreboard = TRUE, ball_path = FALSE, review_pane = TRUE, playlist_display_option = "dv_codes", scouting_options = ov_scouter_options(), default_scouting_table = ov_default_scouting_table(), compound_table = ov_default_compound_table(), shortcuts = ov_default_shortcuts(), launch_browser = TRUE, prompt_for_files = interactive(), ...) {

    ## user data directory
    ## are we running under shiny server, shiny (locally) or shiny (docker)?
    run_env <- if (file.exists("/.dockerenv") || tryCatch(any(grepl("docker", readLines("/proc/1/cgroup"))), error = function(e) FALSE)) "shiny_docker" else if (nzchar(Sys.getenv("SHINY_PORT"))) "shiny_server" else "shiny_local"

    user_dir <- if (run_env %eq% "shiny_local") file.path(rappdirs::user_data_dir(), "ovscout2") else tempfile()
    if (!dir.exists(user_dir)) dir.create(user_dir)
    if (!dir.exists(file.path(user_dir, "autosave"))) dir.create(file.path(user_dir, "autosave"))

    ## do we have any saved preferences (options)?
    opts_file <- file.path(user_dir, "options.rds")
    saved_opts <- if (file.exists(opts_file)) readRDS(opts_file) else list()
    ## if we didn't provide options explicitly, use saved ones (if any) as priority
    if (missing(scouting_options)) {
        for (nm in names(saved_opts)) scouting_options[[nm]] <- saved_opts[[nm]]
    } else {
        ## use the provided options, but fill any missing from saved ones
        for (nm in names(saved_opts)) if (!nm %in% names(scouting_options)) scouting_options[[nm]] <- saved_opts[[nm]]
    }

    ## make sure any unspecified options are given their defaults
    opts <- ov_scouter_options()
    for (nm in names(scouting_options)) opts[[nm]] <- scouting_options[[nm]]
    scts <- ov_default_shortcuts()
    for (nm in names(shortcuts)) scts[[nm]] <- shortcuts[[nm]]

    if (!missing(dvw) && identical(dvw, "demo")) return(ov_scouter_demo(scoreboard = isTRUE(scoreboard), ball_path = isTRUE(ball_path), review_pane = isTRUE(review_pane), scouting_options = scouting_options, default_scouting_table = default_scouting_table, compound_table = compound_table, launch_browser = launch_browser, prompt_for_files = prompt_for_files, ...))
    assert_that(is.flag(launch_browser), !is.na(launch_browser))
    assert_that(is.flag(prompt_for_files), !is.na(prompt_for_files))
    assert_that(playlist_display_option %in% c("dv_codes", "commentary"))
    dots <- list(...)
    dv_read_args <- dots[names(dots) %in% names(formals(datavolley::dv_read))] ## passed to dv_read
    other_args <- dots[!names(dots) %in% names(formals(datavolley::dv_read))] ## passed to the server and UI
    if (missing(season_dir)) season_dir <- NULL
    if (missing(auto_save_dir)) auto_save_dir <- NULL
    if (!is.null(auto_save_dir) && !dir.exists(auto_save_dir)) stop("auto_save_dir does not exist")
    if ((missing(dvw) || is.null(dvw))) {
        if (prompt_for_files) {
            ## start with season directory
            if (is.null(season_dir)) season_dir <- tryCatch(dchoose(caption = "Choose season directory or cancel to skip"), error = function(e) NULL)
            dvw <- tryCatch({
                fchoose(caption = "Choose dvw file or cancel to skip", path = if (!is.null(season_dir) && dir.exists(season_dir)) season_dir else getwd())
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
        if (grepl("\\.dvw$", dvw, ignore.case = TRUE)) {
            dvw_filename <- dvw
            if (!"skill_evaluation_decode" %in% names(dv_read_args)) dv_read_args$skill_evaluation_decode <- "guess"
            dv_read_args$filename <- dvw
            dvw <- do.call(datavolley::dv_read, dv_read_args)
        } else if (grepl("\\.ovs$", dvw, ignore.case = TRUE)) {
            dvw_filename <- dvw
            dvw <- readRDS(dvw)
        } else {
            stop("unrecognized file format: ", dvw)
        }
    } else {
        if (!inherits(dvw, "datavolley")) stop("dvw should be a datavolley object or the path to a .dvw file")
        dvw_filename <- dvw$meta$filename
    }
    ## make sure we have an attack table, TODO add parm for the default to use here
    if (is.null(dvw$meta$attacks)) dvw$meta$attacks <- ov_simplified_attack_table()

    ## deal with video_file parm
    if (is.null(dvw$meta$video)) dvw$meta$video <- tibble(camera = character(), file = character())

    if (!missing(video_file) && !is.null(video_file) && !is.na(video_file) && nchar(video_file)) {
        if (is_youtube_id(video_file)) video_file <- paste0("https://www.youtube.com/watch?v=", video_file)
        if (is_url(video_file)) {
                dvw$meta$video <- tibble(camera = "Camera0", file = video_file)
        } else {
            tryCatch({
                dvw$meta$video <- tibble(camera = "Camera0", file = fs::path_real(video_file))
            }, error = function(e) stop("the provided video_file (", video_file, ") does not exist"))
        }
    }
    if (nrow(dvw$meta$video) > 1) {
        warning("multiple video files have been specified in the dvw file metadata, using only the first one")
        dvw$meta$video <- dvw$meta$video[1, ]
    }

    ## if the dvw file came from somebody/somewhere else, we might have the video but the absolute path in the dvw will be wrong
    if (length(dvw$meta$video$file) == 1 && !is_url(dvw$meta$video$file)) {
        chk <- dvw$meta$video$file
        if (!fs::file_exists(as.character(chk))) {
            ## can't find the file, go looking for it
            chk <- tryCatch(ovideo::ov_find_video_file(dvw_filename = dvw_filename, video_filename = chk), error = function(e) NA_character_)
            if (!is.na(chk)) dvw$meta$video$file <- chk
        }
    }

    ## has any of that resulted in a video file?
    if (!(nrow(dvw$meta$video) == 1 && (file.exists(dvw$meta$video$file) || is_url(dvw$meta$video$file)))) {
        if (prompt_for_files) {
            ## allow file chooser to find video file
            video_file <- fchoose(caption = "Choose video file", path = if (!missing(season_dir) && !is.null(season_dir) && dir.exists(season_dir)) season_dir else getwd())##, filters = matrix(c("dvw files (*.dvw)", "*.dvw", "All files (*.*)", "*.*"), nrow = 2, byrow = TRUE))
            if (length(video_file) == 1) dvw$meta$video <- tibble(camera = "Camera0", file = video_file)
        }
    }

    if (nrow(dvw$meta$video) < 1) {
        stop("no video files specified, either in the dvw file or via the video_file parameter")
    } else {
        if (!is_url(dvw$meta$video$file) && !file.exists(dvw$meta$video$file)) stop("specified video file (", dvw$meta$video$file, ") does not exist. Perhaps specify the local path via the video_file parameter?")
    }

    ## look for the court ref data, if it hasn't been provided
    if (missing(court_ref)) {
        court_ref <- NULL
        if (!is_url(dvw$meta$video$file)) {
            if (packageVersion("ovideo") >= "0.14.3") court_ref <- tryCatch(suppressWarnings(ovideo::ov_get_video_data(dvw$meta$video$file)), error = function(e) NULL)
            if (is.null(court_ref)) {
                crfile <- paste0(fs::path_ext_remove(dvw$meta$video$file), "_video_info.rds")
                if (file.exists(crfile)) tryCatch(court_ref <- readRDS(crfile), error = function(e) {
                    warning("found video_info.rds file but could not extract court_ref component")
                    NULL
                })
            }
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

    ## finally the shiny app
    app_data <- list(dvw_filename = dvw_filename, dvw = dvw, dv_read_args = dv_read_args, with_video = TRUE, video_src = dvw$meta$video$file, court_ref = court_ref, options = opts, options_file = opts_file, default_scouting_table = default_scouting_table, compound_table = compound_table, shortcuts = scts, ui_header = tags$div(), user_dir = user_dir, run_env = run_env, auto_save_dir = auto_save_dir)
    if ("video_file2" %in% names(other_args)) {
        video_file2 <- other_args$video_file2
        other_args$video_file2 <- NULL
    } else if (!is.null(dvw$video_file2)) {
        ## from a saved ods file
        video_file2 <- dvw$video_file2
    } else {
        video_file2 <- NULL
    }
    if (!is.null(video_file2)) {
        if (is_youtube_id(video_file2)) video_file2 <- paste0("https://www.youtube.com/watch?v=", video_file2)
        if (is_url(video_file2)) {
            ## do nothing
        } else if (!fs::file_exists(as.character(video_file2))) {
            ## can't find the file, go looking for it
            ## since this is the second video, we don't want to find a video file named the same as the dvw file (that is probably the first video) but the path to the video file might be wrong, it might be in wherever the dvw is
            fake_dvw_filename <- file.path(dirname(dvw_filename), "blahblahnotafile.dvw")
            video_file2 <- tryCatch(ovideo::ov_find_video_file(dvw_filename = fake_dvw_filename, video_filename = video_file2), error = function(e) NA_character_)
            if (!is.na(video_file2)) video_file2 <- NULL
        }
    }
    if ("video2_offset" %in% names(other_args)) {
        video2_offset <- other_args$video2_offset
        other_args$video2_offset <- NULL
    } else if (!is.null(dvw$video2_offset)) {
        ## from a saved ods file
        video2_offset <- dvw$video2_offset
    } else {
        video2_offset <- NULL
    }
    if ("court_ref2" %in% names(other_args)) {
        court_ref2 <- other_args$court_ref2
        other_args$court_ref2 <- NULL
    } else if (!is.null(dvw$court_ref2)) {
        ## from a saved ods file
        court_ref2 <- dvw$court_ref2
    } else {
        court_ref2 <- NULL
    }
    if (!is.null(video_file2) && nzchar(video_file2)) {
        if (is_youtube_id(video_file2)) video_file2 <- paste0("https://www.youtube.com/watch?v=", video_file2)
        app_data$video_src2 <- video_file2
        app_data$video2_offset <- video2_offset
        if (is.null(court_ref2)) {
            if (!is_url(video_file2)) {
                if (packageVersion("ovideo") >= "0.14.3") court_ref2 <- tryCatch(suppressWarnings(ovideo::ov_get_video_data(video_file2)), error = function(e) NULL)
                if (is.null(court_ref2)) {
                    crfile <- paste0(fs::path_ext_remove(video_file2), "_video_info.rds")
                    if (file.exists(crfile)) tryCatch(court_ref2 <- readRDS(crfile), error = function(e) {
                        warning("found video_info.rds file but could not extract court_ref component")
                        NULL
                    })
                }
            }
        }
        if (!is.null(court_ref2)) {
            if (is.data.frame(court_ref2) && any(c("image_x", "image_y", "court_x", "court_y") %in% names(court_ref2))) {
                ## just the court reference has been passed, not the full video info
                court_ref2 <- list(court_ref = court_ref2)
            }
            if (!is.data.frame(court_ref2$court_ref) || !all(c("image_x", "image_y", "court_x", "court_y") %in% names(court_ref2$court_ref))) {
                stop("court_ref2 is not of the expected format")
            }
            app_data$court_ref2 <- court_ref2
        }
    }
    app_data$serving <- "*" ## HACK for testing
    app_data$play_overlap <- 0.5 ## amount (in seconds) to rewind before restarting the video, after pausing to enter data
    app_data$evaluation_decoder <- skill_evaluation_decoder() ## to expose as a parameter, perhaps
    app_data$scoreboard <- isTRUE(scoreboard)
    app_data$ball_path <- isTRUE(ball_path)
    app_data$review_pane <- isTRUE(review_pane)
    app_data$playlist_display_option <- if (!missing(playlist_display_option)) playlist_display_option else 'dv_codes'
    app_data$season_dir <- if (!missing(season_dir) && !is.null(season_dir) && dir.exists(season_dir)) season_dir else NULL ## minimal check of the season_dir
    if (app_data$with_video && !is_url(dvw$meta$video$file)) {
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
            cat("server.document-root = \"", dirname(app_data$video_src), "\"\nserver.port = \"", video_server_port, "\"\n", sep = "", file = lighttpd_conf_file, append = FALSE)
            lighttpd_pid <- sys::exec_background("lighttpd", c("-D", "-f", lighttpd_conf_file), std_out = FALSE) ## start lighttpd not in background mode
            lighttpd_cleanup <- function() {
                message("cleaning up lighttpd")
                try(tools::pskill(lighttpd_pid), silent = TRUE)
            }
            onStop(function() try({ lighttpd_cleanup() }, silent = TRUE))
        } else {
            ## start servr instance serving from the video source directory
            blah <- servr::httd(dir = dirname(app_data$video_src), port = video_server_port, browser = FALSE, daemon = TRUE)
            onStop(function() {
                message("cleaning up servr")
                servr::daemon_stop()
            })
        }
        app_data$video_server_base_url <- paste0("http://localhost:", video_server_port)
        message(paste0("video server ", video_serve_method, " on port: ", video_server_port))
    } else {
        app_data$video_server_base_url <- ""
    }
    ## initialize the plays and plays2 components
    ## if we've started with an empty dvw and done dv_set_lineups, then we'll have an empty tibble for $plays but something in $plays2
    ## if we are continuing a partially-scouted file that has been reloaded from dvw, then we'll have something in plays but not plays2
    ## if we are restarting from an rds file, it should have something in plays2 but not plays
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
                             court_lines_colour = "#0000CC",
                             continue = "#10C424", continue_light = "#60FC71",
                             cancel = "#D41024", cancel_light = "#DF5463",
                             undo = "#EB6927", undo_light = "#F9AC50",
                             libero = "yellow", libero_light = "#FFFF70", libero_dark = "#FFCD4C",
                             setter = "grey90",
                             playslist_highlight = "orange")
    ## dir for reports
    if ("reports_dir" %in% names(other_args)) {
        if (!dir.exists(other_args$reports_dir)) stop("reports_dir does not exist")
    } else {
        app_data$reports_dir <- tempfile()
        dir.create(app_data$reports_dir)
    }

    app_data <- c(app_data, other_args)

    this_app <- list(ui = ov_scouter_ui(app_data = app_data), server = ov_scouter_server(app_data = app_data))
    shiny::addResourcePath("css", system.file("extdata/css", package = "ovscout2"))
    shiny::addResourcePath("js", system.file("extdata/js", package = "ovscout2"))
    shiny::addResourcePath("reports", app_data$reports_dir)
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser)
}

#' Scouting options
#'
#' @param end_convention string: either "actual" or "intended". Is the end coordinate of an attack or serve the actual end location (where the ball contacted the floor or out of bounds area), or the intended one. The actual might differ from the intended if there is a block touch or the ball hit the net. If "actual", and a block touch is recorded, then the end location of the attack will not be used for the dig location (the dig location will be missing)
#' @param nblockers logical: scout the number of blockers on each attack?
#' @param default_nblockers integer: if `nblockers` is TRUE, what number of blockers should we default to? If `NA`, no default
#' @param transition_sets logical: scout sets in transition? If `FALSE`, just the endpoint of each attack (i.e. the dig) and the subsequent counter-attack are scouted
#' @param attacks_by string: "codes" (X5, V5, etc) or "tempo" (high, medium, quick)
#' @param team_system string: the assumed system that teams are using to assign e.g. passing and hitting responsibilities
#' * "SHM3" - a setter-hitter-middle rotation, with 3 passers (the libero and two outside hitters)
#' @param setter_dump_code string: the attack combination code for a setter dump
#' @param second_ball_attack_code string: the attack combination code for a second-ball attack
#' @param overpass_attack_code string: the attack combination code for an attack on an overpass
#' @param scout_name string: the name of the scout (your name)
#' @param show_courtref logical: if `TRUE`, show the court reference lines overlaid on the video
#'
#' @return A named list
#'
#' @export
ov_scouter_options <- function(end_convention = "actual", nblockers = TRUE, default_nblockers = NA, transition_sets = FALSE, attacks_by = "codes", team_system = "SHM3", setter_dump_code = "PP", second_ball_attack_code = "P2", overpass_attack_code = "PR", scout_name = "", show_courtref = FALSE) {
    end_convention <- match.arg(end_convention, c("actual", "intended"))
    assert_that(is.flag(nblockers), !is.na(nblockers))
    if (is.null(default_nblockers)) default_nblockers <- NA
    assert_that(default_nblockers %in% c(NA, 1:4))
    assert_that(is.flag(transition_sets), !is.na(transition_sets))
    attacks_by <- match.arg(attacks_by, c("codes", "tempo"))
    team_system <- match.arg(team_system, c("SHM3"))
    assert_that(is.string(setter_dump_code))
    assert_that(is.string(second_ball_attack_code))
    assert_that(is.string(overpass_attack_code))
    assert_that(is.string(scout_name))
    assert_that(is.flag(show_courtref), !is.na(show_courtref))
    skill_tempo_map <- tribble(~skill, ~tempo_code, ~tempo,
                               "Serve", "Q", "Jump serve",
                               "Serve", "M", "Jump-float serve",
                               "Serve", "H", "Float serve",
                               "Serve", "T", "Topspin serve")
    ## or (some) beach conventions are T=jump-float, H=standing; VM use H=float far from the service line and T=float from the service line
    list(end_convention = end_convention, nblockers = nblockers, default_nblockers = default_nblockers, transition_sets = transition_sets, attacks_by = attacks_by, team_system = team_system, skill_tempo_map = skill_tempo_map, setter_dump_code = setter_dump_code, second_ball_attack_code = second_ball_attack_code, overpass_attack_code = overpass_attack_code, scout = scout_name, show_courtref = show_courtref)
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
## - propagate clicks to video itself? (controls)
## - have a popup with all buttons required to manually enter a skill code:
##   Team * a
##   Number blah
##   Skill S R etc but not including T C P etc
##   - and text boxes to enter T, C, P etc
## - player selection via the court inset, with players shown in their assumed playing locations ?
