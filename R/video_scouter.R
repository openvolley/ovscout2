#' Launch a Shiny app for scouting
#'
#' @details
#' A note on `scouting_options`
#'
#' If a *.ovs file (i.e. a partially-scouted file, that was previously scouted using this app) has been provided in the `dvw` argument, then it will contain the scouting options used during the previous scouting session. Those options will be re-used EXCEPT if `scouting_options` are also provided here. Any scouting options provided here via the `scouting_options` argument will override options saved in the .ovs file. Thus, it is recommended that `scouting_options` not be provided here along with a .ovs file unless absolutely necessary. If necessary, only the specific, relevant elements of the `scouting_options` list should be provided. Note that *.dvw files do not contain saved options, only .ovs files that were scouted with this app.
#'
#' @param dvw string or datavolley: either the path to a dvw or ovs file or a datavolley object (e.g. as returned by [dv_create()]. Passing the file name (not the datavolley object) is required if any extra arguments are passed via `...`. `dvw` can also be an object as saved by `ov_scouter()` in ovs format. If `dvw` is "demo", the app will be started with a demonstration data set
#' @param video_file string: optionally, the path to the video file. If not supplied (or `NULL`) the video file specified in the dvw file will be used. `video_file` can also be a URL (including a YouTube URL or video ID)
# @param video_file2 string: optionally, the file path or URL to a second video file (e.g. video from the opposite end of the court to `video_file`). If this is a local file, it must be in the same directory as `video_file`
#' @param court_ref data.frame or string: data.frame with the court reference (as returned by [ovideo::ov_shiny_court_ref()]) or the path to the rds file containing the output from this
# @param court_ref2 data.frame or string: data.frame with the court reference for `video_file2` (as returned by [ovideo::ov_shiny_court_ref()]) or the path to the rds file containing the output from this. Note that `court_ref2` must be defined in the same orientation as `court_ref` - for example, the corner of the court considered to be "far left" must be the same physical court corner in both court references
#' @param season_dir string: optional path to a directory with other dvw/ovs files from this season
#' @param auto_save_dir string: optional path to a directory where the dvw will be saved automatically after each rally
# TODO @param pantry_id string: optional ID for <https://getpantry.cloud>. The dvw will be saved automatically to your pantry after each rally. Your `pantry_id` can also be specified as the `PANTRY_ID` environment variable (i.e. use `Sys.setenv(PANTRY_ID = "xyz")` before launching `ov_scouter()`)
#' @param scoreboard logical: if `TRUE`, show a scoreboard in the top-right of the video pane
#' @param ball_path logical: if `TRUE`, show the ball path on the court inset diagram. Note that this will slow the app down slightly
#' @param playlist_display_option string: what to show in the plays table? Either "dv_codes" (scouted codes) or "commentary" (a plain-language interpretation of the touches)
#' @param review_pane logical: if `TRUE`, entry popups will be accompanied by a small video pane that shows a loop of the video of the action in question
#' @param playback_rate numeric: starting playback rate of the video (1.0 is normal speed, higher is faster)
#' @param shortcuts list: named list of keyboard shortcuts, as returned by [ov_default_shortcuts()]
#' @param scouting_options list: a named list with entries as per [ov_scouting_options()]. See Details, below
#' @param app_styling list: named list of styling options, as returned by [ov_app_styling()]
#' @param scout_name string: the name of the scout (your name)
#' @param show_courtref logical: if `TRUE`, show the court reference lines overlaid on the video
#' @param dash logical: support live MPEG DASH streams? If not specified, will default to `TRUE` if `video_file` is a `*.mpd` stream. Note that DASH support is fragile at best. HLS streams are automatically supported and likely to be more reliable
#' @param host string: the IP address of this machine. Only required if you intend to connect to the app from a different machine (in which case use `ov_scouter(..., host = "www.xxx.yyy.zzz", launch_browser = FALSE)`, where www.xxx.yyy.zzz is the IP address of this machine, i.e. the machine running the app)
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
ov_scouter <- function(dvw, video_file, court_ref, season_dir, auto_save_dir, scoreboard = TRUE, ball_path = FALSE, playlist_display_option = "dv_codes", review_pane = TRUE, playback_rate = 1.0, scouting_options = ov_scouting_options(), app_styling = ov_app_styling(), shortcuts = ov_default_shortcuts(), scout_name = "", show_courtref = FALSE, dash = FALSE, host, launch_browser = TRUE, prompt_for_files = interactive(), ...) {

    assert_that(is.string(scout_name))
    assert_that(is.flag(show_courtref), !is.na(show_courtref))

    ## user data directory
    ## are we running under shiny server, shiny (locally) or shiny (docker)?
    run_env <- if (file.exists("/.dockerenv") || (file.exists("/proc/1/cgroup") && isTRUE(any(grepl("docker", readLines("/proc/1/cgroup")))))) "shiny_docker" else if (nzchar(Sys.getenv("SHINY_PORT"))) "shiny_server" else "shiny_local"

    user_dir <- if (run_env %eq% "shiny_local") ovscout2_app_dir() else tempfile()
    if (!dir.exists(user_dir)) tryCatch(dir.create(user_dir, recursive = TRUE), error = function(e) {
        user_dir <- tempfile()
        dir.create(user_dir)
    })
    if (!dir.exists(file.path(user_dir, "autosave"))) dir.create(file.path(user_dir, "autosave"))

    ## do we have any saved preferences?
    ## these are app preferences, not scouting options
    opts_file <- file.path(user_dir, "options.rds")
    saved_opts <- if (file.exists(opts_file)) readRDS(opts_file) else list()
    #### if we didn't provide options explicitly, use saved ones (if any) as priority
    if (missing(scoreboard) && "scoreboard" %in% names(saved_opts)) scoreboard <- saved_opts$scoreboard
    if (missing(ball_path) && "ball_path" %in% names(saved_opts)) ball_path <- saved_opts$ball_path
    if (missing(review_pane) && "review_pane" %in% names(saved_opts)) review_pane <- saved_opts$review_pane
    if (missing(playlist_display_option) && "playlist_display_option" %in% names(saved_opts)) playlist_display_option <- saved_opts$playlist_display_option
    if (missing(scout_name) && "scout_name" %in% names(saved_opts)) scout_name <- saved_opts$scout_name
    if (missing(show_courtref) && "show_courtref" %in% names(saved_opts)) show_courtref <- saved_opts$show_courtref
    if (missing(playback_rate) && "playback_rate" %in% names(saved_opts)) playback_rate <- saved_opts$playback_rate

    if (!missing(dvw) && identical(dvw, "demo")) return(ov_scouter_demo(scoreboard = isTRUE(scoreboard), ball_path = isTRUE(ball_path), review_pane = isTRUE(review_pane), playback_rate = playback_rate, scouting_options = scouting_options, launch_browser = launch_browser, prompt_for_files = prompt_for_files, ...))
    assert_that(is.flag(launch_browser), !is.na(launch_browser))
    assert_that(is.flag(prompt_for_files), !is.na(prompt_for_files))
    assert_that(playlist_display_option %in% c("dv_codes", "commentary"))
    dots <- list(...)

    port <- NA
    if (missing(host) || is.null(host)) {
        host <- getOption("shiny.host", "127.0.0.1")
        port <- NULL
    }
    ##if (host %eq% "0.0.0.0") {
    ##    ## could guess it using getip::getip("local") but this isn't reliable e.g. on a machine with multiple network interfaces
    ##}
    if (!is.null(port)) {
        ## if host was specified, figure out port? then can print qr code for client connection
        port <- get_port(port = getOption("ovscout2.lastport"), host = host)
        options(ovscout2.lastport = port)
        if (!isTRUE(launch_browser)) {
            qrc <- qrcode::qr_code(paste0("http://", host, ":", port))
            plot(qrc) ## or print, but console-based image might not be recognized by qr scanner
        }
    }

    dv_read_args <- dots[names(dots) %in% names(formals(datavolley::dv_read))] ## passed to dv_read
    other_args <- dots[!names(dots) %in% names(formals(datavolley::dv_read))] ## passed to the server and UI
    if (missing(season_dir)) season_dir <- NULL
    if (missing(auto_save_dir)) auto_save_dir <- NULL
    if (!is.null(auto_save_dir) && !dir.exists(auto_save_dir)) stop("auto_save_dir does not exist")
    ## read the input file, if we've been given one
    if (!missing(dvw) && is.string(dvw)) {
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
    }
    ## ask for season directory
    ## but not if we've started from an ovs/dvw file that has a legit video path
    if (is.null(season_dir) && prompt_for_files) {
        if (!(!missing(dvw) && !is.null(dvw) && !is.null(dvw$meta$video) && nrow(dvw$meta$video) == 1 && (file.exists(dvw$meta$video$file) || is_url(dvw$meta$video$file)))) {
            season_dir <- tryCatch(dchoose(caption = "Choose season directory or cancel to skip", path = if (!missing(dvw) && !is.null(dvw) && dir.exists(fs::path_dir(fs::path(dvw)))) fs::path_dir(fs::path(dvw)) else getwd()), error = function(e) NULL)
        }
    }
    if ((missing(dvw) || is.null(dvw))) {
        if (prompt_for_files) {
            dvw <- tryCatch({
                fchoose(caption = "Choose dvw/ovs file or cancel to skip", path = if (!is.null(season_dir) && dir.exists(season_dir)) season_dir else getwd())
            }, error = function(e) NULL)
            if (!is.null(dvw) && (is.character(dvw) && all(!nzchar(dvw) | is.na(dvw)))) dvw <- NULL
            if (!is.null(dvw)) {
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
            }
        } else {
            dvw <- NULL
        }
    }
    if (is.null(dvw)) {
        ## default to an empty one
        suppressWarnings(dvw <- dv_create(teams = c("Home team", "Visiting team"))) ## don't warn about empty rosters
    } else {
        if (!inherits(dvw, "datavolley")) stop("dvw should be a datavolley object or the path to a .dvw or .ovs file")
    }
    dvw_filename <- dvw$meta$filename

    ## deal with video_file parm
    if (is.null(dvw$meta$video)) dvw$meta$video <- tibble(camera = character(), file = character())

    if (!missing(video_file) && !is.null(video_file) && !is.na(video_file) && nchar(video_file)) {
        if (is_youtube_id(video_file)) {
            video_file <- paste0("https://www.youtube.com/watch?v=", video_file)
        }
        if (is_url(video_file)) {
            dvw$meta$video <- tibble(camera = "Camera0", file = video_file)
        } else {
            tryCatch({
                dvw$meta$video <- tibble(camera = "Camera0", file = fs::path_real(video_file))
            }, error = function(e) stop("the provided video_file (", video_file, ") does not exist"))
        }
    }
    if (nrow(dvw$meta$video) > 1) {
        warning("multiple video files have been specified in the scout file metadata, using only the first one", immediate. = TRUE)
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
        stop("no video files specified, either in the scout file or via the video_file parameter")
    } else {
        if (!is_url(dvw$meta$video$file) && !file.exists(dvw$meta$video$file)) stop("specified video file (", dvw$meta$video$file, ") does not exist. Perhaps specify the local path via the video_file parameter?")
    }

    ## look for the court ref data, if it hasn't been provided
    if (missing(court_ref)) {
        court_ref <- NULL
        ## if it's an ovs file we might have saved the court ref into it
        if ("detection_refs" %in% names(dvw) && length(dvw$meta$video$file) > 0 && !is.null(dvw$detection_refs[[dvw$meta$video$file]])) {
            court_ref <- dvw$detection_refs[[dvw$meta$video$file]]
        } else if (!is_url(dvw$meta$video$file)) {
            ## court ref saved into the video file?
            if (packageVersion("ovideo") >= "0.14.3") court_ref <- tryCatch(suppressWarnings(ovideo::ov_get_video_data(dvw$meta$video$file)), error = function(e) NULL)
            if (is.null(court_ref)) {
                crfile <- paste0(fs::path_ext_remove(dvw$meta$video$file), "_video_info.rds")
                if (file.exists(crfile)) tryCatch(court_ref <- readRDS(crfile), error = function(e) {
                    warning("found video_info.rds file but could not extract court_ref component", immediate. = TRUE)
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

    ## sort out scouting options
    ## start with defaults
    opts <- ov_scouting_options()
    ## override with scouting options from the saved dvw, if it has them
    if (!is.null(dvw$scouting_options)) {
        for (nm in names(dvw$scouting_options)) opts[[nm]] <- dvw$scouting_options[[nm]]
    }
    ## and finally if the user has specifically provided scouting options, use those
    did_provide_s_opts <- FALSE
    if (!missing(scouting_options) && length(scouting_options) > 0) {
        did_provide_s_opts <- TRUE
        for (nm in names(scouting_options)) opts[[nm]] <- scouting_options[[nm]]
    }
    ## same with shortcuts
    scts <- ov_default_shortcuts()
    for (nm in names(shortcuts)) scts[[nm]] <- shortcuts[[nm]]

    ## attack_table in options overrides the one in the file. Also if we have no attack table, we need one
    if ((did_provide_s_opts && "attack_table" %in% names(scouting_options)) || is.null(dvw$meta$attacks)) dvw$meta$attacks <- opts$attack_table

    ## finally the shiny app
    app_data <- list(dvw_filename = dvw_filename, dvw = dvw, dv_read_args = dv_read_args, with_video = TRUE, video_src = dvw$meta$video$file, court_ref = court_ref, options = opts, options_file = opts_file, shortcuts = scts, ui_header = tags$div(), user_dir = user_dir, run_env = run_env, auto_save_dir = auto_save_dir, scout_name = scout_name, show_courtref = show_courtref)
    if ("video_file2" %in% names(other_args)) {
        video_file2 <- other_args$video_file2
        other_args$video_file2 <- NULL
    } else if (!is.null(dvw$video_file2)) {
        ## from a saved ovs file
        video_file2 <- dvw$video_file2
    } else {
        video_file2 <- NULL
    }
    if (!is.null(video_file2)) {
        if (is_youtube_id(video_file2)) {
            video_file2 <- paste0("https://www.youtube.com/watch?v=", video_file2)
        }
        if (is_url(video_file2)) {
            ## do nothing
        } else if (!fs::file_exists(as.character(video_file2))) {
            ## can't find the file, go looking for it
            ## since this is the second video, we don't want to find a video file named the same as the dvw file (that is probably the first video) but the path to the video file might be wrong, it might be in wherever the dvw is
            fake_dvw_filename <- file.path(fs::path_dir(fs::path(dvw_filename)), "blahblahnotafile.dvw")
            video_file2 <- tryCatch(ovideo::ov_find_video_file(dvw_filename = fake_dvw_filename, video_filename = video_file2), error = function(e) NA_character_)
            if (!is.na(video_file2)) video_file2 <- NULL
        }
    }
    if ("video2_offset" %in% names(other_args)) {
        video2_offset <- other_args$video2_offset
        other_args$video2_offset <- NULL
    } else if (!is.null(dvw$video2_offset)) {
        ## from a saved ovs file
        video2_offset <- dvw$video2_offset
    } else {
        video2_offset <- NULL
    }
    if ("court_ref2" %in% names(other_args)) {
        court_ref2 <- other_args$court_ref2
        other_args$court_ref2 <- NULL
    } else if ("detection_refs" %in% names(dvw) && length(video_file2) > 0 && !is.null(dvw$detection_refs[[video_file2]])) {
        ## if it's an ovs file we might have saved the court ref into it
        court_ref2 <- dvw$detection_refs[[video_file2]]
    } else {
        court_ref2 <- NULL
    }
    if (!is.null(video_file2) && nzchar(video_file2)) {
        if (is_youtube_id(video_file2)) {
            video_file2 <- paste0("https://www.youtube.com/watch?v=", video_file2)
        }
        app_data$video_src2 <- video_file2
        app_data$video2_offset <- video2_offset
        if (is.null(court_ref2)) {
            if (!is_url(video_file2)) {
                if (packageVersion("ovideo") >= "0.14.3") court_ref2 <- tryCatch(suppressWarnings(ovideo::ov_get_video_data(video_file2)), error = function(e) NULL)
                if (is.null(court_ref2)) {
                    crfile <- paste0(fs::path_ext_remove(video_file2), "_video_info.rds")
                    if (file.exists(crfile)) tryCatch(court_ref2 <- readRDS(crfile), error = function(e) {
                        warning("found video_info.rds file but could not extract court_ref component", immediate. = TRUE)
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

    ## check - if video URL(s) are remote, are we online?
    if ((is_remote_url(app_data$video_src) || is_remote_url(app_data$video_src2)) && !curl::has_internet()) {
        warning("video url(s) look like remote addresses, but you appear to be offline", immediate. = TRUE)
    }
    ## TODO check that both videos are consistent URL/file. Though does it matter?

    ## mpeg-dash support
    if (missing(dash) && (isTRUE(grepl("\\.mpd$", app_data$video_src, ignore.case = TRUE)) || isTRUE(grepl("\\.mpd$", app_data$video_src2, ignore.case = TRUE)))) dash <- TRUE
    app_data$dash <- isTRUE(dash)

    ## ball tracking, experimental!
    app_data$extra_db <- NULL
    app_data$extra_ball_tracking <- FALSE
    if (!is_url(app_data$video_src)) {
        look_for <- paste0(fs::path_ext_remove(app_data$video_src), "_extra.duckdb")
        if (file.exists(look_for)) {
            if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE) || !requireNamespace("dbplyr", quietly = TRUE)) {
                warning("The duckdb, dbplyr, and DBI packages are required in order to use ball tracking outputs", immediate. = TRUE)
            } else {
                con <- tryCatch(DBI::dbConnect(duckdb::duckdb(look_for)), error = function(e) NULL)
                if (!is.null(con)) {
                    app_data$extra_db <- look_for
                    app_data$extra_ball_tracking <- tryCatch("ball_track" %in% DBI::dbListTables(con), error = function(e) FALSE)
                    DBI::dbDisconnect(con, shutdown = TRUE)
                }
            }
        }
    }

    app_data$serving <- "*"
    app_data$play_overlap <- 0.5 ## amount (in seconds) to rewind before restarting the video, after pausing to enter data
    app_data$evaluation_decoder <- skill_evaluation_decoder() ## to expose as a parameter, perhaps
    app_data$scoreboard <- isTRUE(scoreboard)
    app_data$ball_path <- isTRUE(ball_path)
    app_data$review_pane <- isTRUE(review_pane)
    app_data$playback_rate <- playback_rate
    app_data$playlist_display_option <- if (!missing(playlist_display_option)) playlist_display_option else 'dv_codes'
    app_data$season_dir <- if (!missing(season_dir) && !is.null(season_dir) && dir.exists(season_dir)) season_dir else NULL ## minimal check of the season_dir
    if (app_data$with_video && !is_url(dvw$meta$video$file)) {
        video_server_port <- get_port(port_range = c(8000L, 12000L), host = host)
        lighttpd_exe <- ov_find_lighttpd()
        if (!is.null(lighttpd_exe)) {
            video_serve_method <- "lighttpd"
        } else {
            warning("could not find the lighttpd executable, install it with e.g. 'apt install lighttpd' on Ubuntu/Debian or from http://lighttpd.dtech.hu/ on Windows. Using \"servr\" video option, which might be less responsive", immediate. = TRUE)
            video_serve_method <- "servr"
        }
        if (video_serve_method == "lighttpd") {
            ## build config file to pass to lighttpd
            lighttpd_conf_file <- tempfile(fileext = ".conf")
            cat("server.document-root = \"", fs::path_dir(fs::path(app_data$video_src)), "\"\nserver.port = \"", video_server_port, "\"\nserver.modules = (\"mod_setenv\")\nsetenv.add-response-header = ( \"Access-Control-Allow-Origin\" => \"*\" )\n", sep = "", file = lighttpd_conf_file, append = FALSE)
            lighttpd_pid <- sys::exec_background(lighttpd_exe, c("-D", "-f", lighttpd_conf_file), std_out = FALSE) ## start lighttpd not in background mode
            lighttpd_cleanup <- function() {
                message("cleaning up lighttpd")
                try(tools::pskill(lighttpd_pid), silent = TRUE)
            }
            onStop(function() try({ lighttpd_cleanup() }, silent = TRUE))
        } else {
            ## start servr instance serving from the video source directory
            blah <- servr::httd(dir = fs::path_dir(fs::path(app_data$video_src)), port = video_server_port, browser = FALSE, daemon = TRUE)
            onStop(function() {
                message("cleaning up servr")
                servr::daemon_stop()
            })
        }
        app_data$video_server_base_url <- paste0("http://", host, ":", video_server_port)
        message(paste0("video server ", video_serve_method, " on port: ", video_server_port))
    } else {
        app_data$video_server_base_url <- ""
    }
    ## initialize the plays and plays2 components
    ## if we've started with an empty dvw and done dv_set_lineups, then we'll have an empty tibble for $plays but something in $plays2
    ## if we are continuing a partially-scouted file that has been reloaded from dvw, then we'll have something in plays but not plays2
    ## if we are restarting from an ovs file, it should have something in plays2 but not plays
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
    def_app_styling <- ov_app_styling()
    for (nm in names(def_app_styling)) {
        if (!nm %in% names(app_styling)) app_styling[[nm]] <- def_app_styling[[nm]]
    }
    app_data$styling <- app_styling
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
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser, host = host, port = port)
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
#' @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
#' @param compound_table tibble: the table of compound codes
#' @param attack_table tibble: table of attack codes (X5, V5, etc) as returned by [ov_default_attack_table()] or [ov_simplified_attack_table()]
#'
#' @return A named list
#'
#' @export
ov_scouting_options <- function(end_convention = "actual", nblockers = TRUE, default_nblockers = NA, transition_sets = FALSE, attacks_by = "codes", team_system = "SHM3", setter_dump_code = "PP", second_ball_attack_code = "P2", overpass_attack_code = "PR", default_scouting_table = ov_default_scouting_table(), compound_table = ov_default_compound_table(), attack_table = ov_simplified_attack_table()) {
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
    skill_tempo_map <- tribble(~skill, ~tempo_code, ~tempo,
                               "Serve", "Q", "Jump serve",
                               "Serve", "M", "Jump-float serve",
                               "Serve", "H", "Float serve",
                               "Serve", "T", "Topspin serve")
    ## or (some) beach conventions are T=jump-float, H=standing; VM use H=float far from the service line and T=float from the service line
    ## TODO check validity of compound_table
    if (!is.data.frame(attack_table) || !all(c("code", "description", "type", "set_type", "attacker_position", "start_coordinate") %in% names(attack_table)) || nrow(attack_table) < 1) {
        stop("attack_table does not appear to be valid. It should be a tibble or data.frame as returned by e.g. ov_default_attack_table()")
    }
    list(end_convention = end_convention, nblockers = nblockers, default_nblockers = default_nblockers, transition_sets = transition_sets, attacks_by = attacks_by, team_system = team_system, skill_tempo_map = skill_tempo_map, setter_dump_code = setter_dump_code, second_ball_attack_code = second_ball_attack_code, overpass_attack_code = overpass_attack_code, default_scouting_table = default_scouting_table, compound_table = compound_table, attack_table = attack_table)
}


#' Styling to apply to the app
#'
#' Colours can be either hex strings or names, but if using names they must be recognized both by R and as CSS colour names.
#' @details
#' * `review_pane_width` is expressed as a percentage of the browser window width
#'
#' @return A named list of styling parameters
#'
#' @export
ov_app_styling <- function() {
    list(h_court_colour = "#BFEFFF", ## lightblue1
         h_court_highlight_colour = "#43AFD3",
         v_court_colour = "#BCEE68", ## darkolivegreen2
         v_court_highlight_colour = "#5D8022",
         court_lines_colour = "#0000CC",
         continue_colour = "#10C424", continue_light_colour = "#60FC71",
         cancel_colour = "#D41024", cancel_light_colour = "#DF5463",
         undo_colour = "#EB6927", undo_light_colour = "#F9AC50",
         libero_colour = "yellow", libero_light_colour = "#FFFF70", libero_dark_colour = "#FFCD4C",
         playslist_highlight_colour = "orange",
         button_font_size = "11px",
         review_pane_width = 30)
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
