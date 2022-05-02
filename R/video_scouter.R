#' Launch a Shiny app for scouting
#'
#' @param dvw string or datavolley: either the path to a dvw file (which will be read by \code{\link[datavolley]{dv_read}}) or a datavolley object (e.g. as returned by [dv_create()]. Passing the file name (not the datavolley object) is required if any extra arguments are passed via \code{...}
#' @param video_file string: optionally, the path to the video file. If not supplied (or \code{NULL}) the video file specified in the dvw file will be used. Provide `video_file = NA` to run the app without a video file
#' @param court_ref data.frame or string: data.frame with the court reference (as returned by [ovideo::ov_shiny_court_ref()]) or the path to the rds file containing the output from this
#' @param scouting_options list: a named list with entries as per [ov_scouter_options()]
#' @param default_scouting_table tibble: the table of scouting defaults (skill type and evaluation)
#' @param launch_browser logical: if \code{TRUE}, launch the app in the system's default web browser (passed to \code{\link[shiny]{runApp}}'s \code{launch.browser} parameter)
#' @param prompt_for_files logical: if \code{dvw} was not specified, prompt the user to select the dvw file
#' @param ... : extra parameters passed to \code{\link[datavolley]{dv_read}} (if \code{dvw} is a provided as a string) and/or to the shiny server and UI functions
#'
#' @seealso \code{\link[datavolley]{dv_read}}
#'
#' @export
ov_scouter <- function(dvw, video_file, court_ref, scouting_options = ov_scouter_options(), default_scouting_table = ov_default_scouting_table(), launch_browser = TRUE, prompt_for_files = interactive(), ...) {
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
    app_data <- c(list(dvw_filename = dvw_filename, dvw = dvw, dv_read_args = dv_read_args, with_video = !is.na(video_file), court_ref = court_ref, options = opts, default_scouting_table = default_scouting_table, ui_header = tags$div()), other_args)
    app_data$serving <- "*" ## HACK for testing
    app_data$play_overlap <- 0.5 ## amount (in seconds) to rewind before restarting the video, after pausing to enter data
    app_data$evaluation_decoder <- skill_evaluation_decoder() ## to expose as a parameter, perhaps
    this_app <- list(ui = ov_scouter_ui(app_data = app_data), server = ov_scouter_server(app_data = app_data))
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser)
}

#' Scouting options
#'
#' @param nblockers logical: scout the number of blockers on each attack?
#' @param transition_sets logical: scout sets in transition? If `FALSE`, just the endpoint of each attack (i.e. the dig) and the subsequent counter-attack are scouted
#' @param team_system string: the assumed system that teams are using to assign passing responsibility
#' * "SHM3" - a setter-hitter-middle rotation, with 3 passers (the libero and two outside hitters)
#'
#' @return A named list
#'
#' @export
ov_scouter_options <- function(nblockers = TRUE, transition_sets = FALSE, team_system = "SHM3") {
    list(nblockers = nblockers, transition_sets = transition_sets, team_system = team_system)
}

ov_scouter_ui <- function(app_data) {
    ## some startup stuff
    running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
    if (app_data$with_video) {
        video_src <- app_data$dvw$meta$video$file[1]
        if (!fs::file_exists(as.character(video_src))) {
            ## can't find the file, go looking for it
            chk <- ovideo::ov_find_video_file(dvw_filename = app_data$dvw_filename, video_filename = video_src)
            if (!is.na(chk)) video_src <- chk
        }

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
        video_server_base_url <- paste0("http://localhost:", video_server_port)
        message(paste0("video server ", video_serve_method, " on port: ", video_server_port))
    }
    fluidPage(theme=if (running_locally) "spacelab.css" else "https://cdnjs.cloudflare.com/ajax/libs/bootswatch/3.3.7/spacelab/bootstrap.min.css",
              htmltools::findDependencies(shiny::selectizeInput("foo", "bar", choices = "a")), ## workaround for https://github.com/rstudio/shiny/issues/3125
              tags$script("Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) });"), ## handler for running js code directly
              rintrojs::introjsUI(),
              tags$head(tags$style("body{font-size:15px} .well{padding:15px;} .myhidden {display:none;} table {font-size: small;} h2, h3, h4 {font-weight: bold;} .shiny-notification { height: 100px; width: 400px; position:fixed; top: calc(50% - 50px); left: calc(50% - 200px); } .code_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:70%;} .sub_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:40%;} .lineup_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:40%;} .clet {color: red;} .iconbut { font-size: 150%; } #rallystate { position: absolute; font-size: large; color: yellow; margin-top: -50px; background-color: #0000C080; }"),
                        tags$style("#headerblock {border-radius:14px; padding:10px; margin-bottom:5px; min-height:120px; color:black; border: 1px solid #000766; background:#000766; background: linear-gradient(90deg, rgba(0,7,102,1) 0%, rgba(255,255,255,1) 65%, rgba(255,255,255,1) 100%);} #headerblock h1, #headerblock h2, #headerblock h3, #headerblock h4 {color:#fff;}"),
                        tags$style("#hroster {padding-left: 0px; padding-right: 0px; background-color: #bfefff; padding: 12px;} #vroster {padding-left: 0px; padding-right: 0px; background-color: #bcee68; padding: 12px;}"),
                        tags$style("#video_overlay, #video_overlay_img { -webkit-backface-visibility: hidden; -webkit-transform: translateZ(0); }"), ## stop chrome putting the overlay underneath the video
                        if (!is.null(app_data$css)) tags$style(app_data$css),
                        ##key press handling
                        tags$script("$(document).on('keypress', function (e) { var el = document.activeElement; var len = -1; if (typeof el.value != 'undefined') { len = el.value.length; }; Shiny.setInputValue('cmd', e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('keydown', function (e) { var el = document.activeElement; var len = -1; if (typeof el.value != 'undefined') { len = el.value.length; }; Shiny.setInputValue('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('keyup', function (e) { var el = document.activeElement; var len = -1; if (typeof el.value != 'undefined') { len = el.value.length; }; Shiny.setInputValue('controlkeyup', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('shiny:sessioninitialized',function() { Shiny.setInputValue('window_height', $(window).innerHeight()); Shiny.setInputValue('window_width', $(window).innerWidth()); });"),
                        tags$script("var rsztmr; $(window).resize(function() { clearTimeout(rsztmr); rsztmr = setTimeout(doneResizing, 500); }); function doneResizing() { Shiny.setInputValue('window_height', $(window).innerHeight()); Shiny.setInputValue('window_width', $(window).innerWidth()); }"),
                        if (app_data$with_video) tags$script(HTML("var vo_rsztmr;
$(document).on('shiny:sessioninitialized', function() {
    Shiny.setInputValue('dv_height', $('#main_video').innerHeight()); Shiny.setInputValue('dv_width', $('#main_video').innerWidth()); Shiny.setInputValue('vo_voffset', $('#video_holder').innerHeight());
    $(window).resize(function() {
      clearTimeout(vo_rsztmr);
      vo_rsztmr = setTimeout(vo_doneResizing, 500); });
    function vo_doneResizing() {
      Shiny.setInputValue('dv_height', $('#main_video').innerHeight()); Shiny.setInputValue('dv_width', $('#main_video').innerWidth()); Shiny.setInputValue('vo_voffset', $('#video_holder').innerHeight());
    }
});
function dvjs_video_onstart() { Shiny.setInputValue('dv_height', $('#main_video').innerHeight()); Shiny.setInputValue('dv_width', $('#main_video').innerWidth()); Shiny.setInputValue('vo_voffset', $('#video_holder').innerHeight()); }")),
                        tags$title("Volleyball scout and video sync")
                        ),
              if (!is.null(app_data$ui_header)) {
                  app_data$ui_header
              } else {
                  fluidRow(id = "headerblock", column(6, tags$h2("Volleyball scout")),
                           column(3, offset = 3, tags$div(style = "text-align: center;", "Part of the", tags$br(), tags$img(src = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiMwMDA3NjYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwNzY2IiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+", style = "max-height:3em;"), tags$br(), tags$a(href = "https://github.com/openvolley", "openvolley", target = "_blank"), "project")))
              },
              fluidRow(column(9,
                              tags$div(#tags$button(tags$span(icon("step-backward", style = "vertical-align:middle;")), onclick = paste0(cstr, "video_prev();"), title = "Previous"),
                                       #tags$button(tags$span(icon("step-forward", style = "vertical-align:middle;")), onclick = paste0(cstr, "video_next();"), title = "Next"),
                                       tags$button(tags$span(icon("pause-circle", style = "vertical-align:middle;")), onclick = "if (document.getElementById('main_video').paused == true) { document.getElementById('main_video').play(); } else { document.getElementById('main_video').pause(); }", title = "Pause"),
                                       #tags$button(tags$span(icon("stop-circle", style = "vertical-align:middle;")), onclick = paste0(cstr, "video_stop();"), title = "Stop"),
                                       ),
                              if (app_data$with_video) introBox(tags$div(id = "video_holder", style = "position:relative;", tags$video(id = "main_video", style = "border: 1px solid black; width: 90%;", src = file.path(video_server_base_url, basename(video_src)), autoplay = "false")), tags$img(id = "video_overlay_img", style = "position:absolute;"), plotOutput("video_overlay", click = "video_click", dblclick = "video_dblclick"), data.step = 4, data.intro = "Video of the game to scout."), ##controls = "controls",
                              fluidRow(column(4, offset = 8, uiOutput("rally_state"))),
                              fluidRow(column(12, uiOutput("serve_preselect"))),
                              fluidRow(column(8,
                                              ## some elements commented out for now - BR
                                              introBox(##actionButton("all_video_from_clock", label = "Open video/clock time operations menu", icon = icon("clock")),
                                              actionButton("edit_match_data_button", "Edit match data", icon = icon("volleyball-ball")),
                                              actionButton("edit_teams_button", "Edit teams", icon = icon("users")),
                                              actionButton("edit_lineup_button", "Edit lineups", icon = icon("arrows-alt-h")), data.step = 3, data.intro = "Click on these action buttons if you want to edit the starting lineups, edit the rosters, or edit the match metadata."),
                                              uiOutput("save_file_ui", inline = TRUE)
                                              )),
                              tags$div(style = "height: 14px;"),
                              fluidRow(column(5, actionButton("general_help", label = "General Help", icon = icon("question"), style="color: #fff; background-color: #B21212; border-color: #B21212"),
                                              actionButton("show_shortcuts", tags$span(icon("keyboard"), "Show keyboard shortcuts"), style="color: #fff; background-color: #B21212; border-color: #B21212"),
                                              sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1),
                                              uiOutput("show_overlay_ui")
                                              ),
                                       column(7) ## WAS court rotation plot and team rosters
                                       )
                              ),
                       column(3,
                              introBox(DT::dataTableOutput("playslist", width = "98%"), data.step = 1, data.intro = "List of events. Existing events can be edited or deleted. New events can be added. They will appear here."),
                              introBox(wellPanel(mod_courtrot2_ui(id = "courtrot", with_ball_coords = FALSE)), data.step = 2, data.intro = "Team rosters and oncourt rotation."),
                              uiOutput("error_message"))
                       )
              )
}

ov_scouter_server <- function(app_data) {
    function(input, output, session) {
        debug <- 0L

        styling <- list(h_court_colour = "#bfefff", ## lightblue1
                        h_court_highlight = "darkblue",
                        v_court_colour = "#bcee68", ## darkolivegreen2
                        v_court_highlight = "darkgreen")

        plays_cols_to_show <- c("error_icon", "video_time", "set_number", "code", "home_setter_position", "visiting_setter_position", "Score", "is_skill")
        plays_col_renames <- c(Set = "set_number", hs = "home_setter_position", as = "visiting_setter_position")
        is_skill <- function(z) !is.na(z) & (!z %in% c("Timeout", "Technical timeout", "Substitution"))
        reactive_scrolling <- FALSE ## testing, not sure it helps. In principle if multiple scroll requests get lined up before the first has actually been initiated, then it'll skip to just the last

        ## this is temporary stupidity
        app_data$dvw$plays <- plays2_to_plays(app_data$dvw$plays2, dvw = app_data$dvw, evaluation_decoder = app_data$evaluation_decoder)

        if (is.null(app_data$dvw$meta$match$regulation)) stop("dvw does not have regulation information")
        app_data$is_beach <- is_beach(app_data$dvw)

        atbl <- app_data$dvw$meta$attacks
        atbl <- bind_cols(atbl[, setdiff(names(atbl), c("start_x", "start_y"))], setNames(dv_index2xy(atbl$start_coordinate), c("start_x", "start_y")))
        app_data$dvw$meta$attacks <- atbl

        rdata <- reactiveValues(dvw = app_data$dvw)

        pseq <- if (app_data$is_beach) 1:2 else 1:6

        ## court inset showing rotation and team lists
        court_inset <- callModule(mod_courtrot2, id = "courtrot", rdata = rdata, game_state = reactive(game_state), styling = styling, with_ball_coords = FALSE)
        rotateTeams <- reactive(court_inset$rt)
        accept_ball_coords <- court_inset$accept_ball_coords ## the "accept" button

        observe({
            if (nrow(court_inset$click_points$queue) > 1) {## && !is.null(playslist_current_row()) && !is.na(playslist_current_row())) {
                js_show2("courtrot-validate_ball_coords")
                js_show2("courtrot-cancel_ball_coords")
            } else {
                js_hide2("courtrot-validate_ball_coords")
                js_hide2("courtrot-cancel_ball_coords")
            }
        })

        observeEvent(accept_ball_coords(), {
            if (accept_ball_coords() > 0) { ## ignore the initial triggering of this on app startup
                warning("ball coords not implemented here yet")
            }
            ## and clear the clicked coordinates queue
            court_inset$clear_click_queue()
        })

        plays_do_rename <- function(z) names_first_to_capital(dplyr::rename(z, plays_col_renames))
        ## the plays display in the RHS table
        output$playslist <- DT::renderDataTable({
            isolate(mydat <- rdata$dvw$plays) ## render once, then isolate from further renders - will be done by replaceData below
            if (!is.null(input$window_height) && !is.na(input$window_height)) {
                plh <- input$window_height*0.4
            } else {
                plh <- 200
            }
            if (!is.null(mydat)) {
                isolate({
                    last_skill_row <- which(is_skill(mydat$skill))
                    if (length(last_skill_row)) last_skill_row <- max(last_skill_row)
                    sel <- list(mode = "single")
                    if (length(last_skill_row) > 0) {
                        sel$target <- "row"
                        sel$selected <- last_skill_row
                    }
                })
                mydat$is_skill <- is_skill(mydat$skill)
                mydat$set_number <- as.factor(mydat$set_number)
                mydat$Score <- paste(mydat$home_team_score, mydat$visiting_team_score, sep = "-")
                cols_to_hide <- which(plays_cols_to_show %in% c("is_skill")) - 1L ## 0-based because no row names
                cnames <- names(plays_do_rename(mydat[1, plays_cols_to_show, drop = FALSE]))
                cnames[plays_cols_to_show == "error_icon"] <- ""
                out <- DT::datatable(mydat[, plays_cols_to_show, drop = FALSE], rownames = FALSE, colnames = cnames,
                                     extensions = "Scroller",
                                     escape = FALSE, ##filter = "top",
                                     selection = sel, options = list(scroller = TRUE,
                                                                     lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = TRUE, "scrollY" = paste0(plh, "px"), ordering = FALSE, ##autoWidth = TRUE,
                                                                     columnDefs = list(list(targets = cols_to_hide, visible = FALSE)),
                                                                     drawCallback = DT::JS("function(settings) { Shiny.setInputValue('playlist_redrawn', new Date().getTime()); }")
                                                                     ##list(targets = 0, width = "20px")) ## does nothing
                                                                     ))
                out <- DT::formatStyle(out, "is_skill", target = "row", backgroundColor = DT::styleEqual(c(FALSE, TRUE), c("#f0f0e0", "lightgreen"))) ## colour skill rows green
                out <- DT::formatStyle(out, "error_icon", color = "red")
                out
            } else {
                NULL
            }
        }, server = TRUE)
        playslist_proxy <- DT::dataTableProxy("playslist")
        playslist_needs_scroll <- reactiveVal(FALSE)
        playslist_scroll_target <- reactiveVal(-99L)
        observeEvent(input$playlist_redrawn, {
            ## when the table has finished being drawn, scroll it if necessary
            if (playslist_needs_scroll()) {
                playslist_needs_scroll(FALSE)
                if (reactive_scrolling) playslist_scroll_target(playslist_current_row()) else scroll_playlist(playslist_current_row())
            }
            ## and mark current row as selected in the table, but don't re-scroll to it
            playslist_select_row(playslist_current_row(), scroll = FALSE)
        })
        ## keep track of selected playslist row as a reactiveVal
        ##   when updating e.g. video time, set this reactiveVal, then wait for DT to redraw THEN scroll
        playslist_current_row <- reactiveVal(NULL)
        ## the playslist_select_row function just changes the visible selection in the table, and optionally scrolls to it, but does not change playslist_current_row() value
        playslist_select_row <- function(rw, scroll = TRUE) {
            DT::selectRows(playslist_proxy, rw)
            if (isTRUE(scroll)) {
                if (reactive_scrolling) playslist_scroll_target(rw) else scroll_playlist(rw)
            }
        }
        ## when the user changes the selected row, update playslist_current_row
        observeEvent(input$playslist_rows_selected, playslist_current_row(input$playslist_rows_selected))

        observe({
            if (reactive_scrolling && !is.null(playslist_scroll_target()) && !is.na(playslist_scroll_target()) && playslist_scroll_target() > 0) {
                scroll_playlist(playslist_scroll_target())
            }
        })

        scroll_playlist <- function(rw) {
            if (!is.null(rw)) {
                ## scrolling works on the VISIBLE row index, so it depends on any column filters that might have been applied
                visible_rowidx <- which(input$playslist_rows_all == rw)
                scrollto <- max(visible_rowidx-1-5, 0) ## -1 for zero indexing, -5 to keep the selected row 5 from the top
                dojs(paste0("$('#playslist').find('.dataTable').DataTable().scroller.toPosition(", scrollto, ", false);")) ## no anim, faster
            }
        }

        observe({
            ## replace playlist data when dvw$plays changes
            if (!is.null(rdata$dvw$plays) && nrow(rdata$dvw$plays) > 0) replace_playlist_data()
        })
        replace_playlist_data <- function() {
            mydat <- rdata$dvw$plays
            mydat$is_skill <- is_skill(mydat$skill)
            mydat$set_number <- as.factor(mydat$set_number)
            mydat$Score <- paste(mydat$home_team_score, mydat$visiting_team_score, sep = "-")
            DT::replaceData(playslist_proxy, data = mydat[, plays_cols_to_show, drop = FALSE], rownames = FALSE, clearSelection = "none")
        }

        video_state <- reactiveValues(paused = FALSE)
        ## height of the video player element
        vo_height <- reactiveVal("auto")
        observe({
            if (!is.null(input$dv_height) && as.numeric(input$dv_height) > 0) {
                this <- as.numeric(input$dv_height)
                vo_height(this)
                dojs(paste0("document.getElementById('video_overlay').style.height = '", this, "px';"))
                dojs(paste0("document.getElementById('video_overlay_img').style.height = '", this, "px';"))
            } else {
                vo_height("auto")
                dojs(paste0("document.getElementById('video_overlay').style.height = '400px';"))
                dojs(paste0("document.getElementById('video_overlay_img').style.height = '400px';"))
            }
        })
        vo_width <- reactiveVal("auto")
        observe({
            if (!is.null(input$dv_width) && as.numeric(input$dv_width) > 0) {
                this <- as.numeric(input$dv_width)
                vo_width(this)
                dojs(paste0("document.getElementById('video_overlay').style.width = '", this, "px';"))
                dojs(paste0("document.getElementById('video_overlay_img').style.width = '", this, "px';"))
            } else {
                vo_width("auto")
                dojs(paste0("document.getElementById('video_overlay').style.width = '600px';"))
                dojs(paste0("document.getElementById('video_overlay_img').style.width = '600px';"))
            }
        })
        ## height of the video player container, use as negative vertical offset on the overlay element
        observe({
            if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) {
                ##dojs(paste0("document.getElementById('rallystate').style.marginTop = '-", input$vo_voffset - 50, "px';"))
                dojs(paste0("document.getElementById('video_overlay').style.marginTop = '-", input$vo_voffset, "px';"))
                dojs(paste0("document.getElementById('video_overlay_img').style.marginTop = '-", input$vo_voffset, "px';"))
            } else {
                ##dojs("document.getElementById('rallystate').style.marginTop = '-50px';")
                dojs("document.getElementById('video_overlay').style.marginTop = '0px';")
                dojs("document.getElementById('video_overlay_img').style.marginTop = '0px';")
            }
        })

        ## video functions
        do_video <- function(what, ..., id = "main_video") {
            getel <- paste0("document.getElementById('", id, "')")
            myargs <- list(...)
            if (what == "pause") {
                if (video_state$paused) {
                    dojs(paste0(getel, ".play();"))
                    video_state$paused <- FALSE
                } else {
                    dojs(paste0(getel, ".pause();"))
                    video_state$paused <- TRUE
                }
                NULL
            } else if (what == "play") {
                dojs(paste0(getel, ".play();"))
                video_state$paused <- FALSE
            } else if (what == "toggle_pause") {
                dojs(paste0("if (", getel, ".paused == true) { ", getel, ".play(); } else { ", getel, ".pause(); }"))
            } else if (what == "get_time") {
                dojs(paste0("Shiny.setInputValue('video_time', ", getel, ".currentTime)"))
            } else if (what == "get_time_fid") {
                dojs(paste0("Shiny.setInputValue('video_time', ", getel, ".currentTime + '&", myargs[[1]], "')"))
            } else if (what == "set_time") {
                dojs(paste0(getel, ".currentTime='", myargs[[1]], "';"))
            } else if (what == "set_current_video_time") {
                dojs(paste0("Shiny.setInputValue('set_current_video_time', ", getel, ".currentTime + '&", myargs[1], "&' + new Date().getTime())"))
            } else if (what == "tag_current_video_time") {
                dojs(paste0("Shiny.setInputValue('tag_current_video_time', ", getel, ".currentTime + '&", myargs[1], "')"))
            } else if (what == "rew") {
                dojs(paste0(getel, ".currentTime=", getel, ".currentTime - ", myargs[[1]], ";"))
            } else if (what == "ff") {
                dojs(paste0(getel, ".currentTime=", getel, ".currentTime + ", myargs[[1]], ";"))
            } else if (what == "playback_rate") {
                dojs(paste0(getel, ".playbackRate=", myargs[[1]], ";"))
            } else {
                NULL
            }
        }

        observeEvent(input$controlkey, {
            ## keys that might not get detected by keypress but do by keydown?
            if (!is.null(input$controlkey)) {
                temp <- strsplit(input$controlkey, "@")[[1]]
                ## elements are modifiers_and_key element_class element_id cursor_position field_length time
                mycmd <- temp[1]
                myclass <- temp[2]
                myid <- temp[3]
                if (!is.null(myclass) && nzchar(myclass) && myclass %in% c("form-control")) {
                    ## don't process these - they are e.g. key events in DT filter boxes
                    mycmd <- NULL
                }
                if (!is.null(mycmd)) {
                    if (debug > 1) cat("control key: ", mycmd, "\n")
                    mycmd <- strsplit(mycmd, "|", fixed = TRUE)[[1]] ## e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which
                    if (length(mycmd) == 5) {
                        ky <- mycmd[5] ## key pressed, as ASCII code
                        if (debug > 1) cat("key: ", ky, "\n")
                        if (ky %in% c(90, 122)) {
                            ## z
                            ## temporarily hide the modal, so the video can be seen
                            dojs("$('#shiny-modal-wrapper').hide(); $('.modal-backdrop').hide();")
                        } else if (ky %in% utf8ToInt("qQ0")) { ## video navigation
                            do_video("toggle_pause")
                        } else if (ky %in% utf8ToInt("nm13jhl;46$^b,79")) {
                            ## video forward/backward nav
                            ## same as for other ovscout interface, although the fine control is not needed here?
                            vidcmd <- if (ky %in% utf8ToInt("1nhj4$b7")) "rew" else "ff"
                            dur <- if (ky %in% utf8ToInt("h$;^")) 10 else if (ky %in% utf8ToInt("nm13")) 0.1 else if (ky %in% utf8ToInt("b7,9")) 1/30 else 2
                            do_video(vidcmd, dur)
                        }
                    }
                }
            }
        })
        observeEvent(input$controlkeyup, {
            ## keys that might not get detected by keypress but do by keydown?
            if (!is.null(input$controlkey)) {
                temp <- strsplit(input$controlkey, "@")[[1]]
                ## elements are modifiers_and_key element_class element_id cursor_position field_length time
                mycmd <- temp[1]
                myclass <- temp[2]
                myid <- temp[3]
                if (!is.null(myclass) && nzchar(myclass) && myclass %in% c("form-control")) {
                    ## don't process these - they are e.g. key events in DT filter boxes
                    mycmd <- NULL
                }
                if (!is.null(mycmd)) {
                    if (debug > 1) cat("control key up: ", mycmd, "\n")
                    mycmd <- strsplit(mycmd, "|", fixed = TRUE)[[1]] ## e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which
                    if (length(mycmd) == 5) {
                        ky <- mycmd[5]
                        if (ky %in% c("90", "122")) {
                            ## z
                            ## temporarily hide the modal, so the video can be seen
                            dojs("$('#shiny-modal-wrapper').show(); $('.modal-backdrop').show();")
                        }
                    }
                }
            }
        })


        gg_tight <- list(theme(legend.position = "none", panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0, "null"), plot.margin = rep(unit(0, "null"), 4), axis.ticks = element_blank(), axis.ticks.length = unit(0, "null"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()), scale_x_continuous(limits = c(0, 1), expand = c(0, 0)), scale_y_continuous(limits = c(0, 1), expand = c(0, 0)))
        overlay_points <- reactiveVal(NULL)
        observe({
            output$video_overlay <- renderPlot({a
                ## test - red diagonal line across the overlay plot
                ##ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes_string("x", "y")) + geom_path(color = "red") + gg_tight
                ## need to plot SOMETHING else we don't get correct coordinates back
                ##this <- selected_event()
                p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes_string("x", "y")) + gg_tight
                if (!is.null(overlay_points()) && nrow(overlay_points()) > 0) {
                    ixy <- setNames(crt_to_vid(overlay_points()), c("x", "y"))
                    p <- p + geom_point(data = ixy, fill = "dodgerblue", pch = 21, col = "white", size = 6)
                }
                p
            }, bg = "transparent", width = vo_width(), height = vo_height())
        })
        vid_to_crt <- function(obj) {
            courtxy <- data.frame(x = rep(NA_real_, length(obj$x)), y = rep(NA_real_, length(obj$x)))
            if (!is.null(app_data$court_ref)) {
                ##vxy <- c(obj$x, obj$y)
                ##if (length(vxy) == 2 && !any(is.na(vxy))) {
                ##    courtxy <- ovideo::ov_transform_points(vxy[1], vxy[2], ref = app_data$court_ref, direction = "to_court")
                ##}
                if (length(obj$x) > 0) courtxy <- ovideo::ov_transform_points(obj$x, obj$y, ref = app_data$court_ref, direction = "to_court")
            }
            courtxy
        }
        crt_to_vid <- function(obj) {
            imagexy <- data.frame(image_x = rep(NA_real_, length(obj$x)), image_y = rep(NA_real_, length(obj$x)))
            if (!is.null(app_data$court_ref)) {
                ##vxy <- cbind(obj$x, obj$y)
                ##if (length(vxy) == 2 && !any(is.na(vxy))) {
                ##    imagexy <- setNames(ovideo::ov_transform_points(vxy[1], vxy[2], ref = app_data$court_ref, direction = "to_image"), c("image_x", "image_y"))
                ##}
                if (length(obj$x) > 0) imagexy <- setNames(ovideo::ov_transform_points(obj$x, obj$y, ref = app_data$court_ref, direction = "to_image"), c("image_x", "image_y"))
            }
            imagexy
        }
        flash_screen <- function() dojs("$('#video_overlay_img').css('background-color', '#FFFF0080'); setTimeout(function() { $('#video_overlay_img').css('background-color', ''); }, 50);")

        loop_trigger <- reactiveVal(0L)
        observeEvent(input$video_click, {
            ## when video clicked, get the corresponding video time and trigger the loop
            flash_screen()
            time_uuid <- uuid()
            game_state$current_time_uuid <- time_uuid
            do_video("get_time_fid", time_uuid) ## make asynchronous request
            loop_trigger(loop_trigger() + 1L)
            ## TODO MAYBE also propagate the click to elements below the overlay?
        })

        ## video times are a pain, because we get asynchronous replies from the browser via input$video_time
        video_times <- list()
        observeEvent(input$video_time, {
            ## when a time comes in, stash it under its uuid
            temp <- input$video_time
            this_uuid <- sub(".*&", "", temp)
            if (nzchar(this_uuid)) video_times[[this_uuid]] <<- as.numeric(sub("&.+", "", temp))
        })
        retrieve_video_time <- function(id) {
            if (is_uuid(id)) {
                ##cat("looking for time with uuid: ", id, "\n")
                ##cat("available entry uuids: ", paste(names(video_times), collapse = " "), "\n")
                if (nzchar(id) && id %in% names(video_times)) video_times[[id]] else NA_real_
            } else {
                id
            }
        }

        ## rally_codes is a reactive that returns a tibble with columns team, pnum, skill, tempo, eval, combo, target, sz, ez, esz, x_type, num_p, special, custom, t, start_x, start_y, end_x, end_y
        ## rally_codes are the actions in the current rally

        code_trow <- function(team, pnum = 0L, skill, tempo, eval, combo = "~~", target = "~", sz = "~", ez = "~", esz = "~", x_type = "~", num_p = "~", special = "~", custom = "", t = NA_real_, start_x = NA_real_, start_y = NA_real_, end_x = NA_real_, end_y = NA_real_) {
            ## abbreviated parameer names here to make code more concise: pnum = player number, eval = evaluation code, sz = start zone, ez = end zone, esz = end subzone, x_type = extended skill type code, num_p = extended num players code, special = extended special code
            if (missing(tempo)) tempo <- tryCatch(app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == skill], error = function(e) "~")
            if (missing(eval)) eval <- tryCatch(app_data$default_scouting_table$evaluation_code[app_data$default_scouting_table$skill == skill], error = function(e) "~")
            if (nchar(esz) == 2) ez <- ""
            ##tibble(code = sub("~+$", "", paste0(team, zpn(pnum), skill, tempo, eval, combo, target, sz, ez, esz, x_type, num_p, special, custom)), t = t, start_x = start_x, start_y = start_y, end_x = end_x, end_y = end_y)
            if (is.null(pnum) || is.na(pnum) || pnum %eq% "Unknown") pnum <- 0L
            as_tibble(c(lapply(list(team = team, pnum = zpn(pnum), skill = skill, tempo = tempo, eval = eval, combo = combo, target = target, sz = sz, ez = ez, esz = esz, x_type = x_type, num_p = num_p, special = special, custom = custom), as.character), list(t = t, start_x = start_x, start_y = start_y, end_x = end_x, end_y = end_y)))
        }

        ## single click the video to register a tag location, or starting ball coordinates
        observeEvent(loop_trigger(), {
            if (loop_trigger() > 0) {
                courtxy <- vid_to_crt(input$video_click)
                ##court_inset$add_to_click_queue(courtxy)
                if (rally_state() == "click the video to start") {
                    do_video("play")
                    rally_state("click serve start")
                } else if (rally_state() == "click serve start") {
                    ## click was the serve position
                    game_state$start_x <- courtxy$x[1]
                    game_state$start_y <- courtxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy)
                    ## add placeholder serve code, will get updated on next click
                    ## serving player TODO also allow pre-input (check) of this
                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    ## serve type should have been selected in the preselect
                    st <- if (!is.null(input$serve_preselect_type)) input$serve_preselect_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    ## time probably won't have resolved yet, so add it after next click
                    rally_codes(code_trow(team = game_state$serving, pnum = sp, skill = "S", tempo = st, sz = sz, start_x = game_state$start_x, start_y = game_state$start_y))
                    rally_state("click serve end")
                } else if (rally_state() == "click serve end") {
                    do_video("pause")
                    ## click was the end-of-serve position, either error or reception
                    game_state$end_x <- courtxy$x[1]
                    game_state$end_y <- courtxy$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    overlay_points(rbind(overlay_points(), courtxy))
                    ## pop up to find either serve error, or passing player
                    ## passing player options
                    pass_pl_opts <- guess_pass_player_options(game_state, dvw = rdata$dvw, system = app_data$options$team_system)
                    names(pass_pl_opts$choices) <- player_nums_to(pass_pl_opts$choices, team = other(game_state$current_team), dvw = rdata$dvw)
                    pass_pl_opts$choices <- c(pass_pl_opts$choices, Unknown = "Unknown")

                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    serve_type_buttons <- make_fat_radio_buttons(choices = c(Jump = "Q", "Jump-float" = "M", Float = "H", Topspin = "T"),
                                                                 selected = input$serve_preselect_type,
                                                                 input_var = "serve_type", style = "width:100%; height:7vh;")
                    passer_buttons <- make_fat_radio_buttons(choices = pass_pl_opts$choices, selected = pass_pl_opts$selected, input_var = "select_passer", style = "width:100%; height:7vh;")
                    serve_error_buttons <- make_fat_buttons(choices = c("Serve error" = "=", "Serve error (in net)" = "=N", "Serve error (foot fault)" = "=Z", "Serve error (long)" = "=O", "Serve error (out left)" = "=L", "Serve error (out right)" = "=R"), input_var = "was_serve_error", style = "width:100%; height:7vh;")
                    ##browser()
                    showModal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Serve type:")),
                                            do.call(fixedRow, lapply(serve_type_buttons$buttons, function(but) column(2, but))),
                                            tags$hr(),
                                            tags$div("AND"),
                                            tags$br(),
                                            do.call(fixedRow, lapply(serve_error_buttons$buttons, function(but) column(2, but))),
                                            tags$br(),
                                            tags$div("OR"),
                                            tags$br(),
                                            tags$p(tags$strong("Select passer:")),
                                            do.call(fixedRow, lapply(passer_buttons$buttons, function(but) column(1, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", style = "width:100%; height:7vh; background-color:#F44;")),
                                                     column(2, offset = 8, actionButton("assign_passer", "Continue", style = "width:100%; height:7vh; background-color:#4F4;")))
                                            ))
                } else if (rally_state() == "enter serve outcome") {
                    sp <- if (!is.null(input$serve_preselect_player)) input$serve_preselect_player else if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    game_state$current_team <- other(game_state$serving)
                    st <- if (!is.null(input$serve_type)) input$serve_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    pp <- input$select_passer
                    removeModal()
                    sz <- dv_xy2zone(game_state$start_x, game_state$start_y, as_for_serve = TRUE)
                    esz <- paste(dv_xy2subzone(game_state$end_x, game_state$end_y), collapse = "")
                    start_t <- retrieve_video_time(game_state$start_t)
                    end_t <- retrieve_video_time(game_state$end_t)
                    rc <- rally_codes()
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- mutate(rc[Sidx, ], pnum = zpn(sp), tempo = st, esz = esz, t = start_t, end_x = game_state$end_x, end_y = game_state$end_y)
                    }
                    rally_codes(bind_rows(
                        rc,
                        code_trow(team = other(game_state$serving), pnum = pp, skill = "R", tempo = st, sz = sz, esz = esz, t = end_t, start_x = game_state$start_x, start_y = game_state$start_y, end_x = game_state$end_x, end_y = game_state$end_y)))
                    rally_state("click second contact")
                    do_video("rew", app_data$play_overlap); do_video("play")
                } else if (rally_state() == "serve error") {
                    sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                    serve_err_type <- if (!is.null(input$was_serve_error)) input$was_serve_error else "="
                    st <- if (!is.null(input$serve_type)) input$serve_type else app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                    removeModal()
                    special_code <- substr(serve_err_type, 2, 2)
                    esz <- paste(dv_xy2subzone(game_state$end_x, game_state$end_y), collapse = "")
                    rc <- rally_codes()
                    Sidx <- which(rc$skill == "S")
                    if (length(Sidx) == 1) {
                        ## update the serve code entry
                        rc[Sidx, ] <- mutate(rc[Sidx, ], pnum = zpn(sp), tempo = st, eval = "=", esz = esz, special = if (nzchar(special_code)) special_code else "~", t = retrieve_video_time(game_state$start_t), end_x = game_state$end_x, end_y = game_state$end_y)
                    }
                    rally_codes(rc)
                    rally_state("rally ended")
                    game_state$point_won_by <- other(game_state$serving)
                    do_video("play")
                } else if (rally_state() == "click second contact") {
                    ## set (play continues), setter dump, set error, P2 attack, or freeball over (by the receiving team)
                    ## or PR, dig/freeball dig by opposition
                    ## we get a clue if it's the receiving/digging team or their opposition by the side of the court that has been clicked
                    do_video("pause")
                    ## click was the set contact position, or the freeball start position
                    game_state$start_x <- courtxy$x[1]
                    game_state$start_y <- courtxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy)
                    ## popup
                    ## TODO maybe also setter call here
                    c2_buttons <- make_fat_radio_buttons(
                        choices = c(Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball attack" = "P2", "Freeball over" = "F", ## rcv team actions
                                    "Opp. dig" = "aD", "Opp. overpass attack" = "aPR"), ## opp actions
                        selected = "E", input_var = "c2", style = "width:100%; height:7vh;")
                    if (app_data$is_beach) {
                        stop("setter for beach")
                        ## choose the player who didn't pass
                    }
                    sp <- get_setter(game_state)
                    sp <- c(sp, setdiff(get_players(game_state, dvw = rdata$dvw), sp), get_liberos(game_state, dvw = rdata$dvw))
                    names(sp) <- player_nums_to(sp, team = game_state$current_team, dvw = rdata$dvw)
                    sp <- c(sp, Unknown = "Unknown")
                    setter_buttons <- make_fat_radio_buttons(choices = sp, input_var = "c2_player", style = "width:100%; height:7vh;")
                    opp <- c(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw), get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_buttons <- make_fat_radio_buttons(choices = opp, input_var = "c2_opp_player", style = "width:100%; height:7vh;")
                    showModal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Second contact:")),
                                            do.call(fixedRow, lapply(c2_buttons$buttons[1:5], function(but) column(2, but))),
                                            tags$br(),
                                            tags$p("by player"),
                                            tags$br(),
                                            do.call(fixedRow, lapply(setter_buttons$buttons, function(but) column(1, but))),
                                            tags$br(),
                                            tags$div("OR"),
                                            tags$br(),
                                            do.call(fixedRow, lapply(c2_buttons$buttons[6:7], function(but) column(2, but))),
                                            tags$br(),
                                            tags$p("by player"),
                                            tags$br(),
                                            do.call(fixedRow, lapply(opp_buttons$buttons, function(but) column(1, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", style = "width:100%; height:7vh; background-color:#F44;")),
                                                     column(2, offset = 8, actionButton("assign_c2", "Continue", style = "width:100%; height:7vh; background-color:#4F4;")))
                                            ))
                } else if (rally_state() == "second contact details") {
                    ## set uses end position for zone/subzone
                    esz <- paste(dv_xy2subzone(game_state$start_x, game_state$start_y), collapse = "")
                    passq <- guess_pass_quality(game_state, dvw = rdata$dvw)
                    ## TODO, this better
                    rc <- rally_codes()
                    rc$eval[rc$skill %eq% "R"] <- passq
                    rc$eval[rc$skill %eq% "S"] <- passq
                    rally_codes(rc)
                    ## possible values for input$c2 are: Set = "E", "Set error" = "E=", "Setter dump" = "PP", "Second-ball attack" = "P2", "Freeball over" = "F",
                    ##                                   "Opp. dig" = "aD", "Opp. overpass attack" = "aPR"
                    if (input$c2 %in% c("E", "E=", "PP", "P2", "F")) {
                        sp <- input$c2_player
                        start_t <- retrieve_video_time(game_state$start_t)
                        if (input$c2 == "E") {
                            rally_codes(bind_rows(rally_codes(), code_trow(team = game_state$current_team, pnum = sp, skill = "E", esz = esz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y)))
                            rally_state("click third contact")
                        } else if (input$c2 == "E=") {
                            ## set error
                            rally_codes(bind_rows(rally_codes(), code_trow(team = game_state$current_team, pnum = sp, skill = "E", eval = "=", esz = esz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y)))
                            rally_state("rally ended")
                            game_state$point_won_by <- other(game_state$current_team)
                        } else if (input$c2 %in% c("PP", "P2")) {
                            ## setter dump or second ball attack
                            sz <- paste(dv_xy2zone(game_state$start_x, game_state$start_y), collapse = "")
                            ## NOTE we have hard-coded PP, P2 here, TODO fix to allow other codes to be used
                            trg <- if (input$c2 == "PP") "S" else "~"
                            rally_codes(bind_rows(rally_codes(), code_trow(team = game_state$current_team, pnum = sp, skill = "A", tempo = "O", combo = input$c2, target = trg, sz = sz, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y)))
                            rally_state("click attack end point")
                            game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                        } else if (input$c2 == "F") {
                            ## freeball over
                            stop("freeball not coded")
                        }
                    } else {
                        stop("opposition c2 not coded")
                    }
                    removeModal()
                    if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
                    do_video("play")
                } else if (rally_state() == "click third contact") {
                    ## attack, freeball over (by the setting team)
                    ## or dig/freeball dig by on overset, or PR
                    do_video("pause")
                    ## click was the attack contact position, or the freeball start position
                    game_state$start_x <- courtxy$x[1]
                    game_state$start_y <- courtxy$y[1]
                    game_state$start_t <- game_state$current_time_uuid
                    overlay_points(courtxy)
                    ## popup
                    ac <- c(guess_attack_code(game_state, dvw = rdata$dvw), "Other attack")
                    c3_buttons <- make_fat_radio_buttons(choices = c(setNames(ac, ac), c("Opp. dig" = "aD", "Opp. overpass attack" = "aPR")), input_var = "c3", style = "width:100%; height:7vh;")
                    ap <- get_players(game_state, dvw = rdata$dvw)
                    ## TODO default selection better based on rotation
                    names(ap) <- player_nums_to(ap, team = game_state$current_team, dvw = rdata$dvw)
                    ap <- c(ap, Unknown = "Unknown")
                    attacker_buttons <- make_fat_radio_buttons(choices = ap, input_var = "c3_player", style = "width:100%; height:7vh;")
                    if (isTRUE(app_data$options$nblockers)) nblocker_buttons <- make_fat_radio_buttons(choices = c("No block" = 0, "Single block" = 1, "Double block" = 2, "Triple block" = 3), input_var = "nblockers", style = "width:100%; height:7vh;")
                    ## attack error, blocked, replay will be scouted on next entry
                    ## TODO other special codes ?
                    ## TODO "F" freeball
                    opp <- c(get_players(game_state, team = other(game_state$current_team), dvw = rdata$dvw), get_liberos(game_state, team = other(game_state$current_team), dvw = rdata$dvw))
                    names(opp) <- player_nums_to(opp, team = other(game_state$current_team), dvw = rdata$dvw)
                    opp <- c(opp, Unknown = "Unknown")
                    opp_player_buttons <- make_fat_radio_buttons(choices = opp, input_var = "c3_opp_player", style = "width:100%; height:7vh;")
                    showModal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Attack:")),
                                            do.call(fixedRow, lapply(c3_buttons$buttons[seq_along(ac)], function(but) column(2, but))),
                                            tags$br(), tags$p("by player"), tags$br(),
                                            do.call(fixedRow, lapply(attacker_buttons$buttons, function(but) column(1, but))),
                                            if (isTRUE(app_data$options$nblockers)) tags$div(tags$br(), "with", tags$br()),
                                            if (isTRUE(app_data$options$nblockers)) do.call(fixedRow, lapply(nblocker_buttons$buttons, function(but) column(2, but))),
                                            tags$br(), tags$hr(), tags$div("OR"), tags$br(),
                                            do.call(fixedRow, lapply(tail(c3_buttons$buttons, 2), function(but) column(2, but))),
                                            tags$br(), tags$p("by player"), tags$br(),
                                            do.call(fixedRow, lapply(opp_player_buttons$buttons, function(but) column(1, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", style = "width:100%; height:7vh; background-color:#F44;")),
                                                     column(2, offset = 8, actionButton("assign_c3", "Continue", style = "width:100%; height:7vh; background-color:#4F4;")))
                                            ))
                } else if (rally_state() == "third contact details") {
                    ## possible values for input$c3 are: an attack code, Freeball
                    ##                                   "Opp. dig" = "aD", "Opp. overpass attack" = "aPR"
                    if (input$c3 %in% c("aD", "aPR")) {
                        stop("opposition c3 not coded")
                    } else {
                        if (input$c3 == "F") {
                            ## freeball over
                            stop("freeball not coded")
                        } else {
                            ## only an attack code or "Other attack" for the time being
                            ap <- input$c3_player
                            start_t <- retrieve_video_time(game_state$start_t)
                            ac <- input$c3
                            if (nchar(ac) == 2) {
                                tempo <- tryCatch(rdata$dvw$meta$attacks$type[rdata$dvw$meta$attacks$code %eq% input$c3], error = function(e) "~")
                                targ <- tryCatch(rdata$dvw$meta$attacks$set_type[rdata$dvw$meta$attacks$code %eq% input$c3], error = function(e) "~")
                            } else {
                                ## other attack
                                ac <- "~~" ## no combo code
                                tempo <- "H"
                                targ <- "~"
                            }
                            if (nchar(tempo) != 1) tempo <- "~"
                            if (nchar(targ) != 1 || targ %eq% "-") targ <- "~"
                            sz <- dv_xy2zone(game_state$start_x, game_state$start_y)
                            nb <- input$nblockers
                            if (is.null(nb) || !nb %in% 0:3) nb <- "~"
                            ##if (nchar(input$c3) == 2) {
                            rally_codes(bind_rows(rally_codes(), code_trow(team = game_state$current_team, pnum = ap, skill = "A", tempo = tempo, combo = ac, target = targ, sz = sz, num_p = nb, t = start_t, start_x = game_state$start_x, start_y = game_state$start_y)))
                            rally_state("click attack end point")
                            game_state$current_team <- other(game_state$current_team) ## next touch will be by other team
                        }
                    }
                    removeModal()
                    if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
                    do_video("play")
                } else if (rally_state() == "click attack end point") {
                    ## dig, dig error (attack kill), attack error, blocked, blocked for replay, block touch (attack kill)
                    ## or block touch and play continues
                    ## allow attack kill with no dig error?
                    do_video("pause")
                    ## click was the dig or attack kill or error position, or the freeball start position
                    game_state$end_x <- courtxy$x[1]
                    game_state$end_y <- courtxy$y[1]
                    game_state$end_t <- game_state$current_time_uuid
                    overlay_points(courtxy)
                    ## popup
                    c1_buttons <- make_fat_radio_buttons(choices = c("Attack kill" = "A#", "Attack error" = "A=", "Dig" = "D", "Dig error" = "D="), input_var = "c1", style = "width:100%; height:7vh;")
                    digp <- c(get_players(game_state, dvw = rdata$dvw), get_liberos(game_state, dvw = rdata$dvw)) ## TODO better based on rotation
                    names(digp) <- player_nums_to(digp, team = game_state$current_team, dvw = rdata$dvw)
                    digp <- c(digp, Unknown = "Unknown")
                    dig_player_buttons <- make_fat_radio_buttons(choices = digp, selected = "Unknown", input_var = "c1_dig_player", style = "width:100%; height:7vh;")
                    showModal(vwModalDialog(title = "Details", footer = NULL,
                                            tags$p(tags$strong("Attack outcome:")),
                                            do.call(fixedRow, lapply(c1_buttons$buttons[1:2], function(but) column(2, but))),
                                            tags$br(), tags$hr(), tags$div("OR"), tags$br(),
                                            tags$p(tags$strong("Dig outcome:")),
                                            do.call(fixedRow, lapply(c1_buttons$buttons[3:4], function(but) column(2, but))),
                                            tags$br(), tags$p("by player"), tags$br(),
                                            do.call(fixedRow, lapply(dig_player_buttons$buttons, function(but) column(1, but))),
                                            tags$hr(),
                                            fixedRow(column(2, actionButton("cancelrew", "Cancel and rewind", style = "width:100%; height:7vh; background-color:#F44;")),
                                                     column(2, offset = 8, actionButton("assign_c1", "Continue", style = "width:100%; height:7vh; background-color:#4F4;")))
                                            ))
                } else if (rally_state() == "first contact details") {
                    ## possible values for input$c1 are currently: A#, A=, D, D=
                    esz <- paste(dv_xy2subzone(game_state$end_x, game_state$end_y), collapse = "")
                    if (input$c1 %in% c("A#", "A=")) {
                        ##end_t <- retrieve_video_time(game_state$end_t)
                        eval <- substr(input$c1, 2, 2)
                        ## find the attack, should be either the previous skill, or one previous to that with a block in between
                        rc <- rally_codes()
                        Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                        if (!is.na(Aidx)) {
                            rc$esz[Aidx] <- esz
                            rc$end_x[Aidx] <- game_state$end_x
                            rc$end_y[Aidx] <- game_state$end_y
                            rc$eval[Aidx] <- eval
                            rally_codes(rc)
                        }
                        rally_state("rally ended")
                        ## "current" team here is the digging team
                        game_state$point_won_by <- if (eval == "#") other(game_state$current_team) else game_state$current_team
                    } else {
                        ## D or D=
                        digp <- input$c1_dig_player
                        end_t <- retrieve_video_time(game_state$end_t)
                        rc <- rally_codes()
                        ## was the previous skill an attack, or one previous to that an attack with a block in between
                        Aidx <- if (rc$skill[nrow(rc)] == "A") nrow(rc) else if (rc$skill[nrow(rc)] == "B" && rc$skill[nrow(rc) - 1] == "A") nrow(rc) - 1L else NA_integer_
                        if (!is.na(Aidx)) {
                            rc$esz[Aidx] <- esz
                            rc$end_x[Aidx] <- game_state$end_x
                            rc$end_y[Aidx] <- game_state$end_y
                            tempo <- rc$tempo[Aidx]
                        } else {
                            tempo <- "~"
                        }
                        if (input$c1 == "D=") {
                            ## TODO use compound codes here?
                            if (!is.na(Aidx)) rc$eval[Aidx] <- "#"
                            eval <- "="
                        } else {
                            ## TODO use compound codes here?
                            if (!is.na(Aidx)) rc$eval[Aidx] <- "-"
                            eval <- "+"
                        }
                        rally_codes(bind_rows(rc, code_trow(team = game_state$current_team, pnum = digp, skill = "D", eval = eval, tempo = tempo, sz = substr(esz, 1, 1), t = end_t, start_x = game_state$end_x, start_y = game_state$end_y)))
                        if (input$c1 == "D=") {
                            rally_state("rally ended")
                            game_state$point_won_by <- other(game_state$current_team)
                        } else {
                            if (!isTRUE(app_data$options$transition_sets)) {
                                rally_state("click third contact")
                            } else {
                                rally_state("click second contact")
                            }
                        }
                    }
                    removeModal()
                    if (rally_state() != "rally ended") do_video("rew", app_data$play_overlap)
                    do_video("play")
                } else {
                    stop("unknown rally state: ", rally_state())
                }
            }
            if (nrow(rally_codes())) { cat("rally codes:\n"); print_rally_codes(rally_codes()) }
            cat("rally state: ", rally_state(), "\n")
            if (rally_state() == "rally ended") {
                ## add rally codes to scout object now
                rdata$dvw$plays2 <- bind_rows(rdata$dvw$plays2, make_plays2(rally_codes(), game_state = game_state, rally_ended = TRUE, dvw = rdata$dvw))
                ## update game_state
                do_rot <- game_state$point_won_by != game_state$serving
                if (game_state$point_won_by == "*") {
                    game_state$home_score_start_of_point <- game_state$home_score_start_of_point + 1L
                } else {
                    game_state$visiting_score_start_of_point <- game_state$visiting_score_start_of_point + 1L
                }
                if (do_rot) {
                    if (game_state$point_won_by == "*") {
                        game_state$home_setter_position <- rotpos(game_state$home_setter_position, n = length(pseq))
                        temp <- rotvec(as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)]))
                        for (i in pseq) game_state[[paste0("home_p", i)]] <- temp[i]
                        poscode <- paste0("*z", game_state$home_setter_position)
                    } else {
                        game_state$visiting_setter_position <- rotpos(game_state$visiting_setter_position, n = length(pseq))
                        temp <- rotvec(as.numeric(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)]))
                        for (i in pseq) game_state[[paste0("visiting_p", i)]] <- temp[i]
                        poscode <- paste0("az", game_state$visiting_setter_position)
                    }
                    rdata$dvw$plays2 <- bind_rows(rdata$dvw$plays2, make_plays2(poscode, game_state = game_state))
                }
                ## reset for next rally
                game_state$serving <- game_state$current_team <- game_state$point_won_by
                rally_codes(empty_rally_codes)
                game_state$start_x <- game_state$start_y <- game_state$end_x <- game_state$end_y <- NA_real_
                game_state$current_time_uuid <- ""
                game_state$point_won_by <- NA_character_
                rally_state("click serve start")
            }
        })

        ## convert the rally codes into plays2 rows, and build plays from plays2
        observe({
            temp_rally_plays2 <- if (nrow(rally_codes()) > 0) make_plays2(rally_codes(), game_state = game_state) else NULL
            ##            cat(str(temp_rally_plays2))
            ##            cat(str(rdata$dvw$plays2))
            rdata$dvw$plays <- plays2_to_plays(bind_rows(rdata$dvw$plays2, temp_rally_plays2), dvw = rdata$dvw, evaluation_decoder = app_data$evaluation_decoder)
            scroll_playlist(nrow(rdata$dvw$plays))
        })

        observeEvent(input$cancelrew, {
            do_video("rew", 3)
            do_video("play")
            removeModal()
        })
        observeEvent(input$was_serve_error, {
            rally_state("serve error")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$assign_passer, {
            rally_state("enter serve outcome")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$assign_c1, {
            rally_state("first contact details")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$assign_c2, {
            rally_state("second contact details")
            loop_trigger(loop_trigger() + 1L)
        })

        observeEvent(input$assign_c3, {
            rally_state("third contact details")
            loop_trigger(loop_trigger() + 1L)
        })

        output$rally_state <- renderUI({
            tags$div(id = "rallystate", ##style = if (!is.null(input$vo_voffset) && as.numeric(input$vo_voffset) > 0) paste0("margin-top: ", input$vo_offset - 50, "px") else "",
                     tags$strong("Rally state: "), rally_state())
        })
        observeEvent(rally_state(), {
            if (rally_state() == "click serve start") {
                ## show the serve player and tempo pre-select buttons
                sp <- if (game_state$serving == "*") game_state$home_p1 else if (game_state$serving == "a") game_state$visiting_p1 else 0L
                ## sp should be the serving player
                ## other players that could be serving, if the rotation is somehow wrong
                other_sp <- get_players(game_state, team = game_state$serving, dvw = rdata$dvw)
                serve_player_buttons <- make_fat_radio_buttons(choices = c(sp, setdiff(other_sp, sp)), selected = sp, input_var = "serve_preselect_player", style = "width:100%; height:7vh;")
                ## default serve type is either the most common serve type by this player, or the default serve type
                st_default <- get_player_serve_type(px = rdata$dvw$plays, serving_player_num = sp, game_state = game_state)
                if (is.na(st_default)) st_default <- app_data$default_scouting_table$tempo[app_data$default_scouting_table$skill == "S"]
                serve_type_buttons <- make_fat_radio_buttons(choices = c(Jump = "Q", "Jump-float" = "M", Float = "H", Topspin = "T"), selected = st_default, input_var = "serve_preselect_type", style = "width:100%; height:7vh;")
                output$serve_preselect <- renderUI(
                    tags$div(tags$strong("Serve type:"), do.call(fixedRow, lapply(serve_type_buttons$buttons, function(but) column(2, but))),
                             tags$strong("Serve player:"), do.call(fixedRow, lapply(serve_player_buttons$buttons, function(but) column(2, but))))
                )
            } else {
                output$serve_preselect <- NULL
            }
        })

        ## initialize the game state
        rally_state <- reactiveVal("click the video to start")
        rally_codes <- reactiveVal(empty_rally_codes)
        temp <- as.list(tail(app_data$dvw$plays2, 1))
        if (!"serving" %in% names(temp) || is.na(temp$serving)) temp$serving <- "*" ## FIX TODO
        temp$current_team <- temp$serving
##        temp$home_end <- "lower" ## FIX TODO
        temp$start_x <- temp$start_y <- temp$end_x <- temp$end_y <- NA_real_
        temp$current_time_uuid <- ""
        game_state <- do.call(reactiveValues, temp)
        court_inset$home_team_end("upper") ## init

        observe({
            game_state$home_end <- court_inset$home_team_end()
        })

        ## file save
        output$save_file_ui <- renderUI({
            if (is.null(rdata$dvw$plays2)) {
                NULL
            } else {
                downloadButton("save_file_button", "Save file")
            }
        })
        output$save_file_button <- downloadHandler(
            filename = reactive(
                if (!is.null(rdata$dvw$meta$filename) && !is.na(rdata$dvw$meta$filename)) basename(rdata$dvw$meta$filename) else "myfile.dvw"
            ),
            content = function(file) {
                tryCatch(dv_write2(rdata$dvw, file = file),
                         error = function(e) {
                             rds_ok <- FALSE
                             if (running_locally) {
                                 ## this only makes sense if running locally, not deployed on a remote server
                                 tf <- tempfile(fileext = ".rds")
                                 try({
                                     saveRDS(rdata$dvw, file = tf)
                                     rds_ok <- file.exists(tf) && file.size(tf) > 0
                                 }, silent = TRUE)
                             }
                             showModal(modalDialog(title = "Save error",
                                                   tags$div(class = "alert alert-danger", "Sorry, the save failed. The error message was:", tags$br(), tags$pre(conditionMessage(e)), tags$br(), if (rds_ok) paste0("The edited datavolley object has been saved to ", tf, ". You might be able to recover your edited information from that (contact the package authors for assistance)."))))
                             NULL
                         })
            }
        )
    }
}

## a player's most common serve type
## px is a plays object
get_player_serve_type <- function(px, serving_player_num, game_state) {
    if (is.null(px)) return(NA_character_)
    out <- dplyr::select(dplyr::filter(px, .data$skill %eq% "Serve" & .data$team == game_state$serving & .data$player_number %eq% serving_player_num), .data$skill_type)
    out <- mutate(out, stype = case_when(.data$skill_type %eq% "Float serve" ~ "H",
                                         .data$skill_type %eq% "Topspin serve" ~ "T",
                                         .data$skill_type %eq% "Jump-float serve" ~ "M",
                                         .data$skill_type %eq% "Jump serve" ~ "Q"))
    out <- dplyr::arrange(dplyr::count(out, .data$stype), .data$n)
    if (nrow(out) > 0) out$stype[1] else NA_character_
}

make_plays2 <- function(rally_codes, game_state, rally_ended = FALSE, dvw) {
    pseq <- seq_len(if (dv_is_beach(x)) 2L else 6L)
    if (is.data.frame(rally_codes)) {
        codes <- codes_from_rc_rows(rally_codes)
        start_coord <- dv_xy2index(as.numeric(rally_codes$start_x), as.numeric(rally_codes$start_y))
        end_coord <- dv_xy2index(as.numeric(rally_codes$end_x), as.numeric(rally_codes$end_y))
        vt <- rally_codes$t
    } else {
        ## rally_codes are just char, which means that these aren't skill codes, they are auto codes (position codes or similar)
        codes <- rally_codes
        start_coord <- end_coord <- NA_integer_
        vt <- NA_real_
    }
    if (rally_ended) {
        assert_that(game_state$point_won_by %in% c("*", "a"))
        ## add green codes if needed
        ## add the [*a]pXX:YY code at the end of the rally
        scores_end_of_point <- c(game_state$home_score_start_of_point, game_state$visiting_score_start_of_point) + as.integer(c(game_state$point_won_by == "*", game_state$point_won_by == "a"))
        pcode <- paste0(game_state$point_won_by, "p", sprintf("%02d:%02d", scores_end_of_point[1], scores_end_of_point[2]))
        codes <- make_auto_codes(c(codes, pcode), dvw)
        if (length(start_coord) > 1 || !is.na(start_coord)) {
            ## we've added entries to code, so we need to add dummy (NA) coord and video time values as well
            n_extra <- length(codes) - nrow(rally_codes)
            start_coord <- c(start_coord, rep(NA_integer_, n_extra))
            end_coord <- c(end_coord, rep(NA_integer_, n_extra))
            vt <- c(vt, rep(NA_real_, n_extra))
        }
    }
    ##cat("codes: "); print(codes)

    out <- tibble(code = codes, ## col 1
                  point_phase = NA_character_, ## col 2
                  attack_phase = NA_character_, ## col 3
##                  X4 = NA, ## col 4
                  start_coordinate = start_coord, mid_coordinate = NA_integer_, end_coordinate = end_coord, ## cols 5-7
                  time = time_but_utc(), ## col 8
                  set_number = game_state$set_number, home_setter_position = game_state$home_setter_position, visiting_setter_position = game_state$visiting_setter_position, ## cols 9-11, NB these 3 not used directly in dv_read
                  video_file_number = NA, video_time = vt, ## cols 12-13
  ##                X14 = NA, ## col 14
                  home_score_start_of_point = game_state$home_score_start_of_point,
                  visiting_score_start_of_point = game_state$visiting_score_start_of_point,
                  serving = game_state$serving)
    out <- bind_cols(out, as.data.frame(reactiveValuesToList(game_state)[paste0("home_p", pseq)]))
    bind_cols(out, as.data.frame(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)]))
}

plays2_to_plays <- function(plays2, dvw, evaluation_decoder) {
    pseq <- if (is_beach(dvw)) 1:2 else 1:6
    out <- datavolley:::parse_code(plays2$code, meta = dvw$meta, evaluation_decoder = evaluation_decoder, file_type = if (is_beach(dvw)) "beach" else "indoor")$plays
    out$home_setter_position <- plays2$home_setter_position
    out$visiting_setter_position <- plays2$visiting_setter_position
    out$home_team_score <- plays2$home_score_start_of_point ## TODO FIX
    out$visiting_team_score <- plays2$visiting_score_start_of_point ## TODO FIX
  ##code team player_number player_name player_id skill skill_type evaluation_code evaluation attack_code
  ##attack_description set_code set_description set_type start_zone end_zone end_subzone end_cone skill_subtype num_players
  ##num_players_numeric special_code timeout end_of_set substitution point home_team_score visiting_team_score
  ##home_setter_position visiting_setter_position custom_code file_line_number
    out$phase <- datavolley::play_phase(out)
    out$set_number <- plays2$set_number
    out$video_time <- plays2$video_time
    out$error_icon <- ""##ifelse(is.na(x$plays$error_message), "", HTML(as.character(shiny::icon("exclamation-triangle"))))
    bind_cols(out, plays2[, c(paste0("home_p", pseq), paste0("visiting_p", pseq))])
}

is_beach <- function(dvw) isTRUE(grepl("beach", dvw$meta$match$regulation))
zpn <- function(n) sprintf("%02d", as.numeric(n))
other <- function(tm) { oth <- rep(NA_character_, length(tm)); oth[tm %eq% "*"] <- "a"; oth[tm %eq% "a"] <- "*"; oth }
##other <- function(tm) c("a", "*")[as.numeric(factor(tm, levels = c("*", "a")))]

empty_rally_codes <- tibble(code = character(), t = numeric(), start_x = numeric(), start_y = numeric(), end_x = numeric(), end_y = numeric())
print_rally_codes <- function(rc) {
    tr <- function(z) tryCatch(round(z, 1), error = function(e) z)
    rc$code <- codes_from_rc_rows(rc)
    do.call(cat, c(lapply(seq_len(nrow(rc)), function(i) paste0(rc$code[i], ", t=", tr(rc$t[i]), ", start x=", tr(rc$start_x[i]), ", y=", tr(rc$start_y[i]), ", end x=", tr(rc$end_x[i]), ", y=", tr(rc$end_y[i]))), list(sep = "\n")))
}
codes_from_rc_rows <- function(rc) sub("~+$", "", paste0(rc$team, rc$pnum, rc$skill, rc$tempo, rc$eval, rc$combo, rc$target, rc$sz, rc$ez, rc$esz, rc$x_type, rc$num_p, rc$special, rc$custom))

get_setter_pos <- function(game_state, team) {
    if (missing(team)) team <- game_state$current_team
    if (team == "*") game_state$home_setter_position else if (team == "a") game_state$visiting_setter_position else NA_integer_
}

get_setter <- function(game_state, team) {
    pseq <- 1:6 ## indoor only
    if (missing(team)) team <- game_state$current_team
    if (team == "*") {
        as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)])[game_state$home_setter_position]
    } else if (team == "a") {
        as.numeric(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)])[game_state$visiting_setter_position]
    } else {
        0L
    }
}

get_liberos <- function(game_state, team, dvw) {
    if (missing(team)) team <- game_state$current_team
    if (is_beach(dvw)) c() else if (team == "*") dvw$meta$players_h$number[dvw$meta$players_h$special_role %eq% "L"] else if (team == "a") dvw$meta$players_v$number[dvw$meta$players_v$special_role %eq% "L"] else c()
}

get_players <- function(game_state, team, dvw) {
    pseq <- if (is_beach(dvw)) 1:2 else 1:6
    if (missing(team)) team <- game_state$current_team
    if (team == "*") {
        as.numeric(reactiveValuesToList(game_state)[paste0("home_p", pseq)])
    } else if (team == "a") {
        as.numeric(reactiveValuesToList(game_state)[paste0("visiting_p", pseq)])
    } else {
        0L
    }
}

## TODO
## - if on first serve of the set, the click is at the wrong end according to game_state$home_end and game_state$serving == "*", then either auto-flip the court or pause and check with user
## - outside of video pane, have "Rewind 5s", "Pause", "Enter manual code" buttons
## - propagate clicks to video itself? (controls)
## - have a popup with all buttons required to manually enter a skill code:
##   Team * a
##   Number blah
##   Skill S R etc but not including T C P etc
##   - and a text box to enter T, C, P etc
## - player selection via the court inset, with players shown in their assumed playing locations (? what to do with libero?)

## default/pre-selected choices:
## - serves, guess serve type based on player's previous serves AND/OR time between serve and reception contacts
## - reception evaluation (and serve evaluation) based on set contact xy and time between reception and set contacts
## - passing player based on reception xy, rotation, and assumed passing system
## - attacking player based on start xy, rotation, and assumed offensive system
## - attack tempo based on time and distance between set and attack contacts
## - number of blockers based on attack tempo and location (default to 1 on quick, 2 on medium, 3 on high  - make this configurable)

player_nums_to <- function(nums, team, dvw, to = "number lastname") {
    to <- match.arg(to, c("name", "number lastname"))
    temp_players <- if (team == "*") dvw$meta$players_h else if (team == "a") dvw$meta$players_v else stop("team should be '*' or 'a'")
    temp <- left_join(tibble(number = nums), temp_players[, c("number", if (to == "name") "name", if (to == "number lastname") "lastname")], by = "number")
    if (to == "name") {
        temp$name[is.na(temp$name)] <- ""
        temp$name
    } else if (to == "number lastname") {
        temp$lastname[is.na(temp$lastname)] <- ""
        paste(temp$number, temp$lastname, sep = "<br />")
    } else {
        character()
    }
}

## return a list of the possible passing players, and our guess of the most likely passer
## First, we identify the rotation, and check if passing has occured in the past from a serve at that location.
## If yes, the passer (currently on court) with the most receptions will be proposed in priority.
## If no, we assume S-H-M rotation, and articulate the 3-player passing line with each taking left-middle-right channel.
guess_pass_player_options <- function(game_state, dvw, system) {
    beach <- is_beach(dvw)
    pseq <- if (beach) 1:2 else 1:6
    if (game_state$serving %eq% "*") {
        passing_team <- "a"
        passing_rot <- game_state$visiting_setter_position
        passing_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
        libs <- if (beach) c() else dvw$meta$players_v$number[dvw$meta$players_v$special_role %eq% "L"]

        # Define the prior probability of passing given rotation, passing zone, etc... Defined as a simple mean of beta().
        passing_responsibility <- player_responsibility_fn(system = system, skill = "Reception",
                                                           setter_position = passing_rot,
                                                           zone = passing_zone, libs = libs, home_visiting = "visiting")


        passing_responsibility_prior <- setNames(rep(0, 7), c(paste0("visiting_p",1:6),"libero"))
        passing_responsibility_prior[passing_responsibility] <- 1

        ## Update the probability with the history of the game
        passing_history <- dplyr::filter(dvw$plays, skill %eq% "Reception",
                                         .data$visiting_setter_position %eq% as.character(passing_rot),
                                         .data$end_zone %eq% passing_zone,
                                         .data$team %eq% "a")

        passing_responsibility_posterior <- passing_responsibility_prior
        if(nrow(passing_history)>0){
          passing_history <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(dplyr::filter(tidyr::pivot_longer(dplyr::select(passing_history, "team", "player_number", paste0("visiting_p",1:6)), cols = paste0("visiting_p",1:6)), .data$value %eq% .data$player_number), .data$name), n_reception = dplyr::n()))
          passing_responsibility_posterior[passing_history$name] <- passing_responsibility_prior[passing_history$name] + passing_history$n_reception
          passing_responsibility_posterior <-  passing_responsibility_posterior / sum(passing_responsibility_posterior)
        }
        plsel_tmp <- names(sort(passing_responsibility_posterior, decreasing = TRUE))

        poc <- paste0("visiting_p", pseq) ## players on court
    } else if (game_state$serving %eq% "a") {
        passing_team <- "*"
        passing_rot <- game_state$home_setter_position
        passing_zone <- dv_xy2zone(game_state$end_x, game_state$end_y)
        libs <- if (beach) c() else dvw$meta$players_h$number[dvw$meta$players_h$special_role %eq% "L"]

        # Define the prior probability of passing given rotation, passing zone, etc... Defined as a simple mean of beta().
        passing_responsibility <- player_responsibility_fn(system = system, skill = "Reception",
                                                           setter_position = passing_rot,
                                                           zone = passing_zone, libs = libs, home_visiting = "home")


        passing_responsibility_prior <- setNames(rep(0, 7), c(paste0("home_p",1:6),"libero"))
        passing_responsibility_prior[passing_responsibility] <- 1

        # Update the probability with the history of the game

        passing_history <- dplyr::filter(dvw$plays, skill %eq% "Reception",
                                         .data$home_setter_position %eq% as.character(passing_rot),
                                         .data$end_zone %eq% passing_zone,
                                         .data$team %eq% "*")

        passing_responsibility_posterior <- passing_responsibility_prior
        if(nrow(passing_history)>0){
          passing_history <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(dplyr::filter(tidyr::pivot_longer(dplyr::select(passing_history, "team", "player_number",
                                                                                                                             paste0("home_p",1:6)), cols = paste0("home_p",1:6)),
                                                                                           .data$value %eq% .data$player_number), .data$name), n_reception = dplyr::n()))
          passing_responsibility_posterior[passing_history$name] <- passing_responsibility_prior[passing_history$name] + passing_history$n_reception
          passing_responsibility_posterior <-  passing_responsibility_posterior / sum(passing_responsibility_posterior)
        }
        plsel_tmp <- names(sort(passing_responsibility_posterior, decreasing = TRUE))


        poc <- paste0("home_p", pseq) ## players on court
    } else {
        return(list(choices = numeric(), selected = c()))
    }
    pp <- c(as.numeric(reactiveValuesToList(game_state)[poc]), libs)
    plsel <- if(plsel_tmp[1] %eq% "libero") libs[1] else as.numeric(reactiveValuesToList(game_state)[plsel_tmp[1]])
    ## passing player guess based on reception xy c(game_state$end_x, game_state$end_y), rotation, and assumed passing system
    ## for now, either the first libero or last player in pp
    list(choices = pp, selected = plsel)
}

guess_pass_quality <- function(game_state, dvw) {
    if (is_beach(dvw)) {
        ## TODO
        cat("beach, defaulting to '+' pass quality\n")
        return("+")
    }
    ## reference clicks to lower court
    do_flip_click <- (game_state$current_team == "*" && game_state$home_end == "upper") || (game_state$current_team == "a" && game_state$home_end == "lower")
    thisxy <- if (do_flip_click) as.numeric(dv_flip_xy(game_state$start_x, game_state$start_y)) else c(game_state$start_x, game_state$start_y)
    ## sets use "start" coordinates but "end" zone/subzone
    esz <- paste(dv_xy2subzone(game_state$start_x, game_state$start_y), collapse = "")
    ## TODO account for time since previous contact, and better boundaries here
    out <- if (thisxy[2] > 3.5) {
        "/" ## overpass
    } else if (thisxy[2] >= 3 & thisxy[1] >= 2  & thisxy[1] <= 3) {
        "#"
    } else if (thisxy[2] >= 3.25 & thisxy[1] >= 1.5  & thisxy[1] <= 3) {
        "+"
    } else if (thisxy[2] > 2.25 & thisxy[1] >= 1 & thisxy[1] <= 3.5) {
        "!"
    } else {
        "-"
    }
    cat("guessed pass quality: ", out, "\n")
    out
}

guess_attack_code <- function(game_state, dvw) {
    atbl <- dvw$meta$attacks
    do_flip_click <- (game_state$current_team == "*" && game_state$home_end == "upper") || (game_state$current_team == "a" && game_state$home_end == "lower")
    thisxy <- if (do_flip_click) as.numeric(dv_flip_xy(game_state$start_x, game_state$start_y)) else c(game_state$start_x, game_state$start_y)
    d <- sqrt((atbl$start_x - thisxy[1])^2 + (atbl$start_y - thisxy[2])^2)
    ## if setter is back row, slides are unlikely
    if (get_setter_pos(game_state) %in% c(5, 6, 1)) {
        d[grepl("Slide ", atbl$description)] <- d[grepl("Slide ", atbl$description)] + 0.5 ## penalty. Hard-coding "Slide" is not great, could also look for type = "N" but some scouts use this for non-slides. TODO FIX
        ## also back-row right side is less likely than front-row right side if setter is back row
        d[atbl$attacker_position %eq% 9] <- d[atbl$attacker_position %eq% 9] + 0.5
    }
    ## TODO also incorporate set -> attack contact time and distance, to infer tempo
    d[is.na(d)] <- Inf
    ac <- head(atbl$code[order(d)], 5)
    for (i in head(seq_along(ac), -1)) {
        ## swap e.g. V5 X5 so that X5 is first
        if (substr(ac[i], 2, 2) == substr(ac[i+1], 2, 2) && grepl("^V", ac[i]) && grepl("^X", ac[i+1])) ac[c(i, i+1)] <- ac[c(i+1, i)]
    }
    ac
}

## returns a list with components
##  buttons: a list of button tags
##  js: if radio = TRUE, the js needed to make radio-style behaviour and to set the initial choice
make_fat_radio_buttons <- function(...) make_fat_buttons(..., as_radio = TRUE)
make_fat_buttons <- function(choices, selected, input_var, extra_class = c(), as_radio = FALSE, ...) {
    if (length(choices) < 1) return(NULL)
    if (length(names(choices)) < 1) names(choices) <- choices
    cls <- uuid()
    ids <- uuid(n = length(choices))
    ## the actual buttons
    selected <- if (missing(selected)) 1L else if (selected %in% choices) which(choices == selected) else if (selected %in% names(choices)) which(names(choices) == selected) else if (!is.na(selected)) 1L
    clickfun <- if (as_radio) paste0("console.log('clicked'); $('.", cls, "').removeClass('active'); $(this).addClass('active');")##, if (!is.na(selected)) paste0(" $('#", ids[selected], "').click();"))
                else NULL
    buts <- lapply(seq_along(choices), function(i) tags$button(class = paste(c("btn", "btn-default", "fatradio", cls, extra_class, if (i %eq% selected && as_radio) "active"), collapse = " "), id = ids[i], HTML(names(choices)[i]), onclick = paste0(clickfun, " Shiny.setInputValue('", input_var, "', '", choices[[i]], "', {priority: 'event'})"), ...))
    if (!is.na(selected) && as_radio) {
        ##cat("setting selected:\n")
        thisjs <- paste0("Shiny.setInputValue(\"", input_var, "\", \"", choices[[selected]], "\")")
        ##cat(thisjs,"\n")
        dojs(thisjs)
    }
    list(buttons = buts)##, js = if (as_radio) paste0("$('.", cls, "').click(function(e) { $('.", cls, "').removeClass('active'); $(this).addClass('active'); });", if (!is.na(selected)) paste0(" $('#", ids[selected], "').click();")) else NULL)
}


mod_courtrot2_ui <- function(id, with_ball_coords = TRUE) {
    ns <- NS(id)
    tags$div(style = "border-radius: 4px; padding: 4px",
             if (with_ball_coords) fluidRow(style = "min-height: 34px;", ## min-height to retain layout when buttons are hidden
                                       column(4, checkboxInput(ns("ballcoordsCI"), label = "Display ball coordinates", value = TRUE)),
                                       column(4, actionButton(ns("cancel_ball_coords"), "Cancel ball coordinates")),
                                       column(4, actionButton(ns("validate_ball_coords"), label = "Accept ball coordinates"))),
             fluidRow(column(12, plotOutput(ns("court_inset"), click = ns("plot_click"))),),
             fluidRow(column(1, offset = 0, actionButton(ns("rotate_home"),icon("undo"))),
                      column(2, offset = 4, actionButton(ns("court_inset_swap"), label = "\u21f5", class = "iconbut")),
                      column(1, offset = 3, actionButton(ns("rotate_visiting"), icon("undo")))),
             fluidRow(column(6, id = "hroster", uiOutput(ns("htroster"))),
                      column(6, id = "vroster", uiOutput(ns("vtroster"))))
             )
}

mod_courtrot2 <- function(input, output, session, rdata, game_state, styling, with_ball_coords = TRUE) {
    pseq <- if (is_beach(isolate(rdata$dvw))) 1:2 else 1:6
    output$htroster <- renderUI({
        re <- names2roster(rdata$dvw$meta$players_h)
        do.call(tags$div, c(list(tags$strong("Home team"), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
    })
    output$vtroster <- renderUI({
        re <- names2roster(rdata$dvw$meta$players_v)
        do.call(tags$div, c(list(tags$strong("Visiting team"), tags$br()), lapply(re, function(z) tagList(tags$span(z), tags$br()))))
    })

    observeEvent(input$cancel_ball_coords, {
        clear_click_queue()
    })

    rotate_teams <- reactiveValues(home = 0L, visiting = 0L)
    ## we keep a queue of (up to) 3 clicked points
    click_points <- reactiveValues(queue = data.frame(x = numeric(), y = numeric()))
    add_to_click_queue <- function(x, y) {
        if (!is.data.frame(x)) x <- data.frame(x = x, y = y)
        click_points$queue <- if (nrow(click_points$queue) == 3) x else rbind(click_points$queue, x)
        click_points$queue
    }
    clear_click_queue <- function() {
        click_points$queue <- data.frame(x = numeric(), y = numeric())
        click_points$queue
    }
    observeEvent(input$plot_click, {
        req(input$plot_click)
        add_to_click_queue(x = input$plot_click$x, y = input$plot_click$y)
    })

    ## the court plot itself
    court_inset_home_team_end <- reactiveVal("lower")
    ball_coords <- if (with_ball_coords) reactive({input$ballcoordsCI}) else reactive(FALSE)
    output$court_inset <- renderPlot({
        p <- ggplot(data = data.frame(x = c(-0.25, 4.25, 4.25, -0.25), y = c(-0.25, -0.25, 7.25, 7.25)), mapping = aes_string("x", "y")) +
            geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = c(0.5, 0.5, 3.5, 3.5)), fill = styling$h_court_colour) +
            geom_polygon(data = data.frame(x = c(0.5, 3.5, 3.5, 0.5), y = 3 + c(0.5, 0.5, 3.5, 3.5)), fill = styling$v_court_colour) +
            ggcourt(labels = NULL, show_zones = FALSE, show_zone_lines = TRUE, court_colour = "indoor")
        gs <- game_state()
        ##cat("gs: "); cat(str(reactiveValuesToList(gs)))
        if (TRUE) {##!is.null(gs) && nrow(gs) > 0) {
            this_pn <- NULL ##rdata$dvw$plays$player_number[ridx] ## player in the selected row
            htrot <- tibble(number = get_players(gs, team = "*", dvw = rdata$dvw))
            htrot <- dplyr::left_join(htrot, rdata$dvw$meta$players_h[, c("player_id", "number", "lastname", "firstname", "name")], by = "number")
            ##vtrot <- tibble(player_id = as.character(rdata$dvw$plays[ridx, paste0("visiting_player_id", 1:6)]), team_id = rdata$dvw$plays$visiting_team_id[ridx])
            ##vtrot <- dplyr::left_join(vtrot, rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], by = "player_id")
            vtrot <- tibble(number = get_players(gs, team = "a", dvw = rdata$dvw))
            vtrot <- dplyr::left_join(vtrot, rdata$dvw$meta$players_v[, c("player_id", "number", "lastname", "firstname", "name")], by = "number")
            plxy <- cbind(dv_xy(pseq, end = "lower"), htrot)
##            plxy$court_num <- unlist(rdata$dvw$plays[ridx, paste0("home_p", pseq)]) ## the on-court player numbers in the play-by-play data
            ## player names and circles
            ## home team
            p <- p + geom_polygon(data = court_circle(cz = pseq, end = "lower"), aes_string(group = "id"), fill = styling$h_court_colour, colour = styling$h_court_highlight)
            ## highlighted player
            ##if (rdata$dvw$plays$team[ridx] %eq% rdata$dvw$plays$home_team[ridx] && sum(this_pn %eq% plxy$court_num) == 1) {
            ##    p <- p + geom_polygon(data = court_circle(cz = which(this_pn %eq% plxy$court_num), end = "lower"), fill = "yellow", colour = "black")
            ##}
            p <- p + geom_text(data = plxy, aes_string("x", "y", label = "number"), size = 6, fontface = "bold", vjust = 0) +
                geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
            ## visiting team
            plxy <- cbind(dv_xy(pseq, end = "upper"), vtrot)
##            plxy$court_num <- unlist(rdata$dvw$plays[ridx, paste0("visiting_p", pseq)]) ## the on-court player numbers in the play-by-play data
            p <- p + geom_polygon(data = court_circle(cz = pseq, end = "upper"), aes_string(group = "id"), fill = styling$v_court_colour, colour = styling$v_court_highlight)
            ##if (rdata$dvw$plays$team[ridx] %eq% rdata$dvw$plays$visiting_team[ridx] && sum(this_pn %eq% plxy$court_num) == 1) {
            ##    p <- p + geom_polygon(data = court_circle(cz = which(this_pn %eq% plxy$court_num), end = "upper"), fill = "yellow", colour = "black")
            ##}
            p <- p + geom_text(data = plxy, aes_string("x", "y", label = "number"), size = 6, fontface = "bold", vjust = 0) +
                geom_text(data = plxy, aes_string("x", "y", label = "lastname"), size = 3, vjust = 1.5)
##            if (!is.na(rdata$dvw$plays$start_coordinate_x[ridx]) & !is.na(rdata$dvw$plays$end_coordinate_x[ridx]) && ball_coords()) {
##                thisxy <- data.frame(x = as.numeric(rdata$dvw$plays[ridx, c("start_coordinate_x", "mid_coordinate_x", "end_coordinate_x")]),
##                                     y = as.numeric(rdata$dvw$plays[ridx, c("start_coordinate_y", "mid_coordinate_y", "end_coordinate_y")]))
##                p <- p + geom_point(data = thisxy[1, ], shape = 16, col = "green", size = 5) +
##                    geom_point(data = thisxy[3, ], shape = 16, col = "red", size = 5) +
##                    geom_path(data = na.omit(thisxy), arrow = arrow(length = unit(0.05, "npc"), ends = "last"))
##            }
##            if (nrow(click_points$queue) > 0) {
##                p <- p + geom_point(data = click_points$queue, shape = 16) +
##                    geom_path(data = click_points$queue, linetype = "dashed", colour = "black", arrow = arrow(length = unit(0.05, "npc"), ends = "last"))
##            }
            if (court_inset_home_team_end() != "lower") p <- p + scale_x_reverse() + scale_y_reverse()
##            if (gs$home_end != "lower") p <- p + scale_x_reverse() + scale_y_reverse()
        }
        p
    })

    observeEvent(input$rotate_home, {
        rotate_teams$home <- 1L
    })
    observeEvent(input$rotate_visiting, {
        rotate_teams$visiting <- 1L
    })

    observeEvent(input$court_inset_swap, {
        court_inset_home_team_end(other_end(court_inset_home_team_end()))
        dojs("document.getElementById('court_inset_swap').blur();") ## un-focus from button
    })
    accept_ball_coords <- reactiveVal(0L)
    observeEvent(input$validate_ball_coords, {
        accept_ball_coords(isolate(accept_ball_coords()) + 1L)
    })
    return(list(rt = rotate_teams, ball_coords_checkbox = ball_coords, accept_ball_coords = accept_ball_coords, click_points = click_points, add_to_click_queue = add_to_click_queue, clear_click_queue = clear_click_queue, home_team_end = court_inset_home_team_end))
}
