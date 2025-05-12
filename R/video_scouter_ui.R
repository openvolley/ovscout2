
ov_video_ui_element <- function(app_data, yt) {
    tagList(fluidRow(column(4, uiOutput("rally_state"))),
            introBox(tags$div(id = "video_holder", style = "position:relative;",
                              if (app_data$scoreboard) tags$div(id = "tsc_outer", mod_teamscores_ui(id = "tsc")),
                              tags$video(id = "main_video", class = "video-js vjs-has-started", crossorigin = "anonymous", tags$p(class = "vjs-no-js", "This app cannot be used without a web browser that", tags$a(href = "https://videojs.com/html5-video-support/", target = "_blank", "supports HTML5 video")))
                              ),
                     tags$canvas(id = "video_overlay_canvas", style = "position:absolute;", height = "400", width = "600"), plotOutput("video_overlay"), data.step = 3, data.intro = "Video of the game to scout."),
            tags$div(style = "height:32px;")) ## this is allowing for the video control bar to be placed below the video. The control bar is sized at 3em relative to the bar font size (10px)
}


ov_scouter_ui <- function(app_data) {
    ## some startup stuff
    running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
    yt <- isTRUE(is_youtube_url(app_data$video_src)) || isTRUE(!is.null(app_data$video_src2) && is_youtube_url(app_data$video_src2))
    dash <- isTRUE(app_data$dash)
    fluidPage(theme = shinythemes::shinytheme("lumen"),
              htmltools::findDependencies(shiny::selectizeInput("foo", "bar", choices = "a")), ## workaround for https://github.com/rstudio/shiny/issues/3125
              tags$script("Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) });"), ## handler for running js code directly
              rintrojs::introjsUI(),
              ovideo::ov_video_js(youtube = yt, version = 2), ## for the review pane
              tags$head(## tags$meta(`http-equiv` = "Content-Security-Policy", content = "default-src * 'self' 'unsafe-inline' 'unsafe-eval' data: gap: content: blob:"), ## does this help with m3u8 issues? ## worker-src blob:; child-src blob: gap:; img-src 'self' blob: data:;
                       tags$link(href = "css/ovscout2.css", rel = "stylesheet"),
                       tags$style(paste0(".btn, .btn:hover, .btn.active { border-width:1px; font-size: ", app_data$styling$button_font_size , ";} .libero {background-color:", app_data$styling$libero_colour, "; border-color:", app_data$styling$libero_light_colour, "} .libero.active {background-color:", app_data$styling$libero_dark_colour, "; border-color:", app_data$styling$libero_colour, "} .libero.active:hover, .libero:hover {background-color:", app_data$styling$libero_light_colour, "; border-color:", app_data$styling$libero_colour, "} .homebut { background-color:", app_data$styling$h_court_colour, "} .homebut:hover, .homebut:active { background-color:", app_data$styling$h_court_light_colour, "} .visbut { background-color:", app_data$styling$v_court_colour, "} .visbut:hover, .visbut:active { background-color:", app_data$styling$v_court_light_colour, "}")),## .fatradio:focus, .fatradio:hover {background-color:#FFFFFF;}")),
                       tags$style(paste0(".undo {background-color:", app_data$styling$undo_colour, "; border-color:", app_data$styling$undo_light_colour, "} .undo:hover {background-color:", app_data$styling$undo_light_colour, "; border-color:", app_data$styling$undo_colour, "} .continue {background-color:", app_data$styling$continue_colour, "; border-color:", app_data$styling$continue_light_colour, "} .continue:hover {background-color:", app_data$styling$continue_light_colour, "; border-color:", app_data$styling$continue_colour, "} .cancel {background-color:", app_data$styling$cancel_colour, "; border-color:", app_data$styling$cancel_light_colour, "} .cancel:hover {background-color:", app_data$styling$cancel_light_colour, "; border-color:", app_data$styling$cancel_colour, "}")),
                       if (!is.null(app_data$css)) tags$style(app_data$css),
                       tags$link(href = if (running_locally) "css/video-js.min.css" else "//vjs.zencdn.net/8.3.0/video-js.min.css", rel = "stylesheet"),
                       tags$script(src = if (running_locally) "js/video.min.js" else "//vjs.zencdn.net/8.3.0/video.min.js"),
                       if (dash) tags$script(src = if (running_locally) "js/dash.all.min.js" else "//cdnjs.cloudflare.com/ajax/libs/dashjs/4.7.1/dash.all.min.js"),
                       if (dash) tags$script(src = if (running_locally) "js/videojs-dash.min.js" else "//cdnjs.cloudflare.com/ajax/libs/videojs-contrib-dash/5.1.1/videojs-dash.min.js"),
                       if (yt) tags$script(src = "https://cdn.jsdelivr.net/npm/videojs-youtube@2.6.1/dist/Youtube.min.js"), ## for youtube
                        tags$script(src = build_ovscout2_js(app_data)),
                        tags$title("Volleyball scout and video sync")
                        ),
              if (!is.null(app_data$ui_header)) {
                  app_data$ui_header
              } else {
                  fluidRow(id = "headerblock", column(6, tags$h2("Volleyball scout")),
                           column(3, offset = 3, tags$div(style = "text-align: center;", "Part of the", tags$br(), tags$img(src = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiMwMDA3NjYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwNzY2IiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+", style = "max-height:3em;"), tags$br(), tags$a(href = "https://github.com/openvolley", "openvolley", target = "_blank"), "project")))
              },
              tags$div(id = "review_pane", style = paste0("position:absolute; top:30px; right:0; width:", app_data$styling$review_pane_width, "vw; -webkit-transform:translateZ(9998); z-index:9998; display:none;"), ## start hidden
                       ovideo::ov_video_player(id = "review_player", type = "local", controls = FALSE, poster = "data:image/gif,AAAA", style = "border: 1px solid black; width: 100%;", muted = "true", onloadstart = "set_vspinner();", oncanplay = "remove_vspinner();", onerror = "review_player_onerror(event);"),
                       tags$canvas(id = "review_overlay_canvas", style = "position:absolute;", height = "125", width = "200"),
                       plotOutputWithAttribs("review_overlay", width = "100%", height = "100%", click = "rv_click", hover = shiny::hoverOpts("rv_hover", delay = 50, delayType = "throttle"), onmouseup = "Shiny.setInputValue('did_rv_mouseup', new Date().getTime());", onmousedown = "Shiny.setInputValue('did_rv_mousedown', new Date().getTime());")),
              fluidRow(column(1, tags$div(id = "bsbar",
                                          if (app_data$with_video) {
                                              introBox(sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1), data.step = 4, data.intro = "Video controls. Also can be controlled by keyboard shortcuts.")
                                          },
                                          introBox(##actionButton("all_video_from_clock", label = "Open video/clock time operations menu", icon = icon("clock")),
                                              if (app_data$with_video) {
                                                  if (!is.null(app_data$video_src2)) {
                                                      tags$div(style = "display:inline-block;",
                                                               mod_courtref_ui(id = "courtref1", yt = isTRUE(is_youtube_url(app_data$video_src)), video_url = if (is_url(app_data$video_src)) app_data$video_src else file.path(app_data$video_server_base_url, basename(app_data$video_src))),
                                                               mod_courtref_ui(id = "courtref2", yt = is_youtube_url(app_data$video_src2), video_url = if (is_url(app_data$video_src2)) app_data$video_src2 else file.path(app_data$video_server_base_url, basename(app_data$video_src2)), button_label = HTML("Court reference<br />(video 2)")),
                                                               actionButton("v2_offset", "Video 2 time offset"), class = "leftbut")
                                                  } else {
                                                      mod_courtref_ui(id = "courtref1", yt = isTRUE(is_youtube_url(app_data$video_src)), video_url = if (is_url(app_data$video_src)) app_data$video_src else file.path(app_data$video_server_base_url, basename(app_data$video_src)))
                                                  }
                                              },
                                              mod_match_data_edit_ui(id = "match_data_editor"),
                                              mod_team_select_ui(id = "team_selector"),
                                              mod_team_edit_ui(id = "team_editor"),
                                              mod_lineup_edit_ui(id = "lineup_editor"),
                                              if (!is.null(app_data$video_src2)) actionButton("switch_video", "Switch video", class = "leftbut"),
                                              data.step = 2, data.intro = "Click on these buttons if you want to edit the court reference, starting lineups, rosters, or match metadata. The court reference defines where the court is located in the video image."),
                                          tags$hr(),
                                          tags$div(downloadButton("save_rds_button", "Save file", class = "leftbut"),
                                                   uiOutput("save_dvw_button_ui"),
                                                   uiOutput("reports_ui")),
                                          tags$hr(),
                                          introBox(actionButton("general_help", label = "General Help", icon = icon("question"), class = "leftbut"),
                                                   tags$button(class = "btn btn-default leftbut", tags$span(icon("book-open"), "User manual"), onclick = "window.open('https://ovscout2.openvolley.org/articles/ovscout2-user-manual.html', '_blank')"),
                                                   actionButton("preferences", "Preferences", class = "leftbut"),
                                                   actionButton("show_shortcuts", tags$span(icon("keyboard"), "Keyboard", tags$br(), "shortcuts"), class = "leftbut"),
                                                   actionButton("switch_scout_mode", label = tags$span("Change", tags$br(), "scout mode"), class = "leftbut"),
                                                   data.step = 7, data.intro = "Set general preferences, and see the keyboard shortcuts.")
                                          )),
                       column(9, style = "padding-right:2px;",
                              fixedRow(tags$div(id = "video_col", ov_video_ui_element(app_data, yt),
                                                uiOutput("scout_bar_with_video")),
                                       tags$div(id = "court1_col", uiOutput("court1_col_ui"))),
                              fixedRow(column(12, uiOutput("serve_preselect"))),
                              tags$div(style = "height: 8px;"),
                              uiOutput("teamslist_in_click_mode")
                              ),
                       column(2, style = "padding-left:5px; padding-right:5px",
                              uiOutput("court2_ui"),
                              uiOutput("problem_ui"), uiOutput("no_court_ref_ui"),
                              tags$div(id = "code_edit_buttons", style = "height:34px;",
                                       actionButton("edit_code_insert", class = "cebut", label = tags$span(icon("plus")), title = "Insert new code above current"),
                                       actionButton("edit_code_delete", class = "cebut", label = tags$span(icon("trash-can")), title = "Delete current"),
                                       actionButton("edit_code_edit", class = "cebut", label = tags$span(icon("pen-to-square")), title = "Edit current"),
                                       actionButton("edit_code_edit_coords", class = "cebut", label = tags$span(icon("location-dot")), title = "Edit location of current")),
                              uiOutput("code_edit_dialog"),
                              introBox(mod_playslist_ui("playslist", height = if (app_data$scout_mode == "type") "85vh" else "50vh", styling = app_data$styling), data.step = 6, data.intro = "List of actions. New entries appear here as they are scouted.")##, uiOutput("error_message")
                              )
                       ),
              tags$link(href = "css/post.css", rel = "stylesheet"),
              ## not used? tags$script("review_player_onerror = function(e) { $('#review_player').removeClass('loading'); try { var this_src = btoa(document.getElementById(e.target.id).getAttribute('src')); } catch(err) { var this_src = ''; }; Shiny.setInputValue('video_error', e.target.id + '@' + this_src + '@' + e.target.error.code + '@' + new Date().getTime()); }"),
              tags$script(paste0("revpl = new dvjs_controller('review_player','", if (yt) "youtube" else "local", "',true);  revpl.video_onfinished = function() { revpl.video_controller.current=0; revpl.video_play(); }"))
              )
}
