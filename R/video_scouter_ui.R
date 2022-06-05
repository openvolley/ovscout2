ov_scouter_ui <- function(app_data) {
    ## some startup stuff
    running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
    fluidPage(theme = shinythemes::shinytheme("lumen"),
              htmltools::findDependencies(shiny::selectizeInput("foo", "bar", choices = "a")), ## workaround for https://github.com/rstudio/shiny/issues/3125
              tags$script("Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) });"), ## handler for running js code directly
              rintrojs::introjsUI(),
              ovideo::ov_video_js(youtube = FALSE, version = 2), ## for the review pane
              tags$head(tags$style("body{font-size:15px} .well{padding:15px;} .myhidden {display:none;} table {font-size: small;} h2, h3, h4 {font-weight: bold;} .shiny-notification { height: 100px; width: 400px; position:fixed; top: calc(50% - 50px); left: calc(50% - 200px); } .code_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:70%;} .sub_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:40%;} .lineup_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:40%;} .clet {color: red;} .iconbut { font-size: 150%; } #rallystate { position: absolute; font-size: large; color: yellow; background-color: #0000C080; z-index:20; -webkit-transform:translateZ(20); }"),
                        tags$style("#headerblock {border-radius:14px; padding:10px; margin-bottom:5px; min-height:120px; color:black; border: 1px solid #000766; background:#000766; background: linear-gradient(90deg, rgba(0,7,102,1) 0%, rgba(255,255,255,1) 65%, rgba(255,255,255,1) 100%);} #headerblock h1, #headerblock h2, #headerblock h3, #headerblock h4 {color:#fff;} .btn, .btn:hover, .btn.active { border-width:1px; } #c3_other_outer { height:7vh; border-width:1px; padding-left:2px; padding-right:2px; border-radius:4px; background-color:white; } #c3_other_outer.active { background-color:#d5d5d5; }"),
                        tags$style(paste0(".libero {background-color:", app_data$styling$libero, "; border-color:", app_data$styling$libero_light, "} .libero.active {background-color:", app_data$styling$libero_dark, "; border-color:", app_data$styling$libero, "} .libero.active:hover, .libero:hover {background-color:", app_data$styling$libero_light, "; border-color:", app_data$styling$libero, "} .fatradio {width:100%; height:7vh;}")),## .fatradio:focus, .fatradio:hover {background-color:#FFFFFF;}")),
                        tags$style("#video_overlay, #video_overlay_img { -webkit-backface-visibility: hidden; -webkit-transform: translateZ(0); } #problem_ui {position:absolute; left:25%; width:50%; top:10%; -webkit-transform:translateZ(15); z-index:15;}"), ## stop chrome putting the overlay underneath the video
                        tags$style(paste0(".undo {background-color:", app_data$styling$undo, "; border-color:", app_data$styling$undo_light, "} .undo:hover {background-color:", app_data$styling$undo_light, "; border-color:", app_data$styling$undo, "} .continue {background-color:", app_data$styling$continue, "; border-color:", app_data$styling$continue_light, "} .continue:hover {background-color:", app_data$styling$continue_light, "; border-color:", app_data$styling$continue, "} .cancel {background-color:", app_data$styling$cancel, "; border-color:", app_data$styling$cancel_light, "} .cancel:hover {background-color:", app_data$styling$cancel_light, "; border-color:", app_data$styling$cancel, "}")),
                        if (!is.null(app_data$css)) tags$style(app_data$css),
                        tags$link(href = if (running_locally) "css/video-js.min.css" else "//vjs.zencdn.net/7.10.2/video-js.min.css", rel = "stylesheet"),
                        tags$script(src = if (running_locally) "js/video.min.js" else "//vjs.zencdn.net/7.10.2/video.min.js"),
                        tags$style(".video-js .vjs-big-play-button { display: none; } .bareslider { display:inline-block; margin-left:4px; margin-right:4px;} .bareslider .irs-max, .bareslider .irs-min, .bareslider .irs-single, .bareslider .irs-from, .bareslider .irs-to { display:none; } .bareslider .irs-handle { top:0px; } .bareslider .irs-line { top:7px;} .bareslider .irs-bar {top:8px;}"),
                        ##key press handling
                        tags$script("$(document).on('keypress', function (e) { var el = document.activeElement; var len = -1; if (typeof el.value != 'undefined') { len = el.value.length; }; Shiny.setInputValue('cmd', e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('keydown', function (e) { var el = document.activeElement; var len = -1; if (typeof el.value != 'undefined') { len = el.value.length; }; Shiny.setInputValue('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('keyup', function (e) { var el = document.activeElement; var len = -1; if (typeof el.value != 'undefined') { len = el.value.length; }; Shiny.setInputValue('controlkeyup', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime()); });"),
                        tags$script("document.addEventListener('click', function (e) { Shiny.setInputValue('shiftkey', e.shiftKey) }); $(document).on('shiny:sessioninitialized',function() { Shiny.setInputValue('window_height', $(window).innerHeight()); Shiny.setInputValue('window_width', $(window).innerWidth()); });"),
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
              tags$div(id = "review_pane", style = "position:absolute; top:20px; right:20px; width:27vw; -webkit-transform:translateZ(9999); z-index:9999; display:none;", ## start hidden
                            ovideo::ov_video_player(id = "review_player", type = "local", controls = FALSE, poster = "data:image/gif,AAAA", style = "border: 1px solid black; width: 100%;", muted = "true", onloadstart = "set_vspinner();", oncanplay = "remove_vspinner();", onerror = "review_player_onerror(event);")),
              fluidRow(column(9,
                              fluidRow(column(8, tags$div(actionButton("video_rew_10", label = "Back 10s", icon = icon("step-backward")),
                                       actionButton("video_pause", label = "Pause", icon = icon("pause-circle")),
                                       actionButton("video_ff_10", label = "Forward 10s", icon = icon("step-forward")),
                                       tags$div(class = "bareslider", sliderInput("video_volume", label = "Volume", min = 0, max = 1, value = 0, width = "60px", ticks = FALSE)),
                                       actionButton("video_toggle_mute", label = "Unmute", icon = icon("volume-mute"))
                                       )),
                                      column(4, if (!is.null(app_data$dvw$plays2)) downloadButton("save_rds_button", "Save file"), downloadButton("save_dvw_button", "Export to dvw")),
                                             ##column(2, shinyFiles::shinySaveButton("auto_save_file", label = "Auto save", title = "Save file as", filetype = "dvw"), tags$p(style = "font-size: small", "Auto save will automatically save a copy of the file after each rally"))
                                      ),
                              fluidRow(column(4, uiOutput("rally_state"))),
                              if (app_data$with_video)
                                  introBox(tags$div(id = "video_holder", style = "position:relative;",
                                                    if (app_data$scoreboard) tags$div(id = "tsc_outer", mod_teamscores_ui(id = "tsc", styling = app_data$styling)),
                                                    uiOutput("problem_ui"),
                                                    tags$video(id = "main_video", style = "max-width:100%;", class = "video-js", `data-setup` = "{ \"controls\": true, \"autoplay\": false, \"preload\": \"auto\", \"liveui\": true, \"muted\": true }",
                                                               tags$source(src = file.path(app_data$video_server_base_url, basename(app_data$video_src))), ##type="video/mp4"
                                                               tags$p(class = "vjs-no-js", "This app cannot be used without a web browser that", tags$a(href = "https://videojs.com/html5-video-support/", target = "_blank", "supports HTML5 video")))),
                                           tags$img(id = "video_overlay_img", style = "position:absolute;"), plotOutput("video_overlay", click = "video_click", dblclick = "video_dblclick"), data.step = 5, data.intro = "Video of the game to scout."),
                              fluidRow(column(12, uiOutput("serve_preselect"))),
                              fluidRow(column(8,
                                              ## some elements commented out for now - BR
                                              introBox(##actionButton("all_video_from_clock", label = "Open video/clock time operations menu", icon = icon("clock")),
                                                  mod_courtref_ui(id = "courtref"),
                                                  mod_match_data_edit_ui(id = "match_data_editor"),
                                                  mod_team_select_ui(id = "team_selector"),
                                                  mod_team_edit_ui(id = "team_editor"),
                                                  mod_lineup_edit_ui(id = "lineup_editor"),
                                                  data.step = 2, data.intro = "Click on these buttons if you want to edit the court reference, starting lineups, rosters, or match metadata. The court reference defines where the court is located in the video image.")
                                              )),
                              tags$div(style = "height: 14px;"),
                              fluidRow(column(5, actionButton("general_help", label = "General Help", icon = icon("question"), style="color: #fff; background-color: #B21212; border-color: #B21212"),
                                              actionButton("show_shortcuts", tags$span(icon("keyboard"), "Show keyboard shortcuts"), style="color: #fff; background-color: #B21212; border-color: #B21212"),
                                              sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1),
                                              uiOutput("show_courtref_ui")
                                              ),
                                       column(7, wellPanel(introBox(mod_teamslists_ui(id = "teamslists"), data.step = 1, data.intro = "Team rosters. Click on the 'Edit teams' button to change these.")))
                                       )
                              ),
                       column(3,
                              introBox(wellPanel(mod_courtrot2_ui(id = "courtrot", with_ball_coords = FALSE)), data.step = 3, data.intro = "Team lineups and on-court rotations."),
                              introBox(mod_playslist_ui("playslist", height = "35vh", styling = app_data$styling), data.step = 4, data.intro = "List of actions. New entries appear here as they are scouted."),
                              uiOutput("error_message"))
                       ),
tags$script("set_vspinner = function() { $('#review_player').addClass('loading'); }"),
tags$script("remove_vspinner = function() { $('#review_player').removeClass('loading'); }"),
tags$style("video.loading { background: black; }"),
tags$script("review_player_onerror = function(e) { $('#review_player').removeClass('loading'); try { var this_src = btoa(document.getElementById(e.target.id).getAttribute('src')); } catch { var this_src = ''; }; Shiny.setInputValue('video_error', e.target.id + '@' + this_src + '@' + e.target.error.code + '@' + new Date().getTime()); }"),
tags$script("vidplayer = videojs('main_video'); revpl = new dvjs_controller('review_player','local',true);  revpl.video_onfinished = function() { revpl.video_controller.current=0; revpl.video_play(); }")
)
}
