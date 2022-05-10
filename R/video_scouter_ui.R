ov_scouter_ui <- function(app_data) {
    ## some startup stuff
    running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
    fluidPage(theme=if (running_locally) "spacelab.css" else "https://cdnjs.cloudflare.com/ajax/libs/bootswatch/3.3.7/spacelab/bootstrap.min.css",
              htmltools::findDependencies(shiny::selectizeInput("foo", "bar", choices = "a")), ## workaround for https://github.com/rstudio/shiny/issues/3125
              tags$script("Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) });"), ## handler for running js code directly
              rintrojs::introjsUI(),
              tags$head(tags$style("body{font-size:15px} .well{padding:15px;} .myhidden {display:none;} table {font-size: small;} h2, h3, h4 {font-weight: bold;} .shiny-notification { height: 100px; width: 400px; position:fixed; top: calc(50% - 50px); left: calc(50% - 200px); } .code_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:70%;} .sub_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:40%;} .lineup_entry_guide {color:#31708f; background-color:#d9edf7;border-color:#bce8f1;padding:4px;font-size:40%;} .clet {color: red;} .iconbut { font-size: 150%; } #rallystate { position: absolute; font-size: large; color: yellow; margin-top: -50px; background-color: #0000C080; }"),
                        tags$style("#headerblock {border-radius:14px; padding:10px; margin-bottom:5px; min-height:120px; color:black; border: 1px solid #000766; background:#000766; background: linear-gradient(90deg, rgba(0,7,102,1) 0%, rgba(255,255,255,1) 65%, rgba(255,255,255,1) 100%);} #headerblock h1, #headerblock h2, #headerblock h3, #headerblock h4 {color:#fff;}"),
                        tags$style("#hroster {padding-left: 0px; padding-right: 0px; background-color: #bfefff; padding: 12px;} #vroster {padding-left: 0px; padding-right: 0px; background-color: #bcee68; padding: 12px;} .libero {background-color:yellow;} .fatradio {width:100%; height:7vh;}"),
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
                                       tags$button(tags$span(icon("pause-circle", style = "vertical-align:middle;")), onclick = "Shiny.setInputValue('pause_trigger', new Date().getTime());", title = "Pause"),
                                       #tags$button(tags$span(icon("stop-circle", style = "vertical-align:middle;")), onclick = paste0(cstr, "video_stop();"), title = "Stop"),
                                       ),
                              if (app_data$with_video) introBox(tags$div(id = "video_holder", style = "position:relative;", tags$video(id = "main_video", style = "border: 1px solid black; width: 90%;", src = file.path(app_data$video_server_base_url, basename(app_data$video_src)), autoplay = "false")), tags$img(id = "video_overlay_img", style = "position:absolute;"), plotOutput("video_overlay", click = "video_click", dblclick = "video_dblclick"), data.step = 4, data.intro = "Video of the game to scout."), ##controls = "controls",
                              fluidRow(column(4, offset = 8, uiOutput("rally_state"))),
                              fluidRow(column(12, uiOutput("serve_preselect"))),
                              fluidRow(column(8,
                                              ## some elements commented out for now - BR
                                              introBox(##actionButton("all_video_from_clock", label = "Open video/clock time operations menu", icon = icon("clock")),
                                                  mod_match_data_edit_ui(id = "match_data_editor"),
                                                  mod_team_edit_ui(id = "team_editor"),
                                                  mod_lineup_edit_ui(id = "lineup_editor"),
                                                  data.step = 3, data.intro = "Click on these action buttons if you want to edit the starting lineups, edit the rosters, or edit the match metadata.")
                                              )),
                              tags$div(style = "height: 14px;"),
                              fluidRow(column(5, actionButton("general_help", label = "General Help", icon = icon("question"), style="color: #fff; background-color: #B21212; border-color: #B21212"),
                                              actionButton("show_shortcuts", tags$span(icon("keyboard"), "Show keyboard shortcuts"), style="color: #fff; background-color: #B21212; border-color: #B21212"),
                                              sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1),
                                              mod_courtref_ui(id = "courtref"),
                                              uiOutput("show_overlay_ui")
                                              ),
                                       column(7, wellPanel(mod_teamslists_ui(id = "teamslists")))
                                       )
                              ),
                       column(3,
                              introBox(wellPanel(mod_courtrot2_ui(id = "courtrot", with_ball_coords = FALSE)), data.step = 2, data.intro = "Team rosters and oncourt rotation."),
                              introBox(mod_playslist_ui("playslist"), data.step = 1, data.intro = "List of events. Existing events can be edited or deleted. New events can be added. They will appear here."),
                              uiOutput("error_message"))
                       )
              )
}
