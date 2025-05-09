## create a resize observer that watches an element and fires js code when it changes size
## id is the id of the element to observe
## fun is a string of the js to run on resize
## debounce can be used to debounce excessive executions, requires that dbnc is defined in the UI
resize_observer <- function(id, fun, nsfun, debounce = 0, as = "tag") {
    as <- match.arg(as, c("tag", "string")) ## "string" is just the js code as a string, "tag" is wrapped in tags$script(HTML(...))
    if (missing(nsfun)) nsfun <- function(x) paste0(id, "_", x)
    obsfun <- nsfun("rsz_obs") ## name of the observer function
    ## if the observer function has not yet been defined, and the element to observe exists, then create the observer function
    js <- paste0("if (typeof ", obsfun, " === 'undefined' && document.getElementById('", id, "')) {")
    if (debounce > 0) {
        js <- paste0(js, " const ", obsfun, " = new ResizeObserver(dbnc(() => { ", fun, "}, ", debounce, "));")
    } else {
        js <- paste0(js, " const ", obsfun, " = new ResizeObserver(() => { ", fun, " }); ")
    }
    js <- paste0(js, " ", obsfun, ".observe(document.getElementById('", id, "')); }")
    if (as == "tag") tags$script(HTML(js)) else paste0(js, ";")
}

player_constructor_js <- function(id = "main_video", app_data, autoplay = FALSE, muted = TRUE, height = NA, width = NA, ready_extra) {
    yt <- isTRUE(is_youtube_url(app_data$video_src)) || isTRUE(!is.null(app_data$video_src2) && is_youtube_url(app_data$video_src2))
    ## muted can be TRUE/FALSE or a string giving a (js) variable name. Ignored if NA
    if (!is.na(muted) && is.logical(muted)) muted <- tolower(isTRUE(muted))
    if (is.numeric(height) && !isTRUE(height > 0)) height <- NA
    if (is.numeric(width) && !isTRUE(width > 0)) width <- NA
    paste0("vidplayer = videojs('", id, "', ",
           ## options
           "{'techOrder': ['html5'", if (yt) ", 'youtube'", "], ",
           "'controls': true, 'autoplay': ", tolower(isTRUE(autoplay)), ", 'preload': 'auto', 'liveui': true, 'restoreEl': true, 'inactivityTimeout': 0, ",
           if (!is.na(height)) paste0("'height':", height, ", "),
           if (!is.na(width)) paste0("'width':", width, ", "),
           if (!is.na(muted)) paste0("'muted': ", muted, ", "),
           ## sources
           "'sources': ", if (yt) {
                              paste0("[{ 'type': 'video/youtube', 'src': '", app_data$video_src, "'}]")
                          } else {
                              paste0("[{ 'src': '", if (is_url(app_data$video_src)) app_data$video_src else file.path(app_data$video_server_base_url, basename(app_data$video_src)), "'",
                                     if (isTRUE(app_data$live)) paste0(" + '?ovslive=' + new Date().getTime()"),
                                     if (grepl("m3u8$", app_data$video_src)) paste0(", 'type': 'application/x-mpegURL'"), ## otherwise let videojs guess it
                                     "}]")
                          },
           " });\n", ## end options
           ## other setup
           ## "vidplayer.reloadSourceOnError({'errorInterval':5});\n",
           paste0("vidplayer.ready(function() {\n  console.log('VIDPLAYER READY'); ",
                  if (!missing(ready_extra)) sub(";+$", "; ", paste0(stringr::str_trim(ready_extra), ";")),
                  "  Shiny.setInputValue('video_width', vidplayer.videoWidth()); Shiny.setInputValue('video_height', vidplayer.videoHeight());\n});"))
}

build_ovscout2_js <- function(app_data) {
    myjsfile <- file.path(app_data$assets_dir, "ovscout2.js")
    myjs <- paste0(readLines(system.file("extdata/js/ovscout2.js", package = "ovscout2", mustWork = TRUE)), collapse = "\n") ## key press handling
    if (isTRUE(app_data$live)) {
        ## function to dispose of the player and reload it
        myjs <- paste(myjs, "vidplayer_reload_fun = function() {",
                      "  var ct = vidplayer.currentTime(); var pw = vidplayer.width(); var ph = vidplayer.height(); var wasmuted = vidplayer.muted();", ## note some current settings
                      ## dispose of the player and reconstruct it. Note that this gives a flicker as the element is removed and replaced
                      "  vidplayer.dispose();",
                      player_constructor_js(id = "main_video", app_data = app_data, autoplay = TRUE, muted = "wasmuted", height = "ph", width = "pw", ready_extra = "vidplayer.currentTime(ct); vidplayer.play();"),
                      "  vidplayer.one('play', () => { vidplayer_near_end_fun(); });", ## once the player restarts playing, attach the timeupdate watcher
                      "}", sep = "\n")
        ## define the vidplayer_near_end_fun, which reinitializes the player when it nears the file end
        myjs <- paste(myjs, "vidplayer_near_end_fun = function() {",
                      "  if (vidplayer.currentTime() >= (vidplayer.duration() - 5)) {",
                      "    console.log('too close to end of new video');",
                      "    setTimeout(vidplayer_reload_fun, 3000);", ## try again in a bit
                      "  } else {",
                      "    vidplayer.on('timeupdate', function(event) {",
                      "      if (this.currentTime() >= (this.duration() - 5)) {",
                      "        console.log('video near end'); Shiny.setInputValue('video_near_end', true, { priority: 'event' });",
                      "        vidplayer_reload_fun();",
                      "      }",
                      "    });",
                      "  }",
                      "}", sep = "\n")
    }
    if (app_data$with_video) {
        myjs <- paste(myjs, "$(document).on('shiny:sessioninitialized', function() {",
                      resize_observer("review_player", fun = "Shiny.setInputValue('rv_height', $('#review_player').innerHeight()); Shiny.setInputValue('rv_width', $('#review_player').innerWidth());", debounce = 100, as = "string"),
                      player_constructor_js(id = "main_video", app_data = app_data),
                      if (isTRUE(app_data$live)) "vidplayer_near_end_fun();", ## if live, attach the vidplayer_near_end_fun
                      resize_observer("main_video", fun = "$('#video_overlay').css('height', $('#main_video').innerHeight() + 'px'); document.getElementById('video_overlay_canvas').height = $('#main_video').innerHeight(); document.getElementById('video_overlay').style.width = $('#main_video').innerWidth() + 'px'; document.getElementById('video_overlay_canvas').width = $('#main_video').innerWidth(); Shiny.setInputValue('dv_height', $('#main_video').innerHeight()); Shiny.setInputValue('dv_width', $('#main_video').innerWidth()); document.getElementById('video_overlay').style.marginTop = '-' + $('#video_holder').innerHeight() + 'px'; document.getElementById('video_overlay_canvas').style.marginTop = '-' + $('#video_holder').innerHeight() + 'px';", debounce = 100, as = "string"), ";",
                      "pause_main_video_on_click = false;",
                      if (app_data$scout_mode != "type") {
                          paste0("$('#video_overlay').on('click', () => {",
                                 "    if (pause_main_video_on_click) {",
                                 "        vidplayer.pause();",
                                 "        pause_on_type = 0;",
                                 "    }",
                                 "});")
                      },
                      ## add listeners to the whole window to catch changes of focus
                      "function focus_check() { Shiny.setInputValue('focus_check', { is_body: document.activeElement === document.body, id: document.activeElement.id, class: document.activeElement.classList }); };",
                      "window.addEventListener ? window.addEventListener('focus', focus_check, true) : window.attachEvent('onfocusout', focus_check);",
                      "window.addEventListener ? window.addEventListener('blur', focus_check, true) : window.attachEvent('onblur', focus_check);",
                      ## vidplayer.on('loadedmetadata', () => { console.log('METADATA READY'); }); vidplayer.on('ready', () => { console.log('ONREADY') });
                      "});", sep = "\n")
    }
    cat(myjs, file = myjsfile, sep = "\n")
    ## cat("assets js:", myjsfile, "\n") ## for debugging
    paste0("assets/", basename(myjsfile)) ## return the relative url
}
