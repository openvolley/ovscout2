## the UI element includes the crholder div, which holds the video and/or court image, and (if video) to which the videojs player is attached
## need to keep this in existence all the time, else the player stops working (if the player is attached to an element in a modal, that gets destroyed when the modal gets closed?)
mod_courtref_ui <- function(id, yt = FALSE, video_url, button_label = "Court reference") {
    ns <- NS(id)
    jsns <- ns4js(ns)
    show_frame_image <- is.null(video_url) ## extract a frame and show that, otherwise show the actual video
    tagList(actionButton(ns("do_scref"), button_label, class = "leftbut"),
            tags$div(id = ns("crholder"), style = "position:absolute; width:65vw; left:0; top:38px; z-index:9990; display:none;",
                     if (!show_frame_image) HTML(paste0("<video id=\"", ns("cr_player"), "\" style=\"width:100%; height:70vh;\" class=\"video-js vjs-has-started\" crossorigin=\"anonymous\" data-setup='{ ", if (yt) "\"techOrder\": [\"youtube\"], ", "\"controls\": true, \"autoplay\": false, \"loop\": true, \"preload\": \"auto\", \"liveui\": true, \"muted\": true, \"sources\": [{", if (yt) " \"type\": \"video/youtube\", ", "\"src\": \"", video_url, "\"}] }'>\n",
                                                        "<p class=\"vjs-no-js\">This app cannot be used without a web browser that <a href=\"https://videojs.com/html5-video-support/\" target=\"_blank\">supports HTML5 video</a></p></video>")),
                     plotOutputWithAttribs(ns("srplot"),
                                           click = ns("sr_plot_click"), hover = shiny::hoverOpts(ns("sr_plot_hover"), delay = 50, delayType = "throttle"), onmouseup = paste0("Shiny.setInputValue('", ns("did_sr_plot_mouseup"), "', new Date().getTime());"), onmousedown = paste0("Shiny.setInputValue('", ns("did_sr_plot_mousedown"), "', new Date().getTime());"), style = "margin-top:-600px; position:relative; z-index:9999;")
                     ),
            ## instantiate the player
            if (!show_frame_image) tags$head(tags$script(HTML(
              paste0("$(document).on('shiny:sessioninitialized', function() { ", jsns("crpl"), " = videojs('", ns("cr_player"), "'); ",
                     jsns("crpl"), ".ready(function() {",
                     "Shiny.setInputValue('", ns("cr_media_height"), "', ", jsns("crpl"), ".videoHeight());",
                     "Shiny.setInputValue('", ns("cr_media_width"), "', ", jsns("crpl"), ".videoWidth());",
                     resize_observer(ns("cr_player"), fun = paste0(
                       "Shiny.setInputValue('", ns("cr_media_height"), "', ", jsns("crpl"), ".videoHeight());",
                       "Shiny.setInputValue('", ns("cr_height"), "', ", jsns("crpl"), ".currentHeight());",
                       "Shiny.setInputValue('", ns("cr_media_width"), "', ", jsns("crpl"), ".videoWidth());",
                       "var voff = $('#", ns("cr_player"), "').innerHeight(); document.getElementById('", ns("srplot"), "').style.marginTop = '-' + voff + 'px';"), nsfun = jsns, as = "string"), "});  });"))))
            )
}

## main_video_time_js is a string giving the js command needed to retrieve the current time of the main video
mod_courtref <- function(input, output, session, video_file = NULL, video_url = NULL, detection_ref, include_net = FALSE, styling, main_video_time_js) {
    ns <- session$ns
    jsns <- ns4js(ns)
    did_sr_popup <- reactiveVal(0L)
    active <- reactiveVal(FALSE)
    show_frame_image <- is.null(video_url) ## extract a frame and show that, otherwise show the actual video
    yt <- !show_frame_image && is_youtube_url(video_url)
    do_get_video_time <- !missing(main_video_time_js) && nzchar(main_video_time_js)
    observeEvent(input$do_scref, {
        ## trigger the crvt data to be re-initialized each time a popup is spawned
        did_sr_popup(did_sr_popup() + 1L)
        js_show2(ns("crholder"))
        active(TRUE)
        ## request video time of current source
        if (do_get_video_time) dojs(paste0("Shiny.setInputValue('", ns("cr_main_video_time"), "', ", main_video_time_js, ");"))
        showModal(vwModalDialog(title = "Set up court reference", uiOutput(ns("srui")),
                                footer = fluidRow(column(4, uiOutput(ns("sr_save_ui"))), column(4, uiOutput(ns("sr_save_dialog"))),
                                                  column(1, offset = 2, actionButton(ns("sr_cancel"), "Cancel", class = "fatradio cancel")), column(1, uiOutput(ns("sr_apply_ui"), inline = TRUE))), width = 100))
    })

    observeEvent(input$cr_main_video_time, {
        cat("main video time is: ", input$cr_main_video_time, "\n")
        dojs(paste0(jsns("crpl"), ".currentTime(", input$cr_main_video_time, ");"))
    })

    observeEvent(input$sr_cancel, {
        dojs(paste0(jsns("crpl"), ".stop();"))
        js_hide2(ns("crholder"))
        active(FALSE)
        removeModal()
    })

    output$sr_save_ui <- renderUI({
        ## only if running locally, with a video file provided, and only if we have ffmpeg available
        ## this also won't work if the video file is locked, which seems to happen with windows (?)
        ## disable pending further testing
        if (FALSE) {##(!is.null(video_file) && cr_is_ok() && !nzchar(Sys.getenv("SHINY_PORT")) && ovideo::ov_ffmpeg_ok(do_error = FALSE)) {
            ## check if the file already has court data saved into it
            chk <- tryCatch(!is.null(ovideo::ov_get_video_data(video_file)), error = function(e) FALSE)
            output$sr_save_dialog <- renderUI(if (chk) tags$div("The video file already has court data saved in it.") else NULL)
            fluidRow(column(5, actionButton(ns("sr_save"), "Save into video file", class = "fatradio")),
                     column(5, "Note: this will overwrite the existing video file. The original file will be backed up first, but use at your own risk."))
        } else {
            output$sr_save_dialog <- renderUI(NULL)
            NULL
        }
    })
    observeEvent(input$sr_save, {
        output$sr_save_dialog <- renderUI({
            withProgress(message = "Backing up video file ...", {
                tryCatch({
                    fs::file_copy(video_file, paste0(video_file, ".bak"), overwrite = TRUE)
                    setProgress(value = 0.4, message = "Saving court data ...")
                    temp <- list(court_ref = dplyr::select(left_join(crvt$court[, c("image_x", "image_y", "pos")], court_refs_data[, c("court_x", "court_y", "pos")], by = "pos"), -"pos"),
                                 antenna = crvt$antenna, net_height = crvt$net_height, video_framerate = crvt$video_framerate, video_width = crimg()$width, video_height = crimg()$height)
                    ovideo::ov_set_video_data(video_file, obj = temp, replace = TRUE, overwrite = TRUE)
                    setProgress(0.9)
                    tags$div("Saved")
                }, error = function(e) {
                    tags$div("Could not save court reference into video file. The error message was:", conditionMessage(e))
                })
            })
        })
    })

    cr_is_ok <- reactive({
        length(na.omit(crvt$court$image_x)) == 4
    })

    output$sr_apply_ui <- renderUI({
        if (cr_is_ok()) actionButton(ns("sr_apply"), "Apply", class = "fatradio continue") else NULL
    })

    observeEvent(input$sr_apply, {
        ##cat(str(reactiveValuesToList(crvt)))
        ##cat(str(detection_ref()))
        temp <- detection_ref()
        ## transfer crvt values into detection_ref
        temp$court_ref <- dplyr::select(left_join(crvt$court[, c("image_x", "image_y", "pos")], court_refs_data[, c("court_x", "court_y", "pos")], by = "pos"), -"pos")
        temp$antenna <- crvt$antenna
        temp$net_height <- crvt$net_height
        temp$video_framerate <- crvt$video_framerate
        ## TODO possibly also allow video_height, video_width to be overridden?
        temp$video_width <- crimg()$width
        temp$video_height <- crimg()$height
        detection_ref(temp)
        dojs(paste0(jsns("crpl"), ".stop();"))
        js_hide2(ns("crholder"))
        active(FALSE)
        removeModal()
    })

    output$srui <- renderUI({
        blah <- did_sr_popup()
        fluidRow(column(8, tags$div(style = "min-height:68vh;")), ## strut to ensure height of modal, to keep modal background showing behind video
                 column(4, uiOutput(ns("srui_table")),
                        tags$hr(),
                        shiny::fixedRow(column(6, textInput(ns("sr_net_height"), label = "Net height (m):", value = if (!is.null(detection_ref()) && !is.null(detection_ref()$net_height) && !is.na(detection_ref()$net_height)) detection_ref()$net_height else "", width = "10ex")),
                                        ##column(6, textInput(ns("sr_video_framerate"), label = "Video frame rate:", value = if (!is.null(detection_ref()) && !is.null(detection_ref()$video_framerate) && !is.na(detection_ref()$video_framerate)) detection_ref()$video_framerate else "", width = "10ex"))
                                        )
                        ))
    })

    ## the possible court reference points
    ## don't include the top-of-net positions here, they will cause problems with the left join to floor positions
    court_refs_data <- tibble(pos = c("nlb", "nrb", "nl3", "nr3", "lm", "rm", "fl3", "fr3", "flb", "frb"),##, "lnt", "rnt"),
                              lab = c("Near left baseline corner", "Near right baseline corner", "Left end of near 3m line", "Right end of near 3m line", "Left end of the midline", "Right end of the midline", "Left end of far 3m line", "Right end of far 3m line", "Far left baseline corner", "Far right baseline corner"),##, "Left top of the net", "Right top of the net"),
                              court_x = c(0.5, 3.5, 0.5, 3.5, 0.5, 3.5, 0.5, 3.5, 0.5, 3.5),##, 0.5, 3.5),
                              court_y = c(0.5, 0.5, 2.5, 2.5, 3.5, 3.5, 4.5, 4.5, 6.5, 6.5))##, 3.5, 3.5))

    ## crvt holds the edited court ref data
    ## TODO add net_height, possible video width, height, framerate
    crvt <- reactiveValues(court = tibble(image_x = numeric(), image_y = numeric(), court_x = numeric(), court_y = numeric(), pos = character()),
                           antenna = tibble(image_x = numeric(), image_y = numeric(),  antenna = character(), where = character()),
                           net_height = NA_real_, video_height = NA_integer_, video_width = NA_integer_, video_framerate = NA_integer_)

    ## populate crvt each time a popup is instantiated, from detection_ref if it has data
    observeEvent(did_sr_popup(), {
        crvt$court <- if (!is.null(detection_ref()) && !is.null(detection_ref()$court_ref) && nrow(detection_ref()$court_ref) > 0) {
                          left_join(detection_ref()$court_ref, court_refs_data[, c("court_x", "court_y", "pos")], by = c("court_x", "court_y")) ## add pos col
                      } else {
                          tibble(image_x = rep(NA_real_, 4), image_y = NA_real_, court_x = NA_real_, court_y = NA_real_, pos = NA_character_)
                      }
        crvt$antenna <- if (!is.null(detection_ref()) && !is.null(detection_ref()$antenna) && nrow(detection_ref()$antenna) == 4)
                            detection_ref()$antenna
                            else
                                tibble(image_x = rep(NA_real_, 4), image_y = NA_real_,  antenna = c("left", "right", "right", "left"), where = c(rep("floor", 2), rep("net_top", 2)))
        crvt$net_height <- if (!is.null(detection_ref()) && !is.null(detection_ref()$net_height)) detection_ref()$net_height else NA_real_
        crvt$video_height <- if (!is.null(detection_ref()) && !is.null(detection_ref()$video_height)) detection_ref()$video_height else NA_integer_
        video_width <- if (!is.null(detection_ref()) && !is.null(detection_ref()$video_width)) detection_ref()$video_width else NA_integer_
        video_framerate <- if (!is.null(detection_ref()) && !is.null(detection_ref()$video_framerate)) detection_ref()$video_framerate else NA_integer_
    })

    ## TODO also allow crvt to be populated manually, i.e. without an existing detection_ref

    ## helper function to build dropdown inputs for ref positions
    cr_dropdown <- function(id, n, what = NULL) {
        chc <- setNames(court_refs_data$pos, court_refs_data$lab)
        def_sel <- c(1, 2, 10, 9, 6, 7, 11, 12)
        sel <- if (!is.null(what) && what %in% chc) what else chc[def_sel[n]]
        selectInput(ns(id), label = paste0("Reference point ", n), choices = chc, selected = sel, multiple = FALSE)
    }

    ## the table on the right of the UI with the ref position definitions
    output$srui_table <- renderUI({
        ## transfer crvt values into ui
        cr <- crvt$court
        ant <- crvt$antenna
        do.call(tags$div, c(##list(fluidRow(column(4, tags$strong("Reference point")), column(8, tags$strong("Location")))),
                              ## the four court ref points can vary
                              lapply(1:4, function(n) cr_dropdown(paste0("crdd", n), n = n, what = if (n <= nrow(cr)) cr$pos[n] else NULL)),
                              ## antenna points are fixed
                              if (isTRUE(include_net))
                                  list(tags$div(tags$strong("Reference point 5"), "Left end of the midline"),
                                       tags$div(tags$strong("Reference point 6"), "Right end of the midline"),
                                       tags$div(tags$strong("Reference point 7"), "Top of net at right antenna"),
                                       tags$div(tags$strong("Reference point 8"), "Top of net at left antenna"))
                          ))
    })
    ## watch these inputs
    observeEvent(input$crdd1, { if (nrow(crvt$court) > 0) crvt$court$pos[1] <- input$crdd1 })
    observeEvent(input$crdd2, { if (nrow(crvt$court) > 1) crvt$court$pos[2] <- input$crdd2 })
    observeEvent(input$crdd3, { if (nrow(crvt$court) > 2) crvt$court$pos[3] <- input$crdd3 })
    observeEvent(input$crdd4, { if (nrow(crvt$court) > 3) crvt$court$pos[4] <- input$crdd4 })
    observe({
        crvt$net_height <- if (!is.null(input$sr_net_height) && nzchar(input$sr_net_height)) as.numeric(input$sr_net_height) else NA_real_
    })
    observe({
        crvt$video_framerate <- if (!is.null(input$sr_video_framerate) && nzchar(input$sr_video_framerate)) as.numeric(input$sr_video_framerate) else NA_real_
    })

    observe({
        output$srplot <- renderPlot({
            antenna_colour <- "magenta"
            court_colour <- "red"
            out <- if (!is.null(crimg()$image)) {
                       ## plot in 0,1 norm coords
                       asp <- if (!is.null(crimg()$height) && !is.na(crimg()$height) && crimg()$height > 0 && !is.null(crimg()$width) && !is.na(crimg()$width) && crimg()$width > 0) crimg()$height/crimg()$width else 9/16
                       p <- ggplot2::ggplot(mapping = aes(x = .data$image_x, y = .data$image_y)) + ggplot2::coord_fixed(ratio = asp, xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
                       ## p <- p + geom_segment(data = data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), xend = c(1, 1, 0, 0), yend = c(0, 1, 1, 0)), aes(x = x, y = y, xend = xend, yend = yend), col = "green") ## to see the plotting area for debugging
                       if (show_frame_image) p <- p + ggplot2::annotation_custom(grid::rasterGrob(crimg()$image), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
                       ## convert our crvt (edited ref data) into the court overlay data to plot
                       crox <- tryCatch({
                           cr <- crvt$court
                           ## account for changes in dropdowns, i.e. the image location might now be assigned to a different court ref location
                           if (!is.null(cr)) cr <- left_join(dplyr::select(cr, -"court_x", -"court_y"), court_refs_data[, c("court_x", "court_y", "pos")], by = "pos")
                           out <- ovideo::ov_overlay_data(zones = FALSE, serve_zones = FALSE, space = "image", court_ref = cr, crop = TRUE)
                           out$courtxy <- dplyr::rename(out$courtxy, image_x = "x", image_y = "y")
                           out
                       }, error = function(e) NULL)
                       if (!is.null(crox)) {
                           p <- p + geom_segment(data = crox$courtxy, aes(xend = .data$xend, yend = .data$yend), color = court_colour, na.rm = TRUE) + ggplot2::theme_bw()
                       }
                       if (!is.null(crvt$court)) {
                           p <- p + geom_label(data = mutate(crvt$court, point_num = row_number()), ## double check that point_num always matches the UI inputs ordering
                                               aes(label = .data$point_num), color = "white", fill = court_colour, na.rm = TRUE)
                       }
                       if (isTRUE(include_net) && !is.null(crvt$antenna)) {
                           plotx <- mutate(crvt$antenna, n = case_when(.data$antenna == "left" & .data$where == "floor" ~ 5L,
                                                                       .data$antenna == "right" & .data$where == "floor" ~ 6L,
                                                                       .data$antenna == "right" & .data$where == "net_top" ~ 7L,
                                                                       .data$antenna == "left" & .data$where == "net_top" ~ 8L))
                           p <- p + geom_path(data = plotx, aes(group = .data$antenna), color = antenna_colour, na.rm = TRUE) +
                               geom_path(data = plotx[plotx$where == "net_top", ], color = antenna_colour, na.rm = TRUE) +
                               geom_path(data = plotx[plotx$where == "floor", ], color = antenna_colour, na.rm = TRUE) +
                               geom_label(data = plotx, aes(label = .data$n), color = "white", fill = antenna_colour, na.rm = TRUE)
                       }
                       p + ggplot2::theme_void()
                   } else {
                       NULL
                   }
            if (!show_frame_image) {
                crplvar <- jsns(paste0("crpl", did_sr_popup()))
                myjs <- paste0(crplvar, " = videojs('", jsns(paste0("cr_player_", did_sr_popup())), "'); ", crplvar, ".ready(function() {",
                            "Shiny.setInputValue('", ns("cr_media_height"), "', ", crplvar, ".videoHeight());",
                            "Shiny.setInputValue('", ns("cr_media_width"), "', ", crplvar, ".videoWidth());",
                            resize_observer(jsns(paste0("cr_player_", did_sr_popup())), fun = paste0("console.log('resizing'); ",
                                                        "Shiny.setInputValue('", ns("cr_media_height"), "', ", crplvar, ".videoHeight());",
                                                        "Shiny.setInputValue('", ns("cr_height"), "', ", crplvar, ".currentHeight());",
                                                        "Shiny.setInputValue('", ns("cr_media_width"), "', ", crplvar, ".videoWidth());",
                                                        "var voff = $('#", jsns(paste0("cr_player_", did_sr_popup())), "').innerHeight(); document.getElementById('", ns("srplot"), "').style.marginTop = '-' + voff + 'px';"
                                                        ), nsfun = jsns, as = "string"), "});")
                dojs(myjs)
            }
            out
        }, bg = "transparent", height = if (show_frame_image || length(input$cr_height) < 1 || is.na(input$cr_height) || input$cr_height <= 0) 600 else input$cr_height)
    })

    crimg <- reactive({
        vt <- if (!is.null(input$video_time) && !is.na(input$video_time)) input$video_time else 10
        if (show_frame_image) {
            tryCatch({
                image_file <- ovideo::ov_video_frame(normalizePath(video_file, mustWork = FALSE), vt)
                img <- jpeg::readJPEG(image_file, native = TRUE)
                list(image = img, width = dim(img)[2], height = dim(img)[1]) ## TODO could also get framerate here?
            }, error = function(e) {
                NULL
            })
        } else {
            ## from video
            list(image = NA_character_,
                 width = if (!is.null(input$cr_media_width) && !is.na(input$cr_media_width) && input$cr_media_width > 0) input$cr_media_width else 0,
                 height = if (!is.null(input$cr_media_height) && !is.na(input$cr_media_height) && input$cr_media_height > 0) input$cr_media_height else 0)
        }
    })

    sr_clickdrag <- reactiveValues(mousedown = NULL, mousedown_time = NULL, closest_down = NULL, mouseup = NULL)
    observeEvent(input$did_sr_plot_mousedown, {
        ##cat("mouse down\n")
        closest <- NULL
        if (!is.null(input$sr_plot_hover)) {
            ##px <- c(input$sr_plot_click$x, input$sr_plot_click$y)
            px <- c(input$sr_plot_hover$x, input$sr_plot_hover$y)
            isolate({
                refpts <- bind_rows(mutate(crvt$court, what = "court", rownum = row_number()),
                                    mutate(crvt$antenna, what = "antenna", rownum = row_number() + nrow(crvt$court)))
                if (nrow(refpts) > 0) {
                    closest <- refpts$rownum[which.min(sqrt((refpts$image_x - px[1])^2 + (refpts$image_y - px[2])^2))]
                    if (length(closest) < 1) closest <- NA_integer_
                }
            })
        } else {
            px <- NULL
        }
        sr_clickdrag$mousedown <- px
        sr_clickdrag$mousedown_time <- R.utils::System$currentTimeMillis()
        sr_clickdrag$closest_down <- closest
    })

    observeEvent(input$did_sr_plot_mouseup, {
        ##cat("mouse up\n")
        ## was it a click and not a drag?
        if (!is.null(sr_clickdrag$mousedown)) {
            isolate(px <- last_mouse_pos())
            if (!is.null(px) && !was_mouse_drag(sr_clickdrag)) {
                ##cat("click\n")
                ## if a click, use the click position, else use the last_rv_mouse_pos (but this might lag the actual click pos, because of the hover lag)
                if (!was_mouse_drag(sr_clickdrag) && !is.null(input$sr_plot_click)) px <- c(input$sr_plot_click$x, input$sr_plot_click$y)
                ## enter new point if there is an empty slot, or ignore
                if (is.null(crvt$court) || nrow(crvt$court) < 4) {
                    warning("empty crvt$court??")
                    stop("add new court row")
                } else if (any(is.na(crvt$court$image_x))) {
                    next_pt <- min(which(is.na(crvt$court$image_x)))
                    crvt$court[next_pt, c("image_x", "image_y")] <- as.list(px)
                    ## don't use ns here?? why on earth does that not work??
                    crvt$court$pos[next_pt] <- input[[paste0("crdd", next_pt)]]
                    ## TODO court_x and court_y here need updating
                } else if (isTRUE(include_net)) {
                    if (is.null(crvt$antenna) || nrow(crvt$antenna) < 4) {
                        warning("empty crvt$antenna??")
                        stop("add new antenna row")
                    } else if (any(is.na(crvt$antenna$image_x))) {
                        next_pt <- min(which(is.na(crvt$antenna$image_x)))
                        crvt$antenna[next_pt, c("image_x", "image_y")] <- as.list(px)
                    }
                }
            } else {
                ##cat("drag or null start/end point\n")
                ## do nothing
            }
        }
        ## stop dragging
        sr_clickdrag$mousedown <- NULL
        sr_clickdrag$mousedown_time <- NULL
        sr_clickdrag$closest_down <- NULL
    })

    last_mouse_pos <- reactiveVal(NULL)
    observeEvent(input$sr_plot_hover, {
        ## triggered when mouse moved over the plot
        px <- c(input$sr_plot_hover$x, input$sr_plot_hover$y)
        if (!is.null(px)) last_mouse_pos(px)
    })
    last_refresh_time <- NA_real_
    observe({
        px <- last_mouse_pos()
        if (!is.null(px) && !is.null(sr_clickdrag$mousedown) && was_mouse_drag(sr_clickdrag)) {
            ##cat("was drag\n")
            ## did previously click, so now dragging a point
            now_time <- R.utils::System$currentTimeMillis()
            if (is.na(last_refresh_time) || (now_time - last_refresh_time) > 300) {
                ## debounce
                last_refresh_time <<- now_time
                refpts <- bind_rows(mutate(crvt$court, what = "court", rownum = row_number()),
                                    mutate(crvt$antenna, what = "antenna", rownum = row_number()))
                if (nrow(refpts) > 0 && length(sr_clickdrag$closest_down) > 0) {
                    closest <- sr_clickdrag$closest_down
                    if (!is.na(closest)) {
                        if (refpts$what[closest] == "court") {
                            crvt$court[refpts$rownum[closest], c("image_x", "image_y")] <- as.list(px)
                        } else {
                            crvt$antenna[refpts$rownum[closest], c("image_x", "image_y")] <- as.list(px)
                        }
                    }
                }
            } else {
                shiny::invalidateLater(100)
            }
        }
    })
    list(active = active)
}
