ov2_generate_match_report <- function(dvw, app_data) {
    temp_dvw_file <- tempfile(fileext = ".dvw")
    dv_write2(update_meta(rp2(dvw)), file = temp_dvw_file, convert_cones = app_data$scout_mode != "type")
    servable_url <- NULL
    tryCatch({
        withProgress(message = "Generating match report", value = 0, {
            rargs <- list(x = temp_dvw_file, format = "paged_pdf", style = "ov1", vote = FALSE, shiny_progress = TRUE, chrome_print_extra_args = if (app_data$run_env %eq% "shiny_local") NULL else c("--no-sandbox", "--disable-gpu"))
            if ("icon" %in% names(app_data) && file.exists(app_data$icon)) rargs$icon <- app_data$icon
            ##header_extra_pre = "<div style=\"position:absolute; bottom:-7mm; right:2mm; font-size:9px;\">\nReport via <https://openvolley.org/ovscout2>\n</div>\n"
            rcss <- volleyreport::vr_css()
            if ("report_css" %in% names(app_data) && is.list(app_data$report_css) && length(app_data$report_css) > 0) rcss <- modifyList(rcss, app_data$report_css)
            rargs$css <- rcss
            rfile <- do.call(volleyreport::vr_match_summary, rargs)
            unlink(temp_dvw_file)
            servable_file_abs_path <- file.path(app_data$reports_dir, basename(rfile))
            servable_url <- paste0("/reports/", basename(rfile))
            file.copy(rfile, servable_file_abs_path)
            ##onStop(function() try({ unlink(servable_file_abs_path); unlink(rfile) }, silent = TRUE))
            ##onSessionEnded(function() try({ unlink(servable_file_abs_path); unlink(rfile) }, silent = TRUE))
            showModal(vwModalDialog(title = "Match report", footer = NULL, width = 100,
                                    tags$iframe(style = "width:80%; height:100vh;", src = servable_url),
                                    tags$br(),
                                    tags$hr(),
                                    fixedRow(column(2, offset = 10, actionButton("just_cancel", "Return to scouting", class = "continue fatradio")))
                                    ))
        })
    }, error = function(e) {
        showModal(vwModalDialog(title = "Match report", footer = NULL, width = 100,
                                tags$p("Sorry, something went wrong generating the PDF. (The error message was: ", conditionMessage(e), ")"),
                                tags$br(),
                                tags$hr(),
                                fixedRow(column(2, offset = 10, actionButton("just_cancel", "Return to scouting", class = "continue fatradio")))
                                ))
    })
}
