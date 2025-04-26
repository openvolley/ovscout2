## not (yet) exported
## function to listen to a stream and spool it into a local .ts file, and periodically make mp4 snapshots of that video that the main UI can use

## usage follows something like:
## - given the stream rtmp://localhost/live/livestream, run this command in an R session:
##   ovscout2:::ov_file_from_stream("rtmp://localhost/live/livestream", output_filename = "/tmp/stream.mp4")
## - this will create snapshots at /tmp/stream.mp4, so start another R session and scout from that video file:
##   ov_scouter(video = "/tmp/stream.mp4")

ov_file_from_stream <- function(stream, output_filename, freq = 15, cleanup = TRUE) {
    ## internal options
    no_tail <- FALSE ## don't include the last couple of seconds of video, to avoid any possible end-of-file encoding issues
    check_integrity <- FALSE ## check the integrity of the snapshot file after creation, note needs av package and system ffprobe available
    if (check_integrity && !requireNamespace("av", quietly = TRUE)) {
        warning("install the `av` package to enable integrity checking")
        check_integrity <- FALSE
    }
    ## need ffmpeg
    ffmpeg_exe <- ovideo::ov_ffmpeg_exe()
    if (is.null(ffmpeg_exe)) stop("could not find ffmpeg executable")
    if (file.exists(output_filename)) unlink(output_filename)
    working_dir <- tempfile()
    dir.create(working_dir)
    working_file <- file.path(working_dir, "stream.ts")
    convfun <- function() {
        if (no_tail) {
            dur <- tryCatch(av::av_media_info(working_file)$duration, error = function(e) NA_real_)
            if (is.na(dur)) {
                ## try again
                Sys.sleep(1)
                dur <- tryCatch(av::av_media_info(working_file)$duration, error = function(e) NA_real_)
            }
        }
        ## copy working file so that it doesn't get modified during ffmpeg operation
        tempfile <- tempfile(fileext = paste0(".", tools::file_ext(working_file)))
        file.copy(working_file, tempfile)
        ## not sure that this is enough, it can still be written to by ffmpeg during copy, can that leave it in an unusable state?
        tempoutfile <- tempfile(fileext = paste0(".", tools::file_ext(output_filename))) ## write to temp file then rename that once done
        sys::exec_wait(ffmpeg_exe, c("-i", tempfile, "-c", "copy", if (no_tail && !is.na(dur)) c("-t", dur - 2), "-y", tempoutfile))
        if (check_integrity) {
            ## check integrity of tempoutfile
            dur <- tryCatch(av::av_media_info(tempoutfile)$duration, error = function(e) NA_real_)
            if (is.na(dur)) return(NULL)
            ffprobe_exe <- sub("ffmpeg", "ffprobe", ffmpeg_exe) ## that's a bit fragile, TODO fix if check_integrity is actually enabled
            chk <- sys::exec_internal(ffprobe_exe, c("-v", "error", "-i", tempoutfile))$stderr ## TODO check if this is stderr or stdout
            if (length(chk) > 0) return(NULL)
            chk <- sys::exec_internal("ffmpeg", c("-ss", dur - 2, "-v", "error", "-i", tempoutfile, "-f", "null", "-"))$stderr
            if (length(chk) > 0) return(NULL)
        }
        file.rename(output_filename, paste0(output_filename, ".bak"))
        file.rename(tempoutfile, output_filename)
        unlink(c(paste0(output_filename, ".bak"), tempfile))
    }
    main_file_pid <- sys::exec_background(ffmpeg_exe, c("-i", stream, "-c", "copy", working_file))
    on.exit({
        convfun()
        tools::pskill(main_file_pid)
        if (isTRUE(cleanup)) unlink(working_dir, recursive = TRUE)
    })
    while (TRUE) {
        convfun()
        Sys.sleep(freq)
    }
}
