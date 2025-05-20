## build the release bundle and upload to github

build_for <- "win" ## "win" or "unix"

setwd("/path/to/ovscout2")

stopifnot(build_for %in% c("win", "unix"))

pwd <- getwd()
if (!file.exists("DESCRIPTION")) stop("cannot find DESCRIPTION file, are you in the ovscout2 repo base directory?")
pkgInfo <- read.dcf("DESCRIPTION")
if (!identical(pkgInfo[colnames(pkgInfo) == "Package"], "ovscout2")) stop("DESCRIPTION file does not belong to ovscout2, are you in the ovscout2 repo base directory?")
this_release <- NULL
if ("Version" %in% colnames(pkgInfo)) this_release <- pkgInfo[colnames(pkgInfo) == "Version"]
if (is.null(this_release)) stop("could not determine the version of ovscout2")
this_release <- paste0("v", this_release)

tmpdir <- tempfile()
dir.create(tmpdir)

## copy the bat and startup files
startup_files <- if (build_for == "win") c("ov_scouter.bat", "ov_scouter_demo.bat", "ov_scouter.R") else c("ov_scouter", "ov_scouter_demo", "ov_scouter-nonportable.R")
file.copy(file.path("inst/extdata/standalone", build_for, startup_files), tmpdir, overwrite = TRUE)

setwd(tmpdir)

if (build_for == "win") {
    ## 1. download lighttpd for windows and extract
    libdir <- "lib"
    if (!dir.exists(libdir)) dir.create(libdir)
    if (!file.exists(file.path(libdir, "lighttpd/lighttpd.exe"))) {
        lhfile <- tempfile(fileext = ".zip")
        lhurl <- "http://lighttpd.dtech.hu/lighttpd-1.4.49-1-win64-ssl.zip"
        download.file(lhurl, destfile = lhfile)
        unzip(lhfile, exdir = libdir)
        try(unlink(lhfile), silent = TRUE)
    }
    if (!file.exists(file.path(libdir, "lighttpd/lighttpd.exe"))) {
        stop("problem downloading/extracting lighttpd into", libdir)
    }

    ## 2. install ffmpeg
    ffmpeg_dir <- file.path(libdir, "ffmpeg")
    if (!dir.exists(ffmpeg_dir)) dir.create(ffmpeg_dir)
    if (length(dir(ffmpeg_dir, recursive = TRUE, pattern = "ffmpeg\\.exe")) < 1) {
        dl_url <- "https://github.com/BtbN/FFmpeg-Builds/releases/download/latest/ffmpeg-master-latest-win64-gpl.zip"
        zipname <- file.path(ffmpeg_dir, basename(dl_url))
        err <- utils::download.file(dl_url, destfile = zipname, mode = "wb")
        if (!err) utils::unzip(zipname, exdir = ffmpeg_dir, files = file.path("ffmpeg-master-latest-win64-gpl", c("LICENSE.txt", "bin/ffmpeg.exe")))
        unlink(file.path(ffmpeg_dir, basename(dl_url))) ## delete the zip file
    }
    if (length(dir(ffmpeg_dir, recursive = TRUE, pattern = "ffmpeg\\.exe")) < 1) stop("ffmpeg install failed")

    ## 3. pandoc
    pandoc_dir <- file.path(libdir, "pandoc")
    if (!dir.exists(pandoc_dir)) dir.create(pandoc_dir)
    if (length(dir(pandoc_dir, recursive = TRUE, pattern = "pandoc\\.exe")) < 1) {
        dl_url <- "https://github.com/jgm/pandoc/releases/download/2.19/pandoc-2.19-windows-x86_64.zip"
        zipname <- file.path(pandoc_dir, basename(dl_url))
        err <- utils::download.file(dl_url, destfile = zipname, mode = "wb")
        if (!err) utils::unzip(zipname, exdir = pandoc_dir, junkpaths = TRUE)
        unlink(file.path(pandoc_dir, basename(dl_url))) ## delete the zip file
    }
    if (length(dir(pandoc_dir, recursive = TRUE, pattern = "pandoc\\.exe")) < 1) stop("pandoc install failed")
}

## zip everything up
zipfile <- file.path(tmpdir, paste0("ovscout2-", build_for, "-x64.zip"))
cat("zipping ...\n")
res <- utils::zip(zipfile, files = c(startup_files, if (build_for == "win") "lib"))
if (res == 0L) {
    cat("OK.\n")
} else {
    stop("zip failed with error code ", res)
}

setwd(pwd)

## upload to github
## needs your GITHUB_TOKEN env var set

library(piggyback)

rel_info <- pb_list("openvolley/ovscout2") ## existing releases
if (is.null(rel_info) || !any(this_release %in% rel_info$tag)) {
    pb_new_release("openvolley/ovscout2", this_release)
    ## that might not work until issue resolved in piggyback pkg
}
pb_upload(zipfile, repo = "openvolley/ovscout2", name = paste0("ovscout2-", build_for, "-x64.zip"))
