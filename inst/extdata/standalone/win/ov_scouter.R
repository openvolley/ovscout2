## script to manage dependencies and launch the ov_scouter shiny app

DEBUG <- FALSE

rgs <- commandArgs(trailingOnly = TRUE)

options(warn = 1) ## show warnings immediately
## rgs can contain quoted strings (e.g. the script path) which might contain spaces
rgs <- as.character(read.csv(text = paste(rgs, collapse = " "), header = FALSE, sep = "")) ## split on [[:space:]]+, but respecting quoted strings
mypath <- gsub("^\"+", "", gsub("\"+$", "", rgs[1]))

binpath <- file.path(mypath, "lib")
if (DEBUG) cat("using ovscout2-specific R library path for pandoc, ffmpeg, lighttpd binaries:", binpath, "\n")

## check that we have dependencies installed
optsave <- getOption("repos")
options(repos = c(CRAN = "https://cloud.r-project.org", openvolley = "https://openvolley.r-universe.dev"))

## check that a package and its dependencies are installed.
## it is possible that a package has been installed but one or more of its dependencies has not, e.g. if during install the download of the dependency binary failed but that of the main package did not
needs_installing <- function(pkg) {
    ip <- rownames(installed.packages())
    if (!pkg %in% ip) return(pkg)
    unname(na.omit(vapply(tools::package_dependencies(pkg, recursive = TRUE)[[1]], function(dep) if (!dep %in% ip) dep else NA_character_, FUN.VALUE = "")))
}

## dependencies required before installing ovscout2, with optional minimum version number
depsl <- list(fs = NA, jsonlite = NA, curl = NA)
for (pkg in names(depsl)) {
    to_install <- needs_installing(pkg)
    if (!pkg %in% to_install && (!is.na(depsl[[pkg]]) && packageVersion(pkg) < depsl[[pkg]])) to_install <- c(pkg, to_install)
    if (length(to_install) > 0) {
        tryCatch({
            cat("Installing packages:", paste(to_install, collapse = ", "), "\n")
            install.packages(to_install)
        }, error = function(e) {
            stop("Could not install the ", pkg, " package. The error message was: ", conditionMessage(e))
        })
    }
}

## openvolley packages
## we will always attempt to install these, so that they are always updated, except if --noupdate has been specified
do_upd <- !any(grepl("noupdate", tolower(rgs)))
online <- tryCatch(suppressWarnings(curl::has_internet()), error = function(e) FALSE)

depsl <- c("ovscout2")
for (pkg in depsl) {
    tryCatch({
        to_install <- needs_installing(pkg)
        if (!pkg %in% to_install && do_upd && online) {
            ## have it, does it need to be updated?
            latest <- tryCatch(max(jsonlite::fromJSON(paste0("https://openvolley.r-universe.dev/packages/", pkg, "/"))$Version), error = function(e) NA)
            if (is.na(latest)) {
                warning("Can't determine latest version of package:", pkg, "\n")
                latest <- -Inf
            }
            if (packageVersion(pkg) < latest) to_install <- c(pkg, to_install)
        }
        if (length(to_install) > 0) {
            cat("Installing packages:", paste(to_install, collapse = ", "), "\n")
            install.packages(to_install)
        }
    }, error = function(e) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop("Could not install the ", pkg, " package. The error message was: ", conditionMessage(e))
        } else {
            warning("Could not update the ", pkg, " package. The error message was: ", conditionMessage(e))
        }
    })
}

options(repos = optsave) ## restore

## also try and update this file (ov_scouter.R) and ov_scouter.bat from the potentially-reinstalled ovscout2 pkg
if (do_upd) {
    do_restart <- FALSE
    f1 <- file.path(mypath, "ov_scouter.R")
    f2 <- system.file("extdata/standalone/win/ov_scouter.R", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    f1 <- file.path(mypath, "ov_scouter.bat")
    f2 <- system.file("extdata/standalone/win/ov_scouter.bat", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    f1 <- file.path(mypath, "ov_scouter_demo.bat")
    f2 <- system.file("extdata/standalone/win/ov_scouter_demo.bat", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    if (do_restart) stop("ovscout2 updated. Please re-launch it!")
}
## add lighttpd folder to path
## should have locally-bundled binary
lhpaths <- unique(c(fs::path_real(fs::path(binpath, "lighttpd")), fs::path(binpath, "lighttpd")))
if (DEBUG) cat("trying local lighttpd path(s):", lhpaths, "\n")
lhok <- file.exists(sapply(lhpaths, function(pth) fs::path(pth, "lighttpd.exe")))
if (any(lhok)) {
    lhpath <- lhpaths[lhok][1]
    Sys.setenv(path = paste0(lhpath, ";", Sys.getenv("path")))
    if (DEBUG) cat("setting system path to include local lighttpd path:", Sys.getenv("path"), "\n")
} else {
    if (DEBUG) cat("something's gone wrong, could not find local lighttpd binary\n")
    ## highly unlikely, but see if lighttpd is found somewhere else on the system
    have_lighttpd <- tryCatch({
        chk <- sys::exec_internal("lighttpd", "-version")
        TRUE
    }, error = function(e) FALSE)
    if (DEBUG) cat("check for system-level lighttpd installation:", have_lighttpd, "\n")
}

## check that we have ffmpeg
if (!ovideo::ov_ffmpeg_ok()) {
    ## try the local install
    ffpaths <- unique(c(fs::path_real(fs::path(binpath, "ffmpeg")), fs::path(binpath, "ffmpeg")))
    if (DEBUG) cat("trying local ffmpeg path(s):", ffpaths, "\n")
    ffbin <- unlist(lapply(ffpaths, function(pth) dir(pth, recursive = TRUE, full.names = TRUE, pattern = "ffmpeg\\.exe")))
    if (length(ffbin) > 0) {
        ffpath <- fs::path_dir(fs::path(ffbin[1]))
        Sys.setenv(path = paste0(ffpath, ";", Sys.getenv("path")))
        if (DEBUG) cat("setting system path to include local ffmpeg path:", Sys.getenv("path"), "\n")
    } else {
        if (DEBUG) cat("could not find system or local ffmpeg binary\n")
    }
}
## ffmpeg is not critical, don't warn about this here
##if (!ovideo::ov_ffmpeg_ok()) warning("ffmpeg could not be found, some functionality will be disabled")

## check that we have pandoc
if (!ovscout2:::ov_pandoc_ok()) {
    ## try the local install
    pnpaths <- unique(c(fs::path_real(fs::path(binpath, "pandoc")), fs::path(binpath, "pandoc")))
    if (DEBUG) cat("trying local pandoc path(s):", pnpaths, "\n")
    pnbin <- unlist(lapply(pnpaths, function(pth) dir(pth, recursive = TRUE, full.names = TRUE, pattern = "pandoc\\.exe")))
    if (length(pnbin) > 0) {
        pnpath <- fs::path_dir(fs::path(pnbin[1]))
        Sys.setenv(path = paste0(pnpath, ";", Sys.getenv("path")))
        if (DEBUG) cat("setting system path to include local pandoc path:", Sys.getenv("path"), "\n")
    } else {
        if (DEBUG) cat("could not find system or local pandoc binary\n")
    }
}
if (!ovscout2:::ov_pandoc_ok()) warning("pandoc could not be found, some functionality will be disabled")

library(ovscout2)
## check args
dvw <- video_file <- season_dir <- NULL
scout_mode <- "click" ## can be overridden with parm --scout_mode type
for (rg in na.omit(rgs[-1])) {
    if (tolower(rg) == "demo") {
        dvw <- "demo"
    } else if (tryCatch(fs::is_dir(rg), error = function(e) FALSE)) {
        ## if we've been given a directory, treat it as the season_dir parm
        season_dir <- rg
    } else if (grepl("\\.(ovs|dvw)$", rg, ignore.case = TRUE)) {
        dvw <- rg
    } else if (grepl("\\.(mp4|m4v|mov)$", rg, ignore.case = TRUE)) {
        video_file <- rg
    } else if (tolower(rg) == "type") {
        scout_mode <- "type"
    } else if (tolower(rg) == "click") {
        scout_mode <- "click"
    }
}
ov_scouter(dvw = dvw, video_file = video_file, season_dir = season_dir, launch_browser = TRUE, prompt_for_files = TRUE, scout_mode = scout_mode)
