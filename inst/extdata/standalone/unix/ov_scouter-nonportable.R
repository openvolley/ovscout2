## script to manage dependencies and launch the shiny app
## must be called with the path as the first argument
## and dvw file as the second argument
## e.g. Rscript ovscout2-nonportable.R path/to/ov_scouter.R myfile.dvw

DEBUG <- FALSE

rgs <- commandArgs(trailingOnly = TRUE)

options(warn = 1) ## show warnings immediately
mypath <- gsub("^\"+", "", gsub("\"+$", "", rgs[1]))

## check that we have dependencies installed
optsave <- getOption("repos")
options(repos = c(CRAN = "https://cloud.r-project.org", openvolley = "https://openvolley.r-universe.dev"))

## dependencies required before installing ovscout2, with optional minimum version number
depsl <- list(fs = NA, jsonlite = NA, curl = NA)
for (pkg in names(depsl)) {
    if (!requireNamespace(pkg, quietly = TRUE) || (!is.na(depsl[[pkg]]) && packageVersion(pkg) < depsl[[pkg]])) {
        tryCatch({
            cat("Installing package:", pkg, "\n")
            install.packages(pkg)
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
        do_install <- !requireNamespace(pkg, quietly = TRUE)
        if (!do_install && do_upd && online) {
            ## have it, does it need to be updated?
            latest <- tryCatch(max(jsonlite::fromJSON(paste0("https://openvolley.r-universe.dev/packages/", pkg, "/"))$Version), error = function(e) NA)
            if (is.na(latest)) {
                warning("Can't determine latest version of package:", pkg, "\n")
                latest <- -Inf
            }
            do_install <- packageVersion(pkg) < latest
        }
        if (do_install) {
            cat("Installing package:", pkg, "\n")
            install.packages(pkg)
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

## also try and update this file (ov_scouter-nonportable.R) and ov_scouter from the potentially-reinstalled ovscout2 pkg
if (TRUE) {
    do_restart <- FALSE
    f1 <- file.path(mypath, "ov_scouter-nonportable.R")
    f2 <- system.file("extdata/standalone/unix/ov_scouter-nonportable.R", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    f1 <- file.path(mypath, "ov_scouter")
    f2 <- system.file("extdata/standalone/unix/ov_scouter", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    f1 <- file.path(mypath, "ov_scouter_demo")
    f2 <- system.file("extdata/standalone/unix/ov_scouter_demo", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    if (do_restart) stop("ovscout2 updated. Please re-launch it!")
}

## ffmpeg is not critical, don't warn about this here
## check that we have ffmpeg
##if (!ovideo::ov_ffmpeg_ok()) warning("ffmpeg could not be found, some functionality will be disabled")

## check that we have pandoc
if (!ovscout2:::ov_pandoc_ok()) warning("pandoc could not be found, some functionality will be disabled")

library(ovscout2)
## check args
dvw <- video_file <- season_dir <- NULL
for (rg in na.omit(rgs[-1])) {
    if (rg %in% "demo") {
        dvw <- "demo"
    } else if (tryCatch(fs::is_dir(rg), error = function(e) FALSE)) {
        ## if we've been given a directory, treat it as the season_dir parm
        season_dir <- rg
    } else if (grepl("\\.(ovs|dvw)$", rg, ignore.case = TRUE)) {
        dvw <- rg
    } else if (grepl("\\.(mp4|m4v|mov)$", rg, ignore.case = TRUE)) {
        video_file <- rg
    }
}
ov_scouter(dvw = dvw, video_file = video_file, season_dir = season_dir, launch_browser = TRUE, prompt_for_files = TRUE)
