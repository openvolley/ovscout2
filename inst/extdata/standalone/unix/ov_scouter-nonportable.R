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
depsl <- list(remotes = NA, fs = NA, jsonlite = NA, curl = NA)
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
        do_install <- TRUE##!requireNamespace(pkg, quietly = TRUE)
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

##github_deps <- c("openvolley/ovscout2")
##for (pkg in github_deps) {
##    tryCatch({
##        remotes::install_github(pkg, upgrade = if (!requireNamespace("ovscout2", quietly = TRUE)) "never" else "always")
##        ## don't upgrade dependencies on first install, to reduce the number of packages being installed multiple times
##    }, error = function(e) {
##        if (!requireNamespace(basename(pkg), quietly = TRUE)) {
##            stop("Could not install the ", pkg, " package. The error message was: ", conditionMessage(e))
##        }
##    })
##}

options(repos = optsave) ## restore

## also try and update this file (ov_scouter-nonportable.R) and ov_scouter from the potentially-reinstalled ovscout2 pkg
if (TRUE) {
    ##dR0 <- dR1 <- NULL
    ##tryCatch({
    ##    dR0 <- digest::digest(file.path(mypath, "ov_scouter-nonportable.R"), file = TRUE)
    ##    dR1 <- digest::digest(system.file("extdata/standalone/unix/ov_scouter-nonportable.R", package = "ovscout2"), file = TRUE)
    ##}, error = function(e) {
    ##    warning("could not update ov_scouter-nonportable.R")
    ##})
    do_restart <- FALSE
    f1 <- file.path(mypath, "ov_scouter-nonportable.R")
    f2 <- system.file("extdata/standalone/unix/ov_scouter-nonportable.R", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    ##db0 <- db1 <- NULL
    ##tryCatch({
    ##    db0 <- digest::digest(file.path(mypath, "ov_scouter"), file = TRUE)
    ##    db1 <- digest::digest(system.file("extdata/standalone/unix/ov_scouter", package = "ovscout2"), file = TRUE)
    ##}, error = function(e) {
    ##    warning("could not update ov_scouter")
    ##})
    f1 <- file.path(mypath, "ov_scouter")
    f2 <- system.file("extdata/standalone/unix/ov_scouter", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    ##dbd0 <- dbd1 <- NULL
    ##tryCatch({
    ##    dbd0 <- digest::digest(file.path(mypath, "ov_scouter_demo"), file = TRUE)
    ##    dbd1 <- digest::digest(system.file("extdata/standalone/unix/ov_scouter_demo", package = "ovscout2"), file = TRUE)
    ##}, error = function(e) {
    ##    warning("could not update ov_scouter_demo")
    ##})
    f1 <- file.path(mypath, "ov_scouter_demo")
    f2 <- system.file("extdata/standalone/unix/ov_scouter_demo", package = "ovscout2")
    if (file.exists(f1) && file.exists(f2) && fs::file_info(f2)$modification_time > fs::file_info(f1)$modification_time) {
        file.copy(f2, f1, overwrite = TRUE)
        do_restart <- TRUE
    }
    ##if ((!is.null(dR0) && !is.null(dR1) && dR0 != dR1) || (!is.null(db0) && !is.null(db1) && db0 != db1) || (!is.null(dbd0) && !is.null(dbd1) && dbd0 != dbd1)) {
    ##    file.copy(system.file("extdata/standalone/unix/ov_scouter-nonportable.R", package = "ovscout2"), file.path(mypath, "ov_scouter-nonportable.R"), overwrite = TRUE)
    ##    file.copy(system.file("extdata/standalone/unix/ov_scouter", package = "ovscout2"), file.path(mypath,"ov_scouter"), overwrite = TRUE)
    ##    file.copy(system.file("extdata/standalone/unix/ov_scouter_demo", package = "ovscout2"), file.path(mypath,"ov_scouter_demo"), overwrite = TRUE)
    ##    stop("ovscout2 updated. Please re-launch it!")
    ##}
    if (do_restart) stop("ovscout2 updated. Please re-launch it!")
}

## check that we have ffmpeg
if (!ovideo::ov_ffmpeg_ok()) warning("ffmpeg could not be found, some functionality will be disabled")

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
