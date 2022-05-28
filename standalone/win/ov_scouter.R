## script to manage dependencies and launch the ov_scouter shiny app

DEBUG <- FALSE

rgs <- commandArgs(trailingOnly = TRUE)

options(warn = 1) ## show warnings immediately

mypath <- gsub("^\"+", "", gsub("\"+$", "", rgs[1]))
## force R to use local library path
Rlibpath <- file.path(mypath, "lib")
if (DEBUG) cat("using ovscout2-specific R library path:", Rlibpath, "\n")
if (!dir.exists(Rlibpath)) {
    if (DEBUG) cat(Rlibpath, "does not exist, creating.\n")
    dir.create(Rlibpath)
}
.libPaths(c(Rlibpath, .libPaths()))
if (DEBUG) {
    cat("Full R library paths:\n")
    print(.libPaths())
}

## check that we have dependencies installed
optsave <- getOption("repos")
options(repos = c(CRAN = "https://cloud.r-project.org", openvolley = "https://openvolley.r-universe.dev"))

## dependencies required before installing ovscout2, with optional minimum version number
depsl <- list(remotes = NA, fs = NA)
for (pkg in names(depsl)) {
    if (!requireNamespace(pkg, quietly = TRUE) || (!is.na(depsl[[pkg]]) && packageVersion(pkg) < depsl[[pkg]])) {
        tryCatch({
            cat(sprintf("Installing package: %s\n", pkg))
            install.packages(pkg)
        }, error = function(e) {
            stop("Could not install the ", pkg, " package. The error message was: ", conditionMessage(e))
        })
    }
}

## we will always attempt to install these, so that they are always updated
depsl <- list(ovscout2 = NA)
for (pkg in names(depsl)) {
    tryCatch({
        cat(sprintf("Installing package: %s\n", pkg))
        install.packages(pkg)
    }, error = function(e) {
        stop("Could not install the ", pkg, " package. The error message was: ", conditionMessage(e))
    })
}

##github_deps <- c("openvolley/ovscout2")
##for (pkg in github_deps) {
##    tryCatch({
##        remotes::install_github(pkg)
##    }, error = function(e) {
##        if (!requireNamespace(basename(pkg), quietly = TRUE)) {
##            stop("Could not install the ", pkg, " package. The error message was: ", conditionMessage(e))
##        }
##    })
##}

options(repos = optsave) ## restore

## also try and update this file (ov_scouter.R) and ov_scouter.bat from the potentially-reinstalled ovscout2 pkg
if (FALSE) {
    ## skip this for the time being pending more testing
    dR0 <- dR1 <- NULL
    tryCatch({
        dR0 <- digest::digest("ov_scouter.R", file = TRUE)
        dR1 <- digest::digest(system.file("extdata/standalone/win/ov_scouter.R", package = "ovscout2"), file = TRUE)
    }, error = function(e) {
        warning("could not update ov_scouter.R")
    })
    db0 <- db1 <- NULL
    tryCatch({
        db0 <- digest::digest("ov_scouter.bat", file = TRUE)
        db1 <- digest::digest(system.file("extdata/standalone/win/ov_scouter.bat", package = "ovscout2"), file = TRUE)
    }, error = function(e) {
        warning("could not update ov_scouter.bat")
    })
    if ((!is.null(dR0) && !is.null(dR1) && dR0 != dR1) || (!is.null(db0) && !is.null(db1) && db0 != db1)) {
        file.copy(system.file("extdata/standalone/win/ov_scouter.R", package = "ovscout2"), "ov_scouter.R", overwrite = TRUE)
        file.copy(system.file("extdata/standalone/win/ov_scouter.bat", package = "ovscout2"), "ov_scouter.bat", overwrite = TRUE)
        stop("ovscout2 updated. Please re-launch it!")
    }
}
## add lighttpd folder to path
## should have locally-bundled binary
lhpaths <- unique(c(fs::path_real(fs::path(Rlibpath, "lighttpd")), fs::path(Rlibpath, "lighttpd")))
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

library(ovscout2)
## check for dvw, video file args: if not provided the shiny app will prompt for user to select file
dvw <- if (length(rgs) < 2 || is.na(rgs[2]) || !nzchar(rgs[2])) NULL else rgs[2]
vf <- if (length(rgs) < 3 || is.na(rgs[3]) || !nzchar(rgs[3])) NULL else rgs[3]

ov_scouter(dvw = dvw, video_file = vf, launch_browser = TRUE, prompt_for_files = TRUE)
