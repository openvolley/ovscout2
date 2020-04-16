## script to manage dependencies and launch the shiny app
## must be called with the path as the first argument
## and dvw file as the second argument
## e.g. Rscript ovscout.R path/to/ovscout.R myfile.dvw

DEBUG <- FALSE

rgs <- commandArgs(trailingOnly = TRUE)

if (length(rgs) < 1) stop('incorrect usage')

options(warn = 1) ## show warnings immediately

mypath <- gsub("^\"+", "", gsub("\"+$", "", rgs[1]))
## force R to use local library path
i386 <- identical(version$arch, "i386") ## 32-bit windows
Rlibpath <- file.path(mypath, paste0("lib", if (i386) "i386"))
## in principle we could check here if the lib directory is missing the R pkgs, in which case the user might have the wrong zip?
##  (32-bit zip on 64-bit R or vice-versa) - but it should not really matter, it'll just do a lot of R-pkg downloading on
##  the first run
if (DEBUG) cat("using ovscout-specific R library path:", Rlibpath, "\n")
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
options(repos = c(CRAN = "https://cran.rstudio.com/"))

## dependencies required before installing ovscout, with optional minimum version number
depsl <- list(remotes = NA, fs = NA, base64enc = NA)
for (pkg in names(depsl)) {
    if (!requireNamespace(pkg, quietly = TRUE) || (!is.na(depsl[[pkg]]) && packageVersion(pkg) < depsl[[pkg]])) {
        tryCatch({
            cat(sprintf("Installing package: %s\n", pkg))
            install.packages(pkg)
        }, error = function(e) {
            stop("Could not install the ", dpkg, " package. The error message was: ", conditionMessage(e))
        })
    }
}

## we will always attempt to install these, so that they are always updated
github_deps <- c("openvolley/ovscout")
for (pkg in github_deps) {
    tryCatch({
        remotes::install_github(pkg)
    }, error = function(e) {
        if (!requireNamespace(basename(pkg), quietly = TRUE)) {
            stop("Could not install the ", dpkg, " package. The error message was: ", conditionMessage(e))
        }
    })
}
## placeholder for optional packages
##github_deps <- c("openvolley/peranavolley")
##for (pkg in github_deps) {
##    if (online) {
##        remotes::install_github(pkg, quiet = TRUE)
##    } else {
##        if (!requireNamespace(basename(pkg), quietly = TRUE)) {
##            warning("Missing the ", pkg ," package but you appear to be offline so can't install it: some functionality may be disabled.")
##        }
##    }
##}
options(repos = optsave) ## restore

## also try and update this file (ovscout.R) and ovscout.bat from the potentially-reinstalled ovscout pkg
if (FALSE) {
    ## skip this for the time being pending more testing
    dR0 <- dR1 <- NULL
    tryCatch({
        dR0 <- digest::digest("ovscout.R", file = TRUE)
        dR1 <- digest::digest(system.file("extdata/standalone/win/ovscout.R", package = "ovscout"), file = TRUE)
    }, error = function(e) {
        warning("could not update ovscout.R")
    })
    db0 <- db1 <- NULL
    tryCatch({
        db0 <- digest::digest("ovscout.bat", file = TRUE)
        db1 <- digest::digest(system.file("extdata/standalone/win/ovscout.bat", package = "ovscout"), file = TRUE)
    }, error = function(e) {
        warning("could not update ovscout.bat")
    })
    if ((!is.null(dR0) && !is.null(dR1) && dR0 != dR1) || (!is.null(db0) && !is.null(db1) && db0 != db1)) {
        file.copy(system.file("extdata/standalone/win/ovscout.R", package = "ovscout"), "ovscout.R", overwrite = TRUE)
        file.copy(system.file("extdata/standalone/win/ovscout.bat", package = "ovscout"), "ovscout.bat", overwrite = TRUE)
        stop("ovscout updated. Please re-launch it!")
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

library(ovscout)
if (length(rgs) < 2 || is.na(rgs[2]) || !nzchar(rgs[2])) {
    cat("choose dvw file\n")
    if (.Platform$OS.type == "windows") {
        dvw <- utils::choose.files(caption = "Choose dvw file", multi = FALSE, filters = matrix(c("dvw files (*.dvw)", "*.dvw", "All files (*.*)", "*.*"), nrow = 2, byrow = TRUE))
    } else {
        dvw <- file.choose()
    }
}

## styling to use here
resdir <- file.path(Rlibpath, "ovscout_www")
im64 <- function(im) tryCatch(paste0("data:image/", sub("^[^\\.]+\\.", "", im), ";base64,", base64enc::base64encode(file.path(resdir, im))), error = function(e) "")
su_header <- shiny::fluidRow(id = "headerblock", shiny::tags$a(id = "headerlogo", href = "https://untan.gl", shiny::tags$img(alt = "Science Untangled", src = im64("su_title-w.png"))), shiny::column(6, offset = 2, shiny::tags$h2("Volleyball scout and video sync")))
su_css <- paste0("#headerblock { border-radius:0px; border:none; color: white; background: #000040 url(", im64("bgrev.jpg"), ") 0 0/100% auto no-repeat; } #headerlogo {float:right; clear:none;} #headerlogo img {width: 16em; max-width:30vw; margin:10px;}")

## TODO, option to specify video file
ov_shiny_video_sync(dvw = dvw, video_file = NULL, launch_browser = TRUE, ui_header = su_header, css = su_css)
