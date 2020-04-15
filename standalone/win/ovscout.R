## script to manage dependencies and launch the shiny app
## must be called with the path as the first argument
## and dvw file as the second argument
## e.g. Rscript ovscout.R path/to/ovscout.R myfile.dvw

rgs <- commandArgs(trailingOnly = TRUE)

if (length(rgs) < 1) stop('incorrect usage')

options(warn = 1) ## show warnings immediately

mypath <- gsub("^\"+", "", gsub("\"+$", "", rgs[1]))
## force R to use local library path
i386 <- identical(version$arch, "i386") ## 32-bit windows
Rlibpath <- file.path(mypath, paste0("lib", if (i386) "i386"))
if (!dir.exists(Rlibpath)) dir.create(Rlibpath)
.libPaths(c(Rlibpath, .libPaths()))

## check that we have dependencies installed
optsave <- getOption("repos")
options(repos = c(CRAN = "https://cran.rstudio.com/"))

## dependencies required before installing ovscout, with optional minimum version number
depsl <- list(remotes = NA, fs = NA)
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
        remotes::install_github(pkg, quiet = TRUE)
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

## add lighttpd folder to path
have_lighttpd <- tryCatch({
    chk <- sys::exec_internal("lighttpd", "-version")
    TRUE
}, error = function(e) FALSE)
lhpath <- fs::path_real(fs::path(Rlibpath, "lighttpd"))
if (!have_lighttpd && dir.exists(lhpath)) {
    Sys.setenv(path = paste0(lhpath, ";", Sys.getenv("path")))
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
## TODO, option to specify video file
ov_shiny_video_sync(dvw = dvw, video_file = NULL, launch_browser = TRUE)
