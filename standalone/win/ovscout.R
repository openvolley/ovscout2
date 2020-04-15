## script to manage dependencies and launch the shiny app
## must be called with the path as the first argument
## and dvw file as the second argument
## e.g. Rscript ovscout.R path/to/ovscout.R myfile.dvw

rgs <- commandArgs(trailingOnly = TRUE)

if (length(rgs) < 1) stop('incorrect usage')

mypath <- gsub("^\"+", "", gsub("\"+$", "", rgs[1]))
## force R to use local library path
Rlibpath <- file.path(mypath,"lib")
if (!dir.exists(Rlibpath)) dir.create(Rlibpath)
.libPaths(c(Rlibpath, .libPaths()))

## check that we have dependencies installed
optsave <- getOption("repos")
options(repos = c(CRAN = "https://cran.rstudio.com/"))
online <- tryCatch(suppressWarnings(!is.null(nsl("rstudio.com"))), error = function(e) FALSE)
online <- FALSE

## dependencies required before installing ovscout, with optional minimum version number
depsl <- list(remotes = NA)
for (pkg in names(depsl)) {
    if (!requireNamespace(pkg, quietly = TRUE) || (!is.na(depsl[[pkg]]) && packageVersion(pkg) < depsl[[pkg]])) {
        if (online) {
            cat(sprintf("Installing package: %s\n", pkg))
            install.packages(pkg)
        } else {
            warning("Missing a dependency but you appear to be offline so can't install it: things may not work")
        }
    }
}

## we will always attempt to install these, so that they are always updated
github_deps <- c("openvolley/ovscout")
for (pkg in github_deps) {
    if (online) {
        remotes::install_github(pkg, quiet = TRUE)
    } else {
        if (!requireNamespace(basename(pkg), quietly = TRUE)) {
            stop("Missing the ", pkg ," package but you appear to be offline so can't install it.")
        }
    }
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
ovscout::ov_shiny_video_sync(dvw = dvw, video_file = NULL, launch_browser = TRUE)
