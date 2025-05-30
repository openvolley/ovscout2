## script to manage dependencies and launch the shiny app
## must be called with the path as the first argument
## and dvw file as the second argument
## e.g. Rscript ovscout2-nonportable.R path/to/ov_scouter.R myfile.dvw

DEBUG <- FALSE

rgs <- commandArgs(trailingOnly = TRUE)

options(warn = 1) ## show warnings immediately

## rgs can contain quoted strings (e.g. the script path) which might contain spaces
rgs <- as.character(read.csv(text = paste(rgs, collapse = " "), header = FALSE, sep = "")) ## split on [[:space:]]+, but respecting quoted strings

mypath <- gsub("^\"+", "", gsub("\"+$", "", rgs[1]))

## check that we have dependencies installed
optsave <- getOption("repos")
options(repos = c(CRAN = "https://cloud.r-project.org", openvolley = "https://openvolley.r-universe.dev"))

## if the library path is unwritable (e.g. the user installed R as admin on Windows and this is the first startup)
##   install.packages() won't ask about using a user-specific R library path because it only does that in interactive sessions (interactive() isn't TRUE under Rscript)
## so we check it here
lib_path_checked <- FALSE
check_lib_path <- function() {
    ## minor adaptations from utils::install.packages, R Core Team R-core@r-project.org
    lib <- .libPaths()[1L]
    ok <- dir.exists(lib) & (file.access(lib, 2) == 0L)
    if (length(lib) == 1L && .Platform$OS.type == "windows") {
        ok <- dir.exists(lib)
        if (ok) {
            fn <- file.path(lib, paste0("_test_dir_", Sys.getpid()))
            unlink(fn, recursive = TRUE)
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) {
                ok <- FALSE
            } else {
                unlink(fn, recursive = TRUE)
            }
        }
    }
    if (length(lib) == 1L && !ok) {
        warning(gettextf("'lib = \"%s\"' is not writable", lib), domain = NA, immediate. = TRUE)
        userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep))[1L]
        if (requireNamespace("tcltk", quietly = TRUE)) {
            ans <- tcltk::tk_messageBox(type = "yesno", paste0("'lib = \"", lib, "\"' is not writable\nWould you like to use a personal library instead?"))
            if (!isTRUE(ans == "yes")) stop("unable to install packages")
            lib <- userdir
            if (!file.exists(userdir)) {
                ans <- tcltk::tk_messageBox(type = "yesno", paste0("OK to create personal library at\n", sQuote(userdir), "\nto install packages into?"))
                if (!isTRUE(ans == "yes")) stop("unable to install packages")
                if (!dir.create(userdir, recursive = TRUE)) stop(gettextf("unable to create %s", sQuote(userdir)), domain = NA)
                .libPaths(c(userdir, .libPaths()))
            }
        } else {
            stop("unable to install packages")
        }
    }
    lib_path_checked <<- TRUE
}

## check that a package and its dependencies are installed.
## it is possible that a package has been installed but one or more of its dependencies has not, e.g. if during install the download of the dependency binary failed but that of the main package did not
needs_installing <- function(pkg) {
    if (!lib_path_checked) check_lib_path()
    ip <- rownames(installed.packages())
    if (!pkg %in% ip) return(pkg)
    unname(na.omit(vapply(tools::package_dependencies(pkg, recursive = TRUE)[[1]], function(dep) if (!dep %in% ip) dep else NA_character_, FUN.VALUE = "")))
}

## dependencies required before installing ovscout2, with optional minimum version number
cat("checking dependencies: ")
depsl <- list(fs = NA, jsonlite = NA, curl = NA)
for (pkg in names(depsl)) {
    cat(pkg, "")
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
cat("\n")

## openvolley packages
## we will always attempt to install these, so that they are always updated, except if --noupdate has been specified
do_upd <- !any(grepl("noupdate", tolower(rgs)))
online <- tryCatch(suppressWarnings(curl::has_internet()), error = function(e) FALSE)

depsl <- c("ovscout2")
for (pkg in depsl) {
    cat("checking package:", pkg, "\n")
    tryCatch({
        to_install <- needs_installing(pkg)
        if (!pkg %in% to_install && do_upd && online) {
            ## have it, does it need to be updated?
            latest <- tryCatch(max(jsonlite::fromJSON(paste0("https://openvolley.r-universe.dev/api/packages/", pkg, "/"))$Version), error = function(e) NA)
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

## also try and update this file (ov_scouter-nonportable.R) and ov_scouter from the potentially-reinstalled ovscout2 pkg
if (do_upd) {
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
blah <- ovscout2:::ov_pandoc_ok()

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
