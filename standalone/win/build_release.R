## placeholder for script to build the release bundle and upload to github

setwd("/path/to/ovscoutrepo")

i386 <- FALSE ## if TRUE, build version for 32-bit windows
if (i386) {
    if (!identical(version$arch, "i386")) stop("need to run under 32-bit R to build a 32-bit release")
} else {
    if (!identical(version$arch, "x86_64")) stop("need to run under 64-bit R to build a 64-bit release")
}

pwd <- getwd()
if (!file.exists("DESCRIPTION")) stop("cannot find DESCRIPTION file, are you in the ovscout repo base directory?")
pkgInfo <- read.dcf("DESCRIPTION")
if (!identical(pkgInfo[colnames(pkgInfo) == "Package"], "ovscout")) stop("DESCRIPTION file does not belong to ovscout, are you in the ovscout repo base directory?")
this_release <- NULL
if ("Version" %in% colnames(pkgInfo)) {
    this_release <- pkgInfo[colnames(pkgInfo) == "Version"]
}
if (is.null(this_release)) stop("could not determine the version of ovscout")
this_release <- paste0("v", this_release)

setwd("standalone/win")

## 1. Copy R-Portable
chk <- if (i386) file.exists("R-Portable/App/R-Portable/bin/i386/Rscript.exe") else file.exists("R-Portable/App/R-Portable/bin/Rscript.exe")
if (!chk) {
    stop("Download R-Portable from https://sourceforge.net/projects/rportable/files/latest/download and extract into the ovscout/standalone/win/R-Portable directory of this repo. There should be ovscout/standalone/win/App folder")
}

## 2. download lighttpd for windows and extract so that the lighttpd folder is under ovscout/standalone/win/lib
##  or ovscout/standalone/win/libi386 for 32-bit
libdir <- if (i386) "libi386" else "lib"
if (!dir.exists(libdir)) dir.create(libdir)
if (!file.exists(file.path(libdir, "lighttpd/lighttpd.exe"))) {
    lhfile <- tempfile(fileext = ".zip")
    lhurl <- if (i386) "http://lighttpd.dtech.hu/lighttpd-1.4.49-1-win32-ssl.zip" else "http://lighttpd.dtech.hu/lighttpd-1.4.49-1-win64-ssl.zip"
    download.file(lhurl, destfile = lhfile)
    unzip(lhfile, exdir = libdir)
    try(unlink(lhfile), silent = TRUE)
}
if (!file.exists(file.path(libdir, "lighttpd/lighttpd.exe"))) {
    stop("problem downloading/extracting lighttpd")
}

## install packages to our lib dir
old_libpaths <- .libPaths()
.libPaths(c(libdir, "R-Portable/App/R-Portable/library")) ## probably fragile
## this takes ages if installing from scratch
remotes::install_github("openvolley/ovscout", lib = libdir)

## zip everything up
zipfile <- tempfile(fileext = ".zip")
cat("zipping, this will take a while ...\n")
res <- utils::zip(zipfile, files = c("ovscout.bat", "ovscout.R", "R-Portable", if (i386) "libi386" else "lib"))
if (res == 0L) {
    cat("OK.\n")
} else {
    stop("zip failed with error code ", res)
}

file.copy(zipfile, paste0("releases/ovscout", if (i386) "-i386", ".zip"))

setwd(pwd)
.libPaths(old_libpaths)

## upload to github
## needs your GITHUB_TOKEN env var set

library(piggyback)

rel_info <- pb_list("openvolley/ovscout") ## existing releases
if (is.null(rel_info) || !any(this_release %in% rel_info$tag)) {
    pb_new_release("openvolley/ovscout", this_release)
    ## hmm, this didn't work for me, did manually
}
pb_upload(zipfile, repo = "openvolley/ovscout", name = paste0("ovscout", if (i386) "-i386", ".zip"))
