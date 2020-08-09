## build the release bundle and upload to github

setwd("/path/to/ovscoutrepo")

i386 <- FALSE ## if TRUE, build version for 32-bit windows
if (i386) {
    if (!identical(version$arch, "i386")) stop("need to run under 32-bit R to build a 32-bit release")
} else {
    if (!identical(version$arch, "x86_64")) stop("need to run under 64-bit R to build a 64-bit release")
}
if (!.Platform$OS.type == "windows") stop("this must be run on windows")

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

## 0. copy the bat and startup files into standalone/win
file.copy("inst/extdata/standalone/win/ovscout.bat", "standalone/win", overwrite = TRUE)
file.copy("inst/extdata/standalone/win/ovscout.R", "standalone/win", overwrite = TRUE)

setwd("standalone/win")

## 1. Copy R-Portable
chk <- if (i386) file.exists("R-Portable/App/R-Portable/bin/i386/Rscript.exe") else file.exists("R-Portable/App/R-Portable/bin/Rscript.exe")
if (!chk) {
    stop("Download R-Portable from https://sourceforge.net/projects/rportable/files/latest/download and extract into the ovscout/standalone/win/R-Portable directory of this repo. There should be an ovscout/standalone/win/R-Portable/App folder")
}

## 2. download lighttpd for windows and extract, for both 32- and 64-bit architectures
libdir <- if (i386) "libi386" else "lib"
other_libdir <- if (i386) "lib" else "libi386"
if (!dir.exists(libdir)) dir.create(libdir)
if (!dir.exists(other_libdir)) dir.create(other_libdir)
for (thisdir in c(libdir, other_libdir)) {
    if (!file.exists(file.path(thisdir, "lighttpd/lighttpd.exe"))) {
        lhfile <- tempfile(fileext = ".zip")
        lhurl <- if (grepl("i386", thisdir)) "http://lighttpd.dtech.hu/lighttpd-1.4.49-1-win32-ssl.zip" else "http://lighttpd.dtech.hu/lighttpd-1.4.49-1-win64-ssl.zip"
        download.file(lhurl, destfile = lhfile)
        unzip(lhfile, exdir = thisdir)
        try(unlink(lhfile), silent = TRUE)
    }
    if (!file.exists(file.path(thisdir, "lighttpd/lighttpd.exe"))) {
        stop("problem downloading/extracting lighttpd into", thisdir)
    }
}

## some resources
resdir <- file.path(libdir, "ovscout_www")
if (!dir.exists(resdir)) dir.create(resdir)
for (thisres in c("https://untan.gl/images/su_title-w.png", "https://untan.gl/images/bgrev.jpg")) {
    this <- file.path(resdir, basename(thisres))
    if (!file.exists(this)) download.file(thisres, destfile = this)
}

## install packages to our lib dir
old_libpaths <- .libPaths()
.libPaths(c(libdir, "R-Portable/App/R-Portable/library")) ## probably fragile
## this takes ages if installing from scratch
install.packages(c("remotes", "fs", "base64enc"), lib = libdir)
remotes::install_github("openvolley/ovscout", lib = libdir)

## zip everything up
zipfile <- tempfile(fileext = ".zip")
cat("zipping, this will take a while ...\n")
## include everything in the target architecture lib/libi386 folder, but only the lighttpd folder from the non-target-architecture folder
res <- utils::zip(zipfile, files = c("ovscout.bat", "ovscout.R", "R-Portable", if (i386) "libi386" else "lib", if (i386) "lib/lighttpd" else "libi386/lighttpd"))
if (res == 0L) {
    cat("OK.\n")
} else {
    stop("zip failed with error code ", res)
}

file.copy(zipfile, paste0("releases/ovscout", if (i386) "-i386" else "-x64", ".zip"))

setwd(pwd)
.libPaths(old_libpaths)

## upload to github
## needs your GITHUB_TOKEN env var set

library(piggyback)

rel_info <- pb_list("openvolley/ovscout") ## existing releases
if (is.null(rel_info) || !any(this_release %in% rel_info$tag)) {
    pb_new_release("openvolley/ovscout", this_release)
    ## that might not work until issue resolved in piggyback pkg
}
pb_upload(zipfile, repo = "openvolley/ovscout", name = paste0("ovscout", if (i386) "-i386" else "-x64", ".zip"))
