## build the release bundle and upload to github
setwd("/path/to/ovscout2")

pwd <- getwd()
if (!file.exists("DESCRIPTION")) stop("cannot find DESCRIPTION file, are you in the ovscout2 repo base directory?")
pkgInfo <- read.dcf("DESCRIPTION")
if (!identical(pkgInfo[colnames(pkgInfo) == "Package"], "ovscout2")) stop("DESCRIPTION file does not belong to ovscout2, are you in the ovscout2 repo base directory?")
this_release <- NULL
if ("Version" %in% colnames(pkgInfo)) this_release <- pkgInfo[colnames(pkgInfo) == "Version"]
if (is.null(this_release)) stop("could not determine the version of ovscout2")
this_release <- paste0("v", this_release)

tmpdir <- tempfile()
dir.create(tmpdir)

file.copy("inst/extdata/standalone/unix/ov_scouter", tmpdir, overwrite = TRUE)
file.copy("inst/extdata/standalone/unix/ov_scouter_demo", tmpdir, overwrite = TRUE)
file.copy("inst/extdata/standalone/unix/ov_scouter-nonportable.R", tmpdir, overwrite = TRUE)

setwd(tmpdir)

## zip everything up
zipfile <- file.path(tmpdir, "ovscout2-unix-x64.zip")
cat("zipping ...\n")
res <- utils::zip(zipfile, files = c("ov_scouter", "ov_scouter_demo", "ov_scouter-nonportable.R"))
if (res == 0L) {
    cat("OK.\n")
} else {
    stop("zip failed with error code ", res)
}

setwd(pwd)

## upload to github
## needs your GITHUB_TOKEN env var set

library(piggyback)

rel_info <- pb_list("openvolley/ovscout2") ## existing releases
if (is.null(rel_info) || !any(this_release %in% rel_info$tag)) {
    pb_new_release("openvolley/ovscout2", this_release)
    ## that might not work until issue resolved in piggyback pkg
}
pb_upload(zipfile, repo = "openvolley/ovscout2", name = "ovscout2-unix-x64.zip")
