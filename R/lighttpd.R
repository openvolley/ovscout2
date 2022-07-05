#' Install lighttpd
#'
#' This is a helper function to install lighttpd. Currently it only works on Windows platforms. The lighttpd bundle will be downloaded from <http://lighttpd.dtech.hu/> and saved to your user appdata directory.
#'
#' @references <http://lighttpd.dtech.hu/>
#' @param force logical: force reinstallation if lighttpd already exists
#'
#' @return the path to the installed executable
#'
#' @examples
#' \dontrun{
#'   ov_install_lighttpd()
#' }
#'
#' @export
ov_install_lighttpd <- function(force = FALSE) {
    assert_that(is.flag(force), !is.na(force))
    if (!force) {
        existing_exe <- ov_find_lighttpd()
        if (!is.null(existing_exe)) {
            message("lighttpd already exists and force is FALSE, not reinstalling")
            return(existing_exe)
        }
    }
    my_os <- get_os()
    if (my_os != "windows") {
        stop("ov_install_lighttpd only supports windows platforms. You will need to install lighttpd yourself and ensure that it is on the system path.")
    }
    path <- file.path(ovscout2_app_dir(), "lighttpd")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    if (!dir.exists(path)) stop("could not create directory ", path, " for lighttpd")
    bits <- tryCatch(if (.Machine$sizeof.pointer == 8) 64 else 32, error = function(e) 32)
    ## 8-byte address space is 64-bit. Note that we're actually detecting the R address space here, not the OS address space. But I don't think there's a reliable way of detecting the machine architecture
    zipname <- file.path(path, paste0("lighttpd-1.4.49-1-win", bits, "-ssl.zip"))
    wgurl <- paste0("http://lighttpd.dtech.hu/", basename(zipname))
    err <- utils::download.file(wgurl, destfile = zipname, mode = "wb")
    if (!err) utils::unzip(zipname, exdir = path)
    ## now we should see the executable
    chk <- ov_find_lighttpd()
    if (!is.null(chk)) chk else stop("Sorry, lighttpd install failed. You will need to install it yourself and ensure that it is on the system path.")
}

ov_find_lighttpd <- function() {
    exe_name <- paste0("lighttpd", if (get_os() == "windows") ".exe")
    chk <- Sys.which(exe_name)
    if (nzchar(chk)) return(chk)
    ## is it installed in user appdir?
    mydir <- file.path(ovscout2_app_dir(), "lighttpd")
    if (!dir.exists(mydir)) return(NULL)
    chk <- fs::dir_ls(path = mydir, recurse = TRUE, regexp = paste0(exe_name, "$"), type = "file")
    chk <- chk[basename(chk) == exe_name]
    if (length(chk) == 1 && file.exists(chk)) chk else NULL
}

