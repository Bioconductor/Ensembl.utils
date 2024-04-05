
.ENSEMBL_FTP_PUB_URL <- "ftp://ftp.ensembl.org/pub/"
.ENSEMBL_FTP_PUB_GRCH37_URL <- "ftp://ftp.ensembl.org/pub/grch37/"
.ENSEMBLGENOMES_FTP_PUB_URL <- "ftp://ftp.ensemblgenomes.org/pub/"
.ENSEMBL_FTP_RELEASE_PREFIX <- "release-"
.ENSEMBL_DIVISIONS <- c("bacteria", "fungi", "metazoa", "plants", "protists")


.is_single_value <- function(x)
{
    is.vector(x) && is.atomic(x) && length(x) == 1L
}


### 'division' must be NA or one of the Ensembl Genomes divisions i.e.
### "bacteria", "fungi", "metazoa", "plants", or "protists".
.get_Ensembl_FTP_top_url <- function(division=NA, use.grch37=FALSE)
{
    if (!.is_single_value(division))
        stop(wmsg("'division' must be a single value"))
    if (!isTRUEorFALSE(use.grch37))
        stop(wmsg("'use.grch37' must be TRUE or FALSE"))
    if (!is.na(division)) {
        if (!is.character(division) || division == "")
            stop(wmsg("'division' must be a single non-empty string or NA"))
        division <- match.arg(division, .ENSEMBL_DIVISIONS)
        if (use.grch37)
            stop(wmsg("'division' and 'use.grch37' cannot both be specified"))
        top_url <- paste0(.ENSEMBLGENOMES_FTP_PUB_URL, division, "/")
    } else if (use.grch37) {
        top_url <- .ENSEMBL_FTP_PUB_GRCH37_URL
    } else {
        top_url <- .ENSEMBL_FTP_PUB_URL
    }
    top_url
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### get_current_Ensembl_release()
###

### The keys are FTP URLs to Ensembl division top-level directories e.g.
###   "ftp://ftp.ensembl.org/pub/"
###   "ftp://ftp.ensembl.org/pub/grch37/"
###   "ftp://ftp.ensemblgenomes.org/pub/plants/"
### etc...
.Ensembl_FTP_cached_releases <- new.env(parent=emptyenv())

list_Ensembl_releases <- function(division=NA, use.grch37=FALSE,
                                  as.ftp.subdirs=FALSE)
{
    if (!isTRUEorFALSE(as.ftp.subdirs))
        stop(wmsg("'as.ftp.subdirs' must be TRUE or FALSE"))
    top_url <- .get_Ensembl_FTP_top_url(division=division,
                                        use.grch37=use.grch37)
    releases <- .Ensembl_FTP_cached_releases[[top_url]]
    if (is.null(releases)) {
        ## TODO: Put list_ftp_dir() in a place that makes it available
        ## to UCSC.utils, Ensembl.utils, and txdbmaker.
        top_files <- GenomeInfoDb::list_ftp_dir(top_url)
        nc <- nchar(.ENSEMBL_FTP_RELEASE_PREFIX)
        prefixes <- substr(top_files, 1L, nc)
        releases <- top_files[prefixes == .ENSEMBL_FTP_RELEASE_PREFIX]
        releases <- substr(releases, nc + 1L, nchar(releases))
        releases <- sort(as.integer(releases))
        .Ensembl_FTP_cached_releases[[top_url]] <- releases
    }
    if (as.ftp.subdirs)
        releases <- paste0(.ENSEMBL_FTP_RELEASE_PREFIX, releases)
    releases
}

### TODO: Cache the result.
### TODO: Maybe try to re-implement this using the REST API e.g. to get the
### the current version of the main division:
###   response <- httr::GET("https://rest.ensembl.org/info/data/",
###                         httr::add_headers(Accept="application/json"))
###   json_line <- httr::content(response, as="text", encoding="UTF-8")
###   unlist(jsonlite::fromJSON(json_line))
### Can we do the same for the other divisions?
get_current_Ensembl_release <- function(division=NA, use.grch37=FALSE)
{
    releases <- list_Ensembl_releases(division=division,
                                      use.grch37=use.grch37)
    current_release <- max(as.integer(releases))
    ## The latest release is not necessarily **officially** released, in
    ## which case the corresponding FTP dir is incomplete. The following
    ## code tries to detect this situation by checking the presence of the
    ## README file.
    top_url <- .get_Ensembl_FTP_top_url(division=division,
                                        use.grch37=use.grch37)
    README_url <- paste0(top_url, .ENSEMBL_FTP_RELEASE_PREFIX,
                         current_release, "/README")
    res <- try(suppressWarnings(
        download.file(README_url, tempfile(), quiet=TRUE)
    ), silent=TRUE)
    if (inherits(res, "try-error"))
        current_release <- current_release - 1L
    current_release
}

