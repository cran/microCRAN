#' Adds a package-file to the repository
#'
#' Methods for taking a file (".tar.gz", ".zip", ".tgz") and placing it in the
#' repository, all locally.
#'
#' @param fn Path to package
#' @param type Type of package, see explanation in section
#'  "Binary packages" in [utils::install.packages()].
#' @inheritParams build
#' @param is.new Logical, if `TRUE`, it causes an error if the file
#' @md
#' @export
#' @seealso [miniCRAN::addLocalPackage()], [tools::write_PACKAGES()]
#' @returns Invisibly returns the number of packages described in the resulting
#'   `'PACKAGES'`, `'PACKAGES.gz'` and `'PACKAGES.rds'` files.
#'   If `0`, no packages were found and no files were written.
#' @include miniCRAN.R
#' @examples
#' f <- system.file('extdata/microCRAN_0.1.0.zip', package = 'microCRAN', mustWork = TRUE)
#' root <- tempdir()
#' addPackage(f, type = 'win.binary', repo_dir = root)
#'
addPackage <- function(fn, type = c('source','mac.binary','win.binary'),
    repo_dir, is.new = TRUE) {
  type = match.arg(type)
  assertthat::assert_that(
    rlang::is_scalar_logical(is.new)
  )

  DESCRIPTION_fun <- switch(type,
    source = read_DESCRIPTION_tar,
    mac.binary = read_DESCRIPTION_tar,
    win.binary = read_DESCRIPTION_zip
  )
  DESCRIPTION <- DESCRIPTION_fun(fn)
  built <- if (type != 'source') extract_built(DESCRIPTION$Built) else rlang::missing_arg()

  dest_dir <- file.path(repo_dir, repoPrefix(type, rlang::maybe_missing(built)))

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  ext <- switch(type,
    source = 'tar.gz',
    mac.binary = 'tgz',
    win.binary = 'zip'
  )
  dest_fn <- sprintf('%s_%s.%s', DESCRIPTION$Package, DESCRIPTION$Version, ext)
  file.copy(fn, file.path(dest_dir, dest_fn), overwrite = !is.new)
  tools::write_PACKAGES(dest_dir, type = type, unpacked = FALSE, addFiles = TRUE, subdirs = TRUE)
}

extract_built <- function(s) {
  regmatches(s, gregexpr('(?<=R )([0-9]+\\.[0-9]+(?:\\.[0-9A-Za-z]))(?=;)', s, perl = TRUE))[[1]]
}

is_tar_ext <- function(s) {
  grepl('\\.tar.gz$', s)
}

is_tgz_ext <- function(s) {
  grepl('\\.tgz$', s)
}

is_zip_ext <- function(s) {
  grepl('\\.zip$', s)
}


#' Matches package file extension to package type
#'
#' @param fn Filename or path
#' @returns "source", "mac.binary" or "win.binary", depending on file extension,
#'   or throws a [http_condition].
#' @md
#' @keywords internal
r_package_type <- function(fn) {
  if (is_tar_ext(fn)) {
    return('source')
  } else if (is_tgz_ext(fn)) {
    return('mac.binary')
  } else if (is_zip_ext(fn)) {
    return('win.binary')
  }
  stop(http_condition(403,
    "Invalid file extension. Must be either `.tar.gz`, `.tgz`, or `.zip`."))
}

#' Extracts the path in a tar.gz-file for the DESCRIPTION-file
#' @noRd
#' @importFrom assertthat assert_that
find_description <- function(s) {
  matches <- grepl('^(?:[A-Za-z]){3}[A-Za-z0-9.]*(?<!\\.)/DESCRIPTION$', s, perl = TRUE)
  if (sum(matches) == 0) return(character(0))
  s[which(matches)[1]]
}


#' Read DESCRIPTION file from package
#'
#' @param fn Path to either zip, tar.gz or tgz file.
#' @rdname read-DESCRIPTION
#' @export
#' @returns List-object with contents of DESCRIPTION file.
#' @examples
#' package <- system.file('extdata/microCRAN_0.1.0.zip',
#'   package='microCRAN', mustWork=TRUE)
#' read_DESCRIPTION_zip(package)
#'
read_DESCRIPTION_zip <- function(fn) {
  res <- tryCatch({
    files <- utils::unzip(fn, list = TRUE)
    description_file <- find_description(files$Name)
    stopifnot(length(description_file) == 1)
    con <- unz(fn, description_file)
    on.exit(close(con))
    as.list(read.dcf(con)[1,])
  }, error = identity)
  if (inherits(res, 'error')) {
    stop(errorCondition("Invalid R-package file", line = res$message))
  }
  res
}

#' @rdname read-DESCRIPTION
read_DESCRIPTION_tar <- function(fn) {
  if (!file.exists(fn)) {
    stop('File not found: ', fn)
  }
  res <- tryCatch({
    sink(file(nullfile(), 'wt'), type = "message")
    files <- utils::untar(fn, list=TRUE)
    sink(type = "message")
    description_file <- find_description(files)
    stopifnot(length(description_file) == 1)
    d <- tempdir()
    utils::untar(fn, files = description_file, exdir = d)
    as.list(read.dcf(file.path(d, description_file))[1,])
  }, error = identity)
  if (inherits(res, 'error')) {
    stop(errorCondition("Invalid R-package file", line = res$message))
  }
  res
}
