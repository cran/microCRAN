
#' Handler for package repository files
#'
#' @rdname static-assets
#' @description
#' Creates handler for handling access to static files in the repository,
#' i.e., the `src/contrib`, `bin/windows/contrib`, and `bin/macosx/contrib`
#' subdirectories.
#'
#' Use [cran_static_redirect_handler] to let another process (e.g. an Apache httpd
#' or nginx server) handle the request, by redirecting it.
#'
#' @returns A handler (function) for use in a Plumber router "filter" .
#' @inheritParams build
#' @param path_prefix Optional URL-component, if the repository exists at a
#'   subdirectory of the host (see [run]'s `path`). Here, it is only for
#'   decorative purposes.
#' @import plumber
#' @md
#' @export
#' @examples
#' require(plumber)
#' pr() |>
#'  pr_filter('static',
#'   cran_static_path_handler(repo_dir, path_prefix = 'cran'))
#' pr() |>
#'   pr_filter('redirect',
#'     cran_static_redirect_handler("http://my.local.cran:80/cran"))
cran_static_path_handler <- function(repo_dir, path_prefix = NULL) {
  assertthat::assert_that(
    !length(path_prefix) || rlang::is_scalar_character(path_prefix)
  )
  if (length(path_prefix) && !startsWith(path_prefix, '/')) {
    path_prefix <- paste0('/', path_prefix)
  }
  function(req, res) {
    path_parts <- extract_cran_path_parts(req$PATH_INFO)
    if (!length(path_parts) || path_parts[1] == "" && path_parts[2] != "/") {
      forward()
      return()
    }

    if (path_parts[2] == "/") {
      # go to directory listing
      tryCatch({
        directory_listing(req, res, file.path(repo_dir, path_parts[1]),
          path_prefix = path_prefix)
      }, error = \(e) {
        if (inherits(e, 'http_condition') && e$status_code == 403) {
          e$status_code <- 500
        }
        stop(e)
      })
      return(res)
    }

    abs.path <- file.path(repo_dir, path_parts[1], path_parts[2])
    info <- file.info(abs.path)
    if (is.na(info$size)) {
      stop(http_condition(404, 'Resource not found'))
    }

    content_type = switch(path_parts[2],
      PACKAGES = 'text/plain',
      PACKAGES.rds = 'application/rds',
      PACKAGES.gz = 'application/gzip',
      mime::guess_type(path_parts[2])
    )
    res$setHeader("Content-Type", content_type)
    res$setHeader("Content-Length", info$size)
    res$setHeader("Last-Modified", http_date_string(info$mtime))
    res$body <-
      if (req$REQUEST_METHOD == 'GET') {
        readBin(abs.path, 'raw', n = info$size)
      } else {
        # HEAD request (?)
        NULL
      }
    res$status <- 200
    return(res)
  }
}

#' @import plumber
#' @rdname static-assets
#' @param dest_url The url requests are forwarded to, after being appened with
#'   the request; it should contain all path-components up to the `/src` or `/bin`
#'   parts.
#' @export
cran_static_redirect_handler <- function(dest_url) {
  assertthat::assert_that(
    rlang::is_scalar_character(dest_url)
  )
  dest_url <- trimws(dest_url, 'both', '[ \t\r\n\\/]')
  function(req, res) {
    if (!path_is_safe_for_cran(req$PATH_INFO)) {
      forward()
      return()
    }

    res$setHeader("Location", paste0(dest_url, req$PATH_INFO))
    res$status <- 302L
    return(res)
  }
}

# R-package regexes
# https://cran.r-project.org/doc/manuals/R-exts.html#The-DESCRIPTION-file
# This should contain only (ASCII) letters, numbers and dot,
# have at least two characters and start with a letter and not end in a dot.
# Package name
# (?:[A-Za-z]){3}[A-Za-z0-9.]*(?<!\\.)
# Package version
# (?>([0-9])+[.-]?){2,}(?<![.-])

`_cran_path_pattern` = "^(src/contrib|bin/windows/contrib|bin/macosx/contrib)"


#' Test if path matches repository directory structure
#'
#' Tests if the requested resource maps to a safe path in the respository,
#' i.e., that it points to `/src/contrib`, `/bin/windows/contrib` or
#' `/bin/macosx/contrib`, and does not attempt to weasle back out with `..`.
#'
#' @param s The path to check, typically `req$PATH_INFO` in a request handler.
#'
#' @keywords internal
#' @returns Logical
path_is_safe_for_cran <- function(s) {
  if (grepl('(:|\\.\\.)', s) ||
      !grepl("^/(|src/contrib|bin/windows/contrib|bin/macosx/contrib)/?(.*)", s)) {
    return(FALSE)
  }

  return(TRUE)
}

#' Splits repository "contrib"-paths into a directory path and filename.
#'
#' Splits the requested path into directory-part and filename-part,
#' while ensuring some security to where it may point to.
#'
#' @param s The requested path, e.g. `src/contrib` or
#'   `bin/windows/contrib/4.0/my_package_v0.1.0.zip`.
#' @returns A character vector of length 2, with directory path and filename.
#'   If `s` was a directory (e.g., "src/contrib", "src/contrib/",
#'   "bin/windows/contrib/4.0", etc.) the second element is "".
#'   Can return `NULL` if `s` does not fit the repository directory structure.
#' @md
#' @keywords internal
extract_cran_path_parts <- function(s) {
  if (!path_is_safe_for_cran(s)) {
    return(NULL)
  }

  fn <- basename(s)
  path <- dirname(s) |> trimws('left', '/')

  if (fn == 'contrib' || grepl("[0-9]\\.[0-9]$", fn)) {
    path <- file.path(path, fn)
    fn <- "/"
  }

  return(c(path, fn))
}

#' Directory listing
#'
#' Creates a simple HTML page with table of all files and subdirectories.
#' May throw a 403 or 404 code.
#'
#'
#' @param path Path to directory to list
#' @inheritParams cran_static_path_handler
#' @inheritParams cran_error_handler
#' @inheritParams build
#' @importFrom rlang maybe_missing
#' @export
#' @md
#' @returns
#'   `directory_listing` returns the response-object.
#'   `directory_listing_html` returns a HTML-string with the entire page.
#'
directory_listing <- function(req, res, path, path_prefix) {
  if (!dir.exists(path)) {
    stop(http_condition(404))
  }
  ## test listing
  if (file.access(path, 1) == -1) {
    stop(http_condition(403))
  }

  url_path <- paste0(
    maybe_missing(path_prefix, '/'),
    req$PATH_INFO
  )

  res$serializer <- serializer_html()
  res$body <- directory_listing_html(path, url_path,
    add_dir = !endsWith(req$PATH_INFO, '/'))
  res
}

# nocov start
#' @rdname directory_listing
#' @param url_path The entire, relative path, that is displayed to
#'   end user in the URL.
#' @param add_dir Logical, if the requested URL does not end with a '/',
#'   set this to TRUE, else all links will point to the parent directory.
#' @export
directory_listing_html <- function(path, url_path, add_dir = TRUE) {
  files <- list.files(path, include.dirs = TRUE, full.names = FALSE)
  file_info <- file.info(file.path(path, files))

  self_dir <- if (add_dir) paste0(basename(path),'/') else ''

  table <- data.frame(
    Name = basename(rownames(file_info)),
    mtime = file_info$mtime,
    Size = file_info$size,
    isdir = file_info$isdir
  )
  table <- table[with(table, order(-isdir, Name)), c('Name', 'mtime', 'Size')]
  table$Name <- sprintf('<a href="%s%s">%s</a>', self_dir, table$Name, table$Name)
  table$Size <- format_file_size(table$Size)
  table$mtime <- format(file_info$mtime)

  table <- rbind(
    data.frame(
      Name = sprintf('<a href="%s">Parent directory</a>', url_path),
      mtime = '',
      Size = ''
    ),
    table,
    make.row.names = FALSE
  )

  xtab <- xtable::xtable(table)
  names(xtab) <- c('Name','Last modified','Size')
  xtab <- xtable::print.xtable(xtab, type = 'html',
    print.results = FALSE,
    include.rownames = FALSE,
    sanitize.text.function = identity
  )

  url_path_display <- trimws(url_path, 'r', '[ /]')
  head <- sprintf("<html><head><title>Index of %s</title></head>", url_path_display)
  body <- sprintf('<body<h1>Index of %s</h1>\n%s\n<p style="font-style: italic;">microCRAN package</p>\n</body></html>',
    url_path_display, xtab)

  paste0(head, '\n', body)
}
# nocov end

format_file_size <- function(sizes, factor = 0.9) {
  result <- character(length(sizes))
  bytes <- sizes < 1024*factor
  result[bytes] <- sizes[bytes]
  bytes <- sizes >= 1024*factor & sizes < 1024**2 * factor
  result[bytes] <- paste0(round(sizes[bytes]/1024), 'K')
  bytes <- sizes >= 1024**2 * factor & sizes < 1024**3 * factor
  result[bytes] <- paste0(round(sizes[bytes]/1024**2), 'M')
  bytes <- sizes >= 1024**3 * factor
  result[bytes] <- paste0(round(sizes[bytes]/1024**3), 'G')
  result
}

# nocov start
http_date_string <- function(time) {
  lt <- as.POSIXlt(time, tz = "GMT")
  weekdays <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
              "Aug", "Sep", "Oct", "Nov", "Dec")
  weekday <- weekdays[lt$wday + 1]
  month <- months[lt$mon + 1]
  fmt <- paste0(weekday, ", %d ", month, " %Y %H:%M:%S GMT")
  strftime(time, fmt, tz = "GMT")
}
# nocov end
