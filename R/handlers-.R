#' Plumber route for adding package to repository
#'
#' Creates a [Plumber route][plumber::pr_handle()] that handles an incoming
#' R-package.
#' Use rather [build()] to build the entire API.
#'
#' @param pr A Plumber router-object
#' @param path The path to the endpoint
#' @inheritParams build
#' @include packages.R
#' @seealso [build()], [plumber::pr_post]
#' @returns `NULL` invisibly; called to modify the response.
#' @md
#' @export
pr_add_package <- function(pr, path = '/add', repo_dir) {
  pr_handle(pr, methods = c('POST','PUT'), path = path,
    params = list(file = list(
      type = "file", required = "true",
      desc = "R-package file, either zip-file or gzipped tarball"
    )),
    serializer = serializer_text(),
    comments = "Adds/updates a new package/version to the package registry",
    description = "Must be either `.tar.gz` for a source package,
`.tgz` for a MacOS X binary package, or
`.zip` for a windows binary package.

The type of package is determined from the file extension.",
    responses = list(
      "200" = list(description = "When package succesfully added/replaced"),
      "400" = list(description = "If error in uploaded package-file")
    ),
    handler = function(req, res, file) {
      orig_fn <- names(file)[[1]]
      type <- r_package_type(orig_fn)

      temp_fn <- tempfile()
      writeBin(file[[1]], temp_fn)

      method <- req$REQUEST_METHOD

      addPackage(temp_fn, type = type, repo_dir, is.new = method == "POST")

      res$status <- 204L
    }
  )
}

# TODO: Generate error message based on HTTP status code.

#' Creates and handle a (error) condition
#'
#'
#' @param req,res A "request"- and "response"-object, respectively
#' @param e The error/[condition][base::conditions] that was thrown by a handler,
#'   preferable a [http_condition].
#' @keywords internal
#' @returns `NULL` invisibly; called to modify the response.
#' @export
cran_error_handler <- function(req, res, e) {
  if (!inherits(e, 'http_condition')) {
    stop(e)
  }

  res$serializer <- serializer_text()
  res$status <- e$status_code
  res$body <- e$message
  invisible(NULL)
}

#' @rdname cran_error_handler
#' @param status_code Integer HTTP Status Code
#' @param message Optional message to display (text).
#' @param ... Other things to include in exception.
#' @param type Type of condition to generate.
#'  Must be one of `error`, `warning` or `message`.
#' @param call The call stored in the condition object.
http_condition <- function(status_code, message, ..., type = NULL, call = sys.call(-1)) {
  type <- match.arg(type, c("error", "warning", "message"))
  status_code <- as.integer(status_code)
  if (rlang::is_missing(message)) message <- paste0('Status code: ', status_code)

  status_type <- (status_code %/% 100) * 100L
  http_class <- paste0("http_", unique(c(status_code, status_type, "error")))

  structure(
    list(
      message = message, status_code = status_code, status_type = status_type,
      call = call,
      ...
    ),
    class = c(http_class, 'http_condition', type, "condition")
  )
}
