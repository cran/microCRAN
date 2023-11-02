## API interface definitions
#' @import plumber
NULL


`_microCRAN_title` = "microCRAN repository"
`_microCRAN_description` =
"A standalone CRAN repository, from which end-users can install R-packages
without having direct access to the local files in the repository.

Besides the endpoints listed here, the files are available through the
normal paths `/src/contrib`, `/bin/windows/contrib` and `/bin/macosx/contrib`.

To use in R, add this url to R's list of repositories, e.g.:
```
local({
    r <- getOption(\"repos\")
    r[\"microCRAN\"] <- \"http://my.local.microcran:port/\"
    options(repos = r)
})
```
or `available.packages(repos = \"http://my.local.microcran:port/\")` or
`install.packages(repos = \"http://my.local.microcran:port/\")`.
"

# TODO: Add method with endpoint for refreshing subdirectories and PACKAGES.


#' Build and start the microCRAN site
#'
#' `build` creates the [Plumber-router][plumber::Plumber] and
#' `run` starts the service.
#'
#' Point `repo_dir` to you *local* filesystem. If the (sub-)directory does not
#' exist, it will be created when an R-package is added through the corresponding
#' endpoint.
#'
#' @section API Descriptions:
#' The fields `title`, `description`, `contact`, `license`, and `tos`
#' are used for describing the API in the resulting Swagger-documents.
#' These follow the OpenAPI type descriptions, see
#' <https://spec.openapis.org/oas/v3.0.3#info-object>.
#'
#' Field name | Type | Description
#' -----------|------|---------------------
#' title      | string | The title of the API.
#' description | string | A short description of the API.
#' tos | string | A URL to the Terms of Service for the API.
#' contact | list | A "Contact Object", i.e., list-object with fields "name", "url" and "email".
#' license | list | A "License Object", i.e., list-object with fields "name" and "url".
#' version | string | The version of the API.
#'
#'
#' @param repo_dir Path to local directory, where the root of the repository is.
#'   The (source) packages will be stored locally at
#'   `{repo_dir}/src/contrib/`.
#' @param redirect_url Url, if supplied, requests to static assets
#'   (package files, etc.) are redirected to another service instead of being
#'   handled by [cran_static_path_handler].
#'   It can be beneficial to let e.g. an Apache httpd service handle those.
#' @param run Logical, should the method run the site immediately?
#' @param title,description,contact,license,tos Descriptions of the API.
#'   Some defaults are used, see section below or
#'   <https://www.rplumber.io/articles/annotations.html>.
#' @import plumber
#' @importFrom rlang maybe_missing is_missing missing_arg
#' @include handlers-.R
#' @include handlers-static.R
#' @export
#' @md
#' @returns A new [Plumber-router][plumber::Plumber] object.
#' @rdname build
build <- function(repo_dir, url_path, redirect_url,
  title, description, contact, license, tos) {

  assertthat::assert_that(
    is_missing(repo_dir) || rlang::is_scalar_character(repo_dir)
  )

  if (is_missing(redirect_url)) {
    static_handler <- cran_static_path_handler(repo_dir, path_prefix = maybe_missing(url_path, NULL))
  } else {
    static_handler <- cran_static_redirect_handler(redirect_url)
  }

  spec_info <- list(
    title = maybe_missing(title, `_microCRAN_title`),
    description = maybe_missing(description, `_microCRAN_description`),
    termsOfService = maybe_missing(tos, NULL),
    contact = maybe_missing(contact, NULL),
    license = maybe_missing(license, NULL),
    version = "0.9.0"
  )

  plumber::pr() |>
    pr_set_api_spec(function(spec) {
      spec$info <- spec_info
      spec
    }) |>
    pr_filter('log', function(req) {
      ts <- strftime(Sys.time(), format = '%Y-%m-%d %H:%M:%S')
      cat(req$REQUEST_METHOD, ts, req$PATH_INFO, '\n')
      forward()
    }) |>
    pr_get('/', \(res) {
      include_html(
        system.file('www/index.html', package = 'microCRAN', mustWork = TRUE),
        res
      )
    }) |>
    pr_filter('static', static_handler) |>
    pr_set_error(cran_error_handler) |>
    pr_add_package('/add', repo_dir)
}

#' @param pr A [Plumber-router][plumber::Plumber], e.g. as returned from
#'   `build`.
#' @param url_path Optional prefix to endpoint. The CRAN repository will be
#'   available at e.g. `http://127.0.0.1:port/path_prefix/` with the
#'   "contrib.url" as `http://127.0.0.1:port/path_prefix/src/contrib/`.
#' @param host A string that is a valid IPv4 or IPv6 address that is owned by
#'  this server, which the application will listen on.
#'  "0.0.0.0" represents all IPv4 addresses and "::/0" represents all IPv6
#'  addresses.
#' @param port A number or integer that indicates the server port that should
#'   be listened on. Note that on most Unix-like systems including Linux and
#'   Mac OS X, port numbers smaller than 1025 require root privileges.

#' @param ... Additional arguments passed on to [plumber::pr_run()].
#' @md
#' @rdname build
#' @export
run <- function(pr, host = '127.0.0.1', port = 1881, url_path, ...) {
  assertthat::assert_that(
    is_missing(url_path) || rlang::is_scalar_character(url_path) && (
      !url_path %in% c(NA, '', '/')
    )
  )
  pr_ <- pr

  if (!is_missing(url_path)) {
    pr <- pr_get(pr(), url_path, function(req, res) {
        res$setHeader("Location",
          paste0(req$rook.url_scheme,'://', req$HTTP_HOST,  req$PATH_INFO, '/'))
        res$status <- 308L
        return(res)
      }, comments = "Path to repository") |>
      pr_mount(url_path, pr) |>
      pr_set_api_spec(\(spec) {
        spec$info <- pr_$getApiSpec()$info
        spec
      })
  }

  pr_run(pr, host = host, port = port, ...)
  pr
}
