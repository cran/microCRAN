\name{docker}
\alias{docker}
\title{Running microCRAN as a Docker container}
\description{
A \code{Dockerfile} with an \code{init}-script is installed with this package, which
can be used for running a Docker container with this package.

The path to the directory with the file can be found by running

\if{html}{\out{<div class="sourceCode">}}\preformatted{system.file('docker/', package = 'microCRAN', mustWork = TRUE)
}\if{html}{\out{</div>}}

}
\section{Running}{

To run, place microCRAN's source-tarball in the build directory with the \code{Dockerfile}.

Enable BuildKit and build the image:

\if{html}{\out{<div class="sourceCode">}}\preformatted{DOCKER_BUILDKIT=1 docker build .
}\if{html}{\out{</div>}}

The build-process will automatically install any source-tarballed R-package
that is located in the build-context (i.e. directory with the \code{Dockerfile}).

Start the container. Remember to map the port and the directory for the repository,
else the repository is lost if the container is restarted.
See the list below for variables to use.

\if{html}{\out{<div class="sourceCode">}}\preformatted{docker run -d -v /data/my_local_cran:/var/cran -p 1881:1881 -e CRAN_DIR=/var/cran <microcran-image>
}\if{html}{\out{</div>}}

\describe{
\item{\code{CRAN_HOST}}{Host for the service to listen to. See also \link[plumber:pr_run]{plumber::pr_run}.}
\item{\code{CRAN_PORT}}{Port to listen on. Defaults to 1881.}
\item{\code{CRAN_DIR}}{Path to root of local directory where the repository's files are stored.}
\item{\code{CRAN_URL_PATH}}{Absolute path to the repository, as seen from the client.}
\item{\code{CRAN_REDIRECT_URL}}{If static files should be served by something else.}
}
}

\seealso{
\code{\link[=build]{build()}}, \link[plumber:pr_run]{plumber::pr_run}
}
\keyword{misc}
