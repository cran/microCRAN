# set up microCRAN service

ARG RBASE_VERSION=4.3.1

FROM rocker/r-base:${RBASE_VERSION}

LABEL maintainer="Stefan McKinnon Edwards <smhe@kamstrup.com>"

RUN alias ll='ls -l --color="auto"'

RUN apt-get update && \
  apt-get install -y vim less libsodium-dev libcurl4-openssl-dev

RUN R -e "install.packages('remotes')"

# --link requires Buildkit, see https://docs.docker.com/build/buildkit/#getting-started
COPY --link *.tar.gz /tmp

RUN R -q -e "packages <- list.files('/tmp', '*.tar.gz', full.names = TRUE)" \
         -e "sapply(packages, remotes::install_local, dependencies = c('Depends','Imports'))"

COPY --chmod=744 init /init

CMD ["/init"]
