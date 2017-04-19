FROM rocker/r-base
MAINTAINER Jeff Allen <docker@trestletech.com>

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev

RUN R -e 'install.packages(c("devtools","Rcpp","httpuv","plumber"))'

RUN R -e 'devtools::install_github("eaoestergaard/UNPIE")'

ADD /api/* /api/

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(port=8000)"]
CMD ["/api/api.R"]
