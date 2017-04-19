FROM rocker/tidyverse
MAINTAINER Snorri Pall Sigurdsson <sps@schantz.com>

RUN R -e 'devtools::install_github("trestletech/plumber")'
RUN R -e 'devtools::install_github("eaoestergaard/UNPIE")'

ADD /api/* /api/

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb("/api/api.R"); pr$run(port=8000)"]
