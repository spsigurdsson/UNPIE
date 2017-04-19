FROM rocker/tidyverse
MAINTAINER Snorri Pall Sigurdsson

RUN R -e 'devtools::install_github("trestletech/plumber")'
RUN R -e 'devtools::install_github("eaoestergaard/UNPIE")'

ADD /api/* /api/

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(port=8000)"]
CMD ["/api/api.R"]
