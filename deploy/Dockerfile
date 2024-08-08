# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest
#
# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# install renv & restore packages
RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_github("LandSciTech/ccviR")'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "ccviR::run_ccvi_app('demo', port = 80)"]
