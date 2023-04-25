# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

RUN apt-get update -qq && \
    apt-get install -y -qq --no-install-recommends \
        libz-dev \
        libpoppler-cpp-dev \
        pandoc \
        curl

RUN curl -L http://bit.ly/google-chrome-stable -o google-chrome-stable.deb && \
    apt-get -y install ./google-chrome-stable.deb && \
    rm google-chrome-stable.deb

RUN Rscript -e 'install.packages(c("remotes"))'
RUN Rscript -e 'remotes::install_local("..", dependencies = TRUE)'

COPY ../inst/app/server.R app/server.R
COPY ../inst/app/ui.R app/ui.R

EXPOSE 3838

CMD ["R", "-e", "CTUCosting::run_costing_app(host = '0.0.0.0', port = 3838)"]
