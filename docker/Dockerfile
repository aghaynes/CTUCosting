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

COPY .Renviron .Renviron

RUN Rscript -e 'remotes::install_local("CTUCosting", dependencies = TRUE)'

EXPOSE 3838

CMD ["R", "-e", "CTUCosting::run_costing_app(opts = list(host = '0.0.0.0', port = 3838))"]
