# get shiny serveR and a version of R from the rocker project
FROM rocker/shiny:4.0.5

# system libraries
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 

# required R packages
RUN R -e 'install.packages(c(\
    "shiny" \
), \
repos = "http://cran.rstudio.com")'

# copy the app directory into the image
COPY ./shiny-app/* /srv/shiny-server/

# run app
CMD ["/usr/bin/shiny-server"]