#
# R with tidyverse
#
#

# v0.7.6 build
FROM rocker/r-ver:4.3.0
ARG REPO='https://packagemanager.rstudio.com/cran/__linux__/jammy/2023-05-03'

LABEL maintainer="harri.siirtola@tuni.fi"

# use mirrors
RUN sed -i -e 's/http:\/\/archive\.ubuntu\.com\/ubuntu\//mirror:\/\/mirrors\.ubuntu\.com\/mirrors\.txt/' /etc/apt/sources.list

RUN apt-get update && apt-get install -y libicu-dev

# git & buildtools
RUN apt-get install -y git
RUN apt-get install -y build-essential

# for Cairo
RUN apt-get install -y libxt-dev
RUN apt-get install -y libcairo-dev
RUN apt-get install -y libharfbuzz-dev

# for RPostgreSQL
RUN apt-get install -y libpq-dev
RUN apt-get install -y postgresql

# java runtime
RUN apt-get install -y default-jre
# RUN CMD javareconf # not needed?

# timezone
ENV TZ=Europe/Helsinki
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN R -e "install.packages('tidyverse', repos = '$REPO' )"
RUN R -e "install.packages('shiny', repos ='$REPO')"
RUN R -e "install.packages('shinydashboard', repos ='$REPO')"
RUN R -e "install.packages('shinycssloaders', repos ='$REPO')"
RUN R -e "install.packages('shinyWidgets', repos ='$REPO')"
RUN R -e "install.packages('shinybusy', repos ='$REPO')"
RUN R -e "install.packages('colourpicker', repos ='$REPO')"
RUN R -e "install.packages('sodium', repos ='$REPO')"
RUN R -e "install.packages('bigrquery', repos ='$REPO')"
RUN R -e "install.packages('checkmate', repos ='$REPO')"
RUN R -e "install.packages('SqlRender', repos ='$REPO')"
RUN R -e "install.packages('ggpubr', repos ='$REPO')"
RUN R -e "install.packages('ggiraph', repos ='$REPO')"
RUN R -e "install.packages('DT', repos ='$REPO')"

# FinnGenUtilsR
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('FINNGEN/FinnGenUtilsR')"

# setup user shiny
RUN useradd -ms /bin/bash shiny
USER shiny
WORKDIR "/home/shiny"
RUN mkdir app
COPY app/* app/
#RUN mkdir app/bq_auth
#COPY bq_auth/ app/bq_auth/

# set up Renviron
ARG VERSION
RUN echo 'VERSION='$VERSION >> /home/shiny/.Renviron

# run app
CMD ["R", "-e", "shiny::runApp(appDir = '/home/shiny/app', host = '0.0.0.0', port = 8559)"]

## login as root into running container
#
# docker exec -u root -t -i lifetrack /bin/bash
#

## Path mapping: -v /path/on/host:/path/inside/container
#

