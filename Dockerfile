#
# R with tidyverse
#

FROM rocker/r-ver:4.2.3
#ARG REPO='https://packagemanager.rstudio.com/cran/__linux__/jammy/2023-05-03'
ARG REPO='https://packagemanager.posit.co/cran/__linux__/jammy/2023-04-14'

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
RUN apt-get install -y default-jdk

# timezone
ENV TZ=Europe/Helsinki
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN R -e "install.packages('tidyverse', repos = '$REPO', dependencies = TRUE)"
RUN R -e "install.packages('shiny', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('shinydashboard', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('shinycssloaders', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('shinyWidgets', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('shinybusy', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('colourpicker', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('sodium', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('bigrquery', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('checkmate', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('SqlRender', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('ggpubr', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('ggiraph', repos ='$REPO', dependencies = TRUE)"
RUN R -e "install.packages('DT', repos ='$REPO', dependencies = TRUE)"

# github
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('OHDSI/CohortGenerator@main')"
RUN R -e "remotes::install_github('ohdsi/Eunomia@main')"
RUN R -e "remotes::install_github('OHDSI/FeatureExtraction@main')"
RUN R -e "remotes::install_github('OHDSI/ResultModelManager@main')"
RUN R -e "remotes::install_github('ohdsi/ROhdsiWebApi@main')"
RUN R -e "remotes::install_github('javier-gracia-tabuenca-tuni/HadesExtras')"
RUN R -e "remotes::install_github('FINNGEN/FinnGenUtilsR')"

# run as root
COPY app/ .

# set up Renviron
ARG VERSION
RUN echo 'VERSION='$VERSION >> /root/.Renviron

# run app
ENTRYPOINT ["/usr/local/bin/R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 8559)"]

## login as root into running container
#
# docker exec -u root -t -i <container-id> /bin/bash
#

## Path mapping: -v /path/on/host:/path/inside/container
#

