FROM rocker/shiny-verse:4.1

ENV DEBIAN_FRONTEND noninteractive
ENV PYTHONUNBUFFERED 1
ENV TZ UTC

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    build-essential \
    gdal-bin \
    libgdal-dev \
    libhdf5-serial-dev \
    libssl-dev \
    locales \
    procps \
    unzip \
    wget \
    && locale-gen en_US.UTF-8 \
    && update-locale \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'update.packages()'

RUN Rscript -e 'install.packages(c(\
    "data.table",\
    "devtools",\
    "dygraphs",\
    "fasttime",\
    "future",\
    "leaflet",\
    "plotly",\
    "promises",\
    "shinycssloaders",\
    "shinydashboard",\
    "shinyjs",\
    "xts"\
    ))'

RUN Rscript -e 'devtools::install_github("uataq/uataq")'
