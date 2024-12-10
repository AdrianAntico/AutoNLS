# Use the official rocker/shiny base image
FROM rocker/shiny:4.4.1

# Set environment variables for Shiny
ENV SHINY_PORT=3838 \
    CRAN_REPO=https://cloud.r-project.org \
    _R_SHLIB_STRIP_=true

# Install system dependencies
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libgl1-mesa-dev \
    libglu1-mesa \
    libglfw3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R dependencies
RUN install2.r --error --skipinstalled \
  devtools \
  R6 \
  data.table \
  dplyr \
  echarts4r \
  minpack.lm \
  mgcv \
  shiny \
  DT \
  bs4Dash \
  readxl \
  here

RUN R -e "devtools::install_github('AdrianAntico/AutoNLS', upgrade = FALSE, dependencies = FALSE, force = TRUE)"

# CMD /bin/bash \
CMD ["R", "-e", "options('shiny.port'=3838, shiny.host='0.0.0.0'); library(AutoNLS); AutoNLS::run_shiny_app(launch_browser = FALSE)"]

EXPOSE 3838

# Optional: Add a health check
HEALTHCHECK CMD curl --fail http://localhost:3838 || exit 1
