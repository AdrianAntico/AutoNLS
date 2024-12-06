# Use the official rocker/shiny base image
FROM rocker/shiny:latest

# Set environment variables for Shiny
ENV SHINY_PORT=3838 \
    APP_HOME=/srv/shiny-server \
    CRAN_REPO=https://cloud.r-project.org

# Set working directory
WORKDIR ${APP_HOME}

# Copy application files
COPY . ${APP_HOME}

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R dependencies
RUN R -e "install.packages(c('shiny', 'data.table', 'DT', 'echarts4r', 'minpack.lm', 'devtools'), repos='${CRAN_REPO}')"
RUN R -e "devtools::install_local('${APP_HOME}')"

# Set permissions
RUN chown -R shiny:shiny ${APP_HOME}

# Expose Shiny Server port
EXPOSE ${SHINY_PORT}

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]
