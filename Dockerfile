FROM openanalytics/r-base

# install Debian dependencies for R
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    libgit2-dev 
 
# packages needed renv and install
RUN R -e "install.packages(c('renv', 'devtools'), repos='https://cloud.r-project.org'); renv::consent(provided = TRUE)"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com')"

# create root folder for app in container
RUN mkdir /reports
RUN mkdir /data
 
# copy the app to the image
COPY /reports/app.R /reports/app.R
COPY /data/processed /data/processed
 
EXPOSE 8080
 
CMD ["R", "-e", "shiny::runApp(appDir = '/reports', port = 8080, host = '0.0.0.0')"]