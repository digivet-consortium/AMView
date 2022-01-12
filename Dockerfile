FROM rocker/r-base:latest

LABEL maintainer "Wiktor Gustafsson <wiktor.gustafsson@sva.se>"

# Install dependencies
RUN Rscript -e "install.packages(c('shiny', 'markdown'))"

# Copy the app to a temp directory on the image
RUN mkdir /tmp/app
COPY . /tmp/app

# Install package
RUN R CMD INSTALL /tmp/app

## Clean up from R source install
RUN rm -rf /tmp/*

# copy in the Rprofile to define shiny ports
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shinyTemplate::run_app()"]
