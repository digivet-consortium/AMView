FROM rocker/r-base:latest

LABEL maintainer "Wiktor Gustafsson <wiktor.gustafsson@sva.se>"

# Install dependencies
RUN R -e "install.packages(c('shiny', 'markdown'))"

# Copy the app to a temp directory on the image
RUN mkdir /tmp/shinyTemplate
COPY . /tmp/shinyTemplate

# Install package
RUN R CMD INSTALL /tmp/shinyTemplate

## Clean up from R source install
RUN rm -rf /tmp/*

EXPOSE 3838

CMD ["R", "-e", "shinyTemplate::run_app()"]
