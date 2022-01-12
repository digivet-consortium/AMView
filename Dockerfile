FROM rocker/r-bspm:testing

LABEL maintainer "Wiktor Gustafsson <wiktor.gustafsson@sva.se>"

# Copy the app to a temp directory on the image
RUN mkdir /tmp/app
COPY . /tmp/app

RUN Rscript -e "remotes::install_local('/tmp/app')" \
  && echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site \
  && rm -rf /tmp/*

EXPOSE 3838

CMD ["Rscript", "-e", "shinyTemplate::run_app()"]
