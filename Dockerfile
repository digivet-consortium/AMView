FROM rocker/r-bspm:testing

# Copy the app to a temp directory on the image
RUN mkdir /tmp/app
COPY . /tmp/app

RUN apt-get update \
  && Rscript -e "remotes::install_local('/tmp/app')" \
  && echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site \
  && echo "$(grep -i ^package /tmp/app/DESCRIPTION | cut -d : -d \  -f 2)::run_app()" > /root/app.R \
  && rm -rf /tmp/* \
  && rm -rf /var/lib/apt/lists/*

EXPOSE 3838

CMD ["Rscript", "/root/app.R"]
