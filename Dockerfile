FROM rocker/r-base:latest

LABEL maintainer "Wiktor Gustafsson <wiktor.gustafsson@sva.se>"

# set CRAN as repo
RUN R -e "options(repos='https://cloud.r-project.org/')"

# install dependenices
RUN R -e "install.packages(c('shiny', 'remotes'))"

# copy the app to the image
RUN mkdir /root/shinyTemplate
COPY . /root/shinyTemplate

RUN R -e "remotes::install_local('/root/shinyTemplate')"

EXPOSE 3838

CMD ["R", "-e", "shinyTemplate::run_app()"]
