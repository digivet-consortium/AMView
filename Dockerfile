FROM rocker/r-base:latest

LABEL maintainer "Wiktor Gustafsson <wiktor.gustafsson@sva.se>"

# basic shiny functionality
RUN RSCRIPT -e "install.packages('shiny', repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/shinyTemplate
COPY . /root/shinyTemplate

RUN R CMD INSTALL /root/shinyTemplate

EXPOSE 3838

CMD ["R", "-e", "shinyTemplate::run_app()"]
