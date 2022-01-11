FROM rocker/r-base:latest

LABEL maintainer "Wiktor Gustafsson <wiktor.gustafsson@sva.se>"

# install dependenices
RUN R -e "install.packages(c('shiny', 'markdown'))"

# copy the app to the image
RUN mkdir /root/shinyTemplate
COPY . /root/shinyTemplate

# Install package
RUN R CMD INSTALL /root/shinyTemplate

EXPOSE 3838

CMD ["R", "-e", "shinyTemplate::run_app()"]
