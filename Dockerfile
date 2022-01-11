FROM rocker/r-base:latest

LABEL maintainer "Wiktor Gustafsson <wiktor.gustafsson@sva.se>"

# install dependenices
RUN R -e "install.packages(c('shiny', 'markdown'))"

# Install package
RUN cd .. && R CMD build s && R CMD INSTALL shinyTemplate_*.tar.gz

EXPOSE 3838

CMD ["R", "-e", "shinyTemplate::run_app()"]
