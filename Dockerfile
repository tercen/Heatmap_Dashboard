FROM tercen/runtime-r40:4.0.4-1

ENV RENV_VERSION 0.13.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cran.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

COPY . /operator
WORKDIR /operator

RUN R -e "renv::consent(provided=TRUE);renv::restore(confirm=FALSE)"

COPY start.R /start.R

ENTRYPOINT [ "R","--no-save","--no-restore","--no-environ","--slave","-f","/start.R"]

