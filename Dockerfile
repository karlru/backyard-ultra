# get shiny server plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY src/ /srv/shiny-server/src/
COPY data/ /srv/shiny-server/data/

# install R packages required 
RUN R -s -f /srv/shiny-server/src/dependencies.R

RUN rm /srv/shiny-server/index.html

# Make the ShinyApp available at port 80
EXPOSE $PORT

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
CMD ["/usr/bin/shiny-server.sh"]