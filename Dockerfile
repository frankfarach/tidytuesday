# Dockerfile for Tidy Tuesday development
# Upstream repo: https://github.com/rfordatascience/tidytuesday

# Base image off of R, RStudio, and the tidyverse
FROM rocker/verse

MAINTAINER Frank Farach (frank.farach@gmail.com)

COPY . /tidyverse

# Install additional R packages
RUN apt-get update -qq \
    && apt-get -y --no-install-recommends install \
    libproj-dev \
    libpoppler-cpp-dev \
    libapparmor-dev \
    libtesseract-dev \
    libleptonica-dev \
    && R -e "devtools::install_github('dgrtwo/gganimate', dep = TRUE)" \
    && install2.r --error --deps TRUE \
    ggalt \
    skimr \
    tweenr

# Run as follows:
# docker run --rm -d -p 8787:8787
