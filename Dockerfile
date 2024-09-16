# For finding latest versions of the base image see
# https://github.com/SwissDataScienceCenter/renkulab-docker
# ARG RENKU_BASE_IMAGE=renku/renkulab-r:4.2.0-0.13.1
# ARG RENKU_BASE_IMAGE=renku/renkulab-r:latest
ARG RENKU_BASE_IMAGE=renku/renkulab-r:4.3.1-cf5f862
FROM ${RENKU_BASE_IMAGE}

# Uncomment and adapt if code is to be included in the image
# COPY src /code/src

# Uncomment and adapt if your R or python packages require extra linux (ubuntu) software
# e.g. the following installs apt-utils and vim; each pkg on its own line, all lines
# except for the last end with backslash '\' to continue the RUN line
#
USER root
RUN apt-get -y update && \
  apt-get clean && \
  apt-get install -y --no-install-recommends \
  apt-utils \
    man-db \
    libncurses5-dev \
    libncursesw5-dev \
    parallel \
    libgit2-dev \
    tk-dev \
    jq \
    curl \
    tmux \
    neovim \
    fzf \
    ncdu \
    htop \
    curl \
    librsvg2-2 \
    librsvg2-bin \
    librsvg2-dev \
    librsvg2-common \
    diffutils

# note jq and curl are used in renv_install.sh
# librsvg2 libs are common dependencies of some R plotting libraries

# Infinite RStudio session timeout
RUN echo "session-timeout-minutes=0" >> /etc/rstudio/rsession.conf

USER ${NB_USER}

# install the R dependencies
## make the renv install script and renv.lock file
## available in the working dir and run the install

# RUN true fixes issues with local docker build see:
# https://stackoverflow.com/questions/51115856/docker-failed-to-export-image-failed-to-create-image-failed-to-get-layer

COPY .renv_install.sh .
RUN true
COPY renv.lock .
RUN bash .renv_install.sh
## ensure renv lock is in the project directory
COPY renv.lock /home/rstudio/renv.lock
RUN true
COPY install.R /tmp/
RUN R -f /tmp/install.R

# To apply a custom RStudio config uncomment the line below
ENV RSTUDIO_CONFIG_HOME=/home/rstudio/work/shinyisar/.rstudio_config_dir

## Clean up the /home/rstudio directory to avoid confusion in nested R projects
RUN rm /home/rstudio/.Rprofile; rm /home/rstudio/renv.lock

# install the python dependencies
# COPY requirements.txt /tmp/
# RUN pip3 install -r /tmp/requirements.txt

# install the python dependencies
RUN /opt/conda/bin/python3 -m pip install --upgrade pip
COPY requirements.txt environment.yml /tmp/
RUN conda env update -q -f /tmp/environment.yml && \
    /opt/conda/bin/pip install -r /tmp/requirements.txt && \
    conda clean -y --all && \
    conda env export -n "root"

# RENKU_VERSION determines the version of the renku CLI
# that will be used in this image. To find the latest version,
# visit https://pypi.org/project/renku/#history.
#ARG RENKU_VERSION=2.2.0
ARG RENKU_VERSION=2.8.0

########################################################
# Do not edit this section and do not add anything below

# Install renku from pypi or from github if it's a dev version
RUN if [ -n "$RENKU_VERSION" ] ; then \
        source .renku/venv/bin/activate ; \
        currentversion=$(renku --version) ; \
        if [ "$RENKU_VERSION" != "$currentversion" ] ; then \
            pip uninstall renku -y ; \
            gitversion=$(echo "$RENKU_VERSION" | sed -n "s/^[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\(rc[[:digit:]]\+\)*\(\.dev[[:digit:]]\+\)*\(+g\([a-f0-9]\+\)\)*\(+dirty\)*$/\4/p") ; \
            if [ -n "$gitversion" ] ; then \
                pip install --force "git+https://github.com/SwissDataScienceCenter/renku-python.git@$gitversion" ;\
            else \
                pip install --force renku==${RENKU_VERSION} ;\
            fi \
        fi \
    fi

########################################################
