FROM achubaty/spades-project:4.3.3

LABEL org.opencontainers.image.authors="achubaty@for-cast.ca"

## safely allow ssh access to private GitHub repos via the build machine's ssh
RUN mkdir -p -m 0700 ~/.ssh && ssh-keyscan github.com >> ~/.ssh/known_hosts

## update uid:gid for default user to mach some user
## pass USERNAME as environment variable during build if don't want `rstudio`
ARG USERNAME=rstudio
ARG USER_UID=1000
ARG USER_GID=1000

ENV RENV_PATHS_CACHE=/home/${USERNAME}/.cache/R/renv
ENV RENV_PATHS_PREFIX_AUTO=TRUE
ENV RENV_WATCHDOG_ENABLED=FALSE

RUN mkdir -p ${RENV_PATHS_CACHE}

## clone project repo and set up additional project directories
WORKDIR /home/${USERNAME}/GitHub

ARG GH_ORG=FOR-CAST
ARG GH_REPO=Ontario_AOU_ROF
ARG GH_TAG=main
RUN --mount=type=ssh git clone --single-branch -b $GH_TAG --recurse-submodules \
   -j8 https://github.com/$GH_ORG/$GH_REPO

WORKDIR /home/${USERNAME}/GitHub/$GH_REPO

RUN mkdir cache inputs outputs

## pre-install R packages
COPY renv/settings.json renv/settings.json
RUN Rscript -e 'options(Ncpus = max(1, min(8, parallel::detectCores()))); renv::restore()'

## set default project (https://stackoverflow.com/a/53547334/1380598)
RUN mkdir -p /home/${USERNAME}/.rstudio/projects_settings \
    && echo /home/${USERNAME}/GitHub/$GH_REPO/$GH_REPO.Rproj > \
            /home/${USERNAME}/.rstudio/projects_settings/switch-to-project

## set user permissions
RUN groupmod --gid $USER_GID ${USERNAME} \
    && usermod --uid $USER_UID --gid $USER_GID ${USERNAME} \
    && chown -R $USER_UID:$USER_GID /home/${USERNAME}
