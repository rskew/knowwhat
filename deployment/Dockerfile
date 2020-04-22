FROM nginx

## Build frontend to be served by NGINX

# Install build deps
RUN apt update \
  && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
  nodejs \
  git \
  libtinfo5 \
  npm
RUN npm install npm@latest -g
WORKDIR /app
COPY package.json /app
COPY package-lock.json /app
RUN npm install
RUN npm install --global --unsafe-perm purescript@0.13.4 spago@0.10.0 parcel-bundler

# Build Purescript source
COPY src /app/src
COPY test /app/test
COPY spago.dhall /app
COPY packages.dhall /app
RUN spago test

# Build web app
COPY assets /app/assets
RUN parcel build assets/index.html

ENTRYPOINT nginx
