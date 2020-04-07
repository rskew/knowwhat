FROM nginx

# Build frontend to be served by NGINX
RUN apt update \
  && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
  nodejs \
  npm \
  git \
  libtinfo5
RUN npm install --global --unsafe-perm purescript@0.13.4 spago@0.10.0 parcel-bundler
COPY src /app/src
COPY test /app/test
COPY spago.dhall /app
COPY packages.dhall /app
WORKDIR /app
RUN spago build # TODO spago test
# TODO don't install purs and spago twice
RUN npm install
COPY . /app
RUN parcel build assets/index.html
ENTRYPOINT nginx
