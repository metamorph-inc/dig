FROM ubuntu:14.04
MAINTAINER Kevin Smyth <kevin.m.smyth@gmail.com>

RUN apt-get -qq update && apt-get install -y --no-install-recommends apt-transport-https ca-certificates
RUN echo 'deb https://deb.nodesource.com/node_4.x trusty main' > /etc/apt/sources.list.d/nodesource.list && \
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 68576280 && \
apt-get update -qq && \
apt-get install -y --no-install-recommends nodejs git

WORKDIR /srv

ADD package.json /srv/package.json
RUN npm install

ADD . /srv/

RUN node_modules/bower/bin/bower --allow-root update

EXPOSE 4545
VOLUME /srv/public/csvs/

CMD ["node", "app.js"]
