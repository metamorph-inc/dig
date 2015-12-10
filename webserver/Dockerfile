FROM ubuntu:14.04
MAINTAINER Kevin Smyth <kevin.m.smyth@gmail.com>

RUN apt-get -qq update && apt-get install -y --no-install-recommends apt-transport-https ca-certificates
RUN echo 'deb https://deb.nodesource.com/node_4.x trusty main' > /etc/apt/sources.list.d/nodesource.list && \
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 68576280 && \
apt-get update -qq && \
apt-get install -y --no-install-recommends nodejs

WORKDIR /srv

ADD package.json /srv/package.json
RUN npm install

ADD . /srv/

EXPOSE 4545
VOLUME /srv/public/csvs/

CMD ["node", "app.js"]

# docker build -t dig_webserver .
# sudo docker run -p 4545:4545 --link dig -v `pwd`/public/csvs/:/srv/public/csvs/ --name dig_webserver dig_webserver
