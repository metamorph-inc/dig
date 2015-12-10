FROM ubuntu:14.04
MAINTAINER Kevin Smyth <kevin.m.smyth@gmail.com>

RUN apt-get -qq update && apt-get install -y --no-install-recommends apt-transport-https ca-certificates
RUN echo 'deb https://cran.cnr.berkeley.edu/bin/linux/ubuntu trusty/' > /etc/apt/sources.list.d/cran.list && \
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 && \
apt-get update -qq && \
apt-get install -y --no-install-recommends r-base build-essential gdebi-core wget && \
su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\"" && \
wget -q https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.1.759-amd64.deb && \
gdebi -n shiny-server-1.4.1.759-amd64.deb



# /opt/shiny-server/bin/deploy-example default
# rm -rf /tmp/Rtmp3UiJpA/downloaded_packages

ADD Dig /srv/shiny-server/Dig
# RUN chmod -R 755 /srv/shiny-server/Dig

# TODO rmarkdown?
# /etc/shiny-server/shiny-server.conf
# http://localhost:3838/sample-apps/hello/
# sudo restart shiny-server

EXPOSE 3838

CMD ["shiny-server", "--pidfile=/var/run/shiny-server.pid"]

# ln -s /media/sf_kevin/Documents/dig /srv/shiny-server/dig
#  docker run -v `pwd`/Dig:/srv/shiny-server/Dig -v /media/sf_kevin/Downloads/:/media/sf_kevin/Downloads -p 3838:3838 --name dig dig
#  http://localhost:3838/Dig/?csvfilename=BladeMDA-XFoil-BigRun.csv
