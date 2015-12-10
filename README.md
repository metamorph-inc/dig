# dig
Magical information discovery. Designed for mortals.


## Docker deployment

    docker build --tag dig .
    docker run -d -v "$(pwd)"/webserver/public/csvs/:/media/sf_kevin/Downloads -p 3838:3838 --name dig dig
    cd webserver
    docker build -t dig_webserver .
    sudo docker run -d -p 4545:4545 --link dig -v "$(pwd)"/public/csvs/:/srv/public/csvs/ --name dig_webserver dig_webserver

http://localhost:4545/

Fix permission issue (TODO: when and why does this appear)

    docker exec -it dig bash
    cd /srv/shiny-server/Dig
    sudo -u shiny ls
    cd ../
    cp -r Dig xxx
    rm -rf Dig
    mv xxx Dig
    cd Dig
    chmod o+x .
    sudo -u shiny ls
