/*globals require,process,__dirname*/
'use strict';
/**
 * Created by adam on 12/4/15.
 */

var express = require('express');
var app = express();
var bodyParser = require('body-parser');
var winston = require('winston');
var path = require('path');
var fs = require('fs-extra');
var http = require('http');
var Busboy = require('busboy');

var logger = new (winston.Logger)({
    transports: [
        new (winston.transports.Console)({
            level: 'info',
            colorize: true,
            timestamp: true,
            prettyPrint: true,
            handleExceptions: true, // ignored by default when you create the logger, see the logger.create function
            depth: 2
        }),
        new winston.transports.File({filename: 'server.log'})
    ]
});


// configure app to use bodyParser()
// this will let us get the data from a POST
//app.use(bodyParser({limit: '5mb'}));
//app.use(bodyParser.urlencoded({extended: true}));
//app.use(bodyParser.json());

var port = process.env.PORT || 4545;
var csvFolder = path.join(__dirname, 'public', 'csvs');


function generateId() {
    var randomString = Math.random().toString(32).slice(2, 9),
        numPadding = 7 - randomString.length,
        i;

    for (i = 0; i < numPadding; i += 1) {
        randomString += '0';
    }

    return randomString;
}


app.post('/csv', function (req, res) {
    var filename = generateId() + '.csv';

    if (req.headers['content-type'].indexOf('multipart/form-data') === 0) {
        var busboy = new Busboy({ headers: req.headers });
        busboy.on('file', function(fieldname, file, inputFilename, encoding, mimetype) {
            var csvStream = fs.createWriteStream(path.join(csvFolder, filename));
            file.pipe(csvStream);
            csvStream.on('error', function (err) {
                console.log(err);

                res.status(400);
                res.send({
                    message: 'failed. sorry.',
                    id: filename,
                    data: {
                        status: 'error'
                    }
                });
            });
        });
        busboy.on('field', function(fieldname, val, fieldnameTruncated, valTruncated) {
        });
        busboy.on('finish', function() {
            console.log(req.headers['host']);
            res.writeHead(303, { Connection: 'close', Location: 'http://localhost:3838/Dig/?csvfilename=' + filename });
            res.end();
            return;
            res.send({
                message: 'saved successfully',
                id: filename,
                data: {
                    status: 'ok'
                }
            });
        });
        req.pipe(busboy);
    } else {
        var csvStream = fs.createWriteStream(path.join(csvFolder, filename));
        req.pipe(csvStream);
        csvStream.on('close', function () {
            res.send({
                message: 'saved successfully',
                id: filename,
                data: {
                    status: 'ok'
                }
            });
        });

        csvStream.on('error', function (err) {
            console.log(err);

            res.status(400);
            res.send({
                message: 'failed. sorry.',
                id: filename,
                data: {
                    status: 'error'
                }
            });
        });
    }
});

app.use('/', express.static(path.join(__dirname, 'public')));

var server = http.createServer(app);
server.listen(port, function () {
    logger.info('Running on port: ' + port);
});
