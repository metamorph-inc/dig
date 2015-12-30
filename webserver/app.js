/*globals require,process,__dirname,console*/
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
var util = require('util');
var http = require('http');
var Busboy = require('busboy');
var httpProxy = require('http-proxy');
var HttpProxyRules = require('http-proxy-rules');

var shinyUpstreamUrl = 'http://' + (process.env['DIG_PORT_3838_TCP_ADDR'] || 'localhost') + ':3838/';
var port = process.env.PORT || 4545;
var csvFolder = path.join(__dirname, 'public', 'csvs');

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
                logger.warn('Error saving csv: ' + util.inspect(err));

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
            // console.log(req.headers['host']);
            res.writeHead(303, { Connection: 'close', Location: '/Dig/?csvfilename=' + filename });
            res.end();
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
            logger.warn('Error saving csv: ' + util.inspect(err));

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
server.listen(0, function () {
    // console.log(server.address().port);

    var proxyRules = new HttpProxyRules({
        rules: {
            '/Dig/': shinyUpstreamUrl + '/Dig/',
            '/shared/': shinyUpstreamUrl + '/shared/',
        },
        default: 'http://localhost:' + server.address().port
    });

    // Create reverse proxy instance
    var proxy = httpProxy.createProxy();

    // Create http server that leverages reverse proxy instance
    // and proxy rules to proxy requests to different targets
    var reverseHttp = http.createServer(function(req, res) {
        // console.log(req.url);

        // a match method is exposed on the proxy rules instance
        // to test a request to see if it matches against one of the specified rules
        var target = proxyRules.match(req);
        if (target) {
            return proxy.web(req, res, {
                target: target
            }, function (err) {
                logger.warn('Error from upstream ' + util.inspect(err));
                if (res.headersSent === false) {
                    res.writeHead(500, {'Content-Type': 'text/plain'});
                    res.write('error');
                }
                res.end();
            });
        }

        res.writeHead(500, { 'Content-Type': 'text/plain' });
        res.end('The request url and path did not match any of the listed rules!');
    });
    reverseHttp.listen(port, function () {
        logger.warn('Listening on ' + port);
    });

    reverseHttp.on('upgrade', function (req, socket, head) {
        proxy.ws(req, socket, head, {
            target: shinyUpstreamUrl
        });
    });
});
