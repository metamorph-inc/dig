/**
 * Created by adam on 12/4/15.
 */
// server.js

// BASE SETUP
// =============================================================================

// call the packages we need
var express = require('express');        // call express
var app = express();                 // define our app using express
var bodyParser = require('body-parser');
var winston = require('winston');
var path = require('path');
var fs = require('fs-extra');

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
        new (winston.transports.File)({filename: 'server.log'})
    ]
});


// configure app to use bodyParser()
// this will let us get the data from a POST
app.use(bodyParser({limit: '5mb'}));
app.use(bodyParser.urlencoded({extended: true}));
app.use(bodyParser.json());

var port = process.env.PORT || 8080;        // set our port


function generateId() {
    var randomString = Math.random().toString(32).slice(2, 9),
        numPadding = 7 - randomString.length,
        i;

    for (i = 0; i < numPadding; i += 1) {
        randomString += '0';
    }

    return randomString;
}


var sessionDataRoot = path.join(__dirname, 'sessions');

function initSession(data, callback) {
    // Make a temp folder
    var sessionId = generateId();
    var sessionPath = path.join(sessionDataRoot, sessionId);
    fs.mkdirsSync(sessionPath);

    // Dump the CSV
    var csvPath = path.join(sessionPath, 'data.csv');
    var csvStream = fs.createWriteStream(csvPath);
    data.pipe(csvStream);
    csvStream.on('close', function () {
        // Do something else.

        // Respond
        callback(null, {
            message: 'saved successfully',
            id: sessionId,
            data: {
                status: 'ok'
            }
        });
    });

    csvStream.on('error', function (err) {
        console.log(err);

        callback(null, {
            message: 'failed. sorry.',
            id: sessionId,
            data: {
                status: 'error'
            }
        });
    });
}

app.post('/csv', function (req, res) {
    initSession(req, function (err, result) {
        if (err) {
            res.status(400);
            res.send({message: err});
        } else {
            res.send(result);
        }
    });
});

app.use('/home', express.static(path.join(__dirname, 'index.html')));

// START THE SERVER
// =============================================================================
app.listen(port, function () {
    logger.info('Running on port: ' + port);
});


