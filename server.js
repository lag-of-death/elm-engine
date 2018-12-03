const express = require(`express`);

const port = process.env.PORT || 5000;
const publicDir = `${__dirname}/public`;

express()
    .use(express.static(publicDir))
    .listen(port);
