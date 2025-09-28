const { Elm } = require('./build/elm.js');
const app = Elm.Main.init();


// app.ports.request.subscribe(req => {
//    app.ports.response.send(req);
// });