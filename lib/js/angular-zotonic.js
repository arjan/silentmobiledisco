var app = angular.module('zotonic', []);
app.value('uniqueID', function(n) {
    return Math.random().toString(36).substr(2, n);
});

app.factory(
    'zotonicSocket', 
    function($q, $rootScope, uniqueID) {

        return function(socketURL) {

            var calls = {};
            var connected = false;
            var connectQueue = [];
            var q_onopen = $q.defer();

            var onMessage = function(m) {
                console.log('Got message, but no onMessage handler attached!', m);
            };

            var onClose = function(m) {
                console.log('Socket closed..');
            };

            if (socketURL.match(/^\//)) {
                socketURL = document.location.protocol.replace("http", "ws") + "//" + document.location.host + socketURL;
            }
            
            var ws;
            function onopen() {
                $rootScope.$apply(function() {
                    connected = true;
                    console.log('Connected to ' + socketURL);
                    angular.forEach(connectQueue, function(msg) {
                        send(msg);
                    });
                    connectQueue = [];
                    q_onopen.resolve();
                });
            }

            function onmessage(m) {
                $rootScope.$apply(function() {
                    var msg = JSON.parse(m.data);

                    if (typeof msg.reply_id != 'undefined') {
                        // response to a call
                        // FIXME how to handle an unknown call_id?
                        calls[msg.reply_id].resolve(msg.reply);
                        delete calls[msg.reply_id];
                        return;
                    }

                    onMessage(msg);
                });
            }

            function onclose() {
                connected = false;
                $rootScope.$apply(function() {
                    onClose();
                });
            }

            function connect() {
                ws = new WebSocket(socketURL);
                ws.onopen = onopen;
                ws.onmessage = onmessage;
                ws.onclose = onclose;
            }
            
            connect();
            
            function send(payload) {
                if (connected) {
                    ws.send(payload);
                } else {
                    connectQueue.push(payload);
                }
            }

            // Call function; can have a reply
            function call(command, args) {
                args = args || {};
                var id = uniqueID(8);
                calls[id] = $q.defer();
                send("call:" + id + ":" + command + ":" + JSON.stringify(args));
                return calls[id].promise;
            }

            // Simple cast function, without reply
            function cast(command, args) {
                args = args || {};
                send("cast:" + command + ":" + JSON.stringify(args));
            }
            
            window.ws = ws;            
            return {
                onopen: q_onopen.promise,
                setOnMessage: function(m) {
                    onMessage = m;
                },
                setOnClose: function(c) {
                    onClose = c;
                },
                reconnect: connect,
                call: call,
                cast: cast
            };
        };
    });
