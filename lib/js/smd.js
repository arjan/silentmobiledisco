var app = angular.module('smd', []);

app.config(function($interpolateProvider) {
    $interpolateProvider.startSymbol('[[').endSymbol(']]');
});

app.controller('main', function($scope) {

    var mp3src = document.getElementById('mp3src');
    var player = document.getElementById('player');

    var ws = false;

    player.addEventListener('canplay', function() {
        player.pause();
        ws.send("buffering_done");
        console.log('buffering done');

    }, false);
    
    $scope.status = 'disconnected';
    

    $scope.connect = function() {
        player.play();
        
        ws = new WebSocket("ws://" + document.location.host + "/ws");

        ws.onopen = function(){
            $scope.$apply(function() {
                console.log("Socket has been opened!");
            });
        };

        ws.onmessage = function(m) {
            $scope.$apply(function() {
                var msg = JSON.parse(m.data);

                angular.extend($scope,  msg);
                
                switch (msg.status) {
                case 'buffering':
                    player.src = '/media/attachment/' + msg.filename;
                    player.autoplay = true;
                    player.load();
                    
                    break;
                case 'playing':
                    console.log('go!');

                    setTimeout(function() { player.play(); }, 1000);
                    break;
                case 'waiting':
                    player.pause();
                    break;
                }
            });
        };
    };
    
    $scope.song_end = function() {
        ws.send("stop");
    };
    
});
