var app = angular.module('smd', []);

app.config(function($interpolateProvider) {
    $interpolateProvider.startSymbol('[[').endSymbol(']]');
});

app.controller('main', function($scope) {

    var mp3src = document.getElementById('mp3src');
    var player = document.getElementById('player');
    
    $scope.status = 'disconnected';
    
    var ws = false;

    $scope.connect = function() {
        ws = new WebSocket("ws://" + document.location.host + "/ws");

        ws.onopen = function(){
            $scope.$apply(function() {
                console.log("Socket has been opened!");
            });
        };

        ws.onmessage = function(m) {
            $scope.$apply(function() {
                var msg = JSON.parse(m.data);
                console.log(msg);

                angular.extend($scope,  msg);
                console.log(msg.status);

                switch (msg.status) {
                case 'playing':
                    player.src = '/media/attachment/' + msg.filename;
                    player.load();
                    player.play();
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
