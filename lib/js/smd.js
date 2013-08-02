var app = angular.module('smd', ['jqm', 'zotonic']);

app.config(function($routeProvider, $interpolateProvider) {
    $interpolateProvider.startSymbol('[[').endSymbol(']]');
    
    $routeProvider
        .when("/:page", {
            templateUrl: function(params) {
                return '/views/' + params.page + '.html';
            }
        });
});

app.factory('appSocket', function(zotonicSocket) {
    var socket = zotonicSocket('/ws');

    return function(callbackScope) {
        socket.setCallbacks(callbackScope);
        return socket;
    };
});

app.directive('appInit', function($location, $rootScope) {
    return {
        restrict: 'E',
        link: function(scope, elm, attrs) {
            console.log(attrs.player);
            $rootScope.player = attrs.player;
            if (attrs.player) {
                $location.path('/main');
            } else {
                $location.path('/register');
            }
        }
    };
});

app.controller('registerCtrl', function($scope, $location, $rootScope, appSocket) {
    var socket = appSocket($scope);

    $scope.onMessage = function(m) {
        console.log('got generic message', m);

    };

    socket.call('rsc', {id: 123}).then(function(result) {
        console.log(result.rsc.title);
    });


    $scope.start = function() {
        $rootScope.player = $scope.player;
        $location.path('/main');
    };
});

        
/*
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

    function onWebsocketMessage(m) {
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
    }

    $scope.connect = function() {

        console.log($scope.connectForm.$valid);
        return;

        player.play();
        
        ws = new WebSocket("ws://" + document.location.host + "/ws");

        ws.onmessage = function(m) {
            $scope.$apply(function() { onWebsocketMessage(m); }); 
        };
    };
    
    $scope.song_end = function() {
        ws.send("stop");
    };
    
});
*/
