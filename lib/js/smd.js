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

    return function(onMessage) {
        if (typeof onMessage == 'function') {
            socket.setOnMessage(onMessage);
        }
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
    
    var socket = appSocket(
        function onMessage(m) {
            console.log('got generic message', m);
        });
    
    socket.call('rsc', {id: 'depiction'}).then(function(result) {
        console.log(result.rsc.title);
    });

    $scope.start = function() {
        $rootScope.player = $scope.player;
        socket.cast('set_session', {player: $scope.player});
        $location.path('/main');
    };
});

        

app.controller('mainCtrl', function($scope, $rootScope, $location, appSocket) {

    var player;

    $rootScope.$on('$viewContentLoaded', function() {
        player = document.getElementById('player');
        player.addEventListener('canplay', function() {
            player.pause();
            socket.cast('disco_buffering_done');
            console.log('buffering done');
        }, false);
    });

    var socket = appSocket(function(message) {
        angular.extend($scope, message);

        switch (message.status) {
        case 'buffering':
            player.src = '/media/attachment/' + message.filename;
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

    socket.cast('disco_start');

    $scope.logout = function() {
        $rootScope.player = undefined;
        socket.cast('set_session', {player: null});
        socket.cast('disco_stop');
        $location.path('/register');
    };

    $scope.song_end = function() {
        socket.cast('disco_skip');
    };

});
