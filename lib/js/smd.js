var app = angular.module('smd', ['zotonic']);

app.config(function($routeProvider, $interpolateProvider) {
    $interpolateProvider.startSymbol('[[').endSymbol(']]');
    
    $routeProvider
        .when("/:page", {
            templateUrl: function(params) {
                return '/views/' + params.page + '.html';
            }
        });
});

app.filter('as_time', function() {
    return function(v) {
        if (isNaN(v)) {
            return "--";
        }

        var date = new Date(1970,0,1);
        date.setSeconds(Math.floor(v));
        return date.toTimeString().replace(/.*(\d{2}:\d{2}).*/, "$1");
    };
});

app.factory('audioSupported', function() {
    return function() {
        if (!('Audio' in window)) return false;
        if ((new Audio()).canPlayType('audio/mpeg') == "") return false;
        return true;
    };
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

app.directive('appInit', function($location, $rootScope, appSocket, audioSupported) {
    return {
        restrict: 'E',
        link: function(scope, elm, attrs) {
            $rootScope.started = true;
            var socket = appSocket();
            if ($location.path() == '/highscore') 
                return;
            if (!audioSupported()) {
                $location.path('/unsupported');
            } else {
                socket.call('disco_init').then(function(player) {
                    if (player) {
                        $location.path('/main');
                    } else {
                        $location.path('/register');
                    }
                });
            }
        }
    };
});

app.controller('registerCtrl', function($scope, $location, $rootScope, appSocket) {
    
    var socket = appSocket(
        function onMessage(m) {
            console.log('got generic message', m);
        });
    
    $scope.start = function() {

        document.getElementById('player').play(); // trigger play here because otherwise chrome blocks the play event
        
        $rootScope.name = $scope.name;
        socket.cast('disco_register', {name: $scope.name, user_agent: navigator.userAgent});
        $location.path('/main');
    };
});

        

app.controller('mainCtrl', function($scope, $rootScope, $location, appSocket) {

    var player;
    var playing = false;

    $scope.score = 0;
    
    $rootScope.$on('$viewContentLoaded', function() {
        player = document.getElementById('player');
        
        player.addEventListener('canplay', function() {
            player.pause();
            if ($scope.status == 'buffering') {
                socket.cast('disco_buffering_done');
                console.log('buffering done');
                $scope.$apply(function() {
                    $scope.bufferingDone = true;
                });
            }
        }, false);

        player.addEventListener('ended', function() {
            player.src = undefined;
            socket.cast('disco_song_end');
            console.log('Playback ended');
        }, false);

        player.addEventListener('timeupdate', function() {
            $scope.$apply(function() {
                $scope.playback = {
                    currentTime: player.currentTime,
                    duration: player.duration,
                    percent: Math.floor(100 * player.currentTime/player.duration)
                };
            });
        }, false);

    });

    var socket = appSocket(function(message) {
        console.log(message);

        angular.extend($scope, message);
        console.log('msg', message);

        switch (message.status) {
        case 'buffering':
            player.src = '/media/attachment/' + message.filename;
            $scope.bufferingDone = false;
            player.autoplay = true;
            player.load();
            
            break;
        case 'playing':
            console.log('go!!');
            setTimeout(function() { player.play(); }, 1000);
            break;

        case 'waiting':
            player.pause();
            break;
        }
    });

    socket.call('disco_connect').then(function(r) {
        angular.extend($scope, r);
    });

    $scope.start = function() {
        document.getElementById('player').play(); // trigger play here because otherwise chrome blocks the play event
        socket.cast('disco_start');
        console.log('start!');

    };
    
    $scope.logout = function() {
        if (confirm('This will reset your score!! Are you sure?')) {
            $rootScope.name = undefined;
            socket.call('disco_stop').then(function() {
                document.location.reload();
            });
        }
    };

    $scope.song_end = function() {
        if (confirm('This will cost you 1 point. Are you sure?')) {
            socket.cast('disco_skip');
        }
    };

    $scope.show_code = function() {
        alert($scope.secret_code);
    };

    $scope.enter_code = function() {
        if ((code=prompt('Enter the secret code of your dancing partner to earn points!\n\nIf you enter an incorrect code, you get a penalty point.'))) {
            socket.call('disco_guess', {code: code}).then(function(result) {
                if (result) {
                    alert("Yeah!! You did it. Enjoy the rest of the song!");
                } else {
                    alert("That was the wrong code..! boo!");
                }
            });
        }
    };

    $scope.panic = function() {
        if ((confirm('This button will reset your disco state. Use only when the interface does not respond or you think it hangs.\n\nYour will be kept safe.'))) {
            socket.cast('disco_panic');
            setTimeout(function() {
                document.location.reload();
            }, 100);
        }
    };
    
});

app.controller('highscoreCtrl', function($scope, appSocket) {
    var socket = appSocket(function(message) {
        console.log(message);
        if (message.message == 'disco_highscores') {
            $scope.highscores = message.args;
        }
    });

    socket.cast('disco_attach_highscores');
});
