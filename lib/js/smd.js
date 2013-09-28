var app = angular.module('smd', ['zotonic']);

app.directive('appButton', function() {
    return function(scope, elm) {
        elm
            .on("touchstart mousedown", function() {
                $(this).addClass("active");
            })
            .on("touchend mouseup", function() {
                $(this).removeClass("active");
            });
    };
});

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

app.factory('browserSupported', function() {
    return function() {
        if (!('WebSocket' in window)) return false;
        if (!('Audio' in window)) return false;
        if ((new Audio()).canPlayType('audio/mpeg') == "") return false;
        return true;
    };
});

app.directive('messageArea', function($timeout) {
    return {
        restrict: 'E',
        template: '<div class="message-area"></div>',
        replace: true,
        link: function(scope, elm, attrs) {
            scope.$parent.messageArea = scope;
            scope.message = function(msg, cls) {
                var el = $("<div>").text(msg);
                if (cls) el.attr("class", cls);
                el.prependTo(elm);
                $timeout(function() {
                    el.slideUp(function() { el.remove(); });
                }, 5000);
            };
        }
    };
});
              
app.factory('appSocket', function(zotonicSocket) {
    var socket = zotonicSocket(null, true);
    return function(onMessage) {
        if (typeof onMessage == 'function') {
            socket.setOnMessage(onMessage);
        }
        
        return socket;
    };
});

app.directive('appInit', function($location, $rootScope, appSocket, browserSupported) {
    return {
        restrict: 'E',
        link: function(scope, elm, attrs) {
            if (browserSupported()) {
                $rootScope.started = true;
                var socket = appSocket();
                if ($location.path() == '/highscore') {
                    return;
                } else {
                    socket.call('smd_disco', 'init').then(function(state) {

                        switch(state.screen) {
                        case 'stopped':
                            $location.path('/stopped');
                            break;

                        case 'start':
                            $location.path('/main');
                            break;
                            
                        case 'register':
                            $location.path('/register');
                            break;
                        }
                    });
                }
            } else {
                // Sorry.
                $rootScope.unsupported = true;
            }
        }
    };
});

app.value('idlePlayer', (function() {
    var player = new Audio();
    player.loop = true;
    player.src = '/lib/media/waiting.mp3';
    player.load();
    player.pause();
    return player;
})());

app.value('player', (function() {
    return new Audio();
})());
          
app.controller('registerCtrl', function($scope, $location, $rootScope, appSocket, idlePlayer, player) {
    
    var socket = appSocket(
        function onMessage(m) {
            console.log('got generic message', m);
        });
    
    $scope.start = function() {

        player.play(); // trigger play here because otherwise chrome blocks the play event
        idlePlayer.play();
        idlePlayer.pause();
        
        $rootScope.name = $scope.name.trim();
        socket.cast('smd_disco', 'register', {name: $scope.name, user_agent: navigator.userAgent});
        $location.path('/main');
    };
});

        

app.controller('mainCtrl', function($scope, $rootScope, $location, $timeout, appSocket, player, idlePlayer) {

    window.s = $scope;
    
    $scope.score = 0;
    $scope.online_count = 0;
    
    player.addEventListener('canplay', function() {
        player.pause();
        if ($scope.status == 'buffering') {
            console.log('bufffer done', $scope.final_song);

            socket.cast('smd_disco', 'buffering_done', {final_song: $scope.final_song});
            console.log('buffering done');
            $scope.$apply(function() {
                $scope.bufferingDone = true;
            });
        }
    }, false);

    player.addEventListener('timeupdate', function() {
        $scope.$apply(function() {
            $scope.playback = {
                currentTime: player.currentTime,
                duration: player.duration,
                percent: Math.floor(100 * player.currentTime/player.duration)
            };

            if (player.currentTime >= player.duration*0.99 && $scope.status == 'playing') {
                console.log('Playback ended');
                $scope.status = 'waiting'; // temp status, it gets set by the server
                player.src = undefined;
                socket.cast('smd_disco', 'song_end');
            }
        });
    }, false);

    var socket = appSocket(function(message) {

        if (message.message) {
            $scope.messageArea.message(message.message);
            return;
        }

        if (message.disco_reset) {
            setTimeout(function() {document.location.reload();}, 200);
        }
        
        switch (message.status) {
        case 'buffering':
            console.log('buffering...');

            player.src = '/media/attachment/' + message.filename;
            $scope.bufferingDone = false;
            player.autoplay = true;
            player.load();
            
            break;
        case 'playing_final_song':
            player.play();
            break;
            
        case 'playing':
            
            if ($scope.status != 'playing') {
                $scope.enteringCode = false;
                idlePlayer.pause();
                $scope.getReady = true;
                $timeout(function() {
                    $scope.getReady = false;
                    player.play();
                    //player.currentTime = player.duration - 15;
                }, 2000);
                
            }
            if (!$scope.has_scored && message.has_scored) {
                setLights(true);
                $scope.enteringCode = false;
                $scope.messageArea.message("Boom!! Enjoy your dance with " + message.connected_player.name + "!");
            }
            break;

        case 'waiting':
            setLights(true);
            player.pause();
            idlePlayer.play();
            break;
        }

        if (message.status && message.status.length > 0 && message.status != 'playing') {
            setLights(false);
        }
        angular.extend($scope, message);
        
    });

    socket.setOnClose(function() {
        $scope.status = 'waiting'; // better than nothing
        $timeout(function() {
            console.log('Reconnecting...');
            socket.reconnect();
            socket.cast("smd_disco", "start");
        }, 3000);
    });
    
    socket.call('smd_disco', 'connect').then(function(r) {
        angular.extend($scope, r);
    });

    $scope.start = function() {
        idlePlayer.play();
        player.play(); // trigger play here because otherwise chrome blocks the play event
        socket.cast('smd_disco', 'start');
        console.log('start!');
        idlePlayer.pause();
    };
    
    $scope.logout = function() {
        if (confirm('This will reset your score!! Are you sure?')) {
            $rootScope.name = undefined;
            socket.call('smd_disco', 'stop').then(function() {
                document.location.reload();
            });
        }
    };

    $scope.song_end = function() {
        if (confirm('Skipping a song will cost you 2 points. Are you sure?')) {
            socket.cast('smd_disco', 'skip');
        }
    };

    $scope.enter = {
        code: ''
    };
    
    $scope.show_code = function() {
        alert($scope.secret_code);
    };


    $scope.enter_code = function() {
        $scope.enter.code = '';
        $scope.enteringCode = true;
        $scope.enteredCode = '';
    };

    
    $scope.submitEnterCode = function() {
        $("#enternumber").blur();
        if (!$scope.has_scored) {
            socket.call('smd_disco', 'guess', {code: (''+$scope.enter.code)}).then(function(result) {
                $scope.enteringCode = false;
                if (result) {
                    $scope.messageArea.message("Match! You did it. +10 pts!");
                } else {
                    $scope.messageArea.message("No match...boo! -1 pts", "error");
                }
            });
        } else {
            $scope.enteringCode = false;
        }
    };
   
    $scope.cancelEnterCode = function() {
        $scope.enteringCode = false;
    };
    
    $scope.panic = function() {
        if ((confirm('This button will reset your disco state. Use only when the interface does not respond or you think it hangs.\n\nYour score will be kept safe.'))) {
            socket.cast('smd_disco', 'panic');
            setTimeout(function() {
                document.location.reload();
            }, 100);
        }
    };
    
    $scope.song_title = function() {
        if ((confirm('Revealing the title of the song will cost you 1 point.'))) {
            socket.cast('smd_disco', 'reveal_title');
        }
    };

    $scope.lights = false;
    var l = null;
    
    var ci = 0;
    var colors = ["#ff0000", "#00ff00", "#ffff00", "#00ffff", "#ff00ff"];
    function nextColor() {
        var color = colors[ci];
        ci = (ci + 1) % colors.length;
        return color;
    }
    
    function blink() {
        $("div.full-page").css({backgroundColor: nextColor()});
    }
    
    $scope.toggle_lights = function() {
        setLights(!$scope.lights);
    };

    function setLights(flag) {
        $scope.lights = flag;
        if ($scope.lights) {
            if (l) clearInterval(l);
            l = setInterval(blink, 200);
        } else {
            clearInterval(l);
            $("div.full-page").css({backgroundColor: "#ffffff"});
        }
    }
});


app.controller('highscoreCtrl', function($scope, appSocket) {
    var socket = appSocket(function(message) {
        console.log(message);
        if (message.message == 'disco_highscores') {
            $scope.highscores = message.args;
        }
    });

    socket.cast('smd_admin', 'attach_highscores');

    $scope.broadcast = function() {
        if ((msg=prompt("Enter message to send to all connected players:"))) {
            socket.cast('smd_admin', 'broadcast', {message: msg});
        }
    };

    $scope.disco_end = function() {
        if (confirm("DISCO END\n\nAre you sure??? This will stop the disco and everybody will hear the final song.")) {
            socket.cast('smd_admin', 'disco_end');
        }
    };

    $scope.reset = function() {
        if (confirm("RESET DISCO\n\nAre you sure??? This will reset all scores and log everybody out.")) {
            socket.cast('smd_admin', 'disco_reset');
        }
    };
});
