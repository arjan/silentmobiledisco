<div ng-controller="mainCtrl" class="full-page">
    <div class="header">
        <span class="right">
            <span ng-show="online_count>0">Players: [[ online_count ]]</span>
            <button class="secondary" ng-click="logout()">Log out</button>
        </span>
        <h3>[[ name ]] (score: [[ score ]])</h3>
    </div>

    <div class="content">

        <message-area></message-area>
        
        <div ng-switch="status">

            <div ng-switch-when="registered">
                <p class="statusmsg">Click the button to start.</p>
                <div class="buttons">
                    <button ng-click="start()">Join the disco!</button>
                </div>
            </div>
            
            <div ng-switch-when="waiting">
                <p class="statusmsg">Waiting for someone else to join...</p>
                <p class="statusmsg"><img src="/lib/images/loading.gif" /></p>
        
            </div>

            <div ng-switch-when="buffering">
                <p ng-show="!bufferingDone" class="statusmsg">
                    Now loading the track...
                </p>
                <p ng-show="bufferingDone" class="statusmsg">
                    Waiting on your partner to finish loading the track...
                </p>
                <p class="statusmsg"><img src="/lib/images/loading.gif" /></p>
            </div>
            
            <div ng-switch-when="playing">
                <p>
                    <span class="right">My code: [[ secret_code ]]</span>
                    Let's disco!
                </p>

                <h2 ng-show="has_revealed">[[ title ]]</h2>

                <p ng-show="getReady">Get ready...!!!</p>
                <p ng-show="!getReady">[[ playback.currentTime|as_time ]] / [[ playback.duration|as_time ]]</p>

                <div class="buttons">
                    <button ng-show="!has_scored" ng-click="enter_code()">I found my dancing partner!</button>
                    <p ng-show="has_scored">Enjoy your dance with [[ connected_player.name ]]!!</p>
                    
                    <button class="secondary" ng-click="song_title()" ng-disabled="has_revealed">Reveal song title</button>
                    <button class="secondary" ng-click="song_end()">This song is boring…</button>
                </div>
            </div>
            
        </div>

        <button class="panic secondary" ng-click="panic()">Panic!</button>
        
    </div>
</div>
