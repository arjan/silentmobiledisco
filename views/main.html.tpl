<div ng-controller="mainCtrl" class="full-page">
    <div class="header">
        {#<button style="float: left" ng-click="show_code()">Show my secret code</button>#}
        <button class="secondary" style="float:right" ng-click="logout()">Log out</button>
        <h3>[[ name ]] (score: [[ score ]])</h3>
    </div>

    <div class="content">

        <p ng-if="secret_code">
            <span class="right">My code: [[ secret_code ]]</span>
        </p>

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
                <p>Now playing: [[ title ]]</p>
                
                <p>[[ playback.currentTime|as_time ]] / [[ playback.duration|as_time ]]</p>

                <div class="buttons">
                    <button class="secondary" ng-click="song_end()">This song is boring…</button>
                    <button ng-show="!has_scored" ng-click="enter_code()">I found my dancing partner!</button>
                    <p ng-show="has_scored">Enjoy your dance with [[ connected_player.name ]]!!</p>
                </div>
            </div>
            
        </div>

        <button class="panic secondary" ng-click="panic()">Panic!</button>
        
    </div>
</div>
