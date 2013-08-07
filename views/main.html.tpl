<div jqm-page ng-controller="mainCtrl">
    <div jqm-header>
        {#<button style="float: left" ng-click="show_code()">Show my secret code</button>#}
        <button class="secondary" style="float:right" ng-click="logout()">Log out</button>
        <h3>Hello, [[ name ]]</h3>
    </div>

    <p>
        <div ng-show="secret_code" class="right">My code: [[ secret_code ]]</div>
        <b>My score: [[ score ]]</b>
    </p>

    <div ng-switch="status">

        <div ng-switch-when="waiting">
            <p>Waiting for someone else to join...</p>
        </div>

        <div ng-switch-when="buffering">
            <p>Loading the track...</p>
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
        
</div>
