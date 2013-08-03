<div jqm-page ng-controller="mainCtrl">
    <div jqm-header>
        {#<button style="float: left" ng-click="show_code()">Show my secret code</button>#}
        <button class="secondary" style="float:right" ng-click="logout()">Log out</button>
        <h3>Hello, [[ player ]]</h3>
    </div>

    <p><b>My score: [[ score ]]</b></p>
    
    <div ng-switch="status">

        <div ng-switch-when="waiting">
            <p>Waiting for someone else to join...</p>
        </div>

        <div ng-switch-when="buffering">
            <p>Loading the track...</p>
        </div>
        
        <div ng-switch-when="playing">
            <p>Now playing: [[ title ]]</p>

            <div class="buttons">
                <button class="secondary" ng-click="song_end()">This song is boring…</button>
                <button ng-click="enter_code()">I found my dancing partner!</button>
            </div>
        </div>
        
    </div>
    
    <audio id="player" style="height: 1px" />
    
</div>
