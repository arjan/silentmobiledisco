<div jqm-page ng-controller="mainCtrl">
    <div jqm-header>
        <button style="float:right" ng-click="logout()">Log out</button>
        <h3>Hello, [[ player ]]</h3>
    </div>

        <div ng-switch="status">

            <div ng-switch-when="waiting">
                <p>Waiting for someone else to join...</p>
            </div>

            <div ng-switch-when="buffering">
                <p>Loading the track...</p>
            </div>
            
            <div ng-switch-when="playing">
                Now playing: [[ title ]]
                
                <button ng-click="song_end()">Song end</button>
            </div>
            
        </div>

        <audio id="player" style="height: 1px" />
        
</div>
